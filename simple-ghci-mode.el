;;; simple-ghci-mode.el --- Simple ghci mode

;; Copyright (c) 2018 Josef Vlach

;; Homepage: https://github.com/VlachJosef/simple-ghci-mode
;; Package-Version:  0.1

;;; Commentary:
;;
;;; Code:

(require 'hydra "hydra" 't)
(require 'comint)

(defcustom sgm:program-name "stack"
  "Program invoked by the `sgm:run-ghci' command."
  :type 'string
  :group 'ghci)

(defcustom sgm:buffer-name-base "*ghci*"
  "Buffer name for ghci"
  :type 'string
  :group 'ghci)

(defcustom sgm:allowed-files-regexp '(".*.hs$")
  "Regexp to match files when save should run ghci reload"
  :type '(repeat string)
  :group 'ghci)

(defconst sgm:prompt-regexp "^ghci> ")

(defvar-local sgm:program-params nil ;; dir-local
  "Parameters to `sgm:program-name'")

(defvar-local sgm:on-reload-command nil ;; dir-local
  "Command to run on save of a file")

(defvar-local sgm:compilation-environment nil
  "List of environment variables for compilation to inherit") ;; dir-local

(defvar-local sgm:extra-program-params nil
  "List of extras") ;; dir-local

(defvar-local sgm:loaded-modules nil
  "List of modules")

(defvar-local sgm:unprocessed-modules-chunk nil
  "Internal usage - holds unprocessed chunk of ':show modules' command")

(defvar-local sgm:preoutput-filter-print-definition-only nil
  "Internal usage - determines if ':info' commands prints definition only")

(defvar-local sgm:preoutput-filter-processed-info nil
  "Internal usage - holds accumulated output of ':info' command")

(defvar sgm:buffer-project-root nil)

;; Make `sgm:program-name' and `sgm:on-reload-command' safe if their values is a string
;; For example:
;; ((simple-ghci-mode
;;   (sgm:program-name . "cabal")
;;   (sgm:on-reload-command . "Main.main")
;; ))
(put 'sgm:program-name      'safe-local-variable 'stringp)
(put 'sgm:on-reload-command 'safe-local-variable 'stringp)

;; Make `sgm:program-params', `sgm:compilation-environment' and `sgm:extra-program-params' safe if their values is list of strings
;; For example:
;; ((simple-ghci-mode
;;   (sgm:program-params . ("ghci" "--test" "crypto-ledger:lib" "crypto-ledger:crypto-ledger-test"))
;;   (sgm:compilation-environment . '("CFLAGS=-I/usr/local/opt/readline/include" "LDFLAGS=-L/usr/local/opt/readline/lib"))
;;   (sgm:extra-program-params . '("--extra-include-dirs=/usr/local/opt/readline/include" "--extra-lib-dirs=/usr/local/opt/readline/lib"))
;; ))
(put 'sgm:program-params          'safe-local-variable 'sgm:is-list-of-strings)
(put 'sgm:extra-program-params    'safe-local-variable 'sgm:is-list-of-strings)
(put 'sgm:compilation-environment 'safe-local-variable 'sgm:is-list-of-strings)

(defun sgm:is-list-of-strings (list)
  "Check if `list' is list containing only strings"
  (null (memq nil (mapcar 'stringp list))))

(defun sgm:find-root ()
  "Starting from the current `default-directory', find a parent
directory that is an ghci root. An ghci root directory is
identified by the following rules:

  - a directory containing a 'stack.yaml' in it."
  (or sgm:buffer-project-root
      (let ((root (sgm:find-root-impl "stack.yaml")))
        (when root
          (setq-local sgm:buffer-project-root root)))))

(defun sgm:find-root-impl (name-or-pred &optional dir best-root)
  (when (null dir) (setq dir default-directory))
  (let ((parent (if (string-match locate-dominating-stop-dir-regexp dir) nil
                  (file-name-directory (directory-file-name dir)))))
    (cond ((or (null parent)
               (equal dir parent))
           (and best-root (abbreviate-file-name best-root)))
          ((file-exists-p (expand-file-name name-or-pred dir))
           (sgm:find-root-impl name-or-pred parent (if best-root best-root dir)))
          ('t
           (sgm:find-root-impl name-or-pred parent best-root)))))

(defun sgm:buffer-name ()
  "Return the buffer name for running ghci."
  (format "%s<%s>"
          sgm:buffer-name-base
          (abbreviate-file-name (sgm:find-root))))

(defun sgm:run-ghci (callback)
  (let ((project-root (or (sgm:find-root)
                          (error "Could not find project root, type `C-h f sgm:find-root` for help.")))
        (buffer-name (sgm:buffer-name)))

    (with-current-buffer (get-buffer-create buffer-name)
      (pop-to-buffer-same-window (current-buffer))
      (unless (derived-mode-p 'simple-ghci-mode) (simple-ghci-mode))
      (cd project-root)
      (hack-dir-local-variables-non-file-buffer)
      (when (not (or (executable-find sgm:program-name)))
        (error "Could not find %s on PATH. Please customize the sgm:program-name variable." sgm:program-name))
      (setq-local compilation-environment sgm:compilation-environment)
      (funcall callback sgm:program-name (append sgm:program-params sgm:extra-program-params) buffer-name project-root))))

(defun sgm:next-error ()
  (interactive)

  (let* ((mode-buffer (sgm:get-mode-buffer))
         (compilation-context-lines 4)
         (next-error-last-buffer mode-buffer))
    (when mode-buffer (next-error))))

(defvar sgm:mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map
                       (make-composed-keymap compilation-shell-minor-mode-map
                                             comint-mode-map))
    (define-key map (kbd "C-a") 'comint-bol)
    (define-key map (kbd "C-c v") 'sgm:run-hydra)
    (define-key map (kbd "C-c C-v") 'comint-clear-buffer)
    map)
  "Basic mode map for `sgm-start'")

(define-derived-mode simple-ghci-mode comint-mode "sgm"
  "Major mode for `ghci-start'.

  \\{sgm:mode-map}"
  (use-local-map sgm:mode-map)
  (add-hook 'simple-ghci-mode-hook 'sgm:initialize-for-comint-mode)
  (add-hook 'simple-ghci-mode-hook 'sgm:initialize-for-compilation-mode))

(defun sgm:require-buffer ()
  "Throw error if buffer the current buffer is not an ghci-buffer"
  (unless (derived-mode-p 'simple-ghci-mode)
    (error "Current buffer %s is not an simple-ghci-mode buffer" (current-buffer))))

(defun sgm:minibuffer-compilation-status (input-string)
  (let* ((beg (save-excursion
                (goto-char comint-last-output-start)
                (move-beginning-of-line nil)
                (point)))
         (line-beginning (buffer-substring-no-properties beg comint-last-output-start))
         (input-string-complete (if (string-match sgm:prompt-regexp line-beginning)
                                    input-string ;; Do not concat prompt
                                  (concat line-beginning input-string))))

    (when (string-match "^\\([[:alpha:]]*\\), \\(no\\|one\\|two\\|three\\|four\\|five\\|six\\|[[:digit:]]*\\) modules? loaded.$" input-string-complete)
      (message "Compilation %s." (match-string-no-properties 1 input-string-complete)))))

(defmacro sgm:on-hoogle-command (body)
  `(unless (ring-empty-p comint-input-ring)
    (let ((head (ring-ref comint-input-ring 0)))
      (when (string-match ":hoogle" head)
        ,body))))

(defun sgm:pretify-hoogle-align (input-string)
  (sgm:on-hoogle-command
   (when (string-match sgm:prompt-regexp input-string)
     (save-excursion
       (let ((indent-tabs-mode nil)
             (end (point-marker))
             (start (progn
                      (comint-previous-prompt 1)
                      (point))))
         (align-regexp start end "\\(\\s-*\\)\\(::\\)")
         (align-regexp start end "\\(\\s-*\\)\\(--\\)"))))))

(defun sgm:pretify-hoogle-invisibility (input-string)
  (sgm:on-hoogle-command
   (let ((string-to-hide "https://hackage.haskell.org/package/"))
     (save-excursion
       (goto-char comint-last-output-start)
       (move-beginning-of-line nil)
       (while (search-forward string-to-hide nil t)
         (put-text-property (- (point) (length string-to-hide)) (point) 'invisible t))))))

(defmacro sgm:filter-on-command (input-string command body)
  `(if (and (not (ring-empty-p comint-input-ring))
            (string-match ,command (ring-ref comint-input-ring 0)))
       ,body
     ,input-string))

(defun sgm:process-modules (input-string)
  (sgm:filter-on-command
   input-string ":show modules"
   (let ((input-string-unprocessed (concat sgm:unprocessed-modules-chunk input-string))
         (start 0))
     (while (string-match "\\([[:word:].]*\\)[[:space:]]*(" input-string-unprocessed start)
       (push (match-string-no-properties 1 input-string-unprocessed) sgm:loaded-modules)
       (setq start (match-end 0)))  ;; we want to continue from end of whole match
     (setq sgm:unprocessed-modules-chunk (substring input-string-unprocessed (match-end 0)))
     (when (string-match sgm:prompt-regexp input-string-unprocessed) ;; end of output
       (sgm:run-repl-command
        (concat ":module + *" (mapconcat 'identity sgm:loaded-modules " *")))
       (setq sgm:loaded-modules nil
             sgm:unprocessed-modules-chunk nil))
     ""))) ;; don't print anything

(defconst sgm:section-to-skip "\\(^instance \\|^type instance \\)")

(defun sgm:definition-only-or-full-info (input-string)
  (sgm:filter-on-command
   input-string ":info"
   (let ((input-string-processed (concat sgm:preoutput-filter-processed-info input-string)))
     (pcase sgm:preoutput-filter-print-definition-only
       ('nil input-string)
       ('back-reference
        (setq sgm:preoutput-filter-processed-info nil)
        input-string-processed)
       ((and
         'accumulate
         (guard (string-match sgm:prompt-regexp input-string)))
        (let ((prompt (match-string 0 input-string)))
          (sgm:definition-filter-reset)
          (if (string-match sgm:section-to-skip input-string-processed 0) ;; prompt and instance listing may be in same chunk
              (concat (substring input-string-processed 0 (match-beginning 0)) prompt)
            input-string-processed)))  ;; There has been no instance listing
       ((guard (string-match sgm:prompt-regexp input-string))
        (sgm:definition-filter-reset)
        (match-string 0 input-string))
       ('skip-rest "")
       ((guard (string-match sgm:section-to-skip input-string-processed 0))
        (setq sgm:preoutput-filter-print-definition-only 'skip-rest)
        (substring input-string-processed 0 (match-beginning 0)))
       ('accumulate
        (setq sgm:preoutput-filter-processed-info input-string-processed)
        "")
       (_ input-string)))))

(defun sgm:definition-filter-reset ()
  (setq sgm:preoutput-filter-print-definition-only nil
        sgm:preoutput-filter-processed-info nil))

(defun sgm:initialize-for-comint-mode ()
  (sgm:require-buffer)
  (when (derived-mode-p 'comint-mode)
    (setq comint-process-echoes nil)
    (setq comint-scroll-to-bottom-on-output t)
    (setq compilation-skip-threshold 0)
    (setq-local comint-prompt-regexp sgm:prompt-regexp)
    (setq-local comint-use-prompt-regexp t)
    (setq-local comint-prompt-read-only t)
    (setq-local comint-buffer-maximum-size 4096)
    (setq-local comint-preoutput-filter-functions '(sgm:process-modules
                                                    sgm:definition-only-or-full-info))
    (setq-local comint-output-filter-functions '(sgm:minibuffer-compilation-status
                                                 sgm:pretify-hoogle-align
                                                 sgm:pretify-hoogle-invisibility))))

(defface sgm:face-unimportant
  '((t :foreground "gray58")) "highlight less important text")

(defface sgm:face-unimportant-info
  '((t :foreground "wheat3")) "highlight less important text when running :info")

(defface sgm:face-thin-arrow
  '((t :foreground "dark salmon")) "highlight ->")

(defface sgm:face-thick-arrow
  '((t :foreground "plum2")) "highlight =>")

(defface sgm:face-run-command
  '((t :foreground "cyan2")) "Executed `sgm:program-name' command")

(defun sgm:initialize-for-compilation-mode ()
  (setq-local
   compilation-error-regexp-alist
   '(("^\\(.*.hs\\):\\([[:digit:]]+\\):\\([[:digit:]]+\\): error:" 1 2 3 2 1)
     ("^  \\(.*.hs\\):\\([[:digit:]]+\\): " 1 2 3 2 1) ;; Hspec failures
     ("^\\(.*.hs\\):\\([[:digit:]]+\\):\\([[:digit:]]+\\): warning:" 1 2 3 1 1)
     ("-- \\(Defined at\\|Searched from\\) \\(.*\\):\\([[:digit:]]+\\):\\([[:digit:]]+\\)$" 2 3 4 0 2)))
  (setq-local compilation-mode-font-lock-keywords
              '(("-- Defined in ‘.*’" 0 'sgm:face-unimportant prepend)
                ("-- \\(Defined at\\|Searched from\\) .*:[[:digit:]]+:[[:digit:]]+$" 0 'sgm:face-unimportant-info prepend)
                ("(bound at /.*:[[:digit:]]+:[[:digit:]]+)$" 0 'sgm:face-unimportant prepend)
                ("\\[ *[[:digit:]]+ of [[:digit:]]+] Compiling" 0 'sgm:face-unimportant prepend)
                ("( /.*, interpreted )$" 0 'sgm:face-unimportant prepend)
                ("( .*o )$" 0 'sgm:face-unimportant prepend)
                ("Running \\(.*\\)$" 0 'sgm:face-run-command prepend)
                (" -> " 0 'sgm:face-thin-arrow prepend)
                ("\\( ::\\| =>\\)" 0 'sgm:face-thick-arrow prepend)))
  (compilation-setup t))

(defun sgm:check-modified-buffers ()
  "Check modified buffers matching `sgm:allowed-files-regexp' regexps
If there is only one modified buffer then add `sgm:reload-ghci'
to run in `after-save-hook'."
  (let (buffers-to-save)
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (cl-loop for allowed-file-regexp being the elements of sgm:allowed-files-regexp
                 if (and (buffer-file-name) ;;  If `buffer' is not visiting any file, `buffer-file-name' returns nil
                         (string-match allowed-file-regexp (buffer-file-name))
                         (buffer-modified-p))
                 do (push (buffer-name) buffers-to-save))))
    (when (eq 1 (length buffers-to-save))
      (add-hook 'after-save-hook 'sgm:reload-ghci))))

(defun sgm:reload-ghci ()
  (remove-hook 'after-save-hook 'sgm:reload-ghci)
  (let ((mode-buffer (sgm:get-mode-buffer)))
    (when mode-buffer
      (let ((process (get-buffer-process mode-buffer)))
        (when process
          (let* ((command (process-command process))
                 (is-stack (nth 0 command))
                 (is-ghci (nth 1 command)))
            (when (and
                   (equal is-stack "stack")
                   (equal is-ghci "ghci"))
              (with-current-buffer mode-buffer
                (ignore-errors (compilation-forget-errors))
                (comint-clear-buffer)
                (sgm:repl-command ":r")
                  (when (and sgm:on-reload-command
                             (not (equal sgm:on-reload-command "")))
                    (sgm:repl-command sgm:on-reload-command))))))))))

(defun sgm:get-mode-buffer ()
  (let ((mode-buffers (sgm:mode-buffers))
        (root (sgm:find-root)))
    (when root
      (seq-find (lambda (buffer) (string-match root (buffer-name buffer))) mode-buffers))))

(add-hook 'haskell-mode-hook (lambda () (add-hook 'before-save-hook 'sgm:check-modified-buffers)))

(defun sgm:mode-p ()
  "Return non-nil if the current buffer is sgm mode buffer"
  (derived-mode-p 'simple-ghci-mode))

(defun sgm:switch-to-ghci-buffer ()
  (interactive)

  (sgm:switch-to-ghci-buffer-callbacks (lambda (program-name program-params buffer-name project-root)
                                         (sgm:insert-text-to-buffer program-name program-params project-root)
                                         (comint-exec (current-buffer) buffer-name program-name nil program-params))))

(defun sgm:switch-to-ghci-buffer-callbacks (on-no-process &optional always-execute)
  (let ((ghci-buffer (sgm:get-mode-buffer))
        (on-process-running (lambda ()
                              (sgm:stack-quit on-no-process))))
    (if (get-buffer-process ghci-buffer)
        (let ((cb (current-buffer)))
          (unless (equal cb ghci-buffer)
            (switch-to-buffer-other-window ghci-buffer))
          (when always-execute (funcall on-process-running)))
      (sgm:run-ghci (lambda (program-name program-params buffer-name project-root)
                      (funcall on-no-process program-name program-params buffer-name project-root))))))

(defun sgm:mode-buffers ()
  (cl-loop for buffer being the elements of (buffer-list)
           when (with-current-buffer buffer (sgm:mode-p))
           collect buffer into sgm-mode-buffers
           finally return sgm-mode-buffers))

(defun sgm:symbol-or-selection-at-point ()
  (if (use-region-p)
     (buffer-substring (region-beginning) (region-end))
    (thing-at-point 'symbol)))

(defhydra sgm:hydra ()
  "
Search for _a_ repeat _l_ load _t_ type _i_ info _I_ info full _d_ doc _h_ hoogle _s_ repl _D_ DataKinds _n_ no-type-defaults _C_ clean _c_ compile _p_ pedantic _m_ modules _q_ quit"
  ("a" (sgm:repeat-last) nil)
  ("l" (sgm:load-current-file) nil)
  ("s" (sgm:switch-to-ghci-buffer) nil)
  ("d" (sgm:show-doc "doc") nil)
  ("h" (sgm:show-doc "hoogle") nil)
  ("D" (sgm:activate-extension "DataKinds") nil)
  ("n" (sgm:activate-warning "no-type-defaults") nil)
  ("C" (sgm:stack-clean) nil)
  ("c" (sgm:stack-compile) nil)
  ("p" (sgm:stack-compile-pedantic) nil)
  ("t" (sgm:type-for-thing-at-point) nil)
  ("i" (sgm:definition-for-thing-at-point 'accumulate) nil)
  ("I" (sgm:definition-for-thing-at-point 'back-reference) nil)
  ("m" (sgm:load-modules) nil)
  ("q" nil nil :color blue))

(defun sgm:load-modules ()
  (sgm:run-repl-command ":show modules"))

(defun sgm:type-for-thing-at-point ()
  (sgm:run-repl-command (format ":type %s" (sgm:symbol-or-selection-at-point))))

(defun sgm:definition-for-thing-at-point (flag)
  (xref-push-marker-stack)
  (let ((file-name (buffer-file-name))
        (buffer-name (buffer-name))
        (column (current-column))
        (line (line-number-at-pos)))
    (sgm:run-repl-command (format ":info %s" (sgm:symbol-or-selection-at-point))
                          (lambda ()
                            (setq sgm:preoutput-filter-processed-info
                                  (if file-name
                                      (format "-- Searched from %s:%s:%s\n " (abbreviate-file-name file-name) line column)
                                    (format "-- Searched from %s\n " buffer-name)))
                            (setq sgm:preoutput-filter-print-definition-only flag)))))

(defun sgm:activate-extension (ext)
  (sgm:run-repl-command (format ":set -X%s" ext)))

(defun sgm:activate-warning (ext)
  (sgm:run-repl-command (format ":set -W%s" ext)))

(defun sgm:run-repl-command (repl-cmd &optional pre-action)
  (sgm:switch-to-ghci-buffer)
  (when (functionp pre-action) (funcall pre-action))
  (message "Running %s" repl-cmd)
  (sgm:repl-command repl-cmd))

(defun sgm:repeat-last ()
  (sgm:switch-to-ghci-buffer)
  (unless (ring-empty-p comint-input-ring)
    (let ((head (ring-ref comint-input-ring 0)))
      (sgm:repl-command head))))

(defun sgm:load-current-file ()
  (let ((file-to-load buffer-file-name))
    (message "Loading %s file" file-to-load)
    (sgm:switch-to-ghci-buffer)
    (sgm:repl-command (format ":l %s" file-to-load))))

(defun sgm:show-doc (command)
  (let ((symbol (symbol-at-point)))
    (sgm:switch-to-ghci-buffer)
    (end-of-buffer)
    (move-end-of-line 1) ;; end-of-buffer is not enough to keep point at end of buffer with new output
    (sgm:repl-command (format ":%s %s" command symbol))
    (other-window 1)))

(defun sgm:callback-factory (callback)
  `(lambda (process event)
     (when (memq (process-status process) '(signal exit))
       (funcall (quote ,callback)))))

(defun sgm:stack-clean ()
  (sgm:switch-to-ghci-buffer-callbacks (sgm:compile-cmd '("clean")) t))

(defun sgm:stack-compile ()
  (sgm:switch-to-ghci-buffer-callbacks (sgm:compile-cmd '("build")) t))

(defun sgm:stack-compile-pedantic ()
  (sgm:switch-to-ghci-buffer-callbacks (sgm:compile-cmd '("build" "--pedantic")) t))

(defun sgm:stack-quit (callback)
  (let ((process (get-process (sgm:buffer-name))))
    (set-process-sentinel process (sgm:callback-factory callback))
    (sgm:repl-command ":q")))

(defun sgm:compile-cmd (params)
  `(lambda (&optional program-name program-params buffer-name project-root)
     (sgm:stack-command "stack" (append (quote ,params) sgm:extra-program-params) (lambda () (message "Compilation done.")))))

(defun sgm:stack-command (command command-params on-finish)
  (let ((buffer-name (sgm:buffer-name)))

    (sgm:insert-text-to-buffer command command-params default-directory)

    (let ((buf-name (comint-exec buffer-name buffer-name command nil command-params)))
      (set-process-sentinel (get-process buf-name) (sgm:callback-factory on-finish)))))

(defun sgm:run-hydra ()
  (interactive)
  (sgm:hydra/body))

(defun sgm:insert-text-to-buffer (command command-params dir-name)
  (let ((inhibit-read-only t))
    (insert (format "Running %s %s, in %s\n" command (mapconcat 'identity command-params " ") dir-name))))

(defun sgm:repl-command (command)
  (comint-add-to-input-history command)
  (comint-send-string (current-buffer) (concat command "\n")))

(provide 'simple-ghci-mode)
;;; simple-ghci-mode.el ends here
