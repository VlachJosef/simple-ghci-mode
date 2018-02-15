;;; simple-ghci-mode.el --- Simple ghci mode

;; Copyright (c) 2018 Josef Vlach

;; Homepage: https://github.com/VlachJosef/simple-ghci-mode
;; Package-Version:  0.1

;;; Commentary:
;;
;;; Code:

(defcustom sgm:program-name "stack"
  "Program invoked by the `sgm:run-ghci' command."
  :type 'string
  :group 'ghci)

(defcustom sgm:buffer-name-base "*ghci*"
  "Buffer name for ghci"
  :type 'string
  :group 'ghci)

(defvar-local sgm:stack-params nil ;; dir-local
  "Targets used for the stack process.")

(defvar-local sgm:stack-on-reload-command nil ;; dir-local
  "Command to run on save of a file")

(defvar sgm:buffer-project-root nil)

;; Make `sgm:stack-params' safe if its value is list of string values
;; For example:
;; ((simple-ghci-mode
;;   (sgm:stack-params . ("ghci" "--test" "crypto-ledger:lib" "crypto-ledger:crypto-ledger-test"))
;; ))
(put 'sgm:stack-params 'safe-local-variable
     (lambda (project)
       (sgm:is-list-of-strings project)))

;; Make `sgm:stack-on-reload-command' safe if its value is a string
;; For example:
;; ((simple-ghci-mode
;;  (sgm:stack-on-reload-command . "Main.main")
;; ))
(put 'sgm:stack-on-reload-command 'safe-local-variable
     (lambda (command)
       (stringp command)))

(defun sgm:is-list-of-strings (list)
  "Check if `list' is list containing only strings"
  (null (memq nil (mapcar (lambda (item)
                            (stringp item)) list))))

(defun sgm:find-root ()
  "Starting from the current default-directory, find a parent
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
           (sgm:find-root-impl name-or-pred parent dir))
          ('t
           (sgm:find-root-impl name-or-pred parent best-root)))))

(defun sgm:buffer-name ()
  "Return the buffer name for running ghci."
  (format "%s<%s>"
          sgm:buffer-name-base
          (abbreviate-file-name (sgm:find-root))))

(defun sgm:ghci-start () "Start ghci" (interactive) (sgm:run-ghci t))

(defun sgm:run-ghci (&optional pop-p)
  (let* ((project-root (or (sgm:find-root)
	         	   (error "Could not find project root, type `C-h f sgm:find-root` for help.")))
         (buffer-name (sgm:buffer-name)))

    (when (not (or (executable-find sgm:program-name)))
      (error "Could not find %s on PATH. Please customize the sgm:program-name variable." sgm:program-name))

    ;; start new ghci
    (with-current-buffer (get-buffer-create buffer-name)
      (when pop-p (pop-to-buffer-same-window (current-buffer)))
      (unless (comint-check-proc (current-buffer))
        (unless (derived-mode-p 'simple-ghci-mode) (simple-ghci-mode))
        (cd project-root)
        (message "Starting ghci in buffer %s " buffer-name)
        (insert (concat "Running " sgm:program-name "\n"))
        (hack-dir-local-variables-non-file-buffer)
        (comint-exec (current-buffer) buffer-name sgm:program-name nil sgm:stack-params))
      (current-buffer))))

(defvar sgm:mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map
                       (make-composed-keymap compilation-shell-minor-mode-map
                                             comint-mode-map))
    ;;(define-key map (kbd "C-c C-j") 'some-command)
    map)
  "Basic mode map for `sgm-start'")

(define-derived-mode simple-ghci-mode comint-mode "sgm"
  "Major mode for `ghci-start'.

  \\{sgm:mode-map}"
  (use-local-map sgm:mode-map)
  ;;(ignore-errors (scala-mode:set-scala-syntax-mode))
  (add-hook 'simple-ghci-mode-hook 'sgm:initialize-for-comint-mode)
  (add-hook 'simple-ghci-mode-hook 'sgm:initialize-for-compilation-mode))

(defun sgm:require-buffer ()
  "Throw error if buffer the current buffer is not an ghci-buffer"
  (unless (derived-mode-p 'simple-ghci-mode)
    (error "Current buffer %s is not an simple-ghci-mode buffer" (current-buffer))))

(defun sgm:initialize-for-comint-mode ()
  (sgm:require-buffer)
  (when (derived-mode-p 'comint-mode)
    (setq comint-process-echoes t)
    (setq comint-scroll-to-bottom-on-output nil)
    (setq-local comint-use-prompt-regexp t)
    (setq-local comint-prompt-read-only t)
    (setq-local comint-buffer-maximum-size 4096)
    (setq-local comint-output-filter-functions '(comint-postoutput-scroll-to-bottom))))

(defun sgm:initialize-for-compilation-mode ()
  (setq-local
   compilation-error-regexp-alist
   '(("^\\(.*.hs\\):\\([[:digit:]]+\\):\\([[:digit:]]+\\): error:" 1 2 3 2 1)
     ("^  \\(.*.hs\\):\\([[:digit:]]+\\): " 1 2 3 2 1) ;; Hspec failures
     ("^\\(.*.hs\\):\\([[:digit:]]+\\):\\([[:digit:]]+\\): warning:" 1 2 3 1 1)))
  (setq-local compilation-mode-font-lock-keywords nil)
  (compilation-setup t))

(defun sgm:reload-ghci ()
  (when (sgm:switch-to-ghci-buffer)
    (ignore-errors (compilation-forget-errors))
    (comint-clear-buffer)
    (comint-send-string (current-buffer) ":r\n")
    (comint-send-string (current-buffer) (format "%s\n" sgm:stack-on-reload-command))))

(add-hook 'haskell-mode-hook (lambda () (add-hook 'after-save-hook 'sgm:reload-ghci)))

(defun sgm:mode-p ()
  "Return non-nil if the current buffer is sgm mode buffer"
  (derived-mode-p 'simple-ghci-mode))

(defun sgm:switch-to-ghci-buffer ()
  (interactive)
  (let ((root-and-buffers
         (cl-loop for process being the elements of (process-list)
                  for current-process-buffer = (process-buffer process)
                  when (and
                        (equal (process-status process) 'run) ;; proces must be running
                        (bufferp current-process-buffer) ;; process must have associated buffer
                        (buffer-live-p current-process-buffer) ;; buffer must not be killed
                        (with-current-buffer current-process-buffer
                          (and
                           (sgm:mode-p)
                           (process-live-p process))))
                  collect (with-current-buffer current-process-buffer
                            current-process-buffer) into file-buffers
                            finally return file-buffers)))

    (if root-and-buffers (switch-to-buffer-other-window (car root-and-buffers))
      (progn
        (message "No ghci buffer found.")
        nil))))

(provide 'simple-ghci-mode)
;;; simple-ghci-mode.el ends here
