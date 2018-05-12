;;; sbt-or-ghci.el --- Simple ghci mode

;; Copyright (c) 2018 Josef Vlach

;; Homepage: https://github.com/VlachJosef/simple-ghci-mode
;; Package-Version:  0.1

;;; Commentary:
;;
;;; Code:

(require 'simple-ghci-mode)
(require 'sbt-mode-buffer)

(defun sbt-or-sgm:mode-buffers ()
  (cl-loop for buffer being the elements of (buffer-list)
           when (with-current-buffer buffer
                  (or (sgm:mode-p)
                      (sbt:mode-p)))
           collect buffer into mode-buffers
           finally return mode-buffers))

(defun sbt-or-sgm:sbt-or-ghci ()
  "Find all sbt or ghci processes and switch to best candidate buffer or if no best candidate can be decided let user select."
  (interactive)
  (let* ((all-buffers (sbt-or-sgm:mode-buffers))
         (invoked-from-directory (expand-file-name default-directory))
         (best-candidate (seq-find (lambda (buf)
                                     (with-current-buffer buf
                                       (string-prefix-p (expand-file-name default-directory) invoked-from-directory))) all-buffers))
         (other-candidates (seq-filter (lambda (buf)
                                         (with-current-buffer buf
                                           (string-prefix-p invoked-from-directory (expand-file-name default-directory)))) all-buffers)))

    (if (and best-candidate
             (not (equal (current-buffer) best-candidate)))
        (switch-to-buffer-other-window best-candidate)
      (pcase (length other-candidates)
        (0 (if (null all-buffers)
               (message "No SBT or GHCi buffer found." )
             (sbt-or-sgm:select-from all-buffers)))
        (1 (let ((only-candidate (car other-candidates)))
             (if (not (equal (current-buffer) only-candidate))
                 (switch-to-buffer-other-window only-candidate)
               (message "Already in %s buffer" only-candidate))))
        (_ (sbt-or-sgm:select-from other-candidates))))))

(defun sbt-or-sgm:select-from (all-buffers)
  (let ((buffers (mapcar 'buffer-name all-buffers)))
    (switch-to-buffer-other-window
     (cond ((fboundp 'ivy-read)
            (ivy-read "GHCi or SBT buffer: " buffers))
           ((fboundp 'ido-completing-read)
            (ido-completing-read "GHCi or SBT buffer: " buffers))
           (t
            (completing-read "GHCi or SBT buffer: (hit TAB to auto-complete): " buffers nil t))))))

(provide 'sbt-or-ghci)
;;; sbt-or-ghci.el ends here
