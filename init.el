;;; Emacs Init file for Oshal Borkar


;; Basic Startup changes
(tool-bar-mode -1) ; Hides the tool bar
(scroll-bar-mode -1) ; Hides scroll bar
(global-display-line-numbers-mode 1) ; display line numbers
(setq ring-bell-function 'ignore) ; Removes the annoying alert sound
(custom-set-variables '(default-frame-alist '((fullscreen . maximized)))) ; Sets the buffer to use fullscreen

;; Bootstrap script for Straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))


;; List of packages installed via Straight.el
(straight-use-package 'use-package)

;; Use-Package via Straight.el
(use-package el-patch
  :straight t)
;(use-package meow
;  :straight t)


(add-to-list 'default-frame-alist '(font . "Source Code Pro-18")) ; Set the font to Source Code Pro with size 18
