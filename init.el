;;; Init file for Oshal Borkar

;; ------------------------
;; Basic visual / frame defaults
;; ------------------------
;; Disable UI clutter
(tool-bar-mode -1) ; Hides the tool bar
(scroll-bar-mode -1) ; Hides scroll bar
(global-display-line-numbers-mode 1) ; display line numbers
(setq ring-bell-function 'ignore) ; Removes the annoying alert sound
(custom-set-variables '(default-frame-alist '((fullscreen . maximized)))) ; Sets the buffer to use fullscreen
  ;(set-frame-parameter nil 'alpha 60)
;(add-to-list 'default-frame-alist '(alpha . 60))

;; ------------------------
;; Bootstrap straight.el (canonical version)
;; ------------------------
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
(setq straight-use-package-by-default t)

;; Use-Package via Straight.el
(use-package el-patch)
(use-package meow)
(use-package auctex
  :straight t)
(use-package sr-speedbar
  :straight t)
(use-package magit
  :straight t)
(use-package vterm
  :straight t)
(use-package markdown-mode)

(add-to-list 'default-frame-alist
	     '(font . "Source Code Pro-18")) ; Set the font to Source Code Pro with size 18 after loading packages

;; Meow Keybindings

(defun meow-setup ()
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
  (meow-motion-overwrite-define-key
   '("j" . meow-next)
   '("k" . meow-prev)
   '("<escape>" . ignore))
  (meow-leader-define-key
   ;; Use SPC (0-9) for digit arguments.
   '("1" . meow-digit-argument)
   '("2" . meow-digit-argument)
   '("3" . meow-digit-argument)
   '("4" . meow-digit-argument)
   '("5" . meow-digit-argument)
   '("6" . meow-digit-argument)
   '("7" . meow-digit-argument)
   '("8" . meow-digit-argument)
   '("9" . meow-digit-argument)
   '("0" . meow-digit-argument)
   '("/" . meow-keypad-describe-key)
   '("?" . meow-cheatsheet)
   '("f" . find-file)
   '("s" . save-buffer)
   '("qq" . kill-emacs)
   '("SPC" . (lambda () (interactive) (call-interactively 'execute-extended-command)))
   '("bs" . switch-to-buffer)
   '("bk" . kill-buffer)
   '("qf" . delete-frame)
   '("g" . magit-status))
  (meow-normal-define-key
   '("0" . meow-expand-0)
   '("9" . meow-expand-9)
   '("8" . meow-expand-8)
   '("7" . meow-expand-7)
   '("6" . meow-expand-6)
   '("5" . meow-expand-5)
   '("4" . meow-expand-4)
   '("3" . meow-expand-3)
   '("2" . meow-expand-2)
   '("1" . meow-expand-1)
   '("-" . negative-argument)
   '(";" . meow-reverse)
   '("," . meow-inner-of-thing)
   '("." . meow-bounds-of-thing)
   '("[" . meow-beginning-of-thing)
   '("]" . meow-end-of-thing)
   '("a" . meow-append)
   '("A" . meow-open-below)
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("c" . meow-change)
   '("d" . meow-delete)
   '("D" . meow-backward-delete)
   '("e" . meow-next-word)
   '("E" . meow-next-symbol)
   '("f" . meow-find)
   '("g" . meow-cancel-selection)
   '("G" . meow-grab)
   '("h" . meow-left)
   '("H" . meow-left-expand)
   '("i" . meow-insert)
   '("I" . meow-open-above)
   '("j" . meow-next)
   '("J" . meow-next-expand)
   '("k" . meow-prev)
   '("K" . meow-prev-expand)
   '("l" . meow-right)
   '("L" . meow-right-expand)
   '("m" . meow-join)
   '("n" . meow-search)
   '("o" . meow-block)
   '("O" . meow-to-block)
   '("q" . meow-quit)
   '("Q" . meow-goto-line)
   '("r" . meow-replace)
   '("R" . meow-swap-grab)
   '("s" . meow-kill)
   '("t" . meow-till)
   '("u" . meow-undo)
   '("U" . meow-undo-in-selection)
   '("v" . meow-visit)
   '("w" . meow-mark-word)
   '("W" . meow-mark-symbol)
   '("x" . meow-line)
   '("X" . meow-goto-line)
   '("y" . meow-save)
   '("Y" . meow-sync-grab)
   '("z" . meow-pop-selection)
   '("'" . repeat)
   '("<escape>" . ignore))
  (meow-define-keys 'insert
   '("C-[" . meow-insert-exit)))

(meow-setup)
(meow-global-mode 1)


;; Speedbar settings
(setq speedbar-show-unknown-files t)
(setq speedbar-directory-unshown-regexp "^$")

;; ===== Centralize Backups and Auto-Saves =====

;; Backup files (~)
(setq backup-directory-alist `(("." . "~/.cache/emacs/backups")))
(setq version-control t      ;; Use version numbers for backups
      kept-new-versions 10
      kept-old-versions 2
      delete-old-versions t
      backup-by-copying t)   ;; Don’t clobber symlinks

;; Auto-save files (#...#)
(setq auto-save-file-name-transforms
      `((".*" "~/.cache/emacs/auto-saves/" t)))
(setq auto-save-default t
      auto-save-timeout 5   ;; seconds idle before auto-save
      auto-save-interval 20 ;; keystrokes before auto-save
)

;; Create the directories if they don't exist
(make-directory "~/.cache/emacs/backups/" t)
(make-directory "~/.cache/emacs/auto-saves/" t)
