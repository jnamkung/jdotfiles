(defvar root-dir "~/.emacs.d/")
(defun load-lib (name) (load (concat root-dir name ".el")))
(defun load-lib-dir (path) (add-to-list 'load-path (concat root-dir path)))
(load-lib-dir ".")

;;;; Setup
(menu-bar-mode 0)
(setq inhibit-startup-echo-area-message t)
(setq inhibit-startup-message t)
(setq initial-major-mode (quote text-mode))
(setq initial-scratch-message nil)

;;;; Hooks
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;;; Shortcuts
(global-set-key (kbd "M-g") 'goto-line)
(global-set-key (quote [27 up]) (quote scroll-down))
(global-set-key (quote [27 down]) (quote scroll-up))
(global-set-key (kbd "M-p") (lambda () (interactive) (scroll-down 1)))
(global-set-key (kbd "M-n") (lambda () (interactive) (scroll-up 1)))

;; ack
(require 'ack)

;; Save squiggle files somewhere out of the way
(setq backup-directory-alist '(("." . "~/.saves")))

(load-lib "modes")

;; make color tweaks available via interactive function
(defun dotfiles-facefixes ()
  "Apply the color adjustments that someone set via customise (http://xahlee.org/emacs/emacs_custom_system.html) and then checked into dotfiles"
  (interactive)
  ;; cut-and-paste-ish from (customize-set-faces) below, made available as a function
   (set-face-foreground 'font-lock-string-face "green")
   (set-face-foreground 'font-lock-keyword-face "Orange")
   (set-face-foreground 'font-lock-function-name-face "brightblue")
   (set-face-foreground 'erb-face "red"))

;; load .emacs_local.el, if present
(if (file-exists-p (setq local-init-file "~/.emacs_local.el"))
    (load local-init-file))

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(erb-face ((((class color) (min-colors 88) (background light)) (:foreground "red"))))
 '(font-lock-function-name-face ((((class color) (min-colors 88) (background light)) (:foreground "brightblue"))))
 '(font-lock-keyword-face ((((class color) (min-colors 88) (background light)) (:foreground "Orange"))))
 '(font-lock-string-face ((((class color) (min-colors 88) (background light)) (:foreground "green")))))
