(defvar root-dir "~/.emacs.d/")
(defun load-lib (name) (load (concat root-dir name ".el")))
(defun load-lib-dir (path) (add-to-list 'load-path (concat root-dir path)))
(load-lib-dir ".")

;;;; from: http://clojure-doc.org/articles/tutorials/emacs.html
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

;;;; newfangled way of installing packages.
(defvar my-packages '(applescript-mode
                      clojure-mode
                      clojure-test-mode
                      css-mode
                      haml-mode
                      markdown-mode
                      nrepl
                      rainbow-mode
                      scss-mode
                      slim-mode
                      yaml-mode))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;;;; Setup
(menu-bar-mode 0)
(setq inhibit-startup-echo-area-message t)
(setq inhibit-startup-message t)
(setq initial-major-mode (quote text-mode))
(setq initial-scratch-message nil)
(setq-default indent-tabs-mode nil)

;;;; Server Mode
(server-mode)

;;;; Hooks
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;;; Shortcuts
(global-set-key (kbd "M-g") 'goto-line)
(global-set-key (quote [27 up]) (quote scroll-down))
(global-set-key (quote [27 down]) (quote scroll-up))
(global-set-key (kbd "M-p") (lambda () (interactive) (scroll-down 1)))
(global-set-key (kbd "M-n") (lambda () (interactive) (scroll-up 1)))
(global-set-key (kbd "M-#") 'linum-mode)
(global-set-key (kbd "M-+") 'ido-mode)

;; ack
(require 'ack)
(global-set-key (kbd "C-c k") 'ack)

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
 '(erb-delim-face ((((class color) (min-colors 88) (background light)) (:foreground "red"))))
 '(font-lock-function-name-face ((((class color) (min-colors 88) (background light)) (:foreground "brightblue"))))
 '(font-lock-keyword-face ((((class color) (min-colors 88) (background light)) (:foreground "Orange"))))
 '(font-lock-string-face ((((class color) (min-colors 88) (background light)) (:foreground "green")))))

;; load .emacs_local.el, if present
(if (file-exists-p (setq local-init-file "~/.emacs_local.el"))
    (load local-init-file))

;; stuff from zenspider
(require 'bs)
(global-set-key (kbd "C-x C-b") 'bs-show) ; better buffer listings
(global-set-key (kbd "M-s")     'fixup-whitespace) ; best function ever

;; rcodetools -- see http://rubygems.org/gems/rcodetools
(require 'rcodetools)
(define-key ruby-mode-map (kbd "C-c C-z") 'xmp)

;; helm -- see https://github.com/emacs-helm/helm

(add-to-list 'load-path "~/.emacs.d/helm/")
(require 'helm-config)
(global-set-key (kbd "C-c h") 'helm-mini)
;; (helm-mode 1) -- NOTE: enable this for helm-mode in 'M-x', 'C-x C-f', etc.
