(defvar root-dir "~/.emacs.d/")
(defun load-lib (name) (load (concat root-dir name ".el")))
(defun load-lib-dir (path) (add-to-list 'load-path (concat root-dir path)))
(load-lib-dir ".")
(load-lib-dir "./lib")


;;;; package management
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

;; simple way to synchronize a bunch of packages across installs.
;;   (there are more sophisticated ways to do this, like pallet
;;    or el-get, but this seems to work fine for now)
(defvar my-packages '(ag
                      applescript-mode
                      auto-complete
                      clojure-mode
                      clojure-test-mode
                      coffee-mode
                      css-mode
                      expand-region
                      haml-mode
                      highlight-indentation
                      markdown-mode
                      nrepl
                      php-mode
                      rainbow-mode
                      robe
                      smartparens
                      scala-mode2
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
(global-set-key (kbd "C-c k") 'ack)

;; Save squiggle files somewhere out of the way
(setq backup-directory-alist '(("." . "~/.saves")))

(load-lib "modes")

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
 )

;; stuff from zenspider
(require 'bs)
(global-set-key (kbd "C-x C-b") 'bs-show) ; better buffer listings
(global-set-key (kbd "M-s")     'fixup-whitespace) ; best function ever

(require 'ruby-mode)
(require 'rcodetools)
(define-key ruby-mode-map (kbd "C-c C-z") 'xmp)




;; DO THIS LAST

;; load .emacs_local.el, if present
(if (file-exists-p (setq local-init-file "~/.emacs_local.el"))
    (load local-init-file))

;; DO NOT ADD ANYTHING BELOW THIS
