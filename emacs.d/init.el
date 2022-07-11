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
                      ;; auto-complete
                      coffee-mode
                      css-mode
                      ;; dropdown-list
                      expand-region
                      flycheck
                      highlight-indentation
                      js2-mode
                      json-mode
                      markdown-mode
                      multiple-cursors
                      projectile
                      rainbow-mode
                      robe
                      rvm
                      scss-mode
                      slim-mode
                      string-inflection
                      web-mode
                      yaml-mode
                      yasnippet))

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

;; Save squiggle files somewhere out of the way
(defconst emacs-tmp-dir "~/.emacs_saves/")
(make-directory emacs-tmp-dir t)
(setq backup-directory-alist
      `((".*" . ,emacs-tmp-dir)))
(setq auto-save-file-name-transforms
      `((".*" ,emacs-tmp-dir t)))

(load-lib "modes")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(coffee-tab-width 2)
 '(custom-safe-themes
   (quote
    ("235dc2dd925f492667232ead701c450d5c6fce978d5676e54ef9ca6dd37f6ceb" "71ecffba18621354a1be303687f33b84788e13f40141580fa81e7840752d31bf" "8fed5e4b89cf69107d524c4b91b4a4c35bcf1b3563d5f306608f0c48f580fdf8" "90edd91338ebfdfcd52ecd4025f1c7f731aced4c9c49ed28cfbebb3a3654840b" "f0a99f53cbf7b004ba0c1760aa14fd70f2eabafe4e62a2b3cf5cabae8203113b" "8ac31e1bc1920b33d478dfafb0b45989a00ede15a2388ea16093e7d0988c48d0" "60f04e478dedc16397353fb9f33f0d895ea3dab4f581307fbf0aa2f07e658a40" "16248150e4336572ff4aa21321015d37c3744a9eb243fbd1e934b594ff9cf394" "1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" default)))
 '(package-selected-packages
   (quote
    (vue-mode vue-html-mode typescript-mode pianobar projectile yaml-mode web-mode string-inflection slim-mode scss-mode rvm robe rainbow-mode multiple-cursors markdown-mode magit json-mode js2-mode highlight-indentation flycheck expand-region ensime dropdown-list coffee-mode auto-complete applescript-mode ag)))
 '(safe-local-variable-values (quote ((encoding . utf-8)))))
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

(savehist-mode 1)

(global-set-key (kbd "C-c r") 'revert-buffer)

;; magit
(global-set-key (kbd "C-x g") 'magit-status)

(eval-after-load "compile"
  '(progn
     ;; For eslint:
     (add-to-list 'compilation-error-regexp-alist
		  '("^\\(/[^\":\n]+\\)\n *\\([0-9]+\\):[0-9]+ +\\(error\\|warning\\) +" 1 2))))

;; added this to enable debugging in rspec buffers.
(add-hook 'after-init-hook 'inf-ruby-switch-setup)

;; DO THIS LAST

;; load .emacs_local.el, if present
(if (file-exists-p (setq local-init-file "~/.emacs_local.el"))
    (load local-init-file))

;; DO NOT ADD ANYTHING BELOW THIS
