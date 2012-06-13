;;;; Modes

;; zencoding for html
;; (load-lib-dir "zencoding-mode")
;; (require 'zencoding-mode)
;; (add-hook 'sgml-mode-hook 'zencoding-mode) ;; Auto-start on any markup modes

;; scss
(add-to-list 'load-path (expand-file-name "~/.emacs.d/folder-where-you-put-scss-mode-el"))
(autoload 'scss-mode "scss-mode")
(add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))

;; markdown
(require 'markdown-mode)
(add-to-list 'auto-mode-alist '("\.md$" . markdown-mode))

;; slim
(require 'slim-mode)

;; yasnippet
(load-lib-dir "yasnippet")
(require 'yasnippet)
(setq yas/root-directory (concat root-dir "yasnippet/snippets"))
(yas/load-directory yas/root-directory)
(yas/initialize)

;; feature
;; (load-lib-dir "feature-mode")
;; (require 'feature-mode)
;; (add-to-list 'auto-mode-alist '("\.feature$" . feature-mode))

;; css
(autoload 'css-mode "css-mode" "Mode for editing CSS files" t)
(add-to-list 'auto-mode-alist '("\\.css$" . css-mode))
(autoload 'rainbow-mode "rainbow-mode")

;; ruby
(autoload 'ruby-mode "ruby-mode" "Major mode for ruby files" t)
(setq auto-mode-alist (append '(("\\.rb$"   . ruby-mode)
                                ("\\.rake$"   . ruby-mode)
                                ("\\.builder$" . ruby-mode)
                                ("Rakefile"   . ruby-mode)
                                ("Capfile"   . ruby-mode)
                                ("Gemfile"   . ruby-mode)
				) auto-mode-alist))

;; rhtml
(load-lib-dir "rhtml-mode")
(require 'rhtml-mode)
;; (add-hook 'rhtml-mode-hook (lambda () (rinari-launch)))

;; yaml
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

;; coffee
(add-to-list 'load-path "~/.emacs.d/coffee-mode")
(require 'coffee-mode)
(add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))
(add-to-list 'auto-mode-alist '("Cakefile" . coffee-mode))
(defun coffee-custom ()
  "coffee-mode-hook"
  (set (make-local-variable 'tab-width) 2))

(add-hook 'coffee-mode-hook
	  '(lambda() (coffee-custom)))

;; php
(require 'php-mode)
(add-hook 'php-mode-hook
	  '(lambda () (define-abbrev php-mode-abbrev-table "ex" "extends")))
(defun my-php-mode-common-hook ()
  ;; my customizations for php-mode
  (setq tab-width 4)
  (setq c-basic-offset 4)
  (c-set-offset 'topmost-intro-cont 4)
  (c-set-offset 'class-open 0)
  (c-set-offset 'inline-open 0)
  (c-set-offset 'substatement-open 0)
  (c-set-offset 'arglist-intro '+)
  )

;; io
(autoload 'io-mode "io-mode" "Mode for editing Io files" t)
(add-to-list 'auto-mode-alist '("\\.io$" . io-mode))

;; haml
(require 'haml-mode)
(setq haml-mode-hook (function (lambda () (setq indent-tabs-mode nil))))

;; color-themes
(add-to-list 'load-path "~/.emacs.d/color-theme-6.6.0/color-theme.el")
(require 'color-theme)
(add-to-list 'load-path "~/.emacs.d/emacs-color-theme-solarized/")
(require 'color-theme-solarized)

;; scala
(add-to-list 'load-path "~/.emacs.d/scala-mode")
(require 'scala-mode-auto)
(add-hook 'scala-mode-hook
			 '(lambda ()
				 (yas/minor-mode-on)
				 ))
