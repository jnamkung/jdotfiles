(defvar root-dir "~/.emacs.d/")
(defun load-lib (name) (load (concat root-dir name ".el")))
(defun load-lib-dir (path) (add-to-list 'load-path (concat root-dir path)))
(load-lib-dir ".")
(load-lib-dir "yasnippet")
(load-lib-dir "feature-mode")

;;;; Setup
(menu-bar-mode 0)

;;;; Hooks
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;;; Shortcuts
(global-set-key "\M-g" 'goto-line)

;;;; Modes

;; markdown
(require 'markdown-mode)
(add-to-list 'auto-mode-alist '("\.md$" . markdown-mode))

;; yasnippet
(require 'yasnippet)
(setq yas/root-directory (concat root-dir "yasnippet/snippets"))
(yas/load-directory yas/root-directory)

;; feature
(require 'feature-mode)
(add-to-list 'auto-mode-alist '("\.feature$" . feature-mode))

;; html
(setq auto-mode-alist (append '(("\\.phtml$" . html-mode)
				("\\.php$" . html-mode)
				) auto-mode-alist))
;; css
(autoload 'css-mode "css-mode" "Mode for editing CSS files" t)
(add-to-list 'auto-mode-alist '("\\.css$" . css-mode))
(autoload 'rainbow-mode "rainbow-mode")

;; js
(autoload 'espresso-mode "espresso" nil t)
(setq auto-mode-alist (append '(("\\.js\\'" . espresso-mode)
				("\\.json$" . espresso-mode)
				("\\.js.erb$" . espresso-mode)
				) auto-mode-alist))

;; ruby
(autoload 'ruby-mode "ruby-mode" "Major mode for ruby files" t)
(setq auto-mode-alist (append '(("\\.rb$"   . ruby-mode)
                                ("\\.rake$"   . ruby-mode)
                                ("\\.builder$" . ruby-mode)
                                ("Rakefile"   . ruby-mode)
                                ("Capfile"   . ruby-mode)
                                ("Gemfile"   . ruby-mode)
				) auto-mode-alist))



;; yaml
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

;; coffee
(add-to-list 'load-path "~/.emacs.d/coffee-mode")
(require 'coffee-mode)

;; If coffee-mode is not enabled automatically for any files ending in
;; ".coffee" or named "Cakefile", add this to your emacs config as
;; well:

(add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))
(add-to-list 'auto-mode-alist '("Cakefile" . coffee-mode))

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


;; scheme
;; (require 'quack)
;; (custom-set-variables
;;   ;; custom-set-variables was added by Custom.
;;   ;; If you edit it by hand, you could mess it up, so be careful.
;;   ;; Your init file should contain only one such instance.
;;   ;; If there is more than one, they won't work right.
;;  '(gud-gdb-command-name "gdb --annotate=1")
;;  '(large-file-warning-threshold nil)
;;  '(quack-programs (quote ("/Applications/mit-scheme.app/Contents/Resources/mit-scheme" "/Applications/mit-scheme.app" "bigloo" "csi" "csi -hygienic" "gosh" "gsi" "gsi ~~/syntax-case.scm -" "guile" "kawa" "mit-schem" "mit-scheme" "mred -z" "mzscheme" "mzscheme -M errortrace" "mzscheme -il r6rs" "mzscheme -il typed-scheme" "mzscheme3m" "mzschemecgc" "rs" "scheme" "scheme48" "scsh" "sisc" "stklos" "sxi"))))
;; (custom-set-faces
;;   ;; custom-set-faces was added by Custom.
;;   ;; If you edit it by hand, you could mess it up, so be careful.
;;   ;; Your init file should contain only one such instance.
;;   ;; If there is more than one, they won't work right.
;;  )


;; Save squiggle files somewhere out of the way
(setq backup-directory-alist '(("." . "~/.saves")))

(require 'haml-mode)
(setq haml-mode-hook
    (function (lambda ()
                (setq indent-tabs-mode nil))))