;;;; Modes

;; yasnippet
(load-lib-dir "yasnippet")
(require 'yasnippet)
(setq yas/root-directory (concat root-dir "yasnippet/snippets"))
(yas/load-directory yas/root-directory)
(yas/initialize)

;; javascript indention
(setq js-indent-level 2)

;; js erb files
(add-to-list 'auto-mode-alist '("\\.js.erb$" . js-mode))

;; mysql cnf files
(add-to-list 'auto-mode-alist '("\\.cnf$" . conf-mode))

;; markdown mode for *.md files.
(add-to-list 'auto-mode-alist '("\.md$" . markdown-mode))

;; rainbow mode -- for css
;; (autoload 'rainbow-mode "rainbow-mode")
(setq rainbow-html-colors 'auto)

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

;; fix the background color for erb
;;   this needs to happen before loading rhtml-mode

(add-to-list 'auto-mode-alist '("\\.ejs$" . rhtml-mode))

(defface erb-face
  `((t (:background "brightwhite" :foreground "red")))
  "Default inherited face for ERB tag body"
  :group 'rhtml-faces)

(defface erb-delim-face
  `((t (:background "brightwhite" :foreground "red")))
  "Default inherited face for ERB tag delimeters"
  :group 'rhtml-faces)

;; coffee
(add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))
(add-to-list 'auto-mode-alist '("Cakefile" . coffee-mode))
(defun coffee-custom ()
  "coffee-mode-hook"
  (set (make-local-variable 'tab-width) 2))

(add-hook 'coffee-mode-hook
	  '(lambda() (coffee-custom)))

;; php
(add-to-list 'load-path "~/.emacs.d/php-mode")
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

;; io -- Note: as of 9/2013 there was no io-mode on marmalade.
(add-to-list 'load-path "~/.emacs.d/io-mode")
(autoload 'io-mode "io-mode" "Mode for editing Io files" t)
(add-to-list 'auto-mode-alist '("\\.io$" . io-mode))

;; erlang -- Note: we use the one that comes with brew, its more up-to-date then
;;        -- then the one that is (currently?) on marmalade.
(add-to-list 'load-path "~/.emacs.d/erlang-mode")
(require 'erlang-start)
