;;;; Modes

;; ag (ag > ack)
(global-set-key (kbd "C-c k") 'ag)

;; autocomplete
;; (require 'auto-complete-config)
;; (ac-config-default)
;; ;; (setq ac-ignore-case nil)
;; ;; (add-to-list 'ac-modes 'enh-ruby-mode)
;; (add-to-list 'ac-modes 'web-mode)

;; coffee
(add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))
(add-to-list 'auto-mode-alist '("Cakefile" . coffee-mode))
(custom-set-variables '(coffee-tab-width 2))

;; erlang -- Note: we use the one that comes with brew,
;;           it's more up-to-date then
;;        -- then the one that is (currently?) on marmalade.
(add-to-list 'load-path "~/.emacs.d/erlang-mode")
(require 'erlang-start)

;; expand-region
(require 'expand-region)
(global-set-key (kbd "C-c o") 'er/expand-region)

;; highlight-indentation
(require 'highlight-indentation)
(add-hook 'ruby-mode-hook
         (lambda () (highlight-indentation-current-column-mode)))
(add-hook 'coffee-mode-hook
         (lambda () (highlight-indentation-current-column-mode)))
(add-hook 'js-mode-hook
         (lambda () (highlight-indentation-current-column-mode)))

;; io -- Note: as of 9/2013 there was no io-mode on marmalade.
(add-to-list 'load-path "~/.emacs.d/io-mode")
(autoload 'io-mode "io-mode" "Mode for editing Io files" t)
(add-to-list 'auto-mode-alist '("\\.io$" . io-mode))

;; js-mode
(setq js-indent-level 2)
(add-to-list 'auto-mode-alist '("\\.js.erb$" . js-mode))
(add-to-list 'auto-mode-alist '("\\.js.jsx$" . js-mode))

;; mysql cnf files
(add-to-list 'auto-mode-alist '("\\.cnf$" . conf-mode))

;; markdown mode for *.md files.
(add-to-list 'auto-mode-alist '("\.md$" . markdown-mode))

;; php
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
  (c-set-offset 'arglist-intro '+))

;; rainbow mode -- for css
;; (autoload 'rainbow-mode "rainbow-mode")
(setq rainbow-html-colors 'auto)

;; robe
;;  prevent "WARNING: terminal is not fully functional" when inf-ruby starts
(setenv "PAGER" "cat")
(add-hook 'ruby-mode-hook 'robe-mode)

;; ruby
(autoload 'ruby-mode "ruby-mode" "Major mode for ruby files" t)
(setq auto-mode-alist (append '(("\\.rb$"   . ruby-mode)
                                ("\\.rake$"   . ruby-mode)
                                ("\\.builder$" . ruby-mode)
                                ("\\.jbuilder$" . ruby-mode)
                                ("\\.gemspec$" . ruby-mode)
                                ("\\.rabl$"    . ruby-mode)
                                ("Rakefile"   . ruby-mode)
                                ("Capfile"   . ruby-mode)
                                ("Gemfile"   . ruby-mode)
				) auto-mode-alist))

;; sass - don't compile on save
(add-hook 'scss-mode-hook
          '(lambda ()
             (setq scss-compile-at-save nil)
             (setq css-indent-offset 2)))

;; web-mode
(add-to-list 'auto-mode-alist '("\\html.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\text.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(setq web-mode-markup-indent-offset 2)
(setq web-mode-css-indent-offset 2)
(setq web-mode-code-indent-offset 2)

;; Yasnippet
(require 'yasnippet)
;; use the nice dropdown-list widget, instead of selecting snippets in the minibuffer
(require 'dropdown-list)
(setq yas-prompt-functions '(yas-dropdown-prompt
                             yas-ido-prompt
                             yas-completing-prompt))
;; pick up custom snippets in dicksonlabs/snippets
(add-to-list 'yas-snippet-dirs
             (concat root-dir "dicksonlabs/snippets/") t)
;; use default html snippets in web-mode
(add-hook 'web-mode-hook
          #'(lambda () (yas-activate-extra-mode 'html-mode)))
;; do this to make the web-mode-hook stick
(yas-global-mode 1)
