;;; Modes --- mode-specific initialization and configuration
;;; Commentary:
;;;   Try to keep things in here alphabetized by mode
;;; Code:

;; ag (ag > ack)
(global-set-key (kbd "C-c k") 'ag)

;; buffer selection
(require 'bs)
(global-set-key (kbd "C-x C-b") 'bs-show) ; better buffer listings

;; cnf files (e.g. mysql config files)
(add-to-list 'auto-mode-alist '("\\.cnf$" . conf-mode))

;; coffee
(add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))
(add-to-list 'auto-mode-alist '("Cakefile" . coffee-mode))
(custom-set-variables '(coffee-tab-width 2))

;; expand-region
(require 'expand-region)
(global-set-key (kbd "C-c o") 'er/expand-region)

;; flycheck
(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)
(add-hook 'js-mode-hook (lambda () (flycheck-mode t)))
(add-hook 'scss-mode-hook 'flycheck-mode);; disable jshint
(setq-default flycheck-disabled-checkers
              (append flycheck-disabled-checkers
                      '(javascript-jshint)))
;; use eslint with web-mode for jsx files
(flycheck-add-mode 'javascript-eslint 'web-mode)
;; use eslint from ./node_modules when available
;; see: http://emacs.stackexchange.com/questions/21205/flycheck-with-file-relative-eslint-executable
(defun my/use-eslint-from-node-modules ()
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (eslint (and root
                      (expand-file-name "node_modules/eslint/bin/eslint.js"
                                        root))))
    (when (and eslint (file-executable-p eslint))
      (setq-local flycheck-javascript-eslint-executable eslint))))
(add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)

;; js-mode, js2-mode
(setq js-indent-level 2)

;; magit
(global-set-key (kbd "C-x g") 'magit-status)

;; multiple-cursors
(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)

;; markdown mode for *.md files.
(add-to-list 'auto-mode-alist '("\.md$" . markdown-mode))

;; rainbow mode - in css, display color specifiers in the color they specify
;; (autoload 'rainbow-mode "rainbow-mode")
(setq rainbow-html-colors 'auto)

;; robe
;;  prevent "WARNING: terminal is not fully functional" when inf-ruby starts
(setenv "PAGER" "cat")
(add-hook 'ruby-mode-hook 'robe-mode)

;; ruby
(require 'ruby-mode)
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

;; rvm
(require 'rvm)
(rvm-autodetect-ruby)

;; sass - don't compile on save
(add-hook 'scss-mode-hook
          '(lambda ()
             (setq scss-compile-at-save nil)
             (setq css-indent-offset 2)))

;; string-inflection - cycle between snake case, camel case, etc.
(require 'string-inflection)
(global-set-key (kbd "C-c i") 'string-inflection-cycle)
(global-set-key (kbd "C-c c") 'string-inflection-camelcase)        ;; Force to CamelCase
(global-set-key (kbd "C-c l") 'string-inflection-lower-camelcase)  ;; Force to lowerCamelCase
(global-set-key (kbd "C-c j") 'string-inflection-java-style-cycle) ;; Cycle through Java styles

;; web-mode
(add-to-list 'auto-mode-alist '("\\html.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\text.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js.erb$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.es6$" . web-mode))
(setq web-mode-markup-indent-offset 2)
(setq web-mode-css-indent-offset 2)
(setq web-mode-code-indent-offset 2)
;; tell web-mode to treat .es6 files as jsx
(setq web-mode-content-types-alist '(("jsx"  . "\\.es6\\'")))
;; use web-mode for jsx - https://truongtx.me/2014/03/10/emacs-setup-jsx-mode-and-jsx-syntax-checking/
(defadvice web-mode-highlight-part (around tweak-jsx activate)
  (if (equal web-mode-content-type "jsx")
      (let ((web-mode-enable-part-face nil))
        ad-do-it)
    ad-do-it))

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


(provide 'modes)
;;; modes.el ends here
