;; A quick way to produce colors properly with terminal type "putty-256color"
;; a slight modification of screen.el
;;   credit Trent W. Buck

(load "term/xterm")

(defun terminal-init-putty ()
   "Terminal initialization function for putty."
   ;; Use the xterm color initialization code.
   (xterm-register-default-colors)
   (tty-set-up-initial-frame-faces))