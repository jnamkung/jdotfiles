(defvar foo-gnome "loaded gnome.el")

(load "term/xterm")

(defun terminal-init-gnome ()
   "Terminal initialization function for gnome terminal."
   ;; Use the xterm color initialization code.
   (xterm-register-default-colors)
   (tty-set-up-initial-frame-faces))