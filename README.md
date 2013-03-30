bb-mode
=======

bb-mode is a major mode for Emacs. It does basic syntax highlighting
and handles comments (M-;).

How to install it?
------------------

    (require 'bb-mode)
    (setq auto-mode-alist (cons '("\\.bb$" . bb-mode) auto-mode-alist))
    (setq auto-mode-alist (cons '("\\.inc$" . bb-mode) auto-mode-alist))
    (setq auto-mode-alist (cons '("\\.bbappend$" . bb-mode) auto-mode-alist))
    (setq auto-mode-alist (cons '("\\.bbclass$" . bb-mode) auto-mode-alist))
    (setq auto-mode-alist (cons '("\\.conf$" . bb-mode) auto-mode-alist))
