bb-mode
=======

bb-mode is a major mode for Emacs. It does basic syntax highlighting
and handles comments (M-;).

How to install it?
------------------

   (require 'bb-mode)
   (setq auto-mode-alist
         (append '(("\\.bb\\'" . bb-mode)
                   ("\\.inc\\'" . bb-mode)
                   ("\\.bbclass\\'" . bb-mode)
                   ("\\.bbappend\\'" . bb-mode)
                   auto-mode-alist)))
