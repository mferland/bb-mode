;;; bb-mode.el --- major mode for editing bitbake files

;; Copyright (c) 2013 Marc Ferland <marc.ferland@gmail.com>

;; Author: Marc Ferland <marc.ferland@gmail.com>

;; Keywords: languages, faces
;; Last edit: 2013-03-30
;; Version: 1.0

;; bb-mode.el is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2, or (at your option) any later
;; version.
;;
;; It is distributed in the hope that it will be useful, but WITHOUT ANY
;; WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.
;;
;; You should have received a copy of the GNU General Public License along
;; with your copy of Emacs; see the file COPYING.  If not, write to the Free
;; Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA.

;;; Commentary:
;; (require 'bb-mode)
;; (setq auto-mode-alist
;;       (append '(("\\.bb\\'" . bb-mode)
;;                 ("\\.inc\\'" . bb-mode)
;;                 ("\\.bbclass\\'" . bb-mode)
;;                 ("\\.bbappend\\'" . bb-mode)
;;                 auto-mode-alist)))
;;

(defun bb-comment-dwim (arg)
  "Comment or uncomment current line or region.
For detail, see `comment-dwim'."
  (interactive "*P")
  (require 'newcomment)
  (let (
        (comment-start "#") (comment-end "")
        )
    (comment-dwim arg)))

(setq bb-expr-white-space-regexp "[ \t]*")
(setq bb-function-name-opt-regexp "\\([a-zA-Z0-9_-]*\\)")
(setq bb-function-name-regexp "\\([a-zA-Z0-9_-]+\\)")
(setq bb-function-paren-regexp "([ \t]*)")
(setq bb-function-decl-regexp (concat bb-function-name-regexp
                                      bb-expr-white-space-regexp
                                      bb-function-paren-regexp))
(setq bb-function-decl-opt-regexp (concat bb-function-name-opt-regexp
                                          bb-expr-white-space-regexp
                                          bb-function-paren-regexp))
(setq bb-variable-regexp "\\([]\[a-zA-Z0-9\-_\/\${}]+\\)")
(setq bb-variable-assignment-regexp
      (regexp-opt '("=" ":=" "?=" ".=" "??=" "+=" "=+" "=.")))
(setq bb-variable-deref-regexp "\${[a-zA-Z0-9\-_\/]+}")
(setq bb-addtask-regexp (regexp-opt '("before" "after") 'words))
(setq bb-keywords-regexp
      (concat "^"
              (regexp-opt '("inherit"
                            "include"
                            "require"
                            "EXPORT_FUNCTIONS"
                            "addhandler") 'words)
              ))
(setq bb-base-function-decl-regexp
      (concat "^"
              (regexp-opt '("do_setscene"
                            "do_fetch"
                            "do_unpack"
                            "do_patch"
                            "do_configure"
                            "do_compile"
                            "do_install"
                            "do_populate_sysroot"
                            "do_package") 'words)
              ))

(setq bb-font-lock
      `(
        ;; keywords
        (,bb-keywords-regexp 0 font-lock-keyword-face)

        ;; addtask 3rd form: addtask FUNCTION_NAME (after|before) FUNCTION_NAME (after|before) FUNCTION_NAME
        (,(concat "^"
                  "\\(addtask\\)"
                  "[ \t]+"
                  bb-function-name-regexp
                  "[ \t]+"
                  "\\(before\\|after\\)"
                  "[ \t]+"
                  bb-function-name-regexp
                  "[ \t]+"
                  "\\(before\\|after\\)"
                  "[ \t]+"
                  bb-function-name-regexp
                  )
         (1 font-lock-keyword-face)
         (2 font-lock-function-name-face)
         (3 font-lock-keyword-face)
         (4 font-lock-function-name-face)
         (5 font-lock-keyword-face)
         (6 font-lock-function-name-face)
         )
        
        ;; addtask 2nd form: addtask FUNCTION_NAME (after|before) FUNCTION_NAME
        (,(concat "^"
                  "\\(addtask\\)"
                  "[ \t]+"
                  bb-function-name-regexp
                  "[ \t]+"
                  "\\(before\\|after\\)"
                  "[ \t]+"
                  bb-function-name-regexp
                  )
         (1 font-lock-keyword-face)
         (2 font-lock-function-name-face)
         (3 font-lock-keyword-face)
         (4 font-lock-function-name-face)
         )

        ;; addtask 1st form: addtask FUNCTION_NAME
        (,(concat "^"
                  "\\(addtask\\)"
                  "[ \t]+"
                  bb-function-name-regexp
                  )
         (1 font-lock-keyword-face)
         (2 font-lock-function-name-face)
         )

        ;; python function
        (,(concat "^"
                  "\\(python\\)"
                  "[ \t]+"
                  bb-function-decl-opt-regexp)
         (1 font-lock-keyword-face)
         (2 font-lock-function-name-face)
         )

        ;; built-in/basic "do_" routines
        (,(concat bb-base-function-decl-regexp
                  "[ \t]*"
                  bb-function-paren-regexp)
         (1 font-lock-builtin-face)
         )

        ;; shell script function
        (,(concat "^"
                  bb-function-decl-regexp)
         1 font-lock-function-name-face)

        ;; export variable
        (,(concat "^\\(export\\)"
                  "[ \t]+"
                  bb-variable-regexp
                  "[ \t]*"
                  bb-variable-assignment-regexp)
         (1 font-lock-keyword-face)
         (2 font-lock-variable-name-face))

        ;; variable
        (,(concat "^"
                  bb-variable-regexp
                  "[ \t]*"
                  bb-variable-assignment-regexp)
         (1 font-lock-variable-name-face))

        (,bb-variable-deref-regexp 0 font-lock-variable-name-face)
        )
      )

(defvar bb-syntax-table nil "Syntax table for `bb-mode'.")
(setq bb-syntax-table
      (let ((synTable (make-syntax-table)))

        ;; bash style comment: "# ..."
        (modify-syntax-entry ?# "< b" synTable)
        (modify-syntax-entry ?\n "> b" synTable)

        synTable))

(define-derived-mode bb-mode fundamental-mode
  "bb"
  "Major mode for editing bitbake files"
  :syntax-table bb-syntax-table
  
  (setq font-lock-defaults '((bb-font-lock)))
  (define-key bb-mode-map [remap comment-dwim] 'bb-comment-dwim)
  
  (setq bb-expr-white-space-regexp nil)
  (setq bb-function-name-opt-regexp nil)
  (setq bb-function-name-regexp nil)
  (setq bb-function-paren-regexp nil)
  (setq bb-function-decl-regexp nil)
  (setq bb-function-decl-opt-regexp nil)
  (setq bb-variable-regexp nil)
  (setq bb-variable-assignment-regexp nil)
  (setq bb-variable-deref-regexp nil)
  (setq bb-addtask-regexp nil)
  (setq bb-keywords-regexp nil)
  (setq bb-base-function-decl-regexp nil)

  )

(provide 'bb-mode)
