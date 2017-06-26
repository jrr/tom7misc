
;;; homework-mode
;;; this essentially just does syntax coloring. Put it in your .emacs
;;; file, add an autoload line:
;;; 
;;; (setq auto-mode-alist
;;;      (append '(("\\.mst$" . homework-mode)
;;;		   ("\\.wrd$" . homework-mode)) auto-mode-alist))
;;;
;;; and run M-x customize-face to change the fonts defined below.

;;; - Tom 7      6 Jan 2002


(make-empty-face 'tom7-hw-familyname)
(defvar tom7-hw-familyname-face	'tom7-hw-familyname
  "Face name to use for family names in homework documents.")

(make-empty-face 'tom7-hw-familydef)
(defvar tom7-hw-familydef-face	'tom7-hw-familydef
  "Face name to use for family names (at definition) in homework documents.")

(make-empty-face 'tom7-hw-infinite)
(defvar tom7-hw-infinite-face	'tom7-hw-infinite
  "Face name to use for marking lines in unique families that are infinite.")

(defcustom homework-mode-hook nil
  "Normal hook run when entering Homework mode"
  :type 'hook
  :group 'data)

(defvar homework-mode-syntax-table nil
  "Syntax table used while in scribe mode.")

(setq homework-mode-syntax-table (copy-syntax-table text-mode-syntax-table))

;; a-la scribe mode. This is pretty dirty...
(let ((st (syntax-table)))
  (unwind-protect
      (progn
	(setq homework-mode-syntax-table (copy-syntax-table
					text-mode-syntax-table))
	(set-syntax-table homework-mode-syntax-table)
	(modify-syntax-entry ?\" "_   ")
	(modify-syntax-entry ?\\ "_   ")
	(modify-syntax-entry ?% "$   ")
	
	(set-syntax-table st))))

(defun homework-mode ()
  "Tom's mode for editing Homework files."
  (interactive)
  (kill-all-local-variables)
  (setq mode-name "Homework")
  (setq major-mode 'homework-mode)
  (turn-on-font-lock)
  (set-syntax-table homework-mode-syntax-table)
  (run-hooks 'homework-mode-hook))

(font-lock-add-keywords 'homework-mode '(
("^#!?\\([A-Za-z0-9]+\\)$" 1 tom7-hw-familydef-face)
("^\\(!\\)" 1 tom7-hw-infinite-face)
("%/!*\\([A-Za-z0-9]+\\)/[^%]*%" 1 font-lock-variable-name-face)
("%[&`^]*\\([A-Za-z0-9]+\\)%" 1 tom7-hw-familyname-face)
("%[^%?]*[&`^]*[?-]\\([A-Za-z0-9]+\\)%" 1 font-lock-variable-name-face)
("%[^%\"]*\\(\\\"[^%]*\\)%" 1 font-lock-string-face)
("%[&`^]*\\([A-Za-z0-9]+\\):?=[^%]*%" 1 font-lock-variable-name-face)
("%[&`^]*[A-Za-z0-9]+:?=[&`^]*\\([A-Za-z0-9]+\\)%" 1 tom7-hw-familyname-face)
))

;;; homework-mode ends here
