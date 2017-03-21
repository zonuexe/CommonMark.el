;;; commonmark.el --- Major mode for editing CommonMark

;; Copyright (C) 2017 USAMI Kenta

;; Author: USAMI Kenta
;; Package-Requires: ((emacs "24"))
;; Version: 0.0.1
;; Created: 19 March 2017
;; Keywords: hypermedia

;;; Commentary:

;; commonmark.el is lightweight major mode for editing Markdown based on CommonMark.
;; http://commonmark.org/

;;; Code:

(defgroup commonmark nil
  "Major mode for editing CommonMark Markdown."
  :group 'text
  :tag "CommonMark"
  :prefix "commonmark-")

(defcustom commonmark-major-mode 'commonmark-gfm-mode
  "Major mode for editing CommonMark Markdown."
  :type '(choice (const :tag "Plain CommonMark mode" commonmark-markdown-mode)
                 (const :tag "CommonMark GFM Markdown mode" commonmark-gfm-mode)
                 (function "Major mode function")))


;; Faces
(defgroup commonmark-faces nil
  "Faces for CommonMark syntax highlighting."
  :group 'commonmark
  :group 'faces
  :tag "CommonMark Faces")

(defface commonmark-level-1 '((t :inherit outline-1))
  "Face used for level 1 headlines."
  :group 'commonmark-faces)

(defface commonmark-level-2 '((t :inherit outline-2))
  "Face used for level 2 headlines."
  :group 'commonmark-faces)

(defface commonmark-level-3 '((t :inherit outline-3))
  "Face used for level 3 headlines."
  :group 'commonmark-faces)

(defface commonmark-level-4 '((t :inherit outline-4))
  "Face used for level 4 headlines."
  :group 'commonmark-faces)

(defface commonmark-level-5 '((t :inherit outline-5))
  "Face used for level 5 headlines."
  :group 'commonmark-faces)

(defface commonmark-level-6 '((t :inherit outline-6))
  "Face used for level 6 headlines."
  :group 'commonmark-faces)

(defface commonmark-link '((t :inherit link))
  "Face for links."
  :group 'commonmark-faces)


;; Font lock
(defconst commonmark-font-lock
  (list
   ;; ATX headings
   '("^\\(######\\([ 	]+.+?\\)?\\)\\(?:[ 	]+#*[ 	]*\\)?$" 1 'commonmark-level-6)
   '("^\\(#####\\([ 	]+.+?\\)?\\)\\(?:[ 	]+#*[ 	]*\\)?$" 1 'commonmark-level-5)
   '("^\\(####\\([ 	]+.+?\\)?\\)\\(?:[ 	]+#*[ 	]*\\)?$" 1 'commonmark-level-4)
   '("^\\(###\\([ 	]+.+?\\)?\\)\\(?:[ 	]+#*[ 	]*\\)?$" 1 'commonmark-level-3)
   '("^\\(##\\([ 	]+.+?\\)?\\)\\(?:[ 	]+#*[ 	]*\\)?$" 1 'commonmark-level-2)
   '("^\\(#\\([ 	]+.+?\\)?\\)\\(?:[ 	]+#*[ 	]*\\)?$" 1 'commonmark-level-1)
   ))


;;;###autoload
(defun commonmark-mode ()
  "Major mode for editing CommonMark."
  (funcall commonmark-major-mode))

;;;###autoload
(define-derived-mode commonmark-markdown-mode text-mode "CommonMark"
  "Major mode for editing CommonMark."
  (setq font-lock-defaults '(commonmark-font-lock)))

;;;###autoload
(define-derived-mode commonmark-gfm-mode commonmark-markdown-mode "CmGFM"
  "Major mode for editing GitHub Flavored Markdown.")

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.md\\'" . commonmark-mode))

(provide 'commonmark)
;;; commonmark.el ends here