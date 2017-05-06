;;; commonmark.el --- Major mode for editing CommonMark

;; Copyright (C) 2017 USAMI Kenta

;; Author: USAMI Kenta
;; Package-Requires: ((emacs "24.3"))
;; Version: 0.0.1
;; Created: 19 March 2017
;; Keywords: hypermedia

;;; Commentary:

;; commonmark.el is lightweight major mode for editing Markdown based on CommonMark.
;; http://commonmark.org/
;;
;; This mode is simple and complatible with other minor modes.

;;; Code:

(require 'outline)
(require 'thingatpt)


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

(defcustom commonmark-enable-outline-mode t
  "Enable `outline-mode' if the value is T.")

;; Variables

(defvar commonmark-markdown-mode-hook (list 'outline-minor-mode))

(defconst commonmark-goto-address-mail-regexp
  (eval-when-compile
    (rx (+ (any "a-zA-Z0-9.!#$%&'*+/=?^_`{|}~-"))
        "@"
        (any "a-zA-Z0-9")
        (** 0 61 (any "a-zA-Z0-9-"))
        (? (any "a-zA-Z0-9"))
        (* "."
           (any "a-zA-Z0-9")
           (** 0 61 (any "a-zA-Z0-9-"))
           (? (any "a-zA-Z0-9")))))
  "Alternative regexp for E-mail address.
Original spec regexp is follows:
/^[a-zA-Z0-9.!#$%&'*+/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?
(?:\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$/")

(defconst commonmark-url-path-regexp
  "[^]\t\n \"'<>[^`{}]*[^]\t\n \"'<>[^`{}.,;)]+"
  "Regexp matching the host and filename or e-mail part of a URL.")

(defconst commonmark-goto-address-url-regexp
  (eval-when-compile
    (concat
     "\\<"
     (regexp-opt (delete "mailto:"
                         (delete "data:"
                                 (copy-sequence thing-at-point-uri-schemes))))
     commonmark-url-path-regexp)))


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

(defface commonmark-link-text '((t :inherit font-lock-string-face))
  "Face for link texts.")

(defface commonmark-link-url '((t :inherit link))
  "Face for link URLs."
  :group 'commonmark-faces)

(defface commonmark-link-title '((t :inherit font-lock-doc-face))
  "Face for link titles."
  :group 'commonmark-faces)

(defface commonmark-inline-code-face '((t :inherit shadow))
  "Face for fixed-with text like code snippets."
  :group 'commonmark-faces)


;; Font lock
(defconst commonmark-regexp-heading-level-1
  "^\\(#\\([ 	]+.+?\\)?\\)\\(?:[ 	]+#*[ 	]*\\)?$")
(defconst commonmark-regexp-heading-level-2
  "^\\(##\\([ 	]+.+?\\)?\\)\\(?:[ 	]+#*[ 	]*\\)?$")
(defconst commonmark-regexp-heading-level-3
  "^\\(###\\([ 	]+.+?\\)?\\)\\(?:[ 	]+#*[ 	]*\\)?$")
(defconst commonmark-regexp-heading-level-4
  "^\\(####\\([ 	]+.+?\\)?\\)\\(?:[ 	]+#*[ 	]*\\)?$")
(defconst commonmark-regexp-heading-level-5
  "^\\(#####\\([ 	]+.+?\\)?\\)\\(?:[ 	]+#*[ 	]*\\)?$")
(defconst commonmark-regexp-heading-level-6
  "^\\(######\\([ 	]+.+?\\)?\\)\\(?:[ 	]+#*[ 	]*\\)?$")

(defconst commonmark-font-lock
  (eval-when-compile
    (list
     ;; ATX headings
     `(,commonmark-regexp-heading-level-6 1 'commonmark-level-6)
     `(,commonmark-regexp-heading-level-5 1 'commonmark-level-5)
     `(,commonmark-regexp-heading-level-4 1 'commonmark-level-4)
     `(,commonmark-regexp-heading-level-3 1 'commonmark-level-3)
     `(,commonmark-regexp-heading-level-2 1 'commonmark-level-2)
     `(,commonmark-regexp-heading-level-1 1 'commonmark-level-1)
     '("``.*?``"   0 'commonmark-inline-code-face)
     '("`[^`\n]*`" 0 'commonmark-inline-code-face)
     `(,(rx (group "[" (* (not (any "]"))) "]") "("
            (group (* (not (any " " "]"))))
            (? (+ " ") (group "\"" (* (not (any "\""))) "\""))
            ")")
       (1 'commonmark-link-text)
       (2 'commonmark-link-url)
       (3 'commonmark-link-title)))))

;; Variables
(defvar commonmark-outline-heading-alist
  '(("# "      . 1)
    ("## "     . 2)
    ("### "    . 3)
    ("#### "   . 4)
    ("##### "  . 5)
    ("###### " . 6)))


;;;###autoload
(defun commonmark-mode ()
  "Major mode for editing CommonMark."
  (funcall commonmark-major-mode))

;;;###autoload
(define-derived-mode commonmark-markdown-mode text-mode "CommonMark"
  "Major mode for editing CommonMark."

  ;; goto-addr.el
  (setq-local goto-address-url-regexp commonmark-goto-address-url-regexp)
  (setq-local goto-address-mail-regexp commonmark-goto-address-mail-regexp)

  ;; outline
  (setq outline-heading-alist commonmark-outline-heading-alist)
  (setq outline-regexp
        (concat "^" (regexp-opt (mapcar 'car commonmark-outline-heading-alist))))

  (setq font-lock-defaults '(commonmark-font-lock))

  (when commonmark-enable-outline-mode
    (outline-minor-mode)))

;;;###autoload
(define-derived-mode commonmark-gfm-mode commonmark-markdown-mode "CmGFM"
  "Major mode for editing GitHub Flavored Markdown.")

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.md\\'" . commonmark-mode))

(provide 'commonmark)
;;; commonmark.el ends here
