;;; commonmark.el --- Major mode for editing CommonMark

;; Copyright (C) 2017 USAMI Kenta

;; Author: USAMI Kenta
;; Package-Requires: ((emacs "24.3"))
;; Version: 0.0.1
;; Created: 19 March 2017
;; Keywords: hypermedia

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; commonmark.el is lightweight major mode for editing Markdown based on CommonMark.
;; http://commonmark.org/
;;
;; This mode is simple and complatible with other minor modes.

;;; Code:

(require 'org-table nil t)
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

(defcustom commonmark-gfm-enable-orgtbl-mode (featurep 'org-table)
  "Enable `orgtbl-mode' if the value is T.")

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

(defface commonmark-inline-code '((t :inherit shadow))
  "Face for fixed-with text like code snippets."
  :group 'commonmark-faces)

(defface commonmark-thematic-break '((t :strike-through t))
  "Face for thematic break (<hr />)."
  :group 'commonmark-faces)

(defface commonmark-html-comment '((t :inherit font-lock-comment-face))
  "Face for HTML comment block (<!-- -->)"
  :group 'commonmark-faces)


;; Font lock
(defconst commonmark-font-lock-keywords
  (eval-when-compile
    (list
     ;; 4.1 Thematic breaks
     '("^[ 	]\\{0,3\\}\\*[* 	]\\{2,\\}$" 0 'commonmark-thematic-break)
     '("^[ 	]\\{0,3\\}-[- 	]\\{2,\\}$" 0 'commonmark-thematic-break)
     '("^[ 	]\\{0,3\\}-[_ 	]\\{2,\\}$" 0 'commonmark-thematic-break)
     ;; 4.2 ATX headings
     '("^[ 	]\\{0,3\\}\\(######\\([ 	]+.+?\\)?\\)\\(?:[ 	]+#*[ 	]*\\)?$" 1 'commonmark-level-6)
     '("^[ 	]\\{0,3\\}\\(#####\\([ 	]+.+?\\)?\\)\\(?:[ 	]+#*[ 	]*\\)?$" 1 'commonmark-level-5)
     '("^[ 	]\\{0,3\\}\\(####\\([ 	]+.+?\\)?\\)\\(?:[ 	]+#*[ 	]*\\)?$" 1 'commonmark-level-4)
     '("^[ 	]\\{0,3\\}\\(###\\([ 	]+.+?\\)?\\)\\(?:[ 	]+#*[ 	]*\\)?$" 1 'commonmark-level-3)
     '("^[ 	]\\{0,3\\}\\(##\\([ 	]+.+?\\)?\\)\\(?:[ 	]+#*[ 	]*\\)?$" 1 'commonmark-level-2)
     '("^[ 	]\\{0,3\\}\\(#\\([ 	]+.+?\\)?\\)\\(?:[ 	]+#*[ 	]*\\)?$" 1 'commonmark-level-1)
     ;; 4.7 Link reference definitions
     `(,(rx bol
            (group "[" (* (not (any "\n" "]"))) "]") ":"
            (+ " ")
            (group (* (not (any "\n" " "))))
            (* " ") eol)
       (1 'commonmark-link-text)
       (2 'commonmark-link-url))
     `(,(rx bol
            (group "[" (* (not (any "\n" "]"))) "]") ":"
            (+ " ")
            (group (* (not (any "\n" " "))))
            (+ " ") (group "\"" (* (not (any "\n" "\""))) "\"")
            (* " ") eol)
       (1 'commonmark-link-text)
       (2 'commonmark-link-url)
       (3 'commonmark-link-title))
     ;; 6.5 Links
     `(,(rx (group "[" (* (not (any "\n" "]"))) "]") "("
            (group (* (not (any "\n" " " "]"))))
            ")")
       (1 'commonmark-link-text)
       (2 'commonmark-link-url))
     `(,(rx (group "[" (* (not (any "\n" "]"))) "]") "("
            (group (* (not (any "\n" " " "]"))))
            (+ " ") (group "\"" (* (not (any "\n" "\""))) "\"")
            ")")
       (1 'commonmark-link-text)
       (2 'commonmark-link-url)
       (3 'commonmark-link-title))
     `(,(rx (group "[" (* (not (any "\n" "]"))) "]")
            "[" (group (* (not (any "\n" " " "]")))) "]")
       (1 'commonmark-link-text)
       (2 'commonmark-link-url))
     ;; HTML Comment
     '("<!--\\s +\\(\\(?:.\\|\n\\)+?\\s +\\)?-->" 0 'commonmark-html-comment)

     '("~[^\n~]+?~" 0 '(:strike-through t) t)
     '("\\(?:^\\|[^\n*]\\)\\(\\*\\*\\w+?\\*\\*\\)\\(?:[^\n*]\\|$\\)" 1 'bold t)
     '("\\(?:^\\|[^\n*]\\)\\(\\*\\w+?\\*\\)\\(?:[^\n*]\\|$\\)" 1 'italic t)
     '("``[^\n`]+?``" 0 'commonmark-inline-code t)
     '("`[^\n`]+`" 0 'commonmark-inline-code t))))

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

  ;; font-lock.el / font-core.el
  (setq font-lock-defaults '(commonmark-font-lock-keywords))

  ;; newcomment.el
  (setq comment-start "<!--")
  (setq comment-end "-->")

  (when commonmark-enable-outline-mode
    (outline-minor-mode)))

;;;###autoload
(define-derived-mode commonmark-gfm-mode commonmark-markdown-mode "CmGFM"
  "Major mode for editing GitHub Flavored Markdown."
  (when commonmark-gfm-enable-orgtbl-mode
    (orgtbl-mode)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.md\\'" . commonmark-mode))

(provide 'commonmark)
;;; commonmark.el ends here
