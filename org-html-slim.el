;;; org-html-slim.el --- a minimalist HTML org-export backend
;; Copyright (C) Elo Laszlo 2016

;; Author: Elo Laszlo <laszlo@manifold.io>
;; Created: August 2016
;; Description: a minimalist HTML org-export backend
;; Homepage: http://manifold.io/project/org-html-slim
;; Version: 0.1.0
;; Package-Requires: ((s "20160711.525"))
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; org-html-slim is an HTML export backend for org-mode.
;; It is a set of translators for common org elements which
;; output minimal HTML, and use only a limited amount of
;; pre-defined logic.
;;

;;; Code:
(require 'ox-publish)
;; (require 'ox-html)
(require 's)

;;
;; translators
;;

(defun org-html-slim-bold (bold contents info)
  "Transcode BOLD from Org to HTML.

CONTENTS is the text with bold markup.
INFO is a plist holding contextual information."
  (format "<strong>%s</strong>" (or contents "")))



(defun org-html-slim-export-block (export-block contents info)
  "Transcode an EXPORT-BLOCK element from Org to HTML.

CONTENTS is nil. INFO is a plist holding contextual information.

- exports #+BEGIN_HTML...#+END_HTML"
  (when (string= (org-element-property :type export-block) "HTML")
    (org-remove-indentation (org-element-property :value export-block))))



(defun org-html-slim-export-snippet (export-snippet contents info)
  "Transcode a EXPORT-SNIPPET object from Org to HTML.

CONTENTS is nil. INFO is a plist holding contextual information.

- exports @@html:...@@"
  (when (eq (org-export-snippet-backend export-snippet) 'html)
    (org-element-property :value export-snippet)))



(defun org-html-slim-headline (headline contents info)
  "Transcode HEADLINE from Org to HTML.

CONTENTS is the section as defined under the HEADLINE.
INFO is a plist holding contextual information.

- html attributes can be declared in the property
drawer with #+attr_html:"
  (let ((title (car (org-element-property :title headline)))
        (level (org-element-property :level headline))
        (attributes (org-element-property :ATTR_HTML headline)))
    (when attributes
      (setq attributes
            (format
             " %s" (org-html--make-attribute-string
                    (org-export-read-attribute
                     'attr_html
                     (list nil `(attr_html ,(split-string attributes))))))))
    (concat
     (format "<h%d%s>%s</h%d>" level (or attributes "") title level)
     (or contents ""))))



(defun org-html-slim-inner-template (contents info)
  "Return body of document string after HTML conversion.

CONTENTS is the transcoded contents string.
INFO is a plist holding export options.

- a publishing project's :inner-template property
can be defined as a string, containing a :yield: keyword,
or as a function, which accepts CONTENTS and INFO"
  (let ((template (plist-get info :inner-template)))
    (if template
        (cond ((stringp template)
               (s-replace ":yield:" (or contents "") template))
              ((listp template)
               (funcall template contents info)))
      (or contents ""))))



(defun org-html-slim-italic (italic contents info)
  "Transcode ITALIC from Org to HTML.

CONTENTS is the text with italic markup.
INFO is a plist holding contextual information."
  (format "<em>%s</em>" (or contents "")))



(defun org-html-slim-item (item contents info)
  "Transcode list ITEM from Org to HTML.

CONTENTS is the text of the list item.
INFO is a plist holding contextual information."
  (format "<li>%s</li>" (or contents "")))



;; (defun org-html-slim-keyword (keyword contents info)
;;   "token based keyword replacement"
;;   (when (string= (org-element-property :key keyword) "TOC")
;;     (org-element-property :value keyword)))



(defun org-html-slim-link (link contents info)
  "Transcode LINK from Org to HTML.

CONTENTS is the text of the link.
INFO is a plist holding contextual information.

- the .org extension is dropped for links starting with :file
- links starting with 'http' or 'https' are given a 'target=_blank' attribute
- prevents double link creation when the contents of a link start with http"
  (if (eq 'link (org-element-type (org-export-get-parent link)))
      (org-element-property :raw-link link)
    (let ((href (if (string= (org-element-property :type link) 'file)
                    (s-replace-all '(("file:" . "")(".org" . ""))
                                   (org-element-property :raw-link link))
                  (org-element-property :raw-link link)))
          (attributes (if (or (string= (org-element-property :type link) 'http)
                              (string= (org-element-property :type link) 'https))
                          " target=\"_blank\"" "")))
      (format "<a href=\"%s\"%s>%s</a>" href attributes contents))))



(defun org-html-slim-paragraph (paragraph contents info)
  "Transcode a PARAGRAPH element from Org to HTML.

CONTENTS is the contents of the paragraph.
INFO is a plist holding contextual information.

- paragraphs inside a list element are forwarded untouched
- paragraphs inside special elements are forwarded untouched
- paragraph attributes can be declared with #+attr_html:"
  (let ((parent (org-export-get-parent paragraph)))
    (if (or (and (eq (org-element-type parent) 'item)
                 (= (org-element-property :begin paragraph)
                    (org-element-property :contents-begin parent)))
            (and (eq (org-element-type parent) 'special-block)
                 (= (org-element-property :begin paragraph)
                    (org-element-property :contents-begin parent))))
        contents
      (let ((html-attributes (or (org-html-slim--element-attributes paragraph) ""))
            (contents (s-trim (s-collapse-whitespace contents))))
        (format "<p%s>%s</p>" html-attributes contents)))))



(defun org-html-slim-plain-list (list contents info)
  "Transcode a LIST string from Org to HTML.

CONTENTS is the contents of the list element.
INFO is a plist holding contextual information.

- list attributes can be declared with #+attr_html:"
  (let ((html-attributes (or (org-html-slim--element-attributes list) "")))
    (format "<ul%s>%s</ul>" html-attributes contents)))



(defun org-html-slim-plain-text (text info)
  "Transcode a TEXT string from Org to HTML.

TEXT is the string to transcode.
INFO is a plist holding contextual information."
  (s-replace-all '(("&" . "&amp;") ("<" . "&lt;") (">" . "&gt;")) text))



(defun org-html-slim-section (section contents info)
  "Transcode a SECTION element from Org to HTML.

CONTENTS is the contents of the section.
INFO is a plist holding contextual information."
  contents)



(defun org-html-slim-special-block (special-block contents info)
  "Transcode SPECIAL-BLOCK from Org to HTML.

CONTENTS is the text within the #+BEGIN_ and #+END_ markers.
INFO is a plist holding contextual information.

- exports (as an example) #+BEGIN_STYLE...#+END_STYLE tags;
the block type is the end of the marker string
- attributes can be declared with #+attr_html:"
  (let ((block-type (downcase (org-element-property :type special-block)))
        (attributes (or (org-html-slim--element-attributes special-block) "")))
    (format "<%s%s>%s</%s>" block-type attributes contents block-type)))



(defun org-html-slim-src-block (src-block contents info)
  "Transcode CODE from Org to HTML.

CONTENTS is the text of a #+BEGIN_SRC...#+END_SRC block.
INFO is a plist holding contextual information.

- the declared language is added as a class attribute"
  (let ((language (org-element-property :language src-block))
        (contents (s-chomp (car (org-export-unravel-code src-block)))))
    (format "<code class=\"%s\"><pre>%s</pre></code>"
            language (org-html-slim-plain-text contents info))))



(defun org-html-slim-template (contents info)
  "Return full document string after HTML conversion.

CONTENTS is the transcoded contents string.
INFO is a plist holding export options.

- a publishing project's :template property can be
defined as a string, containing a :yield: keyword,
or as a function, which accepts CONTENTS and INFO"
  (let ((template (plist-get info :template)))
    (if template
        (cond ((stringp template)
               (s-replace ":yield:" (or contents "") template))
              ((listp template)
               (funcall template contents info)))
      (or contents ""))))



(defun org-html-slim-verbatim (verbatim contents info)
  "Transcode a LIST string from Org to HTML.

TEXT is the string to transcode.
INFO is a plist holding contextual information."
  (format "<kbd>%s</kbd>" (org-element-property :value verbatim)))



;;
;; utility functions
;;

(defun org-html-slim--element-attributes (element)
  "return :attr_html attribute for ELEMENT as an HTML attribute string.

ELEMENT is an org mode element.
INFO is a plist holding contextual information.

- declared with #+attr_html: :class slim on the appropriate elements"
  (let ((attributes (org-export-read-attribute :attr_html element)))
    (when attributes
      (format " %s" (org-html--make-attribute-string attributes)))))


;;;###autoload
(defun org-html-slim-export-string (contents &optional info)
  "export string using html-slim"
  (org-export-string-as contents 'org-html-slim t info))



;;
;; backend definition
;;

(org-export-define-backend 'org-html-slim
  '((bold . org-html-slim-bold)
    ;; (center-block . org-html-slim-center-block)
    ;; (clock . org-html-slim-clock)
    ;; (code . org-html-slim-code)
    ;; (drawer . org-html-slim-drawer)
    ;; (dynamic-block . org-html-slim-dynamic-block)
    ;; (entity . org-html-slim-entity)
    ;; (example-block . org-html-slim-example-block)
    (export-block . org-html-slim-export-block)
    (export-snippet . org-html-slim-export-snippet)
    ;; (fixed-width . org-html-slim-fixed-width)
    ;; (footnote-definition . org-html-slim-footnote-definition)
    ;; (footnote-reference . org-html-slim-footnote-reference)
    (headline . org-html-slim-headline)
    ;; (horizontal-rule . org-html-slim-horizontal-rule)
    ;; (inline-src-block . org-html-slim-inline-src-block)
    ;; (inlinetask . org-html-slim-inlinetask)
    (inner-template . org-html-slim-inner-template)
    (italic . org-html-slim-italic)
    (item . org-html-slim-item)
    ;; (keyword . org-html-slim-keyword)
    ;; (latex-environment . org-html-slim-latex-environment)
    ;; (latex-fragment . org-html-slim-latex-fragment)
    ;; (line-break . org-html-slim-line-break)
    (link . org-html-slim-link)
    (paragraph . org-html-slim-paragraph)
    (plain-list . org-html-slim-plain-list)
    (plain-text . org-html-slim-plain-text)
    ;; (planning . org-html-slim-planning)
    ;; (property-drawer . org-html-slim-property-drawer)
    ;; (quote-block . org-html-slim-quote-block)
    ;; (quote-section . org-html-slim-quote-section)
    ;; (radio-target . org-html-slim-radio-target)
    (section . org-html-slim-section)
    (special-block . org-html-slim-special-block)
    (src-block . org-html-slim-src-block)
    ;; (statistics-cookie . org-html-slim-statistics-cookie)
    ;; (strike-through . org-html-slim-strike-through)
    ;; (subscript . org-html-slim-subscript)
    ;; (superscript . org-html-slim-superscript)
    ;; (table . org-html-slim-table)
    ;; (table-cell . org-html-slim-table-cell)
    ;; (table-row . org-html-slim-table-row)
    ;; (target . org-html-slim-target)
    (template . org-html-slim-template)
    ;; (timestamp . org-html-slim-timestamp)
    ;; (underline . org-html-slim-underline)
    (verbatim . org-html-slim-verbatim)
    ;; (verse-block . org-html-slim-verse-block)
    )
  :export-block "HTML"
  :options-alist '((:template "TEMPLATE" nil (concat "<!DOCTYPE html>\n"
                                                     "<html>\n"
                                                     "<head><meta charset=\"utf-8\"/></head>\n"
                                                     "<body>\n"
                                                     ":yield:"
                                                     "</body>\n"
                                                     "</html>") t)
                   (:inner-template "INNER_TEMPLATE" nil nil t))
  :menu-entry '(?s "Export through html-slim"
                   ((?b "To temporary buffer" org-html-slim-export-as-html))))


;;;###autoload
(defun org-html-slim-publish-to-html (plist filename pub-dir)
  "Publish an org file to HTML using org-html-slim.

FILENAME is the filename of the Org file to be published.  PLIST
is the property list for the given project.  PUB-DIR is the
publishing directory.

Return output file name."
  (org-publish-org-to 'org-html-slim filename
                      (concat "." (or (plist-get plist :html-extension)
                                      org-html-extension "html"))
                      plist pub-dir))


;;;###autoload
(defun org-html-slim-export-as-html
    (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to an HTML buffer using org-html-slim.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting buffer should be accessible
through the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

When optional argument BODY-ONLY is non-nil, only write code
between \"<body>\" and \"</body>\" tags.

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

Export is done in a buffer named \"*Org html-slim Export*\", which
will be displayed when `org-export-show-temporary-export-buffer'
is non-nil."
  (interactive)
  (org-export-to-buffer 'org-html-slim "*Org html-slim Export*"
    async subtreep visible-only body-only ext-plist
    (lambda () (set-auto-mode t))))



(provide 'org-html-slim)
;;; org-html-slim.el ends here
