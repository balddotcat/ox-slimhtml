;;; slimhtml --- a minimal HTML org export backend
;; Copyright (C) Elo Laszlo 2017

;; Author: Elo Laszlo <laszlo@manifold.io>
;; Created: August 2016
;; Description: a minimal HTML org export backend
;; Homepage: http://manifold.io/project/slimhtml
;; Version: 0.2.0
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


;;; Code:
(require 'ox-html)
(require 'cl)

(defun slimhtml-bold (bold contents info)
  "Transcode BOLD from Org to HTML.

CONTENTS is the text with bold markup.
INFO is a plist holding contextual information.
--
*this*"
  (when contents
    (format "<strong>%s</strong>" contents)))

(defun slimhtml-export-block (export-block contents info)
  "Transcode an EXPORT-BLOCK element from Org to HTML.

CONTENTS is nil. INFO is a plist holding contextual information.
--
#+BEGIN_EXPORT html\nthis\n#+END_EXPORT"
  (let ((contents (org-element-property :value export-block)))
    (when contents (org-remove-indentation contents))))

(defun slimhtml-export-snippet (export-snippet contents info)
  "Transcode a EXPORT-SNIPPET object from Org to HTML.

CONTENTS is nil. INFO is a plist holding contextual information.
--
@@html:this@@"
  (let ((contents (org-element-property :value export-snippet)))
    (when contents contents)))

(defun slimhtml-headline (headline contents info)
  "Transcode HEADLINE from Org to HTML.

CONTENTS is the section as defined under the HEADLINE.
INFO is a plist holding contextual information.
--
* this
 :PROPERTIES:
 :attr_html: :class this
 :END:"
  (let ((text (org-export-data (org-element-property :title headline) info))
        (level (org-export-get-relative-level headline info))
        (attributes (org-element-property :ATTR_HTML headline)))
    (when attributes
      (setq attributes (format " %s" (org-html--make-attribute-string
                                      (org-export-read-attribute 'attr_html
                                       `(nil (attr_html ,(split-string attributes))))))))
    (format "<h%d%s>%s</h%d>%s" level (or attributes "") text level (or contents ""))))

(defun slimhtml-inner-template (contents info)
  "Return body of document string after HTML conversion.

CONTENTS is the transcoded contents string.
INFO is a plist holding export options.
--
#+HTML_CONTAINER: this id=\"this\"
org-html-container-element
#+HTML_SIGNATURE: this {{{THIS}}}"
  (let ((container (plist-get info :html-container))
        (signature (plist-get info :html-signature)))
    (when (and signature (not (string= "" signature)))
      (setq signature (slimhtml-expand-macros signature info)))
    (if (and container (not (string= "" contents)))
        (format "<%s>%s%s</%s>" container contents (or signature "")
                (cl-subseq container 0 (cl-search " " container)))
      contents)))

(defun slimhtml-italic (italic contents info)
  "Transcode ITALIC from Org to HTML.

CONTENTS is the text with italic markup.
INFO is a plist holding contextual information.
--
/this/"
  (when contents
    (format "<em>%s</em>" contents)))

(defun slimhtml-link (link contents info)
  "Transcode LINK from Org to HTML.

CONTENTS is the text of the link.
INFO is a plist holding contextual information.
--
[[this][contents]]

#+OPTIONS: html-link-org-files-as-html:t
org-html-link-org-files-as-html
#+HTML_EXTENSION: html
org-html-extension
#+HTML_LINK_HOME: /this
org-html-link-home

#+attr_html: :class this\n[[link][content]]
http and https links; target=\"_blank\""
  (if (slimhtml-immediate-child-of-p link 'link)
      (org-element-property :raw-link link)
    (if (not contents)
        (format "<em>%s</em>" (org-element-property :path link))
      (let ((link-type (org-element-property :type link))
            (href (org-element-property :raw-link link))
            (attributes (if (slimhtml-immediate-child-of-p link 'paragraph)
                            (slimhtml:attr (org-export-get-parent link)) "")))
        (cond ((string= "file" link-type)
               (let ((path (org-element-property :path link)))
                 (if (file-name-absolute-p path)
                     (setq href (concat "file:" path))
                   (let ((html-extension (plist-get info :html-extension))
                         (home (plist-get info :html-link-home))
                         (use-abs-url (plist-get info :html-link-use-abs-url))
                         (link-org-files-as-html (plist-get info :html-link-org-as-html)))
                     (when (and home use-abs-url)
                       (when (cl-search "./" path :end1 2 :end2 2) (setq path (cl-subseq path 2)))
                       (setq path (concat (file-name-as-directory home) path)))
                     (when (and link-org-files-as-html (string= "org" (downcase (file-name-extension path))))
                       (if (and html-extension (not (string= "" html-extension)))
                           (setq path (concat (file-name-sans-extension path) "." html-extension))
                         (setq path (file-name-sans-extension path))))
                     (setq href path)))))
              ((member link-type '("http" "https"))
               (setq attributes (concat attributes " target=\"_blank\""))))
        (format "<a href=\"%s\"%s>%s</a>" href attributes contents)))))

(defun slimhtml-paragraph (paragraph contents info)
  "Transcode a PARAGRAPH element from Org to HTML.

CONTENTS is the contents of the paragraph.
INFO is a plist holding contextual information.
--
#+attr_html: :class this
this"
  (when contents
    (if (or (slimhtml-immediate-child-of-p paragraph 'item)
            (slimhtml-immediate-child-of-p paragraph 'special-block))
        contents
      (if (slimhtml-has-immediate-child-of-p paragraph 'link)
          (format "<p>%s</p>" contents)
        (format "<p%s>%s</p>" (slimhtml:attr paragraph) contents)))))

(defun slimhtml-section (section contents info)
  "Transcode a SECTION element from Org to HTML.

CONTENTS is the contents of the section.
INFO is a plist holding contextual information."
  contents)

(defun slimhtml-plain-list (plain-list contents info)
  "Transcode a LIST string from Org to HTML.

CONTENTS is the contents of the list element.
INFO is a plist holding contextual information.
--
#+attr_html: :class this
- this"
  (when contents
    (let ((type (case (org-element-property :type plain-list)
                  (ordered "ol")
                  (unordered "ul")
                  (descriptive "dl"))))
      (format "<%s%s>%s</%s>" type (slimhtml:attr plain-list) contents type))))

(defun slimhtml-plain-text (plain-text info)
  "Transcode a TEXT string from Org to HTML.

TEXT is the string to transcode.
INFO is a plist holding contextual information."
  (org-html-encode-plain-text plain-text))

(defun slimhtml-special-block (special-block contents info)
  "Transcode SPECIAL-BLOCK from Org to HTML.

CONTENTS is the text within the #+BEGIN_ and #+END_ markers.
INFO is a plist holding contextual information.
--
#+attr_html: :type text/css
#+BEGIN_STYLEthis#+END_STYLE"
  (when contents
    (let ((block-type (downcase (org-element-property :type special-block))))
      (format "<%s%s>%s</%s>" block-type (slimhtml:attr special-block) contents block-type))))

(defun slimhtml-src-block (src-block contents info)
  "Transcode CODE from Org to HTML.

CONTENTS is the text of a #+BEGIN_SRC...#+END_SRC block.
INFO is a plist holding contextual information.
--
#+BEGIN_SRCthis#+END_SRC"
  (let ((code (org-html-format-code src-block info)))
    (when code
      (format "<code class=\"%s\"><pre>%s</pre></code>"
              (org-element-property :language src-block) code))))

(defun slimhtml-template (contents info)
  "Return full document string after HTML conversion.

CONTENTS is the transcoded contents string.
INFO is a plist holding export options.

{{{macro}}} tokens can be set in INFO; :html-head,
:html-head-extra, :html-preamble and :html-postamble.
--
#+HTML_DOCTYPE: | org-html-doctype
#+HTML_HEAD: | org-html-head
#+TITLE:
#+HTML_HEAD_EXTRA: | org-html-head-extra
#+HTML_PREAMBLE:
#+HTML_POSTAMBLE:
#+OPTIONS: html-link-org-files-as-html:t | org-html-link-org-files-as-html
#+OPTIONS: html-link-use-abs-url:t | org-html-link-use-abs-url
#+HTML_EXTENSION: | org-html-extension
#+HTML_LINK_HOME: | org-html-link-home"
  (let ((doctype (assoc (plist-get info :html-doctype) org-html-doctype-alist))
        (language (plist-get info :language))
        (head (slimhtml-expand-macros (plist-get info :html-head) info))
        (head-extra (slimhtml-expand-macros (plist-get info :html-head-extra) info))
        (title (plist-get info :title))
        (newline "\n"))
    (concat
     (when doctype (concat (cdr doctype) newline))
     "<html" (when language (concat " lang=\"" language "\"")) ">" newline
     "<head>" newline
     (when (not (string= "" head)) (concat head newline))
     (when title (concat "<title>" (if (listp title) (car title) title) "</title>" newline))
     (when (not (string= "" head-extra)) (concat head-extra newline))
     "</head>" newline
     "<body>"
     (or (slimhtml-expand-macros (plist-get info :html-preamble) info) "")
     contents
     (or (slimhtml-expand-macros (plist-get info :html-postamble) info) "")
     "</body>" newline
     "</html>")))

(defun slimhtml-verbatim (verbatim contents info)
  "Transcode VERBATIM string from Org to HTML.

CONTENTS is nil.
INFO is a plist holding contextual information."
  (let ((contents (org-html-encode-plain-text (org-element-property :value verbatim))))
    (when contents
      (format "<kbd>%s</kbd>" contents))))


(defun slimhtml:attr (element &optional property)
  "returns ELEMENT's HTML attributes as a string"
  (let ((attributes (org-export-read-attribute :attr_html element property)))
    (if attributes (concat " " (org-html--make-attribute-string attributes)) "")))

(defun slimhtml-immediate-child-of-p (element container-type)
  (let ((container (org-export-get-parent element)))
    (and (eq (org-element-type container) container-type)
         (= (org-element-property :begin element)
            (org-element-property :contents-begin container)))))

(defun slimhtml-has-immediate-child-of-p (element element-type)
  (org-element-map element element-type
    (lambda (link) (= (org-element-property :begin link)
                      (org-element-property :contents-begin element)))
    nil t))

(defun slimhtml-expand-macros (contents info)
  "Return CONTENTS string, with macros expanded.

CONTENTS is a string, optionally with {{{macro}}}
tokens. INFO is a plist holding export options."
  (if (cl-search "{{{" contents)
      (let* ((author (org-element-interpret-data (plist-get info :author)))
             (date (org-element-interpret-data (plist-get info :date)))
             (email (or (plist-get info :email) ""))
             (title (org-element-interpret-data (plist-get info :title)))
             (export-specific-templates
              (list (cons "author" author)
                    (cons "date"
                          (format "(eval (format-time-string \"$1\" '%s))"
                                  (org-read-date nil t date nil)))
                    (cons "email" email)
                    (cons "title" title)))
             (templates (org-combine-plists export-specific-templates
                                            org-macro-templates)))
        (with-temp-buffer (insert contents)
                          (org-macro-replace-all templates)
                          (buffer-string)))
    contents))


;; backend definition
(org-export-define-backend 'slimhtml
  '((bold . slimhtml-bold)
    (export-block . slimhtml-export-block)
    (export-snippet . slimhtml-export-snippet)
    (headline . slimhtml-headline)
    (inner-template . slimhtml-inner-template)
    (italic . slimhtml-italic)
    (item . org-html-item)
    (link . slimhtml-link)
    (paragraph . slimhtml-paragraph)
    (plain-list . slimhtml-plain-list)
    (plain-text . slimhtml-plain-text)
    (section . slimhtml-section)
    (special-block . slimhtml-special-block)
    (src-block . slimhtml-src-block)
    (template . slimhtml-template)
    (verbatim . slimhtml-verbatim))
  :options-alist
  '((:html-extension "HTML_EXTENSION" nil org-html-extension)
    (:html-link-org-as-html nil "html-link-org-files-as-html" org-html-link-org-files-as-html)
    (:html-doctype "HTML_DOCTYPE" nil org-html-doctype)
    (:html-container "HTML_CONTAINER" nil org-html-container-element space)
    (:html-link-use-abs-url nil "html-link-use-abs-url" org-html-link-use-abs-url)
    (:html-link-home "HTML_LINK_HOME" nil org-html-link-home)
    (:html-preamble "HTML_PREAMBLE" nil "" newline)
    (:html-postamble "HTML_POSTAMBLE" nil "" newline)
    (:html-head "HTML_HEAD" nil org-html-head newline)
    (:html-head-extra "HTML_HEAD_EXTRA" nil org-html-head-extra newline)
    (:html-signature "HTML_SIGNATURE" nil "" newline)))


;;;###autoload
(defun slimhtml-publish-to-html (plist filename pub-dir)
  "Publish an org file to HTML using org-html-slim.

FILENAME is the filename of the Org file to be published.  PLIST
is the property list for the given project.  PUB-DIR is the
publishing directory.

Return output file name."
  (let ((html-extension (or (plist-get plist :html-extension) org-html-extension)))
    (org-publish-org-to 'slimhtml
                        filename
                        (if (and html-extension (not (string= "" html-extension)))
                            (concat "." html-extension) "")
                        plist
                        pub-dir)))


(provide 'slimhtml)
;;; slimhtml.el ends here
