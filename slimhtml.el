
(require 'ox-publish)


(defun slimhtml-bold (bold contents info)
  (when contents
    (format "<strong>%s</strong>" contents)))

(defun slimhtml-export-block (export-block contents info)
  contents)

(defun slimhtml-export-snippet (export-snippet contents info)
  contents)

(defun slimhtml-headline (headline contents info)
  contents)

(defun slimhtml-inner-template (contents info)
  contents)

(defun slimhtml-italic (italic contents info)
  (when contents
    (format "<em>%s</em>" contents)))

(defun slimhtml-item (item contents info)
  contents)

(defun slimhtml-link (link contents info)
  contents)

(defun slimhtml-paragraph (paragraph contents info)
  (when contents
    (format "<p%s>%s</p>" (slimhtml:attr paragraph) contents)))

(defun slimhtml-section (section contents info)
  contents)

(defun slimhtml-plain-list (plain-list contents info)
  contents)

(defun slimhtml-plain-text (plain-text contents info)
  contents)

(defun slimhtml-special-block (special-block contents info)
  contents)

(defun slimhtml-src-block (src-block contents info)
  contents)

(defun slimhtml-template (contents info)
  contents)

(defun slimhtml-verbatim (verbatim contents info)
  contents)


(defun slimhtml:attr (element &optional property)
  (let ((attributes (org-export-read-attribute :attr_html element property)))
    (if attributes (concat " " (org-html--make-attribute-string attributes)) "")))


(org-export-define-backend 'slimhtml
  '((bold . slimhtml-bold)
    ;; (export-block . slimhtml-export-block)
    ;; (export-snippet . slimhtml-export-snippet)
    ;; (headline . slimhtml-headline)
    (inner-template . slimhtml-inner-template)
    (italic . slimhtml-italic)
    ;; (item . slimhtml-item)
    ;; (link . slimhtml-link)
    (paragraph . slimhtml-paragraph)
    ;; (plain-list . slimhtml-plain-list)
    ;; (plain-text . slimhtml-plain-text)
    (section . slimhtml-section)
    ;; (special-block . slimhtml-special-block)
    ;; (src-block . slimhtml-src-block)
    (template . slimhtml-template)
    ;; (verbatim . slimhtml-verbatim)
    ))


(provide 'slimhtml)
