
(require 'ox-publish)
(require 'ox-html)
(require 'cl)


(defun slimhtml-bold (bold contents info)
  (when contents
    (format "<strong>%s</strong>" contents)))

(defun slimhtml-export-block (export-block contents info)
  (let ((contents (org-element-property :value export-block)))
    (when contents (org-remove-indentation contents))))

(defun slimhtml-export-snippet (export-snippet contents info)
  (let ((contents (org-element-property :value export-snippet)))
    (when contents contents)))

(defun slimhtml-headline (headline contents info)
  (let ((text (org-export-data (org-element-property :title headline) info))
        (level (org-export-get-relative-level headline info))
        (attributes (org-element-property :ATTR_HTML headline)))
    (when attributes
      (setq attributes (format " %s" (org-html--make-attribute-string
                                      (org-export-read-attribute 'attr_html
                                       `(nil (attr_html ,(split-string attributes))))))))
    (format "<h%d%s>%s</h%d>%s" level (or attributes "") text level (or contents ""))))

(defun slimhtml-inner-template (contents info)
  contents)

(defun slimhtml-italic (italic contents info)
  (when contents
    (format "<em>%s</em>" contents)))

(defun slimhtml-link (link contents info)
  contents)

(defun slimhtml-paragraph (paragraph contents info)
  (when contents
    (if (or (slimhtml-immediate-child-of-p paragraph 'item)
            (slimhtml-immediate-child-of-p paragraph 'special-block))
        contents
      (format "<p%s>%s</p>" (slimhtml:attr paragraph) contents))))

(defun slimhtml-section (section contents info)
  contents)

(defun slimhtml-plain-list (plain-list contents info)
  (when contents
    (let ((type (case (org-element-property :type plain-list)
                  (ordered "ol")
                  (unordered "ul")
                  (descriptive "dl"))))
      (format "<%s%s>%s</%s>" type (slimhtml:attr plain-list) contents type))))

(defun slimhtml-plain-text (plain-text info)
  (org-html-encode-plain-text plain-text))

(defun slimhtml-special-block (special-block contents info)
  (when contents
    (let ((block-type (downcase (org-element-property :type special-block))))
      (format "<%s%s>%s</%s>" block-type (slimhtml:attr special-block) contents block-type))))

(defun slimhtml-src-block (src-block contents info)
  (let ((code (org-html-format-code src-block info)))
    (when code
      (format "<code class=\"%s\"><pre>%s</pre></code>"
              (org-element-property :language src-block) code))))

(defun slimhtml-template (contents info)
  contents)

(defun slimhtml-verbatim (verbatim contents info)
  (let ((contents (org-html-encode-plain-text (org-element-property :value verbatim))))
    (when contents
      (format "<kbd>%s</kbd>" contents))))


(defun slimhtml:attr (element &optional property)
  (let ((attributes (org-export-read-attribute :attr_html element property)))
    (if attributes (concat " " (org-html--make-attribute-string attributes)) "")))

(defun slimhtml-immediate-child-of-p (element container-type)
  (let ((container (org-export-get-parent element)))
    (and (eq (org-element-type container) container-type)
         (= (org-element-property :begin element)
            (org-element-property :contents-begin container)))))


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
  :options-alist '((:html-doctype "DOCTYPE" nil "html5" t)
                   (:html-preamble "" )
                   (:html-postamble "" ))
  )


(provide 'slimhtml)
