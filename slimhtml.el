
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
               (setq attributes (concat attributes " target=\"_blank\"")))
              ((string= "fuzzy" link-type)
               (setq href (org-export-solidify-link-text href))))
        (format "<a href=\"%s\"%s>%s</a>" href attributes contents)))))

(defun slimhtml-paragraph (paragraph contents info)
  (when contents
    (if (or (slimhtml-immediate-child-of-p paragraph 'item)
            (slimhtml-immediate-child-of-p paragraph 'special-block))
        contents
      (if (slimhtml-has-immediate-child-of-p paragraph 'link)
          (format "<p>%s</p>" contents)
        (format "<p%s>%s</p>" (slimhtml:attr paragraph) contents)))))

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

(defun slimhtml-has-immediate-child-of-p (element element-type)
  (org-element-map element element-type
    (lambda (link) (= (org-element-property :begin link)
                      (org-element-property :contents-begin element)))
    nil t))

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
  :options-alist '((:html-extension "HTML_EXTENSION" nil org-html-extension)
                   (:html-link-org-as-html nil "html-link-org-files-as-html" org-html-link-org-files-as-html)
                   (:html-doctype "HTML_DOCTYPE" nil org-html-doctype)
                   (:html-container "HTML_CONTAINER" nil org-html-container-element)
                   (:html-link-use-abs-url nil "html-link-use-abs-url" org-html-link-use-abs-url)
                   (:html-link-home "HTML_LINK_HOME" nil org-html-link-home)
                   (:html-preamble "HTML_PREAMBLE" nil "" newline)
                   (:html-postamble "HTML_POSTAMBLE" nil "" newline)
                   (:html-head "HTML_HEAD" nil org-html-head newline)
                   (:html-head-extra "HTML_HEAD_EXTRA" nil org-html-head-extra newline)))


(provide 'slimhtml)
