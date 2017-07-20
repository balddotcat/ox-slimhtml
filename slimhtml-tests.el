
(require 'package)
(package-initialize)

(load-file "slimhtml.el")


(defun should-render-as (expected-result org-source &optional info)
  (setq expected-result (concat expected-result "\n"))
  (setq info (plist-put info :html-container nil))
  (should (string= expected-result (org-export-string-as org-source 'slimhtml t info))))


(ert-deftest slimhtml-bold ()
  (should-render-as "<p><strong>this</strong></p>" "*this*"))

(ert-deftest slimhtml-export-block ()
  (should-render-as "<img src=\"this.png\"\\>"
                    "#+BEGIN_EXPORT html\n<img src=\"this.png\"\\>\n#+END_EXPORT"))

(ert-deftest slimhtml-export-snippet ()
  (should-render-as "<p><img src=\"this.png\"\\></p>"
                    "@@html:<img src=\"this.png\"\\>@@"))

(ert-deftest slimhtml-headline ()
  (should-render-as "<h1>this</h1>" "* this\n")
  (should-render-as "<h1 class=\"this\">headline</h1>"
                    "* headline\n :PROPERTIES:\n:attr_html: :class this\n:END:\n"))

(ert-deftest slimhtml-inner-template ()
  (should (string= "<article><p>content</p>\n</article>"
                   (org-export-string-as "#+HTML_CONTAINER: article\ncontent" 'slimhtml t)))
  (should (string= "<article id=\"test\" class=\"something\"><p>content</p>\n</article>"
                   (org-export-string-as (concat "#+HTML_CONTAINER: article id=\"test\" class=\"something\"\n"
                                                 "content")
                                         'slimhtml t))))

(ert-deftest slimhtml-italic ()
  (should-render-as "<p><em>this</em></p>" "/this/"))

(ert-deftest slimhtml-item ()
  (should-render-as "<ul><li>this</li>\n</ul>" "\n - this"))

(ert-deftest slimhtml-link ()
  "fallback"
  (should-render-as "<p><em>link</em></p>"
                    "[[link]]")
  "renders fuzzy links as is"
  (should-render-as "<p><a href=\"link\">content</a></p>"
                    "[[link][content]]")
  (should-render-as "<p><a href=\"link.org\">content</a></p>"
                    "[[link.org][content]]" '(:html-link-org-as-html nil))
  "converts file links from .org to :html-extension"
  (should-render-as "<p><a href=\"link.org\">content</a></p>"
                    "[[link.org][content]]" '(:html-link-org-as-html nil))
  (should-render-as "<p><a href=\"link.org\">content</a></p>"
                    "[[file:link.org][content]]" '(:html-link-org-as-html nil))
  (should-render-as "<p><a href=\"link.html\">content</a></p>"
                    "[[file:link.org][content]]" '(:html-link-org-as-html t :html-extension "html"))
  (should-render-as "<p><a href=\"link\">content</a></p>"
                    "[[file:link.org][content]]" '(:html-link-org-as-html t :html-extension ""))
  "absolute file paths are retained"
  (should-render-as "<p><a href=\"file:/link.org\">content</a></p>"
                    "[[file:/link.org][content]]")
  "html attributes can be set"
  (should-render-as "<p><a href=\"link\" class=\"this\">content</a></p>"
                    "#+attr_html: :class this\n[[link][content]]")
  "http and https links; target=_blank"
  (should-render-as "<p><a href=\"http://link\" target=\"_blank\">content</a></p>"
                    "[[http://link][content]]")
  "avoid double link creation"
  (should-render-as "<p><a href=\"http://link\" target=\"_blank\">http://content</a></p>"
                    "[[http://link][http://content]]")
  "unrecognized link types are rendered as is"
  (should-render-as "<p><a href=\"mailto:user@localhost\">content</a></p>"
                    "[[mailto:user@localhost][content]]")
  ":html-link-home and :html-link-use-abs-url set"
  (should-render-as "<p><a href=\"http://localhost/link.html\">content</a></p>"
                    "[[file:link.org][content]]"
                    '(:html-link-use-abs-url t :html-link-home "http://localhost/" :html-extension "html"))
  "relative paths"
  (should-render-as "<p><a href=\"http://localhost/link.org\">content</a></p>"
                    "[[file:./link.org][content]]"
                    '(:html-link-use-abs-url t :html-link-home "http://localhost/" :html-link-org-as-html nil))
  (should-render-as "<p><a href=\"http://localhost/../link.org\">content</a></p>"
                    "[[file:../link.org][content]]"
                    '(:html-link-use-abs-url t :html-link-home "http://localhost/" :html-link-org-as-html nil)))

(ert-deftest slimhtml-paragraph ()
  (should-render-as "<p>this</p>" "this")
  (should-render-as "<p class=\"this\">paragraph</p>"
                    "#+attr_html: :class this\nparagraph")
  (should-render-as "<ul><li>list item\nthis</li>\n</ul>"
                    "- list item\n  this")
  (should-render-as "<style>#this{color:#f73;}\n</style>"
                    "#+BEGIN_STYLE\n#this{color:#f73;}\n#+END_STYLE"))

(ert-deftest slimhtml-plain-list ()
  (should-render-as "<ul><li>this</li>\n</ul>" " - this")
  (should-render-as "<ul class=\"this\"><li>item</li>\n</ul>"
                    "#+attr_html: :class this\n - item"))

(ert-deftest slimhtml-plain-text ()
  (should-render-as "<p>&amp;&lt;&gt;</p>" "&<>"))

(ert-deftest slimhtml-section ()
  (should-render-as "<p>this</p>" "this"))

(ert-deftest slimhtml-special-block ()
  (should-render-as "<style>#this{color:#f73;}\n</style>"
                    "#+BEGIN_STYLE\n#this{color:#f73;}\n#+END_STYLE")
  (should-render-as "<style type=\"text/css\">#id{color:#f73;}\n</style>"
                    "#+attr_html: :type text/css\n#+BEGIN_STYLE\n#id{color:#f73;}\n#+END_STYLE"))

(ert-deftest slimhtml-src-block ()
  (should-render-as "<code class=\"lisp\"><pre>(message 'this)\n</pre></code>"
                    "#+BEGIN_SRC lisp\n  (message 'this)\n#+END_SRC")
  (should-render-as "<code class=\"sh\"><pre>&amp;&lt;&gt;\n</pre></code>"
                    "#+BEGIN_SRC sh\n  &<>\n#+END_SRC"))

(ert-deftest slimhtml-template ()
  (should (org-export-string-as "" 'slimhtml))
  (let ((expected-result
         (concat "<!DOCTYPE html>\n"
                 "<html lang=\"en\">\n"
                 "<head>\n"
                 "<meta charset=\"utf-8\">\n"
                 "<title>template-test</title>\n"
                 "<link rel=\"stylesheet\" href=\"\" type=\"text/css\">\n"
                 "</head>\n"
                 "<body><article><nav/>"
                 "<p><a href=\"/test-directory/test-link\">contents</a></p>\n"
                 "<footer/></article></body>\n"
                 "</html>"))
        (org-source
         (concat "#+HTML_DOCTYPE: html5\n"
                 "#+HTML_HEAD: <meta charset=\"utf-8\">\n"
                 "#+TITLE: template-test\n"
                 "#+HTML_HEAD_EXTRA: <link rel=\"stylesheet\" href=\"\" type=\"text/css\">\n"
                 "#+HTML_CONTAINER: article\n"
                 "#+HTML_PREAMBLE: <nav/>\n"
                 "#+HTML_POSTAMBLE: <footer/>\n"
                 "#+OPTIONS: html-link-org-files-as-html:t\n"
                 "#+OPTIONS: html-link-use-abs-url:t\n"
                 "#+HTML_EXTENSION: \n"
                 "#+HTML_LINK_HOME: /test-directory\n\n"
                 "[[file:./test-link.org][contents]]")))
    (should (string= expected-result
                     (org-export-string-as org-source 'slimhtml))))
  "header tag"
  (let ((expected-result
         (concat "<!DOCTYPE html>\n"
                 "<html lang=\"en\">\n"
                 "<head>\n"
                 "<title>template-test</title>\n"
                 "</head>\n"
                 "<body><header><nav/> template-test</header>"
                 "<p>content</p>\n"
                 "<footer/></body>\n"
                 "</html>"))
        (org-source
         (concat "#+HTML_DOCTYPE: html5\n"
                 "#+TITLE: template-test\n"
                 "#+HTML_HEADER: <header>%n %t</header>\n"
                 "#+HTML_NAVIGATION: <nav/>\n"
                 "#+HTML_FOOTER: <footer/>\n"
                 "#+HTML_CONTAINER: \n"
                 "content")))
    (should (string= expected-result
                     (org-export-string-as org-source 'slimhtml))))
  "navigation without header tag"
  (let ((expected-result
         (concat "<!DOCTYPE html>\n"
                 "<html lang=\"en\">\n"
                 "<head>\n"
                 "<title>template-test</title>\n"
                 "</head>\n"
                 "<body><nav/><p>content</p>\n<footer/></body>\n"
                 "</html>"))
        (org-source
         (concat "#+HTML_DOCTYPE: html5\n"
                 "#+TITLE: template-test\n"
                 "#+HTML_NAVIGATION: <nav/>\n"
                 "#+HTML_CONTAINER: \n"
                 "#+HTML_FOOTER: <footer/>\n"
                 "content")))
    (should (string= expected-result
                     (org-export-string-as org-source 'slimhtml)))))

(ert-deftest slimhtml-verbatim ()
  (should-render-as "<p><kbd>this</kbd></p>" "=this=")
  (should-render-as "<p><kbd>&amp;&lt;&gt;</kbd></p>" "=&<>="))


(ert-deftest slimhtml:attr ()
  (should-render-as "<h1 class=\"this\">headline</h1>"
                    "* headline\n :PROPERTIES:\n:attr_html: :class this\n:END:\n")
  (should-render-as "<p class=\"this\">paragraph</p>"
                    "#+attr_html: :class this\nparagraph")
  (should-render-as "<ul class=\"this\"><li>item</li>\n</ul>"
                    "#+attr_html: :class this\n - item")
  (should-render-as "<style type=\"text/css\">#id{color:#f73;}\n</style>"
                    "#+attr_html: :type text/css\n#+BEGIN_STYLE\n#id{color:#f73;}\n#+END_STYLE"))

(ert-deftest slimhtml-expand-macros ()
  (should (string= (concat
                    "<!DOCTYPE html>\n"
                    "<html lang=\"hu\">\n"
                    "<head>\nHEAD\n"
                    "<title>test</title>\n"
                    "HEAD_EXTRA\n</head>\n"
                    "<body>"
                    "<div>PREAMBLE<p>content</p>\nSIGNATUREPOSTAMBLE</div>"
                    "</body>\n</html>")
                   (org-export-string-as
                    (concat
                     "#+MACRO: head HEAD\n#+HTML_HEAD: {{{head}}}\n"
                     "#+MACRO: head-extra HEAD_EXTRA\n#+HTML_HEAD_EXTRA: {{{head-extra}}}\n"
                     "#+MACRO: preamble PREAMBLE\n#+HTML_PREAMBLE: {{{preamble}}}\n"
                     "#+MACRO: postamble POSTAMBLE\n#+HTML_POSTAMBLE: {{{postamble}}}\n"
                     "#+MACRO: signature SIGNATURE\n#+HTML_SIGNATURE: {{{signature}}}\n"
                     "content")
                    'slimhtml nil '(:html-doctype "html5" :title "test" :language "hu")))))
