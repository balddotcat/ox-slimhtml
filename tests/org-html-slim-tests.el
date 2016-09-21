
(require 'package)
(package-initialize)

(require 's)
(load (expand-file-name "../org-html-slim.el"
                        (file-name-directory load-file-name)))


(defun should-render-as (source expected-result &optional info)
  (should (s-contains-p expected-result
                        (org-html-slim-export-string source info))))


(ert-deftest org-html-slim-sanity-check ()
  (should-render-as "this" "<p>this</p>\n"))

(ert-deftest org-html-slim-bold ()
  (should-render-as "*this*" "<strong>this</strong>"))


(ert-deftest org-html-slim-export-block ()
  (should-render-as "#+BEGIN_HTML\n<img src=\"this.png\"\\>\n#+END_HTML"
                    "<img src=\"this.png\"\\>\n"))


(ert-deftest org-html-slim-export-snippet ()
  (should-render-as "@@html:<img src=\"this.png\"\\>@@"
                    "<img src=\"this.png\"\\>"))


(ert-deftest org-html-slim-headline ()
  (should-render-as "* this\n"
                    "<h1>this</h1>")
  (should-render-as "* headline\n :PROPERTIES:\n:attr_html: :class this\n:END:\n"
                    "<h1 class=\"this\">headline</h1>"))


(ert-deftest org-html-slim-inner-template ()
  (should-render-as "this"
                    "<article><p>this</p>\n</article>"
                    '(:inner-template "<article>:yield:</article>"))
  (should-render-as "this"
                    "<article><p>this</p>\n</article>"
                    '(:inner-template (lambda (contents info)
                                        (format "<article>%s</article>" contents)))))


(ert-deftest org-html-slim-italic ()
  (should-render-as "/this/"
                    "<em>this</em>"))


(ert-deftest org-html-slim-item ()
  (should-render-as " - this"
                    "<li>this\n</li>"))


(ert-deftest org-html-slim-link ()
  (should-render-as "[[this][contents]]"
                    "<a href=\"this\">contents</a>")
  (should-render-as "[[file:./this.org][contents]]"
                    "<a href=\"./this\">contents</a>")
  (should-render-as "[[http://this.org][contents]]"
                    "<a href=\"http://this.org\" target=\"_blank\">contents</a>")
  (should-render-as "[[https://this.org][contents]]"
                    "<a href=\"https://this.org\" target=\"_blank\">contents</a>")
  (should-render-as "[[https://this.org][http://contents.org]]"
                    "<a href=\"https://this.org\" target=\"_blank\">http://contents.org</a>"))


(ert-deftest org-html-slim-paragraph ()
  (should-render-as "this"
                    "<p>this</p>")
  (should-render-as "#+attr_html: :class this\nparagraph"
                    "<p class=\"this\">paragraph</p>")
  (should-render-as "- list item\n  this"
                    "<li>list item\nthis\n</li>")
  (should-render-as "#+BEGIN_STYLE\n#this{color:#f73;}\n#+END_STYLE"
                    "<style>#this{color:#f73;}\n</style>"))


(ert-deftest org-html-slim-plain-list ()
  (should-render-as " - this"
                    "<ul><li>this\n</li>\n</ul>\n")
  (should-render-as "#+attr_html: :class this\n - item"
                    "<ul class=\"this\"><li>item\n</li>\n</ul>\n"))


(ert-deftest org-html-slim-plain-text ()
  (should-render-as "&<>"
                    "<p>&amp;&lt;&gt;</p>"))


(ert-deftest org-html-slim-section ()
  (should-render-as "this"
                    "<p>this</p>\n"))


(ert-deftest org-html-slim-special-block ()
  (should-render-as "#+BEGIN_STYLE\n#this{color:#f73;}\n#+END_STYLE"
                    "<style>#this{color:#f73;}\n</style>")
  (should-render-as "#+attr_html: :type text/css\n#+BEGIN_STYLE\n#id{color:#f73;}\n#+END_STYLE"
                    "<style type=\"text/css\">#id{color:#f73;}\n</style>"))


(ert-deftest org-html-slim-src-block ()
  (should-render-as "#+BEGIN_SRC lisp\n  (message 'this)\n#+END_SRC"
                    "<code class=\"lisp\"><pre>(message 'this)</pre></code>")
  (should-render-as "#+BEGIN_SRC sh\n  &<>\n#+END_SRC"
                    "<code class=\"sh\"><pre>&amp;&lt;&gt;</pre></code>"))


(ert-deftest org-html-slim-template ()
  (should (string= "<article><p>this</p>\n</article>"
                   (org-export-string-as
                    "this" 'org-html-slim nil
                    '(:template "<article>:yield:</article>"))))
  (should (string= "<article><p>this</p>\n</article>"
                   (org-export-string-as
                    "this" 'org-html-slim nil
                    '(:template (lambda (contents info)
                                  (format "<article>%s</article>" contents)))))))


(ert-deftest org-html-slim-verbatim ()
  (should-render-as "=this="
                    "<kbd>this</kbd>"))


(ert-deftest org-html-slim--element-attributes ()
  (should-render-as "* headline\n :PROPERTIES:\n:attr_html: :class this\n:END:\n"
                    "<h1 class=\"this\">headline</h1>")
  (should-render-as "#+attr_html: :class this\nparagraph"
                    "<p class=\"this\">paragraph</p>")
  (should-render-as "#+attr_html: :class this\n - item"
                    "<ul class=\"this\"><li>item\n</li>\n</ul>\n")
  (should-render-as "#+attr_html: :type text/css\n#+BEGIN_STYLE\n#id{color:#f73;}\n#+END_STYLE"
                    "<style type=\"text/css\">#id{color:#f73;}\n</style>"))
