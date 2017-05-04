
(require 'package)
(package-initialize)

(load-file "slimhtml.el")


(defun should-render-as (expected-result org-source &optional info)
  (setq expected-result (concat expected-result "\n"))
  (should (string= expected-result (org-export-string-as org-source 'slimhtml t info))))


(ert-deftest slimhtml-bold ()
  (should-render-as "<p><strong>this</strong></p>" "*this*"))

(ert-deftest slimhtml-export-block ()
  (should-render-as "<img src=\"this.png\"\\>"
                    "#+BEGIN_HTML\n<img src=\"this.png\"\\>\n#+END_HTML"))

(ert-deftest slimhtml-export-snippet ()
  (should-render-as "<p><img src=\"this.png\"\\></p>"
                    "@@html:<img src=\"this.png\"\\>@@"))

(ert-deftest slimhtml-headline ()
  (should-render-as "<h1>this</h1>" "* this\n")
  (should-render-as "<h1 class=\"this\">headline</h1>"
                    "* headline\n :PROPERTIES:\n:attr_html: :class this\n:END:\n"))

(ert-deftest slimhtml-inner-template ()
  :expected-result :failed
  (should-render-as "<article><p>this</p>\n</article>" "this"
                    '(:inner-template "<article>:yield:</article>"))
  (should-render-as "<article><p>this</p>\n</article>" "this"
                    '(:inner-template (lambda (contents info)
                                        (format "<article>%s</article>" contents)))))

(ert-deftest slimhtml-italic ()
  (should-render-as "<p><em>this</em></p>" "/this/"))

(ert-deftest slimhtml-item ()
  (should-render-as "<ul><li>this\n</li>\n</ul>" "\n - this"))

(ert-deftest slimhtml-link ()
  :expected-result :failed
  (should-render-as "<a href=\"this\">contents</a>"
                    "[[this][contents]]")
  (should-render-as "<a href=\"./this\">contents</a>"
                    "[[file:./this.org][contents]]")
  (should-render-as "<a href=\"http://this.org\" target=\"_blank\">contents</a>"
                    "[[http://this.org][contents]]")
  (should-render-as "<a href=\"https://this.org\" target=\"_blank\">contents</a>"
                    "[[https://this.org][contents]]")
  (should-render-as "<a href=\"https://this.org\" target=\"_blank\">http://contents.org</a>"
                    "[[https://this.org][http://contents.org]]"))

(ert-deftest slimhtml-paragraph ()
  (should-render-as "<p>this</p>" "this")
  (should-render-as "<p class=\"this\">paragraph</p>"
                    "#+attr_html: :class this\nparagraph")
  (should-render-as "<ul><li>list item\nthis\n</li>\n</ul>"
                    "- list item\n  this")
  (should-render-as "<style>#this{color:#f73;}\n</style>"
                    "#+BEGIN_STYLE\n#this{color:#f73;}\n#+END_STYLE"))

(ert-deftest slimhtml-plain-list ()
  (should-render-as "<ul><li>this\n</li>\n</ul>" " - this")
  (should-render-as "<ul class=\"this\"><li>item\n</li>\n</ul>"
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
  :expected-result :failed
  (should-render-as "<article><p>this</p>\n</article>" "this"
                    '(:inner-template "<article>:yield:</article>"))
  (should-render-as "<article><p>this</p>\n</article>" "this"
                    '(:inner-template (lambda (contents info)
                                        (format "<article>%s</article>" contents)))))

(ert-deftest slimhtml-verbatim ()
  (should-render-as "<p><kbd>this</kbd></p>" "=this=")
  (should-render-as "<p><kbd>&amp;&lt;&gt;</kbd></p>" "=&<>="))


(ert-deftest slimhtml:attr ()
  (should-render-as "<h1 class=\"this\">headline</h1>"
                    "* headline\n :PROPERTIES:\n:attr_html: :class this\n:END:\n")
  (should-render-as "<p class=\"this\">paragraph</p>"
                    "#+attr_html: :class this\nparagraph")
  (should-render-as "<ul class=\"this\"><li>item\n</li>\n</ul>"
                    "#+attr_html: :class this\n - item")
  (should-render-as "<style type=\"text/css\">#id{color:#f73;}\n</style>"
                    "#+attr_html: :type text/css\n#+BEGIN_STYLE\n#id{color:#f73;}\n#+END_STYLE"))
