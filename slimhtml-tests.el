
(require 'package)
(package-initialize)

(load-file "slimhtml.el")


(defun should-render-as (expected-result org-source &optional info)
  (should (string= (concat expected-result "\n") (org-export-string-as org-source 'slimhtml t info))))


(ert-deftest slimhtml-bold ()
  (should-render-as "<p><strong>this</strong></p>" "*this*"))

(ert-deftest slimhtml-export-block ()
  :expected-result :failed
  (should-render-as "<img src=\"this.png\"\\>\n"
                    "#+BEGIN_HTML\n<img src=\"this.png\"\\>\n#+END_HTML"))

(ert-deftest slimhtml-export-snippet ()
  :expected-result :failed
  (should-render-as "<img src=\"this.png\"\\>"
                    "@@html:<img src=\"this.png\"\\>@@"))

(ert-deftest slimhtml-headline ()
  :expected-result :failed
  (should-render-as "<h1>this</h1>" "* this\n")
  (should-render-as "<h1>this</h1>"
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
  :expected-result :failed
  (should-render-as "<li>this\n</li>" " - this"))

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
  :expected-result :failed
  (should-render-as "<p>this</p>" "this")
  (should-render-as "<p class=\"this\">paragraph</p>"
                    "#+attr_html: :class this\nparagraph")
  (should-render-as "<li>list item\nthis\n</li>"
                    "- list item\n  this")
  (should-render-as "<style>#this{color:#f73;}\n</style>"
                    "#+BEGIN_STYLE\n#this{color:#f73;}\n#+END_STYLE"))

(ert-deftest slimhtml-plain-list ()
  :expected-result :failed
  (should-render-as "<ul><li>this\n</li>\n</ul>\n" " - this")
  (should-render-as "<ul class=\"this\"><li>item\n</li>\n</ul>\n"
                    "#+attr_html: :class this\n - item"))

(ert-deftest slimhtml-plain-text ()
  :expected-result :failed
  (should-render-as "<p>&amp;&lt;&gt;</p>" "&<>"))

(ert-deftest slimhtml-section ()
  (should-render-as "<p>this</p>" "this"))

(ert-deftest slimhtml-special-block ()
  :expected-result :failed
  (should-render-as "<style>#this{color:#f73;}\n</style>"
                    "#+BEGIN_STYLE\n#this{color:#f73;}\n#+END_STYLE")
  (should-render-as "<style type=\"text/css\">#id{color:#f73;}\n</style>"
                    "#+attr_html: :type text/css\n#+BEGIN_STYLE\n#id{color:#f73;}\n#+END_STYLE"))

(ert-deftest slimhtml-src-block ()
  :expected-result :failed
  (should-render-as "<code class=\"lisp\"><pre>(message 'this)</pre></code>"
                    "#+BEGIN_SRC lisp\n  (message 'this)\n#+END_SRC")
  (should-render-as "<code class=\"sh\"><pre>&amp;&lt;&gt;</pre></code>"
                    "#+BEGIN_SRC sh\n  &<>\n#+END_SRC"))

(ert-deftest slimhtml-template ()
  :expected-result :failed
  (should-render-as "<article><p>this</p>\n</article>" "this"
                    '(:inner-template "<article>:yield:</article>"))
  (should-render-as "<article><p>this</p>\n</article>" "this"
                    '(:inner-template (lambda (contents info)
                                        (format "<article>%s</article>" contents)))))

(ert-deftest slimhtml-verbatim ()
  :expected-result :failed
  (should-render-as "<kbd>this</kbd>" "=this="))


(ert-deftest slimhtml:attr ()
  :expected-result :failed
  (should-render-as "<h1 class=\"this\">headline</h1>"
                    "* headline\n :PROPERTIES:\n:attr_html: :class this\n:END:\n")
  (should-render-as "<p class=\"this\">paragraph</p>"
                    "#+attr_html: :class this\nparagraph")
  (should-render-as "<ul class=\"this\"><li>item\n</li>\n</ul>\n"
                    "#+attr_html: :class this\n - item")
  (should-render-as "<style type=\"text/css\">#id{color:#f73;}\n</style>"
                    "#+attr_html: :type text/css\n#+BEGIN_STYLE\n#id{color:#f73;}\n#+END_STYLE"))
