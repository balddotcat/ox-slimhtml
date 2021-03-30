;;; ox-slimhtml-tests.el --- tests for slimhtml -*- lexical-binding: t; -*-
;; Copyright (C) 2021 Free Software Foundation Inc.

;; Author: Elo Laszlo <hello at bald dot cat>
;; Created: August 2016
;; Package-Version: 0.5.0
;; Keywords: files
;; Package-Requires: ((emacs "24") (cl-lib "0.6"))

;; This file is part of GNU Emacs

;;; License:
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
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; slimhtml is an Emacs org mode export backend.  It is a set of
;; transcoders for common org elements which outputs minimal
;; HTML.  The aim is not to re-invent the wheel over the default
;; org-mode HTML exporter - as it tackles a much bigger, and
;; different problem - but to provide a small set of components for
;; easier customization of HTML output from org.

;;; Code:
(require 'package)
(package-initialize)

(load-file "ox-slimhtml.el")

(defun should-render-as (expected-result org-source &optional info skip-newline)
  (let ((expected-result (if skip-newline expected-result (concat expected-result "\n")))
        (info (plist-put info :html-container nil)))
    (should (string= expected-result
                     (org-export-string-as org-source 'slimhtml t info)))))

(ert-deftest ox-slimhtml-bold ()
  (should-render-as "<p><strong>this</strong></p>" "*this*"))

(ert-deftest ox-slimhtml-export-block ()
  (should-render-as "<img src=\"this.png\"\\>"
                    "#+BEGIN_EXPORT html\n<img src=\"this.png\"\\>\n#+END_EXPORT"))

(ert-deftest ox-slimhtml-export-snippet ()
  (should-render-as "<p><img src=\"this.png\"\\></p>"
                    "@@html:<img src=\"this.png\"\\>@@"))

(ert-deftest ox-slimhtml-headline ()
  (should-render-as "<h1>this</h1>" "* this\n")
  (should-render-as "<h1 class=\"this\">headline</h1>"
                    "* headline\n :PROPERTIES:\n:attr_html: :class this\n:END:\n")
  (should-render-as "<h1>one</h1><h2>two</h2><ul><li>three<ul><li>four</li></ul>\n</li></ul>"
                    "#+OPTIONS: H:2\n* one\n** two\n*** three\n**** four\n"))

(ert-deftest ox-slimhtml-container ()
  (should-render-as "<p>content</p>"
                    "#+HTML_CONTAINER: div\n#+HTML_CONTAINER: \ncontent")
  (should-render-as "<section class=\"this\"><h1>headline</h1>\n</section>"
                    "#+HTML_CONTAINER: section class=\"this\"\n* headline\n" nil 't)
  (should-render-as "<section><h1>headline</h1></section>"
                    "* headline\n:PROPERTIES:\n:html_container: section\n:END:")
  (should-render-as "<section class=\"this\"><h1>headline</h1></section>"
                    "* headline\n:PROPERTIES:\n:html_container: section\n:html_container_class: this\n:END:"))

(ert-deftest ox-slimhtml-inner-template ()
  (should (string= "<article id=\"test\"><p>content</p>\n</article>"
                   (org-export-string-as (concat "#+HTML_CONTAINER: \n"
                                                 "#+HTML_PREAMBLE: <article id=\"test\">\n"
                                                 "#+HTML_POSTAMBLE: </article>\ncontent")
                                         'slimhtml t))))

(ert-deftest ox-slimhtml-italic ()
  (should-render-as "<p><em>this</em></p>" "/this/"))

(ert-deftest ox-slimhtml-item ()
  (should-render-as "<ul><li>this</li>\n</ul>" "\n - this"))

(ert-deftest ox-slimhtml-link ()
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
                    "[[file:link.org][content]]" '(:html-link-org-as-html nil))
  (should-render-as "<p><a href=\"link.org\">content</a></p>"
                    "[[file:link.org][content]]" '(:html-link-org-as-html nil))
  (should-render-as "<p><a href=\"link.html\">content</a></p>"
                    "[[file:link.org][content]]" '(:html-link-org-as-html t :html-extension "html"))
  (should-render-as "<p><a href=\"link\">content</a></p>"
                    "[[file:link.org][content]]" '(:html-link-org-as-html t :html-extension ""))

  "absolute file paths are retained"
  (should-render-as "<p><a href=\"file:/link.html\">content</a></p>"
                    "#+OPTIONS: html-link-use-abs-url:t\n[[file:/link.org][content]]")

  "html attributes can be set"
  (should-render-as "<p><a href=\"link\" class=\"this\">content</a></p>"
                    "#+attr_html: :class this\n[[link][content]]")

  "avoid double link creation"
  (should-render-as "<p><a href=\"http://link\">http://content</a></p>"
                    "[[http://link][http://content]]")

  "unrecognized link types are rendered as is"
  (should-render-as "<p><a href=\"mailto:user@localhost\">content</a></p>"
                    "[[mailto:user@localhost][content]]"))

(ert-deftest ox-slimhtml-paragraph ()
  (should-render-as "<p>this</p>" "this")
  (should-render-as "<p class=\"this\">paragraph</p>"
                    "#+attr_html: :class this\nparagraph")
  (should-render-as "<ul><li>list item\nthis</li>\n</ul>"
                    "- list item\n  this")
  (should-render-as "<style>#this{color:#f73;}\n</style>"
                    "#+BEGIN_STYLE\n#this{color:#f73;}\n#+END_STYLE"))

(ert-deftest ox-slimhtml-plain-list ()
  (should-render-as "<ul><li>this</li>\n</ul>" " - this")
  (should-render-as "<ul class=\"this\"><li>item</li>\n</ul>"
                    "#+attr_html: :class this\n - item"))

(ert-deftest ox-slimhtml-plain-text ()
  (should-render-as "<p>&amp;&lt;&gt;</p>" "&<>"))

(ert-deftest ox-slimhtml-section ()
  (should-render-as "<p>this</p>" "this"))

(ert-deftest ox-slimhtml-special-block ()
  (should-render-as "<style>#this{color:#f73;}\n</style>"
                    "#+BEGIN_STYLE\n#this{color:#f73;}\n#+END_STYLE")
  (should-render-as "<style type=\"text/css\">#id{color:#f73;}\n</style>"
                    "#+attr_html: :type text/css\n#+BEGIN_STYLE\n#id{color:#f73;}\n#+END_STYLE"))

(ert-deftest ox-slimhtml-src-block ()
  (should-render-as "<pre><code class=\"lisp\">(message 'this)\n</code></pre>"
                    "#+BEGIN_SRC lisp\n  (message 'this)\n#+END_SRC")
  (should-render-as "<pre><code class=\"sh\">&amp;&lt;&gt;\n</code></pre>"
                    "#+BEGIN_SRC sh\n  &<>\n#+END_SRC"))

(ert-deftest ox-slimhtml-template ()
  (should (org-export-string-as "" 'slimhtml))
  (let ((expected-result
         (concat "<!DOCTYPE html>\n"
                 "<html lang=\"en\">\n"
                 "<head>\n"
                 "<meta charset=\"utf-8\">\n"
                 "<title>template-test</title>\n"
                 "<link rel=\"stylesheet\" href=\"\" type=\"text/css\">\n"
                 "</head>\n"
                 "<body id=\"test\"><nav/><article>\npreamble"
                 "<p><a href=\"file:/test-link\">contents</a></p>\n"
                 "postamble\n</article><footer/></body>\n"
                 "</html>"))
        (org-source
         (concat "#+HTML_DOCTYPE: html5\n"
                 "#+HTML_HEAD: <meta charset=\"utf-8\">\n"
                 "#+TITLE: template-test\n"
                 "#+HTML_HEAD_EXTRA: <link rel=\"stylesheet\" href=\"\" type=\"text/css\">\n"
                 "#+HTML_HEADER: <nav/>\n"
                 "#+HTML_BODY_ATTR: id=\"test\"\n"
                 "#+HTML_PREAMBLE: <article>\n"
                 "#+HTML_PREAMBLE: preamble\n"
                 "#+HTML_POSTAMBLE: postamble\n"
                 "#+HTML_POSTAMBLE: </article>\n"
                 "#+HTML_FOOTER: <footer/>\n"
                 "#+OPTIONS: html-link-org-files-as-html:t\n"
                 "#+OPTIONS: html-link-use-abs-url:t\n"
                 "#+HTML_EXTENSION: \n"
                 "#+HTML_CONTAINER: \n"
                 "#+HTML_LINK_HOME: /test-directory\n\n"
                 "[[file:/test-link.org][contents]]")))
    (should (string= expected-result
                     (org-export-string-as org-source 'slimhtml))))
  "title tag"
  (let ((expected-result
         (concat "<!DOCTYPE html>\n"
                 "<html lang=\"en\">\n"
                 "<head>\n"
                 "<title>template-test - site</title>\n"
                 "</head>\n"
                 "<body></body>\n"
                 "</html>"))
        (org-source
         (concat "#+HTML_DOCTYPE: html5\n"
                 "#+TITLE: template-test\n"
                 "#+HTML_TITLE: %t - site\n")))
    (should (string= expected-result
                     (org-export-string-as org-source 'slimhtml)))))

(ert-deftest ox-slimhtml-verbatim ()
  (should-render-as "<p><kbd>this</kbd></p>" "=this=")
  (should-render-as "<p><kbd>&amp;&lt;&gt;</kbd></p>" "=&<>="))

(ert-deftest ox-slimhtml--attr ()
  (should-render-as "<h1 class=\"this\">headline</h1>"
                    "* headline\n :PROPERTIES:\n:attr_html: :class this\n:END:\n")
  (should-render-as "<p class=\"this\">paragraph</p>"
                    "#+attr_html: :class this\nparagraph")
  (should-render-as "<ul class=\"this\"><li>item</li>\n</ul>"
                    "#+attr_html: :class this\n - item")
  (should-render-as "<style type=\"text/css\">#id{color:#f73;}\n</style>"
                    "#+attr_html: :type text/css\n#+BEGIN_STYLE\n#id{color:#f73;}\n#+END_STYLE"))

(ert-deftest ox-slimhtml-expand-macros ()
  (should (string= (concat
                    "<!DOCTYPE html>\n"
                    "<html lang=\"hu\">\n"
                    "<head>\nHEAD\n"
                    "<title>test</title>\n"
                    "HEAD_EXTRA\n</head>\n"
                    "<body>"
                    "PREAMBLE<p>content</p>\nPOSTAMBLE"
                    "</body>\n</html>")
                   (org-export-string-as
                    (concat
                     "#+MACRO: head HEAD\n#+HTML_HEAD: {{{head}}}\n"
                     "#+MACRO: head-extra HEAD_EXTRA\n#+HTML_HEAD_EXTRA: {{{head-extra}}}\n"
                     "#+MACRO: preamble PREAMBLE\n#+HTML_PREAMBLE: {{{preamble}}}\n"
                     "#+MACRO: postamble POSTAMBLE\n#+HTML_POSTAMBLE: {{{postamble}}}\n"
                     "#+HTML_CONTAINER: \n"
                     "content")
                    'slimhtml nil '(:html-doctype "html5" :title "test" :language "hu")))))

;;; ox-slimhtml-tests.el ends here
