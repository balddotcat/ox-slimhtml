
(require 'package)
(package-initialize)

(load-file "slimhtml.el")


(defun should-render-as (expected-result org-source &optional info)
  (should (string= (concat expected-result "\n") (org-export-string-as org-source 'slimhtml t info))))


(ert-deftest slimhtml-bold ()
  (should-render-as "<p><strong>this</strong></p>" "*this*"))

(ert-deftest slimhtml-export-block ()
  :expected-result :failed
  (should-render-as "" ""))

(ert-deftest slimhtml-export-snippet ()
  :expected-result :failed
  (should-render-as "" ""))

(ert-deftest slimhtml-headline ()
  :expected-result :failed
  (should-render-as "" ""))

(ert-deftest slimhtml-inner-template ()
  :expected-result :failed
  (should-render-as "" ""))

(ert-deftest slimhtml-italic ()
  (should-render-as "<p><em>this</em></p>" "/this/"))

(ert-deftest slimhtml-item ()
  :expected-result :failed
  (should-render-as "" ""))

(ert-deftest slimhtml-link ()
  :expected-result :failed
  (should-render-as "" ""))

(ert-deftest slimhtml-paragraph ()
  (should-render-as "<p>this</p>" "this")
  (should-render-as "<p class=\"this\">paragraph</p>" "#+attr_html: :class this\nparagraph"))

(ert-deftest slimhtml-plain-list ()
  :expected-result :failed
  (should-render-as "" ""))

(ert-deftest slimhtml-plain-text ()
  :expected-result :failed
  (should-render-as "" ""))

(ert-deftest slimhtml-section ()
  (should-render-as "<p>this</p>" "this"))

(ert-deftest slimhtml-special-block ()
  :expected-result :failed
  (should-render-as "" ""))

(ert-deftest slimhtml-src-block ()
  :expected-result :failed
  (should-render-as "" ""))

(ert-deftest slimhtml-template ()
  :expected-result :failed
  (should-render-as "" ""))

(ert-deftest slimhtml-verbatim ()
  :expected-result :failed
  (should-render-as "" ""))
