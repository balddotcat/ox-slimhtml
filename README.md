[![MELPA](https://melpa.org/packages/ox-slimhtml-badge.svg)](https://melpa.org/#/ox-slimhtml)

# ox-slimhtml

**slimhtml** is an **emacs org mode export backend**. It is a set of transcoders for
common `org` elements which outputs minimal `HTML`. The aim is not to re-invent the
wheel over the default **org-mode HTML exporter** - as it tackles a much bigger, and
different problem - but to provide a small set of components for easier
customization of `HTML` output from `org`.

-   [github.com/balddotcat/ox-slimhtml](https://github.com/balddotcat/ox-slimhtml)
-   [bald.cat/slimhtml](http://bald.cat/slimhtml)

```
    (org-export-string-as "org content" 'slimhtml t info)
    (org-export-to-buffer 'slimhtml "*slimhtml*")

    (org-export-define-derived-backend 'custom-html-exporter
        'slimhtml                             ;; org's default exporter is 'html
      :translate-alist
      '((bold . ox-slimhtml-bold)             ;; technically, this is already set
        (special-block . org-html-special-block)))
```
-   to export an `org-mode` buffer with the Org Export Dispatcher, use `C-c C-e s h`.

---

The aim of `slimhtml` is to provide flexibility and full control over HTML output,
as provided by `org-export`. It is primarily of use to front-end developers, when
the verification of an exact DOM structure is of importance.

By providing minimal components that transcode `org` data into simple HTML
structures, one can extend or build from the ground up, adding features as required.
Naturally, this can benefit optimization efforts, code and content reuse, ease of
customizability for CSS and JavaScript - and the maintenance of custom projects,
especially when using `org-publish` (for blogs or custom widgets).

One often overlooked feature of `org-export` is `org-export-defined-derived-backend`
and the extensibility it provides through the easy definition and interchangeability
of transcoders for each `org` data type. `slimhtml` focuses only on the core
requirements of each transcoder - aside from reducing the complexity of the output,
it's goal is to help one identify which features are required in the current problem
space; it should also be helpful when a completely different structure is required.

`slimhtml` does it's best to retain the current conventions (and settings) already
established through the default HTML exporter, it just outputs less (or rather, a
slim amount of) data - with one minor exception; it allows for `org-macro` expansion
within the PREAMBLE, POSTAMBLE, HEADER, and FOOTER export settings, which is great
for templating.

As a concrete example, the difference between the default exporter and `slimhtml` is;


    <div id="outline-container-orga33f743" class="outline-2">
    <h2 id="orga33f743"><span class="section-number-2">1</span> slimhtml</h2>
    <div class="outline-text-2" id="text-1">
    <p>
    <b>slimhtml</b> is an <b>emacs org mode export backend</b>. It is a set of transcoders for
    common <code>org</code> elements which outputs minimal <code>HTML</code>. The aim is not to re-invent the wheel over the default <b>org-mode HTML exporter</b> - as it tackles a much bigger, and
    different problem - but to provide a small set of components for easier
    customization of <code>HTML</code> output from <code>org</code>.
    </p>



    <h1>slimhtml</h1>
    <p>
    <strong>slimhtml</strong> is an <strong>emacs org mode export backend</strong>. It is a set of transcoders for common <kbd>org</kbd> elements which outputs minimal <kbd>HTML</kbd>. The aim is not to re-invent the wheel over the default <strong>org-mode HTML exporter</strong> - as it tackles a much bigger, and different problem - but to provide a small set of components for easier
    customization of <kbd>HTML</kbd> output from <kbd>org</kbd>.
    </p>



## org-mode HTML export transcoders


### formatting

    *bold*                                     # <strong>bold</strong>
    /italic/                                   # <em>italic</em>
    =verbatim=                                 # <kbd>verbatim</kbd>


### headlines

    * headline text                            # <section class="container">
      :PROPERTIES:                             # <h1 class="headline">headline text</h1>
      :attr_html: :class headline              # </section>
      :html_container: section
      :html_container_class: container
      :END:

    #+OPTIONS: H:[headline level]
    #+HTML_CONTAINER: [default container]


### links

    #+attr_html: :class link                   # <a href="link" class="link">content</a>
    [[link][content]]

    #+OPTIONS: html-link-org-files-as-html:[t/nil] || org-html-link-org-files-as-html
    #+HTML_EXTENSION: [html] || org-html-extension

    #+OPTIONS: html-link-use-abs-url:[t/nil] || org-html-link-use-abs-url


### plain lists

    #+attr_html: :class this                   # <ul class="this">
    - item 1                                   # <li>item 1</li>
    - item 2                                   # <li>item 2</li>
                                               # </ul>

    + item 1                                   # <ol><li>item 1</li></ol>
    - definition :: list                       # <dl><dt>definition</dt><dd>list</dd></dl>


### paragraphs

    #+attr_html: :class this                   # <p class="this">content</p>
    content


### blocks


#### quotes

    #+BEGIN_QUOTE                              # <blockquote>content</blockquote>
    content
    #+END_QUOTE


#### examples

    #+BEGIN_EXAMPLE                            # content
    content
    #+END_EXAMPLE


#### raw html

    #+BEGIN_EXPORT html                        # <span>export block</span>
      <span>export block</span>
    #+END_EXPORT

    #+BEGIN_EXPORT javascript                  # <script>console.log()</script>
      console.log()
    #+END_EXPORT

    #+BEGIN_EXPORT css                         # <style type="text/css">span{}</style>
      span {}
    #+END_EXPORT


#### snippet

    @@html:<span>snippet</span>@@              # <span>snippet</span>


#### special block

    #+attr_html: :type text/css                # <style type="text/css">
    #+BEGIN_STYLE                              # p { font-weight:500; }
      p { font-weight:500; }                   # </style>
    #+END_STYLE


#### source code

    #+BEGIN_SRC javascript                     # <pre>
      code                                     # <code class="javascript">code</code>
    #+END_SRC                                  # </pre>


### template


#### body

    #+HTML_PREAMBLE: preamble {{{macro}}}      # preamble
    content                                    # content
    #+HTML_POSTAMBLE: postamble {{{macro}}}    # postamble


#### html page

    #+HTML_DOCTYPE: || org-html-doctype        # <!DOCTYPE html>   ; html5
    #+HTML_HEAD: || org-html-head              # <html lang="en">  ; when language is set
    #+HTML_TITLE: %t                           #   <head>
    #+HTML_HEAD_EXTRA: || org-html-head-extra  #     head
    #+HTML_BODY_ATTR: id="test"                #     <title>document title</title>
    #+HTML_HEADER: {{{macro}}}                 #     head-extra
    #+HTML_FOOTER: {{{macro}}}                 #   </head>
                                               #   <body id="test">
                                               #     header
                                               #     content
                                               #     footer
                                               #   </body>
                                               # </html>

    {{{macro}}} tokens can also be set in INFO;
    :html-head, :html-head-extra and :html-header.

    :html-title is a string with optional tokens;
    %t is the document's #+TITLE: property.


## org-mode publishing function

    (setq org-publish-project-alist
          '(("project-name"
             :base-directory "~/src"
             :publishing-directory "~/public"
             :publishing-function ox-slimhtml-publish-to-html)))


## tests

    emacs -batch \
          -l ert \
          -l ox-slimhtml-tests.el \
          -f ert-run-tests-batch-and-exit


---


## notes

v0.4.5

    Switch the order: <pre><code> instead of <code><pre>
    https://www.w3.org/TR/html50/grouping-content.html#the-pre-element
