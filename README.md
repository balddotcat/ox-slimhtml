# slimhtml

**slimhtml** is an **emacs org mode export backend**. It is a set of transcoders for
common `org` elements which outputs minimal `HTML`. The aim is not to re-invent the
wheel over the default **org-mode HTML exporter** - as it tackles a much bigger, and
different problem - but to provide a small set of components for easier
customization of `HTML` output from `org`.

-   [github.com/balddotcat/slimhtml](https://github.com/balddotcat/slimhtml)
-   [bald.cat/code/slimhtml](http://bald.cat/code/slimhtml)

```
    (org-export-string-as "org content" 'slimhtml t info)
    (org-export-to-buffer 'slimhtml "*slimhtml*")

    (org-export-define-derived-backend 'custom-html-exporter
        'slimhtml                             ;; org's default exporter is 'html
      :translate-alist
      '((bold . slimhtml:bold)                ;; technically, this is already set
        (special-block . org-html-special-block)))
```

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

    #+BEGIN_SRC javascript                     # <code class="javascript">
      code                                     # <pre>code</pre>
    #+END_SRC                                  # </code>


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
    #+HTML_HEADER: {{{macro}}}                 #     <title>document title</title>
    #+HTML_FOOTER: {{{macro}}}                 #     head-extra
                                               #   </head>
                                               #   <body>
                                               #     preamble
                                               #     content
                                               #     postamble
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
             :publishing-function slimhtml-publish-to-html)))


## tests

    emacs -batch \
          -l ert \
          -l slimhtml-tests.el \
          -f ert-run-tests-batch-and-exit
