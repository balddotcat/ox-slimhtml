**[org-html-slim](http://manifold.io/project/org-html-slim)** is an **HTML export backend** for **org-mode**. It is a set of translators for common org elements which output minimal HTML, and use only a limited amount of pre-defined logic.

-   [source available on github](https://github.com/elolaszlo/org-html-slim)

# setup

    (load "~/lisp/org-html-slim.el")

    (setq org-publish-project-alist
          '(("project-name"
             :base-directory "~/src"
             :publishing-directory "~/public"
             :publishing-function org-html-slim-publish-to-html)))

-   For a quick usage overview and an example configuration, please see [web publishing with org-mode](http://manifold.io/blog/web-publishing-with-org-mode).

# usage


    (org-publish-project "project-name" t) ;; ignore cache, force publish all
    (org-html-slim-export-string "this")

Currently, the following translators are defined; **bold - export-block - export-snippet - headline - inner-template - italic - item - link - paragraph - plain-list - plain-text - section - special-block - src-block - template - verbatim**.

Both the `:template` and `:inner-template` project options can be set to a string containing a **:yield:** keyword, or a function which accepts CONTENTS and INFO.

The **title** and **date** properties - as used in the **info** plists - can be set at the document level, using `#+TITLE:` and `#+DATE:`.

# defining derived backends

**org-mode** and **org-export** are remarkably flexible tools, and there are a number of ways to customize the HTML output; an **org-mode export backend** is simply a collection of filters on org-mode elements.

With a **derived backend**, customizations can be collected and applied to specific elements as required, without effecting already defined filters - one can easily pick and choose between each piece of functionality. Please see [defining derived org-mode export backends](http://manifold.io/blog/defining-a-derived-backend) for more info.
