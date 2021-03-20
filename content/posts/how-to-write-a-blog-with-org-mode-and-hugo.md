+++
title = "How to write a blog with org-mode and hugo."
date = 2021-03-20
lastmod = 2021-03-20T19:19:17+01:00
tags = ["org-mode", "hugo"]
draft = true
categories = ["emacs"]
[menu.main]
  weight = 1001
  identifier = "how-to-write-a-blog-with-org-mode-and-hugo-dot"
+++

Build a blog with Emacs, Hugo and org-mode its more easy than your think. You need to learn some basic
concepts about hugo and org-mode itself.


## Requirements. {#requirements-dot}

We need some tools to archive this. In this post we learn how to install some of them.

-   Emacs
    There are tutorials on internet on how to install Emacs for all the operative systems.
    On linux it's easy to install with your default package manager or guix package manager.
-   Hugo
    In the homepage you can find documentation about how to install hugo. <https://gohugo.io>
-   org-mode
    Installed by default with Emacs.
-   ox-hugo
    Installed with the Emacs package manager


## Install ox-hugo {#install-ox-hugo}

Org-mode has a library called \`ox\` to export org-files to other file types like pdf, markdown, latex and more.

One thing you can do its install aditional export extensions to this library.
just by installing in this case \`ox-hugo\`.

First we need to install \`ox-hugo\`

```emacs-lisp
;; Maybe you need to add melpa repository in your `init.el' file
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; M-x install-package RET ox-hugo
;; Then load it after `ox'
(eval-after-load 'ox
  (require 'ox-hugo))

;; or if your use `use-package' to install and load the paackage.
;; More info: https://github.com/jwiegley/use-package
(use-package ox-hugo
  :after 'ox)
```


## Create new Hugo blog {#create-new-hugo-blog}

First we need to create the basic hugo directory structure.

Go in your projects folder run this hugo cli command:

```shell
hugo new site kcurtet.github.io # remplace with your site folder
```

Then you can see a directory structure like this:

```text
kcurtet.github.io
├── archetypes
├── config.toml
├── content
├── data
├── layouts
├── public
├── resources
├── static
└── themes
```

In the `config.toml` you can see the site configurations.

Now we need to initialize a git repo and then add a default theme for our blog as git submodule. I choose the cactus hugo theme. Don't panic it's easy as run 3 commands:

```shell
cd kcurtet.github.io
git init
git submodule add https://github.com/monkeyWzr/hugo-theme-cactus themes/cactus
```

Now edit your config.toml file with:

```text
theme = "cactus"
```

And voila! your theme is configured.


## Add Blog Content {#add-blog-content}

Now the important part. add blog posts with org-mode.

We need to create a file for our posts. it can be placed anywhere you want but for simplicity in this example i create a new file in the `content` folder called `posts.org`.

```shell
touch content/posts.org # just creates a blank file posts.org
emacs content/posts.org
```

Now with emacs add some headers it the top of the file for ox-hugo.

```org
#+HUGO_BASE_DIR: ./..    ;; The Hugo site directory.
#+HUGO_SECTION: posts ;; The section where to save the posts. aka /content/posts
```

In this case the `HUGO_BASE_DIR` is the parent folder of the file. This tells `ox-hugo` where to store the exports of your file.

Then you need to create your first blog post.

Start with a Heading or Subheading to add some properties to it.

```org
* TODO Hello this is my first blog post
:PROPERTIES:
:EXPORT_FILE_NAME: my-first-post-with-hugo-and-org-mode
:EXPORT_DATE: 2021-03-20
:EXPORT_HUGO_MENU: :menu "main"
:END:

Text to introduce my post.

** Sub titles of the post
   More text...
```

In this example your can create as many headers as posts you want then change the properties to export the content to hugo with the correct file path and date.

`EXPORT_FILE_NAME` tells ox-hugo how to name the file in the content/posts folder in this example.
`EXPORT_DATE` tells ox-hugo the date of the publication.
`EXPORT_HUGO_MENU: :menu "main"` tells ox-hugo to add this entry to the main menu.

The `TODO` keyword in the front of the title is handled by `ox-hugo` by adding to your post the markdown front-matters `draft:true` and Hugo doesn't publish this files because it thinks they are not finished.

And type `C-c C-e H H` to create the markdown files in the content dir.

```text
.
├── archetypes
├── config.toml
├── content
   └── posts
      └── my-first-post-with-hugo-and-org-mod
├── data
├── layouts
├── public
├── resources
├── static
└── themes
```


## Create a static site. {#create-a-static-site-dot}

We can preview the contents in a local server with:

```shell
hugo server -D # -D to see the drafts.
```

Or we can build the blog with this command:

```shell
hugo --minify
```

Now in we have a new `/public` folder with our static site.


## <span class="org-todo todo TODO">TODO</span> Build with github pages. {#build-with-github-pages-dot}
