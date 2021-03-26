+++
title = "Build a static site with Hugo and Orgmode."
date = 2021-03-20
lastmod = 2021-03-26T03:14:36+01:00
draft = false
categories = ["emacs"]
+++

Static site generators are getting popular this years. I was thinking a time ago about creating one with gatsby or jekyll.
But I discover the power of org-mode and how to organize my life with simple text files. And the next step is to write a
blog with my editor only.


## Requirements. {#requirements-dot}

-   Emacs
-   Hugo
-   ox-hugo
-   git


## Create new Hugo site {#create-new-hugo-site}

First we need to create the basic Hugo directory structure.

Create a new project site with this command:

```shell
hugo new site kcurtet.github.io # remplace with your site folder
```

Then we can see the directory structure like this:

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

The basic configuration for the site its done in `config.toml`

Initialize a new git repository.

```shell
cd kcurtet.github.io
git init
git add .
git commit -m "Initial commit"
```

Now we need to add a default theme four our blog.

In my case I choose the [Cactus Theme](https://github.com/monkeyWzr/hugo-theme-cactus)

```shell
git submodule add https://github.com/monkeyWzr/hugo-theme-cactus themes/cactus
```

Now add in the file `config.toml` the theme we have downloaded.

```text
theme = "cactus"
```

This configures Hugo to utilize `cactus` as base theme.


## Install ox-hugo to create the posts. {#install-ox-hugo-to-create-the-posts-dot}

We need `ox-hugo` to export `org-mode` files to Hugo's `Backfriday Markdown`


### Vanilla Emacs {#vanilla-emacs}

This configuration its for pure emacs.

```emacs-lisp
;; Maybe you need to add melpa repository in your `init.el' file
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; M-x install-package RET ox-hugo
;; Then load it after `ox'
(eval-after-load 'ox
  (require 'ox-hugo))
```


### Doom {#doom}

In doom.d/init.el remove the comment for `org` module and add the `+hugo` flag

```emacs-lisp
(org
 +hugo)
```


### use-package {#use-package}

More info: <https://github.com/jwiegley/use-package>

```emacs-lisp
(use-package ox-hugo
  :after 'ox)
```


## Write the content {#write-the-content}

Now its time to write some blog posts. First we create a org file in the content folder of our project.

```shell
touch content/posts.org
```

In the beginning of the file we add this headers to loacate our project.

```org
#+HUGO_BASE_DIR: ../  # Hugo base project folder
#+HUGO_SECTION: posts # Hugo folder in content
```

`HUGO_BASE_DIR`: The project root. In this case is the parent folder of the file.
`HUGO_SECTION` creates a folder `content/posts` where we save the markdown files.

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

In this example we can create as many headers as posts we want, then change the properties to export the content to hugo with the correct file path and date.

`EXPORT_FILE_NAME` the file name of the post in `content/posts` also used as slug.
`EXPORT_DATE` The date of the publication.
`EXPORT_HUGO_MENU: :menu "main"` Add this entry to the main menu.

The `TODO` keyword sets the posts as drafts.

When you are ready to publish your post type `C-c C-e H H` to create the markdown files in `content/posts`.

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


## Build the static site. {#build-the-static-site-dot}

We can preview the contents in a local server with:

```shell
hugo server -D # -D to see the drafts.
```

Or we can build the blog with this command:

```shell
hugo --minify
```

Now in we have a new `/public` folder with our static site.


## CI/CD Build and upload with GitHub pages. {#ci-cd-build-and-upload-with-github-pages-dot}

We want to build our blog every time we upload the project to github.

First we create a file `.github/workflows/gh-pages.yaml` in the git repository.

```shell
mkdir -p .github/workflows
touch .github/workflows/gh-pages.yaml
```

And we add this content:

```yaml
name: github pages

on:
  push:
    branches:
      - main  # Set a branch to deploy

jobs:
  deploy:
    runs-on: ubuntu-18.04
    steps:
      - uses: actions/checkout@v2
        with:
          submodules: true  # Fetch Hugo themes (true OR recursive)
          fetch-depth: 0    # Fetch all history for .GitInfo and .Lastmod

      - name: Setup Hugo
        uses: peaceiris/actions-hugo@v2
        with:
          hugo-version: '0.81.0'
          # extended: true

      - name: Build
        run: hugo --minify

      - name: Deploy
        uses: peaceiris/actions-gh-pages@v3
        with:
          github_token: ${{ secrets.GITHUB_TOKEN }}
          publish_dir: ./public
```

We need a token to push our build to gh-pages branch. Follow this instructions to [add secrets for deploy actions](https://github.com/peaceiris/actions-gh-pages#%EF%B8%8F-first-deployment-with-github%5Ftoken).

When we setup all correctly. we can push our repo to GitHub.

And the action should have created a gh-pages branch with the static site.

Maybe you need to edit the settings of the repository in github to set gh-pages correctly if you don't see your site.

Don't forget to set your `baseUrl` in `config.toml` or Hugo can't link the assets.


## Bonus: Script to export the posts {#bonus-script-to-export-the-posts}

This script is to export my posts with just emacs istalled and internet conexion.

```emacs-lisp
:;exec emacs --batch --quick --load="$0" --funcall=main "$@"
;;; publish.el --- Publish ox-hugo file subtree -*- lexical-binding: t -*-

;; Copyright (c) 2020 Kevin Curtet <kcurtet@gmail.com>

;; Author: Kevin Curtet <kcurtet@gmail.com>
;; Version: 1.0

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Script to export posts from org file with ox-hugo headers.

;; More info on https://kcuret.github.io

;; Usage: publish.el FILE-NAME [ SUBTREE ]

;; if SUBTREE is given it search for a subtree with a regex pattern
;; and only exports that one.


;;; Code:

;; Better errors
(setq debug-on-error t)

(defvar +cache-dir
  (concat (file-name-directory load-file-name) ".packages/")
  "Cache folder where `straight.el' downloads the packages.")

(defvar +publish--package-list '(org ox-hugo)
  "Packages to be installed with `straight.el'.")

(defun +publish--install-dependencies ()
  "Install `straight.el' and `+publish--package-list'."
  (let ((straight-base-dir +cache-dir)
        (bootstrap-file
         (expand-file-name "straight/repos/straight.el/bootstrap.el" +cache-dir))
        (bootstrap-version 5))
    (unless (file-exists-p bootstrap-file)
      (with-current-buffer
          (url-retrieve-synchronously
           "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
           'silent 'inhibit-cookies)
        (goto-char (point-max))
        (eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage)
    ;; Install & load packages in `publish-package-list'.
    (dolist (package +publish--package-list)
      (straight-use-package package)
      (require package))))

(defun +publish--open-file (file-name)
  "Open FILE-NAME as read only."
  (find-file-read-only (expand-file-name file-name))
  (goto-char (point-min)))

(defun main ()
  "Export FILE-NAME ox-hugo posts."
  (+publush--install-dependencies)
  (pcase command-line-args-left
    ;; Take FILE-NAME and SUBTREE from args
    (`(,file-name ,subtree)
     (+publish--open-file file-name)
     (if (re-search-forward (concat "^*+ " subtree) nil t)
         (message "%s" (org-hugo-export-wim-to-md nil nil t t))
       (error "There is no subtree %s in %s" subtree file-name)))
    ;; Take FILE-NAME from args
    (`(,file-name)
     (+publish--open-file file-name)
     (org-hugo-export-wim-to-md t nil nil t))
    (x
     (error "Usage: publish.el FILE [ SUBTREE ]"))))

(provide 'publish)
;;; publish.el ends here
```

The first line invokes emacs in batch mode with a trick `:;` (I get this line from a script in guix sources).
With the `--load` we load the file contents then we run the main function with `--funcall` and we pass the arguments to emacs with `"$@"`.

```shell
:;exec emacs --batch --quick --load="$0" --funcall=main "$@"
```

Now we can run this script from the shell like any other script.

```shell
./package.el # Usage: package.el FILE-NAME [SUBTREE]
```
