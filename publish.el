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
