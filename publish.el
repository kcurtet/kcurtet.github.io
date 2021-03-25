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

;; Script to export ox-hugo posts from org file subtree.

;;; Code:

;; Needed for Debug
;; (setq debug-on-error t)

;; Set emacs dir to .emacs
(defvar temp-dir (concat (file-name-directory load-file-name)
                         ".emacs/"))
(setq user-emacs-directory temp-dir)
;; Remove annoying backups
(setq backup-inhibited t)

(defun straight-package-manager ()
  "Install package manager."
  (message "Loading Straight...")
  (let ((straight-base-dir user-emacs-directory)
        (bootstrap-file
         (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
        (bootstrap-version 5))
    (unless (file-exists-p bootstrap-file)
      (with-current-buffer
          (url-retrieve-synchronously
           "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
           'silent 'inhibit-cookies)
        (goto-char (point-max))
        (eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage)))

(defvar +publish-package-list '(org ox-hugo))

(defun install-all-packages ()
  "Install and requre all packages in `+publish-package-list'."
  (message "Installing packages...")
  (dolist (package +publish-package-list)
    (straight-use-package package)
    (require package)))

(defun build-posts (file)
  "Export FILE posts."
  (with-current-buffer (find-file-noselect file)
    (search-forward "* Blog")
    (org-hugo-export-wim-to-md t nil nil t)))

(defun main ()
  "Main."
  (straight-package-manager)
  (install-all-packages)
  (build-posts (expand-file-name "posts.org" "content/")))

(provide 'publish)
;;; publish.el ends here
