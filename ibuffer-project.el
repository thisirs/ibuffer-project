;;; ibuffer-project.el --- Ibuffer filtering groups for projects

;; Copyright (C) 2013 Sylvain Rousseau <thisirs at gmail dot com>

;; Author: Sylvain Rousseau <thisirs at gmail dot com>
;; Maintainer: Sylvain Rousseau <thisirs at gmail dot com>
;; Created: 13 april 2013
;; Keywords: ibuffer, convenience
;; URL: https://github.com/thisirs/ibuffer-project.git
;; Package-Requires: ((async "1.1))

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
;; ibuffer-project.el adds filtering groups for your projects. See the
;; README file for more information.

;;; Code:

(require 'ibuffer)
(require 'ibuf-ext)
(require 'async)
(require 'format-spec)

(defvar ibuffer-project-alist
  '(("%S" "~/.emacs.d"))
  "Alist of project names vs corresponding paths. Each element
looks like (NAME . PATHS) where NAME is a string describing the
project and PATHS is either the path or a list of paths of the
project. NAME can contain %D that will be replaced by the name of
the root directory of the project, %S will be replaced by the
true name of the root directory of the project and %I will be
replaced by the first line found in file .ibuffer if any at the
root of the project or defaults to %D if not found.")

(defvar ibuffer-project-cache-file
  (concat (file-name-as-directory user-emacs-directory) "cache/ibuffer-project")
  "Path to the ibuffer project cache file.")

(defvar ibuffer-project-is-project-function
  'ibuffer-project-is-project-default
  "Determines the function which is called to decide whether the
  directory given in argument is a root of a project." )

(defvar ibuffer-project-autoinsert "default"
  "Variable that controls where the place-holders are inserted. If
  it is a string a place-holder is inserted right after that
  filter groups' name. If it is a list of string, insert a
  place-holder right after each filter groups' name. If nil, no
  place-holder is inserted.")

(defvar ibuffer-project--list nil
  "Internal variable that contains the project list.")

(defvar ibuffer-project--original nil
  "Internal variable that stores the original
`ibuffer-saved-filter-groups' variable to be able to reuse it
when refreshing.")

(defun ibuffer-project-is-project-default (directory)
  "Return true if the directory DIRECTORY is the root of a
project. That is, if it contains a directory named \".git\" or a
file named \".ibuffer\" but does not contain one named
\".hidden\"."
  (let ((directory (file-name-as-directory directory)))
    (and (or (file-exists-p (concat directory ".git"))
             (file-exists-p (concat directory ".ibuffer")))
         (not (file-exists-p (concat directory ".hidden"))))))

(defun ibuffer-project-find-projects (dir)
  "Return the list of all directories and sub-directories of DIR
including DIR that are project roots according to
`ibuffer-project-is-project-function'. Cancel the search in
sub-directories in a .nosearch file is encountered."
  (let* ((dir (file-name-as-directory dir))
         (list
          (if (funcall ibuffer-project-is-project-function dir)
              (list (expand-file-name dir)))))
    (if (file-exists-p (concat dir ".nosearch"))
        list
      (apply 'append list
             (mapcar
              (lambda (path)
                (if (file-directory-p path)
                    (ibuffer-project-find-projects path)))
              ;; avoiding . and ..
              (directory-files dir t "[^\\.]\\|\\(\\.\\{3,\\}\\)"))))))

(defun ibuffer-project-make-ibuffer-projects-list (format directories)
  "Return a list specially crafted for Ibuffer. For each
directory contained in the list of directories DIRECTORIES,
return an element that looks like (FORMATTED-HEADLINE (filename .
DIRECTORY)) where FORMATTED-HEADLINE is a string formatted by
FORMAT."
  (mapcar
   (lambda (dir)
     (list (format-spec
            format
            `((?D . ,(file-name-nondirectory
                      (directory-file-name dir)))
              (?S . ,(file-name-nondirectory
                      (directory-file-name
                       (file-truename dir))))
              (?I . ,(let ((file (concat (file-name-as-directory dir) ".ibuffer")))
                       (if (file-exists-p file)
                           (with-temp-buffer
                             (insert-file-contents file)
                             (goto-char (point-min))
                             (string-match "^[^\n]*" (buffer-string))
                             (match-string 0 (buffer-string)))
                         (file-name-nondirectory
                          (directory-file-name dir)))))))
           `(filename . ,dir)))
   (apply 'append
          (mapcar
           (lambda (dir)
             (and (file-directory-p dir)
                  (nreverse (ibuffer-project-find-projects dir))))
           (if (listp directories)
               directories
             (list directories))))))

(defun ibuffer-project-write-cache ()
  "Generate the project list according to `ibuffer-project-alist'
and caches it in the file `ibuffer-project-cache-file'."
  (setq ibuffer-project--list
        (apply 'append
               (mapcar
                (lambda (e)
                  (ibuffer-project-make-ibuffer-projects-list
                   (car e) (cdr e)))
                ibuffer-project-alist)))
  (make-directory
   (file-name-directory ibuffer-project-cache-file) t)
  (with-temp-buffer
    (pp ibuffer-project--list (current-buffer))
    (write-region (point-min)
                  (point-max)
                  ibuffer-project-cache-file)))

(defun ibuffer-project-refresh (&optional cache)
  "Alter the variable `ibuffer-saved-filter-groups' to include
projects filter groups. If already altered, refresh it using
original value before including."
  (interactive "P")

  ;; Backup filtering groups if not already done
  (unless ibuffer-project--original
    (setq ibuffer-project--original ibuffer-saved-filter-groups))

  ;; Update `ibuffer-project--list'
  (unless (or cache ibuffer-project--list)
    (if (file-exists-p ibuffer-project-cache-file)
        (with-temp-buffer
          (insert-file-contents ibuffer-project-cache-file)
          (setq ibuffer-project--list (read (buffer-string))))
      (setq cache t)))

  ;; Generate cache
  (if cache
      ;; Generate cache asynchronously
      (async-start
       `(lambda ()
          ;; Pass variables to child
          ,(async-inject-variables "^ibuffer-project-")

          ;; Load this file
          (load-file ,(find-library-name "ibuffer-project"))

          ;; Write cache
          (ibuffer-project-write-cache))
       (lambda (result)
         ;; Re-run the current function now that the cache file is
         ;; written
         (minibuffer-message "IBuffer cache written!")
         (setq ibuffer-project--list nil)
         (ibuffer-project-refresh)))

    (let ((list0 ibuffer-saved-filter-groups)
          next-filter-group)
      ;; Find currently used filter group before altering
      ;; `ibuffer-saved-filter-groups'.
      (while (and (null next-filter-group) list0)
        (if (equal ibuffer-filter-groups (cdr (car list0)))
            (setq next-filter-group (car (car list0))))
        (setq list0 (cdr list0)))
      (setq list0 (or list0 ibuffer-saved-filter-groups))
      (setq next-filter-group (caar list0))

      ;; Restore original filtering groups
      (setq ibuffer-saved-filter-groups ibuffer-project--original)

      ;; Add a place-holder if needed
      (if ibuffer-project-autoinsert
          (let* ((filter-groups (copy-tree ibuffer-saved-filter-groups))
                 (places (if (listp ibuffer-project-autoinsert)
                             ibuffer-project-autoinsert
                           (list ibuffer-project-autoinsert))))
            (mapc
             (lambda (name)
               (let ((filter (assoc name filter-groups)))
                 (setcdr filter (cons 'place-holder (cdr filter)))))
             places)
            (setq ibuffer-saved-filter-groups filter-groups)))

      ;; Replace all the place-holder in `ibuffer-saved-filter-groups'
      (setq ibuffer-saved-filter-groups
            (read (with-temp-buffer
                    (prin1 ibuffer-saved-filter-groups (current-buffer))
                    (goto-char (point-min))
                    (while (re-search-forward "place-holder" nil t)
                      (replace-match
                       (mapconcat 'prin1-to-string
                                  ibuffer-project--list " ")))
                    (buffer-string))))

      ;; Update in Ibuffer
      (if (get-buffer "*Ibuffer*")
          (with-current-buffer "*Ibuffer*"
            (setq ibuffer-filter-groups
                  (cdr (assoc next-filter-group ibuffer-saved-filter-groups))
                  ibuffer-hidden-filter-groups nil)
            (ibuffer-update nil t))))))

(provide 'ibuffer-project)

;;; ibuffer-project.el ends here
