;;; ibuffer-project.el --- Ibuffer filtering groups for projects

;; Copyright (C) 2013-2015 Sylvain Rousseau <thisirs at gmail dot com>

;; Author: Sylvain Rousseau <thisirs at gmail dot com>
;; Maintainer: Sylvain Rousseau <thisirs at gmail dot com>
;; Created: 13 april 2013
;; URL: https://github.com/thisirs/ibuffer-project.git
;; Keywords: ibuffer, convenience
;; Package-Requires: ((async "1.1"))

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

;; This library adds filtering groups to ibuffer for your projects.

;;; Installation:

;; Put the following in your .emacs:
;; (require 'ibuffer-project)
;; (ibuffer-project-refresh t)

;; See documentation on https://github.com/thisirs/ibuffer-project#ibuffer-project

;;; Code:

(require 'ibuffer)
(require 'ibuf-ext)
(require 'async)
(require 'format-spec)

(defvar ibuffer-project-alist
  '(("%S" "~/.emacs.d"))
  "Alist of project template names vs corresponding paths containing projects.

Each element looks like (NAME PATH1 PATH2...) where NAME is a
parametrized string describing the projects contained in PATH1,
PATH2. NAME can contain several %-spec:

- %D is replaced by the directory name
- %S is replaced by the true directory name
- %I is replaced by the first line found in file .ibuffer at the
   root of the project")

(defvar ibuffer-project-is-project-function
  'ibuffer-project-is-project-default
  "Determines the function which is called to decide whether the
directory given in argument is a root of a project." )

(defvar ibuffer-project-autoinsert "default"
  "Variable that controls where the place-holders are inserted.

If it is a string a place-holder is inserted right after that
filter groups' name. If it is a list of string, insert a
place-holder right after each filter groups' name. If it is a
list of conses of the from (NAME . INDEX), insert a place-holder
at index INDEX in the filter groups named NAME. If nil, no
place-holder is inserted.")

(defvar ibuffer-project-cache-file
  (expand-file-name ".ibuffer-project.el" user-emacs-directory)
  "Path to the ibuffer-project cache file.")

(defvar ibuffer-project-place-holder-keyword 'place-holder
  "Symbol signaling a location to be replaced by the generated
group filters. Change it only if it clashes with your original
group filters.")

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

;;;###autoload
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
         (setq ibuffer-project--list nil)
         (ibuffer-project-refresh)
         (minibuffer-message "IBuffer cache written!")))

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

      ;; Insert place-holders according to `ibuffer-project-autoinsert'
      (if ibuffer-project-autoinsert
          (let* ((filter-groups (copy-tree ibuffer-saved-filter-groups))
                 (places (if (listp ibuffer-project-autoinsert)
                             ibuffer-project-autoinsert
                           (list ibuffer-project-autoinsert))))
            (mapc
             (lambda (place)
               (let* ((name (if (consp place) (car place) place))
                      (filter (assoc name filter-groups))
                      (index (if (consp place) (cdr place) 0)))
                 ;; Inserting at right index
                 (setcdr (nthcdr (min index (1- (length filter))) filter)
                         (cons ibuffer-project-place-holder-keyword (nthcdr (1+ index) filter)))))
             places)
            (setq ibuffer-saved-filter-groups filter-groups)))

      ;; Replace all the place-holder in `ibuffer-saved-filter-groups'
      (setq ibuffer-saved-filter-groups
            (read (with-temp-buffer
                    (prin1 ibuffer-saved-filter-groups (current-buffer))
                    (goto-char (point-min))
                    (while (re-search-forward (symbol-name ibuffer-project-place-holder-keyword) nil t)
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
