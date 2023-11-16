;;; mini-bib.el --- A minimal citation manager. -*- lexical-binding: t; -*-

;; Copyright (C) 2023 M. Rincón

;; Version: 0.0.02
;; URL: https://github.com/foxfriday/mini-bib
;; Package-Requires: ((emacs "29") (parsebib "4.3"))

;;; Commentary:
;; This package provides minimal citation management tools. The only advantage over other
;; similar packages is that it's small and easy to audit—though it depends on `parsebib`.
;; Fortunately `parsebib` is well documented and easy to understand. The package provides
;; three functions which make use Emacs completions mechanism.
;; `mini-bib-note` open or creates a note associated with the entry.
;; `mini-bib-cite` insert a citation.
;; `mini-bib-open` opens the file associated with the entry.

;;; Code:
(require 'parsebib)

(defvar mini-bib-bibliography (list "~/Repos/books/bibliography.bib")
  "Default list of bibliographies.")

(defvar mini-bib-notes "~/Repos/books/notes"
  "Default location for the notes.")

(defvar mini-bib-files "~/Repos/books/documents"
  "Default location for the articles.")

(defvar mini-bib-open-cmnd (if (eq system-type 'darwin) "open" "xdg-open")
  "Command used to open a file externally.")

(defvar mini-bib-search-field "title"
  "Default search field for citation, should be in `mini-bib--fields`.")

(defvar mini-bib-search-annotation "author"
  "Default annotation field for citation, should be in `mini-bib--fields`.")

(defvar mini-bib--fields (list "title" "author")
  "List of fields to extract from BibTeX files, if changed, leave title and author.")

(defun mini-bib--trim (name l)
  "Trim a string NAME to no more than L characters."
  (substring name 0 (min (length name) l)))

(defun mini-bib--load (bibs)
  "Load a list or single BibTeX file specified in BIBS."
  (let* ((data (parsebib-parse bibs :fields mini-bib--fields))
         (entries (make-hash-table :size (hash-table-count data) :test 'equal)))
    (maphash (lambda (key value)
               (let* ((field (mini-bib--trim (cdr (assoc mini-bib-search-field value)) 55))
                      (type (mini-bib--trim (cdr (assoc "=type=" value)) 7)))
                 (puthash (format "%-55s\t%-8s\t%s" field type key) value entries)))
             data)
    entries))

(defun mini-bib--read (post)
  "Pick an entry and process the selection with POST."
  (let* ((records (mini-bib--load mini-bib-bibliography))
         (completion-extra-properties
          '(:annotation-function
            (lambda (k)
              (let* ((value (gethash k minibuffer-completion-table nil))
                     (field (mini-bib--trim (cdr (assoc mini-bib-search-annotation value)) 15)))
                (format "\t%s" field)))))
         (selection (completing-read "Select document: " records))
         (entry (gethash selection records nil)))
    (funcall post entry)))

;;;###autoload
(defun mini-bib-note (&optional search annotate)
  "Create or open notes narrowing by SEARCH with ANNOTATE in the margin."
  (interactive)
  (let* ((mini-bib-search-field (if search search mini-bib-search-field))
         (mini-bib-search-annotation (if annotate annotate mini-bib-search-annotation))
         (text "* %s\n:PROPERTIES:\n:AUTHORS: %s\n:END:\n"))
    (mini-bib--read (lambda (entry)
                      (let* ((key (cdr (assoc "=key=" entry)))
                             (title (cdr (assoc "title" entry)))
                             (author (cdr (assoc "author" entry)))
                             (file (expand-file-name (concat key ".org") mini-bib-notes))
                             (old (file-exists-p file)))
                        (find-file file)
                        (unless old (insert (format text title author))))))))

;;;###autoload
(defun mini-bib-cite (&optional search annotate)
  "Cite using narrowing by SEARCH and using ANNOTATE in the margin."
  (interactive)
  (let* ((mini-bib-search-field (if search search mini-bib-search-field))
         (mini-bib-search-annotation (if annotate annotate mini-bib-search-annotation))
         (text (cond ((eq major-mode 'latex-mode) "\\cite{%s}")
                     ((eq major-mode 'org-mode) "[cite:@%s]")
                     (t "%s"))))
    (mini-bib--read (lambda (entry) (insert (format text (cdr (assoc "=key=" entry))))))))

;;;###autoload
(defun mini-bib-open (&optional search annotate)
  "Open document narrowing by SEARCH and using ANNOTATE in the margin."
  (interactive)
  (let* ((mini-bib-search-field (if search search mini-bib-search-field))
         (mini-bib-search-annotation (if annotate annotate mini-bib-search-annotation)))
    (mini-bib--read (lambda (entry)
                      (let* ((key (cdr (assoc "=key=" entry)))
                             (pdf (expand-file-name (concat key ".pdf") mini-bib-files))
                             (epub (expand-file-name (concat key ".epub") mini-bib-files))
                             (doc (expand-file-name (concat key ".doc") mini-bib-files))
                             (docx (expand-file-name (concat key ".docx") mini-bib-files))
                             (file (cond ((file-exists-p pdf) pdf)
                                         ((file-exists-p epub) epub)
                                         ((file-exists-p doc) doc)
                                         ((file-exists-p docx) docx)
                                         (t (error (format "File for %s doesn't exist" key)))))
                             (log-buffer (get-buffer-create "*Messages*")))
                        (make-process :name "mini-bib-open"
                                      :buffer log-buffer
                                      :command (list mini-bib-open-cmnd file)
                                      :stderr log-buffer))))))

(provide 'mini-bib)
;;; mini-bib.el ends here
