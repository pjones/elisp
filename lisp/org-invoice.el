;;; org-invoice.el --- Help manage client invoices in OrgMode
;;
;; Copyright (C) 2008 pmade inc. (Peter Jones pjones@pmade.com)
;;
;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
;;
;; Commentary:
;;
;; Building on top of the terrific OrgMode, org-invoice tries to
;; provide functionality for managing invoices.  Currently, it does
;; this by implementing an OrgMode dynamic block where invoice
;; information is aggregated so that it can be exported.
;;
;; Future plans include integration with invoicing web sites, so that
;; you can submit your invoice from OrgMode to a service such as
;; Freshbooks.
;;
;; Usage:
;;
;; In your ~/.emacs:
;; (require 'org-invoice)
;;
;; In your OrgMode buffer:
;; More to come.
;;
;; Latest version:
;;
;; git clone git://pmade.com/elisp
(eval-when-compile
  (require 'cl)
  (require 'org))

(defgroup org-invoice nil
  "OrgMode Invoice Helper"
  :tag "Org-Invoice" :group 'org)

(defcustom org-invoice-long-date-format "%A, %B %d, %Y"
  "The format string for long dates."
  :type 'string :group 'org-invoice)

(defcustom org-invoice-strip-ts t
  "Remove org timestamps that appear in headings."
  :type 'boolean :group 'org-invoice)

(defcustom org-invoice-default-level 2
  "The heading level at which a new invoice starts.  This value
is used if you don't specify a scope option to the invoice block,
and when other invoice helpers are trying to find the heading
that starts an invoice.

The default is 2, assuming that you structure your invoices so
that they fall under a single heading like below:

* Invoices
** This is invoice number 1...
** This is invoice number 2...

If you don't structure your invoices using those conventions,
change this setting to the number that corresponds to the heading
at which an invoice begins."
  :type 'integer :group 'org-invoice)

(defcustom org-invoice-start-hook nil
  "Hook called when org-invoice is about to collect data from an
invoice heading.  When this hook is called, point will be on the
heading where the invoice begins.

When called, `org-invoice-current-invoice' will be set to the
alist that represents the info for this invoice."
  :type 'hook :group 'org-invoice)

  (defcustom org-invoice-heading-hook nil
  "Hook called when org-invoice is collecting data from a
heading. You can use this hook to add additional information to
the alist that represents the heading.

When this hook is called, point will be on the current heading
being processed, and `org-invoice-current-item' will contain the
alist for the current heading.

This hook is called repeatedly for each invoice item processed."
  :type 'hook :group 'org-invoice)

(defvar org-invoice-current-invoice nil
  "Information about the current invoice.")

(defvar org-invoice-current-item nil
  "Information about the current invoice item.")

(defvar org-invoice-table-params nil
  "The table parameters currently being used.")

(defvar org-invoice-total-time nil
  "The total invoice time for the summary line.")

(defvar org-invoice-total-price nil
  "The total invoice price for the summary line.")

(defun org-invoice-goto-tree (&optional tree)
  "Move point to the heading that represents the head of the
current invoice.  The heading level will be taken from
`org-invoice-default-level' unless tree is set to a string that
looks like tree2, where the level is 2."
  (let ((level org-invoice-default-level))
    (save-match-data
      (when (and tree (string-match "^tree\\([0-9]+\\)$" tree))
        (setq level (string-to-number (match-string 1 tree)))))
    (org-back-to-heading)
    (while (and (> (org-reduced-level (org-outline-level)) level)
                (org-up-heading-safe)))))

(defun org-invoice-heading-info ()
  "Return invoice information from the current heading."
  (let ((title   (org-no-properties (org-get-heading t)))
        (date    (org-entry-get nil "TIMESTAMP" 'selective))
        (work    (org-entry-get nil "WORK" nil))
        (rate    (or (org-entry-get nil "RATE" t) "0"))
        (level   (org-outline-level))
        raw-date long-date)
    (unless date (setq date (org-entry-get nil "TIMESTAMP_IA" 'selective)))
    (unless date (setq date (org-entry-get nil "TIMESTAMP" t)))
    (unless date (setq date (org-entry-get nil "TIMESTAMP_IA" t)))
    (unless work (setq work (org-entry-get nil "CLOCKSUM" nil)))
    (unless work (setq work "00:00"))
    (when date
      (setq raw-date (apply 'encode-time (org-parse-time-string date)))
      (setq long-date (format-time-string org-invoice-long-date-format raw-date)))
    (when (and org-invoice-strip-ts (string-match org-ts-regexp-both title))
      (setq title (replace-match "" nil nil title)))
    (setq work (org-hh:mm-string-to-minutes work))
    (setq rate (string-to-number rate))
    (setq org-invoice-current-item (list (cons 'title title)
          (cons 'date date)
          (cons 'raw-date raw-date)
          (cons 'long-date long-date)
          (cons 'work work)
          (cons 'rate rate)
          (cons 'level level)
          (cons 'price (* rate (/ work 60.)))))
    (run-hook-with-args 'org-invoice-heading-hook)
    org-invoice-current-item))

(defun org-invoice-level-min-max (ls)
  "Return a list where the car is the min level, and the cdr the max."
  (let ((max 0) min level)
    (dolist (info ls)
      (when (cdr (assoc 'date info))
        (setq level (cdr (assoc 'level info)))
        (when (or (not min) (< level min)) (setq min level))
        (when (> level max) (setq max level))))
    (cons (or min 0) max)))
  
(defun org-invoice-collapse-list (ls)
  "Reorganize the given list by dates."
  (let ((min-max (org-invoice-level-min-max ls)) new)
    (dolist (info ls)
      (let* ((date (cdr (assoc 'date info)))
             (work (cdr (assoc 'work info)))
             (price (cdr (assoc 'price info)))
             (long-date (cdr (assoc 'long-date info)))
             (level (cdr (assoc 'level info)))
             (bucket (cdr (assoc date new))))
        (if (and (/= (car min-max) (cdr min-max))
                   (=  (car min-max) level)
                   (=  work 0) (not bucket) date)
            (progn
              (setq info (assq-delete-all 'work info))
              (push (cons 'total-work 0) info)
              (push (cons date (list info)) new)
              (setq bucket (cdr (assoc date new))))
          (when (and date (not bucket))
            (setq bucket (list (list (cons 'date date)
                                     (cons 'title long-date)
                                     (cons 'total-work 0)
                                     (cons 'price 0))))
            (push (cons date bucket) new)
            (setq bucket (cdr (assoc date new))))
          (when (and date bucket)
            (setcdr (assoc 'total-work (car bucket))
                    (+ work (cdr (assoc 'total-work (car bucket)))))
            (setcdr (assoc 'price (car bucket))
                    (+ price (cdr (assoc 'price (car bucket)))))
            (nconc bucket (list info))))))
    (nreverse new)))
  
(defun org-invoice-info-to-table (info)
  "Create a single org table row from the given info alist."
  (let ((title (cdr (assoc 'title info)))
        (total (cdr (assoc 'total-work info)))
        (work  (cdr (assoc 'work info)))
        (price (cdr (assoc 'price info)))
        (with-price (plist-get org-invoice-table-params :price)))
    (unless total
      (setq 
       org-invoice-total-time (+ org-invoice-total-time work)
       org-invoice-total-price (+ org-invoice-total-price price)))
    (setq total (and total (org-minutes-to-hh:mm-string total)))
    (setq work  (and work  (org-minutes-to-hh:mm-string work)))
    (insert-before-markers 
     (concat "|" title
             (cond
              (total (concat "|" total))
              (work  (concat "|" work)))
             (and with-price price (concat "|" (format "%.2f" price)))
             "|" "\n"))))
  
(defun org-invoice-list-to-table (ls)
  "Convert a list of heading info to an org table"
  (let ((with-price (plist-get org-invoice-table-params :price))
        (with-summary (plist-get org-invoice-table-params :summary))
        (with-header (plist-get org-invoice-table-params :headers))
        (org-invoice-total-time 0)
        (org-invoice-total-price 0))
    (insert-before-markers 
     (concat "| Task / Date | Time" (and with-price "| Price") "|\n"))
    (dolist (info ls)
      (insert-before-markers "|-\n")
      (mapc 'org-invoice-info-to-table (if with-header (cdr info) (cdr (cdr info)))))
    (when with-summary
      (insert-before-markers
       (concat "|-\n|Total:|"
               (org-minutes-to-hh:mm-string org-invoice-total-time)
               (and with-price (concat "|" (format "%.2f" org-invoice-total-price)))
               "|\n")))))

(defun org-invoice-collect-invoice-data ()
  "Collect all the invoice data from the current OrgMode tree and
return it.  Before you call this function, move point to the
heading that begins the invoice data, usually using the
`org-invoice-goto-tree' function."
  (let ((org-invoice-current-invoice (list (cons 'point (point))))
        (org-invoice-current-item nil))
    (save-restriction
      (org-narrow-to-subtree)
      (org-clock-sum)
      (run-hook-with-args 'org-invoice-start-hook)
      (cons org-invoice-current-invoice
            (org-invoice-collapse-list 
             (org-map-entries 'org-invoice-heading-info t 'tree 'archive))))))
  
(defun org-dblock-write:invoice (params)
  "Function used by OrgMode for generating a dynamic block."
  (let ((scope (plist-get params :scope))
        (org-invoice-table-params params)
        (zone (move-marker (make-marker) (point)))
        table)
    (when (not scope) (setq scope 'invoice))
    (save-excursion
      (cond
       ((eq scope 'tree) (org-invoice-goto-tree "tree1"))
       ((eq scope 'invoice) (org-invoice-goto-tree))
       ((symbolp scope) (org-invoice-goto-tree (symbol-name scope))))
      (setq table (org-invoice-collect-invoice-data))
      (goto-char zone)
      (org-invoice-list-to-table (cdr table))
      (goto-char zone)
      (org-table-align)
      (move-marker zone nil))))

(defun org-invoice-in-report-p ()
  "Check to see if point is inside an invoice report."
  (let ((pos (point)) start)
    (save-excursion
      (end-of-line 1)
      (and (re-search-backward "^#\\+BEGIN:[ \t]+invoice" nil t)
	   (setq start (match-beginning 0))
	   (re-search-forward "^#\\+END:.*" nil t)
	   (>= (match-end 0) pos)
	   start))))

(defun org-invoice-report (&optional arg)
  "FIXME"
  (interactive "P")
  (let (report-start)
    (when (and arg (org-find-dblock "invoice"))
      (org-show-entry))
    (if (setq report-start (org-invoice-in-report-p))
        (goto-char report-start)
      (org-create-dblock (list :name "invoice")))
    (org-update-dblock)))
  
(provide 'org-invoice)
