;;; org-basecamp.el --- Basecamp (http://basecamphq.com) to-do lists in org
;;
;; Copyright (C) 2010 pmade inc. (Peter Jones pjones@pmade.com)
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
(eval-when-compile
  (require 'org))

(require 'url)
(require 'url-http)
(require 'xml)

(defgroup org-basecamp nil
  "Org integration with Basecamp."
  :tag "Org-Basecamp"
  :group 'org)

(defcustom org-basecamp-mark-todo t
  "If non-nil, set uncompleted todo items from Basecamp with the
first todo keyword in `org-todo-keywords', which is usually TODO.
If nil, don't set a todo state for Basecamp items that haven't
been completed.")

(defvar org-basecamp-extra-headers
  '(("Content-Type" . "application/xml")
    ("Accept"       . "application/xml")))

(defvar org-basecamp-todo-list-id-prefix
  "BASECAMP-TODOLIST-")

(defvar org-basecamp-todo-item-id-prefix
  "BASECAMP-TODOITEM-")

(defun org-basecamp-make-request (path callback &optional cbargs)
  "Makes a request to Basecamp."
  (let* ((apikey (org-entry-get nil "BASECAMP_API_KEY" nil))
         (apihost (org-entry-get nil "BASECAMP_API_HOST" nil))
         (apissl (org-entry-get nil "BASECAMP_API_SSL" nil))
         (apiuser (org-entry-get nil "BASECAMP_USER_ID" nil))
         (proto (if (string= apissl "YES") "https://" "http://"))
         (port  (if (string= apissl "YES") ":443" ":80"))
         (qs (concat "?" (if apiuser (concat "responsible_party=" apiuser))))
         (encoded (base64-encode-string (concat apikey ":X")))
         (auth (list (list (concat apihost port) (cons "Basecamp" encoded))))
         (url (concat proto apihost path qs))
         (url-basic-auth-storage 'auth)
         (url-request-method (or url-request-method "GET"))
         (url-request-extra-headers org-basecamp-extra-headers))
    (if (or (not apikey) (not apihost))
        (error "Basecamp properties not found on current heading, please see docs"))
    (url-retrieve url callback cbargs)))

(defun org-basecamp-parse-results (status)
  (when (plist-get status :error)
    (switch-to-buffer (current-buffer))
    (error "Failed to make request to Basecamp"))
  (goto-char (point-min))
  (re-search-forward "\n\n")
  (xml-parse-region (point) (point-max)))

(defun org-basecamp:xml-child-content (node child-name)
  "Returns the content of the first matching child node."
  (let ((child (car (xml-get-children node child-name))))
    (if child (car (xml-node-children child)))))

(defun org-basecamp-make-new-child ()
  (let ((depth (org-outline-level))
        (next (save-excursion (and (outline-next-heading) (org-outline-level)))))
    (if (and next (> next depth))
        (progn
          (outline-next-heading)
          (org-insert-heading t t))
      (end-of-line)
      (org-insert-heading-respect-content)
      (org-demote-subtree))))

(defun org-basecamp-pull-todo-cb (status orgbuf point)
  (let ((doc (org-basecamp-parse-results status))
        node heading last-tl tl-heading ti-heading ti-state id name)
    (with-current-buffer orgbuf
      (save-excursion
        (goto-char point)
        (org-back-to-heading)
        (setq heading (point))
        (dolist (todo-list (xml-get-children (car doc) 'todo-list))
          (setq id org-basecamp-todo-list-id-prefix)
          (setq id (concat id (org-basecamp:xml-child-content todo-list 'id)))
          (setq tl-heading (org-find-entry-with-id id))
          (when (not tl-heading)
            (setq name (org-basecamp:xml-child-content todo-list 'name))
            (if last-tl
                (progn
                  (goto-char last-tl)
                  (org-insert-heading-respect-content))
              (goto-char heading)
              (org-basecamp-make-new-child))
            (insert name)
            (setq last-tl (point))
            (beginning-of-line)
            (setq tl-heading (point))
            (org-set-property "ID" id))
          (setq node (car (xml-get-children todo-list 'todo-items)))
          (dolist (todo-item (xml-get-children node 'todo-item))
            (setq id org-basecamp-todo-item-id-prefix)
            (setq id (concat id (org-basecamp:xml-child-content todo-item 'id)))
            (setq ti-heading (org-find-entry-with-id id))
            (when (not ti-heading)
              (setq name (org-basecamp:xml-child-content todo-item 'content))
              (setq ti-state (org-basecamp:xml-child-content todo-item 'completed))
              (goto-char tl-heading)
              (end-of-line)
              (org-insert-heading-respect-content)
              (org-demote-subtree)
              (insert name)
              (org-set-property "ID" id)
              (org-set-property "BASECAMP-CREATED-ON" (org-basecamp:xml-child-content todo-item 'created-on))
              (org-set-property "BASECAMP-CREATOR-NAME" (org-basecamp:xml-child-content todo-item 'creator-name))
              (cond
               ((string= ti-state "true") (org-todo 'done))
               (org-basecamp-mark-todo (org-todo 'nextset)))))
          (goto-char tl-heading)
          (hide-subtree))))))
            
(defun org-basecamp-pull-todo ()
  (interactive)
  (let ((cbargs (list (current-buffer) (point)))
        (path "/todo_lists.xml"))
    (org-basecamp-make-request path 'org-basecamp-pull-todo-cb cbargs)))

(provide 'org-basecamp)
