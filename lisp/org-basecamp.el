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

(defun org-basecamp-api-info (inherit)
  "Load the Basecamp API information into an association list.
If `inherit' is non-nil, walk up the org tree looking for it,
otherwise only look for it on the current heading."
  (let ((apikey  (org-entry-get nil "BASECAMP_API_KEY"  inherit))
        (apihost (org-entry-get nil "BASECAMP_API_HOST" inherit))
        (apissl  (org-entry-get nil "BASECAMP_API_SSL"  inherit))
        (apiuser (org-entry-get nil "BASECAMP_API_USER" inherit)))
    (if (or (not apikey) (not apihost) (not apiuser))
        (if inherit (error "Basecamp properties not found in current tree")
          (error "Basecamp properties not found on current heading")))
    (list
     (cons 'key  apikey)
     (cons 'host apihost)
     (cons 'ssl  (string= apissl "YES"))
     (cons 'user apiuser))))
  
(defun org-basecamp-make-request (info path callback &optional cbargs)
  "Makes a request to Basecamp."
  (let* ((apikey (cdr (assoc 'key info)))
         (apihost (cdr (assoc 'host info)))
         (apissl (cdr (assoc 'ssl info)))
         (apiuser (cdr (assoc 'user info)))
         (proto (if apissl "https://" "http://"))
         (port  (if apissl ":443" ":80"))
         (qs (concat "?" (if apiuser (concat "responsible_party=" apiuser))))
         (encoded (base64-encode-string (concat apikey ":X")))
         (auth (list (list (concat apihost port) (cons "Basecamp" encoded))))
         (url (concat proto apihost path qs))
         (url-basic-auth-storage 'auth)
         (url-request-method (or url-request-method "GET"))
         (url-request-extra-headers org-basecamp-extra-headers))
    (url-retrieve url callback cbargs)))

(defun org-basecamp-parse-results (status)
  (when (plist-get status :error)
    (switch-to-buffer (current-buffer))
    (error "Failed to make request to Basecamp"))
  (goto-char (point-min))
  (re-search-forward "\n\n")
  (xml-parse-region (point) (point-max)))

(defun org-basecamp-null-cb (status msg)
  (when (plist-get status :error)
    (switch-to-buffer (current-buffer))
    (error "Failed to make request to Basecamp"))
  (message "%s" msg))

(defun org-basecamp-item-id ()
  "Returns the Basecamp ID for the current heading.  If the
current heading doesn't have a Basecamp ID an error is raised."
  (let* ((id (org-id-get))
         (prefix (substring id 0 (length org-basecamp-todo-item-id-prefix))))
    (unless (string= org-basecamp-todo-item-id-prefix prefix)
      (error "Current heading doesn't have a Basecamp ID"))
    (substring id (length org-basecamp-todo-item-id-prefix))))

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
  "Download all to-do lists with their to-do items and create org
headings for items that don't exist locally.  You must run this
function from a heading that contains the required Basecamp
properties.  These are:

  BASECAMP_API_KEY: Your Basecamp API key (log-in to Basecamp,
                    click the \"My Info\" link.  On your profile
                    page, look for the \"Show your tokens\" link,
                    click it and then copy the API token)

  BASECAMP_API_HOST: The domain name you use to access Basecamp,
                     taken from the URL you use to access
                     Basecamp, without any protocol or path
                     information

  BASECAMP_API_SSL: Optional. Set to YES if you are using SSL w/
                    Basecamp (if the URL starts with https).
                    Otherwise you can omit this property

  BASECAMP_API_USER: Your Basecamp user ID (go to the \"My Info\"
                     page and then copy your user ID from the
                     URL, it will be between \"people\" and
                     \"edit\", copy just the number

Example:

  * Projects
   :PROPERTIES:
   :BASECAMP_API_KEY: 72a5acxb3793a8a98c64f66811f44aed0205747jy
   :BASECAMP_API_HOST: client.basecamphq.com
   :BASECAMP_API_SSL: YES
   :BASECAMP_API_USER: 495849836
   :END:

Headings will be created as children of the current heading."
  (interactive)
  (let ((cbargs (list (current-buffer) (point)))
        (path "/todo_lists.xml")
        (info (org-basecamp-api-info nil)))
    (org-basecamp-make-request info path 'org-basecamp-pull-todo-cb cbargs)))

(defun org-basecamp-post-time ()
  "Create a new Basecamp time tracking entry for the current
heading.  The heading must have been downloaded from Basecamp
using the `org-basecamp-pull-todo' function.  You will be
prompted for the time to submit to Basecamp."
  (interactive)
  (let ((info (org-basecamp-api-info t))
        (id (org-basecamp-item-id))
        (url-request-method "POST")
        (date (org-read-date nil t nil "Basecamp Time Entry"))
        (org-clock-file-total-minutes nil)
        (args (list "Submitted entry to Basecamp"))
        decoded tstart tend minutes hours path url-request-data)
    (save-excursion
      (save-restriction
        (org-narrow-to-subtree)
        (setq decoded (decode-time date))
        (setq tstart (encode-time 1 0 0 (nth 3 decoded) (nth 4 decoded) (nth 5 decoded)))
        (setq tend (encode-time 59 59 23 (nth 3 decoded) (nth 4 decoded) (nth 5 decoded)))
        (org-clock-sum tstart tend)
        (setq minutes org-clock-file-total-minutes)
        (if (= minutes 0) (setq minutes 60))
        (setq minutes (org-minutes-to-hh:mm-string minutes))))
    (setq minutes (read-string "Basecamp Entry Hours: " minutes nil minutes)
          hours (number-to-string (/ (org-hh:mm-string-to-minutes minutes) 60.0))
          path (concat "/todo_items/" id "/time_entries.xml")
          url-request-data 
          (concat "<time-entry>"
                  "<person-id>" (cdr (assoc 'user info)) "</person-id>"
                  "<date>" (format-time-string "%Y-%m-%d" date) "</date>"
                  "<hours>" hours "</hours>"
                  "<description>" "</description>"
                  "</time-entry>\n"))
    (org-basecamp-make-request info path 'org-basecamp-null-cb args)))

(provide 'org-basecamp)
