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

(defvar org-basecamp-extra-headers
  '(("Content-Type" . "application/xml")
    ("Accept" . "application/xml")))

(defun org-basecamp-make-request (path callback &optional cbargs)
  "Makes a request to Basecamp."
  (let* ((apikey (org-entry-get nil "BASECAMP_API_KEY" nil))
         (apihost (org-entry-get nil "BASECAMP_API_HOST" nil))
         (apissl (org-entry-get nil "BASECAMP_API_SSL" nil))
         (apiuser (org-entry-get nil "BASECAMP_USER_ID" nil))
         (proto (if apissl "https://" "http://"))
         (port  (if apissl ":443" ":80"))
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
  
(defun org-basecamp-pull-todo-cb (status orgbuf)
  (let ((xmldoc (org-basecamp-parse-results status)))
    (debug)))

(defun org-basecamp-pull-todo ()
  (interactive)
  (let ((cbargs (list (current-buffer)))
        (path "/todo_lists.xml"))
    (org-basecamp-make-request path 'org-basecamp-pull-todo-cp cbargs)))

(provide 'org-basecamp)
