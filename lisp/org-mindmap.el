;;; org-mindmap.el --- Turn OrgMode Headings Into TikZ Mindmaps
;;
;; Copyright (C) 2009 pmade inc. (Peter Jones pjones@pmade.com)
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
  (require 'cl)
  (require 'org))

(defgroup org-mindmap nil
  "OrgMode Mindmap Generator"
  :tag "Org-Mindmap" :group 'org)

(defcustom org-mindmap-header
  "\\documentclass[landscape,letterpaper]{article}
\\usepackage{tikz}
\\usetikzlibrary{mindmap,trees}
\\pagestyle{empty}
\\begin{document}\n"
  "The LaTeX header used in mindmaps."
  :type 'string :group 'org-mindmap)

(defcustom org-mindmap-footer
  "\\end{document}"
  "The LaTeX footer used in mindmaps."
  :type 'string :group 'org-mindmap)

(defcustom org-mindmap-style-root
  "concept color=black,text=white"
  "The TikZ root style"
  :type 'string :group 'org-mindmap)

(defcustom org-mindmap-style-level-1
  "concept color=green!50!black"
  "The TikZ level 1 style."
  :type 'string :group 'org-mindmap)

(defcustom org-mindmap-style-level-2
  "concept color=blue"
  "The TikZ level 2 style."
  :type 'string :group 'org-mindmap)

(defcustom org-mindmap-style-level-3
  "concept color=red"
  "The TikZ level 3 style."
  :type 'string :group 'org-mindmap)

(defcustom org-mindmap-style-level-4
  "concept color=orange"
  "The TikZ level 1 style."
  :type 'string :group 'org-mindmap)

(defcustom org-mindmap-style-other ""
  "Additional TikZ style strings."
  :type 'string :group 'org-mindmap)

(defvar org-mindmap-export-buffer nil
  "The buffer that is receiving export information")

(defvar org-mindmap-start-level 0
  "The heading level of the root node")

(defun org-mindmap-walk-tree (offset)
  (let* ((title (org-no-properties (org-get-heading t)))
         (level (org-outline-level))
         (ring  (- level org-mindmap-start-level))
         (node  (concat "node[concept] {" title "}"))
         (root  (= 0 offset))
         (child-offset 0)
         (start-from 0)
         (children (save-excursion
                     (and (outline-next-heading) 
                          (> (org-outline-level) level))))
         (others (save-excursion
                   (and 
                    (outline-next-heading)
                    (>= (org-outline-level) level)))))
    (unless root
      (setq start-from (- 90 (* 45 (1- offset)))))
;      (setq start-from (+ start-from (* 60 (abs (- (1- ring) (1- offset)))))))
    (with-current-buffer org-mindmap-export-buffer
      (unless root (insert (concat (make-string level ?\ ) "child {")))
      (when (and (not root) children) (insert "\n"))
      (when (or root children) (insert (make-string level ?\ )))
      (when (and (not root) children) (insert " "))
      (insert node)
      (when children
        (insert (concat 
                 "\n"
                 (make-string level ?\ )
                 "[clockwise from="
                 (number-to-string start-from)
                 "]\n"))))
    (while (and
            children
            (outline-next-heading)
            (= (org-outline-level) (1+ level)))
      (setq child-offset (1+ child-offset))
      (org-mindmap-walk-tree child-offset))
    (when (and children (org-at-heading-p))
      (outline-previous-heading))
    (with-current-buffer org-mindmap-export-buffer
      (unless root (insert "}"))
      (when (and (not root) others) (insert "\n")))
    (with-current-buffer org-mindmap-export-buffer
      (when root (insert ";\n")))))

(defun org-export-as-mindmap ()
  (interactive)
  (let ((file-name (or buffer-file-name "mindmap"))
        (org-mindmap-start-level (org-outline-level)))
    (setq file-name
          (if (string-match "\\..*$" file-name)
              (replace-match ".tex" nil nil file-name)
            (concat file-name ".tex")))
    (setq org-mindmap-export-buffer 
          (generate-new-buffer (generate-new-buffer-name "*org-mindmap*")))
    (with-current-buffer org-mindmap-export-buffer
      (insert org-mindmap-header)
      (insert "\\begin{tikzpicture}\n")
      (insert "\\path[")
      (insert (concat "root concept/.append style={" org-mindmap-style-root "},\n"))
      (insert (concat " level 1 concept/.append style={" org-mindmap-style-level-1 "},\n"))
      (insert (concat " level 2 concept/.append style={" org-mindmap-style-level-2 "},\n"))
      (insert (concat " level 3 concept/.append style={" org-mindmap-style-level-3 "},\n"))
      (insert (concat " level 4 concept/.append style={" org-mindmap-style-level-4 "},\n"))
      (when (> (length org-mindmap-style-other) 0)
        (insert (concat " " org-mindmap-style-other ",\n")))
      (insert " mindmap]\n\n"))
    (save-excursion
      (org-back-to-heading)
      (save-restriction
        (org-narrow-to-subtree)
        (org-mindmap-walk-tree 0)))
    (with-current-buffer org-mindmap-export-buffer
      (insert "\\end{tikzpicture}")
      (insert org-mindmap-footer)
      (write-file file-name t)
      (switch-to-buffer (current-buffer)))))

