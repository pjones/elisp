;;; org-crypt.el --- Public key encryption for org-mode entries

;; Copyright (C) 2009 Peter Jones <pjones@pmade.com>
;; Copyright (C) 2007 John Wiegley <johnw@gnu.org>

;; Emacs Lisp Archive Entry
;; Filename: org-crypt.el
;; Version: 0.3
;; Keywords: org-mode
;; Author: John Wiegley <johnw@gnu.org>
;; Maintainer: Peter Jones <pjones@pmade.com>
;; Description: Adds public key encryption to org-mode buffers
;; URL: http://www.newartisans.com/software/emacs.html
;; Compatibility: Emacs22

;; This file is not part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2, or (at your option) any later
;; version.
;;
;; This is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;; MA 02111-1307, USA.

;;; Commentary:

;; Right now this is just a set of functions to play with.  It depends
;; on the epg library.  Here's how you would use it:
;;
;; 1. To mark an entry for encryption, tag the heading with "crypt".
;;    You can change the tag to any complex tag matching string by
;;    setting the `org-crypt-tag-matcher' variable.
;;
;; 2. Set the encryption key to use in the `org-crypt-key' variable,
;;    or use `M-x org-set-property' to set the property CRYPTKEY to
;;    any address in your public keyring.  The text of the entry (but
;;    not its properties or headline) will be encrypted for this user.
;;    For them to read it, the corresponding secret key must be
;;    located in the secret key ring of the account where you try to
;;    decrypt it.  This makes it possible to leave secure notes that
;;    only the intended recipient can read in a shared-org-mode-files
;;    scenario.
;;
;; 3. Next, at the top of your org-mode buffer, add this line:
;;
;;      -*- mode: org; before-save-hook: (org-encrypt-entries) -*-
;;
;;    This ensures that entries marked for encryption are encrypted
;;    whenever the file is saved.  If you want encryption to be
;;    manual, use `org-encrypt-entries' or `org-encrypt-entry'.
;;
;; 4. To later decrypt an entry, use `org-decrypt-entries' or
;;    `org-decrypt-entry'.  It might be useful to bind this to a key,
;;    like C-c C-/.  I hope that in the future, C-c C-r can be might
;;    overloaded to also decrypt an entry if it's encrypted, since
;;    that fits nicely with the meaning of "reveal".
;;
;; TODO:
;;   - Automatically hook into `before-save-hook' if so configured
;;   - Allow symmetric encryption as well

(require 'epg)

(defgroup org-crypt nil
  "Org Crypt"
  :tag "Org Crypt" :group 'org)

(defcustom org-crypt-tag-matcher "crypt"
  "The tag matcher used to find headings whose contents should be
encrypted.  See the \"Match syntax\" section of the org manual
for more details."
  :type 'string :group 'org-crypt)

(defcustom org-crypt-key nil
  "The default key to use when encrypting the contents of a
heading.  This can also be overridden in the CRYPTKEY property."
  :type 'string :group 'org-crypt)

(defun org-crypt-key-for-heading ()
  "Returns the encryption key for the current heading."
  (save-excursion
    (org-back-to-heading t)
    (or (org-entry-get nil "CRYPTKEY" 'selective) 
        org-crypt-key
        (and (boundp 'epa-file-encrypt-to) epa-file-encrypt-to)
        (error "no crypt key set"))))

(defun org-encrypt-entry ()
  "Encrypt the content of the current headline."
  (interactive)
  (save-excursion
    (org-back-to-heading t)
    (forward-line)
    (let ((folded (org-invisible-p))
	  (epg-context (epg-make-context nil t t))
          (crypt-key (org-crypt-key-for-heading))
          (beg (point))
          end encrypted-text)
      (when (and (not (looking-at "-----BEGIN PGP MESSAGE-----"))
		 (progn
		   (org-end-of-subtree t t)
		   (org-back-over-empty-lines)
		   t))
        (setq end (point)
              encrypted-text
              (epg-encrypt-string 
               epg-context
               (buffer-substring-no-properties beg end)
               (epg-list-keys epg-context crypt-key)))
        (delete-region beg end)
        (insert encrypted-text)
	(when folded
	  (save-excursion
	    (org-back-to-heading t)
	    (hide-subtree)))
        nil))))

(defun org-decrypt-entry ()
  (interactive)
  (save-excursion
    (org-back-to-heading t)
    (forward-line)
    (when (looking-at "-----BEGIN PGP MESSAGE-----")
      (let* ((beg (point))
             (end (save-excursion 
                    (search-forward "-----END PGP MESSAGE-----")
                    (forward-line)
                    (point)))
             (epg-context (epg-make-context nil t t))
             (decrypted-text (epg-decrypt-string
                              epg-context
                              (buffer-substring-no-properties beg end))))
        (delete-region beg end)
        (insert decrypted-text)
        nil))))

(defun org-encrypt-entries ()
  (interactive)
  (org-scan-tags
   'org-encrypt-entry
   (cdr (org-make-tags-matcher org-crypt-tag-matcher))))

(defun org-decrypt-entries ()
  (interactive)
  (org-scan-tags 
   'org-decrypt-entry
   (cdr (org-make-tags-matcher org-crypt-tag-matcher))))

(provide 'org-crypt)

;;; org-crypt.el ends here
