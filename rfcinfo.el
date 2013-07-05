;;; rfcinfo.el --- Displaying and browsing status information about RFCs
;;;                Downloading and jumping to RFC locations

;; Copyright (C) 2005-2013 Christophe Deleuze <christophe.deleuze@free.fr>
;; Created: Feb 2005
;; Version: 0.54 / Jun 2013
;; --Version: 0.53 / Jun 2013
;; --Version: 0.52 / Mar 2013
;; --Version: 0.51 / Jan 2013
;; --Version: 0.5 / Nov 2012
;; --Version: 0.34 / 28 Sep 2012
;; --Version: 0.33 / 12 Mar 2012
;; --Version: 0.31 / xx Aug 2011

;; Last version available at http://christophe.deleuze.free.fr/ZZZ github

;; This file is NOT part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation; either version 2, or (at your option) any later
;; version.

;; This file is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;; more details.

;; You should have received a copy of the GNU General Public License along
;; with this program; if not, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

;;; Commentary:

;; RFCs are documents published by the IETF, including Internet
;; standards.  These are of diffent categories including standard,
;; informational etc.  Since once published, an RFC is never modified,
;; each RFC has an evolving `status' indicating it's been obsolated or
;; updated and if so by what other RFCs. This file provides functions
;; for displaying and browsing RFC's status information as well as
;; downloading them and jumping to a precise location.

;; If rfcview-mode is used to view RFCs, a few keys are also added to
;; rfcview-mode-map so that rfcinfo functions can easily be called
;; from rfcview-mode.  However, rfcinfo does not require rfcview to be
;; loaded or available.  rfcview can be downloaded from
;; http://www.loveshack.ukfsn.org/emacs/rfcview.el

;; The information is taken from an official rfc-index.xml file from
;; IETF RFC repository (see `rfcinfo-remote-repository') which can be
;; copied locally to `rfcinfo-index-xml-file'.  This can be done
;; manually or with `rfcinfo-refresh'.  The information is "imported"
;; into a lisp vector that is then used to answer information queries.
;; Since the import is a bit long (typically a few seconds) the vector
;; is saved as `rfcinfo-dbfile'.

;; rfcinfo-load   loads the content of rfcinfo-dbfile in memory.
;; rfcinfo-import imports from xml file, saves to rfcinfo-dbfile and
;;                loads it.
;; rfcinfo-refresh downloads (fresh) xml file and imports.

;; rfcinfo-refresh should be invoked regularly (probably not more that
;; once a few weeks) to keep available information up to date.

;; rfcinfo-show

;;    show rfc title, authors, date and current status (category, list
;;    of rfcs that are obsoleted/updated, list of rfcs that
;;    obsolete/update it).  Arrow keys allow to navigate through the
;;    above 4 categories of related RFCs.

;; rfcinfo-open

;;    take a 'rfc ref' as argument: rfc number and optionnally a
;;    section number following a '-' (eg 1035-6.4.1), open rfc and
;;    position point on the section header.  The rfc status (category
;;    and update/obsolete stuff) is written in the echo area.  The
;;    section number can also be followed by a "+" and a line offset
;;    to move down (eg 1035-6.4.1+18).  This can be useful to get to a
;;    very paragraph in a long section.
;;
;;    The RFC is either read from a local cache directory or
;;    downloaded with ange-ftp.

;; rfcinfo-goto

;;    move point to header of given section number.

;; ZZZ The above three functions read their argument from the mini-buffer.
;; If point is on a possible argument, this is proposed as default.
;; + prefix arg

;; Should work on all versions of (x)Emacs.
;; Please let me know if this is not the case.

;; Installation
;;
;; copy file rfcinfo.el to some directory and add the following to eg
;; your .emacs file

;; (add-to-list 'load-path <the chosen directory>)
;; (global-set-key "\C-cr" 'rfcinfo-show)
;; (global-set-key "\C-cR" 'rfcinfo-open)
;; 
;; (autoload 'rfcinfo-show "rfcinfo" nil t)
;; (autoload 'rfcinfo-open "rfcinfo" nil t)


;;; -----

;;; Code:

;; Download/View
;; TODO? http://www.ietf.org/rfc/rfcxxxx.txt ftp://ftp.ripe.net/rfc/rfcxxxx.txt

;; Global Variables:

(defvar rfcinfo-remote-repository "/anonymous@ftp.ripe.net:/rfc/"
  "FTP repository where RFCs and index will be downloaded from.

The name is an ange-ftp directory.  You may have to set/customize
ange-ftp-try-passive-mode.")

(defvar rfcinfo-cache-dir "~/.cache/rfc/" "RFC local cache directory.
This is where downloaded RFCs will be stored for later offline
access, if `rfcinfo-cache-flag' is non nil.")

(defvar rfcinfo-cache-flag t
  "If non nil, store a copy of each downloaded RFC in `rfcinfo-cache-dir'.")

(defvar rfcinfo-index-xml-file "~/.cache/rfc/rfc-index.xml"
;(defvar rfcinfo-index-xml-file "/home/deleuzec/rfc-index.xml"
  "The local (path)name of the rfc-index.xml file")

(defvar rfcinfo-dbfile "~/.cache/rfc/rfcinfo.db"
  "Pathname of the file where rfc information will be stored")

(defvar rfcinfo-errata-url-prefix
  "http://www.rfc-editor.org/errata_search.php?rfc="
  "Prefix of the errata URL, for RFCs that have one")


;; no defvar?
(defvar rfcinfo-status nil "array of rfc status")
(defvar rfcinfo-window nil);;ZZZ
(defconst rfcinfo-buffer "*RFC info*")

;;; A few general purpose functions

(defun rfcinfo-foldl (f e l)
  "Constant space fold-left"
  (setq rest l
	acc e)
  (while (consp rest)
    (setq acc (apply f acc (car rest) nil)
	  rest (cdr rest)))
  acc)

(defun rfcinfo-foldl1 (f l)
  "Constant space fold-left1"
  (rfcinfo-foldl f (car l) (cdr l)))

(defun rfcinfo-filter (p l)
  "Constant space filter"
  (nreverse
   (rfcinfo-foldl (lambda (acc e) (if (funcall p e) (cons e acc) acc)) nil l)))

(defun rfcinfo-take (n l)
  "Take first N elements of list L"
  (let ((test (lambda (e) (setq n (1- n)) (>= n 0))))
    (rfcinfo-filter test l)))

(defun rfcinfo-string-n-lines (n s)
  "Take first N lines of string S"
  (let ((lines (split-string s "\n")))
    (apply 'concat (mapcar (lambda (s) (concat s "\n")) ;
		    (rfcinfo-take n lines)))))

;;; First the user entry points

(defun rfcinfo-show (ask)
  "Display RFC information.
Number is taken at point, from buffer name, or asked.  If ASK is
non nil, always ask (found number proposed as default)."
  (interactive "P")
  (let ((nb (car (rfcinfo-read-ref t "Info for " ask))))
    (rfcinfo-do-show nb nil)))

(defun rfcinfo-open (ask)
  "Open RFC and position point to section number.
The RFC is either loaded from `rfcinfo-cache-dir' or downloaded
from `rfcinfo-remote-repository."
  (interactive "P")
  (rfcinfo-do-open (rfcinfo-read-ref t "View " ask t)))

(defun rfcinfo-goto (ask)
  "Goto given section number in current RFC."
  (interactive "P")
  (apply 'rfcinfo-do-goto (rfcinfo-read-sec ask)))
		   
;;; Readind and printing refs and secs

;; We reference a rfc by giving its number and possibly a section
;; number.  The user representation is as a string of the form
;; "1035-4.1.1" or "1035" if no particular section is given.  A line
;; offset to move down after the section heading can also be
;; introducted by a "+" after the section number, eg "1035-4.1.1+21"

;; The string can be prefixed with "RFC" or "RFC " or "RFC-", or in
;; lowercase.

;; The internal representation of a ref is a list containing the rfc
;; number as car and a 'sec' as cdr.  The 'sec' is empty if no section
;; is given, otherwise it is a list of three elements:
;;   1. a regexp to find the section header,
;;   2. the offset line number,
;;   3. the text representation of the sec

;; The ref for the three examples above would be
;; (1035 "^ *4\\.1\\.1\\.? +" 0 "4.1.1"),
;; (1035), and
;; (1035 "^ *4\\.1\\.1+21\\.? +" 21 "4.1.1+21")

;; Produce a sec from the string representation
;; The rationale for the regexp:
;; - should be at beginning of line (with possible blanks)
;; - need to escape '.' from the special regexp meaning
;; - the numbers end with an optional point, and at least one space

(defun rfcinfo-sec-of-string (st)
  "Build a sec from string ST."
  (if (string= st "") nil
    (let* ((sec-lin (split-string st "+"))
	   (sec (car sec-lin))
	   (lin (cadr sec-lin))
	   (string-map-st (lambda (f s) (apply 'concat (mapcar f (string-to-list s))))))
      (list
					; the regexp
       (concat "^ *"
	       (funcall string-map-st
			(lambda (c) (if (= c ?.) "\\." (string c)))
			sec)
	       "\\.? +")
					; the line offset
       (if (null lin) 0 (string-to-number lin))
					; the string representation
       st))))

(defun rfcinfo-ref-of-string (st)
  "Build a ref from string ST."
  (let* ((nb-sec (split-string st "-"))
	 (nbs (car nb-sec)))
    (if (or (null nbs) 
	    (zerop (string-to-number nbs)))
	nil
      (cons (string-to-number nbs)
	    (if (null (cadr nb-sec)) nil
	      (rfcinfo-sec-of-string (cadr nb-sec)))))))



(defun rfcinfo-string-of-ref (ref nbonly)
  "String representation of ref REF.
Ignore section par if NBONLY is non-nil."
  (if (null ref) ""
    (if (or nbonly (null (cdr ref))) (number-to-string (car ref))
      (concat (number-to-string (car ref)) "-" (cadddr ref)))))

(defun rfcinfo-sec-at-point ()
  "Try to find a section number at point."
  (save-excursion
    (let* ((end (progn (skip-chars-forward "0123456789\.")  (point)))
	   (beg (progn (skip-chars-backward "0123456789\.") (point)))
	   
	   (word (buffer-substring-no-properties beg end)))
      (rfcinfo-sec-of-string word))))

;; Try to find a ref at point

;; ref can start with rfc1034 or RFC 1035 or RFC-2205 or 2616.
;; We also accept '+' to read a possible line offset. ZZZ

(defun rfcinfo-ref-at-point ()
  (save-excursion
    ;; skip any [s, in case of [rfc-1035] (rfcview buttons)
    (if (eq (char-after) ?\[) (skip-chars-forward "["))
    (if (string-prefix-p "rfc" (downcase (or (thing-at-point 'word) "")))
	(progn (skip-chars-forward "RFCrfc-")
	       (if (eq (char-after) 32) (forward-char))))
    (let* ((end (progn (skip-chars-forward  "-+0123456789\.") (point)))
    	   (beg (progn (skip-chars-backward "-+0123456789\.") 
    		       ;; in case of RFC-2205..., do not include the initial '-'
    		       (if (eq (char-after) ?-) (1+ (point)) (point))))	    
	   (word (buffer-substring-no-properties beg end)))
      (rfcinfo-ref-of-string word))))

;; if current buffer holds an rfc (ie a file named rfcxxx.txt) return
;; its number (as a ref, so a singleton list) else return nil

(defun rfcinfo-buffer-holds-one ()
  (ignore-errors
    (if (and
	 (string-prefix-p "rfc" (buffer-name))
	 (string= (substring (buffer-name) -4) ".txt"))
	(list (string-to-number (substring (buffer-name) 3 -4)))
      nil)))

(defun rfcinfo-read-ref (nbonly msg ask &optional non-local)
  (let ((def (or (rfcinfo-ref-at-point)
		 (and (not non-local) (rfcinfo-buffer-holds-one)))))
    (if (or ask (not def))
	(let ((sdef (rfcinfo-string-of-ref def nbonly)))
	  (rfcinfo-ref-of-string (read-string
				  (if (null def) (concat msg "RFC number: ")
				    (concat msg "RFC number (default "
					    sdef "): "))
				  nil nil sdef)))
      def)))

(defun rfcinfo-read-sec (ask)
  (let ((def (rfcinfo-sec-at-point)))
    (if (or ask (not def))
	(let ((sdef (caddr def)))
	  (rfcinfo-sec-of-string (read-string
				  (if (null def) "Goto section: "
				    (concat "Goto section (default " sdef "): "))
				  nil nil sdef)))
      def)))


;;; blabla


;; better use locally/dynamic bound echo-flag to activate echo area?

;; we'll enable undo only after having written one time in the buffer,
;; so that undo can't take us back to an empty buffer.
(setq rfcinfo-first-done nil)

(defun rfcinfo-do-show (nb echo)
  ""
  (get-buffer-create rfcinfo-buffer)
  (let ((st (rfcinfo-get-status nb)))
    (if echo (message (rfcinfo-string-n-lines 3 st))
      (if (window-live-p rfcinfo-window) ()
	(setq rfcinfo-window (split-window-vertically)))
      (select-window rfcinfo-window)
      (set-buffer rfcinfo-buffer)
      (if rfcinfo-first-done (buffer-enable-undo)
	(buffer-disable-undo)
	(setq rfcinfo-first-done t))
      (let ((inhibit-read-only t))
	(erase-buffer)
	(insert st)
	(goto-char (point-min))
	(forward-word))
      (rfcinfo-mode)
      (set-window-buffer rfcinfo-window rfcinfo-buffer)
      (fit-window-to-buffer))))

;; ZZZ
(defun deps (typ header)
  (if (null typ) ""
    (rfcinfo-foldl (lambda (acc n)
	     (let* ((rfc (aref rfcinfo-status n))
		    (tit (cadr (assoc 'title rfc))))
	       (concat acc "\n" (format "   %4d - %s" n tit))))
	   header
	   typ)))


(defun rfcinfo-get-status (nb)
  (condition-case nil
      (with-temp-buffer
	(let ((rfc     (aref rfcinfo-status nb)))
	  (if (null rfc) (message (format "%d: unknown RFC" nb))
	    (let ((title   (cadr (assoc 'title rfc)))
		  (authors (cdr  (assoc 'authors rfc)))
		  (status  (cadr (assoc 'status rfc)))
		  (date    (cdr  (assoc 'date rfc)))
		  (upd     (cdr  (assoc 'updates rfc)))
		  (obs     (cdr  (assoc 'obsoletes rfc)))
		  (upd-by  (cdr  (assoc 'updated-by rfc)))
		  (obs-by  (cdr  (assoc 'obsoleted-by rfc)))
		  (errata  (assoc 'errata rfc)))
	      
	      (insert (format "%d -- %s\n%S  %S %i %s\n%s\n%s%s%s%s" nb title status 
			      (car date) (cadr date)
			      (if errata
				  (concat (propertize "Errata"
						      'mouse-face 'highlight
						      'face '(:foreground "red")
						      'help-echo "toto") " ")
				"")
			      (rfcinfo-foldl1 (lambda (acc s) (concat acc ", " s))  authors)
			      (deps upd-by "\nupdated by") (deps obs-by "\nobsoleted by")
			      (deps upd "\nupdates") (deps obs "\nobsoletes"))))
	    ;; apply properties to string
	    (rfcinfo-set-properties)
	    (buffer-substring (point-min) (point-max)))))
    ;; error if we aref out of the array
    (error (error "Unknown RFC number: %d" nb))))

(defun rfcinfo-do-open (ref)
  (let* ((rfc (concat "rfc" (number-to-string (car ref)) ".txt"))
	 (localname (concat rfcinfo-cache-dir rfc)))
    (if (file-exists-p localname) (rfcinfo-view localname (cadr ref) (caddr ref) (cadddr ref))
      (message "Not in cache. Attempting Download...")
      (rfcinfo-view (concat rfcinfo-remote-repository rfc) (cadr ref) (caddr ref) (cadddr ref))
      (if rfcinfo-cache-flag (write-file localname)))
    (rfcinfo-do-show (car ref) t)))


;; position point to last occurence of regexp
;; don't move if not found
;; we search from end of file to avoid finding the TOC entry

(defun rfcinfo-do-goto (regexp fwl st)
  (let ((p (point)))
    (goto-char (point-max))
    (if (null (search-backward-regexp regexp nil t))
	(progn (goto-char p)
	       (error "Coudn't find section %s." st))
      (forward-line fwl)
      (recenter 0))))

(defun rfcinfo-view (name regexp lines sec)
  (view-file-other-window name)
  (if (null regexp) nil
    (rfcinfo-do-goto regexp lines sec)))

;;; TODO undocumented user functions

(defun rfcinfo-bortzmeyer ()
  (interactive)
  (let* ((ref (rfcinfo-read-ref t "View Bortzmeyer blog entry for " nil))
	 (url (concat "http://www.bortzmeyer.org/"
		      (number-to-string (car ref)) ".html")))
    (browse-url url)))
;;    (w3m url)))

;; major mode for RFC info

(put 'rfcinfo-mode 'mode-class 'special)

(defconst rfcinfo-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [down]   'rfcinfo-next)
    (define-key map [up]     'rfcinfo-prev)
    (define-key map " "      'rfcinfo-scroll)
    (define-key map [right]  'rfcinfo-follow)
    (define-key map [return] 'rfcinfo-viewfile)
    (define-key map [left]   'rfcinfo-back)
    (define-key map "q"      'rfcinfo-quit)
    ;; ZZZ bad for documentation
    (define-key map "o"      (lambda () (interactive) (rfcinfo-show t)))
    (define-key map [mouse-2] 'rfcinfo-click)
    (define-key map "e"      'rfcinfo-errata)
    (define-key map "b"      'rfcinfo-bortzmeyer)
    (define-key map "s"      'rfcinfo-search-title)
    (define-key map "?"      'describe-mode)
    map)
  "Keymap for `rfcinfo-mode'.

Woo-bah!")

(define-derived-mode rfcinfo-mode nil "RFC info"
  "Major mode for navigating RFC info.

Category and status of each RFC is shown by the display style
used for its number.  Obsolete RFCs are striked-through, updated
ones are underlined.  Standard tracks RFCs (either proposed,
draft or full) use bold shape, other categories are shown with
colors: blue=informationnal, green=best current practice,
orange=experimental, purple=historic.

\\{rfcinfo-mode-map}"
  (if (not (string= (buffer-name) rfcinfo-buffer))
      (error "You shouldn't set rfcinfo-mode yourself")
    (setq buffer-read-only t)))


(defconst rfcinfo-regexp "\\([[:digit:]]+\\) -"
  "Regexp to locate an RFC number in *RFC info* buffer.")

(setq rfcinfo-scroll-up t)

(defun rfcinfo-scroll ()
  ""
  (interactive)
  (condition-case nil
      (if rfcinfo-scroll-up (progn (scroll-up) (rfcinfo-next))
	(scroll-down) (rfcinfo-prev))
    (error (setq rfcinfo-scroll-up (not rfcinfo-scroll-up))
	   (if rfcinfo-scroll-up (progn (scroll-up) (rfcinfo-next))
	     (scroll-down) (rfcinfo-prev)))))

(defun rfcinfo-prev ()
  "Move cursor to previous dependency."
  (interactive)
  (backward-word)
  (search-backward-regexp rfcinfo-regexp nil t)
  (goto-char (match-end 1)))

(defun rfcinfo-next ()
  "Move cursor to next dependency."
  (interactive)
  (forward-word)
  (search-forward-regexp rfcinfo-regexp nil t)
  (goto-char (match-end 1)))

(defun rfcinfo-status (ask)
  (interactive "P")
  (rfcinfo-do-show (car-safe (rfcinfo-read-ref t "" ask)) nil))

(defun rfcinfo-status-echo (ask)
  (interactive "P")
  (rfcinfo-do-show (car-safe (rfcinfo-read-ref t "" ask)) t))

;; TODO: maybe use text property to check we're on 'another' rfc nb?
(defun rfcinfo-follow ()
  (interactive)
  ;; do nothing if we're on the first line (goto ourselves)
  (if (> (point) 10)
      (rfcinfo-do-show (car (rfcinfo-ref-at-point)) nil)))

(defun rfcinfo-viewfile ()
  (interactive)
  (rfcinfo-do-open (list (car (rfcinfo-ref-at-point)))))

(defun rfcinfo-back ()
  "Go back to previous RFC."
  (interactive)
  (let ((inhibit-read-only t)) (undo-only)
       (fit-window-to-buffer)))

(defun rfcinfo-quit ()
  "Close rfcinfo window."
  (interactive)
  (condition-case nil
      (delete-window rfcinfo-window)
    (error (bury-buffer)))
  ;; ensure we go back to rfcview window if rfcinfo-show was called
  ;; from it
  (select-window (get-lru-window)))

(defun rfcinfo-set-properties ()
  (goto-char (point-min))
  (while (search-forward-regexp rfcinfo-regexp nil t)
    (let* ((nb  (string-to-number (match-string 0)))
	   (rfc (aref rfcinfo-status nb))
	   (cat (cadr (assoc 'status rfc)))
	   (updated  (assoc 'updated-by rfc))
	   (obsolete (assoc 'obsoleted-by rfc))
	   ;; face for category
	   (cf (cond
		((member cat '(standard draft-std proposed-std)) 'bold)
		((eq cat 'bcp)           '(:foreground "green"))
		((eq cat 'informational) '(:foreground "blue"))
		((eq cat 'historic)      '(:foreground "purple"))
		((eq cat 'experimental)  '(:foreground "orange"))))
	   ;; face for obsolete/updated status
	   (of (cond
		;; these are not exclusive, we use obsolete if both are set
		(obsolete '(:strike-through t))
		(updated  '(:underline t)))))

      (add-text-properties
       (match-beginning 1)
       (match-end 1)
       `(mouse-face highlight
         face ,(list cf of)
         help-echo ,(concat "Yo! " (cadr (assoc 'title rfc)))))))
  (goto-char (point-min)))

(defun rfcinfo-errata (arg)
  "Browse to errata page of current RFC, if any."
  (interactive "P")
  (let ((nb (if arg 
		(read-string "Errata for RFC: ")
	      (save-excursion
		(goto-char 0)
		(thing-at-point 'word)))))
    (if (assoc 'errata (aref rfcinfo-status (string-to-number nb)))
	(browse-url (concat rfcinfo-errata-url-prefix nb))
      (message (format "RFC %s has no errata" nb)))))


(defun rfcinfo-click (event)
  "Click click!"
  (interactive "e")
  (let ((pos (posn-point (event-end event))))
    (goto-char pos)
    (if (string= (thing-at-point 'word) "Errata")
	(rfcinfo-errata)
      (rfcinfo-status))))

;;; Searching the DB

(defun rfcinfo-search-any (field word)
  (loop for i from 0 to (1- (length rfcinfo-status))
	if (let ((fi (assq field (aref rfcinfo-status i))))
	     (and fi (string-match word (cadr fi))))
	collect i))

;; case-fold-search

(defun rfcinfo-search-title-get (word)
  (with-temp-buffer
    (insert
     (let ((l (rfcinfo-search-any 'title word)))
       (format "Search results for \"%s\" in title (%i entries)\n%s" word (length l) (deps l ""))))
    (rfcinfo-set-properties)
    (buffer-substring (point-min) (point-max))))
    
(defun rfcinfo-search-title (word)
  ""
  (interactive "MSearch titles for word: ")
  (get-buffer-create rfcinfo-buffer)
  (let ((st (rfcinfo-search-title-get word)))
    (if (window-live-p rfcinfo-window) ()
      (setq rfcinfo-window (split-window-vertically)))
    (select-window rfcinfo-window)
    (set-buffer rfcinfo-buffer)
    (if rfcinfo-first-done (buffer-enable-undo)
      (buffer-disable-undo)
      (setq rfcinfo-first-done t))
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert st)
      (goto-char (point-min))
      (forward-word))
    (rfcinfo-mode)
    (set-window-buffer rfcinfo-window rfcinfo-buffer)
    (fit-window-to-buffer)))

(defun rfcinfo-search-show (word)
  ""
  (interactive "MSearch titles for word: ")
  (get-buffer-create rfcinfo-buffer)
  (let ((st (rfcinfo-search-title word)))
    (if (window-live-p rfcinfo-window) ()
      (setq rfcinfo-window (split-window-vertically)))
    (select-window rfcinfo-window)
    (set-buffer rfcinfo-buffer)
    (if rfcinfo-first-done (buffer-enable-undo)
      (buffer-disable-undo)
      (setq rfcinfo-first-done t))
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert st)
      (goto-char (point-min))
      (forward-word))
    (rfcinfo-mode)
    (set-window-buffer rfcinfo-window rfcinfo-buffer)
    (fit-window-to-buffer)))

;;; Importing, loading, and saving

;; return list of elements with given tag
(defun rfcinfo-filter-tag (tag l)
  (rfcinfo-filter (lambda (e) (eq (car-safe e) tag)) l))

(defun rfcinfo-filter-tags (tags l)
  (mapcar
       (lambda (l) (cons (car l) (cddr l))) ;; this removes the nil attribute
       (rfcinfo-filter (lambda (e) (member (car-safe e) tags)) l)))

(defun rfcinfo-status-symbol (s)
  (let ((string-symbol '(("INTERNET STANDARD" . standard)
			 ("DRAFT STANDARD"    . draft-std)
			 ("PROPOSED STANDARD" . proposed-std)
			 ("UNKNOWN"           . unknown)
			 ("BEST CURRENT PRACTICE" . bcp)
			 ("EXPERIMENTAL"      . experimental)
			 ("HISTORIC"          . historic)
			 ("INFORMATIONAL"     . informational))))
    (cdr (assoc s string-symbol))))

(defun rfcinfo-month-symbol (s)
  (let ((string-symbol 
	 '(("January"  . jan) ("May"    . may) ("September" . sep)
	   ("February" . feb) ("June"   . jun) ("October"   . oct)
	   ("March"    . mar) ("July"   . jul) ("November"  . nov)
	   ("April"    . apr) ("August" . aug) ("December"  . dec))))
    (cdr (assoc s string-symbol))))

;; take an rfc element, fold inner elements
;; result has form (nb (title "title") (field field_values) ... )

(defun rfcinfo-fold-all (es)
  (let ((deps '(obsoletes updates obsoleted-by updated-by))

	;; get the number xxxx from a string of the form "RFCxxxx"
	(nb   '(lambda (s) (string-to-number (substring s 3 7))))

	(date '(lambda (e) (let ((month (rfcinfo-filter-tag 'month e))
				 (year  (rfcinfo-filter-tag 'year  e)))
			     (list (rfcinfo-month-symbol (caddar month))
				   (string-to-number (caddar year))))))
	)
    (rfcinfo-filter 
     (lambda (e) (not (null e))) ;; empty list from non RFC dependencies
     (mapcar (lambda (el)
	       (if (consp el) ;; ???
		   (cond
		    ;; this RFC number
		    ((eq (car el) 'doc-id) (funcall nb (cadr el)))
		    
		    ;; one of the 4 dependency lists
		    ((member (car el) deps)
		     (let* ((ids (rfcinfo-filter-tag 'doc-id el))
			    ;; filter out old non RFC dependencies
			    (l   (rfcinfo-filter (lambda (s) (string= (substring s 0 3) "RFC"))
						 (mapcar 'caddr ids))))
		       (if (null l) nil ;; empty list
			 (cons (car el) (mapcar nb l)))))
		    
		    ((eq (car el) 'date)
		     (cons 'date (funcall date el)))
		    
		    ((eq (car el) 'author)
		     (cons 'author (cddar (rfcinfo-filter-tag 'name  el))))
		    
		    ((eq (car el) 'current-status)
		     (list 'status (rfcinfo-status-symbol (cadr el))))
		    
		    ((eq (car el) 'errata-url)
		     (list 'errata))
		    
		    (t el))
		 el))
	     es))))

(defun rfcinfo-import ()
  ""
  (interactive)
  (message "Importing RFC info from XML file.  This may take some time...")

  (let (;; the elements we need inside an rfc-entry
	(fields '(doc-id title author date obsoletes updates 
			 obsoleted-by updated-by current-status
			 errata-url))
	;; replace the several author elements by one ('authors ...) list
	(fold-authors '(lambda (el)
			 (let ((wo-authors
				(nreverse (rfcinfo-foldl
					   (lambda (acc e) (if (eq (car-safe e) 'author)
							       acc (cons e acc)))
					   nil el)))
			       (authors (mapcar 'cadr (rfcinfo-filter-tag 'author el))))
			   (append wo-authors (list (cons 'authors authors))))))
	)
    (let* (;; all rfc-entry elements
	   (full (rfcinfo-filter-tag 'rfc-entry
				     (car (xml-parse-file rfcinfo-index-xml-file))))
	   ;; filter relevant sub elements
	   (some (mapcar (lambda (e) (rfcinfo-filter-tags fields e)) full))

	   ;; turn each entry in a simple list of (attr-type attr-values)
	   (props (mapcar 'rfcinfo-fold-all some))

	   ;; merge 'author lists in a single 'authors list
	   (rfcs (mapcar fold-authors props))

	   (max  (1+ (caar (last rfcs))))
	   (v    (make-vector max nil)))
      ;; loop from 'cl (TODO: change to elisp basic while?)
      (loop for e in rfcs do
	    (aset v (car e) (cdr e)))
      (setq rfcinfo-status v)
      (with-temp-file rfcinfo-dbfile
	(insert (with-output-to-string (prin1 rfcinfo-status))))
      (message (format "Done (last rfc %i)." (1- max))))))

(defun rfcinfo-refresh ()
  "Download rfc-index.xml file and import it.

File is downloaded from `rfcinfo-remote-repository'."
  (interactive)
  (with-temp-buffer
    (insert-file (concat rfcinfo-remote-repository "rfc-index.xml"))
    (write-file rfcinfo-index-xml-file))
  (rfcinfo-import))

(defun rfcinfo-load ()
  (interactive)
  (condition-case nil
      (with-temp-buffer
	(insert-file rfcinfo-dbfile)
	(setq rfcinfo-status (read (current-buffer))))
    (error (error "Can't load from `rfcinfo-dbfile'"))))

;;; Initializations

(rfcinfo-load)			       ; Load rfcinfo vector in memory

;; If and when rfcview is loaded, we want to add a few keys to its
;; mode map.  However, we don't want to force loading it.  First, it
;; may not be available, second if the user just browses RFC info
;; without displaying RFCs, it would be useless to load it.

;; Elisp reference manual (15.10) discourages use of eval-after-load
;; but I don't know how to do that without it.

(eval-after-load "rfcview"
  '(progn (define-key rfcview-mode-map "O" 'rfcinfo-open)
	  (define-key rfcview-mode-map "i" 'rfcinfo-status-echo)
	  (define-key rfcview-mode-map "I" 'rfcinfo-status)
	  (define-key rfcview-mode-map "G" 'rfcinfo-goto)))

;; unless rfcview honors a rfcview-load-hook... then we can do the following:

;; (defun rfcinfo-change-rfcview-map ()
;;   (progn (define-key rfcview-mode-map "O" 'rfcinfo-open)
;; 	 (define-key rfcview-mode-map "i" 'rfcinfo-status-echo)
;; 	 (define-key rfcview-mode-map "I" 'rfcinfo-status)
;; 	 (define-key rfcview-mode-map "G" 'rfcinfo-goto)))

;; (if (boundp 'rfcview-mode-map)
;;     (rfcinfo-change-rfcview-map)
;;   (add-hook 'rfcview-load-hook 'rfcinfo-change-rfcview-map))

(provide 'rfcinfo)  

;;; rfcinfo.el ends here



;;; Description of rfc-info.xml schema

;; top level elements: bcp-entry, fyi-entry, rfc-entry, std-entry, rfc-not-issued-entry

;; see xml-parse-region from xml.el
;;  lots of white space strings?

;; (car toto)
;; (elt e 1 3 5 ....) each are tags (tag attr content)
;;       with attr=nil
;;       content is whitespace strings and tags...
;; 
;; search for top-level tags
;; * doc-id
;; * title
;; * author + (subtag name)
;; * date   (subtags month year)
;;   format       (file-format char-count page-count)
;;   keywords
;;   abstract      (p)
;; * obsoletes     (subtag doc-id+)
;; * updates       " "
;; * obsoleted-by  " "
;; * updated-by    " "
;;   draft
;;   is-also
;; * current-status
;;   publication-status
;;   stream             Legacy, IETF ... ?
;;   area
;;   wg_acronym
;;   errata-url   (present only if actual errata exist?)

;; xml file has bug for 'Editors'


;;; TODO additional features

;; put on github

;; search on title (search.el)

;; search in abstract

;; show abstract
;;     -> what exactly does abstract element contain in xml index?
;;        document abstract if one exists + [STANDARDS-TRACK] if applicable
;; no abstract: rfc1034 ...

;; xml also has keywords...

;; generate map (viz) and display it in browser
;; display map in emacs?
;; possible control back from browser to emacs?
;;  -> click on rfc on map -> display info in emacs...

;; show local availability status
;; diff status (when open)/summary?
;; sb-rfcview.el ?
;; provide a menu ?
;; also use w3m to download?
;; unfold/fold updby/obsoby trees
;; ?n show next rfc...
;; SPC/DEL for scroll forward/backward ?
;; debian package doc-rfc (see rfcview)
;; user annotations (related rfcs, free text...)
;;   alist, integrated in vector during import (or when load-ing?)
;; set-window-dedicated-p ?
;; customize faces
;; cache dir should be shared ?
;; look at ffap ?
;; faces for tty

;; build sec with +l from point
;;   -> based on rfcview headings

;; how to handle rfcview optional dependency?
;;  -> build-ref only (easily) callable from rfcview-mode

(defun rfcinfo-drop-final-dot (s)
  (if (eq (aref s (1- (length s))) ?.)
      (substring s 0 (1- (length s)))
    s))

(defun rfcinfo-build-ref ()
  "Build a ref for current point in current RFC.
Uses rfcview-local-heading-alist.  ref is displayed and
saved to kill ring."
  (interactive)
  (let ((rfc (car-safe (rfcinfo-buffer-holds-one)))
	(res nil))
    (if (null rfc) (error "Not an rfc here!")
      (setq headers (reverse rfcview-local-heading-alist))
      (while headers
	(let ((beg (elt (cdar headers) 2)))
	  (if (> beg (point)) 
	      (setq headers (cdr headers))
	    (setq res (list beg (rfcinfo-drop-final-dot (caar headers)))
		  headers nil))))
      (let* ((lines (count-lines (car res) (point)))
	     (ref (if (> lines 2)	; use lines only if big enough
		      (format "rfc%d-%s+%d" rfc (cadr res) lines)
		    (format "rfc%d-%s" rfc (cadr res)))))
	(kill-new ref)
	(message "%s (saved to kill ring)" ref)))))

(defun rfcinfo-goto-ref (ref)
  "same as rfcinfo-do-goto, using rfcview-local-heading-alist"
  (interactive)
  (let ((sec (cadddr ref))
	(res nil))
    (setq headers rfcview-local-heading-alist)
    (while headers
      (let ((thissec (caar headers)))
	(if (not (or (string= thissec sec)
		     ;; our sec shouldn't have a final dot,
		     ;; thissec may well have one
		     (string= thissec (concat sec "."))))
	    (setq headers (cdr headers))
	  (setq res (elt (cdar headers) 2)
		headers nil))));(elt (cadar headers) 2))))
    (if res
	(goto-char res)
      (error "Unfound section %s" sec))))

;; refs and secs, use rfcview header detection
;;
;; 1. correct them to not miss any (nor catch other things)
;; 2. allow absence of final dot
;; 3. 

;; TODO

;; search selected if non number in C-c r?

;; display new RFCs when refreshing?

;;; TODO current problems

;; function deps

;; sets mark two times while loading?

;; download part should go in rfcview?

;; clean up xml import code
;; +l in ref counts visible lines in rfcview mode...

;; errata for unknown, browser displays whole list of errata

;; errata should read from minibuffer if not found at point
;;        should use current rfc from rfcview! (or use I, e ?)
;;   if no arg && rfcinfo-mode, use this one
;;   if arg && rfcinfo-mode, ask
;;   if no arg && no rfcinfo-mode, use point, ask if none
;;   if arg && no rfcinfo-mode, use point, ask with default

;; rfc1035-5 currently fails (item number 5 found)
;; same rfc2205-1
;;   -> probably only applies to level 1 headings

;; scroll useless (except if window too small)

;; 3-lines status: show numbers of obsolating/updating rfcs?

;; feature: lookup also by bcp xx, std xx 

;;; rfcview problems

;; rfcview in debian emacs-goodies-el (but 0.12 instead of 0.13)

;; bug rfc1035-3.4.2, 25 (SMTP) (section 3.4.2, taken as heading)
;;  -> check final dot?
;;  -> check previous empty line?

;; misses headings not a column 0 (eg rfc2205)
;;  -> allow blanks for headings at level 2 and more?

;; misses toc in rfc2206 (no end dot in toc entries)

;; headings menu poorly cut when large
;; (do it by sections/subsections?)

;; does not detect multiple references (2205 -> [RSVP93, RFC 1633])
;; rfcview-use-view-mode-p -flag?

;; in speedbar, sections are in reverse order? (rfc1035-3)
;;   speedbar does its own heading parsing?
;;   or uses imenu???

;; see shrink-window-if-larger-than-buffer?

;; init code rfcinfo-load should rather be an autoload or something like that?

;;; rfcview toadds

;; next/previous heading
