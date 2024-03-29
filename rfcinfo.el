;;; rfcinfo.el --- Status information and navigation in RFCs -*- lexical-binding: t; -*-
;; Copyright (C) 2005-2024 Christophe Deleuze

;; AUthor: Christophe Deleuze <christophe.deleuze@free.fr>
;; Created: Feb 2005
;; Version: 
;; URL: https://github.com/cdeleuze/rfcinfo.el
;; Package-Requires: (cl-lib dash)

;; This file is NOT part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3 of the License,
;; or (at your option) any later version.

;; This file is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;; more details.

;; You should have received a copy of the GNU General Public License along
;; with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; RFCs are numbered documents published by the IETF, including
;; Internet standards.  These are of diffent categories including
;; standard, informational etc.  Since once published, an RFC is never
;; modified, each RFC has an evolving "status" indicating it's been
;; obsoleted or updated and if so by what other RFCs.
;;
;; A few "sub-series" are also defined: STDnn (nn being a number) has
;; a title and maps to one or a few RFCs.  BCPnn and FYInn map to a
;; single RFC.  The mappings are subject to change (although the FYI
;; subseries has been concluded and will not change any more, see
;; RFC6360).
;;
;; This file provides functions for displaying and browsing RFC's
;; status information as well as downloading them and jumping to a
;; precise location.  Document status can also be queried from
;; sub-series numbers.

;; We call "docid string" a string identifying an RFC or a sub-series
;; document.  It has the following structure:
;;
;; 1 - a three letter prefix identifying the subseries
;; 2 - a number
;;
;; Those can be directly concatenated or separated by a single space
;; or dash '-', or column ':'.  The prefix is optional, defaulting to
;; "RFC".
;;
;; Thus "RFC1034", "1035", "STD-3", "BCP 33", "FYI6", "rfc:9293" are
;; docid strings.
;;
;; A "loc string" is used to identify a precise point in an RFC
;; document.  It is made of a section number and a optional line
;; offset, separated by a '+'.
;; 
;; For example loc strings "4.1.2" and "4.1.2+42" represent section
;; 4.1.2 and a point 42 lines below the header of section 4.1.2.
;;
;; A docid string for an RFC (not a sub-series) can also contain a
;; loc string, separated from the main part with a dash '-'.
;;
;; Examples: "RFC1034-4.1.2", "1034-4.1.2", "RFC-1034-4.1.2+42"
;;
;; If rfcview-mode or irfc-mode is used to view RFCs, a few keys are
;; also added to their key map so that rfcinfo functions can easily be
;; called from the buffer displaying the RFC.  However, rfcinfo does
;; not require these modes to be loaded or available.

;; rfcview can be downloaded from
;; http://www.loveshack.ukfsn.org/emacs/rfcview.el
;;
;; irfc can be downloaded from
;; http://www.emacswiki.org/emacs/download/irfc.el
;;
;; Information about RFCs and sub-series is imported from an official
;; rfc-index.xml file from IETF RFC repository (configured as
;; `rfcinfo-remote-repository') which can be copied locally to
;; `rfcinfo-index-xml-file'.  This can be done manually or with
;; function `rfcinfo-refresh'.  The information is imported into Lisp
;; vectors that are then used to answer information queries.  Since
;; the import can be quite long (typically much more than a few
;; seconds) the vectors are saved as `rfcinfo-dbfile'.  Import can be
;; asynchronous.

;;; List of provided commands

;; rfcinfo-show

;;    get a docid (ignoring loc part if any), show title, authors,
;;    date and current status (category, list of dependencies (list of
;;    obsoleted/updated and obsoleting/updating)).  Arrow keys allow
;;    to navigate through dependencies.

;;    If the read string is FYI, BCP or STD, display the sub-series
;;    list.  If it's some other string, perform a string search in RFC
;;    titles.

;; rfcinfo-open

;;    get a RFC docid, open the RFC and position point on the given
;;    location.  The rfc status (category and update/obsolete stuff)
;;    is shown in the echo area.
;;
;;    The RFC is either read from a local cache directory or
;;    downloaded with ange-ftp.

;; rfcinfo-goto

;;    get a loc and move point to that location in current RFC.

;; If point is on a docid string, the above three functions use that
;; as argument, otherwise it is read from the minibuffer.  With a
;; prefix argument, always read from the minibuffer, proposing
;; possible argument at point as default.

;; rfcinfo-refresh

;;    download (fresh) xml file and import.  This should be invoked
;;    regularly to keep available information up to date.

;; rfcinfo-load: loads the content of rfcinfo-dbfile in memory.
;; rfcinfo-import: imports from xml file, saves to rfcinfo-dbfile and
;; loads it.

;;; Installation

;; Should work on all versions of (x)Emacs.
;; Please let me know if this is not the case.
;;
;; Copy this file to some directory and add the following to your
;; .emacs file:

;; (add-to-list 'load-path <the chosen directory>)
;; (global-set-key "\C-cr" 'rfcinfo-show)
;; (global-set-key "\C-cR" 'rfcinfo-open)
;; 
;; (autoload 'rfcinfo-show "rfcinfo" nil t)
;; (autoload 'rfcinfo-open "rfcinfo" nil t)
;; (autoload 'rfcinfo-refresh "rfcinfo" nil t)
;;
;; On first run, it will propose to create directory 'rfcinfo-dir',
;; download and create database (see the following variables).

;;; Code:
(require 'cl-lib)
(require 'dash)
;; Global Variables:

;; /-:anonymous@ftp.ripe.net:/rfc/ is apparently not updated since march 2020
;; before emacs 26, use /anonymous... (without the -:)

;; warning: ange-ftp does not work well with tnftp, which is now the
;; default ftp client on debian.  Make sure to use ie inetutils-ftp.

(defvar rfcinfo-remote-repository "/ftp:anonymous@ftp.rfc-editor.org:/in-notes/"
  "FTP repository where RFCs and index will be downloaded from.

The name is an ange-ftp directory.  You may have to set/customize
ange-ftp-try-passive-mode.")

(defvar rfcinfo-dir "~/.cache/rfc/" "RFC local cache directory.
Directory where to store official XML file and `rfcinfo-dbfile'.
If `rfcinfo-cache-flag' is non nil, downloaded RFCs will also be
stored there for offline access.")

(defvar rfcinfo-cache-flag t
  "Non-nil means store a copy of each downloaded RFC in `rfcinfo-dir'.")

(defvar rfcinfo-index-xml-file (concat rfcinfo-dir "rfc-index.xml")
  "The local (path)name of the rfc-index.xml file.")

(defvar rfcinfo-dbfile (concat rfcinfo-dir "rfcinfo.db")
  "Pathname of the file where rfc information will be stored.")

(defvar rfcinfo-abstracts-file (concat rfcinfo-dir "rfcinfo.abstracts")
  "Pathname of the file containing all RFCs abstracts.")

(defvar rfcinfo-errata-url-prefix
  "http://www.rfc-editor.org/errata_search.php?rfc="
  "Prefix of the errata URL, for RFCs that have one.")

(defvar rfcinfo-async-import nil
  "Non-nil means DB import from xml file will be asynchronous.")

;; no defvar?
(defvar rfcinfo-status nil "array of rfc status")
(defvar rfcinfo-window nil)
(defconst rfcinfo-buffer "*RFC info*")

(defvar rfcinfo-doing-init nil
  "Avoid computing import summary.  Set by rfcinfo-init.")

;;; A general purpose function

(defun rfcinfo-string-n-lines (n s)
  "Take first N lines of string S."
  (let ((lines (split-string s "\n")))
    (apply 'concat (mapcar (lambda (s) (concat s "\n")) ;
		    (-take n lines)))))

;;; First the user main entry points

(defun rfcinfo-show (ask)
  "Display RFC information.
Docid is taken at point, from buffer name, or asked.  If ASK is
non nil, always ask (found docid proposed as default)."
  (interactive "P")
  (let ((id (rfcinfo-read-docid "Info for " ask)))
    (rfcinfo-do-show id nil)))

(defun rfcinfo-open (ask)
  "Open RFC and position point to section number.
The RFC is either loaded from `rfcinfo-dir' or downloaded
from `rfcinfo-remote-repository."
  (interactive "P")
  (rfcinfo-do-open (rfcinfo-read-docid "View " ask 'non-local t)))

(defun rfcinfo-goto (ask)
  "Goto given section number in current RFC."
  (interactive "P")
  (rfcinfo-goto-loc (rfcinfo-read-loc ask)))

;;; Reading and printing docids

;; A docid is a cons cell that identifies either:
;; - an RFC: the car is the rfc number, as in (2205)
;;
;; - a sub-series document: the car is the symbol for the sub-series
;;   (std, bcp or fyi), the cdr is the number, as in (std . 3) or (bcp
;;   . 33).
;;
;; In the first case, the cdr is either nil or a 'loc' describing
;; location in the document.
;;
;; A loc is also a cons cell, whose car is a string denoting a section
;; number/letter and the (optional) cdr is a line offset after this
;; section header.  Thus (1034 . ("4.1.1" . 42)) which can also be
;; noted (1034 "4.1.1" . 42) denotes line 42 after section header
;; 4.1.1 of RFC 1034.
;;

;; These regexps have been built with the help of re-builder

;; TODO: should not match TA as "A"!
(defconst rfcinfo-re-loc
  "\\(\\([[:digit:]]+\\|A\\|B\\|C\\|D\\|E\\|F\\)\\(\\.[[:digit:]]+\\)*\\)\\(\\+\\([[:digit:]]+\\)\\)?"
  "Regexp matching a loc string.

Sub matches:
 1 - full section number (eg 1.2.3)
 2 - section number (or appendix letter, A-F)
 5 - line offset")

(defconst rfcinfo-re-subseries
  "\\(STD\\|std\\|BCP\\|bcp\\|FYI\\|fyi\\) ?-?\\([[:digit:]]+\\)"
  "Regexp matching sub-series docid string.")

;; We allow the docid to be enclosed in [], since navigating links in
;; rfcview-mode or irfc-mode puts point on one of those.
(defconst rfcinfo-re-docid (concat "\\[?\\(\\(RFC\\|rfc\\)?[- :]?\\([[:digit:]]+\\)\
\\(-\\(" rfcinfo-re-loc "\\)\\)?\\)\\]?\\|\\(" rfcinfo-re-subseries "\\)")
  "Regexp matching a docid string.

Sub-matches:
 1 - complete rfc docid
 2 - rfc prefix
 3 - rfc number
 5 - rfc suffix
 6 - rfc suffix section part
 9 - rfc suffix line offset part
 10 - complete subseries docid
 11 - subseries prefix
 12 - subseries number")

(defconst rfcinfo-regexp
  (concat "\\(\\([[:digit:]]+\\)\\|\\(" rfcinfo-re-subseries "\\)\\) -")
  "Regexp used in *rfc-info* buffer.

Matches an (RFC) number or a sub-series docid string.")

(defun rfcinfo-match-at-pos (re p)
  "Match regexp RE at position P.

Search regexp from beginning of line, check that point is in
matched text."
  (ignore-errors
    (beginning-of-line)
    (let ((cont t))
      (while cont
	(re-search-forward re (line-end-position))
	(if (and (>= p (match-beginning 0))
		 (<= p (match-end 0)))
	    (setq cont nil))))
    t))

(defun rfcinfo--parse-docid (&optional s)
  "Parse docid from string S, or from buffet at point."
  (if ;; look for docid
      (if s (and (string-match rfcinfo-re-docid s)
		 (= 0 (match-beginning 0))
		 (= (length s) (match-end 0)))
	(save-excursion (rfcinfo-match-at-pos rfcinfo-re-docid (point))))
      ;; found one!
      (if (match-string 1 s)
	  ;; matched an RFC docid
	  (let ((nb  (string-to-number (match-string 3 s)))
		(sec (match-string 6 s))
		(ofs (match-string 9 s)))
	    (cons nb (if sec (cons sec (if ofs (string-to-number ofs))))))
	;; matched a subseries docid
	(cons (intern (downcase (match-string 11 s))) (string-to-number (match-string 12 s))))))

(defun rfcinfo-read-docid (msg ask &optional non-local loc)
  "Get docid at point or prompt for it, displaying MSG.

If nothing is found at point, use RFC number from buffer name,
except if NON-LOCAL is non nil, else prompt for docid.
ASK non-nil means prompt for docid, proposing docid at point as
default.  LOC non-nil means include location part as well."
  (let ((def (or (rfcinfo--parse-docid)
		 (and (not non-local) (rfcinfo-buffer-holds-one)))))
    (if (or ask (not def))
	(let ((sdef (if def (rfcinfo-print-docid def loc))))
	  ;; ZZZ should directly use def if default selected
	  (let ((s (read-string
		    (if (null def) (concat msg "RFC number: ")
		      (concat msg "RFC number (default "
			      sdef "): "))
		    nil nil sdef)))
	    (or (rfcinfo--parse-docid s) s)))
      def)))

(defun rfcinfo-parse-loc (s)
  "If S is a loc string, build cons (section-string . line-number), else nil."
  (ignore-errors
      (let ((case-fold-search nil)) ; appendix letters must be uppercase
	(and (= 0 (string-match rfcinfo-re-loc s))
	     (= (length s) (match-end 0))
	     (let ((ofs (match-string 5 s)))
	       (cons (match-string 1 s) (if ofs (string-to-number ofs))))))))

(defun rfcinfo-loc-at-point ()
  "Get loc at point."
  (rfcinfo-parse-loc
   (save-excursion (and (rfcinfo-match-at-pos rfcinfo-re-loc (point))
			(match-string 0)))))

(defun rfcinfo-read-loc (ask)
  "Get loc at point or prompt for it."
  (let ((loc (rfcinfo-loc-at-point)))
    (if (or ask (not loc))
	;; prompt
	(let ((sloc ; as a string to be displayed in prompt
	       (if loc (concat (car loc) (if (cdr loc) (format "+%i" (cdr loc)) "")))))
	  ;; TODO: could directly use loc if default is selected
	  (rfcinfo-parse-loc (read-string
			      (if (null loc) "Location: "
				(concat "Location (default " sloc "): "))
			      nil nil sloc)))
      ;; do not prompt
      loc)))

(defun rfcinfo-buffer-holds-one ()
  "If current buffer holds an RFC return its docid."
  (ignore-errors
    (if (and
	 (string-prefix-p "rfc" (buffer-name))
	 (string= (substring (buffer-name) -4) ".txt"))
	(list (string-to-number (substring (buffer-name) 3 -4)))
      ;; irfc-mode renames the buffer if irfc-buffer-name-includes-title is non nil
      (if (and (eq major-mode 'irfc-mode)
	       irfc-buffer-name-includes-title)
	  (and (string-match ".*(rfc\\(.*\\).txt)" (buffer-name))
	       (list (string-to-number (match-string 1 (buffer-name)))))
	nil))))

;; TODO: better use locally/dynamic bound echo-flag to activate echo area?

;; we'll enable undo only after having written one time in the buffer,
;; so that undo can't take us back to an empty buffer.
(setq rfcinfo-first-done nil)

(defun rfcinfo-do-show (id echo)
  ""
  (when id
    (setq rfcinfo-current (if (and (listp id) (numberp (car id))) (car id) nil))
    (rfcinfo-display (rfcinfo-get-status id) echo)))

(defun rfcinfo-display (st echo)
  ""
  (unless (get-buffer rfcinfo-buffer)
    (setq rfcinfo-first-done nil))
  (get-buffer-create rfcinfo-buffer)
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
      ;; set point on the first to-follow thing
      (if (search-forward " -" nil t) (backward-char 2)
	;; for empty STD, nothing to follow, go on the ~
	(and (search-forward "~" nil t) (backward-char 1))))
    (rfcinfo-mode)
    (set-window-buffer rfcinfo-window rfcinfo-buffer)
    (fit-window-to-buffer)))

(defun rfcinfo-deps (l header)
  "Build the string of dependencies in list L, starting with
HEADER."
  (if (null l) ""
    (-reduce-from (lambda (acc n)
		     (cond
		      ((numberp n)
		       (let* ((rfc (aref rfcinfo-status n))
			      (tit (cadr (assoc 'title rfc)))
			      (ia  (cadr (assoc 'is-also rfc))))
			 (concat acc "\n"
				 (format "%6s %4d - %s"
					 (if ia (rfcinfo-print-docid ia) "    ") n tit))))
		       ((eq 'std (car n))
			(concat acc "\n" (format "      %4s - %s" (rfcinfo-print-docid n)
						 (cadr (assoc 'title (aref rfcinfo-std-status (cdr n)))))))
		       (t
			(concat acc "\n   " (rfcinfo-print-docid n)))))
		     header
		     l)))

(defun rfcinfo-get-status (id)
  (cond
   ((equal "STD" id) (rfcinfo-list-std))
   ((equal "BCP" id) (rfcinfo-list-sub 'bcp))
   ((equal "FYI" id) (rfcinfo-list-sub 'fyi))
   ((atom id) (rfcinfo-search-title-get id))
   ;; an RFC
   ((numberp (car id)) (rfcinfo-get-rfc-status (car id)))
   ;; a STD
   ((eq 'std (car id)) (rfcinfo-get-std-status (cdr id)))
   ;; other: BCP or FYI
   (t (let ((nb (rfcinfo-lookup-subseries id)))
	(if nb
	    (rfcinfo-get-rfc-status nb)
	  (error (format "Unknown %s" (rfcinfo-print-docid id))))))))
	


(defun rfcinfo-get-rfc-status (nb)
  (condition-case nil
      (with-temp-buffer
	(let ((rfc     (aref rfcinfo-status nb)))
	  (if (null rfc) (message (format "%d: unknown RFC" nb))
	    (let ((title   (cadr (assoc 'title rfc)))
		  (authors (cdr  (assoc 'authors rfc)))
		  (status  (cadr (assoc 'status rfc)))
		  (date    (cdr  (assoc 'date rfc)))
		  (isalso  (cadr  (assoc 'is-also rfc))) ;; we assume only one elt in list
		  (upd     (cdr  (assoc 'updates rfc)))
		  (obs     (cdr  (assoc 'obsoletes rfc)))
		  (upd-by  (cdr  (assoc 'updated-by rfc)))
		  (obs-by  (cdr  (assoc 'obsoleted-by rfc)))
		  (errata  (assoc 'errata rfc)))
	      (insert (format "%s%d -%c %s\n%S %S %i %s\n%s\n%s%s%s%s"
			      (if isalso (concat (rfcinfo-print-docid isalso) " ") "")
			      nb
			      (if (rfcinfo-cached-p nb) ?+ ?-)
			      title status
			      (car date) (cadr date)
			      (if errata
				  (concat (propertize "Errata"
						      'mouse-face 'highlight
						      'face '(:foreground "red")
						      'help-echo "Show errata in web browser") " ")
				"")
			      (-reduce (lambda (acc s) (concat acc ", " s))  authors)
			      (rfcinfo-deps upd-by "\nupdated by")
			      (rfcinfo-deps obs-by "\nobsoleted by")
			      (rfcinfo-deps upd    "\nupdates")
			      (rfcinfo-deps obs    "\nobsoletes"))))
	    ;; apply properties to string
	    (rfcinfo-set-properties)
	    (buffer-substring (point-min) (point-max)))))
    ;; error if we aref out of the array
    (args-out-of-range (error "Unknown RFC number: %d" nb))))

(defun rfcinfo-get-std-status (nb)
  (condition-case nil
      (with-temp-buffer
	(let ((std     (aref rfcinfo-std-status nb)))
	  (if (null std) (message (format "%d: unknown STD" nb))
	    (let ((title   (cadr (assoc 'title std)))
		  (isalso  (cdr  (assoc 'is-also std))))
	      (insert (format "STD%d ~ %s\n%s"
			      nb
			      title
			      (rfcinfo-deps isalso "\ncontents:"))))
	    ;; apply properties to string
	    (rfcinfo-set-properties)
	    (buffer-substring (point-min) (point-max)))))
    ;; error if we aref out of the array
    (args-out-of-range (error "Unknown STD number: %d" nb))))

;;; Download and display RFCs

(defun rfcinfo-do-open (id)
  (let* ((rfc (concat "rfc" (number-to-string (car id)) ".txt"))
	 (localname (concat rfcinfo-dir rfc)))
    (if (file-exists-p localname) (rfcinfo-view localname (cdr id))
      (if (or (> (car id) 99)
	      (yes-or-no-p (format "Really download rfc %i " (car id))))
	  (progn
	    (message "Not in cache. Attempting Download...")
	    (rfcinfo-view (concat rfcinfo-remote-repository rfc) (cdr id))
	    (if rfcinfo-cache-flag (progn
				     (write-file localname)
				     (chmod localname #o444))))))
    (rfcinfo-do-show id t)))


(defun rfcinfo-view (name loc)
  (view-file-other-window name)
  (if loc (rfcinfo-goto-loc loc)))

(defun rfcinfo-cached-p (nb)
  "Does a local cached copy for RFC NB exists?"
  (file-exists-p (concat rfcinfo-dir "rfc" (number-to-string nb) ".txt")))


;;; Handle locations

(defun rfcinfo--normalize-header (s)
  "Remove trailing dot and prefix word, if any.

toc from irfc includes entries like `Appendix A.' (should just be `A')"
  (let ((s (if (eq (aref s (1- (length s))) ?.)
	       (substring s 0 (1- (length s)))
	     s)))
    (string-match "\\(.+ \\)?\\(.+\\)\\.?" s)
    (match-string 2 s)))

(defun rfcinfo--toc ()
  "Get a table of contents: (title . offset) in reverse order."
  (cond
   ((eq major-mode 'rfcview-mode)
    (reverse
     (mapcar (lambda (e) (cons (rfcinfo--normalize-header (elt (cdr e) 0)) (elt (cdr e) 2)))
	     (-filter
	      ;; filter out 'nil text entries' (index, author's addresses)
	      (lambda (e) (elt (cdr e) 0))
	      rfcview-local-heading-alist))))
   
   ((eq major-mode 'irfc-mode)
    (let ((alist nil))
      (maphash
       (lambda (k v)
	 (setq alist (cons (cons (rfcinfo--normalize-header k) v) alist)))
       irfc-heading-numbers-table)
      alist))

   (t (error "Not a known RFC major mode"))))

(defun rfcinfo-build-docid ()
  "Build a docid for current point in current RFC.
Uses toc info from the major mode.  docid is displayed and saved to
kill ring."
  (interactive)
  (let ((rfc (car-safe (rfcinfo-buffer-holds-one)))
	(res)
	(toc))
    (if (null rfc) (error "Not an rfc here!")
      (setq toc (rfcinfo--toc))
      (while toc
	(let ((beg (cdar toc)))
	  (if (> beg (point))
	      (setq toc (cdr toc))
	    (setq res (list beg (caar toc))
		  toc nil))))
      (let* ((lines (count-lines (car res) (point)))
	     (docid (if (> lines 2)	; use lines only if big enough
		      (format "rfc%d-%s+%d" rfc (cadr res) lines)
		    (format "rfc%d-%s" rfc (cadr res)))))
	(kill-new docid)
	(message "%s (saved to kill ring)" docid)))))

(defun rfcinfo-goto-loc (loc)
  "Goto LOC, using table of contents."
  (interactive)
  (let ((sec (car loc))
	(off (cdr loc))
	(res)
	(toc (rfcinfo--toc)))
    (while toc
      (let ((thissec (caar toc)))
	(if (string= thissec sec)
	  (setq res (cdar toc)
		toc nil))
	(setq toc (cdr toc))))
    (if res (progn
	      (goto-char res)
	      (forward-line off))
      (error "Unfound section %s" sec))))

;;; TODO undocumented user functions

(defun rfcinfo-bortzmeyer ()
  "Browse Bortzmeyer blog for RFC at point."
  (interactive)
  (let* ((id  (rfcinfo-read-docid "Browse Bortzmeyer blog for " nil))
	 (url (concat "http://www.bortzmeyer.org/"
		      (number-to-string (car id)) ".html")))
    (browse-url url)))

;; major mode for RFC info

(put 'rfcinfo-mode 'mode-class 'special)

(defvar rfcinfo-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [down]   'rfcinfo-next)
    (define-key map [up]     'rfcinfo-prev)
    (define-key map [tab]    'rfcinfo-nextsection)
    (define-key map [backtab]'rfcinfo-prevsection)
    (define-key map ">"      'rfcinfo-last)
    (define-key map [right]  'rfcinfo-follow)
    (define-key map [return] 'rfcinfo-viewfile)
    (define-key map [left]   'rfcinfo-back)
    (define-key map "q"      'rfcinfo-quit)
    (define-key map "I"      'rfcinfo-quit) ;; so that 'I' toggles display
    ;; TODO: this is bad for documentation
    (define-key map "o"      (lambda () (interactive) (rfcinfo-show t)))
    (define-key map [mouse-2] 'rfcinfo-click)
    (define-key map "e"      'rfcinfo-errata)
    (define-key map "b"      'rfcinfo-bortzmeyer)
    (define-key map "?"      'describe-mode)
    (define-key map " "      'rfcinfo-scroll-up)
    (define-key map [backspace] 'rfcinfo-scroll-down)
    (define-key map "N"      'rfcinfo-last-news)
    (define-key map "a"      'rfcinfo-abstract)
    (define-key map "+"      'rfcinfo-next-rfc)
    (define-key map "-"      'rfcinfo-prev-rfc)
    map)
  "Keymap for `rfcinfo-mode'")

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

(defun rfci--ifv (r p)
  "if with value passing."
  (if r (funcall p r)))

(defun rfcinfo-prev ()
  "Move cursor to previous dependency."
  (interactive)
  (rfci--ifv (save-excursion
	       (backward-word)
	       (and (search-backward-regexp rfcinfo-regexp nil t)
		    (match-end 1)))
	     'goto-char))

(defun rfcinfo-next ()
  "Move cursor to next dependency."
  (interactive)
  (rfci--ifv (save-excursion
	       (forward-word)
	       (and (search-forward-regexp rfcinfo-regexp nil t)
		    (match-end 1)))
	     'goto-char))

(defun rfcinfo-prevsection ()
  (interactive)
  (and (search-backward-regexp "^[^ \n[:digit:]]" nil t 2)
       (rfcinfo-next)))

(defun rfcinfo-nextsection ()
  (interactive)
  (and (search-forward-regexp "^[^ \n[:digit:]]" nil t 1)
       (rfcinfo-next)))

(defun rfcinfo-scroll-up ()
  (interactive)
  (scroll-up)
  (rfcinfo-next))

(defun rfcinfo-scroll-down ()
  (interactive)
  (scroll-down)
  (rfcinfo-next))

(defun rfcinfo-last ()
  (interactive)
  (goto-char (point-max))
  (rfcinfo-prev))

(defun rfcinfo-status-echo (ask)
  "Toggle display of RFC status in echo area."
  (interactive "P")
  (if (eq last-command #'rfcinfo-status-echo)
      (setq this-command nil)
    (rfcinfo-do-show (rfcinfo-read-docid "" ask) t)))

(defun rfcinfo-next-rfc ()
  (interactive)
  (if rfcinfo-current 
      (rfcinfo-do-show (list (1+ rfcinfo-current)) nil)
    (message "No current rfc")))

(defun rfcinfo-prev-rfc ()
  (interactive)
  (if rfcinfo-current 
      (rfcinfo-do-show (list (1- rfcinfo-current)) nil)
    (message "No current rfc")))

(defun rfcinfo-follow ()
  (interactive)
  (if (save-excursion
	(beginning-of-line)
	(= 1 (point)))
      ;; if we're on the first line goto std entry if there's one
      (let ((id (save-excursion
		  (beginning-of-line)
		  (rfcinfo--parse-docid))))
	(if (eq (car id) 'std) (rfcinfo-do-show id nil)
	  (message "This RFC is not part of the STD sub-series")))
    (rfcinfo-do-show (rfcinfo--parse-docid) nil)))

(defun rfcinfo-viewfile ()
  (interactive)
  (rfcinfo-do-open (list (car (rfcinfo--parse-docid)))))

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
  (select-window (get-mru-window)))

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
		;; unknown have no specific face
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
         help-echo ,(car (rassoc cat rfcinfo-status-symbol))))))
  (goto-char (point-min)))

(defun rfcinfo-errata (arg)
  "Browse to errata page of current RFC, if any."
  (interactive "P")
  (let ((nb (if arg
		(read-string "Errata for RFC: ")
	      (save-excursion
		(goto-char 0)
		(search-forward-regexp rfcinfo-regexp)
		(buffer-substring-no-properties (match-beginning 1) (match-end 1))))))
    (if (assoc 'errata (aref rfcinfo-status (string-to-number nb)))
	(browse-url (concat rfcinfo-errata-url-prefix nb))
      (message (format "RFC %s has no errata" nb)))))


(defun rfcinfo-click (event)
  "Click click!"
  (interactive "e")
  (let ((pos (posn-point (event-end event))))
    (goto-char pos)
    (if (string= (thing-at-point 'word) "Errata")
	(rfcinfo-errata nil)
      (rfcinfo-show nil))))

(defun rfcinfo-list-std ()
  (let* ((max (length rfcinfo-std-status))
	 (l (let ((i (1- max))
		  (l))
	      (while (> i 0)
		(if (assoc 'is-also (aref rfcinfo-std-status i))
		    (setq l (cons (cons 'std i) l)))
		(setq i (1- i)))
	      l)))
    (rfcinfo-list-nbs "List of STDs" l)))

(defun rfcinfo-list-sub (sub)
  (let* ((max (length rfcinfo-status))
	 (l (let ((i (1- max))
	       (l))
	   (while (> i 0)
	     (let ((also (cadr (assoc 'is-also (aref rfcinfo-status i)))))
	       (if (eq sub (car also))
		   ;; this list contains (rfc nb . subseries nb)
		   (setq l (cons (cons i (cdr also)) l)))
	       (setq i (1- i))))
	   ;; we sort on subseries nb, then remove it
	   (mapcar 'car (sort l (lambda (a b) (< (cdr a) (cdr b))))))))
    (rfcinfo-list-nbs 
     (format "List of %s sub-series" (upcase (symbol-name sub)))
     l)))

(defun rfcinfo-list-nbs (title nbs)
  "Build a string for TITLE and RFC numbers list NBS"
  (with-temp-buffer
    (insert
     (format "%s\n%s" title
	     (rfcinfo-deps nbs "")))
    (rfcinfo-set-properties)
    (buffer-substring (point-min) (point-max))))

;;; Searching the DB

(defun rfcinfo-search-any (field word)
  (cl-loop for i from 0 to (1- (length rfcinfo-status))
	   if (let ((fi (assq field (aref rfcinfo-status i))))
		(and fi (string-match word (cadr fi))))
	   collect i))

;; case-fold-search

(defun rfcinfo-search-title-get (word)
  (let ((l (rfcinfo-search-any 'title word)))
    (rfcinfo-list-nbs
     (format "Search results for \"%s\" in title (%i)" word (length l))
     l)))

;;; Importing, loading, and saving

;; When importing a new version of `rfcinfo-index-xml-file' we will
;; display a change summary showing what has changed since last
;; import.  The summary will display lists of:
;;   - newly published RFCs
;;   - RFCs that have changed status
;;   - RFCs that have become obsolete
;;   - RFCs that have new updates

(defun rfcinfo-known-rfcs ()
  "Build a list of (nb . status) for currently known rfcs."
  (let ((nb (1- (length rfcinfo-status)))
	r)
    (while (> nb 0)
      (let ((rfc (aref rfcinfo-status nb)))
	(when rfc
	  (setq r (cons (cons nb (cadr (assoc 'status rfc))) r))))
      (setq nb (1- nb)))
    r))

(defun rfcinfo-changes (l1 l2)
  "L1 and L2 are (nb . status) lists.  Return cons of news (ie L2-L1) and
changes (nb l1status l2status)."
  (let ((new)
	(changes))
    (while (consp l2)
      (cond                            ; new ones
       ((null l1) (setq new (append (mapcar 'car (nreverse l2)) new) l2 nil))

       ((equal (car l1) (car l2))      ; no change
	(setq l1 (cdr l1)
	      l2 (cdr l2)))

       ((equal (caar l1) (caar l2))    ; status change
	(setq changes (cons (list (caar l1) (cdar l1) (cdar l2)) changes)
	      l1 (cdr l1)
	      l2 (cdr l2)))

       ((> (caar l1) (caar l2))        ; new one
	(setq new (cons (caar l2) new)
	      l2 (cdr l2)))

       ((< (caar l1) (caar l2))        ; should not happen!
	(message "Oops! Something strange happened in rfcinfo-changes!")
	(setq l1 (cdr l1)))))
    (cons (nreverse new) (nreverse changes))))

(defun rfcinfo-changes-string (l)
  "Return a string representing L, whose elements are (nb oldstatus newstatus)."
  (with-temp-buffer
    (insert (format "RFCs having changed status (%i)\n\n" (length l)))
    (mapc (lambda (e) (insert (format "%6s %4d - %s -> %s\n" "" (car e) (cadr e) (caddr e)))) l)
    (insert "\n")
    (rfcinfo-set-properties)
    (buffer-substring (point-min) (point-max))))

(defun rfcinfo-affected (news)
  "Compute lists of updated and obsoleted from list of new RFCs."
  (let (upd obs)
    (mapc (lambda (n) (let ((rfc (aref rfcinfo-status n)))
			(mapc (lambda (n) (add-to-list 'upd n))
			      (cdr (assoc 'updates rfc)))
			(mapc (lambda (n) (add-to-list 'obs n))
			      (cdr (assoc 'obsoletes rfc))))) news)
    (cons upd obs)))

(defun rfcinfo-import-summary (old new)
  "Return import summary as string, or nil."
  (let* ((diff (rfcinfo-changes old new))
	 (news (car diff))
	 (changes (cdr diff))
	 (affected (rfcinfo-affected news))
	 (stch (if changes (rfcinfo-changes-string changes) ""))
	 (stnw (if news
		   (rfcinfo-list-nbs (format "New RFCs (%i)" (length news))
				     news)
		 ""))
	 (stup (if (car affected)
		   (rfcinfo-list-nbs (format "\n\nNewly updated RFCs (%i)"
					     (length (car affected)))
				     (car affected))
		 ""))
	 (stob (if (cdr affected)
		   (rfcinfo-list-nbs (format "\n\nNewly obsoleted RFCs (%i)"
					     (length (cdr affected)))
				     (cdr affected))
		 "")))
    (if (or news changes) (concat stch stnw stob stup))))

;; return list of elements with given tag
(defun rfcinfo-filter-tag (tag l)
  (-filter (lambda (e) (eq (car-safe e) tag)) l))

(defun rfcinfo-filter-tags (tags l)
  (mapcar
       (lambda (l) (cons (car l) (cddr l))) ;; this removes the nil attribute
       (-filter (lambda (e) (member (car-safe e) tags)) l)))

(setq rfcinfo-status-symbol
      '(("INTERNET STANDARD" . standard)
	("DRAFT STANDARD"    . draft-std)
	("PROPOSED STANDARD" . proposed-std)
	("UNKNOWN"           . unknown)
	("BEST CURRENT PRACTICE" . bcp)
	("EXPERIMENTAL"      . experimental)
	("HISTORIC"          . historic)
	("INFORMATIONAL"     . informational)))

(defun rfcinfo-status-symbol (s)
  (cdr (assoc s rfcinfo-status-symbol)))

(defun rfcinfo-month-symbol (s)
  (let ((string-symbol
	 '(("January"  . jan) ("May"    . may) ("September" . sep)
	   ("February" . feb) ("June"   . jun) ("October"   . oct)
	   ("March"    . mar) ("July"   . jul) ("November"  . nov)
	   ("April"    . apr) ("August" . aug) ("December"  . dec))))
    (cdr (assoc s string-symbol))))

(defun rfcinfo-fold-docid (e)
  (let ((kind (intern (downcase (substring (caddr e) 0 3))))
	(nb (string-to-number (substring (caddr e) 3))))
    (if (eq kind 'rfc) nb
      (cons kind nb))))

(defun rfcinfo-print-docid (id &optional loc full)
  "Print ID.  LOC: include RFC location; FULL: include RFC prefix."
  (if (numberp (car id)) (concat
			  (if full "RFC" "") (number-to-string (car id))
			  (if (and loc (cadr id)) (format "-%s" (cadr id)) "")
			  (if (and loc (cddr id)) (format "+%i" (cddr id)) ""))
    (concat (cl-case (car id)
	      (std "STD")
	      (bcp "BCP")
	      (fyi "FYI")
	      (nic "NIC")
	      (ien "IEN")
	      (rtr "RTR"))
	    (format "%d" (cdr id)))))

(defun rfcinfo-lookup-subseries (docid)
  "Search the rfc which is-also DOCID.

Return nil if none"
  (let ((max (1- (length rfcinfo-status)))
	(i 1)
	(key (list docid))
	(cont t))
  (while (and (<= i max) cont)
    (let ((e (aref rfcinfo-status i)))
      (if e (setq cont (not (equal key (cdr (assoc 'is-also e))))))
      (setq i (1+ i))))
  (if cont nil (1- i))))

;; take an rfc element, fold inner elements
;; result has form (nb (title "title") (field field_values) ... )

(defun rfcinfo-fold-all (es)
  (let ((deps '(obsoletes updates obsoleted-by updated-by))

	;; get the number xxxx from a string of the form "RFCxxxx"
	(nb   '(lambda (s) (string-to-number (substring s 3))))

	(date '(lambda (e) (let ((month (rfcinfo-filter-tag 'month e))
				 (year  (rfcinfo-filter-tag 'year  e)))
			     (list (rfcinfo-month-symbol (caddar month))
				   (string-to-number (caddar year))))))
	)
    (-filter
     (lambda (e) (not (null e))) ;; empty list from non RFC dependencies
     (mapcar (lambda (el)
	       (if (consp el) ;; ???
		   (cond
		    ;; this RFC number
		    ((eq (car el) 'doc-id) (funcall nb (cadr el)))
		    
		    ;; one of the 4 dependency lists
		    ((member (car el) deps)
		     (let* ((ids (rfcinfo-filter-tag 'doc-id el))
			    (l (mapcar 'rfcinfo-fold-docid ids)))
		       (if (null l) nil
			 (cons (car el) l))))
		    
		    ((eq (car el) 'date)
		     (cons 'date (funcall date el)))
		    
		    ((eq (car el) 'author)
		     (cons 'author (cddar (rfcinfo-filter-tag 'name  el))))
		    
		    ((eq (car el) 'current-status)
		     (list 'status (rfcinfo-status-symbol (cadr el))))
		    
		    ((eq (car el) 'errata-url)
		     (list 'errata))

		    ((eq (car el) 'is-also)
		     (cons 'is-also (mapcar 'rfcinfo-fold-docid  (rfcinfo-filter-tag 'doc-id el))))
		    
		    (t el))
		 el))
	     es))))


(defun rfcinfo-do-import ()
  "Import `rfcinfo-index-xml-file', set arrays and save files."

  (let (;; the elements we need inside an rfc-entry
	(fields '(doc-id title author date is-also obsoletes updates
			 obsoleted-by updated-by current-status
			 errata-url abstract))
	;; replace the several author elements by one ('authors ...) list
	;; place abstract element as car of the list, followed by number and other elements
	(fold-authors #'(lambda (el)
			 (let ((wo-authors-abstract
				(nreverse (-reduce-from
					   (lambda (acc e) (if (or
								(eq (car-safe e) 'author)
								(eq (car-safe e) 'abstract))
							       acc (cons e acc)))
					   nil el)))
			       (authors (mapcar 'cadr (rfcinfo-filter-tag 'author el)))
			       (abstract (cdar (rfcinfo-filter-tag 'abstract el))))
			   (cons abstract (append wo-authors-abstract (list (cons 'authors authors)))))))

	;; *this* is long!
	(content (car (xml-parse-file rfcinfo-index-xml-file))))

    ;; first import STD info
    (let* ((full (rfcinfo-filter-tag 'std-entry content))
	   (f2 (mapcar (lambda (e) (rfcinfo-filter-tags '(doc-id title is-also) e)) full))
	   (stds (mapcar 'rfcinfo-fold-all f2))
	   (max (1+ (caar (last stds))))
	   (v (make-vector max nil))
	   )
      (cl-loop for e in stds do
	       (aset v (car e) (cdr e)))
      (setq rfcinfo-std-status v))
  
    ;; now import RFC info
    (let* (;; all rfc-entry elements
	   (full (rfcinfo-filter-tag 'rfc-entry content))

	   ;; filter relevant sub elements
	   (some (mapcar (lambda (e) (rfcinfo-filter-tags fields e)) full))

	   ;; turn each entry in a simple list of (attr-type attr-values)
	   (props (mapcar 'rfcinfo-fold-all some))

	   ;; merge 'author lists in a single 'authors list
	   (rfcs (mapcar fold-authors props))

	   (max  (1+ (cadar (last rfcs))))
	   (v    (make-vector max nil)))

      ;; build array, add 'abstract tag, create abstracts file
      ;; loop from 'cl (TODO: change to elisp basic while?)
      (with-temp-file rfcinfo-abstracts-file
	(cl-loop for e in rfcs do
	      ;; for debugging only
	      ;;(insert (format "%i\n" (cadr e)))

	      ;; insert the abstract in file, store text position in array
	      (let ((p (1- (point))))
		(if (car e) (progn ;; first paragraph
			      (insert (caddr (caar e)))
			      ;; some abstracts have several paragraphs
			      ;; following paragraphs have an ugly initial white space, we remove it
			      (mapc (lambda (e) (insert "\n\n" (substring (caddr e) 1))) (cdar e))
			      (insert "\n\n")))
		  (aset v (cadr e) (if (car e)
				       (cons (cons 'abstract (cons p (- (point) 3)))
					     (cddr e))
				     (cddr e))))))

      (setq rfcinfo-status v)

      ;; save info
      ;; rfcinfo-xml-mdtm should be bound! (by refresh or load)
      (with-temp-file rfcinfo-dbfile
	(insert (with-output-to-string (prin1 rfcinfo-xml-mdtm)))
	(insert (with-output-to-string (prin1 rfcinfo-status)))
	(insert (with-output-to-string (prin1 rfcinfo-std-status))))
      max)))

(defun rfcinfo-import ()
  (interactive)
  (message "Importing RFC info from XML file.  This may be loooong...")
  (let ((old (rfcinfo-known-rfcs))
	(max (rfcinfo-do-import)))
    ;; we don't want to compute the summary for a first time import
    (if rfcinfo-doing-init
	(message "Imported until RFC%i." (1- max))
      (let ((summary (rfcinfo-import-summary old (rfcinfo-known-rfcs))))
	(if summary
	    (progn
	      (rfcinfo-display summary nil)
	      (write-region nil nil (concat rfcinfo-dir ".news"))
	      (message "Done."))
	  (message "Done.  No new or changed RFCs." ))))))

(defun rfcinfo-async-import ()
  (interactive)
  (message "Asynchronously importing RFC info from XML file.  Will tell you when done...")
  (let ((old (rfcinfo-known-rfcs))
	(process (start-process "rfcinfo-import" "rfcinfo-import" "emacs" "-Q" "--batch" "-l" "~/Src/rfcinfo/rfcinfo.el" "-f" "rfcinfo-do-import")))
    (set-process-sentinel process
       `(lambda (p e)
	  (rfcinfo-load)
	  (let ((summary (rfcinfo-import-summary (quote ,old) (rfcinfo-known-rfcs))))
	     (if summary
		 (progn
		   (save-selected-window (rfcinfo-display summary nil)
					 (write-file (concat rfcinfo-dir ".news")))
		   (message "rfcinfo async import done."))
	       (message "rfcinfo async import done.  No new or changed RFCs." )))))))

(defun rfcinfo-refresh (arg)
  "Get mdtm for rfc-index.xml file, download and import it if it's newer.

File is downloaded from `rfcinfo-remote-repository'.
ARG forces download and import."
  (interactive "P")
  (let* ((xml-file (concat rfcinfo-remote-repository "rfc-index.xml"))
	 (mdtm (nth 5 (file-attributes xml-file))))
    (message "rfcinfo: remote xml-mdtm %s" mdtm)
    (if (and (null arg) (equal mdtm rfcinfo-xml-mdtm))
	(message "Remote rfc-index.xml hasn't changed. No need to refresh.")
      (with-temp-buffer
	(insert-file-contents (concat rfcinfo-remote-repository "rfc-index.xml"))
	(and (file-exists-p rfcinfo-index-xml-file)
	     (rename-file rfcinfo-index-xml-file (concat rfcinfo-index-xml-file ".prev") t))
	(write-file rfcinfo-index-xml-file))
      (setq rfcinfo-xml-mdtm mdtm)
      (if rfcinfo-async-import (rfcinfo-async-import)
	(rfcinfo-import)))))

(defun rfcinfo-load ()
  (interactive)
  (message "rfcinfo-load")
  (condition-case nil
      (with-temp-buffer
	(insert-file-contents rfcinfo-dbfile)
	(setq rfcinfo-xml-mdtm (read (current-buffer)))
	(message "rfcinfo: local xml-mdtm %s" rfcinfo-xml-mdtm)
	(setq rfcinfo-status (read (current-buffer)))
	(setq rfcinfo-std-status (read (current-buffer))))
    (error (rfcinfo-init))))

(defun rfcinfo-last-news ()
  "Display last news summary."
  (interactive)
  (let* ((file (concat rfcinfo-dir ".news"))
	 (mdtm (nth 5 (file-attributes file)))
	 (st
	  (with-temp-buffer
	    (insert-file-contents file)
	    (rfcinfo-set-properties)
	    (buffer-string))))
    (rfcinfo-display st nil)
    (message "Last news summary (at %s)." (current-time-string mdtm))))

(defun rfcinfo-abstract (arg)
  "Display RFC abstract, from xml file or in RFC itself if cached.

If RFC is cached but has no abstract, use the one provided in xml
file, if any.  If ARG, always display abstract from xml file."
  (interactive "P")
  (let ((docid (rfcinfo-read-docid "Abstract for " nil)))
    (if (numberp (car docid))
	(if (and (not arg) (rfcinfo-cached-p (car docid)))
	    (condition-case nil
		(progn
		  (rfcinfo-do-open docid)
		  (goto-char (point-min))
		  (re-search-forward "^Abstract")
		  (forward-line)
		  (recenter-top-bottom 1)
		  (message (format "Type 'q' to go back to rfc%i info window." (car docid))))
	      (error (rfcinfo-xml-abstract (car docid) "No abstract in RFC itself. ")))
	  (rfcinfo-xml-abstract (car docid))))))

(defun rfcinfo-xml-abstract (nb &optional msg)
  ;; go get abstract imported from xml file
  (let ((be (cdr (assoc 'abstract (aref rfcinfo-status nb)))))
    (if be
	(let* ((buffer-name (format "Abstract for RFC%i" nb))
	       (exists (get-buffer buffer-name))
	       (abuf (or exists (get-buffer-create buffer-name))))
	  (unless exists ;; may have been created before and still exist
	    (with-current-buffer abuf
	      (erase-buffer)
	      (insert-file-contents rfcinfo-abstracts-file nil (car be) (cdr be))
	      (fill-region (point-min) (point-max))))
	  (view-buffer-other-window abuf 'kill-buffer)
	  (message (concat (if msg msg "") "Type 'q' to go back to *RFC info*.")))
      (message (concat "No abstract in DB for RFC%i. " (if msg msg "")) nb))))

(defun rfcinfo--next-abstract (n)
  "Get rfc number and abstract position for first rfc after N that
has one."
  (let ((be))
    (while (not be)
      (setq n (1+ n)
	    be (cdr (assoc 'abstract (aref rfcinfo-status n)))))
    (cons n be)))
  
(defun rfcinfo-search-in-abstracts (keyword)
  "Search for keyword in abstracts"
  (interactive "sKeyword in abstract: ")
  (let ((buf (find-file-noselect rfcinfo-abstracts-file)))
    (set-buffer buf)
    ;; collect posns in rfcinfo-abstracts-file
    (let ((posns)
	  (rfcs nil)
	  ;; rfc600 is first one with abstract
	  (abs (rfcinfo--next-abstract 599)))

      ;; build list of posns, in increasing order
      (goto-char (point-max))
      (while (re-search-backward (concat "\\b" keyword "\\b") nil t)
	(setq posns (cons (point) posns)))

      ;; match posns to rfc numbers in rfcs
      (dolist (p posns)
	(while (> p (cddr abs))
	  (setq abs (rfcinfo--next-abstract (car abs))))
	(setq rfcs (cons (car abs) rfcs)))

      ;; display result
      (rfcinfo-display
       (rfcinfo-list-nbs
	(format "\"%s\" in abstract (%i)" keyword (length rfcs))
	(-distinct (reverse rfcs)))
       nil))))

;;; Initializations

(defun rfcinfo-init ()
  "To be called when running rfcinfo for the first time.  Called
from rfcinfo-load, when failing."
  (unless (file-directory-p rfcinfo-dir)
    (if (y-or-n-p (format "rfcinfo-init: create directory %s? " rfcinfo-dir))
	(make-directory rfcinfo-dir t)
      (error "Well, so I can't do my work.")))
  (if (y-or-n-p "Can't load from `rfcinfo-dbfile', should I initialize things for you? ")
      (progn
	(setq rfcinfo-xml-mdtm '(0 0))
	(let ((rfcinfo-doing-init t))
	  (rfcinfo-refresh nil)))
    ;; raise error to abort running an autoloaded function
    (error "Aborting.")))
  
  


(rfcinfo-load)			       ; Load vectors in memory

;; If and when rfcview is loaded, we want to add a few keys to its
;; mode map.  However, we don't want to force loading it.  First, it
;; may not be available, second if the user just browses RFC info
;; without displaying RFCs, it would be useless to load it.

;; Elisp reference manual (16.10) discourages use of with-eval-after-load
;; but I don't know how to do that without it...

(with-eval-after-load 'rfcview
  '(progn (define-key rfcview-mode-map "O" 'rfcinfo-open)
	  (define-key rfcview-mode-map "i" 'rfcinfo-status-echo)
	  (define-key rfcview-mode-map "I" 'rfcinfo-show)
	  (define-key rfcview-mode-map "B" 'rfcinfo-bortzmeyer)
	  (define-key rfcview-mode-map "D" 'rfcinfo-build-docid)
	  (define-key rfcview-mode-map "G" 'rfcinfo-goto)))

;; ...unless rfcview honors a rfcview-load-hook

;; same thing for irfc, but it does have a mode hook

(defun rfcinfo-change-irfc-map ()
  (define-key irfc-mode-map "O" 'rfcinfo-open)
  (define-key irfc-mode-map "i" 'rfcinfo-status-echo)
  (define-key irfc-mode-map "I" 'rfcinfo-show)
  (define-key irfc-mode-map "B" 'rfcinfo-bortzmeyer)
  (define-key irfc-mode-map "D" 'rfcinfo-build-docid)
  (define-key irfc-mode-map "\C-cg" 'rfcinfo-goto))

(if (boundp 'irfc-mode-map)
    (rfcinfo-change-irfc-map)
  (add-hook 'irfc-mode-hook 'rfcinfo-change-irfc-mode-map))

(provide 'rfcinfo)

;;; rfcinfo.el ends here
