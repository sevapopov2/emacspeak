;; emacspeak-edb.el --- Speech interface for the EDB package

;; Copyright  (C)  2005  Sergei V. Fleytin <fleytin@mail.ru>

;; Version: 0.1
;; Keywords: emacspeak, database speech interface
;; Author: Sergei V. Fleytin <fleytin@mail.ru>

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA.

;; Commentary: 

;; This software is my attempt to provide speech support for EDB
;; package. EDB, is another database program for Gnu Emacs. It has
;; much more features than traditional forms-mode. Currently, EDB is
;; in a very early stage of development and many of it's features are
;; not yet fully implemented but, in my opinion, it is already very
;; useful. So I decided to write a speech interface for it. Not all
;; features of EDB are supported in this release, But I will try to
;; fix it in future versions.

;; Code:

(require 'emacspeak-preamble)

;; First, let's fix some keybindings.

(declaim (special database-view-mode-map
		  database-edit-mode-map
		  emacspeak-prefix))
(define-key database-view-mode-map emacspeak-prefix 'emacspeak-prefix-command)
(define-key database-edit-mode-map emacspeak-prefix 'emacspeak-prefix-command)
(define-key database-edit-mode-map [home] 'db-beginning-of-line-or-field)
(define-key database-edit-mode-map [end] 'db-end-of-line-or-field)
(define-key database-edit-mode-map [down] 'db-next-line-or-field)
(define-key database-edit-mode-map [up] 'db-previous-line-or-field)
(define-key database-edit-mode-map [right] 'db-forward-char)
(define-key database-edit-mode-map [left] 'db-backward-char)

;; Now, advising some functions.

(defadvice db-find-file (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'open-object)))

(defadvice db-first-field (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (emacspeak-speak-line)))

(defadvice db-next-field (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (emacspeak-speak-line)))

(defadvice db-previous-field (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (emacspeak-speak-line)))

(defadvice db-last-field (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (emacspeak-speak-line)))

(defadvice db-view-mode (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'select-object)))

(defadvice db-next-record (after emacspeak pre act comp)
  "Provide auditory feedback.
  Play auditory icon if possible."
  (when (interactive-p)
    (dtk-stop)
    (emacspeak-auditory-icon 'scroll)
    (if (string-match "Database.*Summary" mode-name)
	(emacspeak-speak-next-window)
    (emacspeak-speak-current-window))))

(defadvice db-previous-record (after emacspeak pre act comp)
  "Provide auditory feedback.
  Play auditory icon if possible."
  (when (interactive-p)
    (dtk-stop)
    (emacspeak-auditory-icon 'scroll)
    (if (string-match "Database.*Summary" mode-name)
	(emacspeak-speak-next-window)
    (emacspeak-speak-current-window))))

(defadvice db-next-line-or-field (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (emacspeak-speak-line)))

(defadvice db-previous-line-or-field (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (emacspeak-speak-line)))

(defadvice db-first-record (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (emacspeak-speak-line)))

(defadvice db-last-record (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (emacspeak-speak-line)))

(defadvice db-summary (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (emacspeak-speak-mode-line)))

(defadvice dbs-view (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (emacspeak-speak-line)))

(defadvice dbs-exit (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'close-object)
    (emacspeak-speak-mode-line)))

(defadvice db-add-record (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (emacspeak-speak-line)))

(defadvice db-quit (after emacspeak pre act)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'close-object)))

(defadvice db-exit (after emacspeak pre act)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'close-object)))

(defadvice db-save-database (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'save-object)))

(defadvice db-mark-record (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'mark-object)))

(defadvice db-isearch-forward (before emacspeak pre act comp)
  "Provide auditory feedback.
  Pause ongoing speech first."
  (when (interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (dtk-pause)))

(defadvice db-isearch-backward (before emacspeak pre act comp)
  "Provide auditory feedback.
  Pause ongoing speech first."
  (when (interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (dtk-pause)))

(defadvice db-search-field (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'search-hit)
    (emacspeak-speak-line)))

(defadvice db-backward-delete-char (around emacspeak pre act)
  "Speak character you're deleting."
  (cond
   ((interactive-p )
    (dtk-tone 500 30 'force)
    (emacspeak-speak-this-char (preceding-char ))
    ad-do-it)
   (t ad-do-it))
  ad-return-value)

(defadvice db-jump-to-record (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (emacspeak-speak-line)))

(defadvice db-next-screen-or-record (after emacspeak pre act comp)
  "Try to speak a new screen or record."
  (when (interactive-p)
    (dtk-stop)
    (emacspeak-auditory-icon 'scroll)
    (if (string-match "Database.*Summary" mode-name)
	(emacspeak-speak-next-window)
    (emacspeak-speak-current-window))))

(defadvice db-previous-screen-or-record (after emacspeak pre act comp)
  "Try to speak a new screen or record."
  (when (interactive-p)
    (dtk-stop)
    (emacspeak-auditory-icon 'scroll)
    (if (string-match "Database.*Summary" mode-name)
	(emacspeak-speak-next-window)
    (emacspeak-speak-current-window))))

(defadvice db-beginning-of-line-or-field (after emacspeak pre act)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'select-object)))

(defadvice db-end-of-line-or-field (after emacspeak pre act)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'select-object)))


(defadvice db-copy-record (after emacspeak pre act)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'open-object)))

(defadvice db-forward-char (around emacspeak pre act comp)
  "Speak character moved to. Produce an auditory icon if we can not move."
  (let ((prevpos (point)))
    ad-do-it
    (if (interactive-p)
	(if (= (point) prevpos)
	    (emacspeak-auditory-icon 'warn-user)
	  (and dtk-stop-immediately (dtk-stop))
	  (emacspeak-speak-char t)))
    ad-return-value))

(defadvice db-backward-char (around emacspeak pre act comp)
  "Speak character moved to. Produce an auditory icon if we can not move."
  (let ((prevpos (point)))
    ad-do-it
    (if (interactive-p)
	(if (= (point) prevpos)
	    (emacspeak-auditory-icon 'warn-user)
	  (and dtk-stop-immediately (dtk-stop))
	  (emacspeak-speak-char t)))
    ad-return-value))

(defadvice db-forward-word (after emacspeak pre act)
  "Speak the word you just moved to."
  (when (interactive-p)
    (skip-syntax-forward " ")
    (emacspeak-speak-word )))

(defadvice db-backward-word (after emacspeak pre act)
  "Speak the word you just moved to."
  (when (interactive-p) (emacspeak-speak-word )))

(provide 'emacspeak-edb)

;;; local variables:
;;; byte-compile-dynamic: t
;;; end: 

