;;; emacspeak-gnus.el --- Speech enable GNUS -- Fluent spoken access to usenet
;;; $Id: emacspeak-gnus.el,v 1.16 2001/05/04 16:44:55 raman Exp $
;;; $Author: raman $
;;; Description:  Emacspeak extension to speech enable Gnus
;;; Keywords: Emacspeak, Gnus, Advice, Spoken Output, News
;;{{{  LCD Archive entry:

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |raman@cs.cornell.edu
;;; A speech interface to Emacs |
;;; $Date: 2001/05/04 16:44:55 $ |
;;;  $Revision: 1.16 $ |
;;; Location undetermined
;;;

;;}}}
;;{{{  Copyright:
;;;Copyright (C) 1995 -- 2000, T. V. Raman
;;; Copyright (c) 1994, 1995 by Digital Equipment Corporation.
;;; All Rights Reserved.
;;;
;;; This file is not part of GNU Emacs, but the same permissions apply.
;;;
;;; GNU Emacs is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.
;;;
;;; GNU Emacs is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;}}}
(eval-when-compile (require 'cl))
(require 'gnus nil t)
(require 'gnus-util nil t)
(require 'gnus-sum nil t)
(require 'voice-lock)
(require 'emacspeak-keymap)
(require 'emacspeak-sounds)
(require 'emacspeak-speak)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{  Introduction:

;;; This module advices gnus to speak.

;;}}}
;;{{{  Customizations:


;;; Keybindings for summary mode:
(and (boundp 'gnus-summary-mode-map)
     (emacspeak-keymap-remove-emacspeak-edit-commands
      gnus-summary-mode-map))

(and (boundp 'gnus-article-mode-map)
     (emacspeak-keymap-remove-emacspeak-edit-commands
      gnus-article-mode-map))

(and (boundp 'gnus-group-mode-map)
     (emacspeak-keymap-remove-emacspeak-edit-commands
      gnus-group-mode-map))

;;}}}
;;{{{  helper functions

(defsubst emacspeak-gnus-summary-speak-subject ()
  (emacspeak-dtk-sync)
  (dtk-speak (gnus-summary-article-subject)))

;;}}}
;;{{{ Advise top-level gnus command
;;; emacs can hang if too many message sfly by as gnus starts
(defadvice gnus (around emacspeak pre act)
  "Temporarily deactivate advice on message"
  (let ((startup (not (gnus-alive-p))))
    (cond
     ((and startup (interactive-p))
      (dtk-speak  "Starting gnus")
      (let ((emacspeak-speak-messages nil))
	ad-do-it)
      (emacspeak-auditory-icon 'news)
      (message "Gnus is ready "))
     (t				; gnus alive or non-interactive call
      ad-do-it
      (when (interactive-p)
	(emacspeak-auditory-icon 'select-object)
	(emacspeak-speak-line))))))

;;}}}
;;{{{  starting up:

(defadvice gnus-group-post-news (after emacspeak pre act comp)
  "Provide auditory feedback"
  (when (interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (emacspeak-speak-line)))


(defadvice gnus-group-get-new-news (around emacspeak pre act)
  "Temporarily deactivate advice on message"
  (when (interactive-p)
    (dtk-speak  "Getting news"))
  (let ((emacspeak-speak-messages nil))
    ad-do-it)
  (when (interactive-p)
    (emacspeak-auditory-icon 'task-done)
    (message "Done ")))

(defadvice gnus-group-suspend (after emacspeak pre act)
  "Provide spoken feedback."
  (when (interactive-p)
    (emacspeak-speak-mode-line)))

;;}}}
;;{{{  Newsgroup selection

(defadvice gnus-group-select-group (after emacspeak pre act comp)
  "Read line after entering into group."
  (when (interactive-p)
    (emacspeak-speak-line)
    (emacspeak-auditory-icon 'select-object)))

(defadvice gnus-group-prev-group (around emacspeak pre act)
  "Speak the newsgroup line.
Produce an auditory icon if possible."
  (let ((saved-point (point)))
    (when (interactive-p)
      (emacspeak-auditory-icon 'select-object))
    ad-do-it
    (when (interactive-p)
      (if (= saved-point (point))
          (dtk-speak "No more newsgroups ")
        (emacspeak-speak-line)))))

(defadvice gnus-group-prev-unread-group (around emacspeak pre act)
  "Speak the newsgroup line.
Produce an auditory icon if possible."
  (let ((saved-point (point)))
    (when (interactive-p)
      (emacspeak-auditory-icon 'select-object))
    ad-do-it
    (when (interactive-p)
      (if (= saved-point (point))
	  (dtk-speak "No more newsgroups ")
	(emacspeak-speak-line)))))

(defadvice gnus-group-next-group (around emacspeak pre act)
  "Speak the newsgroup line.
Produce an auditory icon if possible."
  (let ((saved-point (point)))
    (when (interactive-p)
      (emacspeak-auditory-icon 'select-object))
    ad-do-it
    (when (interactive-p)
      (if (= saved-point (point))
	  (dtk-speak "No more newsgroups")
	(emacspeak-speak-line)))))

(defadvice gnus-group-next-unread-group (around emacspeak pre act)
  "Speak the newsgroup line.
Produce an auditory icon if possible."
  (let ((saved-point (point)))
    (when (interactive-p)
      (emacspeak-auditory-icon 'select-object))
    ad-do-it
    (when (interactive-p)
      (if (= saved-point (point))
	  (dtk-speak "No more newsgroups")
	(emacspeak-speak-line)))))

(defadvice gnus-group-best-unread-group (after emacspeak pre act comp)
  "Provide spoken feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (emacspeak-speak-line)))

(defadvice gnus-group-first-unread-group (after emacspeak pre act comp)
  "Provide spoken feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (emacspeak-speak-line)))

(defadvice gnus-group-jump-to-group (after emacspeak pre act comp)
  (when (interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (emacspeak-speak-line)))

(defadvice gnus-group-unsubscribe-current-group (after emacspeak pre act)
  "Produce an auditory icon indicating
this group is being deselected."
  (when (interactive-p)
    (emacspeak-auditory-icon 'deselect-object)
    (emacspeak-speak-line)))

;;}}}
;;{{{  summary mode

(defadvice gnus-summary-exit (after emacspeak pre act comp)
  "Speak the line in group buffer."
  (when (interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (emacspeak-speak-line)))

(defadvice gnus-summary-prev-subject  (around  emacspeak pre act)
  "Speak the article  line.
Produce an auditory icon if possible."
  (let ((saved-point (point)))
    ad-do-it
    (when (interactive-p)
      (if (= saved-point (point))
          (dtk-speak "No more articles ")
        (progn
          (emacspeak-auditory-icon 'select-object)
          (dtk-speak (gnus-summary-article-subject)))))))

(defadvice gnus-summary-next-subject  (around  emacspeak pre act)
  "Speak the article  line.
Produce an auditory icon if possible."
  (let ((saved-point (point)))
    ad-do-it
    (when (interactive-p)
      (if (= saved-point (point))
          (dtk-speak "No more articles ")
        (progn
          (emacspeak-auditory-icon 'select-object)
          (dtk-speak (gnus-summary-article-subject)))))))

(defadvice gnus-summary-prev-unread-subject  (around  emacspeak pre act)
  "Speak the article  line.
Produce an auditory icon if possible."
  (let ((saved-point (point)))
    ad-do-it
    (when (interactive-p)
      (if (= saved-point (point))
	  (dtk-speak "No more unread articles ")
	(progn
	  (emacspeak-auditory-icon 'select-object)
	  (dtk-speak (gnus-summary-article-subject)))))))

(defadvice gnus-summary-next-unread-subject  (around  emacspeak pre act)
  "Speak the article line.
Produce an auditory icon if possible."
  (let ((saved-point (point)))
    ad-do-it
    (when (interactive-p)
      (if (= saved-point (point))
	  (dtk-speak "No more articles ")
	(progn
	  (emacspeak-auditory-icon 'select-object)
	  (dtk-speak (gnus-summary-article-subject)))))))

(defadvice gnus-summary-goto-subject (around  emacspeak pre act)
  "Speak the article  line.
Produce an auditory icon if possible."
  (let ((saved-point (point)))
    ad-do-it
    (when (interactive-p)
      (if (= saved-point (point))
	  (dtk-speak "No more articles ")
	(progn
	  (emacspeak-auditory-icon 'select-object)
	  (dtk-speak (gnus-summary-article-subject)))))))

(defadvice gnus-summary-catchup-and-exit (after emacspeak pre act)
  "Speak the newsgroup line.
Produce an auditory icon indicating
the previous group was closed."
  (when (interactive-p)
    (emacspeak-auditory-icon 'close-object)
    (emacspeak-speak-line)))

(defadvice gnus-summary-clear-mark-forward (after emacspeak pre act comp)
  (when (interactive-p)
    (emacspeak-auditory-icon 'deselect-object)
    (emacspeak-speak-line)))

(defadvice gnus-summary-mark-as-unread-forward (after emacspeak pre act)
  "Speak the line.
Produce an auditory icon if possible."
  (when (interactive-p)
    (emacspeak-auditory-icon 'mark-object)
    (emacspeak-gnus-summary-speak-subject)))

(defadvice gnus-summary-mark-as-read-forward (after emacspeak pre act)
  "Speak the line.
Produce an auditory icon if possible."
  (when (interactive-p)
    (emacspeak-auditory-icon'mark-object)
    (emacspeak-gnus-summary-speak-subject)))

(defadvice gnus-summary-mark-as-unread-backward (after emacspeak pre act)
  "Speak the line.
Produce an auditory icon if possible."
  (when (interactive-p)
    (emacspeak-auditory-icon 'mark-object)
    (emacspeak-gnus-summary-speak-subject)))

(defadvice gnus-summary-mark-as-read-backward (after emacspeak pre act)
  "Speak the line.
Produce an auditory icon if possible."
  (when (interactive-p)
    (emacspeak-auditory-icon 'mark-object)
    (emacspeak-gnus-summary-speak-subject)))

(defadvice gnus-summary-kill-same-subject-and-select (after emacspeak pre act)
  "Speak the subject and speak the first screenful.
Produce an auditory icon
indicating the article is being opened."
  (when (interactive-p)
    (emacspeak-gnus-summary-speak-subject)))

(defadvice gnus-summary-kill-same-subject (after emacspeak pre act)
  "Speak the line.
Produce an auditory icon if possible."
  (when (interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (emacspeak-gnus-summary-speak-subject)))

(defadvice gnus-summary-next-thread (after emacspeak pre act)
  "Speak the line.
Produce an auditory icon if possible."
  (when (interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (emacspeak-gnus-summary-speak-subject)))

(defadvice gnus-summary-prev-thread (after emacspeak pre act)
  "Speak the line.
Produce an auditory icon if possible."
  (when (interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (emacspeak-gnus-summary-speak-subject)))

(defadvice gnus-summary-up-thread (after emacspeak pre act)
  "Speak the line.
Produce an auditory icon if possible."
  (when (interactive-p)
    (emacspeak-auditory-icon'select-object)
    (emacspeak-gnus-summary-speak-subject)))

(defadvice gnus-summary-down-thread (after emacspeak pre act)
  "Speak the line.
Produce an auditory icon if possible."
  (when (interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (emacspeak-gnus-summary-speak-subject)))

(defadvice gnus-summary-kill-thread (after emacspeak pre act)
  "Speak the line.
Produce an auditory icon if possible."
  (when (interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (emacspeak-gnus-summary-speak-subject)))

;;}}}
;;{{{  Article reading

;;; helper function:

(defvar emacspeak-gnus-large-article 30
  "*Articles having more than
emacspeak-gnus-large-article lines will be considered to be a large article.
A large article is not spoken all at once;
instead you hear only the first screenful.")


(defun emacspeak-gnus-speak-article-body ()
  (declare (special emacspeak-gnus-large-article
                    voice-lock-mode dtk-punctuation-mode))
  (save-excursion
    (set-buffer  "*Article*")
    (goto-char (point-min))
    (setq dtk-punctuation-mode "some")
    (voice-lock-mode 1)
    (emacspeak-dtk-sync)
    (cond
     ((< (count-lines (point-min) (point-max))
         emacspeak-gnus-large-article)
      (emacspeak-speak-buffer))
     (t (emacspeak-auditory-icon 'large-movement)
        (let ((start (point)))
          (move-to-window-line -1)
          (end-of-line)
          (emacspeak-speak-region start (point)))))))

(defadvice gnus-article-describe-key-briefly (around emacspeak pre act comp)
  "Speak what you displayed"
  (cond
   ((interactive-p)
    (let ((emacspeak-advice-advice-princ t))
      ad-do-it))
   (t ad-do-it)))

(defadvice gnus-article-next-button (after emacspeak pre act comp)
  "Provide auditory feedback"
  (when (interactive-p)
    (let ((end (next-single-property-change
                (point) 'gnus-callback)))
      (emacspeak-auditory-icon 'large-movement)
      (message (buffer-substring
                (point)end)))))

(defadvice gnus-article-press-button (before emacspeak pre act comp)
  "Provide auditory feedback"
  (when (interactive-p)
    (emacspeak-auditory-icon 'button)))

(defadvice gnus-article-edit-exit (after emacspeak pre act comp)
  (when (interactive-p)
    (emacspeak-auditory-icon 'close-object)
    (emacspeak-speak-line)))

(defadvice gnus-article-edit-done (after emacspeak pre act comp)
  (when (interactive-p)
    (emacspeak-auditory-icon 'close-object)
    (emacspeak-speak-line)))

;;}}}
(provide 'emacspeak-gnus)
;;{{{  end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}
