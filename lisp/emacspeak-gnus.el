;;; emacspeak-gnus.el --- Speech enable GNUS -- Fluent spoken access to usenet
;;; $Id: emacspeak-gnus.el 9072 2014-04-16 15:27:01Z tv.raman.tv $
;;; $Author: tv.raman.tv $ 
;;; Description:  Emacspeak extension to speech enable Gnus
;;; Keywords: Emacspeak, Gnus, Advice, Spoken Output, News
;;{{{  LCD Archive entry: 

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |raman@cs.cornell.edu 
;;; A speech interface to Emacs |
;;; $Date: 2008-06-21 10:50:41 -0700 (Sat, 21 Jun 2008) $ |
;;;  $Revision: 4532 $ | 
;;; Location undetermined
;;;

;;}}}
;;{{{  Copyright:
;;;Copyright (C) 1995 -- 2011, T. V. Raman 
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

;;{{{  Introduction:

;;; Commentary

;;; This module advices gnus to speak. 
;;; Updating support in 2014 (Emacspeak is nearly 20 years old)

;;}}}
;;{{{ requires
(require 'cl)
(require 'emacspeak-preamble)
(require 'emacspeak-hide)
(require 'gnus)
(require 'gnus-art)
(require 'gnus-sum)
(require 'gm-nnir) ; for smart GMail search
;;}}}
;;{{{  Customizations:

(defgroup emacspeak-gnus nil
  "Emacspeak customizations for the Gnus News/Mail/RSS reader"
  :group 'emacspeak
  :group 'gnus
  :prefix "emacspeak-gnus-")

(defcustom emacspeak-gnus-punctuation-mode  'all
  "Pronunciation mode to use for gnus buffers."
  :type '(choice
          (const  :tag "Ignore" nil)
          (const  :tag "some" some)
          (const  :tag "all" all))
  :group 'emacspeak-gnus)

(defcustom  emacspeak-gnus-large-article 100
  "*Articles having more than
emacspeak-gnus-large-article lines will be considered to be a large article.
A large article is not spoken all at once;
instead you hear only the first screenful."
  :type 'integer
  :group 'emacspeak-gnus 
  )
;;; These customizations to gnus make it convenient to listen to news:
;;; You can read news mostly by using the four arrow keys.
;;; By default all article headers are hidden, so you hear the real news.
;;; You can expose some of the headers with "T" in summary mode.

;;; Keybindings 
(defun emacspeak-gnus-setup-keys ()
  "Setup Emacspeak keys."
  (declare (special gnus-summary-mode-map
                    gnus-group-mmode-map
                    gnus-article-mode-map))
  (define-key gnus-summary-mode-map "\C-t" 'gnus-summary-toggle-header)
  (define-key gnus-summary-mode-map "T" 'emacspeak-gnus-summary-hide-all-headers )
  (define-key gnus-summary-mode-map "t" 'emacspeak-gnus-summary-show-some-headers)
  (define-key gnus-summary-wash-map "D" 'emacspeak-gnus-summary-downcase-article))

(add-hook 'gnus-started-hook 'emacspeak-gnus-setup-keys)

;;}}}
;;{{{  Hiding headers

(defvar  emacspeak-gnus-ignored-most-headers
  (concat
   "^Path:\\|^Posting-Version:\\|^Article-I.D.:\\|^Expires:"
   "\\|^Date-Received:\\|^References:\\|^Control:\\|^Xref:"
   "\\|^Lines:\\|^Posted:\\|^Relay-Version:\\|^Message-ID:\\|^Nf-ID:"
   "\\|^Nf-From:\\|^Approved:\\|^Sender:"
   "\\|^Organization:\\|^Approved:\\|^Distribution:\\|^Apparently-To:"
   "\\|^Keywords:\\|^Copyright:\\|^X-Supersedes:\\|^ACategory: \\|^Slugword:"
   "\\|^Priority:\\|^ANPA:\\|^Codes:"
   "\\|^Originator:\\|^Comment:\\|^NNTP-Posting-Host:\\|Original-To:"
   "\\|^Followup-To:\\|^Original-Cc:\\|^Reply-To:")
  "Article headers to ignore when only important article headers are to be
spoken.
See command \\[emacspeak-gnus-summary-show-some-headers].")
(declaim (special gnus-ignored-headers))
(setq gnus-ignored-headers "^.*:")
(declaim (special gnus-visible-headers))
(setq gnus-visible-headers "^Subject:")

(defun emacspeak-gnus-summary-show-some-headers ()
  "Show only the important article headers,
i.e. sender name, and subject."
  (interactive)
  (declare (special emacspeak-gnus-ignored-most-headers )) 
  (let ((gnus-ignored-headers emacspeak-gnus-ignored-most-headers ))
    (gnus-summary-toggle-header 1)
    (gnus-summary-toggle-header -1)))

(defun emacspeak-gnus-summary-hide-all-headers()
  "Hide all headers in the article.
Use this command if you don't want to listen to any article headers when
reading news."
  (interactive)
  (let ((gnus-ignored-headers "^.*:"))
    (gnus-summary-toggle-header 1 )
    (gnus-summary-toggle-header -1)))

;;}}}
;;{{{  helper functions

(defvar emacspeak-gnus-large-article 30 
  "*Articles having more than
emacspeak-gnus-large-article lines will be considered to be a large article.
A large article is not spoken all at once;
instead you hear only the first screenful.")

(defsubst emacspeak-gnus-summary-speak-subject ()
  (emacspeak-dtk-sync)
  (dtk-speak (gnus-summary-article-subject )))

(defsubst emacspeak-gnus-speak-article-body ()
  (declare (special dtk-punctuation-mode
                    gnus-article-buffer))
  (with-current-buffer gnus-article-buffer
    (goto-char (point-min))
    (emacspeak-dtk-sync)
    (cond
     ((< (count-lines (point-min) (point-max))
         emacspeak-gnus-large-article)
      (emacspeak-speak-buffer  ))
     (t (emacspeak-auditory-icon 'large-movement )
        (let ((start (point))
              (window (get-buffer-window (current-buffer))))
          (with-selected-window window
            (save-excursion
              (move-to-window-line -1)
              (end-of-line)
              (emacspeak-speak-region start (point)))))))))

(defun emacspeak-gnus-summary-downcase-article ()
  "Downcases the article body
Helps to prevent words from being spelled instead of spoken."
  (interactive)
  (gnus-summary-select-article-buffer)
  (article-goto-body)
  (let ((beg (point))
        (end (point-max))
        (inhibit-read-only t))
    (downcase-region beg end))
  (gnus-article-show-summary)
  (emacspeak-auditory-icon 'modified-object)
  (dtk-speak "Downcased article body"))

;;}}}
;;{{{ Advise top-level gnus command

;;; emacs can hang if too many message sfly by as gnus starts
(defadvice gnus (around emacspeak pre act )
  "Temporarily deactivate advice on message"
  (let ((startup (not (gnus-alive-p)))
	(dtk-stop-immediately nil))
    (cond
     ((and startup (ems-interactive-p))
      (dtk-speak  "Starting gnus")
      (let ((emacspeak-speak-messages nil))
	ad-do-it)
      (emacspeak-auditory-icon 'news)
      (message "Gnus is ready ")
      (emacspeak-speak-line))
     (t				; gnus alive or non-interactive call
      ad-do-it
      (when (ems-interactive-p)
	(emacspeak-auditory-icon 'select-object)
	(emacspeak-speak-line))))))

(loop for f in
      '(gnus-group-suspend
        gnus-group-quit
        gnus-group-exit
        gnus-edit-form-done)
      do
      (eval
       `(defadvice ,f (after emacspeak pre act comp)
          "Provide auditory contextual feedback."
          (when (ems-interactive-p)
            (emacspeak-auditory-icon 'close-object)
            (emacspeak-speak-mode-line)))))

(defadvice gnus-group-edit-group-parameters (after emacspeak pre act comp)
  "Provide auditory contextual feedback."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (emacspeak-speak-mode-line)))

(loop for f in
      '(gnus-group-delete-group gnus-group-kill-group)
      do
      (eval
       `(defadvice ,f (after emacspeak pre act comp)
          "Provide auditory feedback."
          (when (ems-interactive-p)
            (emacspeak-auditory-icon 'delete-object)
            (emacspeak-speak-line)))))

;;}}}
;;{{{  starting up:

(loop for f in
      '(gnus-group-post-news gnus-group-mail)
      do
      (eval
       `(defadvice ,f (after emacspeak pre act comp)
          "Provide auditory feedback."
          (when (ems-interactive-p )
            (emacspeak-auditory-icon 'open-object)
            (emacspeak-speak-line)))))

(defadvice gnus-group-get-new-news (around emacspeak pre act comp)
  "Temporarily deactivate advice on message."
  (dtk-speak  "Getting new  gnus")
  (sit-for 2)
  (let ((emacspeak-speak-messages nil ))
    ad-do-it)
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'task-done)
    (message "Done ")))

(defadvice nnheader-message-maybe (around emacspeak pre act comp)
  "Silence emacspeak"
  (let ((emacspeak-speak-messages nil))
    ad-do-it))

;;}}}
;;{{{  Newsgroup selection

(defadvice gnus-group-select-group (after emacspeak pre act comp)
  "Provide auditory feedback.
Produce an auditory icon if possible."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (emacspeak-speak-line)))

(defadvice gnus-group-read-group  (after  emacspeak pre act comp)
  "Speak the article subject line.
Produce an auditory icon indicating 
an object has been opened."
  (when (ems-interactive-p ) 
    (emacspeak-auditory-icon 'open-object)
    (emacspeak-gnus-summary-speak-subject)))

(loop for f in
      '(gnus-group-prev-group
        gnus-group-prev-unread-group
        gnus-group-next-group
        gnus-group-next-unread-group)
      do
      (eval
       `(defadvice ,f (around emacspeak pre act comp)
          "Speak the newsgroup line. Produce an auditory icon if possible."
          (let ((saved-point (point )))
            ad-do-it
            (when (ems-interactive-p )
              (emacspeak-auditory-icon 'select-object)
              (if (= saved-point (point))
                  (dtk-speak "No more newsgroups ")
                (emacspeak-speak-line)))
            ad-return-value))))

(loop for f in
      '(gnus-group-first-unread-group
        gnus-group-best-unread-group
        gnus-group-jump-to-group
        gnus-group-get-new-news-this-group)
      do
      (eval
       `(defadvice ,f (after emacspeak pre act comp)
          "Provide auditory feedback."
          (when (ems-interactive-p )
            (emacspeak-auditory-icon 'select-object)
            (emacspeak-speak-line)))))

(defadvice gnus-group-unsubscribe-current-group (after emacspeak pre act comp)
  "Produce an auditory icon indicating this group is being deselected."
  (when (ems-interactive-p )
    (emacspeak-auditory-icon 'deselect-object)
    (emacspeak-speak-line )))

(defadvice gnus-group-catchup-current (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p )
    (emacspeak-auditory-icon 'mark-object)
    (emacspeak-speak-line)))

(defadvice gnus-group-yank-group (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p )
    (emacspeak-auditory-icon 'yank-object)
    (emacspeak-speak-line)))

(defadvice gnus-group-list-groups (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p )
    (emacspeak-auditory-icon 'open-object)
    (dtk-speak "Listing groups... done")))

(defadvice gnus-topic-mode (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p )
    (emacspeak-auditory-icon (if gnus-topic-mode 'on 'off))
    (message "Toggled topic mode %s" (if gnus-topic-mode "on" "off"))))

(defadvice gnus-article-fill-long-lines (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p )
    (emacspeak-auditory-icon 'modified-object)
    (dtk-speak "wrapped long lines")))

(defadvice gnus-group-list-all-groups (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p )
    (emacspeak-auditory-icon 'open-object)
    (dtk-speak "Listing all groups... done")))

(defadvice gnus-group-list-all-matching (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p )
    (emacspeak-auditory-icon 'open-object)
    (dtk-speak "Listing all matching groups... done")))

(defadvice gnus-group-list-killed (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p )
    (emacspeak-auditory-icon 'open-object)
    (dtk-speak "Listing killed groups... done")))

(defadvice gnus-group-list-matching (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p )
    (emacspeak-auditory-icon 'open-object)
    (dtk-speak "listing matching groups with unread articles... done")))

(defadvice gnus-group-list-zombies (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p )
    (emacspeak-auditory-icon 'open-object)
    (dtk-speak "Listing zombie groups... done")))

(defadvice gnus-group-customize (before emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p )
    (emacspeak-auditory-icon 'open-object)
    (message "Customizing group %s" (gnus-group-group-name))))

;;}}}
;;{{{  summary mode 

(loop for f in
      '(gnus-summary-clear-mark-backward
        gnus-summary-clear-mark-forward
        gnus-summary-unmark-as-processable)
      do
      (eval
       `(defadvice ,f  (around  emacspeak pre act comp)
          "Speak the next article subject. Produce an auditory icon if possible."
          (let ((saved-point (point )))
            ad-do-it
            (when (ems-interactive-p )
              (emacspeak-auditory-icon 'deselect-object)
              (if (= saved-point (point))
                  (dtk-speak "No more articles")
                (emacspeak-gnus-summary-speak-subject)))
            ad-return-value ))))

(loop for f in
      '(gnus-summary-mark-as-dormant
        gnus-summary-mark-as-expirable
        gnus-summary-mark-as-processable
        gnus-summary-mark-as-read-backward
        gnus-summary-mark-as-read-forward
        gnus-summary-mark-as-unread-backward
        gnus-summary-mark-as-unread-forward
        gnus-summary-tick-article-backward
        gnus-summary-tick-article-forward)
      do
      (eval
       `(defadvice ,f  (around  emacspeak pre act comp)
          "Speak the next article subject. Produce an auditory icon if possible."
          (let ((saved-point (point )))
            ad-do-it
            (when (ems-interactive-p )
              (emacspeak-auditory-icon 'mark-object)
              (if (= saved-point (point))
                  (dtk-speak "No more articles")
                (emacspeak-gnus-summary-speak-subject)))
            ad-return-value ))))

(defadvice gnus-summary-mark-region-as-read (after emacspeak pre act comp)
  "Produce an auditory feedback."
  (when (ems-interactive-p )
    (emacspeak-auditory-icon 'mark-object )
    (message "%s articles marked as read"
             (count-lines (region-beginning)
                          (region-end)))))

(loop for f in
      '(gnus-summary-delete-article
        gnus-summary-kill-same-subject
        gnus-summary-kill-thread)
      do
      (eval
       `(defadvice ,f (after emacspeak pre act comp)
          "Speak the next article subject. Produce an auditory icon if possible."
          (when (ems-interactive-p )
            (emacspeak-auditory-icon  'delete-object)
            (emacspeak-gnus-summary-speak-subject )))))

(loop for f in
      '(gnus-summary-catchup-from-here gnus-summary-catchup-to-here)
      do
      (eval
       `(defadvice ,f (after emacspeak pre act comp)
          "Produce an auditory icon if possible."
          (when (ems-interactive-p )
            (emacspeak-auditory-icon  'mark-object)))))

(defadvice  gnus-summary-select-article-buffer (after emacspeak pre act comp)
  "Speak the modeline.
Indicate change of selection with an auditory icon if possible."
  (when (ems-interactive-p  )
    (emacspeak-auditory-icon 'open-object)
    (emacspeak-speak-mode-line)))

(loop for f in
      '(gnus-summary-prev-article
        gnus-summary-next-article
        gnus-summary-prev-unread-article
        gnus-summary-next-unread-article
        gnus-summary-prev-same-subject
        gnus-summary-next-same-subject)
      do
      (eval
       `(defadvice ,f (around emacspeak pre act comp)
          "Speak the article. "
          (let ((saved-point (point )))
            ad-do-it
            (when (ems-interactive-p)
              (if (= saved-point (point))
                  (dtk-speak "No more articles ")
                (emacspeak-auditory-icon 'open-object)
                (emacspeak-gnus-speak-article-body)))
            ad-return-value ))))

(loop for f in
      '(gnus-summary-exit-no-update
        gnus-summary-exit
        gnus-summary-catchup-and-exit)
      do
      (eval
       `(defadvice ,f (around emacspeak pre act comp)
          "Say the next unread newsgroup. Play an auditory icon if possible."
          (let ((cur-group gnus-newsgroup-name ))
            ad-do-it
            (when (ems-interactive-p  )
              (emacspeak-auditory-icon 'close-object)
              (if (eq cur-group (gnus-group-group-name))
                  (dtk-speak "No more unread newsgroups")
                (emacspeak-speak-line)))
            ad-return-value ))))

(loop for f in
      '(gnus-summary-prev-subject
        gnus-summary-next-subject
        gnus-summary-prev-unread-subject
        gnus-summary-next-unread-subject
        gnus-summary-goto-subject
        gnus-summary-prev-thread
        gnus-summary-next-thread
        gnus-summary-up-thread
        gnus-summary-down-thread)
      do
      (eval
       `(defadvice ,f (around  emacspeak pre act comp)
          "Speak the article subject. Produce an auditory icon if possible."
          (let ((saved-point (point )))
            ad-do-it
            (when (ems-interactive-p )
              (if (= saved-point (point))
                  (dtk-speak "No more articles ")
                (emacspeak-auditory-icon 'select-object )
                (emacspeak-gnus-summary-speak-subject)))
            ad-return-value ))))

(loop for f in
      '(gnus-topic-goto-next-topic
        gnus-topic-goto-previous-topic)
      do
      (eval
       `(defadvice ,f (after emacspeak pre act comp)
          "Provide auditory feedback."
          (when (ems-interactive-p)
            (emacspeak-auditory-icon 'item)
            (emacspeak-speak-line)))))

(loop for f in
      '(gnus-summary-mail-forward
        gnus-summary-post-news
        gnus-summary-mail-other-window)
      do
      (eval
       `(defadvice ,f (after emacspeak pre act comp)
          "Provide auditory feedback."
          (when (ems-interactive-p)
            (emacspeak-auditory-icon 'open-object)
            (emacspeak-speak-line)))))

(loop for f in
      '(gnus-summary-reply
        gnus-summary-reply-with-original)
      do
      (eval
       `(defadvice ,f (after emacspeak pre act comp)
          "Provide auditory feedback."
          (when (ems-interactive-p)
            (emacspeak-auditory-icon 'open-object)))))

(defadvice gnus-summary-resend-message (after emacspeak pre act comp)
  "Provide auditory feedback"
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'task-done)))

(defadvice gnus-summary-kill-same-subject-and-select (after emacspeak pre act comp)
  "Speak the subject and speak the first screenful.
Produce an auditory icon
indicating the article is being opened."
  (when (ems-interactive-p )
    (emacspeak-auditory-icon 'delete-object)
    (emacspeak-gnus-summary-speak-subject)
    (sit-for 2)
    (emacspeak-auditory-icon 'open-object)
    (emacspeak-gnus-speak-article-body)))

(defadvice gnus-summary-save-article (after emacspeak pre act comp)
  "Produce an auditory icon if possible."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'save-object)))

(defadvice mm-save-part (after emacspeak pre act comp)
  "Produce an auditory icon if possible."
  (emacspeak-auditory-icon 'save-object))

(defadvice gnus-summary-display-article (after emacspeak pre act comp)
  "Produce an auditory icon if possible."
  (emacspeak-auditory-icon 'open-object))

(defadvice gnus-summary-toggle-header (after emacspeak pre act comp)
  "Produce an auditory icon if possible."
  (when (ems-interactive-p)
    (save-excursion
      (set-buffer  "*Article*")
      (emacspeak-auditory-icon
       (if (gnus-article-hidden-text-p 'headers) 'off 'on)))))

(loop for f in
      '(gnus-summary-show-article
        gnus-summary-first-unread-article
        gnus-summary-goto-last-article)
      do
      (eval
       `(defadvice ,f (after emacspeak pre act comp)
          "Start speaking the article. "
          (when (ems-interactive-p )
            (emacspeak-auditory-icon 'open-object)
            (emacspeak-gnus-speak-article-body)))))

(loop for f in
      '(gnus-summary-next-page
        gnus-summary-prev-page
        gnus-summary-beginning-of-article
        gnus-summary-end-of-article)
      do
      (eval
       `(defadvice ,f (after emacspeak pre act comp)
          "Speak the article page content."
          (declare (special gnus-article-buffer))
          (dtk-stop)
          (emacspeak-auditory-icon 'scroll)
          (save-excursion
            (set-buffer  gnus-article-buffer)
            (let ((start  (point ))
                  (window (get-buffer-window (current-buffer ))))
              (with-selected-window window
                (save-excursion
                  (move-to-window-line -1)
                  (end-of-line)
                  (emacspeak-speak-region start (point )))))))))

;;}}}
;;{{{  Draft specific commands

(defadvice gnus-draft-edit-message (after emacspeak pre act comp)
  "Provide auditory feedback"
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'open-object)))

(defadvice gnus-draft-send-message (after emacspeak pre act comp)
  "Provide auditory feedback"
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'task-done)))

(defadvice gnus-draft-send-all-messages (after emacspeak pre act comp)
  "Provide auditory feedback"
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'task-done)))

(defadvice gnus-draft-toggle-sending (after emacspeak pre act comp)
  "Provide auditory feedback"
  (when (ems-interactive-p)
    (emacspeak-auditory-icon
     (if (= (char-after (line-beginning-position)) ?\ )
	 'deselect-object
       'mark-object))))

(defadvice gnus-summary-hide-all-threads (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'close-object)
    (emacspeak-speak-line)))

;;}}}
;;{{{  Article reading

(defadvice gnus-article-describe-key-briefly (after emacspeak pre act comp)
  "Speak what you displayed."
  (when (ems-interactive-p)
    (dtk-speak ad-return-value))
  ad-return-value)

(loop for f in
      '(gnus-article-edit-exit
        gnus-article-edit-done)
      do
      (eval
       `(defadvice ,f (after emacspeak pre act comp)
          "Provide auditory feedback."
          (when (ems-interactive-p)
            (emacspeak-auditory-icon 'close-object)
            (emacspeak-speak-line)))))

(defadvice gnus-article-mail (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (emacspeak-speak-line)))

(defadvice gnus-article-show-summary  (after emacspeak pre act comp)
  "Speak the modeline.
Indicate change of selection with
an auditory icon if possible."
  (when (ems-interactive-p  )
    (emacspeak-auditory-icon 'select-object)
    (emacspeak-speak-mode-line)))

(loop for f in
      '(gnus-article-next-page
        gnus-article-prev-page
        gnus-article-goto-next-page
        gnus-article-goto-prev-page)
      do
      (eval
       `(defadvice ,f (after emacspeak pre act comp)
          "Speak the current window full of news."
          (when (ems-interactive-p )
            (emacspeak-auditory-icon 'scroll)
            (emacspeak-speak-current-window )))))

(defadvice gnus-article-next-button (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p )
    (let ((end (next-single-property-change
                (point) 'gnus-callback)))
      (emacspeak-auditory-icon 'large-movement)
      (message (buffer-substring
                (point)end )))))

(defadvice gnus-article-press-button (before emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p )
    (emacspeak-auditory-icon 'button)))

(defadvice gnus-article-mode (after emacspeak pre act comp)
  "Turn on voice lock mode."
  (voice-lock-mode (if global-voice-lock-mode 1 -1)))

;;}}}
;;{{{ refreshing the pronunciation  and punctuation mode

(loop
 for hook  in 
 '(
   gnus-article-mode-hook gnus-group-mode-hook gnus-summary-mode-hook
                          gnus-agent-mode-hook  gnus-article-edit-mode-hook
                          gnus-server-mode-hook gnus-category-mode-hook
                          )
 do
 (add-hook
  hook 
  #'(lambda ()
      (dtk-set-punctuations emacspeak-gnus-punctuation-mode)
      (emacspeak-pronounce-refresh-pronunciations))))

(declaim (special emacspeak-pronounce-internet-smileys-pronunciations))
(emacspeak-pronounce-augment-pronunciations 'gnus-article-mode
					    emacspeak-pronounce-internet-smileys-pronunciations)

;;}}}
;;{{{ rdc: mapping font faces to personalities 

;; article buffer personalities

;; Since citation does not normally go beyond 4 levels deep, in my 
;; experience, there are separate voices for the first four levels
;; and then they are repeated
(voice-setup-add-map
 ;; NOTE
 ;; face names are either marked at the end of the line with a comment
 ;; consisting of the emacs version or they are unmarked.  The unmarked 
 ;; face names are for gnus shipped with emacs 22 
 ;; rdc 102206
 '(
   (gnus-cite-1 voice-bolden-medium)
   (gnus-cite-2 voice-lighten) 
   (gnus-cite-3 voice-lighten-extra)
   (gnus-cite-4 voice-smoothen)
   (gnus-cite-5 voice-smoothen-extra)
   (gnus-cite-6 voice-lighten)
   (gnus-cite-7 voice-lighten-extra)
   (gnus-cite-8 voice-bolden)
   (gnus-cite-9 voice-bolden-medium)
   (gnus-cite-10 voice-lighten)
   (gnus-cite-11 voice-lighten-extra)
   (gnus-emphasis-highlight-words voice-lighten-extra)
   (gnus-emphasis-bold voice-bolden-and-animate)
   (gnus-emphasis-strikethru voice-bolden-extra)
   (gnus-emphasis-italic voice-lighten)
   (gnus-emphasis-underline voice-brighten-extra)
   (gnus-signature voice-animate)
   (gnus-header-content voice-bolden)
   (gnus-header-name voice-animate)
   (gnus-header-from voice-bolden)
   (gnus-header-newsgroups voice-bolden)
   (gnus-header-subject voice-bolden)
   ;; ;; summary buffer personalities
   ;; since there are so many distinctions, most variations
   ;; on the same thing are given the same voice.  Any user that
   ;; uses low and high interest is sufficiently advanced to change
   ;; the voice to his own preferences
   (gnus-summary-normal-read voice-smoothen)
   (gnus-summary-high-read voice-bolden)
   (gnus-summary-low-read voice-bolden)
   (gnus-summary-normal-ticked voice-brighten-extra)
   (gnus-summary-high-ticked voice-brighten-extra)
   (gnus-summary-low-ticked voice-brighten-extra)
   (gnus-summary-normal-ancient voice-smoothen-extra)
   (gnus-summary-high-ancient voice-smoothen-extra)
   (gnus-summary-low-ancient voice-smoothen-extra)
   (gnus-summary-normal-undownloaded voice-bolden-and-animate)
   (gnus-summary-high-undownloaded-face voice-bolden-and-animate) ;; emacs 21
   (gnus-summary-high-undownloaded voice-bolden-and-animate)
   (gnus-summary-low-undownloaded-face voice-bolden) ;; emacs 21
   (gnus-summary-low-undownloaded voice-bolden)
   (gnus-summary-low-unread voice-bolden-medium)
   (gnus-summary-high-unread voice-brighten-extra)
   (gnus-summary-selected voice-animate-extra)
   (gnus-summary-cancelled voice-bolden-extra)

   ;; group buffer personalities
   ;; I think the voice used for the groups in the buffer should be the 
   ;; default voice.  I might ask if there is a call for different voices 
   ;; as they are only necessary if users have persistently visible groups
   ;; in the case of empty groups, and voices for the various levels.
   (gnus-group-mail-1-empty voice-bolden-extra)
   (gnus-group-mail-1 default)
   (gnus-group-mail-2-empty voice-bolden-extra)
   (gnus-group-mail-2 default)
   (gnus-group-mail-3-empty  voice-bolden-extra)
   (gnus-group-mail-3 default)
   (gnus-group-mail-low-empty voice-bolden-extra)
   (gnus-group-mail-low default)
   (gnus-group-news-1-empty voice-bolden-extra)
   (gnus-group-news-1 default)
   (gnus-group-news-2-empty voice-bolden-extra)
   (gnus-group-news-2 default)
   (gnus-group-news-3-empty voice-bolden-extra)
   (gnus-group-news-3 default)
   (gnus-group-news-4-empty voice-bolden-extra)
   (gnus-group-news-4 default)
   (gnus-group-news-5-empty voice-bolden-extra)
   (gnus-group-news-5 default)
   (gnus-group-news-6-empty voice-bolden-extra)
   (gnus-group-news-6 default)
   (gnus-group-news-low-empty voice-bolden-extra)
   (gnus-group-news-low default)
   
   ;; server buffer personalities

   (gnus-server-agent voice-bolden)
   (gnus-server-closed voice-bolden-medium)
   (gnus-server-denied voice-bolden-extra)
   (gnus-server-offline voice-animate)
   (gnus-server-opened voice-lighten)))

;;}}}
(provide 'emacspeak-gnus)
;;{{{  end of file 
;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: nil
;;; end: 
;;}}}
