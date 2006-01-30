;;; emacspeak-jabber.el --- Speech-Enable jabber 
;;; $Id: emacspeak-jabber.el,v 23.505 2005/11/25 16:30:50 raman Exp $
;;; $Author: raman $
;;; Description: speech-enable jabber 
;;; Keywords: Emacspeak, jabber
;;{{{  LCD Archive entry: 

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |raman@cs.cornell.edu 
;;; A speech interface to Emacs |
;;; $Date: 2005/11/25 16:30:50 $ |
;;;  $Revision: 23.505 $ | 
;;; Location undetermined
;;;

;;}}}
;;{{{  Copyright:

;;; Copyright (c) 1995 -- 2004, T. V. Raman
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{ Introduction:

;;; Commentary:
;;; emacs-jabber.el implements a  jabber client for emacs
;;; emacs-jabber is hosted at sourceforge.
;;; I use emacs-jabber with my gmail.com account

;;; Code:

;;}}}
;;{{{  Required modules

(require 'emacspeak-preamble)
;;}}}
;;{{{ Advice interactive commands:

(defadvice jabber-connect (after emacspeak pre act comp)
  "Provide auditory icon if possible."
  (when (interactive-p)
    (emacspeak-auditory-icon 'on)))

(defadvice jabber-disconnect (after emacspeak pre act comp)
  "Provide auditory icon if possible."
  (when (interactive-p)
    (emacspeak-auditory-icon 'off)))

(loop for f in
      '(jabber-roster-mode
	jabber-chat-mode
	jabber-browse-mode)
      do
      (eval
       `(defadvice ,f (after emacspeak pre act comp)
	  "Turn on voice lock mode."
	  (declare (special voice-lock-mode))
	  (emacspeak-pronounce-refresh-pronunciations)
	  (setq voice-lock-mode t))))

;;}}}
;;{{{ silence keepalive

(loop for f in 
      '(jabber-keepalive-do
	jabber-keepalive-got-response)
      do
      (eval
       `(defadvice ,f (around emacspeak pre act comp)
	  "Silence keepalive messages."
	  (let ((emacspeak-speak-messages nil))
	    ad-do-it
	    ad-return-value))))

;;}}}
;;{{{ chat buffer:

(defadvice jabber-chat-buffer-send (after emacspeak pre act comp)
  "Produce auditory icon."
  (when (interactive-p)
    (emacspeak-auditory-icon 'close-object)))
(loop for f in 
      '(jabber-chat-with
	jabber-chat-with-jid-at-point)
      do
      (eval
       `(defadvice ,f (after emacspeak pre act comp)
	  "Silence keepalive messages."
	  (when (interactive-p)
	    (emacspeak-auditory-icon 'open-object)
	    (emacspeak-speak-mode-line)))))
;;}}}
;;{{{ alerts
(defcustom emacspeak-jabber-speak-presence-alerts nil
  "Set to T if you want to hear presence alerts."
  :type  'boolean
  :group 'emacspeak-jabber)

(defadvice jabber-presence-default-message (around emacspeak pre
                                                   act comp)
  "Allow emacspeak to control if the message is spoken."
  (cond
   (emacspeak-jabber-speak-presence-alerts ad-do-it)
   (t (let ((emacspeak-speak-messages nil))
        ad-do-it)))
  ad-return-value)

;;;this is what I use as my jabber alert function:
(defun emacspeak-jabber-message-default-message (from buffer
                                                      text)
  "Speak the message."
  (when (or jabber-message-alert-same-buffer
	    (not (memq (selected-window) (get-buffer-window-list buffer))))
    (if (jabber-muc-sender-p from)
	(format "Private message from %s in %s"
		(jabber-jid-resource from)
		(jabber-jid-displayname (jabber-jid-user from)))
      (format "%s: %s" (jabber-jid-displayname from) text))))

;;{{{ interactive commands:

(defun emacspeak-jabber-popup-roster ()
  "Pop to Jabber roster."
  (interactive)
  (declare (special jabber-roster-buffer))
  (pop-to-buffer jabber-roster-buffer)
  (goto-char (point-min))
  (emacspeak-auditory-icon 'select-object)
  (emacspeak-speak-mode-line))

;;}}}

;;}}}
;;{{{ Pronunciations 
(declaim (special emacspeak-pronounce-internet-smileys-pronunciations))
(emacspeak-pronounce-augment-pronunciations 'jabber-chat-mode
					    emacspeak-pronounce-internet-smileys-pronunciations)
(emacspeak-pronounce-augment-pronunciations 'jabber-mode
                                            emacspeak-pronounce-internet-smileys-pronunciations)

;;}}}
;;{{{ Voices

(def-voice-font emacspeak-jabber-roster-user-online-personality
  voice-bolden
  'jabber-roster-user-online
  "Personality for online jabber users.")

(def-voice-font emacspeak-jabber-roster-user-offline-personality
  voice-lighten-extra
  'jabber-roster-user-offline
  "Personality for offline jabber users.")

(def-voice-font emacspeak-jabber-roster-user-away-personality
  voice-lighten
  'jabber-roster-user-away
  "Personality for away jabber users.")

(def-voice-font emacspeak-jabber-roster-user-xa-personality
  voice-lighten-extra
  'jabber-roster-user-xa
  "Personality for extended away jabber users.")

(def-voice-font emacspeak-jabber-roster-user-dnd-personality
  voice-animate-extra
  'jabber-roster-user-dnd
  "Personality for do not disturb jabber users.")

(def-voice-font emacspeak-jabber-roster-user-chatty-personality
  voice-animate
  'jabber-roster-user-chatty
  "Personality for chatty jabber users.")

(def-voice-font emacspeak-jabber-roster-user-error-personality
  voice-bolden-and-animate
  'jabber-roster-user-error
  "Personality for jabber users sending presence errors.")

(def-voice-font emacspeak-jabber-chat-prompt-local-personality
  voice-bolden
  'jabber-chat-prompt-local
  "Personality for jabber chat prompt for what you type.")

(def-voice-font emacspeak-jabber-chat-prompt-foreign-personality
  voice-bolden-and-animate
  'jabber-chat-prompt-foreign
  "Personality for jabber chat prompt for what they send.")

(def-voice-font emacspeak-jabber-chat-prompt-system-personality
  voice-monotone
  'jabber-chat-prompt-system
  "Personality for jabber special and system messages.")

(def-voice-font emacspeak-jabber-rare-time-personality
  voice-smoothen-extra
  'jabber-rare-time-face
  "Personality for the rare time info in jabber chat buffer.")

(def-voice-font emacspeak-jabber-title-small-personality
  voice-animate
  'jabber-title-small
  "Personality for jabber small titles.")

(def-voice-font emacspeak-jabber-title-medium-personality
  voice-smoothen
  'jabber-title-medium
  "Personality for jabber medium titles.")

(def-voice-font emacspeak-jabber-title-large-personality
  voice-bolden
  'jabber-title-large
  "Personality for jabber large titles.")

;;}}}
(provide 'emacspeak-jabber)
;;{{{ end of file 

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end: 

;;}}}
