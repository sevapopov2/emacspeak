;;; tts.jl -- Sawfish interface  to Emacspeak speech servers 
;;; $Id: tts.jl 4047 2006-08-11 19:11:17Z tv.raman.tv $
;;; $Author: tv.raman.tv $
;;; Description:   Interface REP/Sawfish to Emacspeak TTS servers
;;; Keywords: Sawfish, Emacspeak, Audio Desktop
;;{{{  LCD Archive entry:

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |raman@cs.cornell.edu
;;; A speech interface to Emacs |
;;; $Date: 2006-08-11 12:11:17 -0700 (Fri, 11 Aug 2006) $ |
;;;  $Revision: 4047 $ |
;;; Location undetermined
;;;

;;}}}
;;{{{  Copyright:

;;; Copyright (C)  2000 -- 2002, T. V. Raman<raman@cs.cornell.edu>
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
;;{{{ Introduction:

;;; Commentary:
;;; Interface REP/Sawfish to Emacspeak TTS servers

;;}}}
;; Customise options.

(defgroup tts "Speech synthesis")

(defcustom tts-client "telnet"
  "TTS client "
  :group     tts
  :type      string
  :allow-nil nil)

(defcustom tts-host "localhost"
  "Host running TTS  "
  :group     tts
  :type      string
  :allow-nil nil)

(defcustom tts-port "2222"
  "TTS port on server "
  :group     tts
  :type      string
  :allow-nil nil)

(defcustom tts-server "multispeech"
  "TTS server "
  :group     tts
  :type      string
  :allow-nil nil)

(defvar emacspeak "/usr/local/share/emacs/site-lisp/emacspeak"
"Root of Emacspeak installation.")

(defvar tts-process nil
"Handle to tts server connection.")

(defvar tts-speaking-events nil
  "Wether events are to be spoken automatically.")

(defun tts-open-connection ()
  "Open a TTS session."
  (interactive)
  (setq tts-process (make-process))
  (start-process tts-process tts-client tts-host
		 tts-port))

(defun tts-open ()
  "Open a TTS session."
  (interactive)
  (setq tts-process (make-process))
  (start-process tts-process
		 (expand-file-name tts-server
				   (expand-file-name "servers" emacspeak))))

(defun tts-close ()
  "Close a TTS session."
  (interactive)
  (when(and  (processp tts-process)
             (process-running-p tts-process))
    (kill-process tts-process))
  (setq tts-process nil))

(defun tts-running-p ()
  "Is there a tts process up and running?"
  (and (processp tts-process) (process-running-p
                               tts-process)))

(defvar tts-stop-immediately t
  "Non nil means previous speech is flushed immediately.")

(defun tts-say (text)
  "Say some text."
  (unless (and  tts-process
 (process-running-p tts-process))
      (tts-open))
  (when tts-stop-immediately
    (format tts-process "s\n"))
  (format tts-process "q {%s}\nd\n" text))

(defun tts-say-workspace ()
  "Say the name of the current workspace."
  (interactive)
  (tts-say (or (nth current-workspace workspace-names)
                    (format nil "Workspace %d"
                            current-workspace))))

(defvar tts-say-window-details-p nil 
"Non-nil means we also speak the window's position and dimensions.")

(defvar tts-previous-window nil
  "Previously focused window.")

(defun tts-store-window (window)
  "Store leaved window for reference."
  (setq tts-previous-window window))

(defun tts-say-window-details ()
  "Toggle speaking windows details on and off."
  (interactive)
  (setq tts-say-window-details-p
	(not tts-say-window-details-p))
  (tts-say (format nil "Speaking window details %s"
		   (if tts-say-window-details-p
		       "on"
		     "off"))))

(defun tts-say-window (window)
  "Say the name of window W."
  (interactive "%W")
  (when window
    (let ((title (if (desktop-window-p window)
		     "Desktop"
		   (window-name window)))
	  (position (if (desktop-window-p window)
			(cons 0 0)
		      (window-position window)))
	  (dimensions (if (desktop-window-p window)
			  (screen-dimensions)
			(window-dimensions window))))
      (if tts-say-window-details-p
	  (tts-say
	   (format nil "%s at %s with dimensions %s"
		   title position dimensions))
	(tts-say title)))))

(defun tts-say-current-window ()
  "Say the name of the current window."
  (interactive)
  (tts-say-window (input-focus)))

(defun tts-say-window-change (window)
  "Say new window when focus has changed."
  (unless (eq window tts-previous-window)
    (tts-store-window window)
    (tts-say-window window)))

(defun tts-say-workspace-on-change (enable)
  "Enable/disable the reading of a workspace's name when you change to it."
  (if enable
      (unless (in-hook-p 'enter-workspace-hook tts-say-workspace)
        (add-hook 'enter-workspace-hook tts-say-workspace))
    (remove-hook 'enter-workspace-hook tts-say-workspace)))

(defun tts-say-window-on-enter (enable)
  "Enable/disable the reading of a window's name when entering it with mouse."
  (if enable
      (unless (in-hook-p 'enter-notify-hook tts-say-window)
        (add-hook 'enter-notify-hook tts-say-window))
    (remove-hook 'enter-notify-hook tts-say-window)))

(defun tts-say-window-on-focus (enable)
  "Enable/disable the reading of a window's name when it receives focus."
  (if enable
      (unless (in-hook-p 'focus-in-hook tts-say-window-change)
        (add-hook 'focus-in-hook tts-say-window-change))
    (remove-hook 'focus-in-hook tts-say-window-change)))

(defun tts-speak-events (enable)
  "Enable or disable speech feedback on events."
  (tts-say-window-on-focus enable)
  (tts-say-window-on-enter enable)
  (tts-say-workspace-on-change enable)
  (setq tts-speaking-events enable))

(defun tts-toggle-speaking-events ()
  "Toggle speech feedback on events."
  (interactive)
  (tts-speak-events (not tts-speaking-events))
  (tts-say (format nil "Speaking events %s"
		   (if tts-speaking-events
		       "enabled"
		     "disabled"))))

(add-hook 'focus-out-hook tts-store-window)


(provide 'tts)
(message "Loaded tts.jl")

;;; tts.jl ends here
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}
