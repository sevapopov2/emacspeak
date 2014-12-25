;;; emacspeak-alsaplayer.el --- Control alsaplayer from Emacs
;;; $Id: emacspeak-alsaplayer.el 9258 2014-06-26 15:26:44Z tv.raman.tv $
;;; $Author: tv.raman.tv $
;;; Description: Controlling alsaplayer from emacs 
;;; Keywords: Emacspeak, alsaplayer
;;{{{  LCD Archive entry: 

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |raman@cs.cornell.edu 
;;; A speech interface to Emacs |
;;; $Date: 2008-08-01 17:37:36 -0700 (Fri, 01 Aug 2008) $ |
;;;  $Revision: 4532 $ | 
;;; Location undetermined
;;;

;;}}}
;;{{{  Copyright:

;;; Copyright (c) 1995 -- 2011, T. V. Raman
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

;;; Defines a simple derived mode for interacting with
;;; alsaplayer.
;;; alsaplayer navigation commands  work via single keystrokes.

;;; Code:

;;}}}
;;{{{  Required modules
(require 'derived)
(require 'emacspeak-preamble)
(require 'emacspeak-amark)

;;}}}
;;{{{ define a derived mode for alsaplayer interaction

;;;###autoload
(define-prefix-command 'emacspeak-alsaplayer-prefix-command
  'emacspeak-alsaplayer-mode-map)
(defun emacspeak-alsaplayer-header-line ()
  "Return information suitable for header line."
  (declare (special emacspeak-alsaplayer-coding-system))
  (let* ((coding-system-for-read emacspeak-alsaplayer-coding-system)
         (title (shell-command-to-string "alsaplayer --status |
grep title:"))
         (path (shell-command-to-string "alsaplayer --status |
grep path:")))
    (cond
     ((or (null (get-buffer-process (current-buffer)))
          (not (eq 'run (process-status (get-buffer-process
                                         (current-buffer))))))
      "        No Active Session")
     ((>  (length title) 0)
      (substring title 6 -1))
     ((>  (length path) 0)
      (setq path (substring path 6 -1))
      (substring  path
                  (length (file-name-directory path))))
     (t "New Session"))))

(define-derived-mode emacspeak-alsaplayer-mode special-mode 
  "Alsaplayer Interaction"
  "Major mode for alsaplayer interaction. \n\n
\\{emacspeak-alsaplayer-mode-map}"
  (setq header-line-format '((:eval
                              (emacspeak-alsaplayer-header-line)))))

;;}}}
;;{{{ launch  emacspeak-alsaplayer

;;;###autoload
(defgroup emacspeak-alsaplayer nil
  "AlsaPlayer from emacs."
  :group 'emacspeak)

(defcustom emacspeak-alsaplayer-auditory-feedback t
  "Turn this on if you want spoken feedback and auditory icons from alsaplayer."
  :type 'boolean
  :group 'emacspeak-alsaplayer)

(defcustom emacspeak-alsaplayer-rewind-step 10
  "Forward or backward rewind step in seconds."
  :type 'integer
  :group 'emacspeak-alsaplayer)

;;;###autoload
(defcustom emacspeak-alsaplayer-program
  "alsaplayer"
  "Alsaplayer executable."
  :type 'string
  :group 'emacspeak-alsaplayer)

(defcustom emacspeak-alsaplayer-output nil
  "Alsaplayer driver for sound output."
  :type '(choice (const :tag "default" nil)
		 (const "alsa")
		 (const "oss")
		 (const "jack")
		 (const "nas")
		 (const "sgi")
		 (const "sparc")
		 (string :tag "Driver name"))
  :group 'emacspeak-alsaplayer)

(defcustom emacspeak-alsaplayer-coding-system nil
  "Alsaplayer output coding system.
It is used for tags decoding."
  :type '(coding-system :size 0)
  :group 'emacspeak-alsaplayer)

(defcustom emacspeak-alsaplayer-media-directory
  (expand-file-name "~/mp3/")
  "Directory to look for media files."
  :type 'directory
  :group 'emacspeak-alsaplayer)

(defvar emacspeak-alsaplayer-buffer "*alsaplayer*"
  "Buffer for alsaplayer interaction.")
(defcustom emacspeak-alsaplayer-device "default"
  "Device to use for alsaplayer"
  :type '(choice
          (const  :tag "Ignore" nil)
          (const  :tag "Card 1" "hw:1,0")
          (const :tag "ALSA_DEFAULT"  "default")
          (string :tag "Custom"))
  :group  'emacspeak-alsaplayer)

(defun emacspeak-alsaplayer-active-p ()
  "Check if the alsaplayer is running."
  (declare (special emacspeak-alsaplayer-buffer))
  (let ((buffer (get-buffer emacspeak-alsaplayer-buffer)))
    (and buffer
         (get-buffer-process buffer)
         (eq 'run (process-status (get-buffer-process buffer))))))

;;;###autoload
(defun emacspeak-alsaplayer-launch ()
  "Launch Alsaplayer.
user is placed in a buffer associated with the newly created
Alsaplayer session."
  (interactive)
  (declare (special emacspeak-alsaplayer-program emacspeak-alsaplayer-buffer
                    emacspeak-alsaplayer-device))
  (let ((buffer (get-buffer-create emacspeak-alsaplayer-buffer))
        (deactivate-mark nil))
    (save-current-buffer
      (set-buffer buffer)
      (cond
       ((and (get-buffer-process buffer)
             (eq 'run (process-status (get-buffer-process buffer))))
        (pop-to-buffer buffer 'other-window))
       (t
        (setq buffer-undo-list t)
        (shell-command
         (format "%s -r -i daemon %s%s&"
                 emacspeak-alsaplayer-program
                 (if emacspeak-alsaplayer-output
                     (format "-o %s " emacspeak-alsaplayer-output)
                   "")
                 (if emacspeak-alsaplayer-device
                     (format "-d %s " emacspeak-alsaplayer-device)
                   ""))
         (current-buffer))
        (pop-to-buffer buffer 'other-window)
        (emacspeak-alsaplayer-mode)))
      (emacspeak-amark-load)
      (when (and emacspeak-alsaplayer-auditory-feedback (ems-interactive-p))
        (emacspeak-auditory-icon 'open-object)
        (emacspeak-speak-mode-line)))))

;;}}}
;;{{{  Invoke commands:

(defun emacspeak-alsaplayer-send-command(command &optional watch-pattern no-refresh)
  "Send command to Alsaplayer.
Optional second arg watch-pattern specifies line of output to
  focus on.  Optional third arg no-refresh is used to avoid
  getting status twice."
  (declare (special emacspeak-alsaplayer-program
                    emacspeak-alsaplayer-buffer))
  (unless (emacspeak-alsaplayer-active-p)
    (emacspeak-alsaplayer-launch)
    (unless (emacspeak-alsaplayer-active-p)
      (error "Cannot launch alsaplayer session")))
  (save-current-buffer
    (let ((deactivate-mark nil)
          (coding-system-for-read emacspeak-alsaplayer-coding-system))
      (set-buffer (get-buffer emacspeak-alsaplayer-buffer))
      (erase-buffer)
      (shell-command
       (format "%s %s %s"
               emacspeak-alsaplayer-program
               command
               (if no-refresh
                   ""
                 "; alsaplayer --status"))
       (current-buffer)))
      (goto-char (point-min))
      (when (and (search-forward "path: " nil t)
                 emacspeak-alsaplayer-coding-system
                 (not (eq (car default-process-coding-system)
                          emacspeak-alsaplayer-coding-system)))
        (encode-coding-region (point) (line-end-position)
                              emacspeak-alsaplayer-coding-system)
        (decode-coding-region (point) (line-end-position)
                              (car default-process-coding-system))))
  (when (and watch-pattern
             (eq (current-buffer) (get-buffer emacspeak-alsaplayer-buffer)))
    (goto-char (point-min))
    (search-forward watch-pattern  nil t)))

(defun emacspeak-alsaplayer-add-to-queue (resource)
  "Add specified resource to queue."
  (interactive
   (list
    (let ((completion-ignore-case t)
          (read-file-name-completion-ignore-case t)
          (ido-work-directory-list
           (remove-if-not 
            #'(lambda (d)
                (string-match  emacspeak-media-directory-regexp  d))
            ido-work-directory-list)))
      (expand-file-name
       (read-file-name "Media Resource: "
                       emacspeak-alsaplayer-media-directory)))))
  (emacspeak-alsaplayer-send-command
   (format "--enqueue %s"
           (shell-quote-wildcard-pattern
            (if (file-directory-p resource)
                (format "%s/*" resource)
              resource)))
   "playlist_length:")
  (when (and emacspeak-alsaplayer-auditory-feedback
             (ems-interactive-p)
             (eq major-mode 'emacspeak-alsaplayer-mode))
    (emacspeak-auditory-icon 'select-object)
    (emacspeak-speak-line)))

;;;###autoload
(defun emacspeak-alsaplayer-find-and-add-to-queue (pattern)
  "Find  specified resource and add to queue."
  (interactive
   (list
    (read-from-minibuffer "Pattern")))
  (shell-command
   (format "find . -iname '%s' -print0 | xargs -0 alsaplayer -e "
           pattern))
  (when (and emacspeak-alsaplayer-auditory-feedback
             (ems-interactive-p)
             (eq major-mode 'emacspeak-alsaplayer-mode))
    (emacspeak-auditory-icon 'select-object)
    (emacspeak-speak-line)))

(defun emacspeak-alsaplayer-replace-queue (resource)
  "Replace currently playing music."
  (interactive
   (list
    (let ((completion-ignore-case t)
          (read-file-name-completion-ignore-case t))
      (expand-file-name
       (read-file-name "New media resource: "
                       emacspeak-alsaplayer-media-directory)))))
  (emacspeak-alsaplayer-send-command
   (format "--replace %s"
           (shell-quote-wildcard-pattern
            (if (file-directory-p resource)
                (format "%s/*" resource)
              resource)))
   "playlist_length:")
  (when (and emacspeak-alsaplayer-auditory-feedback
             (ems-interactive-p)
             (eq major-mode 'emacspeak-alsaplayer-mode))
    (emacspeak-auditory-icon 'select-object)
    (emacspeak-speak-line)))

(defun emacspeak-alsaplayer-status ()
  "Show alsaplayer status"
  (interactive)
  (emacspeak-alsaplayer-send-command "--status"
                                     "position:"
                                     'no-refresh)
  (when (ems-interactive-p )
    (unless (eq (current-buffer)
                (get-buffer emacspeak-alsaplayer-buffer))
      (switch-to-buffer emacspeak-alsaplayer-buffer))
    (when  emacspeak-alsaplayer-auditory-feedback
      (emacspeak-auditory-icon 'select-object)
      (emacspeak-speak-line))))
(defvar emacspeak-alsaplayer-paused nil
  "Record if player is paused.")

(defun emacspeak-alsaplayer-pause ()
  "Pause or resume alsaplayer"
  (interactive)
  (declare (special emacspeak-alsaplayer-paused))
  (emacspeak-alsaplayer-send-command "--pause"
                                     "position:")
  (when emacspeak-alsaplayer-paused 
    (emacspeak-alsaplayer-send-command "--speed 1.0"))
  (setq emacspeak-alsaplayer-paused (not emacspeak-alsaplayer-paused))
  (when (and emacspeak-alsaplayer-auditory-feedback
             (ems-interactive-p)
             (eq major-mode 'emacspeak-alsaplayer-mode))
    (emacspeak-auditory-icon 'button)
    (emacspeak-speak-line)))

(defun emacspeak-alsaplayer-next ()
  "Next  alsaplayer"
  (interactive)
  (emacspeak-alsaplayer-send-command "--next"
                                     "\\(title\\|path\\):")
  (when (and emacspeak-alsaplayer-auditory-feedback
             (ems-interactive-p)
             (eq major-mode 'emacspeak-alsaplayer-mode))
    (emacspeak-auditory-icon 'select-object)
    (emacspeak-speak-line)))

(defun emacspeak-alsaplayer-previous ()
  "Previous  alsaplayer"
  (interactive)
  (emacspeak-alsaplayer-send-command "--prev"
                                     "\\(title\\|path\\):")
  (when (and emacspeak-alsaplayer-auditory-feedback
             (ems-interactive-p)
             (eq major-mode 'emacspeak-alsaplayer-mode))
    (emacspeak-auditory-icon 'select-object)
    (emacspeak-speak-line)))

(defun emacspeak-alsaplayer-start ()
  "Start  alsaplayer"
  (interactive)
  (emacspeak-alsaplayer-send-command "--start"
                                     "position:")
  (when (and emacspeak-alsaplayer-auditory-feedback
             (ems-interactive-p)
             (eq major-mode 'emacspeak-alsaplayer-mode))
    (emacspeak-auditory-icon 'open-object)
    (emacspeak-speak-line)))

(defun emacspeak-alsaplayer-stop ()
  "Stop  alsaplayer"
  (interactive)
  (emacspeak-alsaplayer-send-command "--stop"
                                     "position:")
  (when (and emacspeak-alsaplayer-auditory-feedback
             (ems-interactive-p)
             (eq major-mode 'emacspeak-alsaplayer-mode))
    (emacspeak-auditory-icon 'close-object)
    (emacspeak-speak-line)))

(defun emacspeak-alsaplayer-relative (offset)
  "Relative seek  alsaplayer"
  (interactive "sOffset")
  (emacspeak-alsaplayer-send-command
   (format  "--relative %s" offset)
   "position:")
  (when (and emacspeak-alsaplayer-auditory-feedback
             (ems-interactive-p)
             (eq major-mode 'emacspeak-alsaplayer-mode))
    (emacspeak-auditory-icon 'large-movement)
    (emacspeak-speak-line)))

(defun emacspeak-alsaplayer-speed (setting)
  "Set speed in alsaplayer."
  (interactive "sSpeed")
  (emacspeak-alsaplayer-send-command
   (format "--speed %s" setting)
   "speed:")
  (when (and emacspeak-alsaplayer-auditory-feedback
             (ems-interactive-p)
             (eq major-mode 'emacspeak-alsaplayer-mode))
    (emacspeak-auditory-icon 'select-object)
    (emacspeak-speak-line)))

(defun emacspeak-alsaplayer-volume (setting)
  "Set volume."
  (interactive "sVolume")
  (emacspeak-alsaplayer-send-command
   (format "--volume %s" setting)
   "volume:")
  (when (and emacspeak-alsaplayer-auditory-feedback
             (ems-interactive-p)
             (eq major-mode 'emacspeak-alsaplayer-mode))
    (emacspeak-auditory-icon 'select-object)
    (emacspeak-speak-line)))

(defun emacspeak-alsaplayer-seek (offset)
  "Absolute seek  alsaplayer"
  (interactive "sPosition")
  (emacspeak-alsaplayer-send-command
   (format "--seek %s" offset)
   "position:")
  (when (and emacspeak-alsaplayer-auditory-feedback
             (ems-interactive-p)
             (eq major-mode 'emacspeak-alsaplayer-mode))
    (emacspeak-auditory-icon 'large-movement)
    (emacspeak-speak-line)))

(defun emacspeak-alsaplayer-jump (track)
  "Jump to specified track."
  (interactive "sTrack Number:")
  (emacspeak-alsaplayer-send-command
   (format "--jump %s" track)
   "\\(title\\|path\\):")
  (when (and emacspeak-alsaplayer-auditory-feedback
             (ems-interactive-p)
             (eq major-mode 'emacspeak-alsaplayer-mode))
    (emacspeak-auditory-icon 'large-movement)
    (emacspeak-speak-line)))

(defun emacspeak-alsaplayer-clear ()
  "Clear or resume alsaplayer"
  (interactive)
  (emacspeak-alsaplayer-send-command "--clear"
                                     "playlist_length:")
  (when (and emacspeak-alsaplayer-auditory-feedback
             (ems-interactive-p)
             (eq major-mode 'emacspeak-alsaplayer-mode))
    (emacspeak-auditory-icon 'delete-object)
    (emacspeak-speak-line)))

(defun emacspeak-alsaplayer-quit ()
  "Quit  alsaplayer"
  (interactive)
  (let ((kill-buffer-query-functions nil))
    (when (emacspeak-alsaplayer-active-p)
      (emacspeak-alsaplayer-send-command "--quit"))
    (when (eq major-mode 'emacspeak-alsaplayer-mode)
      (kill-buffer (current-buffer))
      (delete-window))
    (when (and emacspeak-alsaplayer-auditory-feedback (ems-interactive-p))
      (emacspeak-auditory-icon 'close-object)
      (emacspeak-speak-mode-line))))

;;;###autoload
(defun emacspeak-alsaplayer-cd (directory)
  "Change default directory, and silence its pronunciation."
  (interactive
   (list
    (read-directory-name "Change to directory: ")))
  (cd directory)
  (save-excursion
    (set-buffer emacspeak-alsaplayer-buffer)
    (emacspeak-pronounce-add-buffer-local-dictionary-entry
     (expand-file-name directory)
     ""))
  (emacspeak-amark-load)
  (emacspeak-auditory-icon 'item))
;;}}}
;;{{{ additional temporal navigation 

(defun emacspeak-alsaplayer-forward-step (seconds)
  "Skip forward by  seconds."
  (interactive "p")
  (emacspeak-alsaplayer-send-command
   (format "--relative %i" (or seconds emacspeak-alsaplayer-rewind-step))
   "position:")
  (when (and emacspeak-alsaplayer-auditory-feedback (ems-interactive-p)
             (eq major-mode 'emacspeak-alsaplayer-mode))
    (emacspeak-speak-line)))

(defun emacspeak-alsaplayer-backward-step (seconds)
  "Skip backward by seconds."
  (interactive "p")
  (emacspeak-alsaplayer-send-command
   (format "--relative -%i" (or seconds emacspeak-alsaplayer-rewind-step))
   "position:")
  (when (and emacspeak-alsaplayer-auditory-feedback (ems-interactive-p)
             (eq major-mode 'emacspeak-alsaplayer-mode))
    (emacspeak-speak-line)))

(defun emacspeak-alsaplayer-forward-minute ( minutes)
  "Skip forward by  minutes."
  (interactive "p")
  (emacspeak-alsaplayer-send-command
   (format "--relative %i" (* 60 (or minutes 1)))
   "position:")
  (when (and emacspeak-alsaplayer-auditory-feedback (ems-interactive-p)
             (eq major-mode 'emacspeak-alsaplayer-mode))
    (emacspeak-speak-line)))

(defun emacspeak-alsaplayer-backward-minute ( minutes)
  "Skip backwards by  minutes."
  (interactive "p")
  (emacspeak-alsaplayer-send-command
   (format "--relative -%i" (* 60 (or minutes 1)))
   "position:")
  (when (and emacspeak-alsaplayer-auditory-feedback (ems-interactive-p)
             (eq major-mode 'emacspeak-alsaplayer-mode))
    (emacspeak-speak-line)))

(defun emacspeak-alsaplayer-forward-ten-minutes ( minutes)
  "Skip forward by  chunks of ten minutes."
  (interactive "p")
  (emacspeak-alsaplayer-send-command
   (format "--relative %i" (* 600 (or minutes 1)))
   "position:")
  (when (and emacspeak-alsaplayer-auditory-feedback (ems-interactive-p)
             (eq major-mode 'emacspeak-alsaplayer-mode))
    (emacspeak-speak-line)))

(defun emacspeak-alsaplayer-backward-ten-minutes ( minutes)
  "Skip backwards by  chunks of minutes."
  (interactive "p")
  (emacspeak-alsaplayer-send-command
   (format "--relative -%i" (* 600 (or minutes 1)))
   "position:")
  (when (and emacspeak-alsaplayer-auditory-feedback (ems-interactive-p)
             (eq major-mode 'emacspeak-alsaplayer-mode))
    (emacspeak-speak-line)))

;;}}}
;;{{{ Helper Accessors:

(defsubst emacspeak-alsaplayer-get-field (field)
  "Return specified field value."
  (declare (special emacspeak-alsaplayer-buffer))
  (save-current-buffer
    (set-buffer emacspeak-alsaplayer-buffer)
    (goto-char (point-min))
    (when (search-forward field  nil t)
      (second
       (split-string
        (buffer-substring-no-properties
         (line-beginning-position)
         (line-end-position))
        ": ")))))

(defsubst emacspeak-alsaplayer-get-position ()
  "Return currently displayed position."
  (emacspeak-alsaplayer-get-field "position:"))

(defsubst emacspeak-alsaplayer-get-playlist-length ()
  "Return playlist length."
  (emacspeak-alsaplayer-get-field "playlist_length:"))

(defsubst emacspeak-alsaplayer-get-path ()
  "Return currently displayed path."
  (emacspeak-alsaplayer-get-field "path:"))

;;}}}
;;{{{  saving positions, marking and clipping:

(defvar emacspeak-alsaplayer-mark nil
  "Saved mark position.")
(make-variable-buffer-local 'emacspeak-alsaplayer-mark)

(defun emacspeak-alsaplayer-mark-position   ()
  "Mark currently played position."
  (interactive)
  (declare (special emacspeak-alsaplayer-mark))
  (emacspeak-alsaplayer-status)
  (setq emacspeak-alsaplayer-mark
        (emacspeak-alsaplayer-get-position))
  (when (and (ems-interactive-p )
             emacspeak-alsaplayer-mark)
    (emacspeak-auditory-icon 'mark-object)
    (message "mark set at %s"
             emacspeak-alsaplayer-mark)))

(defun emacspeak-alsaplayer-where ()
  "Speak current position and copy it to kill ring."
  (interactive)
  (let ((where (emacspeak-alsaplayer-get-position))
        (emacspeak-speak-messages t))
    (when where
      (kill-new where)
      (emacspeak-auditory-icon 'yank-object)
      (message "%s" where))))

(defun emacspeak-alsaplayer-info ()
  "Speak current path and copy it to kill ring."
  (interactive)
  (with-current-buffer emacspeak-alsaplayer-buffer
    (dtk-speak (emacspeak-alsaplayer-header-line))))

(defvar emacspeak-alsaplayer-mp3split-program "mp3splt"
  "Program used to clip mp3 files.")

(defun emacspeak-alsaplayer-clip (path start end)
  "Invoke mp3splt to clip selected range."
  (interactive
   (list
    (let ((completion-ignore-case t)
          (read-file-name-completion-ignore-case t))
      (expand-file-name
       (read-file-name "Path:")))
    (read-minibuffer "Start: " emacspeak-alsaplayer-mark)
    (read-minibuffer "End: ")))
  (cd (file-name-directory path))
  (shell-command
   (format "%s %s %s %s"
           emacspeak-alsaplayer-mp3split-program
           path
           (format "%d.%d"
                   (/ start 60)
                   (% start 60))
           (format "%d.%d"
                   (/ end 60)
                   (% end 60))))
  (when (and emacspeak-alsaplayer-auditory-feedback (ems-interactive-p))
    (emacspeak-auditory-icon 'delete-object)))

(defun emacspeak-alsaplayer-toggle-auditory-feedback ()
  "Toggle emacspeak alsaplayer auditory feedback on or off."
  (interactive)
  (setq emacspeak-alsaplayer-auditory-feedback
        (not emacspeak-alsaplayer-auditory-feedback))
  (emacspeak-auditory-icon (if emacspeak-alsaplayer-auditory-feedback
                               'on 'off)))

;;}}}
;;{{{ AMarks:

;;;###autoload
(defun emacspeak-alsaplayer-amark-add (name &optional prompt-position)
  "Set AMark `name' at current position in current audio stream.
Interactive prefix arg prompts for position.
As the default, use current position."
  (interactive "sAMark Name:\nP")
  (declare  (special emacspeak-alsaplayer-mark))
  (emacspeak-alsaplayer-status)
  (emacspeak-amark-add
   (emacspeak-alsaplayer-get-path)
   name
   (cond
    (prompt-position (read-number "Position: "))
    (t (emacspeak-alsaplayer-get-position))))
  (message "Added Amark %s" name))

;;;###autoload
(defun emacspeak-alsaplayer-amark-jump ()
  "Jump to specified AMark."
  (interactive)
  (unless emacspeak-amark-list
    (error "No amarks are available"))
  (let* ((amark (call-interactively 'emacspeak-amark-find))
         (track
          (if amark
              (expand-file-name (emacspeak-amark-path amark))
            (error "Requested amark does not exist")))
         (length
          (progn
            (emacspeak-alsaplayer-replace-queue (file-name-directory track))
            (string-to-number (emacspeak-alsaplayer-get-playlist-length))))
         (tn 0))
    (while (null (emacspeak-alsaplayer-get-path))
      (emacspeak-alsaplayer-status))
    (while (and (not (string= (expand-file-name (emacspeak-alsaplayer-get-path)) track))
                (< (setq tn (1+ tn)) length))
      (emacspeak-alsaplayer-next))
    (when (= tn length)
      (emacspeak-alsaplayer-replace-queue track))
    (emacspeak-alsaplayer-seek (emacspeak-amark-position amark))))

;;}}}
;;{{{ bind keys

(declaim (special emacspeak-alsaplayer-mode-map))

(loop for k in
      '(
        ("m" emacspeak-alsaplayer-mark-position)
        ("M" emacspeak-alsaplayer-amark-add)
        ("J" emacspeak-alsaplayer-amark-jump)
        ("\M-s" emacspeak-amark-save)
        ("\M-l" emacspeak-amark-load)
        ("w" emacspeak-alsaplayer-where)
        ("x" emacspeak-alsaplayer-clip)
        ("." emacspeak-alsaplayer-forward-step)
        ("i" emacspeak-alsaplayer-info)
        ("," emacspeak-alsaplayer-backward-step)
        (">" emacspeak-alsaplayer-forward-minute)
        ("<" emacspeak-alsaplayer-backward-minute)
        ("]" emacspeak-alsaplayer-forward-ten-minutes)
        ("[" emacspeak-alsaplayer-backward-ten-minutes)
        ("a" emacspeak-alsaplayer-add-to-queue)
        ("d" emacspeak-alsaplayer-cd)
        ("f" emacspeak-alsaplayer-find-and-add-to-queue)
        ("A"
         emacspeak-alsaplayer-replace-queue)
        ("c"
         emacspeak-alsaplayer-clear)
        ("g"
         emacspeak-alsaplayer-seek)
        ("j" emacspeak-alsaplayer-jump)
        (" "
         emacspeak-alsaplayer-pause)
        ("n"
         emacspeak-alsaplayer-next)
        ("p"
         emacspeak-alsaplayer-previous)
        ("q" bury-buffer)
        ("Q" emacspeak-alsaplayer-quit)
        ("o" other-window)
        ("r" emacspeak-alsaplayer-relative)
        ("s"
         emacspeak-alsaplayer-start)
        ("S"
         emacspeak-alsaplayer-stop)
        ("t" emacspeak-alsaplayer-toggle-auditory-feedback)
        ("/" emacspeak-alsaplayer-speed)
        ("?"
         emacspeak-alsaplayer-status)
        ("v" emacspeak-alsaplayer-volume)
        ("l" emacspeak-alsaplayer-launch)
        )
      do
      (emacspeak-keymap-update  emacspeak-alsaplayer-mode-map k))

;;}}}
;;{{{ pause/resume if needed

;;;###autoload
(defun emacspeak-alsaplayer-pause-or-resume ()
  "Pause/resume if alsaplayer is running. For use  in
emacspeak-silence-hook."
  (declare (special emacspeak-alsaplayer-buffer))
  (when (and (get-buffer-process emacspeak-alsaplayer-buffer)
             (eq 'run (process-status (get-buffer-process emacspeak-alsaplayer-buffer))))
    (emacspeak-alsaplayer-pause)))

(add-hook 'emacspeak-silence-hook 'emacspeak-alsaplayer-pause-or-resume)

;;}}}
(provide 'emacspeak-alsaplayer)
;;{{{ end of file 

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: nil
;;; end: 

;;}}}

