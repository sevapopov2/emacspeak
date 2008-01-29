;;; emacspeak-alsaplayer.el --- Control alsaplayer from Emacs
;;; $Id: emacspeak-alsaplayer.el 5339 2007-10-11 04:55:04Z tv.raman.tv $
;;; $Author: tv.raman.tv $
;;; Description: Controlling alsaplayer from emacs 
;;; Keywords: Emacspeak, alsaplayer
;;{{{  LCD Archive entry: 

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |raman@cs.cornell.edu 
;;; A speech interface to Emacs |
;;; $Date: 2007-10-10 21:55:04 -0700 (Wed, 10 Oct 2007) $ |
;;;  $Revision: 4532 $ | 
;;; Location undetermined
;;;

;;}}}
;;{{{  Copyright:

;;; Copyright (c) 1995 -- 2007, T. V. Raman
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

(require 'emacspeak-preamble)
;;}}}
;;{{{ define a derived mode for alsaplayer interaction

(defvar emacspeak-alsaplayer-process nil
  "Process handle to alsaplayer." )

;;;###autoload
(define-prefix-command 'emacspeak-alsaplayer-prefix-command
  'emacspeak-alsaplayer-mode-map)
(defun emacspeak-alsaplayer-header-line ()
  "Return information suitable for header line."
  (substring
   (shell-command-to-string "alsaplayer --status | grep title:")
   0 -1))

(define-derived-mode emacspeak-alsaplayer-mode fundamental-mode 
  "Alsaplayer Interaction"
  "Major mode for alsaplayer interaction. \n\n
\\{emacspeak-alsaplayer-mode-map}"
  (setq header-line-format '((:eval (emacspeak-alsaplayer-header-line)))))

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

(defcustom emacspeak-alsaplayer-rewind-step 2
  "Forward or backward rewind step in seconds."
  :type 'integer
  :group 'emacspeak-alsaplayer)

(defcustom emacspeak-alsaplayer-height 1
  "Height of alsaplayer window."
  :type 'number
  :group 'emacspeak-alsaplayer)

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

(defcustom emacspeak-alsaplayer-sound-device nil
  "Alsaplayer sound device.
Default is hw:0,0 for ALSA and /dev/dsp for OSS output."
  :type '(choice (const :tag "default" nil)
		 (string :tag "Device specification"))
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

(defcustom emacspeak-alsaplayer-supported-files
  "\\.\\([Mm][Pp][23]\\|[Oo][Gg][Gg]\\|[Ww][Aa][Vv]\\)$"
  "Regexp matching supported file names."
  :type 'regexp
  :group 'emacspeak-alsaplayer)

(defvar emacspeak-alsaplayer-buffer "*alsaplayer*"
  "Buffer for alsaplayer interaction.")

;;;###autoload
(defun emacspeak-alsaplayer-launch ()
  "Launch Alsaplayer.
user is placed in a buffer associated with the newly created
Alsaplayer session."
  (interactive)
  (declare (special emacspeak-alsaplayer-program
                    emacspeak-alsaplayer-process
                    emacspeak-alsaplayer-buffer
		    emacspeak-alsaplayer-height))
  (if (and emacspeak-alsaplayer-process
           (eq 'run (process-status
                     emacspeak-alsaplayer-process)))
      (when (y-or-n-p "Stop currently playing music? ")
        (emacspeak-alsaplayer-quit)
        (setq emacspeak-alsaplayer-process nil))
    (setq emacspeak-alsaplayer-process nil))
  (let ((deactivate-mark nil)
        (process-connection-type t)
        (coding-system-for-read emacspeak-alsaplayer-coding-system)
        (options (nconc (list "-r" "-i" "daemon")
                        (when emacspeak-alsaplayer-output
                          (list "-o" emacspeak-alsaplayer-output))
                        (when emacspeak-alsaplayer-sound-device
                          (list "-d" emacspeak-alsaplayer-sound-device))))
        (buffer (get-buffer-create emacspeak-alsaplayer-buffer)))
    (save-current-buffer
      (set-buffer buffer)
      (setq buffer-undo-list t)
      (emacspeak-alsaplayer-mode)
      (unless emacspeak-alsaplayer-process
        (setq emacspeak-alsaplayer-process
              (apply 'start-process
                     "alsaplayer"
                     (current-buffer)
                     emacspeak-alsaplayer-program
                     options))
        (accept-process-output emacspeak-alsaplayer-process)
        (erase-buffer)
        (call-process emacspeak-alsaplayer-program
                      nil t t
                      "--status")
        (goto-char (point-min))
        (when (search-forward "path: " nil t)
          (encode-coding-region (point) (line-end-position)
                                emacspeak-alsaplayer-coding-system)
          (decode-coding-region (point) (line-end-position)
                                (car default-process-coding-system)))))
    (pop-to-buffer buffer 'other-window)
    (set-window-text-height nil emacspeak-alsaplayer-height)
    (when (and emacspeak-alsaplayer-auditory-feedback (interactive-p))
      (emacspeak-auditory-icon 'open-object)
      (emacspeak-speak-mode-line))))

;;}}}
;;{{{  Invoke commands:

(defun emacspeak-alsaplayer-send-command(command &optional watch-pattern no-refresh)
  "Send command to Alsaplayer.
Optional second arg watch-pattern specifies line of output to
  focus on.  Optional third arg no-refresh is used to avoid
  getting status twice."
  (declare (special emacspeak-alsaplayer-program
                    emacspeak-alsaplayer-buffer))
  (save-current-buffer
    (let ((deactivate-mark nil)
          (coding-system-for-read emacspeak-alsaplayer-coding-system))
      (set-buffer (get-buffer-create emacspeak-alsaplayer-buffer))
      (erase-buffer)
      (apply 'call-process
             emacspeak-alsaplayer-program
             nil t t
             (if (listp command)
                 command
               (list command)))
      (unless no-refresh
        (call-process
         emacspeak-alsaplayer-program
         nil t t
         "--status"))
      (goto-char (point-min))
      (when (search-forward "path: " nil t)
        (encode-coding-region (point) (line-end-position)
                              emacspeak-alsaplayer-coding-system)
        (decode-coding-region (point) (line-end-position)
                              (car default-process-coding-system)))
      (when watch-pattern
        (goto-char (point-min))
        (re-search-forward watch-pattern  nil t)))))

(defun emacspeak-alsaplayer-add-to-queue (resource)
  "Add specified resource to queue."
  (interactive
   (list
    (let ((completion-ignore-case t)
          (read-file-name-completion-ignore-case t))
      (expand-file-name
       (read-file-name "Media Resource: "
                       (if 
                           (string-match "mp3" (expand-file-name default-directory))
                           default-directory
                         emacspeak-alsaplayer-media-directory))))))
  (emacspeak-alsaplayer-send-command
   (nconc (list "--enqueue")
          (if (file-directory-p resource)
              (directory-files
               (expand-file-name resource)
               'full
               emacspeak-alsaplayer-supported-files)
            (expand-file-name resource)))
   "playlist_length:")
  (when (and emacspeak-alsaplayer-auditory-feedback
             (interactive-p))
    (emacspeak-auditory-icon 'select-object)
    (when (eq major-mode 'emacspeak-alsaplayer-mode)
      (emacspeak-speak-line))))

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
             (interactive-p))
    (emacspeak-speak-line)
    (emacspeak-auditory-icon 'select-object)))

(defun emacspeak-alsaplayer-replace-queue (resource)
  "Replace currently playing music."
  (interactive
   (list
    (let ((completion-ignore-case t)
          (read-file-name-completion-ignore-case t))
      (read-file-name "New Media Resource: "
                      (if 
                          (string-match (format "^%s"
                                                emacspeak-alsaplayer-media-directory)
                                        (expand-file-name default-directory))
                          default-directory
                        emacspeak-alsaplayer-media-directory)))))
  (emacspeak-alsaplayer-send-command
   (nconc (list "--replace")
          (if (file-directory-p resource)
              (directory-files
               (expand-file-name resource)
               'full
               emacspeak-alsaplayer-supported-files)
            (expand-file-name resource)))
   "playlist_length:")
  (when (and emacspeak-alsaplayer-auditory-feedback
             (interactive-p))
    (emacspeak-auditory-icon 'select-object)
    (when (eq major-mode 'emacspeak-alsaplayer-mode)
      (emacspeak-speak-line))))

(defun emacspeak-alsaplayer-status ()
  "Show alsaplayer status"
  (interactive)
  (emacspeak-alsaplayer-send-command "--status"
                                     "position:"
                                     'no-refresh)
  (when (interactive-p)
    (unless (eq (current-buffer)
                (get-buffer emacspeak-alsaplayer-buffer))
      (switch-to-buffer emacspeak-alsaplayer-buffer))
    (when  emacspeak-alsaplayer-auditory-feedback
      (emacspeak-auditory-icon 'select-object)
      (emacspeak-speak-line))))

(defun emacspeak-alsaplayer-pause ()
  "Pause or resume alsaplayer"
  (interactive)
  (emacspeak-alsaplayer-send-command "--pause"
                                     "position:")
  (when (and emacspeak-alsaplayer-auditory-feedback
             (interactive-p))
    (emacspeak-auditory-icon 'button)
    (when (eq major-mode 'emacspeak-alsaplayer-mode)
      (emacspeak-speak-line))))

(defun emacspeak-alsaplayer-next ()
  "Next  alsaplayer"
  (interactive)
  (emacspeak-alsaplayer-send-command "--next"
                                     "\\(title\\|path\\):")
  (when (and emacspeak-alsaplayer-auditory-feedback
             (interactive-p))
    (emacspeak-auditory-icon 'select-object)
    (when (eq major-mode 'emacspeak-alsaplayer-mode)
      (emacspeak-speak-line))))

(defun emacspeak-alsaplayer-previous ()
  "Previous  alsaplayer"
  (interactive)
  (emacspeak-alsaplayer-send-command "--prev"
                                     "\\(title\\|path\\):")
  (when (and emacspeak-alsaplayer-auditory-feedback
             (interactive-p))
    (emacspeak-auditory-icon 'select-object)
    (when (eq major-mode 'emacspeak-alsaplayer-mode)
      (emacspeak-speak-line))))

(defun emacspeak-alsaplayer-start ()
  "Start  alsaplayer"
  (interactive)
  (emacspeak-alsaplayer-send-command "--start"
                                     "position:")
  (when (and emacspeak-alsaplayer-auditory-feedback
             (interactive-p))
    (emacspeak-auditory-icon 'open-object)
    (when (eq major-mode 'emacspeak-alsaplayer-mode)
      (emacspeak-speak-line))))

(defun emacspeak-alsaplayer-stop ()
  "Stop  alsaplayer"
  (interactive)
  (emacspeak-alsaplayer-send-command "--stop"
                                     "position:")
  (when (and emacspeak-alsaplayer-auditory-feedback
             (interactive-p))
    (emacspeak-auditory-icon 'close-object)
    (when (eq major-mode 'emacspeak-alsaplayer-mode)
      (emacspeak-speak-line))))

(defun emacspeak-alsaplayer-relative (offset)
  "Relative seek  alsaplayer"
  (interactive "sOffset")
  (emacspeak-alsaplayer-send-command
   (list "--relative" offset)
   "position:")
  (when (and emacspeak-alsaplayer-auditory-feedback
             (interactive-p))
    (emacspeak-auditory-icon 'large-movement)
    (when (eq major-mode 'emacspeak-alsaplayer-mode)
      (emacspeak-speak-line))))

(defun emacspeak-alsaplayer-speed (setting)
  "Set speed in alsaplayer."
  (interactive "sSpeed")
  (emacspeak-alsaplayer-send-command
   (list "--speed" setting)
   "speed:")
  (when (and emacspeak-alsaplayer-auditory-feedback
             (interactive-p))
    (emacspeak-auditory-icon 'select-object)
    (when (eq major-mode 'emacspeak-alsaplayer-mode)
      (emacspeak-speak-line))))

(defun emacspeak-alsaplayer-volume (setting)
  "Set volume."
  (interactive "sVolume")
  (emacspeak-alsaplayer-send-command
   (list "--volume" setting)
   "volume:")
  (when (and emacspeak-alsaplayer-auditory-feedback
             (interactive-p))
    (emacspeak-auditory-icon 'select-object)
    (when (eq major-mode 'emacspeak-alsaplayer-mode)
      (emacspeak-speak-line))))

(defun emacspeak-alsaplayer-seek (offset)
  "Absolute seek  alsaplayer"
  (interactive "sPosition")
  (emacspeak-alsaplayer-send-command
   (list "--seek" offset)
   "position:")
  (when (and emacspeak-alsaplayer-auditory-feedback
             (interactive-p))
    (emacspeak-auditory-icon 'large-movement)
    (when (eq major-mode 'emacspeak-alsaplayer-mode)
      (emacspeak-speak-line))))

(defun emacspeak-alsaplayer-jump (track)
  "Jump to specified track."
  (interactive "sTrack Number:")
  (emacspeak-alsaplayer-send-command
   (list "--jump" track)
   "\\(title\\|path\\):")
  (when (and emacspeak-alsaplayer-auditory-feedback
             (interactive-p))
    (emacspeak-auditory-icon 'large-movement)
    (when (eq major-mode 'emacspeak-alsaplayer-mode)
      (emacspeak-speak-line))))

(defun emacspeak-alsaplayer-clear ()
  "Clear or resume alsaplayer"
  (interactive)
  (emacspeak-alsaplayer-send-command "--clear"
                                     "playlist_length:")
  (when (and emacspeak-alsaplayer-auditory-feedback
             (interactive-p))
    (emacspeak-auditory-icon 'delete-object)
    (when (eq major-mode 'emacspeak-alsaplayer-mode)
      (emacspeak-speak-line))))

(defun emacspeak-alsaplayer-quit ()
  "Quit  alsaplayer"
  (interactive)
  (emacspeak-alsaplayer-send-command "--quit" nil t)
  (when (eq major-mode 'emacspeak-alsaplayer-mode)
    (kill-buffer (current-buffer))
    (delete-window))
  (when (and emacspeak-alsaplayer-auditory-feedback (interactive-p))
    (emacspeak-speak-mode-line)
    (emacspeak-auditory-icon 'close-object)))

;;}}}
;;{{{ additional temporal navigation 

(defun emacspeak-alsaplayer-forward-step (seconds)
  "Skip forward by  seconds."
  (interactive "p")
  (emacspeak-alsaplayer-send-command
   (list "--relative"
	 (format "%i" (or seconds emacspeak-alsaplayer-rewind-step)))
   "position:")
  (when (and emacspeak-alsaplayer-auditory-feedback (interactive-p))
    (when (eq major-mode 'emacspeak-alsaplayer-mode)
      (emacspeak-speak-line))))

(defun emacspeak-alsaplayer-backward-step (seconds)
  "Skip backward by seconds."
  (interactive "p")
  (emacspeak-alsaplayer-send-command
   (list "--relative"
	 (format "-%i" (or seconds emacspeak-alsaplayer-rewind-step)))
   "position:")
  (when (and emacspeak-alsaplayer-auditory-feedback (interactive-p))
    (when (eq major-mode 'emacspeak-alsaplayer-mode)
      (emacspeak-speak-line))))

(defun emacspeak-alsaplayer-forward-minute ( minutes)
  "Skip forward by  minutes."
  (interactive "p")
  (emacspeak-alsaplayer-send-command
   (list "--relative"
	 (format "%i" (* 60 (or minutes 1))))
   "position:")
  (when (and emacspeak-alsaplayer-auditory-feedback (interactive-p))
    (when (eq major-mode 'emacspeak-alsaplayer-mode)
      (emacspeak-speak-line))))

(defun emacspeak-alsaplayer-backward-minute ( minutes)
  "Skip backwards by  minutes."
  (interactive "p")
  (emacspeak-alsaplayer-send-command
   (list
    "--relative"
    (format "-%i" (* 60 (or minutes 1))))
   "position:")
  (when (and emacspeak-alsaplayer-auditory-feedback (interactive-p))
    (when (eq major-mode 'emacspeak-alsaplayer-mode)
      (emacspeak-speak-line))))

(defun emacspeak-alsaplayer-forward-ten-minutes ( minutes)
  "Skip forward by  chunks of ten minutes."
  (interactive "p")
  (emacspeak-alsaplayer-send-command
   (list
    "--relative"
    (format "%i" (* 600 (or minutes 1))))
   "position:")
  (when (and emacspeak-alsaplayer-auditory-feedback (interactive-p))
    (when (eq major-mode 'emacspeak-alsaplayer-mode)
      (emacspeak-speak-line))))

(defun emacspeak-alsaplayer-backward-ten-minutes ( minutes)
  "Skip backwards by  chunks of minutes."
  (interactive "p")
  (emacspeak-alsaplayer-send-command
   (list
    "--relative"
    (format "-%i" (* 600 (or minutes 1))))
   "position:")
  (when (and emacspeak-alsaplayer-auditory-feedback (interactive-p))
    (when (eq major-mode 'emacspeak-alsaplayer-mode)
      (emacspeak-speak-line))))

;;}}}
;;{{{  saving positions, marking and clipping:

(defvar emacspeak-alsaplayer-mark nil
  "Saved mark position.")

(defsubst emacspeak-alsaplayer-get-position ()
  "Return currently displayed position."
  (declare (special emacspeak-alsaplayer-buffer))
  (emacspeak-alsaplayer-status)
  (save-excursion
    (let ((deactivate-mark nil))
      (set-buffer emacspeak-alsaplayer-buffer)
      (goto-char (point-min))
      (when (search-forward "position:" nil t)
        (second
         (split-string
          (buffer-substring-no-properties
           (line-beginning-position)
           (line-end-position))
          ": "))))))

(defun emacspeak-alsaplayer-mark-position   ()
  "Mark currently played position."
  (interactive)
  (declare (special emacspeak-alsaplayer-mark))e
  (setq emacspeak-alsaplayer-mark
        (emacspeak-alsaplayer-get-position))
  (when (and (interactive-p)
             emacspeak-alsaplayer-mark)
    (message "mark set at %s"
             emacspeak-alsaplayer-mark)
    (when emacspeak-alsaplayer-auditory-feedback
      (emacspeak-auditory-icon 'mark-object))))

(defun emacspeak-alsaplayer-where ()
  "Speak current position and copy it to kill ring."
  (interactive)
  (let ((where (emacspeak-alsaplayer-get-position))
        (emacspeak-speak-messages emacspeak-alsaplayer-auditory-feedback))
    (when where
      (kill-new where)
      (when emacspeak-alsaplayer-auditory-feedback
        (emacspeak-auditory-icon 'yank-object))
      (message "%s" where))))

(defsubst emacspeak-alsaplayer-get-path ()
  "Return currently displayed path."
  (declare (special emacspeak-alsaplayer-buffer))
  (emacspeak-alsaplayer-status)
  (save-excursion
    (let ((deactivate-mark nil))
      (set-buffer emacspeak-alsaplayer-buffer)
      (goto-char (point-min))
      (when (search-forward "path:" nil t)
        (second
         (split-string
          (buffer-substring-no-properties
           (line-beginning-position)
           (line-end-position))
          ": "))))))

(defun emacspeak-alsaplayer-info ()
  "Speak current path and copy it to kill ring."
  (interactive)
  (let ((path (emacspeak-alsaplayer-get-path))
        (emacspeak-speak-messages emacspeak-alsaplayer-auditory-feedback))
    (when path
      (kill-new path)
      (when emacspeak-alsaplayer-auditory-feedback
        (emacspeak-auditory-icon 'yank-object))
      (message "%s" path))))

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
  (when (and emacspeak-alsaplayer-auditory-feedback (interactive-p))
    (emacspeak-auditory-icon 'delete-object)))

;;}}}
;;{{{ bind keys

(declaim (special emacspeak-alsaplayer-mode-map))

(loop for k in
      '(
        ("m" emacspeak-alsaplayer-mark-position)
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
        ("d" cd)
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
        ("q" emacspeak-alsaplayer-quit)
	("o" other-window)
        ("r" emacspeak-alsaplayer-relative)
        ("s"
         emacspeak-alsaplayer-start)
        ("S"
         emacspeak-alsaplayer-stop)
        ("/" emacspeak-alsaplayer-speed)
        ("?"
         emacspeak-alsaplayer-status)
        ("v" emacspeak-alsaplayer-volume)
        ("l" emacspeak-alsaplayer-launch)
        )
      do
      (emacspeak-keymap-update  emacspeak-alsaplayer-mode-map k))

;;}}}
(provide 'emacspeak-alsaplayer)
;;{{{ end of file 

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end: 

;;}}}
 
