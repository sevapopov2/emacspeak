;;;$Id: tools.jl 4047 2006-08-11 19:11:17Z tv.raman.tv $
;;; tools.jl --- Emacs tool for sawfish
;;; $Author: tv.raman.tv $
;;; Description:   Commands for launching or switching to
;;; a running Emacs and some other useful stuff
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
;;; Tool for launching Emacs.

;;}}}
(require 'tts)
;;; Set this to the executable you wish to run  via command `emacs'
(defcustom emacs-program "emacs"
  "Emacs executable to run.")
(defcustom emacs-args "-i"
  "Emacs arguments to pass through command line.")

(defcustom xemacs-program "xemacs"
  "XEmacs executable to run.")
(defcustom xemacs-args "-i"
  "XEmacs arguments to pass through command line.")


(defun launch-or-switch-to  (program #!optional args #!key class)
  "Launch specified program or switch to it if it is already running."
  (let ((w (car
            (delete-if-not
             (lambda (x)
               (string-equal (window-class x) (or class program)))
             (managed-windows)))))
    (if w
	(display-window w)
      (system (format nil "%s %s &" program (or args ""))))))


;;; Interactive command to start emacs or switch to an
;;; existing session.

(defun emacs  ()
  "Switch to a running emacs or start one if necessary."
  (interactive)
  (launch-or-switch-to emacs-program emacs-args #:class "emacs"))

(defun xemacs  ()
  "Switch to a running xemacs or start one if necessary."
  (interactive)
  (launch-or-switch-to xemacs-program xemacs-args #:class "xemacs"))


;;; Interactive command to start Gnopernicus or switch to an
;;; existing one.

(defun gnopernicus  ()
  "Switch to a running Gnopernicus or start one if necessary."
  (interactive)
  (launch-or-switch-to "gnopernicus"))


(defun delete-this-window-safely ()
  "Delete current window safely."
  (interactive)
  (let ((current-window (input-focus)))
    (when current-window
      (delete-window-safely current-window))))

(message "Loaded tools.jl")

;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}

