;;; emacspeak-windmove.el --- windmove  -*- lexical-binding: t; -*- 
;;
;; $Author: tv.raman.tv $
;; Description:  Emacspeak front-end for WINDMOVE
;; Keywords: Emacspeak, windmove
;;;   LCD Archive entry:

;; LCD Archive Entry:
;; emacspeak| T. V. Raman |tv.raman.tv@gmail.com
;; A speech interface to Emacs |
;; 
;;  $Revision: 4074 $ |
;; Location undetermined
;; 

;;;   Copyright:

;; Copyright (C) 1995 -- 2022, T. V. Raman
;; All Rights Reserved.
;; 
;; This file is not part of GNU Emacs, but the same permissions apply.
;; 
;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;; 
;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; Commentary:

;; Package  windmove (bundled with Emacs 21)
;; provides commands for navigating to windows based on
;; relative position.
;; 

;;;  required modules

;;; Code:
(eval-when-compile (require 'cl-lib))
(cl-declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)

;;;  advice window navigation

(cl-loop
 for f in
 '(windmove-left windmove-right windmove-up windmove-down)
 do
 (eval
  `(defadvice  ,f (after emacspeak pre act comp)
     "speak."
     (when (ems-interactive-p)
       (emacspeak-auditory-icon select-object)
       (emacspeak-speak-mode-line)))))

(provide 'emacspeak-windmove)
;;;  end of file

