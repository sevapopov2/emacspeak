;;; emacspeak-wiki.el --- speech-enable emacs-wiki
;;; Description:  Emacspeak extension to speech-enable emacs-wiki package
;;; Keywords: Emacspeak, Emacs wiki, html publishing
;;{{{  LCD Archive entry:

;;}}}
;;{{{  Copyright:

;;; Initial version: Author: Igor B. Poretsky <master@goga.energo.ru>
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

;;{{{ required modules

(require 'emacspeak-preamble)
;;}}}
;;{{{  Introduction:

;;; Commentary:

;;; This module speech-enables emacs-wiki package
;;; that allows to construct hypertext documents
;;; and publish it as html pages.

;;}}}
;;{{{ Advice interactive commands to speak.

(defadvice emacs-wiki-find-file (around emacspeak pre act comp)
  "Produce an auditory icon if possible."
  (if (not (interactive-p))
      ad-do-it
    (ad-set-arg 0 (car (ad-get-arg 0)))
    ad-do-it
    (emacspeak-auditory-icon 'open-object)
    (emacspeak-speak-mode-line))
  ad-return-value)

(defadvice emacs-wiki-follow-name-at-point (after emacspeak pre act comp)
  "Produce an auditory icon if possible."
  (when (interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (emacspeak-speak-mode-line)))

(defadvice emacs-wiki-index (after emacspeak pre act comp)
  "Produce an auditory icon if possible."
  (when (interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (emacspeak-speak-mode-line)))

(defadvice emacs-wiki-next-reference (after emacspeak pre act comp)
  "Produce an auditory icon if possible."
  (when (interactive-p)
    (emacspeak-auditory-icon 'large-movement)
    (emacspeak-speak-text-range 'keymap)))

(defadvice emacs-wiki-previous-reference (after emacspeak pre act comp)
  "Produce an auditory icon if possible."
  (when (interactive-p)
    (emacspeak-auditory-icon 'large-movement)
    (emacspeak-speak-text-range 'keymap)))

(defadvice emacs-wiki-publish (after emacspeak pre act comp)
  "Produce an auditory icon if possible."
  (when (interactive-p)
    (emacspeak-auditory-icon 'save-object)))

;;}}}
;;{{{ mapping font faces to personalities 

(def-voice-font  emacspeak-wiki-link-personality
  voice-bolden
  'emacs-wiki-link-face
  "emacs-wiki-link-face")

(def-voice-font  emacspeak-wiki-bad-link-personality
  voice-lighten
  'emacs-wiki-bad-link-face
  "emacs-wiki-bad-link-face")

(def-voice-font  emacspeak-wiki-header-1-personality
  voice-brighten
  'emacs-wiki-header-1-face
  "emacs-wiki-header-1-face")

(def-voice-font  emacspeak-wiki-header-2-personality
  voice-brighten
  'emacs-wiki-header-2-face
  "emacs-wiki-header-2-face")

(def-voice-font  emacspeak-wiki-header-3-personality
  voice-brighten
  'emacs-wiki-header-3-face
  "emacs-wiki-header-3-face")

(def-voice-font  emacspeak-wiki-header-4-personality
  voice-brighten
  'emacs-wiki-header-4-face
  "emacs-wiki-header-4-face")

(def-voice-font  emacspeak-wiki-header-5-personality
  voice-brighten
  'emacs-wiki-header-5-face
  "emacs-wiki-header-5-face")

(def-voice-font  emacspeak-wiki-header-6-personality
  voice-brighten
  'emacs-wiki-header-6-face
  "emacs-wiki-header-6-face")

;;}}}
(provide 'emacspeak-wiki)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}
