;;; emacspeak-python.el --- Speech enable Python development environment
;;; $Id$
;;; $Author: tv.raman.tv $
;;; Description: Auditory interface to python mode
;;; Keywords: Emacspeak, Speak, Spoken Output, python
;;{{{  LCD Archive entry:

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |raman@cs.cornell.edu
;;; A speech interface to Emacs |
;;; $Date: 2007-08-25 18:28:19 -0700 (Sat, 25 Aug 2007) $ |
;;;  $Revision: 4532 $ |
;;; Location undetermined
;;;

;;}}}
;;{{{  Copyright:

;;; Copyright (c) 1995 -- 2015, T. V. Raman
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

;;{{{ Introduction

;;; Commentary:

;;; This speech-enables python-mode available on sourceforge and ELPA

;;; Code:

;;}}}
;;{{{  Required modules
(require 'cl)
(require 'emacspeak-preamble)
(eval-when-compile
  (require 'python-mode "python-mode" 'no-error))
;;}}}
;;{{{ Forward declarations

(declare-function python-beginning-of-defun "python.el" ())
(declare-function python-end-of-defun "python.el" ())

;;}}}
;;{{{  electric editing

(unless (and (boundp 'post-self-insert-hook)
             post-self-insert-hook
             (memq 'emacspeak-post-self-insert-hook post-self-insert-hook))
  (defadvice py-electric-comment (after emacspeak pre act comp)
    "Speak what you inserted"
    (when (ems-interactive-p)
      (dtk-say " pound "))))

(unless (and (boundp 'post-self-insert-hook)
             post-self-insert-hook
             (memq 'emacspeak-post-self-insert-hook post-self-insert-hook))
  (defadvice py-electric-colon (after emacspeak pre act comp)
    "Speak what you inserted")
  (when (ems-interactive-p)
    (dtk-say " colon ")))

(defadvice py-electric-backspace (around emacspeak pre act)
  "Speak character you're deleting.
Provide contextual feedback when closing blocks"
  (cond
   ((ems-interactive-p)
    (let ((ws (= (char-syntax (preceding-char)) 32)))
      (dtk-tone 500 30 'force)
      (unless ws (emacspeak-speak-this-char (preceding-char)))
      ad-do-it
      (when ws
        (dtk-notify-speak  (format "Indent %s "ad-return-value))
        (emacspeak-auditory-icon  'close-object)
        (sit-for 0.2)
        (save-excursion
          (py-beginning-of-block)
          (emacspeak-speak-line)))))
   (t ad-do-it))
  ad-return-value)

(defadvice py-electric-delete (around emacspeak pre act)
  "Speak character you're deleting."
  (cond
   ((ems-interactive-p)
    (dtk-tone 500 30 'force)
    (emacspeak-speak-this-char (preceding-char))
    ad-do-it)
   (t ad-do-it))
  ad-return-value)

;;}}}
;;{{{ interactive programming

(defadvice py-shell (after emacspeak pre act comp)
  "Provide auditory feedback"
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (emacspeak-speak-mode-line)))

(defadvice py-clear-queue (after emacspeak pre act comp)
  "Provide auditory feedback"
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'task-done)))

(defadvice py-execute-region (after emacspeak pre act comp)
  "Provide auditory feedback"
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'task-done)))

(defadvice py-execute-buffer (after emacspeak pre act comp)
  "Provide auditory feedback"
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'task-done)))

(defadvice py-goto-exception(after emacspeak pre act comp)
  "Speak line you moved to"
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'large-movement)
    (emacspeak-speak-line)))

(defadvice py-down-exception(after emacspeak pre act comp)
  "Speak line you moved to"
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'large-movement)
    (emacspeak-speak-line)))

(defadvice py-up-exception(after emacspeak pre act comp)
  "Speak line you moved to"
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'large-movement)
    (emacspeak-speak-line)))

;;}}}
;;{{{  whitespace management and indentation

(loop
 for f in
 (list 'py-fill-paragraph 'py-fill-comment 'py-fill-string)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "Provide auditory feedback."
     (when (ems-interactive-p)
       (emacspeak-auditory-icon 'fill-object)))))

(defadvice py-newline-and-indent(after emacspeak pre act comp)
  "Speak line so we know current indentation"
  (when (ems-interactive-p)
    (dtk-speak-using-voice voice-annotate
                           (format
                            "indent %s"
                            (current-column)))
    (dtk-force)))

(defadvice py-shift-region-left (after emacspeak pre act comp)
  "Speak number of lines that were shifted"
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'large-movement)
    (dtk-speak
     (format "Left shifted block  containing %s lines"
             (count-lines  (region-beginning)
                           (region-end))))))

(defadvice py-shift-region-right (after emacspeak pre act comp)
  "Speak number of lines that were shifted"
  (when (ems-interactive-p)
    (dtk-speak
     (format "Right shifted block  containing %s lines"
             (count-lines  (region-beginning)
                           (region-end))))))

(defadvice py-indent-region (after emacspeak pre act comp)
  "Speak number of lines that were shifted"
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'large-movement)
    (dtk-speak
     (format "Indented region   containing %s lines"
             (count-lines  (region-beginning)
                           (region-end))))))

(defadvice py-comment-region (after emacspeak pre act comp)
  "Speak number of lines that were shifted"
  (when (ems-interactive-p)
    (dtk-speak
     (format "Commented  block  containing %s lines"
             (count-lines  (region-beginning)
                           (region-end))))))

;;}}}
;;{{{  buffer navigation
(loop
 for f in
 '(
   py-goto-block-or-clause-up py-goto-clause-up
                              py-previous-class py-previous-clause py-previous-def-or-class
                              py-forward-block
                              py-forward-block-bol
                              py-forward-block-or-clause
                              py-forward-block-or-clause-bol
                              py-forward-buffer
                              py-forward-class
                              py-forward-class-bol
                              py-forward-clause
                              py-forward-clause-bol
                              py-forward-comment
                              py-forward-decorator
                              py-forward-def-bol
                              py-forward-def-or-class-bol
                              py-forward-elif-block
                              py-forward-elif-block-bol
                              py-forward-else-block
                              py-forward-else-block-bol
                              py-forward-except-block
                              py-forward-except-block-bol
                              py-forward-expression
                              py-forward-for-block
                              py-forward-for-block-bol
                              py-forward-function
                              py-forward-if-block
                              py-forward-if-block-bol
                              py-forward-line
                              py-forward-minor-block
                              py-forward-minor-block-bol
                              py-forward-paragraph
                              py-forward-partial-expression
                              py-forward-section
                              py-forward-statement-bol
                              py-forward-statements
                              py-forward-top-level
                              py-forward-top-level-bol
                              py-forward-try-block
                              py-forward-try-block-bol
                              py-backward-block py-backward-block-bol
                              py-backward-block-or-clause py-backward-block-or-clause-bol
                              py-backward-class py-backward-class-bol
                              py-backward-clause py-backward-clause-bol
                              py-backward-comment py-backward-decorator py-backward-decorator-bol
                              py-backward-def-bol py-backward-def-or-class-bol
                              py-backward-elif-block py-backward-elif-block-bol
                              py-backward-else-block py-backward-else-block-bol
                              py-backward-except-block py-backward-except-block-bol
                              py-backward-expression py-backward-for-block py-backward-for-block-bol
                              py-backward-function py-backward-if-block py-backward-if-block-bol
                              py-backward-line py-backward-minor-block py-backward-minor-block-bol
                              py-backward-paragraph py-backward-partial-expression py-backward-same-level
                              py-backward-section py-backward-statement-bol py-backward-statements
                              py-backward-top-level py-backward-top-level-p
                              py-backward-try-block py-backward-try-block-bol
                              py-match-paren py-indent-or-complete
                              py-beginning py-beginning-of-block-bol
                              py-beginning-of-block-current-column
                              py-beginning-of-block-or-clause py-beginning-of-class
                              py-beginning-of-class-bol
                              py-beginning-of-clause-bol py-beginning-of-comment py-beginning-of-declarations
                              py-beginning-of-decorator py-beginning-of-decorator-bol
                              py-beginning-of-expression py-beginning-of-line
                              py-beginning-of-list-pps
                              py-beginning-of-minor-block
                              py-beginning-of-partial-expression
                              py-beginning-of-section py-beginning-of-statement-bol
                              py-beginning-of-top-level
                              py-forward-declarations py-backward-declarations
                              py-down py-up
                              py-down-block py-down-block-bol
                              py-down-block-or-clause py-down-block-or-clause-bol
                              py-down-class py-down-class-bol
                              py-down-clause py-down-clause-bol
                              py-down-def py-down-def-bol
                              py-down-def-or-class py-down-def-or-class-bol
                              py-down-minor-block py-down-minor-block-bol
                              py-down-section py-down-section-bol
                              py-down-statement py-down-top-level
                              py-backward-statement py-forward-statement
                              py-goto-block-up  py-go-to-beginning-of-comment
                              py-end py-end-of-block-or-clause
                              py-end-of-class py-end-of-comment
                              py-end-of-decorator py-end-of-expression
                              py-end-of-line py-end-of-list-position
                              py-end-of-partial-expression py-end-of-section
                              py-end-of-statement-bol py-end-of-string
                              py-end-of-top-level
                              py-beginning-of-statement py-end-of-statement
                              py-beginning-of-block py-end-of-block
                              py-beginning-of-clause py-end-of-clause
                              py-next-statement py-previous-statement
                              py-backward-def py-forward-def
                              py-backward-def-or-class py-forward-def-or-class
                              py-beginning-of-def-or-class py-end-of-def-or-class)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "Speak current statement after moving"
     (when (ems-interactive-p)
       (emacspeak-speak-line)
       (emacspeak-auditory-icon 'large-movement)))))

(loop
 for  f in
 '(
   py-mark-class-bol py-mark-clause py-mark-clause-bol py-mark-comment
                     py-mark-comment-bol py-mark-def py-mark-def-bol
                     py-mark-def-or-class py-mark-def-or-class-bol py-mark-except-block
                     py-mark-except-block-bol py-mark-expression py-mark-expression-bol
                     py-mark-if-block py-mark-if-block-bol py-mark-line py-mark-line-bol
                     py-mark-minor-block py-mark-minor-block-bol py-mark-paragraph
                     py-mark-paragraph-bol py-mark-partial-expression
                     py-mark-partial-expression-bol py-mark-section py-mark-statement
                     py-mark-statement-bol py-mark-top-level py-mark-top-level-bol
                     py-mark-try-block py-mark-try-block-bol)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "Speak number of lines marked"
     (when (ems-interactive-p)
       (dtk-speak
        (format
         "Marked block containing %s lines"
         (count-lines (region-beginning) (region-end))))
       (emacspeak-auditory-icon 'mark-object)))))

(loop
 for f in
 '(

   Possible completions are:
            py-narrow-to-block  py-narrow-to-block-or-clause    py-narrow-to-class
            py-narrow-to-clause         py-narrow-to-def        py-narrow-to-def-or-class
            py-narrow-to-statement
            )
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "Provide auditory feedback."
     (when (ems-interactive-p)
       (message "Narrowed  %s lines"
                (count-lines (point-min) (point-max)))))))

(defadvice py-mark-def-or-class (after emacspeak pre act comp)
  "Speak number of lines marked"
  (when (ems-interactive-p)
    (dtk-speak
     (format "Marked block containing %s lines"
             (count-lines (region-beginning)
                          (region-end))))
    (emacspeak-auditory-icon 'mark-object)))

(defadvice py-forward-into-nomenclature(after emacspeak pre act comp)
  "Speak rest of current word"
  (when (ems-interactive-p)
    (emacspeak-speak-word 1)))

(defadvice py-backward-into-nomenclature(after emacspeak pre act comp)
  "Speak rest of current word"
  (when (ems-interactive-p)
    (emacspeak-speak-word 1)))

;;}}}
;;{{{ the process buffer

(defadvice py-process-filter (around emacspeak pre act)
  "Make comint in Python speak its output. "
  (declare (special emacspeak-comint-autospeak))
  (let ((prior (point))
        (dtk-stop-immediately nil))
    ad-do-it
    (when (and  emacspeak-comint-autospeak
                (window-live-p
                 (get-buffer-window (process-buffer (ad-get-arg 0)))))
      (condition-case nil
          (emacspeak-speak-region prior (point))
        (error (emacspeak-auditory-icon 'scroll)
               (dtk-stop))))
    ad-return-value))

;;}}}

;;}}}
;;{{{ Additional navigation
(defun emacspeak-py-previous-block()
  "Move backward to the beginning of the current block.
If already at the beginning then move to previous block."
  (interactive)
  (let ((start (point)))
    (python-beginning-of-defun)
    (unless (eq start (point))
      (beginning-of-line)
      (emacspeak-auditory-icon 'large-movement)
      (emacspeak-speak-line))))

(defun emacspeak-py-next-block()
  "Move forward to the beginning of the next block."
  (interactive)
  (python-end-of-defun)
  (skip-syntax-forward " ")
  (forward-line 1)
  (beginning-of-line)
  (emacspeak-auditory-icon 'large-movement)
  (emacspeak-speak-line))

;;}}}
;;{{{ keybindings

(progn
  (declaim (special  python-mode-map))
  (define-key python-mode-map "\M-a" 'beginning-of-python-def-or-class)
  (define-key python-mode-map "\M-e" 'end-of-python-def-or-class)
  (define-key python-mode-map "\M-n" 'py-next-statement)
  (define-key python-mode-map "\M-p" 'py-previous-statement)
  (define-key python-mode-map "\C-\M-u" 'py-goto-block-up)
  (define-key python-mode-map "\C-\M-n" 'emacspeak-py-next-block)
  (define-key python-mode-map "\C-\M-p" 'emacspeak-py-previous-block)
  )
(add-hook 'python-mode-hook
          'emacspeak-setup-programming-mode)
;;}}}
;;{{{ Voice Mappings:
(voice-setup-add-map
 '(
   (py-number-face voice-lighten)
   (py-XXX-tag-face voice-animate)
   (py-pseudo-keyword-face voice-bolden-medium)
   (py-variable-name-face  emacspeak-voice-lock-variable-name-personality)
   (py-decorators-face voice-lighten)
   (py-builtins-face emacspeak-voice-lock-builtin-personality)
   (py-class-name-face voice-bolden-extra)
   (py-exception-name-face emacspeak-voice-lock-warning-personality)))

;;}}}
;;{{{ pydoc advice:
(defadvice pydoc (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (emacspeak-speak-rest-of-buffer)))

;;}}}

(provide 'emacspeak-py)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: nil
;;; end:

;;}}}
