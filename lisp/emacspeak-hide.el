;;; emacspeak-hide.el --- Provides user commands for hiding and exposing blocks of text
;;; $Id$
;;; $Author: tv.raman.tv $
;;; Description:  Hide and expose blocks of text
;;; Keywords: Emacspeak, Speak, Spoken Output, hide
;;{{{  LCD Archive entry:

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |raman@cs.cornell.edu
;;; A speech interface to Emacs |
;;; $Date: 2007-08-25 18:28:19 -0700 (Sat, 25 Aug 2007) $ |
;;;  $Revision$ |
;;; Location undetermined
;;;

;;}}}
;;{{{  Copyright:

;;;Copyright (C) 1995 -- 2015, T. V. Raman
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

;;{{{  Introduction

;;; Commentary:
;;; Flexible hide and show for emacspeak.
;;; This module allows one to easily hide or expose
;;; blocks of lines starting with a common prefix.
;;; It is motivated by the need to flexibly hide quoted text in email
;;; but is designed to be more general.
;;; the prefix parsing is inspired by filladapt.el
;;; Code:

;;}}}
;;{{{  Required modules

(require 'cl)
(declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)

;;}}}
;;{{{ voice locking for block header lines

(defcustom emacspeak-hidden-header-line-personality voice-annotate
  "Personality used to identify header lines of blocks."
  :type 'symbol
  :group 'emacspeak-hide)

;;; forcibly set this to t to avoid a possible Emacs bug:
(declaim (special line-move-ignore-invisible))
(setq line-move-ignore-invisible t)

;;}}}
;;{{{  Identifying the prefix

;;{{{  define parsing tables

(defvar emacspeak-hide-prefix-token-table
  '(
    ;; Included text in news or mail replies
    ("^[ \t]*[|>]+" citation)
    ;; Included text generated by SUPERCITE.  We can't hope to match all
    ;; the possible variations, your mileage may vary.
    ("^[ \t]*[A-Za-z0-9][^'`\"< \t\n]*>[ \t]*" supercite)
    ;; Lisp comments
    ("^[ \t]*;+" comment)
    ;; C comment block
    ("^[ \t]*/\\*+" comment)
    ("^[ \t]*\\*+" comment)
    ;; C++ comment block
    ("^[ \t]*//"  comment)
    ;; UNIX shell comments
    ("^[ \t]*#+" comment)
    ;; LaTeX comments
    ("^[ \t]*%+" comment)
    ;; Texinfo comments
    ("^[ \t]*@c[ \t]" comment)
    ("^[ \t]*@comment[ \t]" comment)
    )
  "Table of tokens emacspeak-hide knows about.
Format is

   ((REGEXP SYM) ...)

emacspeak-hide uses this table to build a tokenized representation of
the beginning of the current line.  Each REGEXP is matched
against the beginning of the line until a match is found.
Matching is done case-sensitively.  The corresponding SYM is
added to the list, point is moved to (match-end 0) and the
process is repeated.  The process ends when there is no REGEXP in
the table that matches what is at point.")

(defvar emacspeak-hide-prefix-not-token-table
  '(
    "^[ ]*$"
    "^[ \t]*[a-zA-Z]+ "
    )
  "List of regexps that can never be a token.
Before trying the regular expressions in emacspeak-hide-prefix-token-table,
the regexps in this list are tried.  If any regexp in this list
matches what is at point then the token generator gives up and
doesn't try any of the regexps in emacspeak-hide-prefix-token-table.

Regexp matching is done case-sensitively.")

;;}}}
;;{{{  parse the prefix

(defun emacspeak-hide-parse-prefix ()
  "Parse prefix   token after   point and return a prefix spec.
The tokens regular expressions are specified in
emacspeak-hide-prefix-token-table.  The list returned is of this form
  (SYM COL STRING)

SYM is a token symbol as found in emacspeak-hide-prefix-token-table.
COL is the column at which the token ended.
STRING is the token's text."
  (let ((token-list nil)
        (done nil)
        (case-fold-search nil)
        token-table not-token-table)
    (save-excursion
      (forward-line 0)
      (catch 'done
        (setq not-token-table emacspeak-hide-prefix-not-token-table)
        (while not-token-table
          (if (looking-at (car not-token-table))
              (throw 'done nil))
          (setq not-token-table (cdr not-token-table)))
        (setq token-table emacspeak-hide-prefix-token-table
              done nil)
        (while  token-table
          (cond
           ((null (looking-at (car (car token-table))))
            (setq token-table (cdr token-table)))
           (t                           ;got a match
            (goto-char (match-end 0))
            (setq token-list
                  (list (nth 1 (car token-table))
                        (current-column)
                        (buffer-substring-no-properties
                         (match-beginning 0)
                         (match-end 0)))
                  done t)
            (throw 'done token-list))))
        token-list))))

;;}}}
;;{{{  test for a prefix match

;;; Return t if this line matches the specified prefix spec

(defsubst emacspeak-hide-prefix-matches-this-line (prefix)
  (unless (eobp)
    (string-equal (nth 2 prefix)
                  (buffer-substring-no-properties  (point)
                                                   (+ (point) (nth 1  prefix))))))

;;}}}

;;}}}
;;{{{  hiding a block

(defun emacspeak-hide-current-block (prefix)
  "Hide block starting on current line identified by  PREFIX.
Blocks are portions of the buffer having a common prefix.
Hiding results in only the first line of the block being visible.
Returns t if a block was found and hidden."
  (let ((begin (line-beginning-position))
        (start nil)
        (continue t)
        (count 1))
    (save-excursion
      (cond
       ((not prefix)
        (message "Not on a block")
        nil)
       ((= 1 (forward-line 1))
        (message "At bottom of buffer. ")
        nil)
       (t                               ;start looking for a
                                        ;block
        (setq start (point))
        (unless (emacspeak-hide-prefix-matches-this-line prefix)
          (setq continue nil))
        (while (and continue (not (eobp)))
          (beginning-of-line 2)
          (unless (emacspeak-hide-prefix-matches-this-line prefix)
            (setq continue nil))
          (incf count))
        (cond
         ((> count 1)
          (with-silent-modifications
            (add-text-properties start (point)
                                 (list 'invisible t
                                       'cursor-intangible t
                                       'intangible t))
            (add-text-properties begin (point)
                                 (list 'emacspeak-hide-block-prefix (nth 2  prefix)
                                       'emacspeak-hidden-block (first prefix)
                                       'personality emacspeak-hidden-header-line-personality)))
          (message "Hid %s  %s lines"
                   count (first prefix))
          t)
         (t (message "Not on a block") nil)))))))

;;}}}
;;{{{  Exposing a block

;;; Hiding marks the body of a block to be invisible,
;;; and marks the entire block
;;; by setting property emacspeak-hidden-block to name of block
;;; Use this to efficiently identify and unhide the block.

(defun emacspeak-hide-expose-block ()
  "Exposes currently hidden block."
  (let ((start nil)
        (end nil)
        (block-name (get-text-property (point) 'emacspeak-hidden-block)))
    (save-excursion
      (forward-line 0)
      (cond
       (block-name
        (setq start (point))
        (setq end
              (next-single-property-change (point) 'emacspeak-hidden-block
                                           (current-buffer) (point-max)))
        (with-silent-modifications
          (put-text-property start end
                             'emacspeak-hidden-block nil)
          (put-text-property start end
                             'emacspeak-hide-block-prefix nil)
          (add-text-properties start end
                               (list 'invisible nil
                                     'intangible nil
                                     'cursor-intangible nil
                                     'personality nil)))
        (message "Exposed %s block containing %s lines"
                 block-name
                 (count-lines start end))
        end)
       (t (message "Not on a hidden block")
          nil)))))

;;}}}
;;{{{  Hiding and exposing  all blocks in a buffer
;;;###autoload
(defun emacspeak-hide-all-blocks-in-buffer ()
  "Hide all blocks in current buffer."
  (declare (special emacspeak-speak-messages))
  (let ((count 0)
        (emacspeak-speak-messages nil)
        (prefix nil))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (setq prefix (emacspeak-hide-parse-prefix))
        (cond
         ((and prefix
               (emacspeak-hide-current-block prefix))
          (incf count)
          (goto-char
           (next-single-property-change (point)
                                        'emacspeak-hidden-block
                                        (current-buffer)
                                        (point-max))))
         (t (forward-line 1)))))
    (dtk-speak
     (format "Hid %s blocks" count))))

(defun emacspeak-hide-expose-hidden-blocks-in-buffer ()
  "Expose any hidden blocks in current buffer."
  (declare (special emacspeak-speak-messages))
  (let ((count 0)
        (emacspeak-speak-messages nil)
        (block-end nil))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (setq block-end (emacspeak-hide-expose-block))
        (cond
         (block-end
          (goto-char block-end)
          (incf count))
         (t (forward-line 1)))))
    (dtk-speak (format "Exposed %s blocks" count))))

;;}}}
;;{{{ User interface
;;;helper to get prefix
(defsubst emacspeak-hide-get-block-prefix ()
  (declare (special emacspeak-hide-prefix-token-table))
  (let ((block-prefix nil))
    (or (emacspeak-hide-parse-prefix)
        (when (and (not (looking-at "^[ \t]*$"))
                   (y-or-n-p "Define a new block prefix? "))
          (setq block-prefix
                (read-from-minibuffer "Specify prefix: "))
          (push
           (list
            (format "^%s" block-prefix)
            'custom)
           emacspeak-hide-prefix-token-table)
          (setq block-prefix
                (list 'custom
                      (length block-prefix)
                      block-prefix))))))
;;;###autoload
(defun emacspeak-hide-or-expose-block (&optional prefix)
  "Hide or expose a block of text.
This command either hides or exposes a block of text
starting on the current line.  A block of text is defined as
a portion of the buffer in which all lines start with a
common PREFIX.  Optional interactive prefix arg causes all
blocks in current buffer to be hidden or exposed."

  (interactive "P")
  (save-excursion
    (dtk-stop)
    (forward-line 0)
    (cond
     (prefix                            ;work on entire buffer
      (let ((block (next-single-property-change (point-min)
                                                'emacspeak-hidden-block
                                                (current-buffer)
                                                (point-max))))
        (cond
         ((and block
               (or (get-text-property (point-min)
                                      'emacspeak-hidden-block)
                   (get-text-property block 'emacspeak-hidden-block)))
          (emacspeak-hide-expose-hidden-blocks-in-buffer))
         (t (emacspeak-hide-all-blocks-in-buffer)))))
     ((get-text-property (point) 'emacspeak-hidden-block)
      (emacspeak-hide-expose-block))
     (t
      (let ((block-prefix
             (emacspeak-hide-get-block-prefix)))
        (when block-prefix
          (emacspeak-hide-current-block  block-prefix)))))))
;;;###autoload
(defun emacspeak-hide-or-expose-all-blocks ()
  "Hide or expose all blocks in buffer."
  (interactive)
  (emacspeak-hide-or-expose-block 'all))

;;}}}
;;{{{  speaking blocks sans prefix

;;;###autoload
(defun emacspeak-hide-speak-block-sans-prefix ()
  "Speaks current block after stripping its prefix.
If the current block is not hidden, it first hides it.
This is useful because as you locate blocks, you can invoke this
command to listen to the block,
and when you have heard enough navigate easily  to move past the block."
  (interactive)
  (unless (get-text-property (point) 'emacspeak-hidden-block)
    (emacspeak-hide-current-block
     (emacspeak-hide-get-block-prefix)))
  (let ((scratch-buffer (get-buffer-create " *dtk-scratch-buffer* "))
        (block nil)
        (contents nil)
        (start nil)
        (end nil)
        (block-prefix (get-text-property (point) 'emacspeak-hide-block-prefix)))
    (save-excursion
      (forward-line 0)
      (cond
       (block-prefix
        (setq start (point))
        (setq end
              (next-single-property-change (point) 'emacspeak-hide-block-prefix
                                           (current-buffer)
                                           (point-max)))
        (setq contents (buffer-substring start end))
        (setq block (concat "^"
                            (regexp-quote block-prefix)))
        (set-buffer scratch-buffer)
        (setq buffer-undo-list t)
        (with-silent-modifications
          (erase-buffer)
          (insert contents)
          (put-text-property (point-min)
                             (point-max)
                             'invisible nil)
          (goto-char (point-min))
          (while (re-search-forward block nil t)
            (replace-match " ")))
        (emacspeak-speak-region (point-min) (point-max)))
       (t (message "Not on a hidden block"))))))

;;}}}
(provide 'emacspeak-hide)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: nil
;;; end:

;;}}}
