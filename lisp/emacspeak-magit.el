;;; emacspeak-magit.el --- Speech-enable MAGIT: Git Client  -*- lexical-binding: t; -*-
;;; $Id: emacspeak-magit.el 4797 2007-07-16 23:31:22Z tv.raman.tv $
;;; $Author: tv.raman.tv $
;;; Description:  Speech-enable MAGIT An Emacs Interface to magit
;;; Keywords: Emacspeak,  Audio Desktop magit
;;{{{  LCD Archive entry:

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |raman@cs.cornell.edu
;;; A speech interface to Emacs |
;;; $Date: 2007-05-03 18:13:44 -0700 (Thu, 03 May 2007) $ |
;;;  $Revision: 4532 $ |
;;; Location undetermined
;;;

;;}}}
;;{{{  Copyright:
;;;Copyright (C) 1995 -- 2015, T. V. Raman
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
;;; MERCHANTABILITY or FITNMAGIT FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;}}}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{  introduction

;;; Commentary:
;;; MAGIT ==  Git interface in Emacs
;;; git clone git://github.com/magit/magit.git

;;}}}
;;{{{  Required modules

(require 'cl)
(declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)

(eval-when-compile (require 'magit "magit" 'no-error))

;;}}}
;;{{{ Map voices to faces:
(voice-setup-add-map
 '(
   (magit-header voice-bolden)
   (magit-section-title voice-annotate)
   (magit-diff-file-heading voice-animate)
   (magit-diff-file-heading-selection voice-animate)
   (magit-diff-hunk-heading-selection voice-animate-medium)
   (magit-diff-added voice-animate-extra)
   (magit-diff-removed voice-animate-extra)
   (magit-whitespace-warning-face voice-monotone)
   (magit-popup-argument voice-animate)
   (magit-menu-selected-option voice-animate)
   (magit-branch voice-lighten)
   (magit-diff-add voice-animate-extra)
   (magit-diff-del voice-animate-extra)
   (magit-diff-file-header voice-animate)
   (magit-diff-hunk-header voice-animate-medium)
   (magit-diff-none voice-monotone)
   (magit-item-highlight voice-brighten)
   (magit-item-mark voice-lighten-extra)
   (magit-log-graph voice-monotone)
   (magit-log-head-label-bisect-bad voice-smoothen)
   (magit-log-head-label-bisect-good voice-bolden)
   (magit-log-head-label-default voice-monotone)
   (magit-log-head-label-local voice-lighten)
   (magit-log-head-label-patches voice-bolden)
   (magit-log-head-label-remote voice-bolden)
   (magit-log-head-label-tags voice-animate)
   (magit-log-message voice-monotone)
   (magit-log-sha1 voice-monotone)
   (magit-log-tag-label voice-annotate)
   (magit-branch-local voice-brighten)
   (magit-branch-remote voice-lighten)
   (magit-diff-added-highlight voice-animate)
   (magit-diff-base-highlight voice-animate)
   (magit-diff-context-highlight voice-brighten)
   (magit-diff-context voice-lighten)
   (magit-diff-file-heading-highlight voice-bolden-extra)
   (magit-diff-hunk-heading voice-bolden)
   (magit-diff-hunk-heading-highlight voice-brighten)
   (magit-diff-removed-highlight voice-smoothen)
   (magit-dimmed voice-smoothen)
   (magit-hash voice-animate)
   (magit-header-line voice-bolden)
   (magit-popup-heading voice-bolden)
   (magit-popup-key voice-lighten)
   (magit-section-heading voice-bolden)
   (magit-section-highlight voice-animate)
   (magit-section-secondary-heading voice-bolden-medium)
   (magit-tag voice-animate)
   )
 )

;;}}}
;;{{{ Pronunciations in Magit:
(emacspeak-pronounce-add-dictionary-entry 'magit-mode
                                          emacspeak-pronounce-sha-checksum-pattern
                                          (cons 're-search-forward
                                                'emacspeak-pronounce-sha-checksum))
(emacspeak-pronounce-add-super 'magit-mode 'magit-commit-mode)
(emacspeak-pronounce-add-super 'magit-mode 'magit-revision-mode)
(emacspeak-pronounce-add-super 'magit-mode 'magit-log-mod)

(add-hook
 'magit-mode-hook
 'emacspeak-pronounce-refresh-pronunciations)

;;}}}
;;{{{ Advice navigation commands:

(defadvice magit-correct-point-after-command (around emacspeak pre act comp)
  "Fix speech feedback when walking around a magit buffer."
  (let ((from-invisible (invisible-p (point))))
    ad-do-it
    (unless (invisible-p (point))
      (when (and from-invisible
                 (or (eq this-command 'next-line)
                     (eq this-command 'previous-line)))
        (emacspeak-speak-line)))
    ad-return-value))

;;; Advice navigators:
(loop for f in
      '(magit-mark-item
        git-rebase-reword
        git-rebase-edit
        git-rebase-squash
        git-rebase-fixup
        git-rebase-pick)
      do
      (eval
       `(defadvice ,f (after emacspeak pre act comp)
          "Provide auditory feedback."
          (when (ems-interactive-p )
            (emacspeak-auditory-icon 'mark-object)
            (emacspeak-speak-line)))))

(defadvice git-rebase-kill-line (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'deselect-object)
    (emacspeak-speak-line)))

(defadvice git-rebase-undo (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p )
    (emacspeak-auditory-icon (if (buffer-modified-p) 'modified-object 'unmodified-object))))

(defadvice git-rebase-move-line-up (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p )
    (emacspeak-auditory-icon 'left)))

(defadvice git-rebase-move-line-down (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p )
    (emacspeak-auditory-icon 'right)))

(defadvice magit-pop-revision-stack (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'yank-object)))

(loop for f in
      '(magit-copy-item-as-kill
        magit-copy-section-value
        magit-copy-buffer-revision)
      do
      (eval
       `(defadvice ,f (after emacspeak pre act comp)
          "Provide auditory feedback."
          (when (ems-interactive-p)
            (emacspeak-auditory-icon 'mark-object)))))

(loop for f in
      '(magit-toggle-section magit-section-toggle)
      do
      (eval
       `(defadvice ,f (after emacspeak pre act comp)
          "Provide auditory feedback."
          (when (ems-interactive-p)
            (let ((state (magit-section-hidden (magit-current-section))))
              (cond
               (state (emacspeak-auditory-icon 'close-object))
               (t (emacspeak-auditory-icon 'open-object)))
              (emacspeak-speak-line))))))

(loop
 for f in
 '(
   magit-section-forward magit-section-backward
                         magit-ignore-file magit-ignore-item
                         magit-stash magit-stash-snapshot
                         magit-unstage magit-unstage-file magit-unstage-item
                         magit-stage magit-stage-file  magit-stage-item
                         magit-ignore-item-locally
                         magit-goto-next-section magit-goto-previous-section
                         magit-goto-parent-section magit-goto-line
                         magit-goto-section magit-goto-section-at-path)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "Provide auditory feedback"
     (when (ems-interactive-p)
       (emacspeak-auditory-icon 'large-movement)
       (emacspeak-speak-line)))))

;;}}}
;;{{{ Advice generator to advice generated  commands:

(defadvice  magit-key-mode-generate (after emacspeak pre act comp)
  "Advice  the key-group menu for GROUP"
  (let ((group (ad-get-arg 0))))
  (eval
   `(defadvice ,(intern (concat "magit-key-mode-popup-" (symbol-name group))) 
        (after emacspeak  pre act comp)
      ,(concat "Speech-enabled Key menu for " (symbol-name group))
      (dtk-speak
       (save-current-buffer
         (set-buffer ,(format magit-key-mode-buf-name group))
         (buffer-string))))))
;;; load the magit-key-mode file so the above advice gets applied:

(when (locate-library "magit-key-mode")
  (load-library "magit-key-mode"))

(defadvice magit-refresh-popup-buffer (after emacspeak  pre act comp)
  "Voiceify faces."
  (voice-lock-voiceify-faces))

(defadvice magit-invoke-popup-action (after emacspeak  pre act comp)
  "Provide auditory feedback on quit action."
  (when (and (ems-interactive-p) (eq (ad-get-arg 0) ?q))
    (emacspeak-auditory-icon 'close-object)
    (emacspeak-speak-mode-line)))

(defadvice magit-popup-help (after emacspeak  pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'help)
    (emacspeak-speak-line)))

(defadvice magit-popup-toggle-show-common-commands (after emacspeak  pre act comp)
  "Provide auditory feedback."
  (declare (special magit-popup-show-common-commands))
  (when (ems-interactive-p)
    (emacspeak-auditory-icon (if magit-popup-show-common-commands 'on 'off))))

(loop for f in
      '(magit-popup-set-default-arguments magit-popup-save-default-arguments)
      do
      (eval
       `(defadvice ,f (after emacspeak pre act comp)
          "Provide auditory feedback."
          (when (ems-interactive-p)
            (emacspeak-auditory-icon (if (ad-get-arg 0) 'close-object 'save-object))))))

(defadvice magit-invoke-popup (after emacspeak pre act comp)
  "Speech-enable  magit-popup."
  (emacspeak-auditory-icon 'open-object)
  (emacspeak-speak-buffer))

;;}}}
;;{{{ Advice hide/show commands:
(loop for f in
      '(magit-show magit-show-commit
                   magit-show-branches magit-show-branches-mode
                   magit-show-item-or-scroll-down magit-show-item-or-scroll-up
                   magit-show-level
                   magit-show-level-1 magit-show-level-1-all
                   magit-show-level-2 magit-show-level-2-all
                   magit-show-level-3 magit-show-level-3-all
                   magit-show-level-4 magit-show-level-4-all
                   magit-show-only-files magit-show-only-files-all
                   magit-expand-section magit-expand-collapse-section
                   magit-show-section magit-show-stash
                   magit-status
                   magit-visit-item magit-diff-visit-file
                   magit-log
                   magit-log-long
                   magit-log-current
                   magit-log-head
                   magit-log-branches
                   magit-log-all-branches
                   magit-log-all
                   magit-log-buffer-file
                   magit-reflog
                   magit-reflog-head
                   magit-reflog-current
                   magit-wazzup
                   magit-interactive-resolve-item)
      do
      (eval
       `(defadvice ,f (after emacspeak pre act comp)
          "Provide auditory feedback."
          (when (ems-interactive-p)
            (emacspeak-speak-line)
            (emacspeak-auditory-icon 'open-object)))))

(loop for f in
      '(magit-hide-section
        magit-collapse-section
        magit-invoke-popup-option
        magit-log-select-quit
        magit-mode-quit-window)
      do
      (eval
       `(defadvice ,f (after emacspeak pre act comp)
          "Provide auditory feedback."
          (when (ems-interactive-p)
            (emacspeak-speak-line)
            (emacspeak-auditory-icon 'close-object)))))

(loop for f in
      '(magit-quit-window
        magit-quit-branches-window
        magit-key-mode-kill-buffer
        magit-mode-bury-buffer
        magit-log-bury-buffer
        magit-popup-quit)
      do
      (eval
       `(defadvice ,f (after emacspeak pre act comp)
          "Provide auditory feedback."
          (when (ems-interactive-p)
            (emacspeak-auditory-icon 'close-object)
            (with-current-buffer (window-buffer)
              (emacspeak-speak-mode-line))))))

;;}}}
;;{{{ Additional commands to advice:

(loop for f in
      '(magit-add-log
        magit-log-edit
        magit-process-buffer
        magit-annotated-tag)
      do
      (eval
       `(defadvice ,f (after emacspeak pre act comp)
          "Provide auditory feedback."
          (when (ems-interactive-p)
            (emacspeak-auditory-icon 'open-object)))))

(loop for f in
      '(magit-log-edit-commit
        with-editor-cancel)
      do
      (eval
       `(defadvice ,f (after emacspeak pre act comp)
          "Provide auditory feedback."
          (when (ems-interactive-p)
            (emacspeak-auditory-icon 'close-object)))))

(defadvice magit-rebase-interactive (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (and (ems-interactive-p ) (derived-mode-p 'magit-log-select-mode))
    (emacspeak-auditory-icon 'open-object)
    (emacspeak-speak-mode-line)))

(defadvice magit-display-process (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (message "Displayed process buffer in other window.")))

(loop for f in
      '(magit-refresh
        magit-refresh-all
        magit-change-what-branch-tracks
        magit-tag
        magit-diff
        magit-diff-with-mark
        magit-diff-working-tree
        magit-diff-dwim
        magit-diff-staged
        magit-diff-unstaged
        magit-diff-while-committing
        magit-diff-paths
        magit-reverse
        magit-apply
        magit-apply-item
        magit-cherry-pick-item
        magit-stage-all
        magit-stage-modified
        magit-unstage-all
        magit-reset
        magit-reset-soft
        magit-reset-hard
        magit-reset-head
        magit-reset-working-tree
        magit-checkout
        magit-branch
        magit-branch-and-checkout
        magit-branch-spinoff
        magit-branch-reset
        magit-branch-rename
        magit-create-branch
        magit-merge
        magit-automatic-merge
        magit-manual-merge
        magit-merge-editmsg
        magit-merge-nocommit
        magit-merge-preview
        magit-merge-abort
        magit-rebase-step
        magit-rewrite-start
        magit-rewrite-stop
        magit-rewrite-finish
        magit-rewrite-abort
        magit-rewrite-set-used
        magit-rewrite-set-unused
        magit-format-patch
        magit-am-abort
        magit-branches-window-checkout)
      do
      (eval
       `(defadvice ,f (after emacspeak pre act comp)
          "Provide auditory feedback."
          (when (ems-interactive-p)
            (emacspeak-auditory-icon 'task-done)))))

;;}}}
;;{{{ Advise process-sentinel:

(defadvice magit-process-finish (after emacspeak pre act comp)
  "Provide auditory feedback."
  (let ((arg (ad-get-arg 0)))
    (unless (integerp arg)
      (setq arg (process-exit-status arg)))
    (unless (= arg 0)
      (emacspeak-auditory-icon 'warn-user))))

(defadvice magit-process-sentinel (after emacspeak pre act comp)
  "Provide auditory feedback."
  (emacspeak-auditory-icon 'task-done))

;;}}}
;;{{{ Branches:

(loop for f in
      '(magit-remove-branch
        magit-remove-branch-in-remote-repo
        magit-branch-delete
        magit-tag-delete
        magit-revert-item
        magit-discard
        magit-discard-item)
      do
      (eval
       `(defadvice ,f (after emacspeak pre act comp)
          "Provide auditory feedback."
          (when (ems-interactive-p)
            (emacspeak-auditory-icon 'delete-object)
            (emacspeak-speak-line)))))

;;}}}
;;{{{ Setting Command Options:

(defadvice magit-key-mode-add-option (after emacspeak pre act comp) 
  "Provide auditory feedback."
  (let ((for-group (ad-get-arg 0))
        (option-name (ad-get-arg 1)))
    (cond
     ((not (member option-name magit-key-mode-current-options))
      (emacspeak-auditory-icon 'delete-object)
      (message "Removed %s for %s" option-name for-group))
     (t (emacspeak-auditory-icon 'select-object)
        (message "Added %s for %s" option-name for-group)))))

(defadvice magit-key-mode-exec-at-point (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'button)))

(defsubst emacspeak-magit-key-mode-header-line ()
  "Currently set options and args for use in header-line."
  (declare (special magit-key-mode-current-options magit-key-mode-current-args))
  (let ((options
         (mapconcat
          #'identity
          magit-key-mode-current-options
          " "))
        (args
         (mapconcat
          #'identity
          (loop for k being the hash-keys of magit-key-mode-current-args
                collect
                (format "%s %s"
                        k (gethash k magit-key-mode-current-args)))
          " ")))
    (format "%s %s" options args)))    

(defadvice magit-key-mode-add-argument (after emacspeak pre act comp)
  "Speak header line where we accumulate and reflect current state."
  (emacspeak-speak-header-line))
(defadvice magit-key-mode-command (after emacspeak pre act comp)
  "Provide auditory feedback."
  (emacspeak-auditory-icon 'button)
  (emacspeak-speak-line))

(defadvice magit-key-mode(after emacspeak pre act comp)
  "Provide auditory icon."
  (setq header-line-format
        '(:eval (emacspeak-magit-key-mode-header-line)))
  (emacspeak-auditory-icon 'open-object))

;;}}}
(provide 'emacspeak-magit)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: nil
;;; end:

;;}}}
