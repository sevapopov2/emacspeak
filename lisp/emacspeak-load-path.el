;;; emacspeak-load-path.el -- Setup Emacs load-path for compiling Emacspeak
;;; $Id: emacspeak-load-path.el 8055 2012-12-21 19:37:09Z tv.raman.tv $
;;; $Author: tv.raman.tv $ 
;;; Description:  Sets up load-path for emacspeak compilation and installation
;;; Keywords: Emacspeak, Speech extension for Emacs
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
;;;Copyright (C) 1995 -- 2011, T. V. Raman 
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
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;}}}
(defvar emacspeak-directory
  (expand-file-name "../" (file-name-directory load-file-name))
  "Directory where emacspeak is installed. ")

(defvar emacspeak-lisp-directory
  (expand-file-name "lisp/" emacspeak-directory)
  "Directory containing lisp files for  Emacspeak.")  

(unless (member emacspeak-lisp-directory load-path )
  (setq load-path
        (cons emacspeak-lisp-directory load-path )))

(defvar emacspeak-resource-directory (expand-file-name "~/.emacspeak")
  "Directory where Emacspeak resource files such as pronunciation dictionaries are stored. ")

(setq byte-compile-warnings t)
                                        ;'(redefine callargs free-vars unresolved obsolete))

(condition-case nil
    (progn
      (called-interactively-p nil)
      (defsubst ems-interactive-p  ()
        "called-interactively-p 'interactive"
        (called-interactively-p 'interactive)))
  (error (defalias 'ems-interactive-p  'interactive-p )))

(if (fboundp 'process-live-p)
    (defalias 'ems-process-live-p 'process-live-p)
  (defun ems-process-live-p (process)
    "Returns non-nil if process is alive."
    (memq (process-status process) '(run open listen connect stop))))

(if (fboundp 'help-print-return-message)
    (defalias 'ems-print-help-return-message 'help-print-return-message)
  (defalias 'ems-print-help-return-message 'print-help-return-message))

(unless (fboundp 'with-silent-modifications)
  (defmacro with-silent-modifications (&rest body)
    "Execute BODY, pretending it does not modify the buffer.
If BODY performs real modifications to the buffer's text, other
than cosmetic ones, undo data may become corrupted.
Typically used around modifications of text-properties which do not really
affect the buffer's content."
    (declare (debug t) (indent 0))
    (let ((modified (make-symbol "modified")))
      `(let* ((,modified (buffer-modified-p))
              (buffer-undo-list t)
              (inhibit-read-only t)
              (inhibit-modification-hooks t)
              deactivate-mark
              buffer-file-name
              buffer-file-truename)
         (unwind-protect
             (progn
               ,@body)
           (unless ,modified
             (restore-buffer-modified-p nil)))))))

(provide 'emacspeak-load-path)
