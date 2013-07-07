;;; emacspeak-rpm.el --- speech-enable RPM
;;; $Id: emacspeak-rpm.el 7823 2012-06-03 01:16:29Z tv.raman.tv $
;;; $Author: tv.raman.tv $
;;; Description:  Emacspeak extension to speech-enable RPM
;;; Keywords: Emacspeak, rpm, Red Hat Package Manager
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

;;; Copyright (C) 1995 -- 2011, T. V. Raman<raman@cs.cornell.edu>
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

;;{{{ required modules

(require 'emacspeak-preamble)
;;}}}
;;{{{  Introduction:

;;; Commentary:

;;; This module speech-enables rpm.el
;;; rpm.el can be downloaded from
;;; http://www.uni-karlsruhe.de/~Detlev.Zundel/download/rpm.el
;;; and provides a nice interface to managing and browsing
;;; rpm.

;;; Code:

;;}}}
;;{{{ speech-enable interactive commands.

(defadvice rpm (after emacspeak pre act comp)
  "Provide speech feedback."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (message "Welcome to RPM")))

(defadvice rpm-invert-sort (after emacspeak pre act comp)
  "Provide speech feedback."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'task-done)
    (message "Inverted sort order")))
(defadvice rpm-toggle-format (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'task-done)))
(defadvice rpm-mark (after emacspeak pre act comp)
  "Provide speech feedback."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'mark-object)
    (emacspeak-speak-line)))
(defadvice rpm-mark-delete (after emacspeak pre act comp)
  "Provide speech feedback."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'mark-object)
    (emacspeak-speak-line)))
(defadvice rpm-unmark (after emacspeak pre act comp)
  "Provide speech feedback."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'deselect-object)
    (emacspeak-speak-line)))

(defadvice rpm-requires (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'open-object)))

(defadvice rpm-quit (after emacspeak pre act comp)
  "Provide speech feedback."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'close-object)
    (emacspeak-speak-mode-line)))

(defadvice rpm-rebuild-index (after emacspeak pre act comp)
  "Provide speech feedback."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'task-done)
    (message "Rebuilt index")))

(defadvice rpm-proc-sentinel (after emacspeak pre act )
  "Provide auditory feedback"
  (emacspeak-auditory-icon 'task-done))

(defadvice rpm-do-visit (after emacspeak pre act )
  "Provide auditory feedback"
  (emacspeak-auditory-icon 'open-object))

;;}}}
;;{{{ fix interactive commands 

;;}}}
(provide 'emacspeak-rpm)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}

;;; emacspeak-rpm.el --- search utilities
;;; $Id: emacspeak-rpm.el 7823 2012-06-03 01:16:29Z tv.raman.tv $
;;; $Author: tv.raman.tv $
;;; Description:  Emacspeak extension to make Web searching convenient
;;; Keywords: Emacspeak, WWW interaction
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

;;; Copyright (C) 1995 -- 2011, T. V. Raman<raman@cs.cornell.edu>
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

;;{{{ required modules

(require 'cl)
(declaim  (optimize  (safety 0) (speed 3)))
(require 'advice)
(require 'emacspeak-speak)
(require 'thingatpt)
(require 'emacspeak-personality)
(require 'emacspeak-sounds)
(require 'browse-url)

;;}}}
;;{{{  Introduction:

;;; Commentary:

;;; This module provides utility functions for searching the WWW

;;; Code:

;;}}}

(provide 'emacspeak-rpm)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}
