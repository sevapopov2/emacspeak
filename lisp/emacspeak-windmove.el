;;; emacspeak-windmove.el --- speech-enable windmove 
;;; $Id: emacspeak-windmove.el,v 17.0 2002/11/23 01:29:01 raman Exp $
;;; $Author: raman $
;;; Description:  Emacspeak front-end for WINDMOVE
;;; Keywords: Emacspeak, windmove
;;{{{  LCD Archive entry:

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |raman@cs.cornell.edu
;;; A speech interface to Emacs |
;;; $Date: 2002/11/23 01:29:01 $ |
;;;  $Revision: 17.0 $ |
;;; Location undetermined
;;;

;;}}}
;;{{{  Copyright:

;;; Copyright (C) 1999 T. V. Raman <raman@cs.cornell.edu>
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

;;{{{  Introduction:

;;; Commentary:

;;; Package  windmove (bundled with Emacs 21)
;;; provides commands for navigating to windows based on
;;; relative position.
;;; 

;;}}}
;;{{{ required modules

;;; Code:

(eval-when-compile (require 'cl))
(declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-speak)
(require 'emacspeak-sounds)

;;}}}
;;{{{ advice window navigation

(loop for f in
      (list 'windmove-left
            'windmove-right
            'windmove-up
            'windmove-down)
      do
      (eval
       (`
	(defadvice  (, f) (after emacspeak pre act comp)
	  "Provide auditory feedback."
	  (when (interactive-p)
	    (emacspeak-auditory-icon 'select-object)
	    (emacspeak-speak-mode-line))))))

;;}}}

(provide 'emacspeak-windmove)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}
