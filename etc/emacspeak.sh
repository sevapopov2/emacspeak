#!/bin/sh
# emacspeak - execute emacs with speech enhancements
#$Id: emacspeak.sh,v 1.1.2.2 2012/03/02 16:45:38 master Exp $
# WARNING:
# This file is not the original one: modified for Debian speech support
#

if [ -f /etc/emacspeak.conf ]; then
    . /etc/emacspeak.conf
fi

CL_ALL=""
for CL in $* ; do
    case "$CL" in
        -o) DTK_PROGRAM=stereo-outloud
            ;;
        -m) DTK_PROGRAM=multispeech
            ;;
        -e) DTK_PROGRAM=espeak
            ;;
        -d) DTK_PROGRAM=dtk-soft
            ;;
        *) CL_ALL="$CL_ALL $CL"
            ;;
    esac
done
export DTK_PROGRAM

# Once we use Emacspeak as an all-purpose shell:
EDITOR="emacsclient --alternate-editor emacspeak"
VISUAL=$EDITOR
export EDITOR
export VISUAL

EMACSPEAK=yes
export EMACSPEAK
exec emacs $CL_ALL
