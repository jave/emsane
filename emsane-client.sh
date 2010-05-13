#!/bin/sh
emacsclient -e "(emsane-scanadf-emacsclient-notify \"`pwd`/$1\" \"$EMSANE_STATE\")"