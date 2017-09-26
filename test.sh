#!/usr/bin/env bash

#####################
# Need this since LinkedIn doesn't play nicely
#####################

stack exec -- site-kyleondy check | sed '/linkedin\.com/d'
