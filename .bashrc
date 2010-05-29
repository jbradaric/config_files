#!/bin/sh

## Last Modified: Sat May 29, 2010  08:46PM

# If not running interactively, don't do anything
[ -z "$PS1" ] && return

# exports
#################################################
#export PATH=/usr/local/bin:$PATH
#export CDPATH=.:/media/external:$CDPATH
export EDITOR=vim
export HISTCONTROL=ignoreboth
export HISTSIZE=10000
export MPD_HOST="localhost"
export MPD_PORT=6601
export HAXE_LIBRARY_PATH=/opt/haxe/std:.
export LESS='--LINE-NUMBERS --quit-if-one-screen --quit-on-intr'

# history file options
#################################################
shopt -s histappend # append to history file
shopt -s checkwinsize # update LINES and COLUMNS after commands if necessary

# Turn on vi mode
#################################################
set -o vi

# misc options
#################################################
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"
unset MAILCHECK     # disable new mail warning
shopt -s cdspell

# bash completion
if [ -f /etc/bash_completion ]; then
  . /etc/bash_completion
fi

# functions
if [ -f $HOME/.bash_functions ]; then
  . $HOME/.bash_functions
fi

# aliases
if [ -f $HOME/.bash_aliases ]; then
  . $HOME/.bash_aliases
fi

# prompt colours
#################################################
export PS1="\[\033[35m\]\t\[\033[m\] - \[\e[32;1m\]\u@\h: \[\e[1;34m\]\w \$ \[\e[0;32m\]"

# vim: filetype=sh:ts=2:sts=2:sw=2:expandtab
