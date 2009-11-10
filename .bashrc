#!/bin/sh

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

# aliases
#################################################
alias pacman='yaourt'
alias ls='ls --color=auto --group-directories-first'
alias sl='ls --color=auto --group-directories-first' # to fix spelling errors
alias la='ls -Al --color=auto --group-directories-first'
alias ps='ps aux'
alias up='cd ..'
alias up2='cd ../..'
alias up3='cd ../../..'
alias futurama="curl -Is slashdot.org | egrep '^X-(F|B)' | cut -d \- -f 2"
alias man='man -P /usr/bin/most'
alias next='mpc next'
alias prev='mpc prev'
alias play='mpc play'
alias pause='mpc pause'

# prompt colours
#################################################
export PS1="\[\033[35m\]\t\[\033[m\] - \[\e[32;1m\]\u@\h: \[\e[1;34m\]\w \$ \[\e[0;32m\]"

if [ -f /etc/bash_completion ]; then
  . /etc/bash_completion
fi

# Emulate the "set autopushd" of zsh
#################################################
function cd() {
  builtin pushd "${*:-$HOME}" > /dev/null
}

alias back="popd > /dev/null"
#################################################

# Function to extract archives
function extract () {
  if [ -f "$1" ]; then
    case "$1" in
      *.tar.bz2)      tar xjvf "$1"   ;;
      *.tar.gz)       tar xzvf "$1"   ;;
      *.bz2)          bunzip2 "$1"    ;;
      *.rar)          unrar x "$1"    ;;
      *.gz)           gunzip "$1"     ;;
      *.tar)          tar xvf "$1"    ;;
      *.tbz2)         tar xvjf "$1"   ;;
      *.tgz)          tar xvzf "$1"   ;;
      *.zip)          unzip "$1"      ;;
      *)              echo "Don't know how to extract '$1'..." ;;
    esac
  else
    echo "'$1' is not a valid file!"
  fi
}

function edit () {
  if [ $# -gt 0 ]; then
    gvim --remote-silent ${*} > /dev/null 2>&1
  else
    gvim
  fi
}

alias gvim=edit

# start/stop/restart services
####################################################
function start () {
  for arg in $*; do
    sudo /etc/rc.d/$arg start
  done
}

function stop () {
  for arg in $*; do
    sudo /etc/rc.d/$arg stop
  done
}


function restart () {
  for arg in $*; do
    sudo /etc/rc.d/$arg restart
  done
}


# vim: filetype=sh:ts=2:sts=2:sw=2:expandtab
