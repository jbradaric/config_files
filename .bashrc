#!/bin/sh

# If not running interactively, don't do anything
[ -z "$PS1" ] && return

# exports
#################################################
export PATH=/usr/local/bin:$PATH
export CDPATH=.:/media/external:$CDPATH
export EDITOR=vim
export HISTCONTROL=$HISTCONTROL${HISTCONTROL+,}ignoredups,erasedups
export HISTSIZE=10000

# history file options
#################################################
shopt -s histappend # append to history file
shopt -s checkwinsize   # 

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
alias leafpad='leafpad --tab-width 4'
#alias student='ssh bradaric@student.math.hr'
#alias proxy='ssh bradaric@student.math.hr -D 9999'
alias ls='ls --color=auto --group-directories-first'
alias sl='ls --color=auto --group-directories-first'
alias la='ls -Al --color=auto --group-directories-first'
alias ps='ps aux'
alias up='cd ..'
alias up2='cd ../..'
alias up3='cd ../../..'
alias futurama="curl -Is slashdot.org | egrep '^X-(F|B)' | cut -d \- -f 2"
alias playlist="audtool2 playlist-addurl"

# prompt colours
#################################################
export PS1="${debian_chroot:+($debian_chroot)}\[\e[32;1m\]\u@\h: \[\e[1;34m\]\w \$ \[\e[0;32m\]"

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

# when called with arguments, open gvim with --remote
# without arguments, just open gvim
function edit () {
    if [ $# -gt 0 ]; then
        gvim --remote "$1"
    else
        gvim
    fi
}

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
