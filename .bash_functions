#!/bin/bash

#################################################
##
## Some useful functions
## Last Modified: Sat May 29, 2010  08:45PM
##
#################################################

# Emulate the "set autopushd" of zsh
#################################################
function cd() {
  builtin pushd "${*:-$HOME}" > /dev/null
}

# Function to extract archives
#################################################
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
    echo "'$1' is not a valid archive file!"
  fi
}

function edit () {
  if [ $# -gt 0 ]; then
    gvim --remote-silent "$*" > /dev/null 2>&1
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
