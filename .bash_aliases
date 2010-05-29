#!/bin/bash

#################################################
##
## Some useful aliases for bash
## Last Modified: Sat May 29, 2010  08:45PM
##
#################################################

alias ls='ls --color=auto --group-directories-first'
alias sl='ls --color=auto --group-directories-first' # to fix spelling errors
alias la='ls -Al --color=auto --group-directories-first'
alias ps='ps aux'
alias up='cd ..'
alias up2='cd ../..'
alias up3='cd ../../..'
alias futurama="curl -Is slashdot.org | egrep '^X-(F|B)' | cut -d \- -f 2"
alias man='man -P /usr/bin/most'
# alias mpich2='/opt/mpich2/bin/mpd'
# alias mpicc='/opt/mpich2/bin/mpicc'
# alias mpicxx='/opt/mpich2/bin/mpicxx'
# alias mpiexec='/opt/mpich2/bin/mpiexec'
alias serije='cd /data/encrypted/video/serije'
alias filmovi='cd /data/encrypted/video/filmovi'
alias getmyip='wget www.whatismyip.com/automation/n09230945.asp -O - -o /dev/null'
alias update='sudo clyde -Syu --aur'
alias gimme='sudo clyde -S'
alias search='clyde -Ss'
alias gtfo='sudo clyde -R'
alias back="popd > /dev/null"
alias gvim=edit
