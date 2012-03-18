# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
ZSH_THEME="jurica"

# Set to this to use case-sensitive completion
CASE_SENSITIVE="true"

# Comment this out to disable weekly auto-update checks
DISABLE_AUTO_UPDATE="true"

# Uncomment following line if you want to disable colors in ls
# DISABLE_LS_COLORS="true"

# Uncomment following line if you want to disable autosetting terminal title.
# DISABLE_AUTO_TITLE="true"

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=(git history-substring-search)

source $ZSH/oh-my-zsh.sh

# Customize to your needs...

# The following lines were added by compinstall

zstyle ':completion:*' completer _expand _complete _ignored _correct _approximate
zstyle ':completion:*' max-errors 2 numeric

autoload -Uz compinit
compinit

autoload -U promptinit
promptinit

autoload -U colors
colors

# edit the current line in Vim
autoload -U edit-command-line
zle -N edit-command-line
bindkey -M vicmd v edit-command-line

setopt autopushd pushdminus pushdsilent pushdtohome
setopt autocd
setopt cdablevars
setopt interactivecomments
setopt nobanghist
setopt HIST_REDUCE_BLANKS
setopt HIST_IGNORE_SPACE
setopt SH_WORD_SPLIT
setopt nohup
setopt correct


# End of lines added by compinstall
# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=10000
setopt appendhistory
setopt histexpand
unsetopt histverify
setopt SHARE_HISTORY
setopt histignoredups
setopt histignorespace
unsetopt beep
bindkey -v
# End of lines configured by zsh-newuser-install

# Search history with up/down arrows
# bindkey "^[[A" history-search-backward
# bindkey "^[[B" history-search-forward

# fix the home and end keys
bindkey "^[[1~" beginning-of-line
bindkey "^[[4~" end-of-line

source /etc/profile.d/autojump.zsh
source $ZSH/plugins/history-substring-search/history-substring-search.plugin.zsh
source $HOME/.zshenv

#################################################
##
## Some useful aliases for zsh
## Last Modified: Sat May 29, 2010  08:45PM
##
#################################################

alias ls='ls --color=auto --group-directories-first'
alias la='ls -Al --color=auto --group-directories-first'
alias ps='ps aux'
alias up='cd ..'
alias up2='cd ../..'
alias up3='cd ../../..'
alias futurama="curl -Is slashdot.org | egrep '^X-(F|B)' | cut -d \- -f 2"
#alias man='man -P /usr/bin/most'
alias getmyip='wget www.whatismyip.com/automation/n09230945.asp -O - -o /dev/null'
alias update='yaourt -Syua'
alias gimme='yaourt -S'
alias search='yaourt -Ss'
alias gtfo='sudo pacman -Rs'
alias back="popd > /dev/null"
alias unrar='unrar -o+' # set overwrite to yes
alias mplayer='mplayer -fs -nolirc -cache 5000 -cache-min 50' # always start in fullscreen
alias back="popd > /dev/null"

# auto extension aliases
alias -s html=$BROWSER
alias -s php=$BROWSER
alias -s com=$BROWSER
alias -s net=$BROWSER
alias -s jpg=mirage
alias -s png=mirage
alias -s gif=mirage
alias -s gz='tar xf'
alias -s bz2='tar xf'
alias -s avi=mplayer
alias -s mkv=mplayer
alias -s mp4=mplayer

alias vim='nocorrect vim'
alias ssh='nocorrect ssh'
