# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

# If not running interactively, don't do anything
[ -z "$PS1" ] && return

# don't put duplicate lines in the history. See bash(1) for more options
# ... or force ignoredups and ignorespace
HISTCONTROL=ignoredups:ignorespace

# append to the history file, don't overwrite it
shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=1000
HISTFILESIZE=20000

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# set a fancy prompt (non-color, unless we know we "want" color)
case "$TERM" in
    xterm-color) color_prompt=yes;;
esac

# uncomment for a colored prompt, if the terminal has the capability
force_color_prompt=yes

if [ -n "$force_color_prompt" ]; then
    if [ -x /usr/bin/tput ] && tput setaf 1 >&/dev/null; then
	# We have color support; assume it's compliant with Ecma-48
	# (ISO/IEC-6429). (Lack of such support is extremely rare, and such
	# a case would tend to support setf rather than setaf.)
	color_prompt=yes
    else
	color_prompt=
    fi
fi

if [ -f /usr/share/git/completion/git-prompt.sh  ]; then
    #Show git pending changes (needs to be set before PS1) #TODO check if file exists
    source /usr/share/git/completion/git-prompt.sh
    export GIT_PS1_SHOWDIRTYSTATE=1
elif [ -f /usr/local/git/contrib/completion/git-prompt.sh ]; then
    #Mac OS X
    source /usr/local/git/contrib/completion/git-prompt.sh
    export GIT_PS1_SHOWDIRTYSTATE=1
else
    function __git_ps1 () { echo ''; }
fi

if [ "$color_prompt" = yes ]; then
    PS1='\[\e[1;32m\]\u@\h\[\e[1;31m\] \w\[\e[1;33m\]$(__git_ps1)\[\e[1;35m\] \$\[\e[00m\] '
else
    PS1='\u@\h \w$(__git_ps1) \$ '
fi
unset color_prompt force_color_prompt
# 00m: white; 31m: red; 32m: green; 33m: yellow; 34m: blue
# https://wiki.archlinux.org/index.php/Color_Bash_Prompt#List_of_colors_for_prompt_and_Bash

# If this is an xterm set the title to user@host:dir
case "$TERM" in
xterm*|rxvt*)
    PS1="\[\e]0;\u@\h: \w\a\]$PS1"
    ;;
*)
    ;;
esac

# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto'
    #alias dir='dir --color=auto'
    #alias vdir='vdir --color=auto'

    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

# some more ls aliases
alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF'

# Add an "alert" alias for long running commands.  Use like so:
#   sleep 10; alert
alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'

# Alias definitions.
# You may want to put all your additions into a separate file like
# ~/.bash_aliases, instead of adding them here directly.
# See /usr/share/doc/bash-doc/examples in the bash-doc package.

if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if [ -f /etc/bash_completion ] && ! shopt -oq posix; then
    . /etc/bash_completion
fi
#For Mac OS X
[[ $PS1 && -f /usr/local/share/bash-completion/bash_completion ]] && \
    . /usr/local/share/bash-completion/bash_completion

#Virtualenvwrapper
#export WORKON_HOME=$HOME/.virtualenvs
#export PROJECT_HOME=$HOME/Projects
#source virtualenvwrapper.sh

## Config backup/restore functions
function backupconfig ()
{
    #General public dotfiles
    ls -A ~/repos/dotfiles/ | grep -v .git | xargs -I {} cp -a ~/{} ~/repos/dotfiles/
    cd ~/repos/dotfiles/
    git commit -a -m "${HOSTNAME} config"
    cd -

    #Private dotfiles
    ls -A ~/Dropbox/Informatica/dotfiles/ | grep -v -e .git -e ssh | xargs -I {} cp -a ~/{} ~/Dropbox/Informatica/dotfiles/
    cp ~/.ssh/config ~/Dropbox/Informatica/dotfiles/.ssh/
    cd ~/Dropbox/Informatica/dotfiles/
    git commit -a -m "${HOSTNAME} $(date '+%y%m%d') config"
    cd -
}

function restoreconfig ()
{
    ls -A ~/repos/dotfiles/ | grep -v .git | xargs -I {} cp -a ~/repos/dotfiles/{} ~/
    ls -A ~/Dropbox/Informatica/dotfiles/ | grep -v -e .git -e ssh | xargs -I {} cp ~/Dropbox/Informatica/dotfiles/{} ~/
    cp ~/Dropbox/Informatica/dotfiles/.ssh/config ~/.ssh/config
}

#X11
[[ $(tty) == "/dev/tty1" ]] && exec startx
