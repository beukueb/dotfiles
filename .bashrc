# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

if [ ! -v CVN_PATH_SET ]; then
    export PATH=/usr/local/bin:$PATH;
    export CVN_PATH_SET=;
fi

# If not running interactively, don't do anything else
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

function searchlib ()
{
    searchdir=~/Dropbox/Science/Library
    echo Searching $searchdir for $1
    cd $searchdir 
    find . -name '*.pdf' -exec pdfgrep -Hin -m1 "$1" {} \;
    cd -
}

function doi2bib () { curl -H "Accept: application/x-bibtex" data.crossref.org/$1; }

function sshmnt ()
{
    if [[ ! -d ~/mnt/$1 ]]; then
	mkdir ~/mnt/$1
    fi
    if [[ -v $2 ]]; then REMLOC=$2; else REMLOC=`ssh $1 'echo $HOME'`; fi
    sshfs $1:$REMLOC ~/mnt/$1
}

#Git functions
function gic () { git $1 origin  $(git rev-parse --abbrev-ref HEAD); }
function gin () { # Fetch new remote branch
    newbranch=$(git remote show origin | grep new | tr -s ' ' | cut -f2 -d' ')
    git fetch origin $newbranch
    git checkout $newbranch
}

#Python envs
PYTHONENVS=~/.envs
function mkvirtualenv ()
{
    local envname arg projdir localhist ipyhist pyversion
    envname=$1
    shift
    while getopts 'a:p:hi' arg
    do
	case ${arg} in
            a) projdir=${OPTARG};;
            p) pyversion=${OPTARG};;
	    h) localhist=;;
	    i) ipyhist=;;
            *) return 1 # illegal option
        esac
    done
    if [ -v pyversion ]; then pyversion=python3; fi
    python3 -m venv $PYTHONENVS/$envname
    complete -W "$(ls $PYTHONENVS)" workon
    # Setup projdir
    if [ -v projdir ]
    then
	ln -s $projdir $PYTHONENVS/$envname/project
	cd $projdir
    fi
    if [ -v localhist ]; then touch $PYTHONENVS/$envname/.bash_history; fi
    if [ -v ipyhist ]; then ipython profile create $envname; fi
    workon $envname
}

function rmvirtualenv ()
{
    rm -rf $PYTHONENVS/$1
    complete -W "$(ls $PYTHONENVS)" workon
    if [ -d ~/.ipython/profile_$1 ]; then rm -rf ~/.ipython/profile_$1; fi
}

function workon ()
{
    . $PYTHONENVS/$1/bin/activate
    if [ -d $PYTHONENVS/$1/project ]
    then cd $(readlink $PYTHONENVS/$1/project)
    fi
    if [ -f $PYTHONENVS/$1/.bash_history ]
    then
	if [ ! -v DEF_HISTFILE ]; then DEF_HISTFILE=$HISTFILE; fi
	PROMPT_COMMAND="history -a; history -c; history -r;" #$PROMPT_COMMAND"
	HISTFILE=$PYTHONENVS/$1/.bash_history
	function deactivate(){
	    HISTFILE=$DEF_HISTFILE;
	    unset DEF_HISTFILE
	    unset PROMPT_COMMAND
	    unalias ipython >/dev/null 2>&1
	    . $VIRTUAL_ENV/bin/activate
	    deactivate
	}
    fi
    if [ -d ~/.ipython/profile_$1 ]
    then
	alias ipython="ipython --profile=$1"
	if [ ! -v DEF_HISTFILE ]; then
	    function deactivate(){
		unalias ipython >/dev/null 2>&1
		. $VIRTUAL_ENV/bin/activate
		deactivate
	    }
	fi
    fi
    if [ -f $PYTHONENVS/$1/project/README.md ] #&& emacsclient -a false -e 't'
    then emacsclient -n -u -e '(find-file "README.md")' '(search-forward "TODO")' \
		     "(select-frame-set-input-focus (selected-frame))"
    fi
}
complete -W "$(ls $PYTHONENVS)" workon

#Make current dir project dir for virtualenv
function workondir ()
{
    if [ -v VIRTUAL_ENV ]; then ln -sfhv $(pwd) $VIRTUAL_ENV/project; fi
}

#X11
[[ $(tty) == "/dev/tty1" ]] && exec startx
