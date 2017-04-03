[[ -o interactive ]] && echo "+++Reading .zshenv"

WORDCHARS='*?_[]~=&;!#$%^(){}'
# default is: *?_-.[]~=/&;!#$%^(){}<>
# other: "*?_-.[]~=&;!#$%^(){}<>\\"
WORDCHARS=${WORDCHARS:s,/,,}

export EDITOR=nano # to be overwritten later
export PAGER=less

# Setup path

# set PATH so it includes user's private bin if it exists
if [ -d "$HOME/bin" ] ; then
    export PATH=~/bin:$PATH
fi

# History
HISTFILE=$HOME/.zsh-history
HISTSIZE=10000
SAVEHIST=5000

## Sourcing OS-specific things
OS=$(uname -s); export OS
if [[ -f ~/.zsh.d/zsh.${OS} ]]; then
    if [[ ! -z $ZSHDEBUG ]]; then
        echo +++ ~/.zsh.d/zsh.${OS}
    fi
    source ~/.zsh.d/zsh.${OS}
fi

## Sourcing machine-specific things
export HOSTPREFIX=`hostname` # | cut -d. -f1 | sed 's/.*/\L&/'`

if [[ -f ~/.zsh.d/zsh.${HOSTPREFIX} ]]; then
    if [[ ! -z $ZSHDEBUG ]]; then
        echo +++ ~/.zsh.d/zsh.${HOSTPREFIX}
    fi
    source ~/.zsh.d/zsh.${HOSTPREFIX}
fi

export VAGRANT_DEFAULT_PROVIDER=virtualbox

[[ $TERM == "dumb" ]] && unsetopt zle && PS1='$ '
[[ ! $TERM == "dumb" ]] && TERM=xterm-256color
