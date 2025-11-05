[[ -o interactive ]] && echo "+++Reading .zshenv"

# Setup path
# Establish a sane base PATH if it's empty (common in non-interactive shells)
if [[ -z "${PATH// }" ]]; then
  export PATH="/usr/local/bin:/opt/homebrew/bin:/usr/bin:/bin:/usr/sbin:/sbin"
fi

# Add personal bins without overwriting the base PATH
[[ -d "$HOME/bin" ]] && export PATH="$HOME/bin:$PATH"
[[ -d "$HOME/.npm-global/bin" ]] && export PATH="$HOME/.npm-global/bin:$PATH"

export EDITOR=mg # light emacs-like perfect for command line
export PAGER=less

export WORDCHARS='*?_[]~=&;!#$%^(){}'
# default is: *?_-.[]~=/&;!#$%^(){}<>
# other: "*?_-.[]~=&;!#$%^(){}<>\\"
WORDCHARS=${WORDCHARS:s,/,,}

# History
HISTFILE=$HOME/.zsh-history
HISTSIZE=10000
SAVEHIST=5000

## Sourcing OS-specific things
if [[ -o interactive ]]; then
  OS=$(uname -s); export OS
  [[ -f ~/.zsh.d/zsh.${OS} ]] && source ~/.zsh.d/zsh.${OS}
  export HOSTPREFIX="lalakis"
  [[ -f ~/.zsh.d/zsh.${HOSTPREFIX} ]] && source ~/.zsh.d/zsh.${HOSTPREFIX}
  [[ -f "$HOME/.cargo/env" ]] && source "$HOME/.cargo/env"
fi

[[ $TERM == "dumb" ]] && unsetopt zle && PS1='$ '
[[ ! $TERM == "dumb" ]] && TERM=xterm-256color
