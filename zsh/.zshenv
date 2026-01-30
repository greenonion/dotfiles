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

  # Per-machine selector using ~/.zsh-host only
  # Create the file with something like `echo work > ~/.zsh-host`
  typeset -g HOSTTYPE="default"

  if [[ -r "$HOME/.zsh-host" ]]; then
      HOSTTYPE="$(<"$HOME/.zsh-host")"
  fi

  # Normalize: lowercase and filename-safe
  HOSTTYPE=${(L)HOSTTYPE}
  HOSTTYPE=${HOSTTYPE//[^A-Za-z0-9._-]/-}

  # Load per-machine config, e.g. ~/.zsh.d/zsh.personal
  [[ -r "$HOME/.zsh.d/zsh.${HOSTTYPE}" ]] && source "$HOME/.zsh.d/zsh.${HOSTTYPE}"

  [[ -f "$HOME/.cargo/env" ]] && source "$HOME/.cargo/env"
fi
