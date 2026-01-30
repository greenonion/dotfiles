# Setup path
# Setup PATH
# .zshenv runs for every zsh invocation; keep this fast and non-interactive-safe.

# Establish a sane base PATH if it's empty
if [[ -z "${PATH-}" || -z "${PATH// }" ]]; then
  export PATH="/usr/bin:/bin:/usr/sbin:/sbin"
fi

# Ensure common install prefixes are available early (macOS Intel + Apple Silicon)
typeset -U path
for p in \
  /opt/homebrew/bin /opt/homebrew/sbin \
  /usr/local/bin /usr/local/sbin \
  "$HOME/bin" \
  "$HOME/.local/bin" \
  "$HOME/.npm-global/bin"
do
  [[ -d "$p" ]] && path=($p $path)
done

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

if [[ -o interactive ]]; then
  [[ -f "$HOME/.cargo/env" ]] && source "$HOME/.cargo/env"
fi
