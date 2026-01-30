# Aliases
# various ls helpers
alias ls='ls -GF'
alias l.='ls -d .*'
alias ll='ls -lh'
alias l='ls -lh'
alias la='ls -alh'
alias lr='ls -lR'

# colorize grep (prefer GNU grep if available)
if command -v ggrep >/dev/null 2>&1; then
    alias grep='ggrep --color=auto'
elif grep --color=auto '' /dev/null >/dev/null 2>&1; then
    alias grep='grep --color=auto'
fi

# make less a little more sane
alias less='less -RX'
# time helpers
alias epoch='date +%s'

if command -v gdate >/dev/null 2>&1; then
    alias dt='gdate "+%Y-%m-%dT%H:%M:%S.%3N%zZ"'
else
    alias dt='date "+%Y-%m-%dT%H:%M:%S%z"'
fi
