eval "$(rbenv init -)"

# set PATH so it includes user's private bin if it exists
if [ -d "$HOME/bin" ] ; then
    PATH="$HOME/bin:$PATH"
fi

# Aliases
# various ls helpers
alias ls='ls -GF'
alias l.='ls -d .*'
alias ll='ls -lh'
alias l='ls -lh'
alias la='ls -alh'
alias lr='ls -lR'
# colorize greps
alias grep='grep --color=auto'
alias egrep='egrep --color=auto'
alias fgrep='fgrep --color=auto'
# make less a little more sane
alias less='less -RX'
# time helpers
alias epoch='date +%s'
alias dt='gdate "+%Y-%m-%dT%H:%M:%S.%3N%zZ"'

# look up a process quickly
function pg {
    # doing it again afterwards for the coloration
    ps aux | grep -F -i $1 | grep -F -v grep | grep -F -i $1
}

# Check if a URL is up
function chk-url() {
    curl -sL -w "%{http_code} %{url_effective}\\n" "$1" -o /dev/null
}

# HTTP verbs
alias get='curl -s -XGET'
alias post='curl -s -XPOST'
alias put='curl -s -XPUT'
alias delete='curl -s -XDELETE'

# Enable git auto-completion
source ~/git-completion.bash

# bash prompt
# if [ -f "$(brew --prefix)/opt/bash-git-prompt/share/gitprompt.sh" ]; then
. ~/git-prompt.sh
export GIT_PS1_SHOWDIRTYSTATE=1
export GIT_PS1_SHOWCOLORHINTS=1
export PS1='\[\e[32m\]\u@\h:\w\[\e[0m\]$(__git_ps1 " [%s]")\$ '

# enable bash 4 goodies
shopt -s globstar autocd

# bind history completion to arrow keys
bind '"\e[A": history-search-backward'
bind '"\e[B": history-search-forward'

# specify locale
export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8

# emacs specifics
alias e='emacsclient -n'
alias ec='emacsclient -c -n'

export EDITOR='emacsclient -c'

# ruby/rails specifics
alias be='bundle exec'
alias ber='bundle exec rspec'

# java specifics (os x)
export JAVA_HOME=`/usr/libexec/java_home -v 1.8`
