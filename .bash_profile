eval "$(rbenv init -)"

alias ls="ls -G"
alias ll="ls -lah"
alias la="ls -A"
alias l="ll"

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
