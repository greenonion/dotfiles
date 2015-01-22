eval "$(rbenv init -)"

alias ls="ls -G"
alias ll="ls -la"
alias la="ls -A"

# Enable git auto-completion
source ~/git-completion.bash

# bash prompt
# if [ -f "$(brew --prefix)/opt/bash-git-prompt/share/gitprompt.sh" ]; then
. ~/git-prompt.sh
export GIT_PS1_SHOWDIRTYSTATE=1
export GIT_PS1_SHOWCOLORHINTS=1
export PS1='\[\e[32m\]\u@\h:\w\[\e[0m\]$(__git_ps1 " [%s]")\$ '

# specify locale for this stupid old bash
export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8

# emacs specifics
alias e='emacsclient -t'
alias ec='emacsclient -c -n'

# ruby/rails specifics
alias be='bundle exec'
alias ber='bundle exec rspec'
# faster boot
export RUBY_HEAP_MIN_SLOTS=1000000
export RUBY_HEAP_SLOTS_INCREMENT=1000000
export RUBY_HEAP_SLOTS_GROWTH_FACTOR=1
export RUBY_GC_MALLOC_LIMIT=1000000000
export RUBY_HEAP_FREE_MIN=500000

# skroutz specifics
alias panopticon="ssh wowbagger@panopticon.skroutz.gr"
alias stg="ssh -t wowbagger@gntmgr.skroutz.gr"
