eval "$(rbenv init -)"

alias ls="ls -G"
alias ll="ls -la"
alias la="ls -A"

# Enable git auto-completion
source ~/git-completion.bash

# bash prompt
GIT_PROMPT_THEME=Custom
if [ -f "$(brew --prefix)/opt/bash-git-prompt/share/gitprompt.sh" ]; then
    source "$(brew --prefix)/opt/bash-git-prompt/share/gitprompt.sh"
fi

# ruby/rails specifics
alias be='bundle exec'
alias ber='bundle exec rspec'
