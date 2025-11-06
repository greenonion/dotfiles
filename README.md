# Configuration files for Vim, bash, git and Sublime Text

These are optimized for use on Mac OS X. Some Debian relics (for i3 mostly) included.

## Using

I've started using `stow` to create symlinks from these files. For example, to create a symlink for all zsh files do:

`> stow -t ~ zsh`

After changes, to resync do:

`> stow -R -t ~ zsh`

To remove symlinks do:

`> stow -D -t ~ zsh`

### Supported apps via stow

1. Zsh - `stow -t ~ zsh`
2. Ghostty - `stow -t ~ ghostty`
3. Emacs Doom - `stow -t ~ doom`

## Requirements
1.  Git auto-completion requires [git-completion.bash](https://github.com/git/git/blob/master/contrib/completion/git-completion.bash).
2.  Bash prompt git awareness requires [bash-git-prompt](https://github.com/magicmonty/bash-git-prompt).

    `brew install bash-git-prompt` 
