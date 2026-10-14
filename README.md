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
4. Pi AGENTS.md - `stow -t ~ pi`

## Zsh platform and machine settings

Zsh loads `.zsh.d/zsh.$(uname -s)` for platform settings, then the machine
profile selected by `~/.zsh-host`. For goriot:

```sh
printf 'goriot\n' > ~/.zsh-host
stow -t ~ zsh
```

Linux provides `ec` for a terminal Emacs client. The goriot profile also uses
that client for `EDITOR` and `VISUAL`; it starts an Emacs daemon if necessary.
Finish editing with `C-x C-c` to return to the calling program.

Install Doom itself at `~/.emacs.d` (its `bin` directory is already on the Zsh
PATH); `stow -t ~ doom` links only the personal settings at `~/.doom.d`.
After moving an existing Doom installation, run `~/.emacs.d/bin/doom sync`.

Zoxide and Starship are optional and initialized only when installed.

## Requirements
1.  Git auto-completion requires [git-completion.bash](https://github.com/git/git/blob/master/contrib/completion/git-completion.bash).
2.  Bash prompt git awareness requires [bash-git-prompt](https://github.com/magicmonty/bash-git-prompt).

    `brew install bash-git-prompt` 
