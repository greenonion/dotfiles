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

## Git configuration via chezmoi

Git configuration is managed by chezmoi. Other configurations still use Stow.
The root `.chezmoiroot` file selects `chezmoi/` as the source subtree; chezmoi
does not manage the Stow packages.

Install chezmoi with `mise use --global chezmoi@latest`, or on macOS with
`brew install chezmoi`. In `~/.config/chezmoi/chezmoi.toml`, set the absolute
path to this repository's checkout (use your own path on each machine):

```toml
sourceDir = "/home/nikos/dvp/dotfiles"
```

Before the first application, inspect any existing Git includes and machine
settings. Back up `~/.gitconfig` to a new filename. For this host's initial
migration, the backup is `~/.gitconfig.before-chezmoi`. Do not overwrite it.
Then preview and apply only Git configuration:

```sh
chezmoi managed
chezmoi diff ~/.gitconfig
chezmoi apply ~/.gitconfig
```

The managed-file list should contain only `.gitconfig`. Do not let another
manager or a symlink own that destination. The shared Git configuration does
not set `core.editor`; set `VISUAL` or `EDITOR` for each machine instead.

For later edits, change the source and review before applying:

```sh
chezmoi edit ~/.gitconfig
chezmoi diff ~/.gitconfig
chezmoi apply ~/.gitconfig
```

Editing `~/.gitconfig` directly does not update the repository. Commit and sync
source changes through Git as usual, then preview and apply on each machine.
If chezmoi is not on the current shell's PATH, use `mise exec -- chezmoi`.

Validate this integration without changing your home directory:

```sh
mise exec -- bash tests/chezmoi-git.sh
```

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
