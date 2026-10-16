# Chezmoi Git Configuration Implementation Plan

**Goal:** Manage only `~/.gitconfig` with chezmoi in the existing repository.

**Architecture:** `.chezmoiroot` selects the `chezmoi/` subtree. Stow retains all other files. Use a plain shared Git configuration and leave editor selection to each machine's environment.

**Tech Stack:** chezmoi, Git, Bash, Python 3 (test assertions).

## Global Constraints

- Do not commit or stage changes.
- Preserve existing uncommitted SSH-agent work.
- Back up the active Git configuration before applying.
- No service, package, network, or media changes.

---

### Task 1: Migrate and validate Git configuration

**Files:**
- Create: `.chezmoiroot`, `chezmoi/dot_gitconfig`, `tests/chezmoi-git.sh`.
- Remove after migration: `.gitconfig`.
- Modify: `README.md`.
- Configure locally: `~/.config/chezmoi/chezmoi.toml`.

**Interfaces:** chezmoi maps `chezmoi/dot_gitconfig` to `~/.gitconfig`. Existing Stow packages keep their current paths.

1. Add `tests/chezmoi-git.sh`. It must assert that the only managed file is `.gitconfig`, apply into a temporary home, validate the agreed Git settings, check that `core.editor` is absent, and check that a second application has no differences.
2. Run `mise exec -- bash tests/chezmoi-git.sh` before migration. Expect failure because `.chezmoiroot` is absent.
3. Write `.chezmoiroot` containing `chezmoi`, move `.gitconfig` to `chezmoi/dot_gitconfig`, and remove its `[core]` editor entry.
4. Document installation, local sourceDir configuration, backup, preview, apply, and source-edit workflow in README.md.
5. Run `mise exec -- bash tests/chezmoi-git.sh` and `git diff --check`; require success.
6. Configure the local sourceDir as `/home/nikos/dvp/dotfiles`, back up `~/.gitconfig` without overwriting an existing backup, preview the change, and apply only `~/.gitconfig`.
7. Check global Git values, an empty chezmoi diff, Plex service state, mounts, free space, and failed systemd units. Do not change services.
