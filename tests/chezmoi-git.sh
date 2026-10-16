#!/usr/bin/env bash
set -euo pipefail

repo=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/.." && pwd)
[[ -f "$repo/.chezmoiroot" ]] || { printf '%s\n' 'Missing .chezmoiroot' >&2; exit 1; }
work=$(mktemp -d)
trap 'rm -rf -- "$work"' EXIT
mkdir -p "$work/home"
printf '' > "$work/config.toml"
cm=(chezmoi --source "$repo" --destination "$work/home" --config "$work/config.toml" --persistent-state "$work/state.boltdb")

"${cm[@]}" managed --include=files --path-style=relative > "$work/managed"
[[ $(< "$work/managed") == '.gitconfig' ]]
"${cm[@]}" apply
python3 - "$work/home/.gitconfig" <<'PY'
import subprocess
import sys

path = sys.argv[1]
expected = {
    'user.name': 'Nikos Fertakis',
    'user.email': 'nikos.fertakis@gmail.com',
    'init.defaultBranch': 'main',
    'pull.ff': 'only',
    'rerere.enabled': 'true',
    'grep.lineNumber': 'true',
    'fetch.prune': 'true',
    'rebase.autosquash': 'true',
    'alias.st': 'status',
}
for key, value in expected.items():
    result = subprocess.run(['git', 'config', '--file', path, '--get', key],
                            check=True, capture_output=True, text=True)
    assert result.stdout.strip() == value, (key, result.stdout)
result = subprocess.run(['git', 'config', '--file', path, '--get', 'core.editor'],
                        capture_output=True, text=True)
assert result.returncode == 1, 'Leave editor selection to the machine environment'
PY
"${cm[@]}" apply
"${cm[@]}" diff > "$work/diff"
[[ ! -s "$work/diff" ]]
printf '%s\n' 'PASS: only Git configuration is managed; values and repeat application verified.'
