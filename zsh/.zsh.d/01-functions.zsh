# Functions
# look up a process quickly
pg() {
    [[ -n "${1-}" ]] || return 2

    if command -v pgrep >/dev/null 2>&1; then
        pgrep -afi -- "$1"
    else
        ps aux | grep -F -i -- "$1" | grep -F -v grep
    fi
}

# Check if a URL is up
function chk-url() {
    curl -sL -w "%{http_code} %{url_effective}\\n" "$1" -o /dev/null
}

# cd back up to the highest level git repo dir
# thanks Dan!
function cds () {
    local top
    top=$(git rev-parse --show-toplevel 2>/dev/null) || return 1
    builtin cd "$top"
}
