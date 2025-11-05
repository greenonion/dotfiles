# Functions
# look up a process quickly
function pg {
    # doing it again afterwards for the coloration
    ps aux | grep -F -i $1 | grep -F -v grep | grep -F -i $1
}

# Check if a URL is up
function chk-url() {
    curl -sL -w "%{http_code} %{url_effective}\\n" "$1" -o /dev/null
}

# cd back up to the highest level git repo dir
# thanks Dan!
function cds () {
    ORIGINAL_PWD=`pwd`
    while [ ! -d ".git" -a `pwd` != "/" ]
    do
        cd ..
    done
    if [ ! -d ".git" ]
    then
        cd $ORIGINAL_PWD
    fi
}
