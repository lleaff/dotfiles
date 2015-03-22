###grep case insensitive
function __grepi() {
	grep -i $@
}
alias grepi=__grepi

###autofill -o argument with first arg
function __gccs() {
	gcc $@ -pedantic -Wall -o ${1%.*}
}
alias gccs=__gccs

function __g++s() {
	g++ $@ -pedantic -Wall -o ${1%.*}
}
alias g++s=__g++s

###default arguments to compile c programs
function __gccc99() {
	gcc $@ -std=c99 -pedantic -Wall -o ${1%.*}
}
alias gccc99=__gccc99

###launch gvim
function __gvim() {
	gvim $@
}
alias gvim=__gvim

###"git" enhancement
function __git() {
	case $* in
		add* ) shift 1; command git add -v "$@" ;;
		status* ) shift 1; command git status -s "$@" ;;
		pull* ) shift 1; command git pull -v "$@" ;;
		logf* ) shift 1; command git log --stat --pretty=oneline "$@" ;; #log formatted
		* ) command git "$@" ;;
	esac
}
alias git=__git

###make with specific makefile
function __makef() {
	make -f $@
}
alias makef=__makef

###simulate improved ls output with find
function __lsm() {
	case $* in
		-a ) shift 1; command find "$@" -maxdepth "1" ! -name '.' ! -name '..' -printf "%-54.53f%Cd.%Cm.%CY   %s\n" ;;
		* ) command find "$@" -maxdepth "1" ! -name '.' ! -name '..' ! -name '.*' ! -name '~*' -printf "%-54.53f%Cd.%Cm.%CY   %s\n" ;;
	esac
}
alias lsm=__lsm
function __lsma() {
	find $1 -maxdepth "1" ! -name '.' ! -name '..' -printf "%-54.53f%Cd.%Cm.%CY   %s\n"
}
alias lsam=__lsma
function __lsma() {
	find $1 -maxdepth "1" ! -name '.' ! -name '..' -printf "%-54.53f%Cd.%Cm.%CY   %s\n"
}
alias lsam=__lsma
function __lsms() {
	find $1 -maxdepth "1" ! -name '.' ! -name '..' ! -name '.*' ! -name '~*' -printf "%f\n"
}
alias lssm=__lsms
#original ls under "lso" alias
function __lso() {
	command ls $*
}
alias lsom=__lso

function __ftree() {
    SEDMAGIC='s;[^/]*/;|____;g;s;____|; |;g'

    if [ "$#" -gt 0 ] ; then
       dirlist="$@"
    else
       dirlist="."
    fi

    for x in $dirlist; do
         find "$x" -print | sed -e "$SEDMAGIC"
    done
}
alias tree=__ftree
