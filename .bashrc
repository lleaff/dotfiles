###default arguments to compile c programs
function __gccc99() {
	for f in $1; do gcc $f -std=c99 -pedantic -Wall -o ${f%.*}; done
}
alias gccc99=__gccc99
alias gccs=__gccc99

###launch gvim
function __gvim() {
	for f in $1; do eval '("C://Program Files (x86)/Vim/vim74/gvim.exe" $f &)'; done
}
alias gvim=__gvim

###always add -v (verbose) argument to git add
function __git() {
	case $* in
		add* ) shift 1; command git add -v "$@" ;;
		status* ) shift 1; command git status -s "$@" ;;
		* ) command git "$@" ;;
	esac
}
alias git=__git

alias lsa="ls -a"

###ask for ssh password, keep this at the end
eval `ssh-agent`
ssh-add
