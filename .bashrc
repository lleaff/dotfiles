function __gccc99() {
	for f in $1; do gcc $f -std=c99 -pedantic -Wall -o ${f%.*}; done
}
alias gccs=__gccc99

function __gvim() {
	for f in $1; do eval '("C://Program Files (x86)/Vim/vim74/gvim.exe" $f &)'; done
}
alias gvim=__gvim

#ask for ssh password, keep it at the end
eval `ssh-agent`
ssh-add
