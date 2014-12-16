function __gccs() {
	for f in $1; do gcc $f -o ${f%.*} -pedantic; done
}
alias gccs=__gccs

function __gvim() {
	for f in $1; do eval '("C://Program Files (x86)/Vim/vim74/gvim.exe" $f &)'; done
}
alias gvim=__gvim

#ask for ssh password, keep it at the end
eval `ssh-agent`
ssh-add
