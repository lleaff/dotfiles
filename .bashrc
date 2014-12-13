function __gccs() {
for f in $1; do gcc $f -o ${f%.*}; done
}
alias gccs=__gccs

#ask for ssh password, keep it at the end
eval `ssh-agent`
ssh-add
