function __gccs() {
for f in $1; do gcc $f -o ${f%.*}; done
}
alias gccs=__gccs

eval `ssh-agent`
ssh-add
