test -r ~/dotfiles/.termaliases && source ~/dotfiles/.termaliases

VISUAL=/usr/bin/vim; export EDITOR
EDITOR=/usr/bin/vim; export EDITOR

# For Windows
###ask for ssh password, keep this at the end
#eval `ssh-agent`
#ssh-add

nocolor='\e[0m'
fgwhite='\e[0;37m'
bgblack='\e[40m'

export PS1="${fgwhite}${bgblack}\w${nocolor} "
