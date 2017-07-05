export TERM=xterm-256color

# Environment variables
if [[ -f ~/.env ]]; then
  set -o allexport; source ~/.env; set +o allexport;
fi 

if [[ -f ~/.termaliases ]]; then source ~/.termaliases;
elif [[ -f ~/dotfiles/.termaliases ]]; then source ~/dotfiles/.termaliases; fi

#nocolor='\e[0m'
#fgwhite='\e[0;37m'
#bgblack='\e[40m'
#export PS1="${fgwhite}${bgblack}\w${nocolor} "

[ -f ~/.fzf.bash ] && source ~/.fzf.bash

# Add to PATH
if [[ -f ~/.path ]]; then source ~/.path; fi

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion
