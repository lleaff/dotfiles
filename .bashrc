export TERM=xterm-256color

if [[ -f ~/.env ]]; then source ~/.env; fi # Environement variables

#MinGW (Windows)
if [[ $OSTYPE == "msys" ]]; then

  export PATH="/c/PortableProgramFiles/:$PATH"

  alias whereis=where

  ###ask for ssh password, keep this at the end
  #eval `ssh-agent`
  #ssh-add

else

  # OSX
  if [[ $OSTYPE == darwin* ]]; then
    export PATH="$PATH:$HOME/Applications/nodejs-v4/bin"
  fi

  export VISUAL=/usr/bin/vim
  export EDITOR=/usr/bin/vim
fi


if [[ -f ~/.termaliases ]]; then source ~/.termaliases;
elif [[ -f ~/dotfiles/.termaliases ]]; then source ~/dotfiles/.termaliases; fi


#nocolor='\e[0m'
#fgwhite='\e[0;37m'
#bgblack='\e[40m'
#export PS1="${fgwhite}${bgblack}\w${nocolor} "

export PATH="$HOME/bin:$HOME/.local/bin:$PATH:$HOME/.bin"

[ -f ~/.fzf.bash ] && source ~/.fzf.bash
