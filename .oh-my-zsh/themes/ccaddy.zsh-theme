bgColor='black'
fgColor=6
pathColor=$fgColor

# Background jobs
jobsIndicator='%(1j. %F{$fgColor}[%j].)'

# Git
ZSH_THEME_GIT_PROMPT_PREFIX=' %F{yellow}'
ZSH_THEME_GIT_PROMPT_SUFFIX=' %f'
ZSH_THEME_GIT_PROMPT_DIRTY='%F{red} *%f'
ZSH_THEME_GIT_PROMPT_CLEAN=''

# Status: (from agnoster)
# - was there an error
# - am I root
# - are there background jobs?
prompt_status() {
  RETVAL=$?
  local symbols
  symbols=()
  [[ $RETVAL -ne 0 ]] && symbols+="%{%F{red}%}✘"
  [[ $UID -eq 0 ]] && symbols+="%{%F{yellow}%}⚡"
  [[ $(jobs -l | wc -l) -gt 0 ]] && symbols+="%{%F{cyan}%}⚙"

  [[ -n "$symbols" ]] && echo "$symbols "
}

# Left side
PROMPT='%K{${bgColor}}$(prompt_status)%F{${pathColor}}%2c %{%f%k%b%}%F{${bgColor}}%f '
# Right side
RPROMPT='%K{${bgColor}}$jobsIndicator$(git_prompt_info)%{%f%k%b%}'
