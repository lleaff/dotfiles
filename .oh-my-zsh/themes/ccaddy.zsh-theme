bgColor='black'
fgColor=white
pathColor=$fgColor


# Convert integer to symbol characters
numToSymbol() {
	num=$1
	symbol=''
	while [[ $num > 0 ]]; do
		digit=$(( $num % 10 ))
		case $digit in
			0 ) ;;
			1 ) symbol='❶'$symbol ;;
			2 ) symbol='❷'$symbol ;;
			3 ) symbol='❸'$symbol ;;
			4 ) symbol='❹'$symbol ;;
			5 ) symbol='❺'$symbol ;;
			6 ) symbol='❻'$symbol ;;
			7 ) symbol='❼'$symbol ;;
			8 ) symbol='❽'$symbol ;;
			9 ) symbol='❾'$symbol ;;
			10 ) symbol='❿'$symbol ;;
		esac
		num=$(( num / 10 ))
	done
	echo $symbol
}

# Background jobs
jobsIndicator() {
	echo '%k%(1j.%F{$fgColor}'$(numToSymbol $(jobs -l | wc -l))'.)%K{$bgColor}'
}

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
  #[[ $(jobs -l | wc -l) -gt 0 ]] && symbols+="%{%F{cyan}%}⚙"

  [[ -n "$symbols" ]] && echo "$symbols "
}

# Left side
PROMPT='%K{${bgColor}}$(prompt_status)%F{${pathColor}}%2c %{%f%k%b%}%F{${bgColor}}%f '
# Right side
RPROMPT='%K{${bgColor}}$(jobsIndicator)$(git_prompt_info)%{%f%k%b%}'
