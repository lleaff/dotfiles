" Vim color file
" Maintainer:	lleaff
" Last Change:	$Date: 2015/04/09 19:50:05 $
" Last Change:	$Date: 2015/04/09 19:50:05 $
" URL:		http://hans.fugal.net/vim/colors/desert.vim
" Version:	$Id: lleaff-desert.vim v1 <- desert.vim,v 1.1 2004/06/13
"   19:30:30 vimboss Exp $

" cool help screens
" :he group-name
" :he highlight-groups
" :he cterm-colors

let g:colors_name="lleaff-uno"
set background=dark
if version > 580
    " no guarantees for version 5.8 and below, but this makes it stop
    " complaining
    hi clear
    if exists("syntax_on")
	syntax reset
    endif
endif

hi Normal	guifg=White guibg=grey20

" highlight groups
hi Cursor	guibg=khaki guifg=slategrey
"hi CursorIM
"hi Directory
"hi DiffAdd
"hi DiffChange
"hi DiffDelete
"hi DiffText
"hi ErrorMsg
hi VertSplit	guibg=#c2bfa5 guifg=grey50 gui=none
hi Folded	guibg=grey30 guifg=gold
hi FoldColumn	guibg=grey30 guifg=tan
hi IncSearch	guifg=slategrey guibg=khaki
"hi LineNr
hi ModeMsg	guifg=goldenrod
hi MoreMsg	guifg=SeaGreen
hi NonText	guifg=LightBlue guibg=grey30
hi Question	guifg=springgreen
hi Search	guibg=peru guifg=wheat
hi SpecialKey	guifg=yellowgreen
hi StatusLine	guibg=#c2bfa5 guifg=black gui=none
hi StatusLineNC	guibg=#c2bfa5 guifg=grey50 gui=none
hi Title	guifg=indianred
hi Visual	gui=none guifg=khaki guibg=olivedrab
"hi VisualNOS
hi WarningMsg	guifg=salmon
"hi WildMenu
"hi Menu
"hi Scrollbar
"hi Tooltip

" syntax highlighting groups
hi Comment	guifg=SkyBlue
hi Constant	guifg=#ffa0a0
hi Identifier	guifg=palegreen
hi Statement	guifg=khaki
hi PreProc	guifg=indianred
hi Type		guifg=darkkhaki
hi Special	guifg=navajowhite
"hi Underlined
hi Ignore	guifg=grey40
"hi Error
hi Todo		guifg=orangered guibg=yellow2

" color terminal definitions
hi SpecialKey	ctermfg=darkgreen
hi NonText		cterm=bold ctermfg=darkblue
hi Directory	ctermfg=darkcyan
hi ErrorMsg		cterm=NONE ctermfg=7 ctermbg=black
hi IncSearch	cterm=NONE ctermfg=yellow ctermbg=green
hi Search		cterm=NONE ctermfg=grey ctermbg=blue
hi MoreMsg		ctermfg=darkgreen
hi ModeMsg		cterm=NONE ctermfg=brown
hi LineNr		ctermfg=darkgrey ctermbg=black
hi Question		ctermfg=green
hi StatusLine	cterm=bold ctermfg=darkgrey ctermbg=black
hi StatusLineNC cterm=NONE ctermfg=darkgrey ctermbg=NONE
hi VertSplit	ctermfg=black ctermbg=black
hi TabLineFill	ctermfg=black ctermbg=darkgrey
hi TabLine		ctermfg=darkgrey ctermbg=black
hi TabLineSel	ctermfg=black ctermbg=NONE
hi Title		ctermfg=darkblue ctermbg=NONE
hi Visual		cterm=reverse
hi VisualNOS	cterm=bold,underline
hi WarningMsg	ctermfg=1
hi WildMenu		ctermfg=0 ctermbg=3
hi Folded		ctermfg=darkgrey ctermbg=NONE
hi FoldColumn	ctermfg=darkgrey ctermbg=NONE
hi DiffAdd		ctermbg=4
hi DiffChange	ctermbg=5
hi DiffDelete	cterm=bold ctermfg=4 ctermbg=6
hi DiffText		cterm=bold ctermbg=1
" syntax highlighting groups
hi Comment		ctermfg=6
hi Constant		ctermfg=173 "210
hi Special		ctermfg=5
hi Identifier	ctermfg=6
hi Statement	ctermfg=3
hi PreProc		ctermfg=140"5
hi Type			ctermfg=2

hi Underlined	cterm=underline ctermfg=5
hi Ignore		cterm=bold ctermfg=7
hi Ignore		ctermfg=darkgrey
hi Error		cterm=bold ctermfg=7 ctermbg=1
hi SignColumn	ctermbg=black
hi SyntasticErrorSign	ctermfg=white ctermbg=darkred
hi YCMErrorSign	ctermfg=white ctermbg=darkred
hi SyntasticError	ctermfg=white ctermbg=darkred
hi YCMErrorSection	ctermfg=white ctermbg=darkred
hi SyntasticWarningSign	ctermfg=white ctermbg=darkyellow
hi YCMWarningSign	ctermfg=white ctermbg=darkblue
hi SyntasticWarning	ctermfg=white ctermbg=darkblue
hi YCMWarningSection	ctermfg=white ctermbg=darkblue


"vim: sw=4