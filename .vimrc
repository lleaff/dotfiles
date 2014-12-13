" To enable the saving and restoring of screen positions.
let g:screen_size_restore_pos = 1

" To save and restore screen for each Vim instance.
" This is useful if you routinely run more than one Vim instance.
" For all Vim to use the same settings, change this to 0.
let g:screen_size_by_vim_instance = 1

if has('win32')
	set showtabline=2 "always show tab bar
	language English_United States "Override environment language detection 
endif

set nocompatible 
filetype off 
set rtp+=~/.vim/ "for Windows

"---------------------------------------------P-L-U-G-I-N-S---------------------------------------------
"To install vim-plug:
"curl -Lo ~/.vim/autoload/plug.vim https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
call plug#begin('~/.vim/plugged')
"###########################################
"##################Plugins##################
"###########################################
Plug 'tpope/vim-sensible' "Configurations everyone agree on

Plug 'yegappan/mru' "Most Recently Used files, use :MRU command

Plug 'scrooloose/syntastic' "Syntax checking plugin
let g:syntastic_check_on_open=1
"YouCompleteMe
if has("win32")
	Plug $HOME . '/.vim/ycm.git' "Code completion engine
	"let g:yce_path_to_python_interpreter = 'C:/Python27/python.exe'
elseif has("unix")
	Plug 'Valloric/YouCompleteMe'
endif

Plug 'scrooloose/nerdtree', { 'on': 'NERDTreeToggle' } "File browser
"Plug 'jistr/vim-nerdtree-tabs' "NERDTree independant of tabs
Plug 'ctrlpvim/ctrlp.vim' "Fuzzy file, buffer, mru, tag, etc finder (Active fork of kien/) :help ctrlp-mappings

Plug 'ChoiZ/taglist.vim' "Source code browser (supports C/C++, java, perl, python, tcl, sql, php, etc) [v4.6, vim-scripts/ branch isn't updated]

Plug 'tomtom/tcomment_vim' "Comment toggle, handles embedded filetypes

Plug 'fanchangyong/a.vim' "Switch between source and header files in C/C++ code (:A, :AT (new tab))

"Plug 'bling/vim-airline' "status bar/tabline modification ====> dslkdfj <====

"Plug 'vim-scripts/restore_view.vim' " A plugin for automatically restoring files cursor position and folding
"if has("unix")
"	set viewoptions=cursor,folds,slash,unix
"elseif has("win32")
"	set viewoptions=cursor,folds,slash
"endif
" let g:skipview_files = ['*\.vim']

"Plug 'vim-scripts/FuzzyFinder' "buffer/file/command/tag/etc explorer with fuzzy matching :FufHelp

Plug 'wincent/command-t' "Fuzzy file finding

Plug 'kana/vim-gf-user' "Improvements to 'gf', open file under cursor
"=========Languages specific=========
"===JavaScript
Plug 'marijnh/tern_for_vim' "JavaScript code-analysis engine (r: YouCompleteMe, jshint (npm install -g jshint))
Plug 'jelera/vim-javascript-syntax' "JavaScript (r: syntastic)
Plug 'pangloss/vim-javascript' "JavaScript, vastly improved indentation and syntax support
"Plug 'vim-scripts/JavaScript-Indent' "Javascript indentation, if not using pangloss' syntax
Plug 'beautify-web/js-beautify'
Plug 'maksimr/vim-jsbeautify' "beautify using js-beautify based on ~/.vim/.editorconfig (r: beautify-web/js-beautify)
let g:editorconfig_Beautifier=$HOME . '/.vim/.editorconfig'
autocmd FileType javascript noremap <buffer>  <c-f> :call JsBeautify()<cr>
" for html
autocmd FileType html noremap <buffer> <c-f> :call HtmlBeautify()<cr>
" for css or scss
autocmd FileType css noremap <buffer> <c-f> :call CSSBeautify()<cr>
Plug 'kchmck/vim-coffee-script' "CoffeeScript syntax, indentating, compiling, and more.
Plug 'moll/vim-node' "Node.js tools and utilities										 ^
Plug 'guileen/vim-node-dict' "node.js dictionary 										 |
Plug 'ahayman/vim-nodejs-complete' "node.js omnifunc function of vi  v

Plug 'vim-scripts/cmake' "syntax update
Plug 'vim-scripts/cmake.vim' "indent

"===CSS
Plug 'ap/vim-css-color' "Highlight colors in CSS files
"Plug 'nathanaelkane/vim-indent-guides' "visual indent guides with bg color, toggle with <leader>ig
Plug 'Yggdroot/indentLine' "visual indent guides with thin vertical lines
Plug 'Raimondi/delimitMate' "Automatically add closing brackets
Plug 'tpope/vim-surround' "Delete surroundings: ds*, Change surroundings: cs**, Surround: ys<move>*, Surround line: yss* (r: nocompatible)

"=========cosmetic=========
Plug 'junegunn/rainbow_parentheses.vim' "Simpler Rainbow Parentheses

Plug 'jpalardy/spacehi.vim' "<F3> Toggle show white space characters

"#########################################################
"##################Color schemes, Colors##################
"#########################################################
Plug 'tomasr/molokai'
Plug 'altercation/vim-colors-solarized'
Plug 'Lokaltog/vim-distinguished'
Plug 'chriskempson/base16-vim' "Many great themes
Plug 'ciaranm/inkpot' "Plurple-pink-yellow
Plug 'junegunn/seoul256.vim'
"Bundle 'chriskempson/tomorrow-theme', {'rtp': 'vim/'}
Plug 'Junza/Spink' "Low color contrast brownish theme
Plug 'zenorocha/dracula-theme', {'rtp': 'vim/'}

" All of your Plugins must be added before the following line
call plug#end()
filetype plugin indent on
" To ignore plugin indent changes, instead use:
"filetype plugin on

" Vim-Plug help
" -------------
" PlugInstall [name ...][#threads]		Install plugins
" PlugUpdate [name ...][#threads]			Install or update plugins
" PlugClean[!]												Remove unused directories (bang version will clean without prompt)
" PlugUpgrade													Upgrade vim-plug itself
" PlugStatus													Check the status of plugins
" PlugDiff														See the updated changes from the previous PlugUpdate
" PlugSnapshot [output path]					Generate script for restoring the current snapshot of the plugin
"-------------------------------------------------------------------------------------------------------




"#######################################
"###############Cosmetics###############
"#######################################

" Colors, font
if has("gui_running")
	if has("gui_gtk2") 					" =========Linux
		set guifont=Inconsolata\ 12
		colorscheme base16-eighties
		set background=dark
	elseif has("gui_macvim") 		" =========OS X
		set guifont=Menlo\ Regular:h14
		colorscheme base16-embers
		set background=light
	elseif has("gui_win32") 		" =========Windows
		set guioptions -=T "no toolbar
		set guioptions -=m "no menubar
		set guifont=Meslo\ LG\ S\ DZ\ Regular:h10:cANSI

		colorscheme lleaff-chalk | set background=dark
		autocmd FileType c colorscheme Dracula | set background=dark
		autocmd FileType javascript colorscheme base16-mocha | set background=dark
	endif
endif

"set foldcolumn=2

"===Airline
set encoding=utf-8
if !exists('g:airline_symbols')
	let g:airline_symbols = {}
endif

if !exists('g:airline_symbols')
	let g:airline_symbols = {}
endif

" unicode symbols
let g:airline_left_sep = '»'
let g:airline_left_sep = ' ' " 
let g:airline_right_sep = '«'
let g:airline_right_sep = ' ' " 
"let g:airline_symbols.linenr = '␊'
"let g:airline_symbols.linenr = '␤'
"let g:airline_symbols.linenr = '¶'
"let g:airline_symbols.branch = '⎇'
let g:airline_symbols.paste = 'ρ'
let g:airline_symbols.paste = 'Þ'
"let g:airline_symbols.paste = '∥'
let g:airline_symbols.whitespace = 'Ξ'




"######################################
"###############Settings###############
"######################################

if has("win32")
	source ~/.vim/mswin-partial.vim
endif

:au BufAdd,BufNewFile * nested tab sball "Open each buffer in a new tab

" Original window size and position
set lines=70
set columns=116  
winpos 0 0 

syntax enable "Turn on syntax highlighting
set number "Show line numbers

set foldmethod=syntax
set foldlevel=7 "Open folds N levels when opening file

set shiftwidth=2| set tabstop=2

set listchars=tab:>-,eol:↵
"set list "Enables crlf glyph

set noerrorbells visualbell t_vb=				" Disable beeping
autocmd GUIEnter * set visualbell t_vb= " Disable flashing


"===========Restore window pos and curosr

if has("gui_running")
  function! ScreenFilename()
    if has('amiga')
      return "s:.vimsize"
    elseif has('win32')
      return $HOME.'\_vimsize'
    else
      return $HOME.'/.vimsize'
    endif
  endfunction

  function! ScreenRestore()
    " Restore window size (columns and lines) and position
    " from values stored in vimsize file.
    " Must set font first so columns and lines are based on font size.
    let f = ScreenFilename()
    if has("gui_running") && g:screen_size_restore_pos && filereadable(f)
      let vim_instance = (g:screen_size_by_vim_instance==1?(v:servername):'GVIM')
      for line in readfile(f)
        let sizepos = split(line)
        if len(sizepos) == 5 && sizepos[0] == vim_instance
          silent! execute "set columns=".sizepos[1]." lines=".sizepos[2]
          silent! execute "winpos ".sizepos[3]." ".sizepos[4]
          return
        endif
      endfor
    endif
  endfunction

  function! ScreenSave()
    " Save window size and position.
    if has("gui_running") && g:screen_size_restore_pos
      let vim_instance = (g:screen_size_by_vim_instance==1?(v:servername):'GVIM')
      let data = vim_instance . ' ' . &columns . ' ' . &lines . ' ' .
            \ (getwinposx()<0?0:getwinposx()) . ' ' .
            \ (getwinposy()<0?0:getwinposy())
      let f = ScreenFilename()
      if filereadable(f)
        let lines = readfile(f)
        call filter(lines, "v:val !~ '^" . vim_instance . "\\>'")
        call add(lines, data)
      else
        let lines = [data]
      endif
      call writefile(lines, f)
    endif
  endfunction

  if !exists('g:screen_size_restore_pos')
    let g:screen_size_restore_pos = 1
  endif
  if !exists('g:screen_size_by_vim_instance')
    let g:screen_size_by_vim_instance = 1
  endif
  autocmd VimEnter * if g:screen_size_restore_pos == 1 | call ScreenRestore() | endif
  autocmd VimLeavePre * if g:screen_size_restore_pos == 1 | call ScreenSave() | endif
endif





"########################################
"###############Keys remap###############
"########################################
"
" Remap the numpad Enter key to Esc
"map! <kEnter> <Esc>
"map <kEnter> <Esc>
" doesn't work tho

nnoremap <silent> K 5k
vnoremap <silent> K 5k
nnoremap <silent> J 5j
vnoremap <silent> J 5j

let mapleader=","

map <C-S-Tab> :tabprevious<CR> 		" Previous tab
nmap <C-S-Tab> :tabprevious<CR>
imap <C-S-Tab> <Esc>:tabprevious<CR>i
map <C-Tab> :tabnext<CR> 					" Next tab
nmap <C-Tab> :tabnext<CR>
imap <C-Tab> <Esc>:tabnext<CR>i

"Disable middle mouse click paste
noremap <MiddleMouse> <LeftMouse> | noremap! <MiddleMouse> <LeftMouse> | map <2-MiddleMouse> <LeftMouse> | imap <2-MiddleMouse> <LeftMouse> | map <3-MiddleMouse> <LeftMouse> | imap <3-MiddleMouse> <LeftMouse> | map <4-MiddleMouse> <LeftMouse> | imap <4-MiddleMouse> <LeftMouse>

command -nargs=0 -bar UpdateFileAndSave if &modified
			\|    if empty(bufname('%'))
				\|        browse confirm write
				\|    else
					\|        confirm write
					\|    endif
					\|endif
nnoremap <silent> <C-S> :<C-u>UpdateFileAndSave<CR>
"inoremap <silent> <C-S> :<C-u>UpdateFileAndSave<CR> "TODO: Find how to call function in insert mode

"nnoremap <silent> <C-W> :q<CR>

" Change font size
nnoremap <C-Up> :silent! let &guifont = substitute(
			\ &guifont,
			\ ':h\zs\d\+',
			\ '\=eval(submatch(0)+1)',
			\ '')<CR>
nnoremap <C-Down> :silent! let &guifont = substitute(
			\ &guifont,
			\ ':h\zs\d\+',
			\ '\=eval(submatch(0)-1)',
			\ '')<CR>
