set nocompatible 
filetype off 
set rtp+=~/.vim/ "for Windows

" Detect kernel (Darwin => OSX)
let s:uname = system("uname -s")

if has("gui_running")
	" To enable the saving and restoring of screen positions.
	let g:screen_size_restore_pos = 1

	" To save and restore screen for each Vim instance.
	" This is useful if you routinely run more than one Vim instance.
	" For all Vim to use the same settings, change this to 0.
	let g:screen_size_by_vim_instance = 1
endif

if has('win32')
	set showtabline=2 "always show tab bar
	language English_United States "Override environment language detection 
endif

"---------------------------------------------P-L-U-G-I-N-S---------------------------------------------
"To install vim-plug:
"curl -Lo ~/.vim/autoload/plug.vim https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
call plug#begin('~/.vim/plugged')
"###########################################
"##################Plugins##################
"###########################################
Plug 'tpope/vim-sensible' "Configurations everyone agree on

"Plug 'yegappan/mru' "Most Recently Used files, use :MRU command

Plug 'scrooloose/syntastic' "Syntax checking plugin (options at #syntastic)

if has("unix")
	Plug 'tpope/vim-eunuch' " :Remove :Move :Rename :Chmod :SudoWrite :SudoEdit
endif

"YouCompleteMe, YCM
if has("win32")
	Plug $HOME . '/.vim/ycm_win64' "Code completion engine
	"let g:yce_path_to_python_interpreter = 'C:/Python27/python.exe'
elseif has("unix")
	Plug 'Valloric/YouCompleteMe'
endif
autocmd FileType c 		let g:ycm_global_ycm_extra_conf	= '~/.vim/ycm_files/c/.ycm_extra_conf.py'
autocmd FileType cpp 	let g:ycm_global_ycm_extra_conf	= '~/.vim/ycm_files/cpp/.ycm_extra_conf.py'
let g:ycm_server_keep_logfiles = 1
let g:ycm_server_log_level = 'debug'

"===File/Buffer management
Plug 'scrooloose/nerdtree', { 'on': 'NERDTreeToggle' } "File browser
"Plug 'jistr/vim-nerdtree-tabs' "NERDTree independant of tabs
Plug 'ctrlpvim/ctrlp.vim'  ", Fuzzy file, buffer, mru, tag, etc finder (Active fork of kien/) :help ctrlp-mappings
command MRU CtrlPMRU

Plug 'tpope/vim-fugitive' " Git wrapper (:Gwrite (=git add), :Gcommit, :Gpush, :Gstatus, :Gbrowse, ...)

Plug 'ChoiZ/taglist.vim', { 'on': ['TlistOpen', 'TlistToggle'] } "Source code browser (supports C/C++, java, perl, python, tcl, sql, php, etc) [v4.6, vim-scripts/ branch isn't updated]

Plug 'tomtom/tcomment_vim' "Comment toggle, handles embedded filetypes


"Plug 'bling/vim-airline' "status bar/tabline modification ====> dslkdfj <====

" Disabled because it doesn't work
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

"Plug 'sjl/gundo.vim' "Undo tree visualization
"nnoremap <leader>u :GundoToggle<CR>

Plug 'tpope/vim-surround' "Delete surroundings: ds*, Change surroundings: cs**, Surround: ys<move>*, Surround line: yss* (req: nocompatible)
Plug 'tpope/vim-repeat' "Enable repeating supported plugin maps with .

Plug 'tpope/vim-abolish' "Easily search for, substitute, and abbreviate mutlitple variants of a word

"Plug 'reedes/vim-pencil' "Rethinking Vim as a tool for writing

Plug 'bruno-/vim-husk' " More complete emacs-mode mappings for Vim command line (Alt-B, Alt-F, etc)

Plug 'terryma/vim-multiple-cursors'

"=========Languages specific=========
"awk, bash, c, git, latex, lua, matlab, & perl support
Plug 'WolfgangMehner/vim-plugins'

"===C & C++
Plug 'fanchangyong/a.vim', { 'for': ['c', 'c++'] } "Switch between source and header files in C/C++ code (:A, :AT (new tab))
if has("win32") " Add standard library headers to path on Windows
	let &path.="D:/Qt/Tools/mingw482_32/i686-w64-mingw32/include,"
endif

"===Rust
Plug 'wting/rust.vim' "Vim support for Rust file detection and syntax highlighting
"===JavaScript
Plug 'marijnh/tern_for_vim' "JavaScript code-analysis engine (r: YouCompleteMe, jshint (npm install jshint), cd ~/.vim/plugged/tern_for_vim && sudo npm install -g)
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
Plug 'moll/vim-node' "Node.js tools and utilities										 	^
Plug 'guileen/vim-node-dict' "node.js dictionary 										 	|
Plug 'ahayman/vim-nodejs-complete' "node.js omnifunc function of vi  	v

Plug 'vim-scripts/cmake' "syntax update
Plug 'vim-scripts/cmake.vim' "indent

"===CSS
Plug 'ap/vim-css-color' "Highlight colors in CSS files
"Plug 'nathanaelkane/vim-indent-guides' "visual indent guides with bg color, toggle with <leader>ig
Plug 'Yggdroot/indentLine' "visual indent guides with thin vertical lines
Plug 'Raimondi/delimitMate' "Automatically add closing brackets and quotes

"===========cosmetic===========
Plug 'junegunn/rainbow_parentheses.vim' "Simpler Rainbow Parentheses

Plug 'jpalardy/spacehi.vim' "<F3> Toggle show white space characters

Plug 'epage/vim-autohighlight'

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
Plug 'fugalh/desert.vim' "dark colorscheme, works in terminal

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
	if has("gui_gtk2") 				" =========Linux
		set guioptions -=T "no toolbar
		set guioptions -=m "no menubar
		"set guifont=DejaVu\ Sans\ Mono\ Regular\ 11
		set guifont=Meslo\ LG\ S\ DZ\ Regular\ 11
		colorscheme lleaff-chalk
		set background=dark
	elseif has("gui_macvim") 		" =========OS X
		set guifont=Menlo\ Regular:h14
		colorscheme base16-embers
		set background=light
	elseif has("gui_win32") 		" =========Windows
		set guioptions -=T "no toolbar
		set guioptions -=m "no menubar
		set guifont=Meslo\ LG\ S\ DZ\ Regular:h10:cANSI
		colorscheme lleaff-atelierdune | set background=dark
	endif
	"autocmd FileType c colorscheme base16-ocean | set background=dark
	"autocmd FileType javascript colorscheme base16-mocha | set background=dark
	highlight SyntasticErrorSign guifg=#cccccc guibg=#7D4D4D		" ^ Doesn't work
	highlight YCMErrorSign guifg=#cccccc guibg=#7D4D4D				" |
	highlight YCMErrorLine guifg=#cccccc guibg=#7D4D4D				" |
	highlight SyntasticWarningSign guifg=#cccccc guibg=#976D4F 		" |
	highlight YCMWarningSign guifg=#cccccc guibg=#976D4F 			" |
	highlight YCMWarningLine guifg=#cccccc guibg=#976D4F 			" v


	"Highlight the nth column so you know when lines get too long
	autocmd Filetype vim,sh,c,cpp,c#,javascript,java 
				\ set colorcolumn=150 

	if &background == 'black'
		hi Search guibg=#303220 guifg=NONE
	elseif &background == 'light'
		hi Search guibg=#708559 guifg=NONE
	endif
else
	colorscheme lleaff-desert
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

" Always show sign gutter (Doesn't work with YCM because YCM removes the signs)
"autocmd BufEnter * sign define dummy
"autocmd BufEnter * execute 'sign place 9999 line=1 name=dummy buffer=' . bufnr('')



"######################################
"###############Settings###############
"######################################
"
"=== Store swap (.swp, .swo) files in a central location
:set directory=~/.vim/tmp

if has("win32")					" =========Windows
	source ~/.vim/mswin-partial.vim
elseif has("unix") 					
	if s:uname != "Darwin" 		" =========Linux
		source ~/.vim/mswin-partial.vim
	endif
endif

"if has("gui_win32")
"	set shell=C:/Program\ Files\ (x86)/Git/bin/sh.exe
"	set shellcmdflag=--login\ -i
"	set shellxquote=\"
"endif

"au FocusLost * :wa "Save all buffers when window loses focus

"au BufAdd,BufNewFile * nested tab sball "Open each buffer in a new tab

let &path.="src/include,/usr/incude/AL," "Look for header files

syntax enable "Turn on syntax highlighting
set number "Show line numbers

set foldmethod=syntax
set foldlevel=7 "Open folds N levels when opening file

set smartindent
set shiftwidth=4| set tabstop=4 "indent size

set listchars=tab:>-,eol:↵
"set list "Enables crlf glyph

set ttyfast "Scrolling stuff

set gdefault "Automatically add g option to :s and stuff like that

set smartcase	" ^ ignore case if search is all lowercase
set ignorecase	" v

set incsearch	" ^ highlight search results
set showmatch	" |
"set hlsearch	" v
"nnoremap <leader><space> :noh<cr>

set noerrorbells visualbell t_vb=		" Disable beeping
autocmd GUIEnter * set visualbell t_vb= " Disable flashing

"=== #syntastic
let g:syntastic_check_on_open=1
"let g:syntastic_cpp_compiler = 'clang++'
let g:syntastic_cpp_compiler_options = ' -std=c++11' " -stdlib=libc++'
let g:syntastic_error_symbol = '✗' 		"^sign interface symbols
let g:syntastic_warning_symbol = '!'	"v
"This needs to be kept near the end or it gets overwritten by smth else
highlight SyntasticErrorSign guifg=#cccccc guibg=#7D4D4D 
highlight SyntasticWarningSign guifg=#cccccc guibg=#976D4F 

"==============Status line
"Default status line: set statusline=%<%f\ %h%m%r%=%-14.(%l,%c%V%)\ %P

" tpope's status line
"set statusline=[%n]\ %<%.99f\ %h%w%m%r%y%=%-16(\ %l,%c-%v\ %)%P

"=============Default window size and position
"if has("gui_running")
"	set lines=70
"	set columns=116  
"	winpos 0 0 
"endif

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

"==============Restore cursor position===========
" When editing a file, always jump to the last known cursor position.
" Don't do it when the position is invalid or when inside an event handler
" (happens when dropping a file on gvim).
" Also don't do it when the mark is in the first line, that is the default
" position when opening a file.
autocmd BufReadPost *
  \ if line("'\"") > 1 && line("'\"") <= line("$") |
  \   exe "normal! g`\"" |
  \ endif

"#######################################
"###############Templates###############
"#######################################

autocmd BufNewFile *.html 0r $HOME/.vim/templates/html5_basic.txt

"##########################################
"###############Keys Mappings##############
"##########################################
"
" Remap the numpad Enter key to Esc
" (doesn't work in some terminal emulators)
map! <kEnter> <Esc>
map <kEnter> <Esc>

map <OM> <Esc>

"======Movement
nnoremap <silent> K 5gk
vnoremap <silent> K 5gk
nnoremap <silent> J 5gj
vnoremap <silent> J 5gj

nnoremap j gj	" ^ Move vertically by visual line
nnoremap k gk	" v

nnoremap <C-k> <Esc>:normal! J<CR>

nmap <tab> %
vmap <tab> %

nnoremap gV `[v`] " Highlight last inserted text

let mapleader=","

map <C-S-Tab> :tabprevious<CR> 		" Previous tab
nmap <C-S-Tab> :tabprevious<CR>
imap <C-S-Tab> <Esc>:tabprevious<CR>i
map <C-Tab> :tabnext<CR> 					" Next tab
nmap <C-Tab> :tabnext<CR>
imap <C-Tab> <Esc>:tabnext<CR>i

"******* current file directory commands WINDOWS **********************
"%:p:h:8 gets the current file's directory and :8 is what puts it
" into dos short form

"open explorer in the current file's directory
map ,e :!start explorer %:p:h:8<CR>

"open windows command prompt in the current file's directory
map ,c :!start cmd /k cd %:p:h:8<CR>

"open cygwin bash in the current file's directory
map ,b :!start bash --login -i -c 'cd `cygpath "%:p:h:8"`;bash'<CR>

"******* end current file directory commands WINDOWS ******************

"=====Save file as root even if vim is launched without sudo
cmap w!! w !sudo tee > /dev/null %

"=====Disable middle mouse click paste
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

"=====Change font size
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

"==========================================================================
"=========================== How to install Vim ===========================
"==========================================================================
" On Windows x64 {
" 	http://sourceforge.net/projects/cream/
" 	https://tuxproject.de/projects/vim/					"copy over ^
"  	Ycm { 												"Always choose 64 bit version otherwise YCM will fail to run
" 		http://sourceforge.net/projects/mingw-w64/		"install then run those two commands (adjust version number if needed) v
" 			cp C:/MinGW/lib/gcc/x86_64-w64-mingw32/4.8.1/include/c++/bits/* C:/MinGW/lib/gcc/x86_64-w64-mingw32/4.8.1/include/c++/x86_64-w64-mingw32/bits/*
" 			curl http://pastebin.com/raw.php?i=bZxiqYwW > C:/MinGW/lib/gcc/x86_64-w64-mingw32/4.8.1/specs
" 		https://www.python.org/downloads/release/python-279/
" 		http://sourceforge.net/projects/clangonwin/
" 		https://bitbucket.org/Haroogan/vim-youcompleteme-for-windows/downloads
" 	}
" }
"
