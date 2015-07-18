set nocompatible 
filetype off 
if has('win32')
	set rtp+=~/.vim/ "for Windows
endif

" Detect kernel (Darwin => OSX)
let s:uname = substitute(system("uname"), '\n', '', '')

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

"------------------------------------------------------------
"------------------------------------------------------------
"------------------------------------------------------------
"------------------------------------------------------------
" =Plugins
"------------------------------------------------------------
"------------------------------------------------------------
"------------------------------------------------------------
"------------------------------------------------------------
"To install vim-plug:
"curl -Lo ~/.vim/autoload/plug.vim https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
call plug#begin('~/.vim/plugged')
"============================================================
Plug 'tpope/vim-sensible' "Configurations everyone agree on

Plug 'scrooloose/syntastic' "Syntax checking plugin (options at #syntastic)

if has("unix")
	" :Remove :Move :Rename :Chmod :SudoWrite :SudoEdit
	Plug 'tpope/vim-eunuch'
endif

"YouCompleteMe, YCM
if has("win32")
	Plug $HOME . '/.vim/ycm_win64' "Code completion engine
	"let g:yce_path_to_python_interpreter = 'C:/Python27/python.exe'
elseif has("unix")
	Plug 'Valloric/YouCompleteMe'
endif
autocmd FileType c let g:ycm_global_ycm_extra_conf =
			\ '~/.vim/ycm_files/c/.ycm_extra_conf.py'
autocmd FileType cpp let g:ycm_global_ycm_extra_conf =
			\ '~/.vim/ycm_files/cpp/.ycm_extra_conf.py'
let g:ycm_server_keep_logfiles = 1
let g:ycm_server_log_level = 'debug'

Plug 'junegunn/vim-easy-align'
" Start interactive EasyAlign in visual mode (e.g. vip<Enter>)
vmap <Enter> <Plug>(EasyAlign)
" Start interactive EasyAlign for a motion/text object (e.g. gaip)
nmap ga <Plug>(EasyAlign)

" =File/Buffer management
"============================================================
Plug 'scrooloose/nerdtree', { 'on': 'NERDTreeToggle' } "File browser
"Plug 'jistr/vim-nerdtree-tabs' "NERDTree independant of tabs
"Fuzzy file, buffer, mru, tag, etc finder (Active fork of kien/)
" :help ctrlp-mappings
Plug 'ctrlpvim/ctrlp.vim'
command MRU CtrlPMRU

" Git wrapper (:Gwrite (=git add), :Gcommit, :Gpush, :Gstatus, :Gbrowse)
Plug 'tpope/vim-fugitive'
"
"Source code browser (supports C/C++, java, perl, python, tcl, sql, php,
" etc) [v4.6, vim-scripts/ branch isn't updated]
"Plug 'ChoiZ/taglist.vim', { 'on': ['TlistOpen', 'TlistToggle'] }

Plug 'tomtom/tcomment_vim' "Comment toggle, handles embedded filetypes

"buffer/file/command/tag/etc explorer with fuzzy matching :FufHelp
"Plug 'vim-scripts/FuzzyFinder'

Plug 'wincent/command-t' "Fuzzy file finding

Plug 'kana/vim-gf-user' "Improvements to 'gf', open file under cursor

"Plug 'sjl/gundo.vim' "Undo tree visualization
"nnoremap <leader>u :GundoToggle<CR>

"Delete surroundings: ds*, Change surroundings: cs**,
" Surround: ys<move>*, Surround line: yss* (req: nocompatible)
Plug 'tpope/vim-surround'
Plug 'tpope/vim-repeat' "Enable repeating supported plugin maps with .

"Easily search for, substitute, and abbreviate mutltiple variants of a word
" foobar -> FooBAR ; Foobar x FooBAR
Plug 'tpope/vim-abolish'

"Automatically clears search highlight
Plug 'pgdouyon/vim-evanesco'

"Plug 'reedes/vim-pencil' "Rethinking Vim as a tool for writing

"More complete emacs-mode mappings for Vim command line (Alt-B, Alt-F, etc)
Plug 'bruno-/vim-husk'

Plug 'terryma/vim-multiple-cursors'

" Editorconfig support, allows easily setting editor options on a
"	per-project basis. Sample .editorconfig:
"	root=true \n[*] \nindent_size = 4 \nindent_style = tab
Plug 'editorconfig/editorconfig-vim'

" =Languages specific, syntax
"============================================================
"awk, bash, c, git, latex, lua, matlab, & perl support
Plug 'WolfgangMehner/vim-plugins'

"=== C & C++
"Switch between source and header files in C/C++ code (:A, :AT (new tab))
Plug 'fanchangyong/a.vim', { 'for': ['c', 'c++'] }
"if has("win32") " Add standard library headers to path on Windows
"	let &path.='D:/Qt/Tools/mingw482_32/i686-w64-mingw32/include,'
"endif

"=== Rust
"Support for Rust file detection and syntax highlighting
Plug 'wting/rust.vim'
"=== JavaScript
" JavaScript code-analysis engine (r: YouCompleteMe, jshint (npm i -g),
" 	cd ~/.vim/plugged/tern_for_vim && sudo npm install)
Plug 'marijnh/tern_for_vim', { 'do': 'npm install' }
" Plug 'jelera/vim-javascript-syntax' "JavaScript (r: syntastic)
"JavaScript, vastly improved indentation and syntax support
Plug 'pangloss/vim-javascript'
"Javascript indentation, if not using pangloss' syntax
"Plug 'vim-scripts/JavaScript-Indent'
Plug 'beautify-web/js-beautify'
"beautify using js-beautify based on .editorconfig
"(r: beautify-web/js-beautify)
Plug 'maksimr/vim-jsbeautify'
autocmd FileType javascript noremap <buffer>  <c-f> :call JsBeautify()<cr>
" for html
autocmd FileType html noremap <buffer> <c-f> :call HtmlBeautify()<cr>
" for css or scss
autocmd FileType css noremap <buffer> <c-f> :call CSSBeautify()<cr>
Plug 'kchmck/vim-coffee-script' "CoffeeScript syntax, indentating,
" 							compiling, and more.
" Plug 'moll/vim-node' "Node.js tools and utilities						^
" Plug 'guileen/vim-node-dict' "node.js dictionary 				 		|
" Plug 'ahayman/vim-nodejs-complete' "node.js omnifunc function of vi	v

"=== HTML
Plug 'othree/html5.vim'
"=Jade
Plug 'digitaltoad/vim-jade'

"===CSS
Plug 'ap/vim-css-color' "Highlight colors in CSS files
Plug 'hail2u/vim-css3-syntax'
"=SCSS
Plug 'cakebaker/scss-syntax.vim'

"===CMake
Plug 'vim-scripts/cmake' "syntax update
Plug 'vim-scripts/cmake.vim' "indent


Plug 'Yggdroot/indentLine' "visual indent guides with thin vertical lines
Plug 'Raimondi/delimitMate' "Automatically add closing brackets and quotes

" =Cosmetic
"============================================================
Plug 'junegunn/rainbow_parentheses.vim' "Simpler Rainbow Parentheses
let g:rainbow#pairs = [['(', ')'], ['[', ']']]
let g:rainbow#blacklist = [ 0 ]

Plug 'jpalardy/spacehi.vim' "<F3> Toggle show white space characters

"visual indent guides with bg color, toggle with <leader>ig
"Plug 'nathanaelkane/vim-indent-guides'

Plug 'epage/vim-autohighlight'

" =Colorschemes, Colors
"============================================================
Plug 'xolox/vim-misc' " Dependency for vim-colorscheme-switcher
" Cycle through colorschemes with F8/Shift-F8
Plug 'xolox/vim-colorscheme-switcher'
"------------------------------------------------------------
Plug 'tomasr/molokai'
Plug 'altercation/vim-colors-solarized'
Plug 'Lokaltog/vim-distinguished'
Plug 'chriskempson/base16-vim' "Many great themes
Plug 'atelierbram/vim-colors_atelier-schemes'
Plug 'ciaranm/inkpot' "Plurple-pink-yellow
Plug 'junegunn/seoul256.vim'
Plug 'Junza/Spink' "Low color contrast brownish theme
Plug 'zenorocha/dracula-theme', {'rtp': 'vim/'}
Plug 'fugalh/desert.vim' "Term/GUI, dark
Plug 'kristiandupont/shades-of-teal' "GUI, dark, blueish, low-contrast
Plug 'sandeepsinghmails/Dev_Delight' "GUI, Light, colorful
Plug 'jonathanfilip/vim-lucius' "GUI/256Term
Plug 'lleaff/candy-crush-chronicle.vim' "GUI/256Term


"------------------------------------------------------------
" All of your Plugins must be added before the following line
call plug#end()
filetype plugin indent on
" To ignore plugin indent changes, instead use:
"filetype plugin on

" Vim-Plug help
" -------------
" PlugInstall [name ...][#threads]	Install plugins
" PlugUpdate [name ...][#threads]	Install or update plugins
" PlugClean[!]						Remove unused directories (bang
"								version will clean without prompt)
" PlugUpgrade						Upgrade vim-plug itself
" PlugStatus						Check the status of plugins
" PlugDiff							See the updated changes from the
"								previous PlugUpdate
" PlugSnapshot [output path]		Generate script for restoring the
" 								current snapshot of the plugin
"--------------------------------------------------------------------------

"------------------------------------------------------------
"------------------------------------------------------------
"------------------------------------------------------------
"------------------------------------------------------------
" =Cosmetics
"------------------------------------------------------------
"------------------------------------------------------------
"------------------------------------------------------------
"------------------------------------------------------------
" Colors, font
if has("gui_running")
	if has("gui_gtk2") 				" =========Linux
		set guioptions -=T "no toolbar
		set guioptions -=m "no menubar
		"set guifont=DejaVu\ Sans\ Mono\ Regular\ 11
		set guifont=Meslo\ LG\ S\ DZ\ Regular\ 11
		colorscheme candy-crush-chronicle
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
	highlight SyntasticErrorSign guifg=#cccccc guibg=#7D4D4D	" ^ Doesn't
	highlight YCMErrorSign guifg=#cccccc guibg=#7D4D4D			" | work
	highlight YCMErrorLine guifg=#cccccc guibg=#7D4D4D			" |
	highlight SyntasticWarningSign guifg=#cccccc guibg=#976D4F 	" |
	highlight YCMWarningSign guifg=#cccccc guibg=#976D4F 		" |
	highlight YCMWarningLine guifg=#cccccc guibg=#976D4F 		" v

	if &background == 'black'
		hi Search guibg=#303220 guifg=NONE
	elseif &background == 'light'
		hi Search guibg=#708559 guifg=NONE
	endif
else
	if s:uname == "Darwin"
		colorscheme lucius
		LuciusLight
	else
		"colorscheme candy-crush-chronicle
		colors lucius
		let g:lucius_no_term_bg = 1
	endif
endif

"Highlight the nth column so you know when lines get too long
autocmd Filetype vim,sh,c,cpp,c#,javascript,java,jade,css,scss 
			\ set colorcolumn=78 


"set foldcolumn=2

"===Airline
" Symbols for non-Powerline-patched fonts
if exists('g:airline_unicode_symbols')
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
endif

" Always show sign gutter (Doesn't work with YCM because YCM clears
	" the signs)
"autocmd BufEnter * sign define dummy
"autocmd BufEnter * execute 'sign place 9999 line=1
"	  \ name=dummy buffer=' . bufnr('')

"------------------------------------------------------------
"------------------------------------------------------------
"------------------------------------------------------------
"------------------------------------------------------------
" =Settings
"------------------------------------------------------------
"------------------------------------------------------------
"------------------------------------------------------------
"------------------------------------------------------------

"=== Store swap (.swp, .swo) files in a central location
set directory=~/.vim/tmp

"=== Activate mouse in terminal
set mouse=a

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
"set list "
"Enables crlf glyph

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

"========================
"=Completion
"========================
"longest: insert longest common text of all matches
"menuone: pop-up menu even if there's only one match
set completeopt=longest,menuone

"=== #syntastic
let g:syntastic_check_on_open=1
"let g:syntastic_cpp_compiler = 'clang++'
let g:syntastic_cpp_compiler_options = ' -std=c++10' " -stdlib=libc++'
let g:syntastic_error_symbol = '✗' 		"^sign interface symbols
let g:syntastic_warning_symbol = '!'	"v
let g:syntastic_html_tidy_quiet_messages = { "level" : "warnings" }
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
      let vim_instance =
		  \(g:screen_size_by_vim_instance==1?(v:servername):'GVIM')
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
      let vim_instance =
		  \(g:screen_size_by_vim_instance==1?(v:servername):'GVIM')
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

"------------------------------------------------------------
"------------------------------------------------------------
"------------------------------------------------------------
"------------------------------------------------------------
" =Keys Mappings, Keybinds, Bindings
"------------------------------------------------------------
"------------------------------------------------------------
"------------------------------------------------------------
"------------------------------------------------------------
let mapleader=","

" Remap the numpad Enter key to Esc
map! <kEnter> <Esc>
map <kEnter> <Esc>
" (works with $TERM=xterm)
map <OM> <Esc>

"======Movement
nnoremap <silent> K 5gk
vnoremap <silent> K 5gk
nnoremap <silent> J 5gj
vnoremap <silent> J 5gj

nnoremap j gj	" ^ Move vertically by visual line
nnoremap k gk	" v

nnoremap <C-k> <Esc>:normal! J<CR>

"nmap <tab> %
"vmap <tab> %

"======for AZERTY keyboards (bypass Shift, ...)
map ù %
map à @

"======Windows
"Note: In Terminal Vim <tab> is the same as <C-i>
nnoremap <tab> <C-w>

nmap <tab>e :tabn<CR>
nmap <tab>z :tabp<CR>

nnoremap <silent> + :exe "resize " . (winheight(0) * 3/2)<CR>
nnoremap <silent> - :exe "resize " . (winheight(0) * 2/3)<CR>


" Backspace
noremap <BS> <C-o>
noremap <C-o> <C-i>

"=======
nnoremap gV `[v`] " Highlight last inserted text

map <leader>f :NERDTreeToggle<CR>

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

"------------------------------------------------------------
" =Commands
"------------------------------------------------------------
" CDC = Change to Directory of Current file
command CDC cd %:p:h

command NT NERDTreeToggle

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
