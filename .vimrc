" Note: Skip initialization for vim-tiny or vim-small.
if 0 | endif

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

"@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
"@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
"@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
"@@@                                                      @@@
"@@@ =Plugins                                             @@@
"@@@                                                      @@@
"@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
"@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
"@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
" =Neobundle configuration
"____________________________________________________________
"To install Dein:
" curl https://raw.githubusercontent.com/Shougo/dein.vim/master/bin/installer.sh > dein_installer.sh
" sh ./dein_installer.sh {specify the installation directory}"

set runtimepath^=~/.vim/bundle/dein.vim/repos/github.com/Shougo/dein.vim
call dein#begin(expand('~/.vim/bundle/dein.vim'))

" Let dein manage dein
" Required:
call dein#add('Shougo/dein.vim')

" If you want to install not installed plugins on startup.
"if dein#check_install()
"  call dein#install()
"endif

" =Bundle list
"____________________________________________________________

call dein#add('tpope/vim-sensible') "Configurations everyone agree on

call dein#add('scrooloose/syntastic') "Syntax checking plugin (options at #syntastic)

if has("unix")
	" :Remove :Move :Rename :Chmod :SudoWrite :SudoEdit
call dein#add('tpope/vim-eunuch')
endif

" "YouCompleteMe, YCM
" "if has("win32")
" "	Plug $HOME . '/.vim/ycm_win64' "Code completion engine
" "	"let g:yce_path_to_python_interpreter = 'C:/Python27/python.exe'
" "elseif has("unix")
" call dein#add('Valloric/YouCompleteMe')
" "endif
" autocmd FileType c let g:ycm_global_ycm_extra_conf =
" 			\ '~/.vim/ycm_files/c/.ycm_extra_conf.py'
" autocmd FileType cpp let g:ycm_global_ycm_extra_conf =
" 			\ '~/.vim/ycm_files/cpp/.ycm_extra_conf.py'
" let g:ycm_server_keep_logfiles = 1
" let g:ycm_server_log_level = 'debug'

" =Neocomplete
call dein#add('Shougo/neocomplete.vim')


call dein#add('Konfekt/FastFold')

call dein#add('Shougo/vimproc', {'build': 'make'})
call dein#add('m2mdas/phpcomplete-extended', {'on_ft': 'php'}) " (r: Shougo/vimproc)
autocmd  FileType  php setlocal omnifunc=phpcomplete_extended#CompletePHP

call dein#add('junegunn/vim-easy-align')
" Start interactive EasyAlign in visual mode (e.g. vip<Enter>)
vmap <Enter> <Plug>(EasyAlign)
" Start interactive EasyAlign for a motion/text object (e.g. gaip)
nmap ga <Plug>(EasyAlign)

" =File/Buffer management
"============================================================
call dein#add('scrooloose/nerdtree', {'on': 'NERDTreeToggle' }) "File browser
"call dein#add('jistr/vim-nerdtree-tabs') "NERDTree independent of tabs
"Fuzzy file, buffer, mru, tag, etc finder (Active fork of kien/)
" :help ctrlp-mappings
call dein#add('ctrlpvim/ctrlp.vim')
command MRU CtrlPMRU

" Git wrapper (:Gwrite (=git add), :Gcommit, :Gpush, :Gstatus, :Gbrowse)
call dein#add('tpope/vim-fugitive')
"
"Source code browser (supports C/C++, java, perl, python, tcl, sql, php,
" etc) [v4.6, vim-scripts/ branch isn't updated]
"call dein#add('ChoiZ/taglist.vim') { 'on': ['TlistOpen', 'TlistToggle'] }

call dein#add('tomtom/tcomment_vim') "Comment toggle, handles embedded filetypes

"buffer/file/command/tag/etc explorer with fuzzy matching :FufHelp
"call dein#add('vim-scripts/FuzzyFinder')

call dein#add('wincent/command-t') "Fuzzy file finding

call dein#add('kana/vim-gf-user') "Improvements to 'gf', open file under cursor

"call dein#add('sjl/gundo.vim') "Undo tree visualization
"nnoremap <leader>u :GundoToggle<CR>

"Delete surroundings: ds*, Change surroundings: cs**,
" Surround: ys<move>*, Surround line: yss* (req: nocompatible)
call dein#add('tpope/vim-surround')
call dein#add('tpope/vim-repeat') "Enable repeating supported plugin maps with .

"Easily search for, substitute, and abbreviate mutltiple variants of a word
" foobar -> FooBAR ; Foobar x FooBAR
call dein#add('tpope/vim-abolish')

call dein#add('junegunn/vim-pseudocl') " Requirement for vim-oblique
call dein#add('junegunn/vim-oblique') " Improved /-search, (r: vim-oblique)
"Automatically clears search highlight
"call dein#add('pgdouyon/vim-evanesco') "if no vim-oblique

"Plug 'reedes/vim-pencil' "Rethinking Vim as a tool for writing

"More complete emacs-mode mappings for Vim command line (Alt-B, Alt-F, etc)
call dein#add('bruno-/vim-husk')

call dein#add('terryma/vim-multiple-cursors')

" Editorconfig support, allows easily setting editor options on a
"	per-project basis. Sample .editorconfig:
"	root=true \n[*] \nindent_size = 4 \nindent_style = tab
call dein#add('editorconfig/editorconfig-vim')

call dein#add('christoomey/vim-tmux-navigator')
let g:tmux_navigator_no_mappings = 1
nnoremap <silent> <C-w> h :TmuxNavigateLeft<cr>
nnoremap <silent> <C-w> j :TmuxNavigateDown<cr>
nnoremap <silent> <C-w> k :TmuxNavigateUp<cr>
nnoremap <silent> <C-w> l :TmuxNavigateRight<cr>

" =Languages specific, syntax
"============================================================
"awk, bash, c, git, latex, lua, matlab, & perl support
"call dein#add('WolfgangMehner/vim-plugins')
"=== C & C++
"Switch between source and header files in C/C++ code (:A, :AT (new tab))
call dein#add('fanchangyong/a.vim', {'for': ['c', 'c++']})
"if has("win32") " Add standard library headers to path on Windows
"	let &path.='D:/Qt/Tools/mingw482_32/i686-w64-mingw32/include,'
"endif
"=== Haskell
call dein#add('eagletmt/ghcmod-vim', {'on_ft': ['haskell']})
"=== Rust
"Support for Rust file detection and syntax highlighting
call dein#add('rust-lang/rust.vim', {'on_ft': ['rust']})
"=== JavaScript
" JavaScript code-analysis engine (r: eslint (npm i -g),
" 	cd ~/.vim/plugged/tern_for_vim && sudo npm install)
call dein#add('marijnh/tern_for_vim', { 'build': {'others': 'npm install'},
                                      \ 'if': 'executable("npm")',
                                      \ 'on_ft': 'javascript'})
" For syntax checking:
" $ npm install -g eslint babel-eslint eslint-plugin-react

" YAJS doesn't include indent so keep vim-js-indent
call dein#add('othree/yajs.vim', {'on_ft': 'javascript', 'for': 'javascript'})
call dein#add('gavocanov/vim-js-indent', {'on_ft': 'javascript'})
" extends syntax for with jQuery,backbone,etc.
call dein#add('othree/javascript-libraries-syntax.vim', {'on_ft': 'javascript'})

call dein#add('mxw/vim-jsx', {'on_ft': ['javascript', 'javascript.jsx', 'html']})
let g:jsx_ext_required = 0 " use JSX syntax in .js files too

call dein#add('beautify-web/js-beautify', {'on_ft': ['javascript', 'javascript.jsx', 'html']})
"beautify using js-beautify based on .editorconfig
"(r: beautify-web/js-beautify)
call dein#add('maksimr/vim-jsbeautify', {'on_ft': ['javascript', 'javascript.jsx', 'html']})
autocmd FileType javascript noremap <buffer>  <c-f> :call JsBeautify()<cr>
" for html
autocmd FileType html noremap <buffer> <c-f> :call HtmlBeautify()<cr>
" for css or scss
autocmd FileType css noremap <buffer> <c-f> :call CSSBeautify()<cr>

 "CoffeeScript syntax, indentating, compiling, and more.
call dein#add('kchmck/vim-coffee-script', {'for': ['coffee', 'cson']})

" Plug 'moll/vim-node' "Node.js tools and utilities						^
" Plug 'guileen/vim-node-dict' "node.js dictionary 				 		|
" Plug 'ahayman/vim-nodejs-complete' "node.js omnifunc function of vi	v
"=== HTML
call dein#add('othree/html5.vim', {'on_ft': ['html']})
"=Jade
call dein#add('digitaltoad/vim-jade', {'on_ft': ['jade']})
"===CSS
call dein#add('ap/vim-css-color') ", {'on_ft': ['css']}) "Highlight colors in CSS files
call dein#add('hail2u/vim-css3-syntax', {'on_ft': ['css']})
"=SCSS
call dein#add('cakebaker/scss-syntax.vim', {'for': ['scss']})
"===C#, CSharp
call dein#add('OmniSharp/omnisharp-vim', {'for': ['cs', 'csharp']})
"===Swift
call dein#add('keith/swift.vim', {'for': ['swift']})
"===Markdown
call dein#add('plasticboy/vim-markdown', {'for': ['md', 'markdown']})
"===CMake
call dein#add('vim-scripts/cmake', {'for': ['cmake']}) "syntax update
call dein#add('vim-scripts/cmake.vim', {'for': ['cmake']}) "indent


call dein#add('Yggdroot/indentLine') "visual indent guides with thin vertical lines
let g:indentLine_conceallevel = 0 " Default: 2
call dein#add('Raimondi/delimitMate') "Automatically add closing brackets and quotes

" =Cosmetic
"============================================================
call dein#add('junegunn/rainbow_parentheses.vim') "Simpler Rainbow Parentheses
let g:rainbow#pairs = [['(', ')'], ['[', ']']]
let g:rainbow#blacklist = [ 0 ]

" :FixWhitespace
call dein#add('bronson/vim-trailing-whitespace')

"visual indent guides with bg color, toggle with <leader>ig
"Plug 'nathanaelkane/vim-indent-guides'

call dein#add('epage/vim-autohighlight')

" =Colorschemes, Colors
"============================================================
"call dein#add('xolox/vim-misc') " Dependency for vim-colorscheme-switcher
"" Cycle through colorschemes with F8/Shift-F8
"call dein#add('xolox/vim-colorscheme-switcher')
"------------------------------------------------------------
"call dein#add('tomasr/molokai')
"call dein#add('altercation/vim-colors-solarized')
"call dein#add('Lokaltog/vim-distinguished')
"call dein#add('chriskempson/base16-vim') "Many great themes
"call dein#add('atelierbram/vim-colors_atelier-schemes')
"call dein#add('ciaranm/inkpot') "Plurple-pink-yellow
call dein#add('junegunn/seoul256.vim')
"call dein#add('Junza/Spink') "Low color contrast brownish theme
"call dein#add('zenorocha/dracula-theme', {'rtp': 'vim/'})
"call dein#add('fugalh/desert.vim') "Term/GUI, dark
"call dein#add('kristiandupont/shades-of-teal') "GUI, dark, blueish, low-contrast
"call dein#add('sandeepsinghmails/Dev_Delight') "GUI, Light, colorful
"call dein#add('jonathanfilip/vim-lucius') "GUI/256Term
"call dein#add('lleaff/candy-crush-chronicle.vim') "GUI/256Term


"____________________________________________________________
" END Dein configuration
"____________________________________________________________
" Required:
call dein#end()
" Required:
filetype plugin indent on

if dein#check_install()
  call dein#install()
endif

"____________________________________________________________


"@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
"@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
"@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
"@@@                                                      @@@
"@@@ =Cosmetics                                           @@@
"@@@                                                      @@@
"@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
"@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
"@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

" =Colors, font
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

	if &background == 'black'
		hi Search guibg=#303220 guifg=NONE
	elseif &background == 'light'
		hi Search guibg=#708559 guifg=NONE
	endif
else
	if s:uname == "Darwin" " OSX
		" colorscheme lucius
		" LuciusLight
		colors seoul256-light
		let g:lucius_no_term_bg = 1
	else " Linux
		"colorscheme candy-crush-chronicle
		colors seoul256-light
		" Make terminal bg color transparent (at 'Tail' anchor)

		let g:lucius_no_term_bg = 1 " For lucius and candy-crush-chronicle themes
	endif
endif

"Highlight the nth column so you know when lines get too long
autocmd Filetype vim,sh,c,cpp,c#,javascript,java,jade,css,scss,swift
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

"@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
"@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
"@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
"@@@                                                      @@@
"@@@ =Settings                                            @@@
"@@@                                                      @@@
"@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
"@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
"@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

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

set conceallevel=0

"set foldmethod=syntax "Disabled for perf issues, using Konfekt/FastFold
"instead
set foldlevel=7 "Open folds N levels when opening file

" Indentation
set smartindent
set shiftwidth=2
set tabstop=2 "A tab is x spaces
set softtabstop=2 "Insert x spaces when tab is pressed
set expandtab  "Always use spaces instead of tabs
set shiftround "Round indent to nearest shiftwidth multiple

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
"=== #syntastic
let g:syntastic_check_on_open=1
let g:syntastic_mode_map = { 'mode': 'active',
                            \ 'active_filetypes': ['python', 'javascript'],
                            \ 'passive_filetypes': [] }
let g:syntastic_javascript_checkers = ['eslint']
" let g:syntastic_always_populate_loc_list = 1
" let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0
"let g:syntastic_cpp_compiler = 'clang++'
let g:syntastic_cpp_compiler_options = ' -std=c++10' " -stdlib=libc++'
let g:syntastic_error_symbol = '✗' 		"^sign interface symbols
let g:syntastic_warning_symbol = '!'	"v
let g:syntastic_html_tidy_quiet_messages = { "level" : "warnings" }
"This needs to be kept near the end or it gets overwritten by smth else
highlight SyntasticErrorSign guifg=#cccccc guibg=#7D4D4D 
highlight SyntasticWarningSign guifg=#cccccc guibg=#976D4F 

" =Neocomplete configuration
"------------------------------------------------------------
let g:neocomplete#enable_at_startup = 1
"let g:neocomplete#sources#syntax#min_keyword_length = 3 " Default: 4

" Plugin key-mappings.
inoremap <expr><C-g>     neocomplete#undo_completion()
inoremap <expr><C-l>     neocomplete#complete_common_string()

" Recommended key-mappings.
" <CR>: close popup and save indent.
inoremap <silent> <CR> <C-r>=<SID>my_cr_function()<CR>
function! s:my_cr_function()
  "return (pumvisible() ? "\<C-y>" : "" ) . "\<CR>"
  " For no inserting <CR> key.
  return pumvisible() ? "\<C-y>" : "\<CR>"
endfunction
" <TAB>: completion.
inoremap <expr><TAB>  pumvisible() ? "\<C-n>" : "\<TAB>"
inoremap <expr><S-TAB>  pumvisible() ? "\<C-p>" : "\<S-TAB>"
" <C-h>, <BS>: close popup and delete backword char.
inoremap <expr><C-h> neocomplete#smart_close_popup()."\<C-h>"
" Close popup by <Space>.
"inoremap <expr><Space> pumvisible() ? "\<C-y>" : "\<Space>"

" AutoComplPop like behavior.
"let g:neocomplete#enable_auto_select = 1

" Shell like behavior(not recommended).
"set completeopt+=longest
"let g:neocomplete#enable_auto_select = 1
"let g:neocomplete#disable_auto_complete = 1
"inoremap <expr><TAB>  pumvisible() ? "\<Down>" : "\<C-x>\<C-u>"

" Enable omni completion.
autocmd FileType css setlocal omnifunc=csscomplete#CompleteCSS
autocmd FileType html,markdown setlocal omnifunc=htmlcomplete#CompleteTags
autocmd FileType javascript setlocal omnifunc=javascriptcomplete#CompleteJS
autocmd FileType python setlocal omnifunc=pythoncomplete#Complete
autocmd FileType xml setlocal omnifunc=xmlcomplete#CompleteTags

"============================================================


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

"@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
"@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
"@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
"@@@                                                      @@@
"@@@ =Keys Mappings, =Keybinds, =Bindings, =Mappings      @@@
"@@@                                                      @@@
"@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
"@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
"@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

let mapleader=","

"QWERTY
map ; :

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

map  H ^
nmap H ^
vmap H ^
map  L $
nmap L $
vmap L $

nnoremap j gj	" ^ Move vertically by visual line
vnoremap j gj	" |
nnoremap k gk	" |
vnoremap k gk	" v

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
nmap <tab>w :tabp<CR>
nmap <tab>z :tabp<CR>  "Azerty

nnoremap <silent> + :exe "resize " . (winheight(0) * 3/2)<CR>
nnoremap <silent> - :exe "resize " . (winheight(0) * 2/3)<CR>

imap kj <Esc>

" Backspace
noremap <BS> <C-o>
noremap <C-o> <C-i>

"=======
nnoremap gV `[v`] " Highlight last inserted text

map <leader>f :NERDTreeToggle<CR>

function MoveToPrevTab()
  "there is only one window
  if tabpagenr('$') == 1 && winnr('$') == 1
    return
  endif
  "preparing new window
  let l:tab_nr = tabpagenr('$')
  let l:cur_buf = bufnr('%')
  if tabpagenr() != 1
    close!
    if l:tab_nr == tabpagenr('$')
      tabprev
    endif
    sp
  else
    close!
    exe "0tabnew"
  endif
  "opening current buffer in new window
  exe "b".l:cur_buf
endfunc

function MoveToNextTab()
  "there is only one window
  if tabpagenr('$') == 1 && winnr('$') == 1
    return
  endif
  "preparing new window
  let l:tab_nr = tabpagenr('$')
  let l:cur_buf = bufnr('%')
  if tabpagenr() < tab_nr
    close!
    if l:tab_nr == tabpagenr('$')
      tabnext
    endif
    sp
  else
    close!
    tabnew
  endif
  "opening current buffer in new window
  exe "b".l:cur_buf
endfunc

nmap <tab>E :call MoveToNextTab()<CR>
nmap <tab>Z :call MoveToPrevTab()<CR>

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

"@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
"@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
"@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
"@@@                                                      @@@
"@@@ =Commands                                            @@@
"@@@                                                      @@@
"@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
"@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
"@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
" CDC = Change to Directory of Current file
command CDC cd %:p:h

command NT NERDTreeToggle

"------------------------------------------------------------
" =Tail
"    Commands that need to stay at the end
"------------------------------------------------------------
" =Cosmetics
" Relegated to the end since themes seem to load asynchronously ?
hi Normal ctermbg=none
"hi IncSearch ctermbg=red
hi Search ctermbg=153 ctermfg=0

"@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
"@@@@@@@@@@@@@@@@@@@@@@@@@@@ How to install Vim @@@@@@@@@@@@@@@@@@@@@@@@@@@
"@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
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
