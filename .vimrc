" Note: Skip initialization for vim-tiny or vim-small.
if 0 | endif

set langmenu=en_US.UTF-8

if &compatible
  set nocompatible
endif

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

call plug#begin('~/.vim/plugged')

" =Bundle list
"____________________________________________________________

Plug 'tpope/vim-sensible' "Configurations everyone agree on

Plug 'scrooloose/syntastic' "Syntax checking plugin (options at #syntastic

if has("unix")
	" :Remove :Move :Rename :Chmod :SudoWrite :SudoEdit
Plug 'tpope/vim-eunuch'
endif

" "YouCompleteMe, YCM
" "if has("win32")
" "	Plug $HOME . '/.vim/ycm_win64' "Code completion engine
" "	"let g:yce_path_to_python_interpreter = 'C:/Python27/python.exe'
" "elseif has("unix")
" Plug 'Valloric/YouCompleteMe'
" "endif
" autocmd FileType c let g:ycm_global_ycm_extra_conf =
" 			\ '~/.vim/ycm_files/c/.ycm_extra_conf.py'
" autocmd FileType cpp let g:ycm_global_ycm_extra_conf =
" 			\ '~/.vim/ycm_files/cpp/.ycm_extra_conf.py'
" let g:ycm_server_keep_logfiles = 1
" let g:ycm_server_log_level = 'debug'

" =Neocomplete
Plug 'Shougo/neocomplete.vim'

" Makes . command work with more commands
Plug 'tpope/vim-repeat'

Plug 'Konfekt/FastFold'

Plug 'Shougo/vimproc.vim', {'build' : 'make'}
"call dein#add('Shougo/vimproc', {
"      \ 'build' : {
"      \     'windows' : 'tools\\update-dll-mingw',
"      \     'cygwin' : 'make -f make_cygwin.mak',
"      \     'mac' : 'make',
"      \     'linux' : 'make',
"      \     'unix' : 'gmake',
"      \    }
"      \ })

Plug 'm2mdas/phpcomplete-extended', {'for': 'php'} " (r: Shougo/vimproc)
autocmd  FileType  php setlocal omnifunc=phpcomplete_extended#CompletePHP

Plug 'junegunn/vim-easy-align'
" Start interactive EasyAlign in visual mode (e.g. vip<Enter>)
vmap <Enter> <Plug>(EasyAlign)
" Start interactive EasyAlign for a motion/text object (e.g. gaip)
nmap ga <Plug>(EasyAlign)

" Session management
" Use :Obsess (with optional file/directory name) to start recording to a
" session file and :Obsess! to stop and throw it away. That's it. Load a session
" in the usual manner: vim -S, or :source it.
Plug 'tpope/vim-obsession'

" =File/Buffer management
"============================================================
"File browser
Plug 'scrooloose/nerdtree', {'on': ['NERDTreeToggle', 'NERDTreeFind']}
"Plug 'jistr/vim-nerdtree-tabs' "NERDTree independent of tabs
"Fuzzy file, buffer, mru, tag, etc finder (Active fork of kien/)
" :help ctrlp-mappings
Plug 'ctrlpvim/ctrlp.vim'
command! MRU CtrlPMRU

" Ctrl + f global search [https://github.com/dyng/ctrlsf.vim#arguments]
" #CtrlSF config
Plug 'dyng/ctrlsf.vim'

" Git wrapper (:Gwrite (=git add), :Gcommit, :Gpush, :Gstatus, :Gbrowse)
Plug 'tpope/vim-fugitive'
" Show diff in gutter
Plug 'airblade/vim-gitgutter'
let g:gitgutter_sign_column_always = 1

"
"Source code browser (supports C/C++, java, perl, python, tcl, sql, php,
" etc) [v4.6, vim-scripts/ branch isn't updated]
"Plug 'ChoiZ/taglist.vim' { 'on': ['TlistOpen', 'TlistToggle'] }

Plug 'tomtom/tcomment_vim' "Comment toggle, handles embedded filetypes

"buffer/file/command/tag/etc explorer with fuzzy matching :FufHelp
"Plug 'vim-scripts/FuzzyFinder'

" call dein#add('wincent/command-t', {
"     \   'build_commands': ['make', 'ruby'],
"     \   'build': {
"     \      'unix': 'cd ruby/command!-t && { make clean; ruby extconf.rb && make }'
"     \   }
"     \ })  "Fuzzy file finding

Plug 'kana/vim-gf-user' "Improvements to 'gf', open file under cursor

"Plug 'sjl/gundo.vim' "Undo tree visualization
"nnoremap <leader>u :GundoToggle<CR>

"Delete surroundings: ds*, Change surroundings: cs**,
" Surround: ys<move>*, Surround line: yss* (req: nocompatible)
Plug 'tpope/vim-surround'
Plug 'tpope/vim-repeat' "Enable repeating supported plugin maps with .

" <leader>c Rename tag
Plug 'othree/xml.vim'

"Easily search for, substitute, and abbreviate mutltiple variants of a word
" foobar -> FooBAR ; Foobar x FooBAR
Plug 'tpope/vim-abolish'

Plug 'junegunn/vim-pseudocl' " Requirement for vim-oblique
Plug 'junegunn/vim-oblique' " Improved /-search, (r: vim-oblique)
"Automatically clears search highlight
"Plug 'pgdouyon/vim-evanesco' "if no vim-oblique

" Show "Match 123 of 456 /search term/" in Vim searches
Plug 'henrik/vim-indexed-search'

"Plug 'reedes/vim-pencil' "Rethinking Vim as a tool for writing

"More complete emacs-mode mappings for Vim command line (Alt-B, Alt-F, etc)
Plug 'bruno-/vim-husk'

" Config #multiple-cursors
Plug 'terryma/vim-multiple-cursors'

" Editorconfig support, allows easily setting editor options on a
"	per-project basis. Sample .editorconfig:
"	root=true \n[*] \nindent_size = 4 \nindent_style = tab
Plug 'editorconfig/editorconfig-vim'

Plug 'christoomey/vim-tmux-navigator'
let g:tmux_navigator_no_mappings = 1
nnoremap <silent> <C-w>h :TmuxNavigateLeft<cr>
nnoremap <silent> <C-w>j :TmuxNavigateDown<cr>
nnoremap <silent> <C-w>k :TmuxNavigateUp<cr>
nnoremap <silent> <C-w>l :TmuxNavigateRight<cr>
inoremap <silent> <C-w>h <Esc>:TmuxNavigateLeft<cr>
inoremap <silent> <C-w>j <Esc>:TmuxNavigateDown<cr>
inoremap <silent> <C-w>k <Esc>:TmuxNavigateUp<cr>
inoremap <silent> <C-w>l <Esc>:TmuxNavigateRight<cr>



" =Languages specific, syntax
"============================================================
"awk, bash, c, git, latex, lua, matlab, & perl support
"Plug 'WolfgangMehner/vim-plugins'
"=== C & C++
"Switch between source and header files in C/C++ code (:A, :AT (new tab))
Plug 'fanchangyong/a.vim', {'for': ['c', 'cpp']}
"if has("win32") " Add standard library headers to path on Windows
"	let &path.='D:/Qt/Tools/mingw482_32/i686-w64-mingw32/include,'
"endif
"=== Haskell
Plug 'eagletmt/ghcmod-vim', {'for': ['haskell']}
"=== Rust
"Support for Rust file detection and syntax highlighting
Plug 'rust-lang/rust.vim', {'for': ['rust']}
"=== Python
" https://github.com/python-mode/python-mode
" <C-c>g for :RopeGotoDefinition
Plug 'python-mode/python-mode'
let g:pymode_doc_bind = "<C-h>"
Plug 'tmhedberg/SimpylFold'
let g:SimpylFold_docstring_preview = 1

"=== JavaScript
" JavaScript code-analysis engine (r: eslint (npm i -g),
" 	cd ~/.vim/{PLUGINS_DIR}/tern_for_vim && npm install)
" Plug 'marijnh/tern_for_vim', { 'build': {'others': 'npm install'},
"                                          \ 'if': 'executable("npm")',
"                                          \ 'for': 'javascript'}
Plug 'marijnh/tern_for_vim'
" For syntax checking:
" $ npm install -g eslint babel-eslint eslint-plugin-react

Plug 'pangloss/vim-javascript', {'for': 'javascript'}
let g:javascript_opfirst = 0
" Support Flow-type syntax highlighting
let g:javascript_plugin_flow = 1
" let g:javascript_conceal_arrow_function       = "⇒"

" YAJS doesn't include indent so keep vim-js-indent on old Vim version
" Plug 'othree/yajs.vim', {'for': 'javascript'}
" Plug 'othree/es.next.syntax.vim', {'for': 'javascript'}
" Plug 'gavocanov/vim-js-indent', {'for': 'javascript'}
" extends syntax for with jQuery,backbone,etc.
Plug 'othree/javascript-libraries-syntax.vim', {'for': 'javascript'}

Plug 'mxw/vim-jsx', {'for': ['javascript', 'javascript.jsx', 'html']}
let g:jsx_ext_required = 0 " use JSX syntax in .js files too

" https://github.com/flowtype/vim-flow
" r: flow-bin (npm install -g flow-bin | yarn global add flow-bin)
Plug 'flowtype/vim-flow', {'for': 'javascript' }
let g:flow#autoclose = 1 " Auto close |quickfix| window opened if no error

" =CoffeeScript syntax, indentating, compiling, and more.
Plug 'kchmck/vim-coffee-script', {'for': ['coffee', 'cson']}

" =TypeScript
Plug 'leafgarland/typescript-vim', {'for': ['typescript']}
Plug 'Quramy/tsuquyomi', {'for': ['typescript']}

" Plug 'moll/vim-node' "Node.js tools and utilities						^
" Plug 'guileen/vim-node-dict' "node.js dictionary 				 		|
" Plug 'ahayman/vim-nodejs-complete' "node.js omnifunc function of vi	v

" =ES2015 template string
" :JsPreTmpl {filetype}
Plug 'Quramy/vim-js-pretty-template'

"===GraphQL
Plug 'paulrosania/vim-graphql', { 'branch': 'tagged-template-literals' }
" Highlight GraphQL in JavaScriptp template strings
let g:graphql_tag_names = ['gql', 'graphql']

"=== HTML
Plug 'othree/html5.vim', {'for': ['html']}
"=Jade/Pug
Plug 'digitaltoad/vim-jade', {'for': ['jade']}
Plug 'digitaltoad/vim-pug', {'for': ['pug']}
"===CSS
", {'for': ['css']} "Highlight colors in CSS files
Plug 'ap/vim-css-color'
Plug 'hail2u/vim-css3-syntax', {'for': ['css']}
" CSS syntax highlighting for CSS-in-JS npm module styled-components
Plug 'fleischie/vim-styled-components', {'for': ['javascript']}
"=SCSS
Plug 'cakebaker/scss-syntax.vim', {'for': ['scss']}
"===C#, CSharp
Plug 'OmniSharp/omnisharp-vim', {'for': 'cs'}
"===Swift
Plug 'keith/swift.vim', {'for': 'swift'}
"===Markdown
Plug 'plasticboy/vim-markdown', {'for': 'markdown'}
"===CMake
Plug 'vim-scripts/cmake', {'for': 'cmake'} "syntax update
Plug 'vim-scripts/cmake.vim', {'for': 'cmake'} "indent


Plug 'Yggdroot/indentLine' "visual indent guides with thin vertical lines
let g:indentLine_conceallevel = 1 " Default: 2
Plug 'Raimondi/delimitMate' "Automatically add closing brackets and quotes

" =Cosmetic
"============================================================
Plug 'gcavallanti/vim-noscrollbar' " Horizontal scroll indicator

Plug 'junegunn/rainbow_parentheses.vim' "Simpler Rainbow Parentheses
let g:rainbow#pairs = [['(', ')'], ['[', ']']]
let g:rainbow#blacklist = [ 0 ]

" :FixWhitespace
Plug 'bronson/vim-trailing-whitespace'

"visual indent guides with bg color, toggle with <leader>ig
"Plug 'nathanaelkane/vim-indent-guides'

"Plug 'epage/vim-autohighlight' " Disabled, 404
Plug 'obxhdx/vim-auto-highlight'
" AutoHighlightWord

Plug 'tiagofumo/vim-nerdtree-syntax-highlight'
let g:NERDTreeFileExtensionHighlightFullName = 1
let g:NERDTreeExactMatchHighlightFullName = 1
let g:NERDTreePatternMatchHighlightFullName = 1
" Highlight folders using exact match
let g:NERDTreeHighlightFolders = 1 " enables folder icon highlighting using exact match
let g:NERDTreeHighlightFoldersFullName = 1 " highlights the folder name

" Plug 'ryanoasis/vim-devicons'

" =Colorschemes, Colors
"============================================================
"Plug 'xolox/vim-misc' " Dependency for vim-colorscheme-switcher
"" Cycle through colorschemes with F8/Shift-F8
"Plug 'xolox/vim-colorscheme-switcher'
"------------------------------------------------------------
Plug 'tomasr/molokai'
"Plug 'altercation/vim-colors-solarized'
"Plug 'Lokaltog/vim-distinguished'
"Plug 'chriskempson/base16-vim' "Many great themes
"Plug 'atelierbram/vim-colors_atelier-schemes'
"Plug 'ciaranm/inkpot' "Plurple-pink-yellow
Plug 'junegunn/seoul256.vim'
"Plug 'Junza/Spink' "Low color contrast brownish theme
"Plug 'zenorocha/dracula-theme', {'rtp': 'vim/'}
"Plug 'fugalh/desert.vim' "Term/GUI, dark
"Plug 'kristiandupont/shades-of-teal' "GUI, dark, blueish, low-contrast
"Plug 'sandeepsinghmails/Dev_Delight' "GUI, Light, colorful
"Plug 'jonathanfilip/vim-lucius' "GUI/256Term
Plug 'lleaff/candy-crush-chronicle.vim' "GUI/256Term


"____________________________________________________________
" END Plugin manager configuration
"____________________________________________________________
" Required:
call plug#end()

" Required:
filetype plugin indent on

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
" GUI
if has("gui_running")
	if has("gui_gtk2") 				" =========Linux
		set guioptions -=T "no toolbar
		set guioptions -=m "no menubar
		"set guifont=DejaVu\ Sans\ Mono\ Regular\ 11
		" set guifont=Meslo\ LG\ S\ DZ\ Regular\ 11
		set guifont=DejaVu\ Sans\ Mono\ 10
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
  " Terminal
	if s:uname == "Darwin" " OSX
		" colorscheme lucius
		" LuciusLight
		colors seoul256-light
		let g:lucius_no_term_bg = 1
	else " Linux
		"colorscheme candy-crush-chronicle
		colors seoul256
		" Make terminal bg color transparent (at 'Tail' anchor)

		"let g:lucius_no_term_bg = 1 " For lucius and candy-crush-chronicle themes
	endif
endif

"Highlight the nth column so you know when lines get too long
autocmd Filetype vim,sh,c,cpp,c#,javascript,java,jade,css,scss,swift,python
			\ set colorcolumn=81

" Automatic word highlight plugin
hi AutoHighlightWord ctermbg=238

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
" Better mouse support https://stackoverflow.com/a/19253251/4718923
if has("mouse_sgr")
  set ttymouse=sgr
else
  set ttymouse=xterm2
end

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

if has("unix")
  set listchars=tab:>-,eol:↵
endif

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

" The Platinum Searcher
" https://github.com/monochromegane/the_platinum_searcher/releases/latest
if executable('pt')
  " Use pt over grep
  set grepprg=pt\ --nogroup\ --nocolor
  " Use pt in CtrlP for listing files. Lightning fast and respects .gitignore
  let g:ctrlp_user_command = 'pt %s -l --nocolor -g ""'
  " pt is fast enough that CtrlP doesn't need to cache
  let g:ctrlp_use_caching = 0
endif

" :Grepper, :GrepperGit
Plug 'mhinz/vim-grepper'

"========================
"=Completion
"========================
"=== #syntastic
"
set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*


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
" https://upload.wikimedia.org/wikipedia/commons/1/15/Xterm_256color_chart.svg
" ctermbg=215 ctermfg=255
highlight SyntasticErrorSign guifg=#cccccc guibg=#7D4D4D   ctermbg=088 ctermfg=015
highlight SyntasticWarningSign guifg=#cccccc guibg=#976D4F ctermbg=130 ctermfg=255
" highlight SyntasticError guifg=#cccccc guibg=#7D4D4D       ctermbg=124 ctermfg=255
" highlight SyntasticWarning guifg=#cccccc guibg=#976D4F     ctermbg=130 ctermfg=255
" highlight SpellBad guifg=#cccccc guibg=#7D4D4D             ctermbg=215 ctermfg=255
" highlight SpellCap guifg=#cccccc guibg=#976D4F             ctermbg=215 ctermfg=255

" Needs eslint and eslint_d to be installed (npm install -g eslint eslint_d)
let g:syntastic_javascript_checkers = ['eslint']
" eslint_d keeps a background eslint process running so it doesn't have to be
"  restarted on each check.
let g:syntastic_javascript_eslint_exec = 'eslint_d'
" sudo -H pip3 install flake8
let g:syntastic_python_checkers=['flake8', 'python3']

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
autocmd FileType javascript,javascript.jsx setlocal omnifunc=javascriptcomplete#CompleteJS
autocmd FileType python setlocal omnifunc=pythoncomplete#Complete
autocmd FileType xml setlocal omnifunc=xmlcomplete#CompleteTags


" =NERDTree configuration
"------------------------------------------------------------
let NERDTreeIgnore=['.\.o$', '^__pycache__$', '.\.pyc$', '^.git$']


"#######################################################
"############### =Filetypes              ###############
"#######################################################
" Map file extensions to filetype

autocmd BufRead,BufNewFile .babelrc set filetype=json
autocmd BufRead,BufNewFile Dockerfile[-^][^.]* set filetype=dockerfile
autocmd BufRead,BufNewFile env-template set filetype=sh

"============================================================


"==============Status line
"Default status line: set statusline=%<%f\ %h%m%r%=%-14.(%l,%c%V%)\ %P

set statusline=%<%f\ %h%m%r%=%-14.(%l,%c%V%)\ %{noscrollbar#statusline(20,'┄','━',['╼'],['╾'])}

set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

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

"#######################################################
"############### =Templates              ###############
"#######################################################

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

nmap > >>
nmap < <<

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

map <C-b> gE

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
nnoremap <BS> <C-o>
"nnoremap <C-o> <C-i>

" vim-tmux-navigator
"let g:tmux_navigator_no_mappings = 1 " Not working for some reason
"nnoremap <silent> <C-w-h> :TmuxNavigateLeft<cr>
"nnoremap <silent> <C-w-j> :TmuxNavigateDown<cr>
"nnoremap <silent> <C-w-k> :TmuxNavigateUp<cr>
"nnoremap <silent> <C-w-l> :TmuxNavigateRight<cr>

nmap <leader>t :tabnew<CR>

nmap <leader>q :q<CR>
nmap <leader>x :x<CR>

" Git fugitive shortcuts
nmap <leader>gb :Gblame<CR>

" #CtrlSF config
"===============
" search starting from project root (looks backward for .git, ...)
" let g:ctrlsf_default_root = 'project'
" --home-ptignore: use .ptignore file in $home
let g:ctrlsf_extra_backend_args = {
    \ 'pt': '--home-ptignore'
    \ }
" defines ctrlsf using literal pattern or regular expression pattern as default.
"  default value is 0, which means literal pattern.
let g:ctrlsf_regex_pattern = 1
" left, right, top and bottom
let g:ctrlsf_position = 'bottom'
" window height/width, can be percent or absolute: '30%' or '200'
" let g:ctrlsf_winsize = '30%'
" input :ctrlsf in command line for you, just a handy shortcut.
nmap <C-f>f <plug>CtrlSFPrompt
" input :ctrlsf foo in command line where foo is the current visual selected
"  word, waiting for further input.
"vmap <C-f>f <plug>ctrlsfvwordpath
" like <plug>ctrlsfvwordpath, but execute it immediately.
vmap <C-f> <plug>CtrlSFVWordExec
" input :ctrlsf foo in command line where foo is word under the cursor.
nmap <C-f>n <plug>CtrlSFCWordPath
" input :ctrlsf foo in command line where foo is word under the cursor, adding
"  word boundary around searching word.
nmap <C-f>b <plug>CtrlSFCCWordPath
" Input :CtrlSF foo in command line where foo is the last search pattern of vim.
nmap <C-f>p <Plug>CtrlSFPWordPath
" Open/Toggle the global find window
" nnoremap <C-f>o :CtrlSFOpen<CR>
nnoremap <C-f>o :CtrlSFToggle<CR>
nnoremap <C-f>t :CtrlSFToggle<CR>
inoremap <C-f>t <Esc>:CtrlSFToggle<CR>


"=======
nnoremap gV `[v`] " Highlight last inserted text

" Check if NERDTree is open or active
function! IsNERDTreeOpen()
  return exists("t:NERDTreeBufName") && (bufwinnr(t:NERDTreeBufName) != -1)
endfunction

function! NERDTreeToggleFind()
  if IsNERDTreeOpen()
    NERDTreeToggle
  else
    NERDTreeFind
  endif
endfunc

map <leader>f :call NERDTreeToggleFind()<CR>
map <leader>F :NERDTreeToggle<CR>

function! MoveToPrevTab()
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

function! MoveToNextTab()
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

" command! -nargs=0 -bar UpdateFileAndSave if &modified
" 			\|    if empty(bufname('%'))
" 				\|        browse confirm write
" 				\|    else
" 					\|        confirm write
" 					\|    endif
" 					\|endif
" nnoremap <silent> <C-S> :<C-u>UpdateFileAndSave<CR>
" "inoremap <silent> <C-S> :<C-u>UpdateFileAndSave<CR> "TODO: Find how to call function in insert mode

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

" "#multiple-cursors config
" https://github.com/terryma/vim-multiple-cursors#interactions-with-other-plugins
" Called once right before you start selecting multiple cursors
function! Multiple_cursors_before()
  if exists(':NeoCompleteLock')==2
    exe 'NeoCompleteLock'
  endif
endfunction

" Called once only when the multiple selection is canceled (default <Esc>)
function! Multiple_cursors_after()
  if exists(':NeoCompleteUnlock')==2
    exe 'NeoCompleteUnlock'
  endif
endfunction

" Allow quitting multi-cursors insert mode with kj
" https://github.com/terryma/vim-multiple-cursors/issues/67#issuecomment-143547973
inoremap kj <esc>
let g:multi_cursor_exit_from_insert_mode=0
let g:multi_cursor_quit_key='q'
let g:multi_cursor_insert_maps={'k':1}

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
command! CDC cd %:p:h

command! NT NERDTreeToggle

command! Tn tabnew

"------------------------------------------------------------
" =Tail
"    Commands that need to stay at the end
"------------------------------------------------------------
" =Cosmetics
" Relegated to the end since themes seem to load asynchronously ?
hi Normal ctermbg=none
"hi IncSearch ctermbg=red
hi Search ctermbg=153 ctermfg=0

"@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
"@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
"@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
"@@@                                                      @@@
"@@@ =Command line abbreviations                          @@@
"@@@                                                      @@@
"@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
"@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
"@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

" Use %% like $PWD, e.g.: :e %%/someFile
cabbr <expr> %% expand('%:p:h')

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
