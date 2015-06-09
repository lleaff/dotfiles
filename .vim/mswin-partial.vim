" CTRL-X and SHIFT-Del are Cut
vnoremap <C-X> "+x
"vnoremap <S-Del> "+x

" CTRL-C and CTRL-Insert are Copy
vnoremap <C-C> "+y
vnoremap <C-Insert> "+y

" CTRL-V and SHIFT-Insert are Paste
map <C-V>   	"+gP
map <S-Insert>  	"+gP

cmap <C-V>  	<C-R>+
cmap <S-Insert> 	<C-R>+

" Pasting blockwise and linewise selections is not possible in Insert and
" Visual mode without the +virtualedit feature.  They are pasted as if they
" were characterwise instead.
" Uses the paste.vim autoload script.

exe 'inoremap <script> <C-V>' paste#paste_cmd['i']
exe 'vnoremap <script> <C-V>' paste#paste_cmd['v']

imap <S-Insert> 	<C-V>
vmap <S-Insert> 	<C-V>

" Use CTRL-Q and CTRL-A to do what CTRL-V used to do
noremap <C-Q>   	<C-V>
noremap <C-A>   	<C-V>

    " Vim support file to help with paste mappings and menus
" Maintainer:   Bram Moolenaar <Bram@vim.org>
" Last Change:  2006 Jun 23

" Define the string to use for items that are present both in Edit, Popup and
" Toolbar menu.  Also used in mswin.vim and macmap.vim.

" Pasting blockwise and linewise selections is not possible in Insert and
" Visual mode without the +virtualedit feature.  They are pasted as if they
" were characterwise instead.  Add to that some tricks to leave the cursor in
" the right position, also for "gi".
if has("virtualedit")
  let paste#paste_cmd = {'n': ":call paste#Paste()<CR>"}
  let paste#paste_cmd['v'] = '"-c<Esc>' . paste#paste_cmd['n']
  let paste#paste_cmd['i'] = 'x<BS><Esc>' . paste#paste_cmd['n'] . 'gi'

  func! paste#Paste()
    let ove = &ve
    set ve=all
    normal! `^
    if @+ != ''
      normal! "+gP
    endif
    let c = col(".")
    normal! i
    if col(".") < c " compensate for i<ESC> moving the cursor left
      normal! l
    endif
    let &ve = ove
  endfunc
else
  let paste#paste_cmd = {'n': "\"=@+.'xy'<CR>gPFx\"_2x"}
  let paste#paste_cmd['v'] = '"-c<Esc>gix<Esc>' . paste#paste_cmd['n'] . '"_x'
  let paste#paste_cmd['i'] = 'x<Esc>' . paste#paste_cmd['n'] . '"_s'
endif
