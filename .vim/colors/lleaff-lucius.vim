" ============================================================================
" Name:     lleaff-lucius
" Author:   lleaff
" Version:  0.0.1
" ============================================================================
" Name:     Lucius vim color scheme
" Author:   Jonathan Filip <jfilip1024@gmail.com>
" Version:  8.1.7
" ----------------------------------------------------------------------------
"
" Options:
"
" g:lucius_contrast  (default: 'normal')
"
" This option determines the contrast to use for text/ui elements. It can be
" set to 'low', 'normal', or 'high'. At this time there is no 'high' for the
" light scheme.
"
" g:lucius_contrast_bg  (default: 'normal')
"
" Setting this option makes the background a higher contrast. Current settings
" are 'normal' and 'high'.
"
" g:lucius_use_bold (default: 1)
"
" Setting this will cause the color scheme to use bold fonts for some items.
"
" g:lucius_use_underline (default: 1)
"
" Setting this will cause the color scheme to use underlined fonts for some
" items.
"
" g:lucius_no_term_bg (default: 0)
"
" Setting this will cause the color scheme to not set a background color in
" the terminal (useful for transparency or terminals with different background
" colors).
"
" ============================================================================


" ============================================================================
" Options:
" ============================================================================

unlet! g:colors_name
hi clear
"if exists("syntax_on")
""    syntax reset
"endif

if exists("g:lucius_use_bold")
    let s:use_bold = g:lucius_use_bold
else
    let s:use_bold = 1
endif

if exists("g:lucius_use_underline")
    let s:use_underline = g:lucius_use_underline
else
    let s:use_underline = 1
endif

if exists("g:lucius_no_term_bg")
    let s:no_term_bg = g:lucius_no_term_bg
else
    let s:no_term_bg = 0
endif


" ============================================================================
" Color Map:
" ============================================================================

let s:color_map = {
    \ 'bg': 'bg', 'fg': 'fg', 'NONE': 'NONE',
    \ '#000000':  '16', '#00005f':  '17', '#000087':  '18', '#0000af':  '19',
    \ '#0000d7':  '20', '#0000ff':  '21', '#005f00':  '22', '#005f5f':  '23',
    \ '#005f87':  '24', '#005faf':  '25', '#005fd7':  '26', '#005fff':  '27',
    \ '#008700':  '28', '#00875f':  '29', '#008787':  '30', '#0087af':  '31',
    \ '#0087d7':  '32', '#0087ff':  '33', '#00af00':  '34', '#00af5f':  '35',
    \ '#00af87':  '36', '#00afaf':  '37', '#00afd7':  '38', '#00afff':  '39',
    \ '#00d700':  '40', '#00d75f':  '41', '#00d787':  '42', '#00d7af':  '43',
    \ '#00d7d7':  '44', '#00d7ff':  '45', '#00ff00':  '46', '#00ff5f':  '47',
    \ '#00ff87':  '48', '#00ffaf':  '49', '#00ffd7':  '50', '#00ffff':  '51',
    \ '#5f0000':  '52', '#5f005f':  '53', '#5f0087':  '54', '#5f00af':  '55',
    \ '#5f00d7':  '56', '#5f00ff':  '57', '#5f5f00':  '58', '#5f5f5f':  '59',
    \ '#5f5f87':  '60', '#5f5faf':  '61', '#5f5fd7':  '62', '#5f5fff':  '63',
    \ '#5f8700':  '64', '#5f875f':  '65', '#5f8787':  '66', '#5f87af':  '67',
    \ '#5f87d7':  '68', '#5f87ff':  '69', '#5faf00':  '70', '#5faf5f':  '71',
    \ '#5faf87':  '72', '#5fafaf':  '73', '#5fafd7':  '74', '#5fafff':  '75',
    \ '#5fd700':  '76', '#5fd75f':  '77', '#5fd787':  '78', '#5fd7af':  '79',
    \ '#5fd7d7':  '80', '#5fd7ff':  '81', '#5fff00':  '82', '#5fff5f':  '83',
    \ '#5fff87':  '84', '#5fffaf':  '85', '#5fffd7':  '86', '#5fffff':  '87',
    \ '#870000':  '88', '#87005f':  '89', '#870087':  '90', '#8700af':  '91',
    \ '#8700d7':  '92', '#8700ff':  '93', '#875f00':  '94', '#875f5f':  '95',
    \ '#875f87':  '96', '#875faf':  '97', '#875fd7':  '98', '#875fff':  '99',
    \ '#878700': '100', '#87875f': '101', '#878787': '102', '#8787af': '103',
    \ '#8787d7': '104', '#8787ff': '105', '#87af00': '106', '#87af5f': '107',
    \ '#87af87': '108', '#87afaf': '109', '#87afd7': '110', '#87afff': '111',
    \ '#87d700': '112', '#87d75f': '113', '#87d787': '114', '#87d7af': '115',
    \ '#87d7d7': '116', '#87d7ff': '117', '#87ff00': '118', '#87ff5f': '119',
    \ '#87ff87': '120', '#87ffaf': '121', '#87ffd7': '122', '#87ffff': '123',
    \ '#af0000': '124', '#af005f': '125', '#af0087': '126', '#af00af': '127',
    \ '#af00d7': '128', '#af00ff': '129', '#af5f00': '130', '#af5f5f': '131',
    \ '#af5f87': '132', '#af5faf': '133', '#af5fd7': '134', '#af5fff': '135',
    \ '#af8700': '136', '#af875f': '137', '#af8787': '138', '#af87af': '139',
    \ '#af87d7': '140', '#af87ff': '141', '#afaf00': '142', '#afaf5f': '143',
    \ '#afaf87': '144', '#afafaf': '145', '#afafd7': '146', '#afafff': '147',
    \ '#afd700': '148', '#afd75f': '149', '#afd787': '150', '#afd7af': '151',
    \ '#afd7d7': '152', '#afd7ff': '153', '#afff00': '154', '#afff5f': '155',
    \ '#afff87': '156', '#afffaf': '157', '#afffd7': '158', '#afffff': '159',
    \ '#d70000': '160', '#d7005f': '161', '#d70087': '162', '#d700af': '163',
    \ '#d700d7': '164', '#d700ff': '165', '#d75f00': '166', '#d75f5f': '167',
    \ '#d75f87': '168', '#d75faf': '169', '#d75fd7': '170', '#d75fff': '171',
    \ '#d78700': '172', '#d7875f': '173', '#d78787': '174', '#d787af': '175',
    \ '#d787d7': '176', '#d787ff': '177', '#d7af00': '178', '#d7af5f': '179',
    \ '#d7af87': '180', '#d7afaf': '181', '#d7afd7': '182', '#d7afff': '183',
    \ '#d7d700': '184', '#d7d75f': '185', '#d7d787': '186', '#d7d7af': '187',
    \ '#d7d7d7': '188', '#d7d7ff': '189', '#d7ff00': '190', '#d7ff5f': '191',
    \ '#d7ff87': '192', '#d7ffaf': '193', '#d7ffd7': '194', '#d7ffff': '195',
    \ '#ff0000': '196', '#ff005f': '197', '#ff0087': '198', '#ff00af': '199',
    \ '#ff00d7': '200', '#ff00ff': '201', '#ff5f00': '202', '#ff5f5f': '203',
    \ '#ff5f87': '204', '#ff5faf': '205', '#ff5fd7': '206', '#ff5fff': '207',
    \ '#ff8700': '208', '#ff875f': '209', '#ff8787': '210', '#ff87af': '211',
    \ '#ff87d7': '212', '#ff87ff': '213', '#ffaf00': '214', '#ffaf5f': '215',
    \ '#ffaf87': '216', '#ffafaf': '217', '#ffafd7': '218', '#ffafff': '219',
    \ '#ffd700': '220', '#ffd75f': '221', '#ffd787': '222', '#ffd7af': '223',
    \ '#ffd7d7': '224', '#ffd7ff': '225', '#ffff00': '226', '#ffff5f': '227',
    \ '#ffff87': '228', '#ffffaf': '229', '#ffffd7': '230', '#ffffff': '231',
    \ '#080808': '232', '#121212': '233', '#1c1c1c': '234', '#262626': '235',
    \ '#303030': '236', '#3a3a3a': '237', '#444444': '238', '#4e4e4e': '239',
    \ '#585858': '240', '#626262': '241', '#6c6c6c': '242', '#767676': '243',
    \ '#808080': '244', '#8a8a8a': '245', '#949494': '246', '#9e9e9e': '247',
    \ '#a8a8a8': '248', '#b2b2b2': '249', '#bcbcbc': '250', '#c6c6c6': '251',
    \ '#d0d0d0': '252', '#dadada': '253', '#e4e4e4': '254', '#eeeeee': '255',
    \ }


" ============================================================================
" Functions:
" ============================================================================

function! s:AddCterm(name)
    exec "let l:gfg = synIDattr(synIDtrans(hlID('" . a:name .
                \ "')), 'fg', 'gui')"
    exec "let l:gbg = synIDattr(synIDtrans(hlID('" . a:name .
                \ "')), 'bg', 'gui')"
    let l:gfg = l:gfg == "" ? "NONE" : l:gfg
    let l:gbg = l:gbg == "" ? "NONE" : l:gbg
    exec "hi " . a:name . " ctermfg=" . s:color_map[l:gfg] .
                \ " ctermbg=" . s:color_map[l:gbg]
endfunction

function! s:AddSpCterm(name)
    exec "let l:gsp = synIDattr(synIDtrans(hlID('" . a:name .
                \ "')), 'sp', 'gui')"
    let l:gsp = l:gsp == "" ? "NONE" : l:gsp
    exec "hi " . a:name . " ctermfg=" . s:color_map[l:gsp]
endfunction


" ============================================================================
" Text Groups:
" ============================================================================

let s:normal_items = [
            \ "ColorColumn", "Comment", "Conceal", "Constant", "Cursor", "CursorColumn",
            \ "CursorIM", "CursorLine", "CursorLineNr", "DiffAdd", "DiffChange",
            \ "DiffDelete", "Directory", "Error", "ErrorMsg", "Identifier",
            \ "IncSearch", "LineNr", "MatchParen", "ModeMsg", "MoreMsg",
            \ "NonText", "Pmenu", "PmenuSbar", "PmenuSel",
            \ "PmenuThumb", "PreProc", "Question", "Search", "SignColumn",
            \ "Special", "SpecialKey", "Statement", "StatusLineNC", "TabLine",
            \ "TabLineFill", "Todo", "Type", "VertSplit", "Visual",
            \ "WarningMsg", "WildMenu",
            \ ]

let s:bold_items = [
            \ "DiffText", "FoldColumn", "Folded", "StatusLine", "TabLineSel",
            \ "Title", "CursorLineNr",
            \ ]

let s:underline_items = [
            \ "Underlined", "VisualNOS"
            \ ]

let s:undercurl_items = [
            \ "SpellBad", "SpellCap", "SpellLocal", "SpellRare"
            \ ]


" ============================================================================
" Color Definitions:
" ============================================================================

" ----------------------------------------------------------------------------
" 'Normal' Colors:
" ----------------------------------------------------------------------------

hi clear Normal
hi Normal gui=none cterm=none term=none

hi Normal       guifg=#bcbcbc

hi Normal                        guibg=#303030

call s:AddCterm("Normal")


" ----------------------------------------------------------------------------
" Extra setup
" ----------------------------------------------------------------------------

" Clear default settings
for s:item in s:normal_items + s:bold_items + s:underline_items + s:undercurl_items
    exec "hi " . s:item . " guifg=NONE guibg=NONE gui=none"
                \ . " ctermfg=NONE ctermbg=NONE cterm=none term=none"
endfor

let g:colors_name="lleaff-lucius"


" ----------------------------------------------------------------------------
" Text Markup:
" ----------------------------------------------------------------------------

hi NonText      guifg=#5f5f87
hi SpecialKey   guifg=#5f875f
hi Comment      guifg=#6c6c6c
hi Conceal      guifg=#6c6c6c
hi Constant     guifg=#afaf87
hi Directory    guifg=#87af87
hi Identifier   guifg=#87af5f
hi PreProc      guifg=#5faf87
hi Special      guifg=#af87af
hi Statement    guifg=#5fafd7
hi Title        guifg=#00afd7
hi Type         guifg=#5fafaf
    
" ----------------------------------------------------------------------------
" Highlighting:
" ----------------------------------------------------------------------------

hi Cursor       guifg=bg
hi CursorColumn guifg=NONE
hi CursorIM     guifg=bg
hi CursorLine   guifg=NONE
hi Visual       guifg=NONE
hi VisualNOS    guifg=fg        guibg=NONE
hi CursorColumn                 guibg=#444444
hi CursorLine                   guibg=#444444
hi IncSearch    guifg=bg
hi MatchParen   guifg=fg        guibg=#87af00
hi Search       guifg=bg
hi Visual                       guibg=#005f87
hi Cursor                       guibg=#5f87af
hi CursorIM                     guibg=#5f87af
hi Error        guifg=#d75f5f   guibg=#870000
hi IncSearch                    guibg=#00afaf
hi Search                       guibg=#d78700
hi Todo         guifg=#afaf00   guibg=#5f5f00


" ----------------------------------------------------------------------------
" Messages:
" ----------------------------------------------------------------------------

hi Question     guifg=fg
hi ErrorMsg     guifg=#d75f5f
hi ModeMsg      guifg=#87afaf
hi MoreMsg      guifg=#87afaf
hi WarningMsg   guifg=#af875f


" ----------------------------------------------------------------------------
" UI:
" ----------------------------------------------------------------------------

hi ColorColumn  guifg=NONE
hi Pmenu        guifg=bg
hi PmenuSel     guifg=fg
hi PmenuThumb   guifg=fg
hi StatusLine   guifg=bg
hi TabLine      guifg=bg
hi TabLineSel   guifg=fg
hi WildMenu     guifg=fg
hi ColorColumn                  guibg=#3a3a3a
hi CursorLineNr guifg=#9e9e9e   guibg=#444444
hi FoldColumn                   guibg=#4e4e4e
hi Folded                       guibg=#4e4e4e
hi LineNr       guifg=#626262   guibg=#444444
hi PmenuSel                     guibg=#005f87
hi SignColumn                   guibg=#4e4e4e
hi StatusLineNC guifg=#4e4e4e
hi TabLineFill  guifg=#4e4e4e
hi VertSplit    guifg=#626262
hi WildMenu                     guibg=#005f87
hi FoldColumn   guifg=#a8a8a8
hi Folded       guifg=#a8a8a8
hi Pmenu                        guibg=#8a8a8a
hi PmenuSbar    guifg=#8a8a8a   guibg=#bcbcbc
hi PmenuThumb                   guibg=#585858
hi SignColumn   guifg=#8a8a8a
hi StatusLine                   guibg=#8a8a8a
hi StatusLineNC                 guibg=#8a8a8a
hi TabLine                      guibg=#8a8a8a
hi TabLineFill                  guibg=#8a8a8a
hi TabLineSel                   guibg=#005f87
hi VertSplit                    guibg=#8a8a8a

" ----------------------------------------------------------------------------
" Diff:
" ----------------------------------------------------------------------------

hi DiffAdd      guifg=fg
hi DiffChange   guifg=fg
hi DiffDelete   guifg=fg

hi DiffAdd                      guibg=#5f875f
hi DiffChange                   guibg=#87875f
hi DiffDelete                   guibg=#875f5f
hi DiffText                     guibg=#87875f
hi DiffText     guifg=#d7d75f


" ----------------------------------------------------------------------------
" Spelling:
" ----------------------------------------------------------------------------

hi SpellBad     guisp=#ff5f5f
hi SpellCap     guisp=#5fafd7
hi SpellLocal   guisp=#d7af5f
hi SpellRare    guisp=#5faf5f


" ----------------------------------------------------------------------------
" Miscellaneous:
" ----------------------------------------------------------------------------

hi Ignore       guifg=bg
hi Underlined   guifg=fg


" ============================================================================
" Text Emphasis:
" ============================================================================

if s:use_bold == 1
    for s:item in s:bold_items
        exec "hi " . s:item . " gui=bold cterm=bold term=none"
    endfor
endif

if s:use_underline == 1
    for s:item in s:underline_items
        exec "hi " . s:item . " gui=underline cterm=underline term=none"
    endfor
    for s:item in s:undercurl_items
        exec "hi " . s:item . " cterm=underline"
    endfor
endif

for s:item in s:undercurl_items
    exec "hi " . s:item . " gui=undercurl term=none"
endfor


" ============================================================================
" Cterm Colors:
" ============================================================================

for s:item in s:normal_items + s:bold_items + s:underline_items
    call s:AddCterm(s:item)
endfor

for s:item in s:undercurl_items
    call s:AddSpCterm(s:item)
endfor

if s:no_term_bg == 1
    hi Normal ctermbg=NONE
endif


" ============================================================================
" Alternative Bold Definitions:
" ============================================================================

let s:alternative_bold_items = ["Identifier", "PreProc", "Statement",
            \ "Special", "Constant", "Type"]

for s:item in s:alternative_bold_items
    exec "let s:temp_gui_fg = synIDattr(synIDtrans(hlID('" . s:item .
                \ "')), 'fg', 'gui')"
    exec "let s:temp_cterm_fg = synIDattr(synIDtrans(hlID('" . s:item .
                \ "')), 'fg', 'cterm')"
    exec "hi B" . s:item . " guifg=" . s:temp_gui_fg . " ctermfg=" .
                \ s:temp_cterm_fg . " gui=bold cterm=bold term=none"
endfor


" ============================================================================
" Plugin Specific Colors:
" ============================================================================

" Tagbar:
hi link TagbarAccessPublic Constant
hi link TagbarAccessProtected Type
hi link TagbarAccessPrivate PreProc

" Vimwiki:
hi link VimwikiHeader1 BIdentifier
hi link VimwikiHeader2 BPreProc
hi link VimwikiHeader3 BStatement
hi link VimwikiHeader4 BSpecial
hi link VimwikiHeader5 BConstant
hi link VimwikiHeader6 BType

" vim: tw=78
