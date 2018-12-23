```
"  _____             _ ___ ___ ___      "
" |   __|___ ___ _ _| |_  |  _|  _|     "
" |__   | -_| . | | | |  _|_  | . |     "
" |_____|___|___|___|_|___|___|___|.vim "
```

seoul256-lleaff.vim ![travis-ci](https://travis-ci.org/junegunn/seoul256-lleaff.vim.svg?branch=master)
------------

*seoul256-lleaff.vim* is a low-contrast Vim color scheme based on [Seoul Colors](http://www.seoul.go.kr/v2012/seoul/symbol/color.html).
Works on 256-color terminal or on GVim.

### seoul256-lleaff

![seoul256-lleaff](https://raw.github.com/junegunn/i/master/seoul256-lleaff.png)

### seoul256-lleaff (light version)

![seoul256-lleaff-light](https://raw.github.com/junegunn/i/master/seoul256-lleaff-light.png)

Installation
------------

Use your favorite plugin manager.

- [vim-plug](https://github.com/junegunn/vim-plug)
  1. Add `Plug 'junegunn/seoul256-lleaff.vim'` to .vimrc
  2. Run `:PlugInstall`

Color schemes
-------------

```vim
" Unified color scheme (default: dark)
colo seoul256-lleaff

" Light color scheme
colo seoul256-lleaff-light

" Switch
set background=dark
set background=light
```

Change background color
-----------------------

![seoul256-lleaff-bg](https://raw.github.com/junegunn/i/master/seoul256-lleaff-bg.png)

```vim
" seoul256-lleaff (dark):
"   Range:   233 (darkest) ~ 239 (lightest)
"   Default: 237
let g:seoul256_lleaff_background = 236
colo seoul256-lleaff

" seoul256-lleaff (light):
"   Range:   252 (darkest) ~ 256 (lightest)
"   Default: 253
let g:seoul256_lleaff_background = 256
colo seoul256-lleaff
```

If `g:seoul256_lleaff_background` is set, seoul256-lleaff will choose the right version based
on the value and `set background=dark/light` will not switch versions.

If you'd like to switch versions with custom background colors, set
`g:seoul256_lleaff_background` to be a dark value, and additionally define
`g:seoul256_lleaff_light_background` for seoul256-lleaff-light.

```vim
let g:seoul256_lleaff_background = 233
let g:seoul256_lleaff_light_background = 256

colo seoul256-lleaff
colo seoul256-lleaff-light
```

Alternate 256-xterm -> sRGB mapping
-------------------------------------

The GUI RGB colors are derived from 256-color-terminal color codes in the
source code, with the help of a lookup table.

By default, the table contains RGB values of terminal colors as displayed by
iTerm2 on macOS. If you're using another terminal emulator (urxvt,
xfce4-terminal,... pretty much any terminal on Linux), the colors aren't
displayed in the same way. That's why you may see a difference in color of GUI
and terminal [n]vim in Linux.

If `let g:seoul256_lleaff_srgb` is set to 1, the color mapping is altered
to suit the way urxvt (and various other terminals) renders them. That way, the
colors of the terminal and GUI versions are uniformly colored on Linux.

```vim
let g:seoul256_lleaff_srgb = 1
```

Current background color
------------------------

When loaded, soul256.vim will set up two global variables so that you can use
them to customize other plugins:

- `g:seoul256_lleaff_current_fg` - Current foreground color in ANSI code
- `g:seoul256_lleaff_current_bg` - Current background color in ANSI code

iTerm2 color scheme
-------------------

- [Official iTerm2 color scheme](iterm2)
- [seoul256-lleaff-iTerm2](https://github.com/mikker/seoul256-lleaff-iTerm2) by
[Mikkel Malmberg](https://github.com/mikker).

Emacs color theme
-----------------

- [seoul256-lleaff-emacs](https://github.com/anandpiyer/seoul256-lleaff-emacs)

Author
------

Junegunn Choi

License
-------

MIT
