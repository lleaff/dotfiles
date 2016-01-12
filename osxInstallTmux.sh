#!/bin/bash

SRCDIR=$HOME/sources/tmux

mkdir -p $SRCDIR
cd $SRCDIR

TMUX_ARCHIVE=ar-tmux.tar.gz
LIBEVENT_ARCHIVE=ar-libevent.tar.gz
curl -L "http://downloads.sourceforge.net/project/tmux/tmux/tmux-2.0/tmux-2.0.tar.gz" > $TMUX_ARCHIVE
curl -L "http://downloads.sourceforge.net/project/levent/release-2.0.22-stable/libevent-2.0.22-stable.tar.gz" > $LIBEVENT_ARCHIVE

tar xzf $TMUX_ARCHIVE
tar xzf $LIBEVENT_ARCHIVE

cd libevent*
./configure --prefix=/opt
make
sudo make install

cd ../tmux*
LDFLAGS="-L/opt/lib" CPPFLAGS="-I/opt/include" LIBS="-lresolv" ./configure --prefix=/opt
make
sudo make install
