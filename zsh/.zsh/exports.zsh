# {{{ Environment
export HISTFILE="${HOME}/.zsh_history"
export HISTSIZE=5000
export SAVEHIST=5000
export LESSHISTFILE="-"
export PAGER="less"
export READNULLCMD="${PAGER}"
export BROWSER="firefox"
export XTERM="urxvt"
export EDITOR="emacsclient -c"
export GREP_COLOR="1;33"
export _JAVA_OPTIONS='-Dawt.useSystemAAFontSettings=off'
#export _JAVA_OPTIONS='-Dswing.defaultlaf=com.sun.java.swing.plaf.gtk.GTKLookAndFeel'
export JAVA_FONTS=/usr/share/fonts/TTF
export WINEARCH=32
export WINEPREFIX=~/win32
export DMENU_OPTIONS='-fn xft:monospace-12 -r -x 0 -y 18 -nb #303030 -nf #909090 -sf #303030'
export TODOTXT_DEFAULT_ACTION=ls
export PATH="${HOME}/.emacs.d/bin:/opt/visit/bin:$PATH"
# }}}

# {{{ Dircolors - with rxvt-256color support
#eval `dircolors -b "${HOME}/.dir_colors"`
# }}}

# {{{ Manual pages
#     - colorize, since man-db fails to do so
export LESS_TERMCAP_mb=$'\E[01;31m'   # begin blinking
export LESS_TERMCAP_md=$'\E[01;31m'   # begin bold
export LESS_TERMCAP_me=$'\E[0m'       # end mode
export LESS_TERMCAP_se=$'\E[0m'       # end standout-mode
export LESS_TERMCAP_so=$'\E[1;33;40m' # begin standout-mode - info box
export LESS_TERMCAP_ue=$'\E[0m'       # end underline
export LESS_TERMCAP_us=$'\E[1;32m'    # begin underline
# }}}

#{{{
# Enable C-s in shell
stty -ixon
#}}}

# vim:filetype=zsh
