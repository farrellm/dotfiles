#
# ~/.config/fish/config.fish
#

# If not running interactively, don't do anything
if not status --is-interactive
    exit
end

# setenv EDITOR vim
set -x PATH ~/bin ~/.local/bin ~/.cabal/bin /usr/local/bin $PATH
set -x JAVA_HOME /usr/lib/jvm/default

if test "$TERM" = "xterm"
    set -x TERM "xterm-256color"
end

set -x EDITOR vim

# turn off audible bell
if test -n "$DISPLAY"
    xset -b
end

# aliases
alias ls='ls --color=auto'
alias ll='ls -l --color=auto'
alias em='emacs -nw'

abbr -a ga 'git add'
abbr -a gp 'git push'
abbr -a gl 'git log'
abbr -a gs 'git status'
abbr -a gd 'git diff --color'
abbr -a gdc 'git diff --cached'
abbr -a gm 'git commit -m'
abbr -a gma 'git commit -am'
abbr -a gb 'git branch'
abbr -a gc 'git checkout'
abbr -a gra 'git remote add'
abbr -a grr 'git remote rm'
abbr -a gpu 'git pull'
abbr -a gcl 'git clone'

eval (ssh-agent -c) >/dev/null
