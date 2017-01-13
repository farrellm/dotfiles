#
# ~/.config/fish/config.fish
#

# If not running interactively, don't do anything
if not status --is-interactive
   exit
end

# setenv EDITOR vim
setenv PATH ~/bin ~/.local/bin /usr/local/bin $PATH
setenv JAVA_HOME /usr/lib/jvm/default

if test "$TERM" = "xterm"
    set -x TERM "xterm-256color"
end

# turn off audible bell
xset -b

# aliases
alias ls='ls --color=auto'
alias ll='ls -l --color=auto'
alias em='emacs -nw'

alias git=hub

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

source ~/.config/fish/prompt.fish
eval sh ~/.config/fish/zenburn.sh
