#
# ~/.config/fish/config.fish
#

setenv EDITOR vim
setenv PATH ~/bin ~/.cabal/bin $PATH
setenv JAVA_HOME /usr/lib/jvm/java-7-openjdk

# If not running interactively, don't do anything
if not status --is-interactive
   exit
end

# turn off audible bell
xset -b

# aliases
alias ls='ls --color=auto'
alias ll='ls -l --color=auto'

alias em='emacs -nw'

alias rdesktop-ts='rdesktop -u mfarrell -d TWOSIGMA -z -g 1356x738 localhost:7777'
alias rdesktop-ts2='rdesktop -u mfarrell -d TWOSIGMA -z -g 2048x1500 localhost:7777'


alias git=hub

alias ga='git add'
alias gp='git push'
alias gl='git log'
alias gs='git status'
alias gd='git diff --color'
alias gdc='git diff --cached'
alias gm='git commit -m'
alias gma='git commit -am'
alias gb='git branch'
alias gc='git checkout'
alias gra='git remote add'
alias grr='git remote rm'
alias gpu='git pull'
alias gcl='git clone'

if status --is-interactive;
#	eval (keychain --eval --agents ssh id_rsa --quiet)
#       [ -e $HOME/.keychain/(hostname)-fish ]; and . $HOME/.keychain/(hostname)-fish
end

source ~/.config/fish/prompt.fish
