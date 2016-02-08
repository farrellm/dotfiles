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

if status --is-interactive;
#	eval (keychain --eval --agents ssh id_rsa --quiet)
#       [ -e $HOME/.keychain/(hostname)-fish ]; and . $HOME/.keychain/(hostname)-fish
end

source ~/.config/fish/prompt.fish
