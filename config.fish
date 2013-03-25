#
# ~/.config/fish/config.fish
#

# If not running interactively, don't do anything
if not status --is-interactive
	exit
end

# turn off audible bell
xset -b


setenv EDITOR vim
setenv PATH ~/bin $PATH /sbin /usr/sbin
setenv JAVA_HOME /usr/lib/jvm/java-7-openjdk
# setenv LD_LIBRARY_PATH /usr/local/opencv/share/OpenCV/java/


# aliases
alias ls='ls --color=auto'
alias ll='ls -l --color=auto'

alias em='emacs -nw'

alias rdesktop-ts='rdesktop -u mfarrell -d TWOSIGMA -z -g 1356x738 localhost:7777'
alias rdesktop-ts2='rdesktop -u mfarrell -d TWOSIGMA -z -g 2048x1500 localhost:7777'
alias vncandroid='vncserver'
alias vncipad='vncserver -geometry 2048x1536'

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

# login stuff
if status --is-login
	# start X
	test -z $DISPLAY; and test $XDG_VTNR -eq 1; and exec startx
end


function fish_prompt --description "Write out the prompt"
    # Just calculate these once, to save a few cycles when displaying the prompt
    if not set -q __fish_prompt_hostname
	set -g __fish_prompt_hostname (hostname|cut -d . -f 1)
    end

    if not set -q __fish_prompt_normal
	set -g __fish_prompt_normal (set_color normal)
    end

    switch $USER
	case root
	    if not set -q __fish_prompt_cwd
		if set -q fish_color_cwd_root
			set -g __fish_prompt_cwd (set_color $fish_color_cwd_root)
		else
			set -g __fish_prompt_cwd (set_color $fish_color_cwd)
		end
	    end
	    echo -n -s "$USER" @ "$__fish_prompt_hostname" ' ' "$__fish_prompt_cwd" (prompt_pwd) "$__fish_prompt_normal" '# '

	case '*'
	    if not set -q __fish_prompt_cwd
		set -g __fish_prompt_cwd (set_color $fish_color_cwd)
	    end
	    echo -n -s "$USER" @ "$__fish_prompt_hostname" ' ' "$__fish_prompt_cwd" (prompt_pwd) "$__fish_prompt_normal" '> ' \a
    end
end
