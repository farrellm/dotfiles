#
# ~/.config/fish/config.fish
#

setenv EDITOR vim
setenv PATH ~/bin $PATH /sbin /usr/sbin
setenv JAVA_HOME /usr/lib/jvm/java-7-openjdk
# setenv LD_LIBRARY_PATH /usr/local/opencv/share/OpenCV/java/

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
alias vncandroid='vncserver -geometry 720x1280'
alias vncipad='vncserver -geometry 2048x1536'

function brightness
    sudo sh -c "echo $argv > /sys/class/backlight/acpi_video0/brightness "
end

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

# alias gg='log --graph --all --format=format:"%C(bold blue)%h%C(reset) - %C(bold green)(%ar)%C(reset) %C(white)%s%C(reset) %C(dim white)- %an%C(reset)%C(bold yellow)%d%C(reset)" --abbrev-commit --date=relative'
alias gg='git log --graph --oneline --all'

# login stuff
if status --is-login
	# start X
	test -z $DISPLAY; and test $XDG_VTNR -eq 1; and exec startx
end

function prompt_git
    git rev-parse --abbrev-ref HEAD
end

function is_git
    git rev-parse --git-dir 1>/dev/null 2>/dev/null
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
	    if is_git
		echo -n -s "$__fish_prompt_cwd" (prompt_pwd) "$__fish_prompt_normal" " (" (prompt_git) "$__fish_prompt_normal" ')> ' \a
	    else
		echo -n -s "$__fish_prompt_cwd" (prompt_pwd) "$__fish_prompt_normal" '> ' \a
	    end
    end
end
