# Disable welcome
set -U fish_greeting

# Enable vim-like 
fish_vi_key_bindings
set fish_cursor_default block
set fish_cursor_insert line
set fish_cursor_replace_one underscore
set fish_cursor_visual block
set fish_vi_force_cursor

# Add user installed binaries to path
fish_add_path ~/.local/bin/
fish_add_path ~/.pyenv/bin/

# Pyenv
if command -sq pyenv
  status is-interactive; and pyenv init --path | source
  pyenv init - | source
end 

# neovim
if command -sq nvim
  alias vi=nvim
  alias vim=nvim
end


# exa
if command -sq exa
  alias ls="exa"
  alias ll="ls --long --header --git --classify --tree --level 1"
  alias tree="ll --level 3 --ignore-glob=.git/ --color=always"
  abbr lla "ll --all"
  abbr llm "ll --sort=modified"
  abbr lls "ll --sort=size"
else
  alias ll="ls -lh"
  alias lla="ls -lha"
  alias llm="ls --sort=modified"
  alias lls="ls --sort=size"
end

# Use "play" to pull credentials from ~
if command -sq ansible
  alias play="ansible-playbook -e @~/.secrets.yml"
end

# Git Abbreviations
abbr -a gitco git checkout
abbr -a gita git add
abbr -a gitaa git add .
abbr -a gitca git commit -a -m \"
abbr -a gits git status
abbr -a gitl git log --oneline --decorate --all --graph

# Prompt

function fish_prompt
    # Line 1
		if test -n "$SSH_TTY"
			set_color yellow
			printf '%s' $USER
			set_color normal

			printf ' at '

			set_color magenta
			printf '%s' (prompt_hostname)
			set_color normal
			printf ' in '
		end

		set_color blue
    printf '%s' (prompt_pwd)
		set_color normal

		printf '%s' (fish_git_prompt)

		# Line 2
    if fish_is_root_user
        set_color red
		end
    printf '\n ↪ '
    set_color normal
end

