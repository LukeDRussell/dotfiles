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
    if not set -q VIRTUAL_ENV_DISABLE_PROMPT
        set -g VIRTUAL_ENV_DISABLE_PROMPT true
    end
    set_color yellow
    printf '%s' $USER
    set_color normal
    printf ' at '

    set_color magenta
    echo -n (prompt_hostname)
    set_color normal
    printf ' in '

    set_color $fish_color_cwd
    printf '%s' (prompt_pwd)
    set_color normal

    # Line 2
    if test -n "$VIRTUAL_ENV"
        printf "(%s) " (set_color blue)(basename $VIRTUAL_ENV)(set_color normal)
    end
    printf ' ↪ '
    set_color normal
end
