# -*- sh -*-

# Read by login shells only (the first time you log in).
# NOT read by subshells (like what screen creates).

[[ -s "$HOME/.profile" ]] && source "$HOME/.profile" # Load the default .profile if it exists

# .bashrc is read by subshells. This will make login shells read it too.
if [ -f $HOME/.bashrc ]; then
    source $HOME/.bashrc
fi

source $HOME/.bash/better-paths
