# -*- sh -*-

# Read by login shells only (the first time you log in).
# NOT read by subshells (like what screen creates).

# Load the default .profile, if it exists
[[ -s "$HOME/.profile" ]] && source "$HOME/.profile"

# .bashrc is read by subshells. This will make login shells read it too.
[[ -s "$HOME/.bashrc" ]] && source "$HOME/.bashrc"

# load path fixer-upper last
source $HOME/.bash/better-paths
