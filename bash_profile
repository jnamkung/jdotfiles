# -*- sh -*-

# Read by login shells only (the first time you log in).
# NOT read by subshells (like what screen creates).

# Load the default .profile, if it exists
[[ -s "$HOME/.profile" ]] && source "$HOME/.profile"

# .bashrc is read by subshells. This will make login shells read it too.
[[ -s "$HOME/.bashrc" ]] && source "$HOME/.bashrc"

# load path fixer-upper after loading all other config files, BEFORE setting up rvm
source $HOME/.bash/better-paths

# Load RVM as a function
[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm" # Load RVM into a shell session *as a function*
