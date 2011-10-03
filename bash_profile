# Read by login shells only (the first time you log in).
# NOT read by subshells (like what screen creates).

# .bashrc is read by subshells. This will make login shells read it too.
if [ -f $HOME/.bashrc ]; then
    source $HOME/.bashrc
fi



