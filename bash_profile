# Read by login shells only (the first time you log in).
# NOT read by subshells (like what screen creates).

source ~/.bash/paths

# .bashrc is read by subshells. This will make login shells read it too.
if [ -f ~/.bashrc ]; then
    source ~/.bashrc
fi



