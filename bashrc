# Read by subshells. Login shells source this file via .bash_profile
#

# Fix GNU screen's 256-color terminal setttings
if [ $STY ]; then
    # we're running inside GNU screen...
    if [ -n "$TERMCAP" ]; then
        # ...where the $TERMCAP screen sets is wrong for 256-color terminals...
        if (( `expr "$TERM" : '.*256col'` )); then
            # ...so we blow it away, assuming either the system-wide
            # termcap will have something better, or we have
            # taken advantage of $TERMPATH and/or .termcap, or
            # we'll be setting $TERMCAP ourselves after this
            unset TERMCAP
        fi
    fi
fi

# rvm startup alters $PATH; do it before sourcing ~/.bash/paths
[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm"

# Set aliases in ~/.bash/aliases
# Set prompts and whatnot in ~/.bash/config
# Set paths in ~/.bash/paths
# Add bash-completions to ~/.bash/completions

source $HOME/.bash/paths
source $HOME/.bash/aliases
source $HOME/.bash/completions
source $HOME/.bash/config


# Use .bashrc_local for your local or user settings
if [ -f $HOME/.bashrc_local ]; then
    source ~/.bashrc_local
elif [ -f $HOME/.local ]; then
    # backwards compatibility with poorly-named local bash rc file
    source ~/.local
fi
# Don't put anything after this: .bashrc_local should override anything in here
