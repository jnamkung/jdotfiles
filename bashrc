# -*- sh -*-

# This is read by subshells. Login shells source this file via .bash_profile
#
# Set aliases in ~/.bash/aliases
# Set prompts and whatnot in ~/.bash/config
# Set prompts in ~/.bash/paths
#
# Don't set PATH here because it'll keep growing.  Source paths in bash_profile

# These don't carry through to subshells so they go here instead of
# .bash_profile

# source a .bashrc.orig, if it exists
[[ -s "$HOME/.bashrc.orig" ]] && source "$HOME/.bashrc.orig"

source ~/.bash/aliases
source ~/.bash/completions
source ~/.bash/config
source ~/.bash/colorfixes

# Use .localrc for settings specific to one system
[[ -s "$HOME/.localrc" ]] && source "$HOME/.localrc"

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

export AWS_SDK_LOAD_CONFIG=1
export AWS_SHARED_CREDENTIALS_FILE=$HOME/.aws/credentials
export AWS_CONFIG_FILE=$HOME/.aws/config


# get readable date from a datestamp in milliseconds
msdate() { msd=`echo "$1" / 1000 | bc` ; date -r $msd; }

# tabtab source for serverless package
# uninstall by removing these lines or running `tabtab uninstall serverless`
[ -f /Users/ju/.nvm/versions/node/v8.11.4/lib/node_modules/serverless/node_modules/tabtab/.completions/serverless.bash ] && . /Users/ju/.nvm/versions/node/v8.11.4/lib/node_modules/serverless/node_modules/tabtab/.completions/serverless.bash
# tabtab source for sls package
# uninstall by removing these lines or running `tabtab uninstall sls`
[ -f /Users/ju/.nvm/versions/node/v8.11.4/lib/node_modules/serverless/node_modules/tabtab/.completions/sls.bash ] && . /Users/ju/.nvm/versions/node/v8.11.4/lib/node_modules/serverless/node_modules/tabtab/.completions/sls.bash
# tabtab source for slss package
# uninstall by removing these lines or running `tabtab uninstall slss`
[ -f /Users/ju/.nvm/versions/node/v8.11.4/lib/node_modules/serverless/node_modules/tabtab/.completions/slss.bash ] && . /Users/ju/.nvm/versions/node/v8.11.4/lib/node_modules/serverless/node_modules/tabtab/.completions/slss.bash
