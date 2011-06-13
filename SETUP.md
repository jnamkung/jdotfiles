# Basic MacOS setup

## For the Mac

### Install homebrew

    https://github.com/mxcl/homebrew/wiki/installation

### Install xcode. Make sure to skip iOS development, it'll save you 15 minutes.

### Install git

### Install rvm

    bash < <(curl -s https://rvm.beginrescueend.com/install/rvm)

### Install dotfiles

    mkdir ~/Projects
    cd ~/Projects
    git@github.com:dicksonlabs/dotfiles.git
    rake

### Install emacs 23 (to keep the dotfiles happy)

    brew install emacs

## Github

### Make your ssh public keys

    ssh-keygen

### Add a public key

    https://github.com/account/ssh

### Checkout a project.

    git clone your-project-name

## RVM

### cd to a project directory that uses rvm

    cd ~/Projects/...

### Install GNU readline

    brew install readline

### Run the RVM command it tells you to run with the GNU readline you
  just installed (version number will probably have changed)

    rvm install ruby-1.9.2-head --with-readline-dir=/usr/local/Cellar/readline/6.2.1/
    gem install bundler
    bundle

## Terminal setup

### Install SIMBL
### Download TerminalColours

    http://github.com/timmfin/terminalcolours/raw/master/TerminalColours-SL-64bit.zip

### Install TerminalColours bundle into

    ~/Library/Application\ Support/SIMBL/Plugins


