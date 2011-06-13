# Dickson Labs config files

copied from Ryan Bates

## Installation

Run:

    rake

## Features

``p <dir>`` will cd to your Projects directory

Tab completion is also added to rake and cap commands:

    rake db:mi<tab>
    cap de<tab>

To speed things up, the results are cached in local .rake_tasks~ and
.cap_tasks~. It is smart enough to expire the cache automatically in
most cases, but you can simply remove the files to flush the cache.

There are a few key bindings set. Many of these require option to be
set as the meta key. Option-left/right arrow will move cursor by word,
and control-left/right will move to beginning and end of line.
Control-option-N will open a new tab with the current directory under
Mac OS X Terminal.

If there are some shell configuration settings which you want secure
or specific to one system, place it into a ~/.localrc file. This will
be loaded automatically if it exists.

