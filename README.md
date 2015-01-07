Pristine
========
Pristine takes git repositories and generates beautiful, purely static HTML
summaries of the contents therein.

As opposed to hosting one's code on a third-party service, pristine generates
stand-alone project summaries that can be served from the user's own webservers,
emailed around, or whatever else. It's just static content - nothing fancy, ya'
know?

Install
-------
Pristine isn't on cabal yet, so for now, do the following (make sure you already
have the [haskell-platform](http://www.haskell.org/platform)):

    $ git clone https://github.com/chrisdotcode/pristine
    $ cd pristine
    $ cabal sandbox init
    $ cabal install -j --haddock-hyperlink-source

The pristine binary should be installed somewhere in `dist/`.

Documentation
-------------
Pristine - The git respository static summary generator

Usage:

    pristine [--version] [DIRECTORY] [-n|--name ARG] [-l|--license LICENSE]
             [-o|--output OUTPUT] [-m|--maintainer MAINTAINER]
             [-e|--clone-link CLONELINK] [-d|--download-link DOWNLOADLINK]
        Generates static summaries of git repositories.

### -h,--help
Show the help text.

### --version
Print version information.

### DIRECTORY
The directory containing the project's git repo.

### -n,--name ARG
The project's name (defaults to the repository's directory name).

### -l,--license LICENSE
The license for the project. This is one of:

* `none`
* `gpl2`
* `gpl3`
* `lgpl2`
* `lgpl3`
* `agpl3`
* `bsd2`
* `bsd3`
* `mit`
* `mpl2`
* `apache2`
* `public`
* `reserved`

### -o,--output OUTPUT
The directory to ouput the generated file.

### -m,--maintainer MAINTAINER
The maintainer of the project.

### -e,--clone-link CLONELINK
The project's git clone location.

### -d,--download-link DOWNLOADLINK
The project's download/home page, if one is offered.
