# dictClean

An R package to clean data using simple "dictionary" files

## Installation

From inside R:

`remotes::install_github("dushoff/dictClean")`

This will fail for old versions of "remotes", because the default branch is now "main" instead of "master". `, ref="main"` before the last paren may well fix this problem, but probably what you want to do is update remotes:

`install_packages("remotes")`

… and then try again

_Alternative_ If you can't install_github because of permission problems, try:

* [download this zip file](https://github.com/dushoff/dictClean/archive/refs/heads/main.zip)
* unzip it
* `R CMD INSTALL dictClean-main` from the directory where you unzipped.
