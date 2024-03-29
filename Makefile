## This is dictClean
## https://dushoff.github.io/dictClean/

current: target
-include target.mk

# -include makestuff/perl.def

vim_session:
	bash -cl "vmt"

######################################################################

## pkgall:
## quickinstall:

######################################################################

### Makestuff

Sources += Makefile $(wildcard *.md)

Ignore += makestuff
msrepo = https://github.com/dushoff

Makefile: makestuff/Makefile
makestuff/Makefile:
	git clone $(msrepo)/makestuff
	ls makestuff/Makefile

-include makestuff/os.mk

-include makestuff/pipeR.mk
-include makestuff/rpkg.mk

-include makestuff/git.mk
-include makestuff/visual.mk
