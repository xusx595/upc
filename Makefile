# C and C++ compilers to use for building the translator
CC = gcc
CXX = g++

# Default location for installation
PREFIX = /usr/local/bupc_translator

# override default build directory (empty for auto-detect)
BUILDDIR=

# override if your version of copy doesn't like these flags
CPYDIR = cp -R -p

#-------------------------------------------------------
# You should not need to modify anything below this line

all clean install update-nodist-list:
	@echo "------------------------------------------------------------" ; \
	if test "$(BUILDDIR)" ; then			\
	  echo "WARNING: forcing BUILDDIR=$(BUILDDIR)"; \
	  builddir="$(BUILDDIR)" ;			\
	else						\
	  CC="$(CC)"; export CC;			\
	  CXX="$(CXX)"; export CXX;			\
	  builddir=`config-aux/config-builddir` ;	\
	fi ; 						\
	if test "$$builddir" ; then			\
	  echo "Making $@ in BUILDDIR=$$builddir" ;	\
	  echo " $(CC) is: `$(CC) --version 2>&1 | head -1`" ; \
	  echo " $(CXX) is: `$(CXX) --version 2>&1 | head -1`" ; \
	  echo " $(MAKE) is: `$(MAKE) --version 2>&1 | head -1`" ; \
	  echo "------------------------------------------------------------" ; \
	  if test "$$builddir" = "targ_ilp32_osx" -o "$$builddir" = "targ_lp64_osx"; then   \
	    strip=NOSTRIP=1 ; export strip ;            \
	  fi ;                                          \
	  if test -n "$${CFLAGS+set}"; then             \
	    echo "WARNING: unsetting CFLAGS" ;          \
	    unset CFLAGS ;                              \
	  fi ;                                          \
	  $(MAKE) BUILDDIR="$$builddir" $$strip $@-internal ;	\
	else						\
	  exit 1;					\
	fi

all-internal:
	$(MAKE) -C open64/osprey1.0/$(BUILDDIR) all CC="$(CC)" CXX="$(CXX)"
	@echo "------------------------------------------------------------" 
	@echo " *** Build complete! *** "
	@echo ""
	@echo "You may now install the compiler with:"
	@echo ""
	@echo "  $(MAKE) install PREFIX=/prefix/to/use"
	@echo ""
	@echo "Alternatively, to use this translator directly from the build tree:"
	@echo ""
	@echo "for LOCAL compilations, add the following path in the 'translator'"
	@echo " setting of the runtime's upcc.conf or private ~/.upccrc file, "
	@echo " or pass it to 'upcc -translator=...'"
	@echo ""
	@echo " translator = `pwd`/open64/osprey1.0/$(BUILDDIR)"
	@echo ""
	@echo "for REMOTE (ssh-based) compilations, use the path:"
	@echo ""
	@echo " translator = `whoami`@`hostname`:`pwd`/open64/osprey1.0/$(BUILDDIR)"
	@echo ""
	@echo "------------------------------------------------------------" 

clean-internal:
	$(MAKE) -C open64/osprey1.0/$(BUILDDIR) clean
	rm -f config-aux/abi

# Files to install
INSTALL_LIST = \
	libupc/upcr_trans_extra*.[ch]		\
	$(BUILDDIR)/be/be			\
	$(BUILDDIR)/driver.nue/driver		\
	$(BUILDDIR)/driver.nue/sgiupc		\
	$(BUILDDIR)/gccfe/gfec			\
	$(BUILDDIR)/whirl2c/whirl2c		\
	$(BUILDDIR)/whirl2c/whirl2c_be		\
	`find $(BUILDDIR) -name '*.so' -type f` \
	`find $(BUILDDIR) -name '*.so' \! -type f`

install-internal:
	@mkdir -p "$(PREFIX)" || exit $? ; 				\
	prefix=`cd "$(PREFIX)" ; pwd` ; 				\
	cd open64/osprey1.0 ; 						\
	for file in $(INSTALL_LIST) ; do 				\
	  fdir=`dirname "$$file" | perl -pe 's@$(BUILDDIR)@targ@'` ; 	\
	  fbase=`basename "$$file"` ; 					\
	  instdir="$$prefix/$$fdir" ; 					\
	  mkdir -p "$$instdir" || exit $? ; 				\
	  echo "Installing: $$instdir/$$fbase" ; 			\
	  $(CPYDIR) "$$file" "$$instdir/$$fbase" || exit $? ; 		\
	  if test -x "$$instdir/$$fbase" &&                             \
	     test "$(NOSTRIP)" = "" ; then                              \
	    echo "Stripping: $$instdir/$$fbase" ; 			\
	    strip "$$instdir/$$fbase" || echo "  FAILURE IGNORED" ;     \
	  fi ;                                                          \
	done ; 								\
	echo "------------------------------------------------------------------------------" ; \
	echo " *** Installation complete! ***" ; 						\
	echo " " ; 										\
	echo " To use this translator LOCALLY, insert the following pathname:" ;		\
	echo " " ; 										\
	echo "    $$prefix/targ" ; 								\
	echo " " ; 										\
	echo " in the 'translator' setting of upcc.conf or ~/.upccrc, " ; 			\
	echo " or pass it to upcc -translator=path" ; 						\
	echo " " ; 										\
	echo " for REMOTE (ssh-based) compilations, use the path:" ; 				\
	echo " " ; 										\
	echo " translator = `whoami`@`hostname`:$$prefix/targ" ; 				\
	echo " " ; 										\
	echo "------------------------------------------------------------------------------" 


# Files and directory trees to always distribute
EXTRA_DIST = \
	Makefile \
	LICENSE.TXT \
	README \
	config-aux \
	open64/osprey1.0/libupc/upcr_trans_extra.c \
	open64/osprey1.0/libupc/upcr_trans_extra.w2c.h \
	open64/osprey1.0/libupc/upcr_trans_extra_auto_nb.c \
	open64/osprey1.0/libupc/upcr_trans_extra_vect.c \
	open64/osprey1.0/libupc/upcr_trans_extra_vect.w2c.h \
	open64/osprey1.0/gccfe/gnu/gstab.h \
	open64/osprey1.0/gccfe/gnu/stab.def \
	open64/osprey1.0/gccfe/gnu/c-parse.y \
	open64/osprey1.0/include/nl_types.h \
	open64/osprey1.0/targia32_ia64_nodebug \
	open64/osprey1.0/build_ia64 \
	open64/osprey1.0/targ_alpha_tru64 \
	open64/osprey1.0/targ_ilp32_osx \
	open64/osprey1.0/targ_lp64_osx \
	open64/osprey1.0/targ_ppc_aix \
	open64/osprey1.0/targ_lp64_sunos \
	open64/osprey1.0/targ_sunos \
	open64/osprey1.0/targ_ilp32_freebsd \
	open64/osprey1.0/targ_lp64_freebsd \
	open64/osprey1.0/be/be/so_locations \
	open64/osprey1.0/be/lno/BUPC-LNO-NOTES.txt \
	open64/osprey1.0/driver/sort_options.*

# Files/directories to never distribute
NO_DIST = \
	./clean-stamp \
	./nodist-list

nodist-list: 
	$(MAKE) update-nodist-list

# re-create the list of files to prune for distribution
update-nodist-list-internal:
	@if test "`uname`.$(BUILDDIR)" != Linux.targia32_ia64_nodebug; then \
	  echo "ERROR: $@ must be run for a 32-bit Linux target"; \
	  exit 1; \
	fi
	$(MAKE) clean-internal
	touch clean-stamp
	sleep 60 # sleep to ensure timestamp update
	$(MAKE) all-internal # build everything
	# now touch anything that needs to be kept for EXTRA_DIST
	for file in $(EXTRA_DIST) ; do \
	  if test -f $$file ; then \
	    head $$file > /dev/null ; \
	  else \
	    find $$file -type f -exec head {} \; > /dev/null ; \
	  fi ; \
        done
	sleep 60 # sleep to ensure timestamp update
	find . -type f \! -anewer clean-stamp -print | grep -v /.git/ > nodist-list # scan for untouched files
	# sanity check
	if grep config-aux/config-builddir nodist-list >/dev/null 2>&1; then \
	  echo "ERROR: atime updates are broken - aborting"; \
	  rm nodist-list; \
	  exit 1; \
	fi
	# remove hard-coded nodist files
	for file in $(NO_DIST) ; do \
	  echo $$file >> nodist-list ; \
	done
	# filter out a few known patterns of extra-dist files, and produce a canonical list
	mv nodist-list nodist-list.old
	cat nodist-list.old | \
	    grep -v README | \
            grep -v Exported | \
            env LC_ALL=C sort | uniq > nodist-list
	rm -f nodist-list.old

TRANS_VERSION = $(shell perl -ne 'if (m/release (\d+\.\d+\.\d+)/) { print "$$1\n"; }' -- open64/osprey1.0/be/whirl2c/w2c_driver.cxx)
DIST_DIRNAME = berkeley_upc_translator-$(TRANS_VERSION)
TARBALL_NAME = $(DIST_DIRNAME).tar.gz
DIST_TMPDIR = $${TMPDIR:-/tmp}

# create a pruned translator distribution
dist: nodist-list
	@if test -e "$(TARBALL_NAME)"; then \
	  echo ERROR: \"$(TARBALL_NAME)\" already exists ; \
	  exit 1 ; \
        fi
	@if test -e "$(DIST_TMPDIR)/$(DIST_DIRNAME)"; then \
	  echo ERROR: \"$(DIST_TMPDIR)/$(DIST_DIRNAME)\" already exists ; \
	  exit 1 ; \
        fi
	@echo --- Checking source tree...
	@if test "`find . -name '*.o' -print`" != "" ; then \
	  echo ERROR: make dist should only be run on a CLEAN git checkout of the translator source tree ; \
	  exit 1 ; \
        fi
	@cd open64/osprey1.0/gccfe/gnu/ && \
	if test \! -e c-parse.c; then \
	  echo "ERROR: c-parse.c is missing"; \
	  exit 1; \
	elif test c-parse.y -nt c-parse.c; then \
	  echo "ERROR: c-parse.c is out-of-date (older than c-parse.y)"; \
	  exit 1; \
	elif test "`grep Id: 'c-parse.c'`" != "`grep Id: 'c-parse.y'`"; then \
	  echo "ERROR: c-parse.c is out-of-date (rcsid does not match c-parse.y)"; \
	  exit 1; \
	fi
	@echo --- Constructing pruned source tree...
	cp -R . $(DIST_TMPDIR)/$(DIST_DIRNAME) && \
	cd "$(DIST_TMPDIR)/$(DIST_DIRNAME)" && \
	sleep 60 && touch open64/osprey1.0/gccfe/gnu/c-parse.c && \
	xargs rm -Rf < nodist-list && \
	find . -depth -type d -empty -exec rmdir {} \;
	@echo --- Preparing translator distribution tarball...
	if test -d .git ; then \
	  rm -Rf $(DIST_TMPDIR)/$(DIST_DIRNAME)/.git ; \
	  $${GIT=git} describe > $(DIST_TMPDIR)/$(DIST_DIRNAME)/version.git || exit $$? ; \
	fi
	( cd $(DIST_TMPDIR) && tar cvpf - "$(DIST_DIRNAME)" ) | gzip -9 -c > $(TARBALL_NAME) && \
	rm -Rf "$(DIST_TMPDIR)/$(DIST_DIRNAME)"
	@echo $(TARBALL_NAME) is ready for testing and distribution.

distcheck:
	$(MAKE) dist
	@rm -Rf _build _inst
	@mkdir _build && cd _build && tar xfz ../$(TARBALL_NAME)
	$(MAKE) -C _build/$(DIST_DIRNAME) all
	$(MAKE) -C _build/$(DIST_DIRNAME) install PREFIX=`pwd`/_inst
	$(MAKE) -C _build/$(DIST_DIRNAME) nodist-list
	$(MAKE) -C _build/$(DIST_DIRNAME) clean
	$(MAKE) -C _build/$(DIST_DIRNAME) dist
	@rm -Rf _build _inst
	@echo PASS

force:

.PHONY: all clean force dist update-nodist-list distcheck

