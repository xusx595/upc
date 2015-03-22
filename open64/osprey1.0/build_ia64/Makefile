DIRS = driver.nue gccfe be whirl2c wopt ipl lno
CLEANDIRS = $(DIRS) targ_info libiberty libcomutil libcmplrs

all:
	
	for d in $(DIRS) ; do \
	 $(MAKE) -C $$d || exit 1; \
	done 
	test -f driver.nue/sgiupc || ( cd driver.nue && ln -s driver sgiupc )

clean:
	for d in $(CLEANDIRS) ; do \
	 $(MAKE) -C $$d clean || exit 1; \
	done

.PHONY: all clean

