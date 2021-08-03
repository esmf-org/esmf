# $Id$ 

ALL: build_here

# This needs to be the path of this directory relative to the
# top level esmf directory.
LOCDIR	  = src/Infrastructure/IO/PIO

# Leave this line alone.
include $(ESMF_DIR)/makefile

#TODO: Fix hard-coded CC issue below... 
tree_lib:
	mkdir -p Build
	cd Build; \
	env CC=mpicc FC=$(ESMF_F90COMPILER) CFLAGS="$(ESMF_CXXOPTFLAG) $(ESMF_SO_CXXCOMPILEOPTS)" cmake -DNetCDF_PATH=$(ESMF_NETCDF_INCLUDE)/.. -DCMAKE_INSTALL_PREFIX=../Install -DPIO_ENABLE_TIMING=OFF ../ParallelIO; \
	make -f ./Makefile; \
	make -f ./Makefile install; \
	cp -fp ../Install/include/* $(ESMF_INCDIR); \
	cp -fp ../Install/lib/* $(ESMF_LDIR)

DIRS      =

CLEANDIRS   = Build Install
CLEANFILES  =
CLOBBERDIRS =
