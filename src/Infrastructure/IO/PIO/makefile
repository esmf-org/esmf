# $Id$

ALL: build_here

# This needs to be the path of this directory relative to the
# top level esmf directory.
LOCDIR	  = src/Infrastructure/IO/PIO

# Leave this line alone.
include $(ESMF_DIR)/makefile

PIO_CMAKE_OPTS = -DPIO_ENABLE_TIMING=OFF

ifeq ("$(ESMF_BOPT)","g")
  PIO_CMAKE_OPTS += -DPIO_ENABLE_LOGGING=ON -DCMAKE_BUILD_TYPE=DEBUG
endif

ifdef ESMF_NETCDF_INCLUDE
  PIO_CMAKE_OPTS += -DNetCDF_PATH=$(ESMF_NETCDF_INCLUDE)/..
else
  $(error PIO requires NetCDF!!!)
endif

ifdef ESMF_PNETCDF_INCLUDE
  PIO_CMAKE_OPTS += -DPnetCDF_PATH=$(ESMF_PNETCDF_INCLUDE)/..
else
  PIO_CMAKE_OPTS += -DWITH_PNETCDF=off
endif

#TODO: Fix hard-coded CC issue below...
tree_lib:
	mkdir -p Build
	cd Build; \
	env CC=mpicc FC=$(ESMF_F90COMPILER) CFLAGS="$(ESMF_CXXOPTFLAG) $(ESMF_SO_CXXCOMPILEOPTS)" cmake -DCMAKE_INSTALL_PREFIX=../Install $(PIO_CMAKE_OPTS) ../ParallelIO; \
	make -f ./Makefile; \
	make -f ./Makefile install; \
	cp -fp ../Install/include/* $(ESMF_INCDIR); \
	cp -fp ../Install/lib/* $(ESMF_LDIR)

DIRS      =

CLEANDIRS   = Build Install
CLEANFILES  =
CLOBBERDIRS =
