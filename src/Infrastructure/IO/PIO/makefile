# $Id$

ALL: build_here

# This needs to be the path of this directory relative to the
# top level esmf directory.
LOCDIR	  = src/Infrastructure/IO/PIO

# Leave this line alone.
include $(ESMF_DIR)/makefile

PIO_CMAKE_OPTS = -Wno-dev -DPIO_ENABLE_TIMING=OFF -DPIO_ENABLE_EXAMPLES=OFF -DPIO_ENABLE_FORTRAN=OFF

ifeq ("$(ESMF_BOPT)","g")
  PIO_CMAKE_OPTS += -DPIO_ENABLE_LOGGING=ON -DCMAKE_BUILD_TYPE=DEBUG
else
  PIO_CMAKE_OPTS += -DPIO_ENABLE_LOGGING=OFF -DCMAKE_BUILD_TYPE=release
endif

ifdef ESMF_NETCDF_INCLUDE
  ifneq ("$(wildcard $(ESMF_NETCDF_LIBPATH)/libnetcdf.a)","")
    PIO_CMAKE_OPTS += -DNetCDF_C_INCLUDE_DIR=$(ESMF_NETCDF_INCLUDE) -DNetCDF_C_LIBRARY=$(ESMF_NETCDF_LIBPATH)/libnetcdf.a
  else
    PIO_CMAKE_OPTS += -DNetCDF_C_INCLUDE_DIR=$(ESMF_NETCDF_INCLUDE) -DNetCDF_C_LIBRARY=$(ESMF_NETCDF_LIBPATH)/libnetcdf.so
  endif
else
  $(error PIO requires NetCDF!!!)
endif

ifdef ESMF_PNETCDF_INCLUDE
  PIO_CMAKE_OPTS += -DPnetCDF_PATH=$(ESMF_PNETCDF_INCLUDE)/../
else
  PIO_CMAKE_OPTS += -DWITH_PNETCDF=off
  PIO_CONF_OPTS += --disable-pnetcdf
endif


ifndef PIO_AUTOTOOLS_BUILD
# Use the PIO native cmake build system to build and install
tree_lib:
	cmake --version
	mkdir -p Build
	cd Build; \
	env CC=$(ESMF_CCOMPILER) FC=$(ESMF_F90COMPILER) CFLAGS="$(ESMF_COPTFLAG) $(ESMF_SO_CCOMPILEOPTS)" cmake -DCMAKE_INSTALL_PREFIX=../Install $(PIO_CMAKE_OPTS) ../ParallelIO; \
	make VERBOSE=1 -f ./Makefile; \
	make -f ./Makefile install; \
	cp -fp ../Install/include/* $(ESMF_INCDIR); \
	cp -fp ../Install/lib/* $(ESMF_LDIR)
else
# Use the PIO native autoconf build system to build and install
tree_lib:
	mkdir -p Install
	cd ParallelIO; \
	cp -f ../configure . ;\
	./configure CC=$(ESMF_CCOMPILER) CFLAGS="$(ESMF_COPTFLAG) $(ESMF_SO_CCOMPILEOPTS)" $(PIO_CONF_OPTS) --prefix=$(ESMF_DIR)/src/Infrastructure/IO/PIO/Install ;\
	make -f ./Makefile ;\
	make -f ./Makefile install ;\
	cp -fp ../Install/include/* $(ESMF_INCDIR); \
	cp -fp ../Install/lib/* $(ESMF_LDIR)
endif
DIRS      =

CLEANDIRS   = Build Install
CLEANFILES  =
CLOBBERDIRS =
