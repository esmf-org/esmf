# Example GNU Makefile for user ESMF application

# -> adjust the path in the following lines to match your situation:
ifndef ESMF_INSTALL_PREFIX_ABSPATH
ESMF_INSTALL_PREFIX_ABSPATH = /you/need/to/adjust/this/path
endif
ifndef ESMF_INSTALL_LIBDIR
ESMF_INSTALL_LIBDIR = lib
endif
# -> adjust the path in the following lines to match your situation:
include $(ESMF_INSTALL_PREFIX_ABSPATH)/$(ESMF_INSTALL_LIBDIR)/esmf.mk

.SUFFIXES: .f90

.f90.o:
	$(ESMF_F90COMPILER) -c $(ESMF_F90COMPILEOPTS) $(ESMF_F90COMPILEPATHS) $(ESMF_F90COMPILECPPFLAGS) $<
        
.o:
	$(ESMF_F90LINKER) $(ESMF_F90LINKOPTS) $(ESMF_F90LINKPATHS) $(ESMF_F90LINKRPATHS) -o $@ $^ $(ESMF_F90ESMFLINKLIBS)        


# -----------------------------------------------------------------------------
esmf_application: esmf_application.o

# -----------------------------------------------------------------------------
check:
	@if [ -f esmf_application ]; then \
           echo "PASS: check"; \
        fi

# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------

.PHONY: clean
clean:
	rm -f esmf_application *.o *.mod
