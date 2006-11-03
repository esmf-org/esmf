# Example GNU Makefile for user ESMF application

################################################################################
################################################################################
## This Makefile must be able to find the "esmf.mk" Makefile fragment in the  ##
## 'include' line below. When this Makefile is used as an example the user    ##
## must adjust the path in order to find the "esmf.mk" according to their     ##
## situation!                                                                 ##
## It is _not_ necessary to set and use the ESMF_INSTALL_LIBDIR_ABSPATH       ##
## variable, it is perfectly fine to hardcode the path to "esmf.mk" directly  ##
## into the 'include' line.                                                   ##
################################################################################
ifndef ESMF_INSTALL_LIBDIR_ABSPATH
ESMF_INSTALL_LIBDIR_ABSPATH = /this/is/just/a/dummy/path
endif
################################################################################
## This is the important statement: Make sure 'include' finds your "esmf.mk"!!!
include $(ESMF_INSTALL_LIBDIR_ABSPATH)/esmf.mk
################################################################################
################################################################################

.SUFFIXES: .f90

.f90.o:
	$(ESMF_F90COMPILER) -c $(ESMF_F90COMPILEOPTS) $(ESMF_F90COMPILEPATHS) $(ESMF_F90COMPILECPPFLAGS) $<
        
.o:
	$(ESMF_F90LINKER) $(ESMF_F90LINKOPTS) $(ESMF_F90LINKPATHS) $(ESMF_F90LINKRPATHS) -o $@ $^ $(ESMF_F90ESMFLINKLIBS)        


# -----------------------------------------------------------------------------
esmf_application: esmf_application.o

# -----------------------------------------------------------------------------
.PHONY: check
check:
	@if [ -f esmf_application ]; then \
           echo "PASS: check"; \
        fi

# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
.PHONY: clean
clean:
	rm -f esmf_application *.o *.mod
