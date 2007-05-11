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

.SUFFIXES: .f90 .F90 .c .C

.f90:
	$(ESMF_F90COMPILER) -c $(ESMF_F90COMPILEOPTS) $(ESMF_F90COMPILEPATHS) $(ESMF_F90COMPILECPPFLAGS) $<
	$(ESMF_F90LINKER) $(ESMF_F90LINKOPTS) $(ESMF_F90LINKPATHS) $(ESMF_F90LINKRPATHS) -o $@ $*.o $(ESMF_F90ESMFLINKLIBS)        

.F90:
	$(ESMF_F90COMPILER) -c $(ESMF_F90COMPILEOPTS) $(ESMF_F90COMPILEPATHS) $(ESMF_F90COMPILEFREECPP) $(ESMF_F90COMPILECPPFLAGS) $<
	$(ESMF_F90LINKER) $(ESMF_F90LINKOPTS) $(ESMF_F90LINKPATHS) $(ESMF_F90LINKRPATHS) -o $@ $*.o $(ESMF_F90ESMFLINKLIBS)        
        
.c:
	$(ESMF_CXXCOMPILER) -c $(ESMF_CXXCOMPILEOPTS) $(ESMF_CXXCOMPILEPATHSLOCAL) $(ESMF_CXXCOMPILEPATHS) $(ESMF_CXXCOMPILECPPFLAGS) $<
	$(ESMF_CXXLINKER) $(ESMF_CXXLINKOPTS) $(ESMF_CXXLINKPATHS) $(ESMF_CXXLINKRPATHS) -o $@ $*.o $(ESMF_CXXESMFLINKLIBS)

.C:
	$(ESMF_CXXCOMPILER) -c $(ESMF_CXXCOMPILEOPTS) $(ESMF_CXXCOMPILEPATHSLOCAL) $(ESMF_CXXCOMPILEPATHS) $(ESMF_CXXCOMPILECPPFLAGS) $<
	$(ESMF_CXXLINKER) $(ESMF_CXXLINKOPTS) $(ESMF_CXXLINKPATHS) $(ESMF_CXXLINKRPATHS) -o $@ $*.o $(ESMF_CXXESMFLINKLIBS)



# -----------------------------------------------------------------------------
all: esmf_application

esmf_application:

esmc_application:

# -----------------------------------------------------------------------------
.PHONY: check
check:
	@if [ -f esmf_application ]; then \
          if [ -f esmc_application ]; then \
            echo "PASS: check"; \
          fi; \
        fi

# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
.PHONY: clean
clean:
	rm -f esmf_application esmc_application *.o *.mod
