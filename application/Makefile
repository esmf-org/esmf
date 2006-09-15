# Example GNU Makefile for user ESMF application

# -> adjust the path in the following line to match your situation:
include ../lib/libg/Linux.intel.32.mpich.default/esmf.mk

.SUFFIXES: .f90

.f90.o:
	$(ESMF_F90COMPILER) -c $(ESMF_F90COMPILEOPTS) $(ESMF_F90COMPILEPATHS) $(ESMF_F90COMPILECPPFLAGS) $<
        
.o:
	$(ESMF_F90LINKER) $(ESMF_F90LINKOPTS) $(ESMF_F90LINKPATHS) $(ESMF_F90LINKRPATHS) -o $@ $^ $(ESMF_F90ESMFLINKLIBS)        


# -----------------------------------------------------------------------------

esmf_application: esmf_application.o

# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------

.PHONY: clean
clean:
	rm -f esmf_application *.o *.mod
