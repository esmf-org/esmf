# $Id: makefile,v 1.11 2004/06/16 16:21:06 nscollins Exp $

ALL: build_here

CFLAGS    =
FFLAGS    = 
SOURCEC   = 
SOURCEF   = wrf_io.F90 field_routines.F90 module_wrf_error.F wrf_abort.F
CODE      = ext_ncd_get_dom_ti.code ext_ncd_get_var_td.code ext_ncd_get_var_ti.code ext_ncd_put_dom_ti.code ext_ncd_put_var_td.code ext_ncd_put_var_ti.code transpose.code
STOREH    = 

OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))

MANSEC    = IOobj
LIBBASE   = libesmf

# This needs to be the path of this directory relative to the
# top level esmf directory.
LOCDIR    = src/Infrastructure/IO/src

# Leave this line alone.
include $(ESMF_DIR)/makefile

# The DIRS line needs to contain all subdirectories which exist 
# directly below this directory, and have either library,
# example/test code, or documents which need to be generated.
DIRS =

CLEANDIRS   =
CLEANFILES  = wrf_io.F field_routines.F *.o *mod
CLOBBERDIRS =

libf:${LIBNAME}(${OBJSF}) wrf_io.o field_routines.o

wrf_io.o: wrf_io.F
field_routines.o: field_routines.F

ifeq ($(ESMF_ARCH),IRIX64)
wrf_io.F: wrf_io.F90 $(CODE)
	@echo "using local rule in irix section"
	sed -e '1,/BEGINIOFLAGS/d' -e '/ENDIOFLAGS/,$$d' $< > wrf_io_flags.h
	$(CPP) -D__FILE__='"'wrf_io.F90'"' -traditional -x assembler-with-cpp -E -P -I./ $< | $(M4)  -Uinclude -Uindex -Ulen - > $@

field_routines.F: field_routines.F90 wrf_io.F
	@echo "using local rule in irix section"
	$(CPP) -D__FILE__='"'field_routines.F90'"' -traditional -x assembler-with-cpp -E -P -I./ $<  > $@

else
wrf_io.F: wrf_io.F90 $(CODE)
	@echo "using local rule in non-irix section"
	sed -e '1,/BEGINIOFLAGS/d' -e '/ENDIOFLAGS/,$$d' $< > wrf_io_flags.h
	$(CPP) -U__FILE__ -D__FILE__='"'$(*.F).F90'"' -traditional -x assembler-with-cpp -E -P -I./ $< | $(M4)  -Uinclude -Uindex -Ulen - > $@

field_routines.F: field_routines.F90 wrf_io.F90
	@echo "using local rule in non-irix section"
	$(CPP) -U__FILE__ -D__FILE__='"'$(*.F).F90'"' -traditional -x assembler-with-cpp -E -P -I./ $< > $@

endif
