# $Id: build_rules.mk,v 1.12.2.1 2008/07/16 00:19:14 theurich Exp $
# 
# Unicos.default.default
#

############################################################
# Default compiler setting.
#
ESMF_F90DEFAULT         = ftn -h ssp -dy
ESMF_F90LINKERDEFAULT   = CC -h new_for_init -h ssp
ESMF_CXXDEFAULT         = CC -h new_for_init -h ssp

############################################################
# Default MPI setting.
#
ifeq ($(ESMF_COMM),default)
export ESMF_COMM := mpi
endif

############################################################
# MPI dependent settings.
#
ifeq ($(ESMF_COMM),mpiuni)
# MPI stub library -----------------------------------------
ESMF_F90COMPILECPPFLAGS+= -DESMF_MPIUNI
ESMF_CXXCOMPILECPPFLAGS+= -DESMF_MPIUNI
ESMF_CXXCOMPILEPATHS   += -I$(ESMF_DIR)/src/Infrastructure/stubs/mpiuni
ESMF_MPIRUNDEFAULT      = $(ESMF_DIR)/src/Infrastructure/stubs/mpiuni/mpirun
else
ifeq ($(ESMF_COMM),mpi)
# Vendor MPI -----------------------------------------------
ESMF_F90LINKLIBS       += -lmpi
ESMF_CXXLINKLIBS       += -lmpi
ESMF_MPIRUNDEFAULT      = mpirun.unicos
ESMF_MPIMPMDRUNDEFAULT  =
else
ifeq ($(ESMF_COMM),user)
# User specified flags -------------------------------------
else
$(error Invalid ESMF_COMM setting: $(ESMF_COMM))
endif
endif
endif

############################################################
# Print compiler version string
#
ESMF_F90COMPILER_VERSION    = ${ESMF_F90COMPILER} -V
ESMF_CXXCOMPILER_VERSION    = ${ESMF_CXXCOMPILER} -V

############################################################
# on X1 optimization level must be > 1 else no optimization
#
ESMF_OPTLEVELDEFAULT  = 2

############################################################
# Blank out variables to prevent rpath encoding
#
ESMF_F90LINKRPATHS      =
ESMF_CXXLINKRPATHS      =

############################################################
# Help ftn to understand Fortran suffices
#
ESMF_F90COMPILEFREECPP   = -f free -N 255 -F -M1549
ESMF_F90COMPILEFREENOCPP = -f free -N 255
ESMF_F90COMPILEFIXCPP    = -f fixed -N 132 -F -M1549
ESMF_F90COMPILEFIXNOCPP  = -f fixed -N 132

############################################################
# Tell ftn to use module files
#
ESMF_F90IMOD        = -em -J

############################################################
# X1 does not have a ranlib -> "true" is a noop command
#
ESMF_RANLIBDEFAULT         = true

############################################################
# Link against libesmf.a using the F90 linker front-end
#
ESMF_F90LINKLIBS +=

############################################################
# Link against libesmf.a using the C++ linker front-end
#
ESMF_CXXLINKLIBS +=

############################################################
# Blank out shared library options
#
ESMF_SL_LIBS_TO_MAKE  =

############################################################
# Exclude I1 and I2 data kinds
#
ESMF_F90COMPILECPPFLAGS += -DESMF_NO_INTEGER_1_BYTE
ESMF_F90COMPILECPPFLAGS += -DESMF_NO_INTEGER_2_BYTE

############################################################
# Define CPPRULES here instead of using those in common.mk
#
ESMF_CPPRULES         = defined
ESMF_CPPDEFAULT       = cpp

# change to not depend upon gcc -E -P behavior, remove -P and add filter 
# to delete #line
# amend bad cpp ".TRUE.", ". NOT." , "=>' output
# fix leading space that seems to show up on some lines
.cpp.F90:
	${ESMF_CPP} -E -I${ESMF_INCDIR} $< | tr "@^" "\n#" | $(ESMF_SED) -e 's/^ //' -e '/^#line/d' -e 's/ \. /./g' -e 's/\. not\./.not./g' -e 's/= >/=>/g' > $(dir$<)$(notdir $@)
.cppF90.F90:
	cp $< $<.cpp; ${ESMF_CPP} -E -I${ESMF_INCDIR} $(FPPDEFS) $<.cpp  | tr "@^|" "\n#'" | $(ESMF_SED) -e 's/^ //' -e '/^#line/d' -e 's/ \. /./g' -e 's/\. not\./.not./g' -e 's/= >/=>/g' > $(dir$<)$(notdir $@); rm -f $<.cpp
