#
# Earth System Modeling Applications (ESMA) base makefile fragment.
# This fragment defines Generic macros for compiling and linking the
# GOCART Grid Component into GFS.
#
# REVISION HISTORY:
#
# 23jul09  da Silva  First Crack
#
#--------------------------------------------------------------------------

I = -I
LIB_SDF = /usrx/local/netcdf.3.5.0/lib/libnetcdf.a

ifndef ARCH             # Architecture, e.g., IRIX64 
  ARCH := $(shell uname -s)
endif

# Installation Directories
# ------------------------
  ESMABIN = $(ESMADIR)/$(ARCH)/bin
  ESMALIB = $(ESMADIR)/$(ARCH)/lib
  ESMAINC = $(ESMADIR)/$(ARCH)/include
  ESMAMOD = $(ESMADIR)/$(ARCH)/include
  ESMAETC = $(ESMADIR)/$(ARCH)/etc
  ESMADOC = $(ESMADIR)/$(ARCH)/doc
  ESMACFG = $(ESMADIR)/$(ARCH)/Config

incdir = $(ESMAINC)
libdir = $(ESMALIB)

GOCART_INCS = GMAO_gfio_r8 GMAO_mpeu GMAO_pilgrim MAPL_Base \
              MAPL_cfio_r4 Chem_Base Chem_Shared GEOSchem_GridComp

ifeq ($(GOCART_MODE),stub) 
  GOCART_LIBS = libGOCART_GridComp.a libChem_Base.a

else
  GOCART_LIBS = libGOCART_GridComp.a libCARMA_GridComp.a \
              libCO2_GridComp.a libCO_GridComp.a libCFC_GridComp.a \
              libO3_GridComp.a libOC_GridComp.a libBC_GridComp.a \
              libSS_GridComp.a libSU_GridComp.a libRn_GridComp.a \
              libChem_Base.a libChem_Shared.a libGMAO_gfio_r8.a \
              libGMAO_mpeu.a libGMAO_pilgrim.a libMAPL_Base.a \
              libMAPL_cfio_r4.a
endif

INC_GOCART = $(foreach dir,$(GOCART_INCS),$(I)$(incdir)/$(dir) )
LIB_GOCART = $(foreach lib,$(GOCART_LIBS),$(libdir)/$(lib) ) $(LIB_SDF)
