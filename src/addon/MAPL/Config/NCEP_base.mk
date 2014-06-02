# +-======-+ 
#  Copyright (c) 2003-2007 United States Government as represented by 
#  the Admistrator of the National Aeronautics and Space Administration.  
#  All Rights Reserved.
#  
#  THIS OPEN  SOURCE  AGREEMENT  ("AGREEMENT") DEFINES  THE  RIGHTS  OF USE,
#  REPRODUCTION,  DISTRIBUTION,  MODIFICATION AND REDISTRIBUTION OF CERTAIN 
#  COMPUTER SOFTWARE ORIGINALLY RELEASED BY THE UNITED STATES GOVERNMENT AS 
#  REPRESENTED BY THE GOVERNMENT AGENCY LISTED BELOW ("GOVERNMENT AGENCY").  
#  THE UNITED STATES GOVERNMENT, AS REPRESENTED BY GOVERNMENT AGENCY, IS AN 
#  INTENDED  THIRD-PARTY  BENEFICIARY  OF  ALL  SUBSEQUENT DISTRIBUTIONS OR 
#  REDISTRIBUTIONS  OF THE  SUBJECT  SOFTWARE.  ANYONE WHO USES, REPRODUCES, 
#  DISTRIBUTES, MODIFIES  OR REDISTRIBUTES THE SUBJECT SOFTWARE, AS DEFINED 
#  HEREIN, OR ANY PART THEREOF,  IS,  BY THAT ACTION, ACCEPTING IN FULL THE 
#  RESPONSIBILITIES AND OBLIGATIONS CONTAINED IN THIS AGREEMENT.
#  
#  Government Agency: National Aeronautics and Space Administration
#  Government Agency Original Software Designation: GSC-15354-1
#  Government Agency Original Software Title:  GEOS-5 GCM Modeling Software
#  User Registration Requested.  Please Visit http://opensource.gsfc.nasa.gov
#  Government Agency Point of Contact for Original Software:  
#  			Dale Hithon, SRA Assistant, (301) 286-2691
#  
# +-======-+ 
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
