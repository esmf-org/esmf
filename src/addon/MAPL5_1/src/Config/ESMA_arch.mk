#
# Earth System Modeling Applications (ESMA) base makefile fragment.
# This fragment costumizes ESMF_base.mk for each each architecture. 
#
# REVISION HISTORY:
#
# 06Jun2003  da Silva  First Crack
# 07aug2003  Zaslavsky Added -64 option for IRIX64
# 29Oct2003  Sawyer    Merged back most material lost in 1.26
# 01Dec2003  da Silva  Mods to IRIX64 for building GMAO legacy code
# 02Dec2003  da Silva  Changed INC_MPI to /usr/include (blank causes -I )
# 28Mar2005  Owens/RT  Added courant.
# 01Apr2005  Todling   Changed location of perl on IRIX64 to /usr/bin
# 04Apr2005  da Silva  Changed back perl on IRIX as /usr/bin/perl breaks fdp
# 26Apr2005  da Silva  Merged in Carlos mods for building FVGSI.
# 27Mar2006  da Silva  On IRIX, removed $(OMPFLAG) from FFLAGS and kept its
#                      "-mp" definition; user is supposed to activated where
#                      desired, not by default. Also, added OMPFLAG to Altix.
# 18Apr2006  Stassi    Added MPFLAG for Intel Fortran Compiler
# 05Jan2007  Todling   Add -lsz from palm current baselibs
# 26Jan2007  da Silva  Merged in Kokron's discover port
# 23May2007  Stassi    Removed lf95 as possible Linux default f90 compiler
# 13Aug2008  da Silva  Removed g77 from LIBSCI under Linux.
# 25Oct2008  da Silva  Improved Mac support, made openmpi default on Linux, 
#                      added detection of intelmpi. Slowly, moving towards
#                      use of mpif90. Added F2PY customization.
# 22Jun2009  Stassi    Use mklpath.pl script to get Intel MKLPATH
# 19Jul2012  Thompson  Added blocks for Intel 12 to work on pleiades and
#                      discover with the AVX option as a choice FOPT path.
#                      Added detection of MPT as a possible MPI stack for
#                      PGI on Pleiades. Moved to CUDA 4.1 as default GPU 
#                      level.
# 24Sep2012  Thompson  Added blocks for Intel 13 to work on pleiades and
#                      discover with the AVX option as a choice FOPT path.
# 26Mar2013  Thompson  Added blocks for Open MPI that uses 
#                      'mpif90 -showme:link' to identify the correct flags
#                      for linking with Open MPI.
#                      Corrected MPT LIB_MPI via Dan Kokron.
#                      Corrected Intel 13 FPE to use '-fp-model source' as
#                      '-fp-model precise' is deprecated.
#                      For PGI, added Kepler info, moved to use gcc and 
#                      g++ for CC and CXX, and turned off the dual
#                      TARGET_ARCH by default.
# 03Dec2013  Thompson  Added entries for Intel 14 following Intel 13
#                      example. Removed sections for OSF1, AIX, IRIX64.
#                      Removed options specific to Intel Fortran 8, 9, 10.
#                      Fixed behavior of OMPFLAG. Additional entries for
#                      PGI in re arch and GPU.
# 04Aug2014  Thompson  Move to a generic target architecture (-tp=px-64)
#                      on PGI. This avoids any issues with having the wrong
#                      executable (in most cases). Also, use PGI_MAJOR
#                      to select the CUDA version to compile to as PGI 13 
#                      does not support CUDA 6.0 and higher.
# 29Aug2014  Thompson  Add support for Intel 15 and gfortran 4.9.1. Added
#                      new possible environment variable ESMA_DEVEL_BOPT
#                      which can allow for "quasi-permanent" setting of
#                      BOPT=Og or BOPT=g
# 09Sep2014  Thompson  Remove references to ia64 or i686 machines. Remove
#                      MPFLAG from the Intel section as it was always turned
#                      off for all Intel compilers. Clarified some of the
#                      multiple endif chains
# 28Oct2014  Thompson  Remove references to Intel 11 and 12. Alter the
#                      "high optimization" FOPT flags for Intel 15 to add
#                      support for Haswell.
# 23Apr2015  Thompson  Initial support for Intel 16.
# 23Jul2015  Thompson  Per suggestions of Max Suarez, have Intel act like
#                      GNU and PGI and put automatics on the heap rather
#                      than the stack. Current behavior is like GNU: all
#                      automatics smaller than 32k on the stack, the rest
#                      on the heap.
#
#--------------------------------------------------------------------------

# -----
# TO DO: Remove default for BASEDIR, FC from here; user must set BASEDIR
# -----  and ESMA_FC to control these parameters.
#
  ifndef BASEDIR
     $(warning BASEDIR is undefined --- this will cause an error in the next release!)
  endif

#                             ---------------------
#                             User defined defaults
#                             ---------------------
  ifdef ESMA_FC
     FC := $(ESMA_FC)
  endif
  ifdef ESMA_F2PY
     F2PY := $(ESMA_F2PY)
  endif
  ifdef ESMA_F2PY_FLAGS
     F2PY_FLAGS += $(ESMA_F2PY_FLAGS)
  endif

#                               -----
#                               Linux
#                               -----

ifeq ($(ARCH),Linux)

# Linux default compilers
# -----------------------
  ifndef ESMA_FC
     FC := ifort
  endif
  CC  = cc
  CXX = c++
  CPP = cpp

  ifdef ESMA_DEVEL_BOPT
     ifeq ($(ESMA_DEVEL_BOPT),Og)
        BOPT = Og
     endif
     ifeq ($(ESMA_DEVEL_BOPT),g)
        BOPT = g
     endif
  endif

#
#                    Linux Site Specific
#                    -------------------

# MAT: I do not think these are needed on modern
#      builds of the model. Carefully make sure 
#      before deleting
#
# Add -lgpfs to LIB_HDF5 on borg/discover nodes
# ---------------------------------------------
  ifeq ($(findstring borg,$(NODE)),borg)
     LIB_HDF5 += -lgpfs
  endif
  ifeq ($(findstring discover,$(NODE)),discover)
     LIB_HDF5 += -lgpfs
  endif

# When building for profiling, use BOPT=Og
# ----------------------------------------
  ifeq ("$(DOING_APROF)","yes")
     BOPT = Og
     LIB_APROF = -Wl,@$(BASELIB)/allinea-profiler.ld
  endif

#
#                    Linux Compiler Specific
#                    -----------------------

# Absoft compiler
# ---------------
  ifeq ($(FC), f90) 
      EXTENDED_SOURCE = -W 132
      FREE_SOURCE = -f free
      FIXED_SOURCE = -f fixed
      LIB_ESMF = $(BASELIB)/esmf/libesmf.a #$(BASELIB)/esmf/libnetcdf_stubs.a
      LIB_MPI = -L$(BASELIB)/mpi -lpmpich++ -lfmpich -lmpich -lmpichfsup
      LIB_SYS = -lstdc++ -lpthread -lU77 -lrt
      FREAL4 =
      FREAL8 = -N113
      M = -p
      FINCS += -I$(BASEINC)/mpi
      FDEFS += -DABSOFT -DNO_R16 
      XFLAGS += -YEXT_NAMES=LCS -YEXT_SFX=_ $(EXTENDED_SOURCE)
      FOPT   = -g -trap=INVALID,DIVBYZERO,OVERFLOW
  endif

# Intel Fortran Compiler (ifort or mpiifort)
# ------------------------------------------
  ifeq ($(word 1,$(shell $(FC) --version)), ifort)

#   Determine compiler version
#   --------------------------
    IFORT_VER := $(subst ., ,$(word 3,$(shell ifort --version)))
    IFORT_MAJOR := $(word 1,$(IFORT_VER))
    IFORT_MINOR := $(word 2,$(IFORT_VER))
    FPIC := -fPIC
    EXTENDED_SOURCE := -extend_source
    FREE_SOURCE := -free
    FIXED_SOURCE := -fixed
    OMPFLAG  := -openmp
    PP  := -fpp
    BIG_ENDIAN := -convert big_endian
    BYTERECLEN := -assume byterecl
    FPE = -fpe0 -fp-model source -heap-arrays 32
    ALIGNCOM = -align dcommons
    MCMODEL = -mcmodel medium  -shared-intel
    FREAL4 =
    FREAL8 = -r8
    FOPT2 += 
    ifeq ("$(BOPT)","g")
       FOPT = $(FOPTG) -O0 -ftz -align all -fno-alias -traceback -debug -nolib-inline -fno-inline-functions -assume protect_parens,minus0 -prec-div -prec-sqrt -check bounds -check uninit -fp-stack-check -ftrapuv -warn unused
       ifeq ($(IFORT_MAJOR),16)
          FOPT += -init=snan,arrays
       endif
    else
       ifeq ($(IFORT_MAJOR),13)
          FOPT = $(FOPT3) -vec-report0 -ftz -align all -fno-alias
#         FOPT = $(FOPT3) -axAVX -xSSE4.1 -vec-report0 -ftz -align all -fno-alias
       else
       ifeq ($(IFORT_MAJOR),14)
          FOPT = $(FOPT3) -vec-report0 -ftz -align all -fno-alias
#         FOPT = $(FOPT3) -axAVX -xSSE4.1 -vec-report0 -ftz -align all -fno-alias
       else
       ifeq ($(IFORT_MAJOR),15)
          FOPT = $(FOPT3) -qopt-report0 -ftz -align all -fno-alias
#         FOPT = $(FOPT3) -axCORE-AVX2,AVX -xSSE4.2 -qopt-report0 -ftz -align all -fno-alias -align array32byte
          
#         For lower precision, but possibly better performance from AVX instructions, enable this
#         FPE := -fpe3 -fp-model fast=2 -no-prec-div
 
#         -openmp is deprecated in 15
          OMPFLAG := -qopenmp

          ifeq ("$(BOPT)","MIC")
             MICOPT += $(OMPFLAG)
             #MICOPT += $(OMPFLAG) -no-fma
             #MICOPT += $(OMPFLAG) -qopt-report-phase=offload -watch=mic-cmd
             AR := xiar
             AR_FLAGS += -qoffload-build
          else
             FOPT += -qno-offload
          endif

       else
       ifeq ($(IFORT_MAJOR),16)
          FOPT = $(FOPT3) -qopt-report0 -ftz -align all -fno-alias
#         FOPT = $(FOPT3) -axCORE-AVX2,AVX -xSSE4.2 -qopt-report0 -ftz -align all -fno-alias -align array32byte
          
#         For lower precision, but possibly better performance from AVX instructions, enable this
#         FPE := -fpe3 -fp-model fast=2 -no-prec-div

          # Intel 16 seems to require -fimf-arch-consistency=true to allow zero-diff on Haswell and Sandy
          FPE += -fimf-arch-consistency=true

#         -openmp is deprecated in 16
          OMPFLAG := -qopenmp

          ifeq ("$(BOPT)","MIC")
             MICOPT += $(OMPFLAG)
             #MICOPT += $(OMPFLAG) -no-fma
             #MICOPT += $(OMPFLAG) -qopt-report-phase=offload -watch=mic-cmd
             AR := xiar
             AR_FLAGS += -qoffload-build
          else
             FOPT += -qno-offload
          endif

       else
          FOPT = $(FOPT3)
       endif # IFORT 16
       endif # IFORT 15
       endif # IFORT 14
       endif # IFORT 13
    endif # BOPT=g

    # Always add traceback to FOPT on Intel
    FOPT += -traceback

    ifeq ("$(BOPT)","Og")
       FOPT += -g
    endif

    LIB_ESMF = $(BASELIB)/libesmf.a

    CC  = gcc
    CXX = g++

    ifdef ESMA_CC
       CC := $(ESMA_CC)
    endif

#   Handle MPI on x86_64
#   --------------------
    ifdef I_MPI_ROOT
        FC := mpiifort
        ifeq ($(MACH), x86_64) 
          INC_MPI := $(I_MPI_ROOT)/include64
          LIB_MPI := -L$(I_MPI_ROOT)/lib64  -lmpi -lmpiif # Intel MPI
          LIB_MPI_OMP := -L$(I_MPI_ROOT)/lib64  -lmpi_mt -lmpiif # Intel MPI
        else
          INC_MPI := $(I_MPI_ROOT)/include
          LIB_MPI := -L$(I_MPI_ROOT)/lib  -lmpi -lmpiif # Intel MPI
          LIB_MPI_OMP := -L$(I_MPI_ROOT)/lib  -lmpi_mt -lmpiif # Intel MPI
        endif
    else
    ifdef OPENMPI
        FC := mpif90
        INC_MPI := $(OPENMPI)/include
        OPENMPI_LINK_FLAGS := $(shell mpif90 -showme:link) -lmpi_cxx
        LIB_MPI := -L$(OPENMPI)/lib $(OPENMPI_LINK_FLAGS)
    else
    ifdef MVAPICH2
        FC := mpif90
        INC_MPI := $(MVAPICH2)/include
        LIB_MPI := -L$(MVAPICH2)/lib  -lmpich
    else
    ifdef M_MPI_ROOT
        FC := mpif90
        INC_MPI := $(M_MPI_ROOT)/include
        LIB_MPI := -L$(M_MPI_ROOT)/lib  -lmpich
    else
    ifdef MPI_HOME
        FC := mpif90
        INC_MPI := $(MPI_HOME)/include
        LIB_MPI := -L$(MPI_HOME)/lib  -lmpich
    else
    # This detects the MPT setup at NCCS
    ifdef MPT_VERSION
        FC := mpif90
        INC_MPI := $(MPI_ROOT)/include
        LIB_MPI := -L$(MPI_ROOT)/lib  -lmpi -lmpi++
    else
    ifdef FPATH
        FPATHS := $(subst :, ,$(FPATH))
        ifeq ($(MACH), x86_64) 
          FC := mpif90 
          INC_MPI := $(filter /nasa/sgi/mpt%,$(FPATHS)) \
                     $(filter /opt/scali%,$(FPATHS))
          INC_MPI := $(word 1,$(INC_MPI))
          LIB_MPI := -L$(subst include,lib,$(INC_MPI)) -lmpi -lmpi++
        endif
    endif # FPATH
    endif # MPT_VERSION
    endif # MPI_HOME
    endif # M_MPI_ROOT
    endif # MVAPICH2
    endif # OPENMPI
    endif # I_MPI_ROOT

#   Define LIB_SYS
#   --------------
    LIB_SYS := -lirc -ldl -lc -lpthread -lrt 

#   MKL math library
#   ----------------
    ifeq ($(wildcard $(ESMABIN)/mklpath.pl),$(ESMABIN)/mklpath.pl)
       MKLPATH = $(shell $(ESMABIN)/mklpath.pl)
    endif
    ifdef MKLPATH
       ifeq ($(wildcard $(MKLPATH)/libmkl_intel_lp64.so),)
           LIB_SCI += -lmkl_intel_lp64 -lmkl_sequential -lmkl_core
       else
           LIB_SCI += -L$(MKLPATH) -lmkl_intel_lp64 -lmkl_sequential -lmkl_core
       endif
    endif

#   Customize for each MACH
#   -----------------------
    GCC_DIR = $(shell dirname `gcc --print-libgcc-file-name`)
    ifeq ($(MACH), x86_64) 
       OVERRIDE_LIMITS =
       LOOP_VECT =
       FDEFS += $(D)HAVE_SHMEM
    endif # x86_64

    LIB_SYS += -L$(GCC_DIR) -lstdc++

    CFLAGS += $(FPIC)
    fFLAGS += $(FPIC) $(EXTENDED_SOURCE) $(FPE) $(OVERRIDE_LIMITS) $(ALIGNCOM)
    FFLAGS += $(FPIC) $(EXTENDED_SOURCE) $(FPE) $(OVERRIDE_LIMITS) $(ALIGNCOM)
    f90FLAGS += $(FPIC) $(FPE) $(OVERRIDE_LIMITS) $(ALIGNCOM)
    F90FLAGS += $(FPIC) $(FPE) $(OVERRIDE_LIMITS) $(ALIGNCOM)

#   Some safeguards
#   ---------------
    ifeq ($(INC_MPI),)
      FC := mpif90
      INC_MPI := $(dir $(shell which mpif90))../include
      LIB_MPI := -L$(dir $(shell which mpif90))../lib -lmpi -lmpi_cxx # -lmpi_f77
    endif

    ifeq ($(ESMA_PROFILE),TAU)
      ORIGFC := $(FC)
      export ORIGFC
      TAUROOTDIR   = /discover/swdev/mathomp4/tau-2.23/intel-14.0.1.106_mvapich2-2.0b
      TAU_MAKEFILE =  $(TAUROOTDIR)/x86_64/lib/Makefile.tau-icpc-mpi-pdt
      PDTROOTDIR   = /discover/swdev/mathomp4/src/pdtoolkit-3.20/x86_64/intel14.0.1.106
      #TAU_OPTIONS  = '-optPdtF90Parser="$(PDTROOTDIR)/bin/f95parse" -optPreProcess -optVerbose -optPDTInst -optTauSelectFile="$(ESMADIR)/src/Config/select.tau"'
      TAU_OPTIONS  = '-optPdtF90Parser="$(PDTROOTDIR)/bin/f95parse" -optPreProcess -optVerbose -optPDTInst -optRevert'
      #TAU_OPTIONS  = '-optPdtF90Parser="$(PDTROOTDIR)/bin/f95parse" -optPreProcess -optVerbose -optPDTInst -optRevert -optKeepFiles'
      export TAU_OPTIONS
      FC = $(TAUROOTDIR)/x86_64/bin/tau_f90.sh -tau_options=$(TAU_OPTIONS) -tau_makefile=$(TAU_MAKEFILE)
      F2PY = f2py --f77exec=$(ORIGFC) --f90exec=$(ORIGFC)
    endif
  endif

# Lahey F95 Compiler
# ------------------
  ifeq ($(ESMA_FC), lf95) 

      EXTENDED_SOURCE := --wide
      FREE_SOURCE = -free
      FIXED_SOURCE = -fixed
      FREAL4   := 
      FREAL8   := -CcdRR8  

      OMPFLAG = --openmp

      fFLAGS   += --warn $(EXTENDED_SOURCE)
      f90FLAGS += --warn $(EXTENDED_SOURCE)
      FFLAGS   += --warn $(EXTENDED_SOURCE)
      F90FLAGS += --warn $(EXTENDED_SOURCE)
      LDFLAGS  += --warn $(EXTENDED_SOURCE)

      LIB_SCI  = -llapackmt -lblasmt 
      LIB_SYS = -ldl -lc -lpthread -lrt 

#     AssumeOpenMPI is installed
#     --------------------------
      INC_MPI = /usr/include
      LIB_MPI = -lmpi
      override FC = mpif90
      OMPI_FC = lf95
      export OMPI_FC 

  endif

# GNU Fortran Compiler
# --------------------
  ifeq ($(ESMA_FC), gfortran) 

      CC = gcc

      LIB_ESMF = $(BASELIB)/libesmf.a

      EXTENDED_SOURCE := -ffixed-line-length-132
      FREE_SOURCE = 
      FIXED_SOURCE = -ffixed-form
      FREAL4   := 
      FREAL8   := -fdefault-real-8 -fdefault-double-8
      FINT8    := -fdefault-integer-8
      # This is needed for m_fpe.F90
      NO_RANGE_CHECK   := -fno-range-check 
      FPIC   := -fPIC
      # For some reason this does not work at the moment.
      #BIG_ENDIAN := -fconvert=swap
      FPE = -ffpe-trap=zero,overflow -fbacktrace
      ALIGNCOM = -falign-commons
      BYTERECLEN = -frecord-marker=4

      OMPFLAG = -fopenmp
      PP = -cpp

      ifeq ("$(BOPT)","g")
         FOPT = -O0 -g3 -gdwarf-2 -fbounds-check
      else
      ifeq ("$(BOPT)","Og")
         FOPT = -Og -g3 -gdwarf-2
      else
         #FOPT = $(FOPT3)
         #FOPT = $(FOPT3) -march=native -funroll-loops
         #FOPT = $(FOPT3) -march=native -funroll-loops -ffast-math
         FOPT = $(FOPT3) -march=westmere -mtune=generic -funroll-loops
      endif
      endif

      # Suggested by ESMF
      FOPT += -fcoarray=single

      CFLAGS   += $(FPIC)
      fFLAGS   += $(FPIC) $(D)__GFORTRAN__ $(EXTENDED_SOURCE) $(NO_RANGE_CHECK) $(FPE) $(ALIGNCOM)
      FFLAGS   += $(FPIC) $(D)__GFORTRAN__ $(EXTENDED_SOURCE) $(NO_RANGE_CHECK) $(FPE) $(ALIGNCOM)
      f90FLAGS += $(FPIC) $(D)__GFORTRAN__ -ffree-line-length-none $(NO_RANGE_CHECK) $(FPE) $(ALIGNCOM)
      F90FLAGS += $(FPIC) $(D)__GFORTRAN__ -ffree-line-length-none $(NO_RANGE_CHECK) $(FPE) $(ALIGNCOM)

      ifdef MVAPICH2
          FC := mpif90
          INC_MPI := $(MVAPICH2)/include
          LIB_MPI := -L$(MVAPICH2)/lib  -lmpich
      else
      ifdef OPENMPI
          FC := mpif90
          INC_MPI := $(OPENMPI)/include
          OPENMPI_LINK_FLAGS := $(shell mpif90 -showme:link) -lmpi_cxx
          LIB_MPI := -L$(OPENMPI)/lib $(OPENMPI_LINK_FLAGS)
      endif
      endif

#     MKL math library
#     ----------------
      ifeq ($(wildcard $(ESMABIN)/mklpath.pl),$(ESMABIN)/mklpath.pl)
          MKLPATH = $(shell $(ESMABIN)/mklpath.pl)
      endif
      ifneq ($(MKLPATH),)
      ifdef MKLPATH
          LIB_SCI += -L$(MKLPATH) -Wl,--no-as-needed -lmkl_gf_lp64 -lmkl_sequential -lmkl_core -lpthread -lm
      endif
      endif
    
      LIB_SYS = -ldl -lc -lpthread -lrt -lstdc++

  endif


# Wrapper around PGI compiler for the Cray XT-4 platform
# ------------------------------------------------------
  ifeq ($(FC),ftn)

    LIB_ESMF = $(BASELIB)/libesmf.a
    INC_MPI  = $(MPICH_DIR)/include
    LIB_MPI  = -L$(MPICH_DIR)/lib -lmpichf90

    LIB_SYS = -lstd -lrt -lC
    EXTENDED_SOURCE = -Mextend
    TARGET   = -target=linux
    FREAL4   = -r4
    FREAL8   = -r8
    FPE      = -Ktrap=divz,inv,ovf
    PP       = -Mpreprocess

    ifeq ("$(BOPT)","g")
#      FOPT   = $(FOPTG) -Ktrap=fp -Mbounds -Mchkptr
#      FOPT   = $(FOPTG) -O0 -Ktrap=fp -Mbounds
       FOPT   = $(FOPTG) -O0 -Ktrap=fp
    else
       FOPT   = -fast -Kieee
#      FOPT   = -fast -Mvect=nosse -Kieee
    endif
    BIG_ENDIAN =
    fFLAGS += $(EXTENDED_SOURCE) $(FPE) $(TARGET)
    FFLAGS += $(EXTENDED_SOURCE) $(FPE) $(TARGET)
    f90FLAGS += $(FPE) $(TARGET)
    F90FLAGS += $(FPE) $(TARGET)
    CFLAGS   += -DpgiFortran
    CXXFLAGS +=
    CC  := cc
    CXX := CC
  endif

# Portland Group Compiler (also CUDA Fortran)
# -------------------------------------------
  ifeq ($(ESMA_FC), pgfortran)

      # Determine compiler version
      # --------------------------
      PGI_VER := $(subst -, ,$(subst ., ,$(word 2,$(shell pgfortran --version | sed 1d))))
      PGI_MAJOR := $(word 1,$(PGI_VER))
      PGI_MINOR := $(word 2,$(PGI_VER))

      ifndef MPI_HOME
         MPI_HOME := $(dir $(shell which mpif90))..
      endif
      FC := $(MPI_HOME)/bin/mpif90
      FREE_SOURCE = -Mfree
      FIXED_SOURCE = -Mfixed
      EXTENDED_SOURCE = -Mextend
      LIB_ESMF = $(BASELIB)/libesmf.a
      FREAL4 = 
      FREAL8 = -r8
      FPE = -Ktrap=fp #Equiv to -Ktrap=divz,inv,ovf
      FPIC = -fpic
      BACKSLASH_STRING = -Mbackslash
      BIG_ENDIAN = -Mbyteswapio

      OMPFLAG  := -mp

      INC_PGI := $(dir $(shell which pgfortran))../include

      ifeq ("$(BOPT)","g")
         GPU_TARGET :=
         FOPT = -O0 -g -Kieee -Minfo=all -Mbounds -traceback -Mchkfpstk -Mchkstk -Mdepchk $(GPU_TARGET)
      else
      ifeq ("$(BOPT)","GPU")

         # Note here we specifically compile to the target hosting the GPU. GPUs cannot support unified
         # binaries, so do not even try.

         ifeq ($(PGI_MAJOR),13)
            GPU_CUDA_VER := 5.0
         endif
         ifeq ($(PGI_MAJOR),14)
            GPU_CUDA_VER := 5.5
         endif
         ifeq ($(PGI_MAJOR),15)
            GPU_CUDA_VER := 6.5
         endif

         ifndef GPU_CC_REV
            GPU_CC_REV := cc35
         endif

         GPU_TARGET := -Mcuda=nofma,ptxinfo,$(GPU_CUDA_VER),$(GPU_CC_REV),maxregcount:72 -acc -ta=nvidia:wait,nofma,$(GPU_CUDA_VER),$(GPU_CC_REV),maxregcount:72 -Minfo=accel,ccff

         FOPT = -fast -Kieee $(GPU_TARGET)
         USER_FDEFS += $(D)GPU_PRECISION=MAPL_R8 # Select precision for GPU Code that can be double prec (DQSAT, deledd)
         USER_FDEFS += $(D)GPU_MAXLEVS=137       # Select max level for GPU Code (could save space with this)
         USER_FDEFS += $(D)_CUDA                 # Set this always so the GEOS-5 dependency builder can use it.
      else
         GPU_TARGET :=
         FOPT = -fast -Kieee -g
      endif
      endif

      # PGI autoselects the optimization target to the be host currently compiling
      # thus if you compile on a Sandy Bridge (=sandybridge-64) node, you will
      # generate code that cannot run on a Westmere (=nehalem). For safety's sake, 
      # we default to a generic target processor (=px-64). This allows for good layout
      # regression as well as reproducible results. Note: speed does not seem to be
      # affected by this, but a specific target can always be selected.
      #
      # Note: We append this to FPE since not every file obeys FOPT. But nearly all
      #       obey FPE
      FPE += -tp=px-64

      ifeq ("$(BOPT)","Og")
         FOPT += -g -traceback
      endif

      #USER_FDEFS += $(D)OVERCAST # Uncomment for OVERCAST Radiation code

      fFLAGS   += $(FPIC) $(EXTENDED_SOURCE) $(BACKSLASH_STRING) $(FPE)
      FFLAGS   += $(FPIC) $(EXTENDED_SOURCE) $(BACKSLASH_STRING) $(FPE)
      f90FLAGS += $(FPIC) $(BACKSLASH_STRING) $(FPE)
      F90FLAGS += $(FPIC) $(BACKSLASH_STRING) $(FPE)
      
      CFLAGS += $(FPIC) -DpgiFortran
      PP = -Mpreprocess

      LDFLAGS += -pgcpplibs -tp=px-64

      INC_MPI = $(MPI_HOME)/include

      # Test if we are running Open MPI
      ifeq ($(findstring openMpi,$(INC_MPI)),openMpi)
         OPENMPI_LINK_FLAGS := $(shell mpif90 -showme:link)
         LIB_MPI := -L$(subst include,lib,$(INC_MPI)) $(OPENMPI_LINK_FLAGS)
      else
      ifeq ($(findstring openmpi,$(INC_MPI)),openmpi)
         OPENMPI_LINK_FLAGS := $(shell mpif90 -showme:link)
         LIB_MPI := -L$(subst include,lib,$(INC_MPI)) $(OPENMPI_LINK_FLAGS)
      else
      ifeq ($(findstring mvapich2,$(INC_MPI)),mvapich2)
         LIB_MPI := -L$(subst include,lib,$(INC_MPI)) -lmpich
      else
      ifeq ($(findstring mvapich,$(INC_MPI)),mvapich)
         LIB_MPI := -L$(subst include,lib,$(INC_MPI)) -lmpich
      else
      # Test if we are using mpt at NAS
      ifdef FPATH
          FPATHS := $(subst :, ,$(FPATH))
         INC_MPI := $(filter /nasa/sgi/mpt%,$(FPATHS)) \
                    $(filter /opt/scali%,$(FPATHS))
         INC_MPI := $(word 1,$(INC_MPI))
         LIB_MPI := -L$(subst include,lib64,$(INC_MPI)) -lmpi -lmpi++ -lstdc++
      else
         LIB_MPI := -L$(subst include,lib,$(INC_MPI)) -lmpich
      endif # FPATH
      endif # mvapich
      endif # mvapich2
      endif # openmpi
      endif # openMpi

      LIB_SYS = -ldl -lstd -lrt -lC $(GPU_TARGET)

#     MKL math library
#     ----------------
      ifeq ($(wildcard $(ESMABIN)/mklpath.pl),$(ESMABIN)/mklpath.pl)
          MKLPATH = $(shell $(ESMABIN)/mklpath.pl)
      endif
      ifneq ($(MKLPATH),)
      ifdef MKLPATH
          INC_SCI += -I$(MKLPATH)/include
          LIB_SCI += -Wl,--start-group $(MKLPATH)/libmkl_intel_lp64.a $(MKLPATH)/libmkl_sequential.a $(MKLPATH)/libmkl_core.a -Wl,--end-group -lpthread -lm
      endif
      endif
    
      CC = gcc
      CXX = g++

  endif

endif  #    Linux

#                               -----------------
#                               Darwin (Mac OS X)
#                               -----------------

ifeq ($(ARCH),Darwin)

# Linux default compilers
# -----------------------
  ifndef ESMA_FC
     FC = gfortran
  else
     override FC = $(ESMA_FC)
  endif
  CC  = gcc
  CXX = g++
  CPP = /usr/bin/cpp -xc++
  SED = /usr/bin/sed
  MAKEFLAGS = -w
  DLLEXT = dylib
  LIB_HDF = -lmfhdf -ldf -ljpeg -lz -lsz 
  LIB_SYS = 
  OMPFLAG = 

#
#                    Darwin Site Specific
#                    -------------------

# Darwin MPI default: assumes openmpi
# -----------------------------------
  INC_MPI := $(dir $(shell which mpif90))../include
  LIB_MPI := -L$(dir $(shell which mpif90))../lib -lmpi -lmpi_cxx -lmpi_f77

  MAC_VER := $(subst ., ,$(word 3,$(shell sw_vers -productVersion)))
  MAC_MAJOR := $(word 1,$(MAC_VER))
  MAC_MINOR := $(word 2,$(MAC_VER))

  # MAT INC_SYS doesn't seem used in GEOS-5. But this was added...commenting out for now
  #     pending further testing
  #INC_SYS := $(shell xcodebuild -version -sdk macosx$(MAC_MAJOR).$(MAC_MINOR) Path)/usr/include

#                    Darwin Compiler Specific
#                    -----------------------

# Linux default compilers
# -----------------------
  ifndef ESMA_FC
     FC := ifort
  endif
  CC  = cc
  CXX = c++
  CPP = cpp

  ifdef ESMA_DEVEL_BOPT
     ifeq ($(ESMA_DEVEL_BOPT),Og)
        BOPT = Og
     endif
     ifeq ($(ESMA_DEVEL_BOPT),g)
        BOPT = g
     endif
  endif

  RANLIB_FLAGS = -c


# Intel Fortran Compiler (ifort or mpiifort)
# ------------------------------------------
  ifeq ($(word 1,$(shell $(FC) --version)), ifort)

#   Determine compiler version
#   --------------------------
    IFORT_VER := $(subst ., ,$(word 3,$(shell ifort --version)))
    IFORT_MAJOR := $(word 1,$(IFORT_VER))
    IFORT_MINOR := $(word 2,$(IFORT_VER))
    FPIC := -fPIC
    EXTENDED_SOURCE := -extend_source
    FREE_SOURCE := -free
    FIXED_SOURCE := -fixed
    OMPFLAG  := -openmp
    PP  := -fpp
    BIG_ENDIAN := -convert big_endian
    BYTERECLEN := -assume byterecl
    FPE = -fpe0
    ALIGNCOM = -align dcommons
    MCMODEL = -mcmodel medium  -shared-intel
    FREAL4 =
    FREAL8 = -r8
    FOPT2 += 
    ifeq ("$(BOPT)","g")
       FOPT = $(FOPTG) -O0 -ftz -align all -fno-alias -traceback -debug -nolib-inline -fno-inline-functions -assume protect_parens,minus0 -prec-div -prec-sqrt -check bounds -check uninit -fp-stack-check -ftrapuv -warn unused
    else
       ifeq ($(IFORT_MAJOR),13)
          FOPT = $(FOPT3) -vec-report0 -ftz -align all -fno-alias
#         FOPT = $(FOPT3) -axAVX -xSSE4.1 -vec-report0 -ftz -align all -fno-alias
       else
       ifeq ($(IFORT_MAJOR),14)
          FOPT = $(FOPT3) -vec-report0 -ftz -align all -fno-alias
#         FOPT = $(FOPT3) -axAVX -xSSE4.1 -vec-report0 -ftz -align all -fno-alias
       else
       ifeq ($(IFORT_MAJOR),15)
          FOPT = $(FOPT3) -qopt-report0 -ftz -align all -fno-alias -m64
#         FOPT = $(FOPT3) -axCORE-AVX2,AVX -xSSE4.2 -qopt-report0 -ftz -align all -fno-alias -align array32byte
#         -openmp is deprecated in 15
          OMPFLAG := -qopenmp
       ifeq ($(IFORT_MAJOR),16)
          FOPT = $(FOPT3) -qopt-report0 -ftz -align all -fno-alias -m64
#         FOPT = $(FOPT3) -axCORE-AVX2,AVX -xSSE4.2 -qopt-report0 -ftz -align all -fno-alias -align array32byte
#         -openmp is deprecated in 15
          OMPFLAG := -qopenmp
       else
          FOPT = $(FOPT3)
       endif # IFORT 16
       endif # IFORT 15
       endif # IFORT 14
       endif # IFORT 13
    endif # BOPT=g

    ifeq ("$(BOPT)","Og")
       FOPT += -g -traceback
    endif

    LIB_ESMF = $(BASELIB)/libesmf.a

    CC  = gcc
    CXX = g++

    F2PY += --fcompiler=intelem

    # This makes the archiver ifort
    #AR := $(FC)
    #AR_FLAGS := -staticlib -o 

#   Handle MPI on x86_64
#   --------------------
    ifdef I_MPI_ROOT
        FC := mpiifort
        ifeq ($(MACH), x86_64) 
          INC_MPI := $(I_MPI_ROOT)/include64
          LIB_MPI := -L$(I_MPI_ROOT)/lib64  -lmpi -lmpiif # Intel MPI
          LIB_MPI_OMP := -L$(I_MPI_ROOT)/lib64  -lmpi_mt -lmpiif # Intel MPI
        else
          INC_MPI := $(I_MPI_ROOT)/include
          LIB_MPI := -L$(I_MPI_ROOT)/lib  -lmpi -lmpiif # Intel MPI
          LIB_MPI_OMP := -L$(I_MPI_ROOT)/lib  -lmpi_mt -lmpiif # Intel MPI
        endif
    else
    ifdef OPENMPI
        FC := mpif90
        INC_MPI := $(OPENMPI)/include
        OPENMPI_LINK_FLAGS := $(shell mpif90 -showme:link) -lmpi_cxx
        LIB_MPI := -L$(OPENMPI)/lib $(OPENMPI_LINK_FLAGS)
    else
    ifdef MVAPICH2
        FC := mpif90
        INC_MPI := $(MVAPICH2)/include
        LIB_MPI := -L$(MVAPICH2)/lib  -lmpich
    else
    ifdef M_MPI_ROOT
        FC := mpif90
        INC_MPI := $(M_MPI_ROOT)/include
        LIB_MPI := -L$(M_MPI_ROOT)/lib  -lmpich
    else
    ifdef MPI_HOME
        FC := mpif90
        INC_MPI := $(MPI_HOME)/include
        LIB_MPI := -L$(MPI_HOME)/lib  -lmpich
    else
    ifdef FPATH
        FPATHS := $(subst :, ,$(FPATH))
        ifeq ($(MACH), x86_64) 
          FC := mpif90 
          INC_MPI := $(filter /nasa/sgi/mpt%,$(FPATHS)) \
                     $(filter /opt/scali%,$(FPATHS))
          INC_MPI := $(word 1,$(INC_MPI))
          LIB_MPI := -L$(subst include,lib,$(INC_MPI)) -lmpi -lmpi++
        endif
    endif # FPATH
    endif # MPI_HOME
    endif # M_MPI_ROOT
    endif # MVAPICH2
    endif # OPENMPI
    endif # I_MPI_ROOT

#   Define LIB_SYS
#   --------------
    LIB_SCI := 
    #LIB_SYS := -ldl -lc -lpthread -lrt 
	 # librt does not exist on Darwin
    LIB_SYS := -ldl -lc -lpthread 

    ifeq ($(IFORT_MAJOR), 13)
          LIB_SYS := -lirc $(LIB_SYS)
          FPE += -fp-model source
    else
    ifeq ($(IFORT_MAJOR), 14)
          LIB_SYS := -lirc $(LIB_SYS)
          FPE += -fp-model source
    else
    ifeq ($(IFORT_MAJOR), 15)
          LIB_SYS := -lirc $(LIB_SYS)
          FPE += -fp-model source
    else
          LIB_SYS +=  # This should not be used; better to handle Major verison
    endif # Intel 15
    endif # Intel 14
    endif # Intel 13

#   MKL math library
#   ----------------
    ifeq ($(wildcard $(ESMABIN)/mklpath.pl),$(ESMABIN)/mklpath.pl)
       MKLPATH = $(shell $(ESMABIN)/mklpath.pl)
    endif
    ifdef MKLPATH
       ifeq ($(wildcard $(MKLPATH)/libmkl_intel_lp64.so),)
           LIB_SCI += -lmkl_intel_lp64 -lmkl_sequential -lmkl_core
       else
           LIB_SCI += -L$(MKLPATH) -lmkl_intel_lp64 -lmkl_sequential -lmkl_core
       endif
    endif

#   Customize for each MACH
#   -----------------------
    GCC_DIR = $(shell dirname `gcc --print-libgcc-file-name`)
    ifeq ($(MACH), x86_64) 
       OVERRIDE_LIMITS =
       LOOP_VECT =
       FDEFS += $(D)HAVE_SHMEM
    endif # x86_64

    LIB_SYS += -L$(GCC_DIR) -lstdc++
    #LIB_SYS += -L$(GCC_DIR) -lc++

    CFLAGS += $(FPIC)
    fFLAGS += $(FPIC) $(EXTENDED_SOURCE) $(FPE) $(OVERRIDE_LIMITS) $(ALIGNCOM)
    FFLAGS += $(FPIC) $(EXTENDED_SOURCE) $(FPE) $(OVERRIDE_LIMITS) $(ALIGNCOM)
    f90FLAGS += $(FPIC) $(FPE) $(OVERRIDE_LIMITS) $(ALIGNCOM)
    F90FLAGS += $(FPIC) $(FPE) $(OVERRIDE_LIMITS) $(ALIGNCOM)

#   Some safeguards
#   ---------------
    ifeq ($(INC_MPI),)
      FC := mpif90
      INC_MPI := $(dir $(shell which mpif90))../include
      LIB_MPI := -L$(dir $(shell which mpif90))../lib -lmpi -lmpi_cxx # -lmpi_f77
    endif

  endif


# GNU Fortran Compiler
# --------------------
  ifeq ($(FC), gfortran) 

      LIB_ESMF = $(BASELIB)/libesmf.a

      EXTENDED_SOURCE := -ffixed-line-length-132
      FREE_SOURCE = 
      FIXED_SOURCE = -ffixed-form
      FREAL4   := 
      FREAL8   := -fdefault-real-8

      fFLAGS   += $(D)__GFORTRAN__ $(EXTENDED_SOURCE)
      FFLAGS   += $(D)__GFORTRAN__ $(EXTENDED_SOURCE)
      f90FLAGS += $(D)__GFORTRAN__ -ffree-line-length-256
      F90FLAGS += $(D)__GFORTRAN__ -ffree-line-length-256

  endif


# G95 Fortran Compiler
# --------------------
  ifeq ($(FC), g95) 

      LIB_ESMF = $(BASELIB)/libesmf.a

      EXTENDED_SOURCE := -ffixed-line-length-132
      FREE_SOURCE = 
      FIXED_SOURCE = -ffixed-form
      FREAL4   := 
      FREAL8   := -r8 -i4

      fFLAGS   +=  $(EXTENDED_SOURCE)
      FFLAGS   +=  $(EXTENDED_SOURCE)
      f90FLAGS +=  -ffree-line-length-huge
      F90FLAGS +=  -ffree-line-length-huge

  endif # g95

endif  #    Darwin

# if defined, use compilers from ESMFMKFILE
ifdef ESMFMKFILE
  FC = $(ESMF_F90COMPILER)
  CXX = $(ESMF_CXXCOMPILER)
endif
