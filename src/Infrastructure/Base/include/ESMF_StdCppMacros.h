#if 0
! $Id: ESMF_StdCppMacros.h,v 1.2 2004/03/16 18:03:12 nscollins Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2003, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the GPL.
!
!==============================================================================
!
#endif
#if 0
!------------------------------------------------------------------------------
! Macros for any functions which must be overloaded T/K/R (type/kind/rank).
! Internal macros used only by ESMF source code.  NOT intended for Public Use.
! Defined from ranks 1 to 7, the maximum supported by fortran.
!
! These are defined outside the source file because they contain a 
! variable number of commas, and so the contents could not be given 
! literally as a single macro argument.  However, these symbols can 
! be used as macro arguments, and after initial substitution they 
! are rescanned by the preprocessor and expanded safely.
!
! col are colons per dim, used for declarations: dimension(:,:)
! len are sizes per dim, used for passing into other functions
! rng are lower and upper bounds of indices: allocate(ptr(lb:ub))
! lb is used alone to reference the first element of an array: ptr(lb)
!------------------------------------------------------------------------------
#endif

#define COL1 :
#define COL2 :,:
#define COL3 :,:,:
#define COL4 :,:,:,:
#define COL5 :,:,:,:,:
#define COL6 :,:,:,:,:,:
#define COL7 :,:,:,:,:,:,:

#define LEN1 counts(1)
#define LEN2 counts(1),counts(2)
#define LEN3 counts(1),counts(2),counts(3)
#define LEN4 counts(1),counts(2),counts(3),counts(4)
#define LEN5 counts(1),counts(2),counts(3),counts(4),counts(5)
#define LEN6 counts(1),counts(2),counts(3),counts(4),counts(5),counts(6)
#define LEN7 counts(1),counts(2),counts(3),counts(4),counts(5),counts(6),counts(7)

#define RNG1 lb(1):ub(1)
#define RNG2 lb(1):ub(1),lb(2):ub(2)
#define RNG3 lb(1):ub(1),lb(2):ub(2),lb(3):ub(3)
#define RNG4 lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4)
#define RNG5 lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5)
#define RNG6 lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6)
#define RNG7 lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6),lb(7):ub(7)

#define LOC1 lb(1)
#define LOC2 lb(1),lb(1)
#define LOC3 lb(1),lb(1),lb(1)
#define LOC4 lb(1),lb(1),lb(1),lb(1)
#define LOC5 lb(1),lb(1),lb(1),lb(1),lb(1)
#define LOC6 lb(1),lb(1),lb(1),lb(1),lb(1),lb(1)
#define LOC7 lb(1),lb(1),lb(1),lb(1),lb(1),lb(1),lb(1)


#if 0
!------------------------------------------------------------------------------
! Expand a string into each of the T/K/R procedure interface blocks
!------------------------------------------------------------------------------
#endif

#define InterfaceMacro(funcname) \
!------------------------------------------------------------------------------ @\
! <This section created by macro - do not edit directly> @\
    module procedure ESMF_##funcname##1DI1 @\
    module procedure ESMF_##funcname##1DI2 @\
    module procedure ESMF_##funcname##1DI4 @\
    module procedure ESMF_##funcname##1DI8 @\
    module procedure ESMF_##funcname##2DI1 @\
    module procedure ESMF_##funcname##2DI2 @\
    module procedure ESMF_##funcname##2DI4 @\
    module procedure ESMF_##funcname##2DI8 @\
    module procedure ESMF_##funcname##3DI1 @\
    module procedure ESMF_##funcname##3DI2 @\
    module procedure ESMF_##funcname##3DI4 @\
    module procedure ESMF_##funcname##3DI8 @\
    module procedure ESMF_##funcname##4DI1 @\
    module procedure ESMF_##funcname##4DI2 @\
    module procedure ESMF_##funcname##4DI4 @\
    module procedure ESMF_##funcname##4DI8 @\
    module procedure ESMF_##funcname##5DI1 @\
    module procedure ESMF_##funcname##5DI2 @\
    module procedure ESMF_##funcname##5DI4 @\
    module procedure ESMF_##funcname##5DI8 @\
    module procedure ESMF_##funcname##6DI1 @\
    module procedure ESMF_##funcname##6DI2 @\
    module procedure ESMF_##funcname##6DI4 @\
    module procedure ESMF_##funcname##6DI8 @\
    module procedure ESMF_##funcname##7DI1 @\
    module procedure ESMF_##funcname##7DI2 @\
    module procedure ESMF_##funcname##7DI4 @\
    module procedure ESMF_##funcname##7DI8 @\
    module procedure ESMF_##funcname##1DR4 @\
    module procedure ESMF_##funcname##1DR8 @\
    module procedure ESMF_##funcname##2DR4 @\
    module procedure ESMF_##funcname##2DR8 @\
    module procedure ESMF_##funcname##3DR4 @\
    module procedure ESMF_##funcname##3DR8 @\
    module procedure ESMF_##funcname##4DR4 @\
    module procedure ESMF_##funcname##4DR8 @\
    module procedure ESMF_##funcname##5DR4 @\
    module procedure ESMF_##funcname##5DR8 @\
    module procedure ESMF_##funcname##6DR4 @\
    module procedure ESMF_##funcname##6DR8 @\
    module procedure ESMF_##funcname##7DR4 @\
    module procedure ESMF_##funcname##7DR8 @\
! < end macro - do not edit directly >  @\
!------------------------------------------------------------------------------ @\

#if 0
!------------------------------------------------------------------------------
! Expand a string into each of the T/K/R procedures.  Assumes one macro
! which contains only the general protex documentation, and the rest do NOT
! contain protex.
!------------------------------------------------------------------------------
#endif

#define DeclarationMacro(funcname) \
!------------------------------------------------------------------------------ @\
! <This section created by macro - do not edit directly> @\
 @\
!! < start of macros which become actual subroutine bodies after expansion > @\
 @\
funcname##Doc() @\
funcname##Macro(integer, I1, 1, COL1, LEN1, RNG1, LOC1) @\
funcname##Macro(integer, I2, 1, COL1, LEN1, RNG1, LOC1) @\
funcname##Macro(integer, I4, 1, COL1, LEN1, RNG1, LOC1) @\
funcname##Macro(integer, I8, 1, COL1, LEN1, RNG1, LOC1) @\
funcname##Macro(integer, I1, 2, COL2, LEN2, RNG2, LOC2) @\
funcname##Macro(integer, I2, 2, COL2, LEN2, RNG2, LOC2) @\
funcname##Macro(integer, I4, 2, COL2, LEN2, RNG2, LOC2) @\
funcname##Macro(integer, I8, 2, COL2, LEN2, RNG2, LOC2) @\
funcname##Macro(integer, I1, 3, COL3, LEN3, RNG3, LOC3) @\
funcname##Macro(integer, I2, 3, COL3, LEN3, RNG3, LOC3) @\
funcname##Macro(integer, I4, 3, COL3, LEN3, RNG3, LOC3) @\
funcname##Macro(integer, I8, 3, COL3, LEN3, RNG3, LOC3) @\
funcname##Macro(integer, I1, 4, COL4, LEN4, RNG4, LOC4) @\
funcname##Macro(integer, I2, 4, COL4, LEN4, RNG4, LOC4) @\
funcname##Macro(integer, I4, 4, COL4, LEN4, RNG4, LOC4) @\
funcname##Macro(integer, I8, 4, COL4, LEN4, RNG4, LOC4) @\
funcname##Macro(integer, I1, 5, COL5, LEN5, RNG5, LOC5) @\
funcname##Macro(integer, I2, 5, COL5, LEN5, RNG5, LOC5) @\
funcname##Macro(integer, I4, 5, COL5, LEN5, RNG5, LOC5) @\
funcname##Macro(integer, I8, 5, COL5, LEN5, RNG5, LOC5) @\
funcname##Macro(integer, I1, 6, COL6, LEN6, RNG6, LOC6) @\
funcname##Macro(integer, I2, 6, COL6, LEN6, RNG6, LOC6) @\
funcname##Macro(integer, I4, 6, COL6, LEN6, RNG6, LOC6) @\
funcname##Macro(integer, I8, 6, COL6, LEN6, RNG6, LOC6) @\
funcname##Macro(integer, I1, 7, COL7, LEN7, RNG7, LOC7) @\
funcname##Macro(integer, I2, 7, COL7, LEN7, RNG7, LOC7) @\
funcname##Macro(integer, I4, 7, COL7, LEN7, RNG7, LOC7) @\
funcname##Macro(integer, I8, 7, COL7, LEN7, RNG7, LOC7) @\
funcname##Macro(real, R4, 1, COL1, LEN1, RNG1, LOC1) @\
funcname##Macro(real, R8, 1, COL1, LEN1, RNG1, LOC1) @\
funcname##Macro(real, R4, 2, COL2, LEN2, RNG2, LOC2) @\
funcname##Macro(real, R8, 2, COL2, LEN2, RNG2, LOC2) @\
funcname##Macro(real, R4, 3, COL3, LEN3, RNG3, LOC3) @\
funcname##Macro(real, R8, 3, COL3, LEN3, RNG3, LOC3) @\
funcname##Macro(real, R4, 4, COL4, LEN4, RNG4, LOC4) @\
funcname##Macro(real, R8, 4, COL4, LEN4, RNG4, LOC4) @\
funcname##Macro(real, R4, 5, COL5, LEN5, RNG5, LOC5) @\
funcname##Macro(real, R8, 5, COL5, LEN5, RNG5, LOC5) @\
funcname##Macro(real, R4, 6, COL6, LEN6, RNG6, LOC6) @\
funcname##Macro(real, R8, 6, COL6, LEN6, RNG6, LOC6) @\
funcname##Macro(real, R4, 7, COL7, LEN7, RNG7, LOC7) @\
funcname##Macro(real, R8, 7, COL7, LEN7, RNG7, LOC7) @\
 @\
! < end macro - do not edit directly >  @\
!------------------------------------------------------------------------------ @\


#if 0
!------------------------------------------------------------------------------
! Expand a string into a function call for each of the T/K/R procedures.
!------------------------------------------------------------------------------
#endif

#define AllTypesMacro(fname) \
 @\
      fname##Macro(integer, I1, 1, COL1) @\
      fname##Macro(integer, I2, 1, COL1) @\
      fname##Macro(integer, I4, 1, COL1) @\
      fname##Macro(integer, I8, 1, COL1) @\
 @\
      fname##Macro(integer, I1, 2, COL2) @\
      fname##Macro(integer, I2, 2, COL2) @\
      fname##Macro(integer, I4, 2, COL2) @\
      fname##Macro(integer, I8, 2, COL2) @\
 @\
      fname##Macro(integer, I1, 3, COL3) @\
      fname##Macro(integer, I2, 3, COL3) @\
      fname##Macro(integer, I4, 3, COL3) @\
      fname##Macro(integer, I8, 3, COL3) @\
 @\
      fname##Macro(integer, I1, 4, COL4) @\
      fname##Macro(integer, I2, 4, COL4) @\
      fname##Macro(integer, I4, 4, COL4) @\
      fname##Macro(integer, I8, 4, COL4) @\
 @\
      fname##Macro(integer, I1, 5, COL5) @\
      fname##Macro(integer, I2, 5, COL5) @\
      fname##Macro(integer, I4, 5, COL5) @\
      fname##Macro(integer, I8, 5, COL5) @\
 @\
      fname##Macro(integer, I1, 6, COL6) @\
      fname##Macro(integer, I2, 6, COL6) @\
      fname##Macro(integer, I4, 6, COL6) @\
      fname##Macro(integer, I8, 6, COL6) @\
 @\
      fname##Macro(integer, I1, 7, COL7) @\
      fname##Macro(integer, I2, 7, COL7) @\
      fname##Macro(integer, I4, 7, COL7) @\
      fname##Macro(integer, I8, 7, COL7) @\
 @\
      fname##Macro(real, R4, 1, COL1) @\
      fname##Macro(real, R8, 1, COL1) @\
 @\
      fname##Macro(real, R4, 2, COL2) @\
      fname##Macro(real, R8, 2, COL2) @\
 @\
      fname##Macro(real, R4, 3, COL3) @\
      fname##Macro(real, R8, 3, COL3) @\
 @\
      fname##Macro(real, R4, 4, COL4) @\
      fname##Macro(real, R8, 4, COL4) @\
 @\
      fname##Macro(real, R4, 5, COL5) @\
      fname##Macro(real, R8, 5, COL5) @\
 @\
      fname##Macro(real, R4, 6, COL6) @\
      fname##Macro(real, R8, 6, COL6) @\
 @\
      fname##Macro(real, R4, 7, COL7) @\
      fname##Macro(real, R8, 7, COL7) @\
! < end macro - do not edit directly >  @\
 @\

