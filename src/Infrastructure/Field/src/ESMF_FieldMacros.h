#if 0
! $Id: ESMF_FieldMacros.h,v 1.4 2004/03/15 21:52:54 nscollins Exp $
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
! Macros for the Field class.
! these are defined because they contain a variable number of commas.
! they are used as parms in macro calls, and are rescanned by the
! preprocessor after expansion.   7 is the maximum rank supported by Fortran.
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

#define FieldInterfaceMacro(funcname) \
!------------------------------------------------------------------------------ @\
! <This section created by macro - do not edit directly> @\
    module procedure ESMF_##funcname##1DI2 @\
    module procedure ESMF_##funcname##1DI4 @\
    module procedure ESMF_##funcname##1DI8 @\
    module procedure ESMF_##funcname##2DI2 @\
    module procedure ESMF_##funcname##2DI4 @\
    module procedure ESMF_##funcname##2DI8 @\
    module procedure ESMF_##funcname##3DI2 @\
    module procedure ESMF_##funcname##3DI4 @\
    module procedure ESMF_##funcname##3DI8 @\
    module procedure ESMF_##funcname##4DI2 @\
    module procedure ESMF_##funcname##4DI4 @\
    module procedure ESMF_##funcname##4DI8 @\
    module procedure ESMF_##funcname##5DI2 @\
    module procedure ESMF_##funcname##5DI4 @\
    module procedure ESMF_##funcname##5DI8 @\
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
! < end macro - do not edit directly >  @\
!------------------------------------------------------------------------------ @\

#if 0
!------------------------------------------------------------------------------
! Create a new array based on an unallocated F90 array and a list of counts.
!------------------------------------------------------------------------------
#endif

#define FieldGetDataPointerMacro(mname, mtypekind, mrank, mdim, mlen, mrng, mloc) \
!------------------------------------------------------------------------------ @\
! <Created by macro - do not edit directly > @\
!BOP @\
! !IROUTINE: ESMF_FieldGetDataPointer - Retrieve F90 pointer directly from a Field @\
 @\
! !INTERFACE: @\
      ! Private name; call using ESMF_FieldGetDataPointer() @\
      subroutine ESMF_FieldGetDataPointer##mrank##D##mtypekind(field, f90ptr, copyflag, rc) @\
! @\
! !ARGUMENTS: @\
      type(ESMF_Field), intent(in) :: field @\
      mname (ESMF_KIND_##mtypekind), dimension(mdim), pointer :: f90ptr @\
      type(ESMF_CopyFlag), intent(in), optional :: copyflag @\
      integer, intent(out), optional :: rc   @\
! @\
! !DESCRIPTION: @\
! Retrieves data from a field, returning a direct F90 pointer to the start @\
!  of the actual data array.  @\
! @\
! The arguments are: @\
!  \begin{description} @\
!  \item[field] @\
!   The {\tt ESMF\_Field} to query. @\
!  \item[f90ptr] @\
!   An unassociated Fortrn 90 pointer of the proper Type, Kind, and Rank as the data @\
!   in the Field.  When this call returns successfully, the pointer will now reference @\
!   the data in the Field.  This is either a reference or a copy, depending on the @\
!   setting of the following argument.  The default is to return a reference. @\
!  \item[{[copyflag]}] @\
!   Defaults to {\tt ESMF\_DATA\_REF}.  If set to {\tt ESMF\_DATA\_COPY}, a separate @\
!   copy of the data will be made and the pointer will point at the copy. @\
!  \item[{[rc]}] @\
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. @\
!  \end{description} @\
! @\
!EOP @\
 @\
        ! Local variables @\
        type (ESMF_Array) :: array          ! array object @\
        integer :: status                   ! local error status @\
        logical :: rcpresent                ! did user specify rc? @\
 @\
        mname (ESMF_KIND_##mtypekind), dimension(mdim), pointer :: newp  @\
 @\
        ! Initialize return code; assume failure until success is certain @\
        status = ESMF_FAILURE @\
        rcpresent = .FALSE. @\
        array%this = ESMF_NULL_POINTER @\
 @\
        if (present(rc)) then @\
          rcpresent = .TRUE. @\
          rc = ESMF_FAILURE @\
        endif @\
 @\
        ! Test to see if array already allocated, and fail if so. @\
        !if (allocated(f90arr)) then @\
        !  print *, "Error: Data Pointer cannot already be allocated" @\
        !  return @\
        !endif @\
 @\
        call ESMF_FieldGetData(field, array, rc=status) @\
        if (status .ne. ESMF_SUCCESS) then @\
          print *, "Error: FieldGetData failed" @\
          return @\
        endif @\
 @\
        call ESMF_ArrayGetData(array, f90ptr, copyflag, rc=status) @\
        if (status .ne. ESMF_SUCCESS) then @\
          print *, "Error: ArrayGetData failed" @\
          return @\
        endif @\
 @\
        if (rcpresent) rc = status @\
 @\
        end subroutine ESMF_FieldGetDataPointer##mrank##D##mtypekind  @\
 @\
! < end macro - do not edit directly >  @\
!------------------------------------------------------------------------------ @\

