#if 0
! $Id: ESMF_StateMacros.h,v 1.1 2004/02/05 21:53:55 nscollins Exp $
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
! Macros for the State class.
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

#define LOC1 1
#define LOC2 1,1
#define LOC3 1,1,1
#define LOC4 1,1,1,1
#define LOC5 1,1,1,1,1
#define LOC6 1,1,1,1,1,1
#define LOC7 1,1,1,1,1,1,1


#if 0
!------------------------------------------------------------------------------
! Expand a string into each of the T/K/R procedure interface blocks
!------------------------------------------------------------------------------
#endif

#define StateInterfaceMacro(funcname) \
!------------------------------------------------------------------------------ @\
! <This section created by macro - do not edit directly> @\
    module procedure ESMF_##funcname##I21D @\
    module procedure ESMF_##funcname##I41D @\
    module procedure ESMF_##funcname##I81D @\
    module procedure ESMF_##funcname##I22D @\
    module procedure ESMF_##funcname##I42D @\
    module procedure ESMF_##funcname##I82D @\
    module procedure ESMF_##funcname##I23D @\
    module procedure ESMF_##funcname##I43D @\
    module procedure ESMF_##funcname##I83D @\
    module procedure ESMF_##funcname##I24D @\
    module procedure ESMF_##funcname##I44D @\
    module procedure ESMF_##funcname##I84D @\
    module procedure ESMF_##funcname##I25D @\
    module procedure ESMF_##funcname##I45D @\
    module procedure ESMF_##funcname##I85D @\
    module procedure ESMF_##funcname##R41D @\
    module procedure ESMF_##funcname##R81D @\
    module procedure ESMF_##funcname##R42D @\
    module procedure ESMF_##funcname##R82D @\
    module procedure ESMF_##funcname##R43D @\
    module procedure ESMF_##funcname##R83D @\
    module procedure ESMF_##funcname##R44D @\
    module procedure ESMF_##funcname##R84D @\
    module procedure ESMF_##funcname##R45D @\
    module procedure ESMF_##funcname##R85D @\
! < end macro - do not edit directly >  @\
!------------------------------------------------------------------------------ @\

#if 0
!------------------------------------------------------------------------------
! Create a new array based on an unallocated F90 array and a list of counts.
!------------------------------------------------------------------------------
#endif

#define StateGetDataPointerMacro(mname, mtypekind, mrank, mdim, mlen, mrng, mloc) \
!------------------------------------------------------------------------------ @\
! <Created by macro - do not edit directly > @\
!BOP @\
! !IROUTINE: ESMF_StateGetDataPointer##mtypekind##mrank##D - Retrieve F90 pointer directly from a State @\
 @\
! !INTERFACE: @\
      subroutine ESMF_StateGetDataPointer##mtypekind##mrank##D(state, dataname, f90ptr, copyflag, statename, rc) @\
! @\
! !ARGUMENTS: @\
      type(ESMF_State), intent(in) :: state @\
      character(len=*), intent(in) :: dataname @\
      mname (ESMF_KIND_##mtypekind), dimension(mdim), pointer :: f90ptr @\
      type(ESMF_CopyFlag), intent(in), optional :: copyflag @\
      character(len=*), intent(in), optional :: statename @\
      integer, intent(out), optional :: rc   @\
! @\
! !DESCRIPTION: @\
! Retrieves data from a state, returning a direct F90 pointer to the start @\
!  of the actual data array.  @\
! @\
! The arguments are: @\
!  \begin{description} @\
!  \item[state] @\
!   The {\tt ESMF\_State} to query. @\
! @\
!  \item[dataname] @\
!   The name of the Bundle, Field, or Array to return. @\
! @\
!  \item[f90ptr] @\
!   An unassociated Fortrn 90 pointer of the proper Type, Kind, and Rank as the data @\
!   in the State.  When this call returns successfully, the pointer will now reference @\
!   the data in the State.  This is either a reference or a copy, depending on the @\
!   setting of the following argument.  The default is to return a reference. @\
! @\
!  \item[{[copyflag]}] @\
!   Defaults to {\tt ESMF\_DATA\_REF}.  If set to {\tt ESMF\_DATA\_COPY}, a separate @\
!   copy of the data will be made and the pointer will point at the copy. @\
! @\
!  \item[{[statename]}] @\
!   Optional.  If multiple states are present, a specific state name must be given. @\
! @\
!  \item[{[rc]}] @\
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. @\
!  \end{description} @\
! @\
 @\
! @\
!EOP @\
! !REQUIREMENTS: @\
 @\
        ! Local variables @\
        type (ESMF_Bundle) :: bundle        ! bundle object @\
        type (ESMF_Field) :: field          ! field object @\
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
        ! TODO: make this check the data type, and switch based on that. @\
        ! For now, assume field only. @\
        call ESMF_StateGetField(state, dataname, field, statename, status) @\
        if (status .ne. ESMF_SUCCESS) then @\
          print *, "Error: StateGetField failed" @\
          return @\
        endif @\
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
        end subroutine ESMF_StateGetDataPointer##mtypekind##mrank##D   @\
 @\
! < end macro - do not edit directly >  @\
!------------------------------------------------------------------------------ @\

