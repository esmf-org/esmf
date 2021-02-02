! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2021, University Corporation for Atmospheric Research, 
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
! Laboratory, University of Michigan, National Centers for Environmental 
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
#define ESMF_FILENAME "ESMF_XGrid.F90"
!==============================================================================
!
!     ESMF XGrid module
module ESMF_XGridMod
!
!==============================================================================
!
! This file contains the XGrid class definition and base XGrid
! class methods.
!
!------------------------------------------------------------------------------
! INCLUDES
#include "ESMF.h"

!------------------------------------------------------------------------------
!
!BOPI
! !MODULE: ESMF_XGridMod - 2D representation of the boundary layer
!
! !DESCRIPTION:
! The code in this file implements the {\tt ESMF\_XGrid} class, which 
! represents a 2D boundary layer usually between the
! atmosphere on one side and ocean and land on the other in an Earth
! system model. There are dynamical and thermodynamical processes on
! either side of the boundary layer and on the boundary layer itself.
! The boundary layer exchanges fluxes from either side and adjusts
! boundary condition for model components involved. For climate modeling,
! it's critical that the fluxes transferred by the boundary layer are
! conservative.   
!
! The XGrid type is implemented in Fortran 90.
!
!------------------------------------------------------------------------------
! !USES:
  use ESMF_UtilTypesMod
  use ESMF_UtilMod
  use ESMF_BaseMod
  use ESMF_LogErrMod
  use ESMF_DistGridMod
  use ESMF_ArrayMod
  use ESMF_GridMod
  use ESMF_MeshMod
  use ESMF_XGridGeomBaseMod
  use ESMF_InitMacrosMod

  implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
  private

!------------------------------------------------------------------------------
! ! ESMF_XGridType
! ! Definition of the XGrid class.

  ! defines the side relative to an XGrid, SIDEA, SIDEB, or BALANCED
  type ESMF_XGridSide_Flag
#ifndef ESMF_NO_SEQUENCE
    sequence
#endif
    integer                     :: side
  end type ESMF_XGridSide_Flag

  type(ESMF_XGridSide_Flag), parameter :: &
    ESMF_XGRIDSIDE_A=ESMF_XGridSide_Flag(0), &
    ESMF_XGRIDSIDE_B=ESMF_XGridSide_Flag(1), &
    ESMF_XGRIDSIDE_BALANCED=ESMF_XGridSide_Flag(2)

  ! package the collapsed indices and weights matrices
  type ESMF_XGridSpec
#ifndef ESMF_NO_SEQUENCE
    sequence
#endif
    integer, pointer            :: factorIndexList(:,:) => null()     ! factorIndexList
    real(ESMF_KIND_R8), pointer :: factorList(:) => null()  ! factorList
  end type ESMF_XGridSpec

  ! the XGridType definition
  type ESMF_XGridType
#ifndef ESMF_NO_SEQUENCE
    sequence
#endif
    type(ESMF_Base)                        :: base                      ! base class object
    type(ESMF_DistGrid)                    :: distgridM                 ! load balanced distgrid in the middle
    type(ESMF_DistGrid), pointer           :: distgridA(:) => null()    ! A side distgrid
    type(ESMF_DistGrid), pointer           :: distgridB(:) => null()    ! B side distgrid
    type(ESMF_XGridGeomBase), pointer      :: sideA(:) => null()        ! geometric types
    type(ESMF_XGridGeomBase), pointer      :: sideB(:) => null()        ! geometric types
    type(ESMF_Mesh)                        :: mesh                      ! overlay mesh, not always stored
    real(ESMF_KIND_R8), pointer            :: area(:)         => null() ! area of xgrid
    real(ESMF_KIND_R8), pointer            :: centroid(:,:)   => null() ! centroids of xgrid
    type(ESMF_XGridSpec), pointer          :: sparseMatA2X(:) => null() ! descriptors of mapping sparsemat
    type(ESMF_XGridSpec), pointer          :: sparseMatX2A(:) => null() 
    type(ESMF_XGridSpec), pointer          :: sparseMatB2X(:) => null()
    type(ESMF_XGridSpec), pointer          :: sparseMatX2B(:) => null()
    type(ESMF_Array), pointer              :: fracA2X(:) => null()      ! side A regridding fraction 
    type(ESMF_Array), pointer              :: fracX2A(:) => null()      ! side A regridding fraction 
    type(ESMF_Array), pointer              :: fracB2X(:) => null()      ! side B regridding fraction 
    type(ESMF_Array), pointer              :: fracX2B(:) => null()      ! side B regridding fraction 
    type(ESMF_Array)                       :: fracX                     ! middle mesh fraction always 1.0
    type(ESMF_Array), pointer              :: frac2A(:)  => null()      ! side A merge fraction
    type(ESMF_Array), pointer              :: frac2B(:)  => null()      ! side B merge fraction
    logical                                :: is_proxy         ! .true. for a proxy xgrid
    logical                                :: storeOverlay     ! .false. do not save mesh in the middle
    integer                                :: online           ! 1 if Xgrid is computed based on user input, 0 offline
    type (ESMF_Status)                     :: status
    ESMF_INIT_DECLARE
  end type

!------------------------------------------------------------------------------
! ! ESMF_XGrid    
! ! The XGrid data structure that is passed between implementation and
! ! calling languages.

  type ESMF_XGrid
#ifndef ESMF_NO_SEQUENCE
    sequence
#endif
    type (ESMF_XGridType), pointer :: xgtypep
    ESMF_INIT_DECLARE
  end type

!------------------------------------------------------------------------------
! !PUBLIC TYPES:
  public ESMF_XGrid
  public ESMF_XGridType
  public ESMF_XGridSpec
  public ESMF_XGridSide_Flag, ESMF_XGRIDSIDE_A, ESMF_XGRIDSIDE_B, &
    ESMF_XGRIDSIDE_BALANCED

!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:
!
! - ESMF-public methods:
   public ESMF_XGridValidate           ! Check internal consistency
   public ESMF_XGridMatch              ! Check if two XGrids match
   public ESMF_XGridWriteVTK           ! Write midmesh as vtk file for examination

   public assignment(=)
   public operator(==)
   public operator(/=) 

! - ESMF-internal methods:
   public ESMF_XGridGetInit            ! For Standardized Initialization
   public ESMF_XGridSerialize
   public ESMF_XGridDeserialize
   public ESMF_XGridInitialize         ! Default initiailze XGrid member variables

!
!
!EOPI
   
!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
  character(*), parameter, private :: version = &
    '$Id$'

!==============================================================================
!
! INTERFACE BLOCKS
!
!==============================================================================
!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: assignment (=) - set one xgrid equal to another
!
! !INTERFACE:
      interface assignment (=)
   
! !PRIVATE MEMBER FUNCTIONS:
      module procedure ESMF_XGridSpecAssign
      module procedure ESMF_XGridSideAssign


! !DESCRIPTION:
!    Set one xgrid equal to another note that since its 
!    a pointer copy the xgrids are actually the same
 
!EOPI
      end interface
!
!
!==============================================================================
!BOPI
! !INTERFACE:
      interface operator (==)

! !PRIVATE MEMBER FUNCTIONS:
         module procedure ESMF_XGridSideEqual

! !DESCRIPTION:
!     This interface overloads the equality operator for the specific
!     ESMF GridConn.  It is provided for easy comparisons of 
!     these types with defined values.
!
!EOPI
      end interface
!
!------------------------------------------------------------------------------
!BOPI
! !INTERFACE:
      interface operator (/=)

! !PRIVATE MEMBER FUNCTIONS:
         module procedure ESMF_XGridSideNotEqual

! !DESCRIPTION:
!     This interface overloads the inequality operator for the specific
!     ESMF GridConn.  It is provided for easy comparisons of 
!     these types with defined values.
!
!EOPI
      end interface


!===============================================================================
! XGridOperator() interfaces
!===============================================================================


! -------------------------- ESMF-public method -------------------------------
  interface operator(==)
    module procedure ESMF_XGridEQ
  end interface
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
  interface operator(/=)
    module procedure ESMF_XGridNE
  end interface
!------------------------------------------------------------------------------


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! MACROS for meta array index
#define XG_S_DGA        1
#define XG_S_DGB        2
#define XG_S_GA         3
#define XG_S_GB         4

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


!-------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_XGridEQ()"
!BOPI
! !IROUTINE:  ESMF_XGridEQ - Compare two XGrids for equality
!
! !INTERFACE:
  function ESMF_XGridEQ(xgrid1, xgrid2)
! 
! !RETURN VALUE:
    logical :: ESMF_XGridEQ

! !ARGUMENTS:
    type(ESMF_XGrid), intent(in) :: xgrid1
    type(ESMF_XGrid), intent(in) :: xgrid2

! !DESCRIPTION:
!   Test if both {\tt xgrid1} and {\tt xgrid2} alias the same ESMF XGrid 
!   object.
!
!EOPI
!-------------------------------------------------------------------------------

    ESMF_INIT_TYPE xginit1, xginit2
    integer :: localrc1, localrc2
    logical :: lval1, lval2

    ! Use the following logic, rather than "ESMF-INIT-CHECK-DEEP", to gain 
    ! init checks on both args, and in the case where both are uninitialized,
    ! to distinguish equality based on uninitialized type (uncreated,
    ! deleted).

    ! TODO: Consider moving this logic to C++: use Base class? status?
    !       Or replicate logic for C interface also.

    ! check inputs
    xginit1 = ESMF_XGridGetInit(xgrid1)
    xginit2 = ESMF_XGridGetInit(xgrid2)

    ! TODO: this line must remain split in two for SunOS f90 8.3 127000-03
    if (xginit1 .eq. ESMF_INIT_CREATED .and. &
      xginit2 .eq. ESMF_INIT_CREATED) then
      ESMF_XGridEQ = associated(xgrid1%xgtypep,xgrid2%xgtypep)
    else
      ESMF_XGridEQ = ESMF_FALSE
    endif

  end function ESMF_XGridEQ
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_XGridNE()"
!BOPI
! !IROUTINE:  ESMF_XGridNE - Compare two XGrids for non-equality
!
! !INTERFACE:
  function ESMF_XGridNE(xgrid1, xgrid2)
! 
! !RETURN VALUE:
    logical :: ESMF_XGridNE

! !ARGUMENTS:
    type(ESMF_XGrid), intent(in) :: xgrid1
    type(ESMF_XGrid), intent(in) :: xgrid2

! !DESCRIPTION:
!   Test if both {\tt xgrid1} and {\tt xgrid2} alias the same ESMF XGrid 
!   object.
!
!EOPI
!-------------------------------------------------------------------------------

    ESMF_INIT_TYPE xginit1, xginit2
    integer :: localrc1, localrc2
    logical :: lval1, lval2

    ! Use the following logic, rather than "ESMF-INIT-CHECK-DEEP", to gain 
    ! init checks on both args, and in the case where both are uninitialized,
    ! to distinguish equality based on uninitialized type (uncreated,
    ! deleted).
    
    ESMF_XGridNE = .not.ESMF_XGridEQ(xgrid1, xgrid2)

  end function ESMF_XGridNE
!-------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_XGridSpecAssign()"
!BOPI
! !IROUTINE:  ESMF_XGridSpecAssign - set one xgrid spec struct equal to another

! !INTERFACE:

   subroutine ESMF_XGridSpecAssign(dval, sval)
!
! !ARGUMENTS:
 type(ESMF_XGridSpec), intent(out) :: dval
 type(ESMF_XGridSpec), intent(in) :: sval
!
! !DESCRIPTION:
!      Set one xgrid spec structure equal to another
!
!     The arguments are:
!     \begin{description}
!     \item [dval]
!           destination structure
!     \item [dval]
!           source structure
!     \end{description}
!
!EOPI

 ! deep copy of the lists
 allocate(dval%factorIndexList(size(sval%factorIndexList,1), &
    size(sval%factorIndexList,2)))
 allocate(dval%factorList(size(sval%factorList,1)))

 dval%factorIndexList = sval%factorIndexList
 dval%factorList = sval%factorList

 end subroutine

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_XGridSideAssign()"
!BOPI
! !IROUTINE:  ESMF_XGridSideAssign - set one xgrid side struct equal to another

! !INTERFACE:

   subroutine ESMF_XGridSideAssign(dval, sval)
!
! !ARGUMENTS:
 type(ESMF_XGridSide_Flag), intent(out) :: dval
 type(ESMF_XGridSide_Flag), intent(in) :: sval
!
! !DESCRIPTION:
!      Set one xgrid side structure equal to another
!
!     The arguments are:
!     \begin{description}
!     \item [dval]
!           destination structure
!     \item [dval]
!           source structure
!     \end{description}
!
!EOPI

 ! deep copy of the lists

 dval%side = sval%side

 end subroutine

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_XGridMatch"

!BOPI
! !IROUTINE:  ESMF_XGridMatch - Check if two XGrids match

! !INTERFACE:
      function ESMF_XGridMatch(xgrid1, xgrid2, rc)
!
! !RETURN VALUE:
      logical :: ESMF_XGridMatch
!
! !ARGUMENTS:
      type(ESMF_XGrid), intent(in)   :: xgrid1, xgrid2 
      integer, intent(out), optional :: rc   
!
! !DESCRIPTION:
!      Compare two {\tt XGrid}s and check if they match each other. The 
!      comparison is incremental. First the internal pointer association
!      is checked to see if they are the same object. A deep check of 
!      individual XGrid members is carried out subsequently.
!
!      The method returns an error code if problems are found.  
!
!     The arguments are:
!     \begin{description}
!     \item [xgrid1]
!           First {\tt ESMF\_XGrid} to match.
!     \item [xgrid2]
!           Second {\tt ESMF\_XGrid} to match.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if the {\tt xgrid} 
!           is valid.
!     \end{description}
!
!EOPI

      integer :: localrc, i

      type(ESMF_XGridType), pointer :: fp1, fp2
      type(ESMF_Status) :: xgridstatus
      integer :: ngridA1, ngridB1
      integer :: ngridA2, ngridB2

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      ESMF_XGridMatch = .false.

      ! Check variables
      ESMF_INIT_CHECK_DEEP(ESMF_XGridGetInit,xgrid1,rc)
      ESMF_INIT_CHECK_DEEP(ESMF_XGridGetInit,xgrid2,rc)

      ! Identical pointer
      if(associated(xgrid1%xgtypep, xgrid2%xgtypep)) then
        ESMF_XGridMatch = .true.
        if(present(rc)) rc = ESMF_SUCCESS
        return
      endif

      ! Compare Grids contained
      fp1 = xgrid1%xgtypep
      fp2 = xgrid2%xgtypep
      ! Side A Grids
      if(associated(fp1%sideA)) ngridA1 = size(fp1%sideA, 1)
      if(associated(fp2%sideA)) ngridA2 = size(fp2%sideA, 1)
      if(ngridA1 /= ngridA2) then
        if(present(rc)) rc = ESMF_SUCCESS
        return
      endif
      if(.not. associated(fp1%sideA, fp2%sideA)) then
        do i = 1, ngridA1
          if(.not. ESMF_XGridGeomBaseMatch(fp1%sideA(i), fp2%sideA(i))) then
            if(present(rc)) rc = ESMF_SUCCESS
            return
          endif
        enddo
      endif
      ! Side B Grids
      if(associated(fp1%sideB)) ngridB1 = size(fp1%sideB, 1)
      if(associated(fp2%sideB)) ngridB2 = size(fp2%sideB, 1)
      if(ngridB1 /= ngridB2) then
        if(present(rc)) rc = ESMF_SUCCESS
        return
      endif
      if(.not. associated(fp1%sideB, fp2%sideB)) then
        do i = 1, ngridB1
          if(.not. ESMF_XGridGeomBaseMatch(fp1%sideB(i), fp2%sideB(i))) then
            if(present(rc)) rc = ESMF_SUCCESS
            return
          endif
        enddo
      endif

      ! Balanced DistGrid
      if(ESMF_DistGridMatch(fp1%distgridM, fp2%distgridM) &
        == ESMF_DISTGRIDMATCH_NONE) then
        if(present(rc)) rc = ESMF_SUCCESS
        return
      endif

      ! Side A DistGrids
      if(associated(fp1%distgridA)) ngridA1 = size(fp1%distgridA, 1)
      if(associated(fp2%distgridA)) ngridA2 = size(fp2%distgridA, 1)
      if(ngridA1 /= ngridA2) then
        if(present(rc)) rc = ESMF_SUCCESS
        return
      endif
      if(.not. associated(fp1%distgridA, fp2%distgridA)) then
        do i = 1, ngridA1
          if(ESMF_DistGridMatch(fp1%distgridA(i), fp2%distgridA(i)) &
            == ESMF_DISTGRIDMATCH_NONE) then
            if(present(rc)) rc = ESMF_SUCCESS
            return
          endif
        enddo
      endif
      ! Side B DistGrids
      if(associated(fp1%distgridB)) ngridB1 = size(fp1%distgridB, 1)
      if(associated(fp2%distgridB)) ngridB2 = size(fp2%distgridB, 1)
      if(ngridB1 /= ngridB2) then
        if(present(rc)) rc = ESMF_SUCCESS
        return
      endif
      if(.not. associated(fp1%distgridB, fp2%distgridB)) then
        do i = 1, ngridB1
          if(ESMF_DistGridMatch(fp1%distgridB(i), fp2%distgridB(i)) &
            == ESMF_DISTGRIDMATCH_NONE) then
            if(present(rc)) rc = ESMF_SUCCESS
            return
          endif
        enddo
      endif

      ! TODO: Compare the SparseMat objects

      ! All critical internal objects match
      ESMF_XGridMatch = .true.

      if (present(rc)) rc = ESMF_SUCCESS

      end function ESMF_XGridMatch

!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_XGridValidate"

!BOPI
! !IROUTINE:  ESMF_XGridValidate - Check validity of a XGrid

! !INTERFACE:
      subroutine ESMF_XGridValidate(xgrid, rc)
!
! !ARGUMENTS:
      type(ESMF_XGrid), intent(inout) :: xgrid 
      integer, intent(out), optional :: rc   
!
! !DESCRIPTION:
!      Validates that the {\tt xgrid} is internally consistent.
!      Currently this method determines if the {\tt xgrid} is uninitialized 
!      or already destroyed. 
!
!      The method returns an error code if problems are found.  
!
!     The arguments are:
!     \begin{description}
!     \item [xgrid]
!           {\tt ESMF\_XGrid} to validate.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if the {\tt xgrid} 
!           is valid.
!     \end{description}
!
!EOPI

      integer :: localrc, i

      type(ESMF_XGridType), pointer :: xgtypep
      type(ESMF_Status) :: xgridstatus
      integer :: ngridA, ngridB

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! Check variables
      ESMF_INIT_CHECK_DEEP(ESMF_XGridGetInit,xgrid,rc)

      if (.not.associated(xgrid%xgtypep)) then 
         call ESMF_LogSetError(rcToCheck=ESMF_RC_OBJ_BAD, &
            msg="Uninitialized or already destroyed XGrid: xgtypep unassociated", &
             ESMF_CONTEXT, rcToReturn=rc)
         return
      endif 

      xgtypep => xgrid%xgtypep

      ! Make sure the xgrid is ready before trying to look at contents
      call ESMF_BaseGetStatus(xgtypep%base, xgridstatus, rc=localrc)
      if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      if (xgridstatus .ne. ESMF_STATUS_READY) then
         call ESMF_LogSetError(rcToCheck=ESMF_RC_OBJ_BAD, &
            msg="Uninitialized or already destroyed XGrid: xgridstatus not ready", &
             ESMF_CONTEXT, rcToReturn=rc)
         return
      endif 

      ! Verify all associated arrays are sized consistently
      if(associated(xgtypep%sideA)) ngridA = size(xgtypep%sideA, 1)
      if(associated(xgtypep%sideB)) ngridB = size(xgtypep%sideB, 1)

      if(associated(xgtypep%area) .and. associated(xgtypep%centroid)) then
        if(size(xgtypep%area, 1) /= size(xgtypep%centroid, 1)) then
           call ESMF_LogSetError(rcToCheck=ESMF_RC_OBJ_BAD, &
             msg="number of area cells differs from number of centroid cells", &
             ESMF_CONTEXT, rcToReturn=rc)
           return
        endif
      endif

      if(associated(xgtypep%sparseMatA2X) .or. associated(xgtypep%sparseMatX2A)) then
        if(associated(xgtypep%distgridA)) then
          if(size(xgtypep%distgridA, 1) /= ngridA) then
             call ESMF_LogSetError(rcToCheck=ESMF_RC_OBJ_BAD, &
               msg="number of distgrids on side A differs from number of Grids on side A", &
               ESMF_CONTEXT, rcToReturn=rc)
             return
          endif
        endif
      endif

      if(associated(xgtypep%sparseMatA2X)) then
        if(size(xgtypep%sparseMatA2X, 1) /= ngridA) then
           call ESMF_LogSetError(rcToCheck=ESMF_RC_OBJ_BAD, &
             msg="number of sparseMat objects on side A differs from number of Grids on side A", &
             ESMF_CONTEXT, rcToReturn=rc)
           return
        endif
        do i = 1, ngridA
          if(size(xgtypep%sparseMatA2X(i)%factorIndexList, 2) /= &
            size(xgtypep%sparseMatA2X(i)%factorList, 1)) then
             call ESMF_LogSetError(rcToCheck=ESMF_RC_OBJ_BAD, &
               msg="number of elements in sparseMatA2X is inconsistent", &
               ESMF_CONTEXT, rcToReturn=rc)
            return
          endif
        enddo
      endif

      if(associated(xgtypep%sparseMatX2A)) then
        if(size(xgtypep%sparseMatX2A, 1) /= ngridA) then
           call ESMF_LogSetError(rcToCheck=ESMF_RC_OBJ_BAD, &
             msg="number of sparseMat objects on side A differs from number of Grids on side A", &
             ESMF_CONTEXT, rcToReturn=rc)
           return
        endif
        do i = 1, ngridA
          if(size(xgtypep%sparseMatX2A(i)%factorIndexList, 2) /= &
            size(xgtypep%sparseMatX2A(i)%factorList, 1)) then
             call ESMF_LogSetError(rcToCheck=ESMF_RC_OBJ_BAD, &
               msg="number of elements in sparseMatX2A is inconsistent", &
               ESMF_CONTEXT, rcToReturn=rc)
            return
          endif
        enddo
      endif

      ! B side
      if(associated(xgtypep%sparseMatB2X) .or. associated(xgtypep%sparseMatX2B)) then
        if(associated(xgtypep%distgridB)) then
          if(size(xgtypep%distgridB, 1) /= ngridB) then
             call ESMF_LogSetError(rcToCheck=ESMF_RC_OBJ_BAD, &
               msg="number of distgrids on side B differs from number of Grids on side B", &
               ESMF_CONTEXT, rcToReturn=rc)
             return
          endif
        endif
      endif

      if(associated(xgtypep%sparseMatB2X)) then
        if(size(xgtypep%sparseMatB2X, 1) /= ngridB) then
           call ESMF_LogSetError(rcToCheck=ESMF_RC_OBJ_BAD, &
             msg="number of sparseMat objects on side B differs from number of Grids on side B", &
             ESMF_CONTEXT, rcToReturn=rc)
           return
        endif
        do i = 1, ngridB
          if(size(xgtypep%sparseMatB2X(i)%factorIndexList, 2) /= &
            size(xgtypep%sparseMatB2X(i)%factorList, 1)) then
             call ESMF_LogSetError(rcToCheck=ESMF_RC_OBJ_BAD, &
               msg="number of elements in sparseMatB2X is inconsistent", &
               ESMF_CONTEXT, rcToReturn=rc)
            return
          endif
        enddo
      endif

      if(associated(xgtypep%sparseMatX2B)) then
        if(size(xgtypep%sparseMatX2B, 1) /= ngridB) then
           call ESMF_LogSetError(rcToCheck=ESMF_RC_OBJ_BAD, &
             msg="number of sparseMat objects on side B differs from number of Grids on side B", &
             ESMF_CONTEXT, rcToReturn=rc)
           return
        endif
        do i = 1, ngridB
          if(size(xgtypep%sparseMatX2B(i)%factorIndexList, 2) /= &
            size(xgtypep%sparseMatX2B(i)%factorList, 1)) then
             call ESMF_LogSetError(rcToCheck=ESMF_RC_OBJ_BAD, &
               msg="number of elements in sparseMatX2B is inconsistent", &
               ESMF_CONTEXT, rcToReturn=rc)
            return
          endif
        enddo
      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_XGridValidate

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!
! This section includes all XGrid internal methods.
!
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_XGridSerialize"

!BOPI
! !IROUTINE: ESMF_XGridSerialize - Serialize xgrid info into a byte stream
!
! !INTERFACE:
      subroutine ESMF_XGridSerialize(xgrid, buffer, length, offset, &
                                    attreconflag, inquireflag, rc) 
!
! !ARGUMENTS:
      type(ESMF_XGrid), intent(in)                      :: xgrid 
      character, pointer, dimension(:)                  :: buffer
      integer, intent(inout)                            :: length
      integer, intent(inout)                            :: offset
      type(ESMF_AttReconcileFlag), intent(in), optional :: attreconflag
      type(ESMF_InquireFlag), intent(in), optional      :: inquireflag
      integer, intent(out), optional                    :: rc 
!
! !DESCRIPTION:
!      Takes an {\tt ESMF\_XGrid} object and adds all the information needed
!      to save the information to a stream or recreate the object based on this
!      information.   Expected to be used by {\tt ESMF\_StateReconcile()}.
!
!     The arguments are:
!     \begin{description}
!     \item [xgrid]
!           {\tt ESMF\_XGrid} object to be serialized.
!     \item [buffer]
!           Data buffer which will hold the serialized information.
!     \item [length]
!           Current length of buffer, in bytes.  If the serialization
!           process needs more space it will allocate it and update
!           this length.
!     \item [offset]
!           Current write offset in the current buffer.  This will be
!           updated by this routine and return pointing to the next
!           available byte in the buffer.
!     \item[{[attreconflag]}]
!           Flag to tell if Attribute serialization is to be done
!     \item[{[inquireflag]}]
!           Flag to tell if serialization is to be done (ESMF_NOINQUIRE)
!           or if this is simply a size inquiry (ESMF_INQUIREONLY)
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI

      integer :: localrc, ngridA, ngridB, i
      type(ESMF_XGridType), pointer :: fp    ! xgrid type
      type(ESMF_AttReconcileFlag) :: lattreconflag
      type(ESMF_InquireFlag) :: linquireflag
      integer :: s(4)               ! association status variables
      integer :: flag

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_XGridGetInit,xgrid,rc)

      ! deal with optional attreconflag and inquireflag
      if (present(attreconflag)) then
        lattreconflag = attreconflag
      else
        lattreconflag = ESMF_ATTRECONCILE_OFF
      endif

      if (present (inquireflag)) then
        linquireflag = inquireflag
      else
        linquireflag = ESMF_NOINQUIRE
      end if

      ! shortcut to internals
      fp => xgrid%xgtypep

      call ESMF_BaseSerialize(fp%base, buffer, offset, &
                                 lattreconflag, linquireflag, rc=localrc)
      if (ESMF_LogFoundError(localrc, &
                                 ESMF_ERR_PASSTHRU, &
                                 ESMF_CONTEXT, rcToReturn=rc)) return

      ! set up the association status array and other meta-data
      s = 0
      ngridA = 0
      ngridB = 0
      if(associated(fp%distgridA)) s(XG_S_DGA) = 1
      if(associated(fp%distgridB)) s(XG_S_DGB) = 1
      if(associated(fp%sideA)) then
        s(XG_S_GA) = 1
        ngridA = size(fp%sideA,1)
      endif
      if(associated(fp%sideB)) then
        s(XG_S_GB) = 1
        ngridB = size(fp%sideB,1)
      endif

      ! call into the C api first to serialize this status array and other meta-data
      ! this ensures consistency when deserialization
      flag = 0
      if(fp%storeOverlay) flag = 1
      call c_ESMC_XGridSerialize(s, ngridA, ngridB, fp%online, flag, &
                                 buffer, length, offset, linquireflag, localrc)
      if (ESMF_LogFoundError(localrc, &
                                 ESMF_ERR_PASSTHRU, &
                                 ESMF_CONTEXT, rcToReturn=rc)) return

      ! serialize the balanced distgrid
      call c_ESMC_DistGridSerialize(fp%distgridM, buffer, length, offset, &
                                   linquireflag, localrc)
      if (ESMF_LogFoundError(localrc, &
                               ESMF_ERR_PASSTHRU, &
                               ESMF_CONTEXT, rcToReturn=rc)) return

      ! serialize the rest of the XGrid members
      if(associated(fp%distgridA)) then
          ngridA = size(fp%distgridA,1)
          do i = 1, ngridA
            call c_ESMC_DistGridSerialize(fp%distgridA(i), buffer, length, offset, &
                                         linquireflag, localrc)
            if (ESMF_LogFoundError(localrc, &
                                     ESMF_ERR_PASSTHRU, &
                                     ESMF_CONTEXT, rcToReturn=rc)) return
          enddo
      endif

      if(associated(fp%distgridB)) then
          ngridB = size(fp%distgridB,1)
          do i = 1, ngridB
            call c_ESMC_DistGridSerialize(fp%distgridB(i), buffer, length, offset, &
                                         linquireflag, localrc)
            if (ESMF_LogFoundError(localrc, &
                                     ESMF_ERR_PASSTHRU, &
                                     ESMF_CONTEXT, rcToReturn=rc)) return
          enddo
      endif

      ! serialize the Grids
      if(associated(fp%sideA)) then
          ngridA = size(fp%sideA,1)
          do i = 1, ngridA
            call ESMF_XGridGeomBaseSerialize(fp%sideA(i), buffer=buffer, &
                         length=length, offset=offset, &
                         attreconflag=lattreconflag, inquireflag=linquireflag, &
                         rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                                     ESMF_ERR_PASSTHRU, &
                                     ESMF_CONTEXT, rcToReturn=rc)) return
          enddo
      endif

      if(associated(fp%sideB)) then
          ngridB = size(fp%sideB,1)
          do i = 1, ngridB
            call ESMF_XGridGeomBaseSerialize(fp%sideB(i), buffer=buffer, &
                         length=length, offset=offset, &
                         attreconflag=lattreconflag, inquireflag=linquireflag, &
                         rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                                     ESMF_ERR_PASSTHRU, &
                                     ESMF_CONTEXT, rcToReturn=rc)) return
          enddo
      endif

      ! serialize the Fracs
      if(associated(fp%fracA2X)) then
          ngridA = size(fp%fracA2X,1)
          do i = 1, ngridA
            call c_esmc_arrayserialize (fp%fracA2X(i), buffer, length, offset,  &
                lattreconflag, linquireflag, localrc)
            if (ESMF_LogFoundError(localrc, &
                                     ESMF_ERR_PASSTHRU, &
                                     ESMF_CONTEXT, rcToReturn=rc)) return
          enddo
      endif

      if(associated(fp%fracB2X)) then
          ngridB = size(fp%fracB2X,1)
          do i = 1, ngridB
            call c_esmc_arrayserialize (fp%fracB2X(i), buffer, length, offset,  &
                lattreconflag, linquireflag, localrc)
            if (ESMF_LogFoundError(localrc, &
                                     ESMF_ERR_PASSTHRU, &
                                     ESMF_CONTEXT, rcToReturn=rc)) return
          enddo
      endif

      ! serialize the Fracs
      if(associated(fp%fracX2A)) then
          ngridA = size(fp%fracX2A,1)
          do i = 1, ngridA
            call c_esmc_arrayserialize (fp%fracX2A(i), buffer, length, offset,  &
                lattreconflag, linquireflag, localrc)
            if (ESMF_LogFoundError(localrc, &
                                     ESMF_ERR_PASSTHRU, &
                                     ESMF_CONTEXT, rcToReturn=rc)) return
          enddo
      endif

      if(associated(fp%fracX2B)) then
          ngridB = size(fp%fracX2B,1)
          do i = 1, ngridB
            call c_esmc_arrayserialize (fp%fracX2B(i), buffer, length, offset,  &
                lattreconflag, linquireflag, localrc)
            if (ESMF_LogFoundError(localrc, &
                                     ESMF_ERR_PASSTHRU, &
                                     ESMF_CONTEXT, rcToReturn=rc)) return
          enddo
      endif

      if(fp%online == 1) then
        call c_esmc_arrayserialize (fp%fracX, buffer, length, offset,  &
            lattreconflag, linquireflag, localrc)

        if (ESMF_LogFoundError(localrc, &
                                 ESMF_ERR_PASSTHRU, &
                                 ESMF_CONTEXT, rcToReturn=rc)) return
      endif

      ! serialize the Frac2s
      if(associated(fp%frac2A)) then
          ngridA = size(fp%frac2A,1)
          do i = 1, ngridA
            call c_esmc_arrayserialize (fp%frac2A(i), buffer, length, offset,  &
                lattreconflag, linquireflag, localrc)
            if (ESMF_LogFoundError(localrc, &
                                     ESMF_ERR_PASSTHRU, &
                                     ESMF_CONTEXT, rcToReturn=rc)) return
          enddo
      endif

      if(associated(fp%frac2B)) then
          ngridB = size(fp%frac2B,1)
          do i = 1, ngridB
            call c_esmc_arrayserialize (fp%frac2B(i), buffer, length, offset,  &
                lattreconflag, linquireflag, localrc)
            if (ESMF_LogFoundError(localrc, &
                                     ESMF_ERR_PASSTHRU, &
                                     ESMF_CONTEXT, rcToReturn=rc)) return
          enddo
      endif

      if(fp%storeOverlay) then
        call ESMF_MeshSerialize(mesh=fp%mesh, buffer=buffer, &
           length=length, offset=offset, &
           inquireflag=linquireflag, &
           rc=localrc)
        if (ESMF_LogFoundError(localrc, &
           ESMF_ERR_PASSTHRU, &
           ESMF_CONTEXT, rcToReturn=rc)) return
      endif

      if  (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_XGridSerialize

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_XGridDeserialize"

!BOPI
! !IROUTINE: ESMF_XGridDeserialize - Deserialize a byte stream into a XGrid
!
! !INTERFACE:
      function ESMF_XGridDeserialize(buffer, offset, &
                                    attreconflag, rc) 
!
! !RETURN VALUE:
      type(ESMF_XGrid)                      :: ESMF_XGridDeserialize   
!
! !ARGUMENTS:
      character, pointer, dimension(:)      :: buffer
      integer, intent(inout)                :: offset
      type(ESMF_AttReconcileFlag), optional :: attreconflag
      integer, intent(out), optional        :: rc 
!
! !DESCRIPTION:
!      Takes a byte-stream buffer and reads the information needed to
!      recreate a XGrid object.  Recursively calls the deserialize routines
!      needed to recreate the subobjects.
!      Expected to be used by {\tt ESMF\_StateReconcile()} and
!      by {\tt ESMF\_XGridWrite()} and {\tt ESMF\_XGridRead()}.
!
!     The arguments are:
!     \begin{description}
!     \item [buffer]
!           Data buffer which holds the serialized information.
!     \item [offset]
!           Current read offset in the current buffer.  This will be
!           updated by this routine and return pointing to the next
!           unread byte in the buffer.
!     \item[{[attreconflag]}]
!           Flag to tell if Attribute serialization is to be done
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI

      integer :: localrc, ngridA, ngridB, i, step
      type(ESMF_XGridType), pointer :: fp    ! xgrid type
      integer staggerloc
      type(ESMF_AttReconcileFlag) :: lattreconflag
      integer :: s(4)               ! association status variables
      integer :: flag

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if  (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! deal with optional attreconflag
      if (present(attreconflag)) then
        lattreconflag = attreconflag
      else
        lattreconflag = ESMF_ATTRECONCILE_OFF
      endif

      ! In case of error, make sure this is invalid.
      nullify(ESMF_XGridDeserialize%xgtypep)

      ! Shortcut to internals
      allocate(fp, stat=localrc)
      if (ESMF_LogFoundAllocError(localrc, &
                                     msg="space for new XGrid object", &
                                     ESMF_CONTEXT, rcToReturn=rc)) return

      call ESMF_XGridInitialize(fp, rc=localrc)
      if (ESMF_LogFoundError(localrc, &
                                 ESMF_ERR_PASSTHRU, &
                                 ESMF_CONTEXT, rcToReturn=rc)) return

      ! Deserialize Base
      fp%base = ESMF_BaseDeserialize(buffer, offset, lattreconflag, rc=localrc)
      if (ESMF_LogFoundError(localrc, &
                                 ESMF_ERR_PASSTHRU, &
                                 ESMF_CONTEXT, rcToReturn=rc)) return
                                 
      call ESMF_BaseSetInitCreated(fp%base, rc=localrc)
      if (ESMF_LogFoundError(localrc, &
                                 ESMF_ERR_PASSTHRU, &
                                 ESMF_CONTEXT, rcToReturn=rc)) return

      ! call into the C api to deserialize this status array and other meta-data
      flag = 0
      call c_ESMC_XGridDeserialize(s, ngridA, ngridB, fp%online, flag, &
                                 buffer, offset, localrc)
      if (ESMF_LogFoundError(localrc, &
                                 ESMF_ERR_PASSTHRU, &
                                 ESMF_CONTEXT, rcToReturn=rc)) return

      ! Deserialize the balanced distgrid
      call C_ESMC_DistGridDeserialize(fp%distgridM, buffer, offset, &
                                   localrc)
      if (ESMF_LogFoundError(localrc, &
                               ESMF_ERR_PASSTHRU, &
                               ESMF_CONTEXT, rcToReturn=rc)) return

      ! set up memory based on the association status array and other meta-data
      if(s(XG_S_DGA) == 1) allocate(fp%distgridA(ngridA))
      if(s(XG_S_DGB) == 1) allocate(fp%distgridB(ngridB))
      if(s(XG_S_GA) == 1) allocate(fp%sideA(ngridA))
      if(s(XG_S_GB) == 1) allocate(fp%sideB(ngridB))
      if(fp%online == 1) then
        allocate(fp%fracA2X(ngridA))
        allocate(fp%fracB2X(ngridB))
        allocate(fp%fracX2A(ngridA))
        allocate(fp%fracX2B(ngridB))
        allocate(fp%frac2A(ngridA))
        allocate(fp%frac2B(ngridB))
      endif

      ! Deserialize the rest of the XGrid members
      if(associated(fp%distgridA)) then
          do i = 1, ngridA
            call C_ESMC_DistGridDeserialize(fp%distgridA(i), buffer, offset, &
                                     localrc)
            if (ESMF_LogFoundError(localrc, &
                                     ESMF_ERR_PASSTHRU, &
                                     ESMF_CONTEXT, rcToReturn=rc)) return
          enddo
      endif

      if(associated(fp%distgridB)) then
          do i = 1, ngridB
            call C_ESMC_DistGridDeserialize(fp%distgridB(i), buffer, offset, &
                                     localrc)
            if (ESMF_LogFoundError(localrc, &
                                     ESMF_ERR_PASSTHRU, &
                                     ESMF_CONTEXT, rcToReturn=rc)) return
          enddo
      endif

      ! Deserialize the Grids
      if(associated(fp%sideA)) then
          do i = 1, ngridA
            fp%sideA(i) = ESMF_XGridGeomBaseDeserialize(buffer=buffer, offset=offset, &
                                     rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                                     ESMF_ERR_PASSTHRU, &
                                     ESMF_CONTEXT, rcToReturn=rc)) return
          enddo
      endif

      if(associated(fp%sideB)) then
          do i = 1, ngridB
            fp%sideB(i) = ESMF_XGridGeomBaseDeserialize(buffer=buffer, offset=offset, &
                                     rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                                     ESMF_ERR_PASSTHRU, &
                                     ESMF_CONTEXT, rcToReturn=rc)) return
          enddo
      endif

      ! Deserialize the Frac Arrays
      if(associated(fp%fracA2X)) then
          do i = 1, ngridA
            call c_ESMC_ArrayDeserialize(fp%fracA2X(i), buffer, offset, &
                lattreconflag, localrc)
            if (ESMF_LogFoundError(localrc, &
                                     ESMF_ERR_PASSTHRU, &
                                     ESMF_CONTEXT, rcToReturn=rc)) return
          enddo
      endif

      if(associated(fp%fracB2X)) then
          do i = 1, ngridB
            call c_ESMC_ArrayDeserialize(fp%fracB2X(i), buffer, offset, &
                lattreconflag, localrc)
            if (ESMF_LogFoundError(localrc, &
                                     ESMF_ERR_PASSTHRU, &
                                     ESMF_CONTEXT, rcToReturn=rc)) return
          enddo
      endif
      if(associated(fp%fracX2A)) then
          do i = 1, ngridA
            call c_ESMC_ArrayDeserialize(fp%fracX2A(i), buffer, offset, &
                lattreconflag, localrc)
            if (ESMF_LogFoundError(localrc, &
                                     ESMF_ERR_PASSTHRU, &
                                     ESMF_CONTEXT, rcToReturn=rc)) return
          enddo
      endif
      if(associated(fp%fracX2B)) then
          do i = 1, ngridB
            call c_ESMC_ArrayDeserialize(fp%fracX2B(i), buffer, offset, &
                lattreconflag, localrc)
            if (ESMF_LogFoundError(localrc, &
                                     ESMF_ERR_PASSTHRU, &
                                     ESMF_CONTEXT, rcToReturn=rc)) return
          enddo
      endif
      if(fp%online == 1) then
          call c_ESMC_ArrayDeserialize(fp%fracX, buffer, offset, &
              lattreconflag, localrc)
          if (ESMF_LogFoundError(localrc, &
                                   ESMF_ERR_PASSTHRU, &
                                   ESMF_CONTEXT, rcToReturn=rc)) return
      endif
      if(associated(fp%frac2A)) then
          do i = 1, ngridA
            call c_ESMC_ArrayDeserialize(fp%frac2A(i), buffer, offset, &
                lattreconflag, localrc)
            if (ESMF_LogFoundError(localrc, &
                                     ESMF_ERR_PASSTHRU, &
                                     ESMF_CONTEXT, rcToReturn=rc)) return
          enddo
      endif
      if(associated(fp%frac2B)) then
          do i = 1, ngridB
            call c_ESMC_ArrayDeserialize(fp%frac2B(i), buffer, offset, &
                lattreconflag, localrc)
            if (ESMF_LogFoundError(localrc, &
                                     ESMF_ERR_PASSTHRU, &
                                     ESMF_CONTEXT, rcToReturn=rc)) return
          enddo
      endif

      if(flag == 1) then
        fp%storeOverlay = .true.
        fp%mesh = ESMF_MeshDeserialize(buffer=buffer, offset=offset, &
                                 rc=localrc)
        if (ESMF_LogFoundError(localrc, &
                                 ESMF_ERR_PASSTHRU, &
                                 ESMF_CONTEXT, rcToReturn=rc)) return
      endif

      fp%is_proxy = .true.
      ESMF_XGridDeserialize%xgtypep => fp
      
      ! Add copy of this object into ESMF garbage collection table
      call c_ESMC_VMAddFObject(ESMF_XGridDeserialize, ESMF_ID_XGRID%objectID)
      
      ESMF_INIT_SET_CREATED(ESMF_XGridDeserialize)

      if  (present(rc)) rc = ESMF_SUCCESS

      end function ESMF_XGridDeserialize

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_XGridWriteVTK"

!BOPI
! !IROUTINE: ESMF_XGridWriteVTK - Write the midmesh in XGrid
!
! !INTERFACE:
      subroutine ESMF_XGridWriteVTK(xgrid, filename, &
         nodeArray1, nodeArray2, nodeArray3, &
         elemArray1, elemArray2, elemArray3, &
         rc)
!
! !ARGUMENTS:
      type(ESMF_XGrid), intent(in)            :: xgrid
      character(len=*), intent(in)            :: filename
      type(ESMF_Array), intent(in), optional  :: nodeArray1
      type(ESMF_Array), intent(in), optional  :: nodeArray2
      type(ESMF_Array), intent(in), optional  :: nodeArray3
      type(ESMF_Array), intent(in), optional  :: elemArray1
      type(ESMF_Array), intent(in), optional  :: elemArray2
      type(ESMF_Array), intent(in), optional  :: elemArray3
      integer,          intent(out), optional :: rc
!
! !DESCRIPTION:
!      Write the midmesh in XGrid as vtk file and possibly some data for examination
!
!     The arguments are:
!     \begin{description}
!     \item [xgrid]
!           XGrid to write. Must be created upon entry.
!     \item [filename]
!           VTK filename
!   \item[{[nodeArray1-3]}]
!         Arrays built on the node location of the mesh
!   \item[{[elemArray1-3]}]
!         Arrays built on the element location of the mesh
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI

      integer :: localrc

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if  (present(rc)) rc = ESMF_RC_NOT_IMPL

      if(xgrid%xgtypep%storeoverlay) then
        call ESMF_MeshWriteVTK(xgrid%xgtypep%mesh, filename=filename, &
         nodeArray1=nodeArray1, nodeArray2=nodeArray2, nodeArray3=nodeArray3, &
         elemArray1=elemArray1, elemArray2=elemArray2, elemArray3=elemArray3, &
         rc=localrc)
        if (ESMF_LogFoundError(localrc, &
           ESMF_ERR_PASSTHRU, &
           ESMF_CONTEXT, rcToReturn=rc)) return
      endif

      if  (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_XGridWriteVTK

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_XGridGetInit"
!BOPI
! !IROUTINE:  ESMF_XGridGetInit - Get initialization status.

! !INTERFACE:
    function ESMF_XGridGetInit(d)
!
! !ARGUMENTS:
       type(ESMF_XGrid), intent(in), optional :: d
       ESMF_INIT_TYPE :: ESMF_XGridGetInit
!
! !DESCRIPTION:
!      Get the initialization status of the Deep class {\tt xgrid}.
!
!     The arguments are:
!     \begin{description}
!     \item [s]
!           {\tt ESMF\_XGrid} from which to retrieve status.
!     \end{description}
!
!EOPI


       if (present(d)) then
         ESMF_XGridGetInit = ESMF_INIT_GET(d)
       else
         ESMF_XGridGetInit = ESMF_INIT_CREATED
       endif

    end function ESMF_XGridGetInit

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_XGridInitialize"

!BOPI
! !IROUTINE: ESMF_XGridInitialize - Default initialize xgrid member variables
!
! !INTERFACE:
      subroutine ESMF_XGridInitialize(xgtypep, rc) 
!
! !ARGUMENTS:
      type(ESMF_XGridType), pointer :: xgtypep 
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!      Takes an {\tt ESMF\_XGrid} object and default initialize its
!      auxiliary data members. Only to be used by other XGrid Create methods.
!
!     The arguments are:
!     \begin{description}
!     \item [xgtypep]
!           {\tt ESMF\_XGridType} object to be default initialized.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
        xgtypep%status  = ESMF_STATUS_UNINIT
        xgtypep%online         = 0
        xgtypep%is_proxy       = .false. 
        xgtypep%storeOverlay   = .false. 
        nullify(xgtypep%sideA)
        nullify(xgtypep%sideB)
        nullify(xgtypep%fracA2X)
        nullify(xgtypep%fracB2X)
        nullify(xgtypep%fracX2A)
        nullify(xgtypep%fracX2B)
        nullify(xgtypep%frac2A)
        nullify(xgtypep%frac2B)
        nullify(xgtypep%area)
        nullify(xgtypep%centroid)
        nullify(xgtypep%sparseMatA2X)
        nullify(xgtypep%sparseMatX2A)
        nullify(xgtypep%sparseMatB2X)
        nullify(xgtypep%sparseMatX2B)

        if(present(rc)) rc = ESMF_SUCCESS

    end subroutine ESMF_XGridInitialize

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_XGridSideEqual"
!BOPI
! !IROUTINE: ESMF_XGridSideEqual - Equality of XGridSides
!
! !INTERFACE:
      function ESMF_XGridSideEqual(XGridSide1, XGridSide2)

! !RETURN VALUE:
      logical :: ESMF_XGridSideEqual

! !ARGUMENTS:

      type (ESMF_XGridSide_Flag), intent(in) :: &
        XGridSide1,      &! Two xgrid sides to compare for
        XGridSide2        ! equality

! !DESCRIPTION:
!     This routine compares two ESMF XGridSide to see if
!     they are equivalent.
!
!     The arguments are:
!     \begin{description}
!     \item[XGridSide1, XGridSide2]
!          Two XGridSide to compare for equality
!     \end{description}
!
!EOPI

      ESMF_XGridSideEqual = (XGridSide1%side == &
                              XGridSide2%side)

      end function ESMF_XGridSideEqual
!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_XGridSideNotEqual"
!BOPI
! !IROUTINE: ESMF_XGridSideNotEqual - Non-equality of XGridSides
!
! !INTERFACE:
      function ESMF_XGridSideNotEqual(XGridSide1, XGridSide2)

! !RETURN VALUE:
      logical :: ESMF_XGridSideNotEqual

! !ARGUMENTS:

      type (ESMF_XGridSide_Flag), intent(in) :: &
         XGridSide1,      &! Two XGridSide Statuses to compare for
         XGridSide2        ! inequality

! !DESCRIPTION:
!     This routine compares two ESMF XGridSide to see if
!     they are unequal.
!
!     The arguments are:
!     \begin{description}
!     \item[XGridSide1, XGridSide2]
!          Two XGridSides to compare for inequality
!     \end{description}
!
!EOPI

      ESMF_XGridSideNotEqual = (XGridSide1%side /= &
                                 XGridSide2%side)

      end function ESMF_XGridSideNotEqual


end module ESMF_XGridMod
