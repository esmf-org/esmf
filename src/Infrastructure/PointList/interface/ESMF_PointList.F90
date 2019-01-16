! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2019, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
#define ESMF_FILENAME "ESMF_PointList.F90"
!==============================================================================
!
! ESMF PointList Module
module ESMF_PointListMod
!
!==============================================================================
!
! This file contains the Fortran wrapper code for the C++ implementation of
!  the PointList class.
!
!------------------------------------------------------------------------------
! INCLUDES
#include "ESMF.h"

!==============================================================================
!BOPI
! !MODULE: ESMF_PointListMod
!
! !DESCRIPTION:
!
!   Fortran API wrapper of C++ implementation of PointList
!
!------------------------------------------------------------------------------

! !USES:
  use ESMF_UtilTypesMod     ! ESMF utility types
  use ESMF_InitMacrosMod    ! ESMF initializer macros
  use ESMF_LogErrMod        ! ESMF error handling
  use ESMF_IOUtilMod
  use ESMF_GridMod
  use ESMF_MeshMod
  use ESMF_ArrayMod
  use ESMF_DistGridMod
  use ESMF_F90InterfaceMod
  use ESMF_StaggerLocMod
  use ESMF_LocStreamMod

  implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
  private

!------------------------------------------------------------------------------
! !  ESMF_PointListType
! !  MUST STAY IN SYNC WITH C++ header file
!
  type ESMF_PointList
#ifndef ESMF_NO_SEQUENCE
    sequence
#endif
    private
    type(ESMF_Pointer) :: this    ! opaque pointer to C++ class data
    ESMF_INIT_DECLARE
  end type

!------------------------------------------------------------------------------
! !PUBLIC TYPES:
  public ESMF_PointList

!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:
!
  public ESMF_PointListGetInit
  public ESMF_PointListSetInitCreated

  public ESMF_PointListCreate
  public ESMF_PointListDestroy

  public ESMF_PointListGet
  public ESMF_PointListGetForLoc
  public ESMF_PointListAdd

  public ESMF_PointListPrint
  public ESMF_PointListWriteVTK

!EOPI
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
  character(*), parameter, private :: version = &
    '$Id$'

!==============================================================================
!
! INTERFACE BLOCKS
!
!==============================================================================


! -------------------------- ESMF-public method -------------------------------
!BOPI
! !IROUTINE: ESMF_PointListCreate -- Generic interface

! !INTERFACE:
interface ESMF_PointListCreate

! !PRIVATE MEMBER FUNCTIONS:
!
        module procedure ESMF_PointListCreateFrmGrid
        module procedure ESMF_PointListCreateFrmMesh
        module procedure ESMF_PointListCreateFrmLocStream
        module procedure ESMF_PointListCreateFrmInput

! !DESCRIPTION:
! This interface provides a single entry point for the various
!  types of {\tt ESMF\_PointListCreate} functions.
!EOPI
end interface




!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_PointListGetInit"
!BOPI
! !IROUTINE: ESMF_PointListGetInit - Get the Init status

! !INTERFACE:
  function ESMF_PointListGetInit(d)
!
! !RETURN VALUE:
    ESMF_INIT_TYPE :: ESMF_PointListGetInit
!
! !ARGUMENTS:
    type(ESMF_PointList), intent(in),optional :: d
!
! !DESCRIPTION:
!   Get the init status
!
!   The arguments are:
!   \begin{description}
!   \item[d]
!     The class to be queried
!   \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    if (present(d)) then
      ESMF_PointListGetInit=ESMF_INIT_GET(d)
    else
      ESMF_PointListGetInit=ESMF_INIT_CREATED
    endif
  end function ESMF_PointListGetInit
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_PointListSetInitCreated()"
!BOPI
! !IROUTINE: ESMF_PointListSetInitCreated - Set PointList init code to "CREATED"

! !INTERFACE:
  subroutine ESMF_PointListSetInitCreated(pointlist, rc)
!
! !ARGUMENTS:
    type(ESMF_PointList), intent(inout)           :: pointlist
    integer,                intent(out),  optional  :: rc
!
!
! !DESCRIPTION:
!   Set init code in PointList object to "CREATED".
!
!   The arguments are:
!   \begin{description}
!   \item[pointlist]
!     Specified {\tt ESMF\_PointList} object.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    ! initialize return code; assume routine not implemented
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Set init code
    ESMF_INIT_SET_CREATED(pointlist)

    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_PointListSetInitCreated
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_PointListCreateFrmGrid"
!BOPI
! !IROUTINE: ESMF_PointListCreateFrmGrid - Create a new PointList from input Grid

! !INTERFACE:
  function ESMF_PointListCreateFrmGrid(grid, staggerLoc, maskValues, rc)
!
! !RETURN VALUE:
    type(ESMF_PointList) :: ESMF_PointListCreateFrmGrid
!
! !ARGUMENTS:
    type(ESMF_Grid), intent(in)       :: grid
    type(ESMF_StaggerLoc), intent(in) :: staggerLoc
    integer(ESMF_KIND_I4), optional   :: maskValues(:)
    integer, intent(out), optional    :: rc
!
! !DESCRIPTION:
!   Allocates memory for a new {\tt ESMF\_PointList} object and
!   constructs its internals from input Grid.
!
!   The arguments are:
!   \begin{description}
!   \item[{grid}]
!     The grid to get the information from to create the PointList.
!   \item[{staggerLoc}]
!     stagger location
!   \item[{maskValues}]
!     Values to set as masked
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    integer             :: localrc      ! local return code
    type(ESMF_PointList)  :: pointlist
    type(ESMF_InterArray) :: maskValuesArg

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    pointlist%this = ESMF_NULL_POINTER

    ! convert mask values
    maskValuesArg = ESMF_InterArrayCreate(maskValues, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    ! Call C++ create code
    call c_ESMC_PointListCreateFrmGrid(grid, staggerLoc%staggerloc, maskValuesArg, pointlist, localrc)
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call c_ESMC_PointListSort(pointlist, localrc)
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_InterArrayDestroy(maskValuesArg, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

    ! Set return values
    ESMF_PointListCreateFrmGrid = pointlist

    ESMF_INIT_SET_CREATED(ESMF_PointListCreateFrmGrid)

    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function ESMF_PointListCreateFrmGrid
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_PointListCreateFrmMesh"
!BOPI
! !IROUTINE: ESMF_PointListCreateFrmMesh - Create a new PointList from input Mesh

! !INTERFACE:
  function ESMF_PointListCreateFrmMesh(mesh, meshLoc, maskValues, rc)
!
! !RETURN VALUE:
    type(ESMF_PointList) :: ESMF_PointListCreateFrmMesh
!
! !ARGUMENTS:
    type(ESMF_Mesh), intent(in)     :: mesh
    type(ESMF_MeshLoc), intent(in)  :: meshLoc
    integer(ESMF_KIND_I4), optional :: maskValues(:)
    integer, intent(out), optional  :: rc
!
! !DESCRIPTION:
!   Allocates memory for a new {\tt ESMF\_PointList} object and
!   constructs its internals from input Mesh.
!
!   The arguments are:
!   \begin{description}
!   \item[{mesh}]
!     The mesh to get the information from to create the PointList.
!   \item[{meshLoc}]
!     mesh location
!   \item[{maskValues}]
!     Values to set as masked
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    integer             :: localrc      ! local return code
    type(ESMF_PointList)  :: pointlist
    type(ESMF_InterArray) :: maskValuesArg

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    pointlist%this = ESMF_NULL_POINTER

    ! convert mask values
    maskValuesArg = ESMF_InterArrayCreate(maskValues, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    ! Call C++ create code
    call c_ESMC_PointListCreateFrmMesh(mesh, meshLoc, maskValuesArg, pointlist, localrc)
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_InterArrayDestroy(maskValuesArg, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

    ! Set return values
    ESMF_PointListCreateFrmMesh = pointlist

    ESMF_INIT_SET_CREATED(ESMF_PointListCreateFrmMesh)

    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function ESMF_PointListCreateFrmMesh
!------------------------------------------------------------------------------



#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_PointListCreateFrmLocStream"
!BOPI
! !IROUTINE: ESMF_PointListCreateFrmLocStream - Create a new PointList from input LocStream

! !INTERFACE:
  function ESMF_PointListCreateFrmLocStream(locstream, maskValues, rc)
!
! !RETURN VALUE:
    type(ESMF_PointList) :: ESMF_PointListCreateFrmLocStream
!
! !ARGUMENTS:
    type(ESMF_LocStream), intent(in)  :: locstream
    integer(ESMF_KIND_I4), intent(in), optional   :: maskValues(:)
    integer, intent(out), optional    :: rc
!
! !DESCRIPTION:
!   Allocates memory for a new {\tt ESMF\_PointList} object and
!   constructs its internals from input LocStream.
!
!   The arguments are:
!   \begin{description}
!   \item[{locStream}]
!     The Location Stream to get the information from to create the PointList.
!   \item[{maskValues}]
!     Values to set as masked
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    integer             :: localrc      ! local return code
    integer             :: i,j,k,dimcount,regrid_dims, num_local_pts, num_maskValues
    integer             :: cl,cu,cc
    integer             :: localDECount,lDE
    logical             :: d3Present,maskPresent,masked_value
    character(len=ESMF_MAXSTR)    :: keystring
    character(len=ESMF_MAXSTR), pointer  :: myKeyNames(:)
    type(ESMF_Array),pointer           :: myKeyArrays(:)
    type(ESMF_Array),pointer           :: thisKeyArray
    integer,allocatable           :: seqInd(:)
    real(ESMF_KIND_R8), pointer :: farrayPtrX(:),farrayPtrY(:),farrayPtrZ(:)
    real(ESMF_KIND_R8), allocatable :: mycoords(:), cart_coords(:)
    integer(ESMF_KIND_I4), pointer :: maskarray(:)
    type(ESMF_DistGrid) :: distgridOut
    integer :: seqCount
    type(ESMF_Array)       :: XArr,YArr,ZArr,MArr

    type(ESMF_CoordSys_Flag) :: coordSysLocal
    type(ESMF_PointList)  :: pointlist

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    pointlist%this = ESMF_NULL_POINTER


    call ESMF_LocStreamGet(locstream, coordSys=coordSysLocal, localDECount=localDECount, &
                           distgrid=distgridOut, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    dimcount = 0
    !dimension 1
    if (coordSysLocal .eq. ESMF_COORDSYS_CART) then
      keystring='ESMF:X'
    else
      keystring='ESMF:Lon'
    endif
    call ESMF_LocStreamGetKey(locstream, keyName=keystring, &
                              keyArray=XArr, rc=localrc)
    if (.not. ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) then
      dimcount = dimcount+1
    else
      return
    endif

    !dimension 2
    if (coordSysLocal .eq. ESMF_COORDSYS_CART) then
      keystring='ESMF:Y'
    else
      keystring='ESMF:Lat'
    endif
    call ESMF_LocStreamGetKey(locstream, keyName=keystring, &
                              keyArray=YArr, rc=localrc)
    if (.not. ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) then
      dimcount = dimcount+1
    else
      return
    endif

    !dimension 3?
    if (coordSysLocal .eq. ESMF_COORDSYS_CART) then
      keystring='ESMF:Z'
    else
      keystring='ESMF:Radius'
    endif
    !only deal with third dimension if it's present
    call ESMF_LocStreamGetKey(locstream, keyName=keystring, &
                              isPresent=d3Present, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    if (d3Present) then
      call ESMF_LocStreamGetKey(locstream, keyName=keystring, &
                                keyArray=ZArr, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      dimcount = dimcount+1
    endif

    allocate(mycoords(dimcount), stat=localrc)
    if (ESMF_LogFoundAllocError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call c_ESMC_PointListCalcCartDim(coordSysLocal, dimcount, regrid_dims, localrc)
    if (ESMF_LogFoundAllocError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    allocate(cart_coords(regrid_dims), stat=localrc)
    if (ESMF_LogFoundAllocError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return


    !only deal with mask info if it's present
    call ESMF_LocStreamGetKey(locstream, keyName='ESMF:Mask', &
                              isPresent=maskPresent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    if (maskPresent) then
      call ESMF_LocStreamGetKey(locstream, keyName='ESMF:Mask', &
                                keyArray=MArr, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    if (present(maskValues)) then
      if (.not. maskPresent) then
        call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_WRONG, &
             msg='- LocStream has no masking info for use with specified mask values', &
             ESMF_CONTEXT, rcToReturn=rc)
        return
      endif
      num_maskValues = size(maskValues)
    else
      num_maskValues = 0
    endif

    !must count the points first to create the pointlist with the proper size
    num_local_pts=0
    do lDE=0,localDECount-1
      if (maskPresent) then
        call ESMF_ArrayGet(MArr, localDE=lDE, farrayPtr=maskarray, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
      endif

      call  ESMF_LocStreamGetBounds(locstream, localDE=lDE, &
                              computationalLBound=cl, computationalUBound=cu, &
                              rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return

      do j=cl,cu
        masked_value=.false.

        !redundant if statement here is needed to avoid bug in Intel optimizer, which
        !occurs with versions 15.0.3, 16.0.0 and possibly others
        if (num_maskValues .gt. 0) then
        do k=1,num_maskValues
          if (maskArray(j) .eq. maskValues(k)) then
            masked_value=.true.
            exit
          endif
        enddo
        endif
        if (.not. masked_value) num_local_pts = num_local_pts + 1
      enddo
    enddo

    pointlist = ESMF_PointListCreate(maxpts=num_local_pts,numdims=regrid_dims, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    !now we add the points
    do lDE=0,localDECount-1
      call ESMF_ArrayGet(XArr, localDE=lDE, farrayPtr=farrayPtrX, rc=localrc)
      if (ESMF_LogFoundError(localrc, &
          msg="expecting coordinate keys to be REAL*8", &
          ESMF_CONTEXT, rcToReturn=rc)) return
      call ESMF_ArrayGet(YArr, localDE=lDE, farrayPtr=farrayPtrY, rc=localrc)
      if (ESMF_LogFoundError(localrc, &
          msg="expecting coordinate keys to be REAL*8", &
          ESMF_CONTEXT, rcToReturn=rc)) return
      if (dimcount .eq. 3) then
        call ESMF_ArrayGet(ZArr, localDE=lDE, farrayPtr=farrayPtrZ, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
          msg="expecting coordinate keys to be REAL*8", &
          ESMF_CONTEXT, rcToReturn=rc)) return
      endif

      !Allocate space for seqInd
      allocate(seqInd(size(farrayPtrX)), stat=localrc)
      if (ESMF_LogFoundAllocError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      call ESMF_DistGridGet(distgridOut, localDe=lDE, &
                            seqIndexList=seqInd, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return

      call  ESMF_LocStreamGetBounds(locstream, localDE=lDE, &
                              computationalLBound=cl, computationalUBound=cu, &
                              rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      cc=cu-cl+1

      if (size(farrayPtrX) .ne. size(farrayPtrY) .or. size(farrayPtrX) .ne. cc) then
        call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_WRONG, &
          msg='- coord arrays must be equal in size and greater than size 0', &
          ESMF_CONTEXT, rcToReturn=rc)
        return
      endif

      if (dimcount .eq. 3 ) then
        if (size(farrayPtrZ) .ne. size(farrayPtrX)) then
          call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_WRONG, &
            msg='- coord arrays must be equal in size and greater than size 0', &
            ESMF_CONTEXT, rcToReturn=rc)
          return
        endif
      endif

      if (maskPresent) then
        call ESMF_ArrayGet(MArr, localDE=lDE, farrayPtr=maskarray, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            msg="expecting mask key to be INTEGER*4", &
            ESMF_CONTEXT, rcToReturn=rc)) return

        if (size(maskarray) .ne. size(farrayPtrX)) then
          call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_WRONG, &
            msg='- mask array must be equal in size to coordinate arrays', &
            ESMF_CONTEXT, rcToReturn=rc)
          return
        endif
      endif

      do j=cl,cu
        masked_value=.false.

        !redundant if statement here is needed to avoid bug in Intel optimizer, which
        !occurs with versions 15.0.3, 16.0.0 and possibly others
        if (num_maskValues .gt. 0) then
        do k=1,num_maskValues
          if (maskArray(j) .eq. maskValues(k)) then
            masked_value=.true.
            exit
          endif
        enddo
        endif
        if (.not. masked_value) then
          mycoords(1)=farrayPtrX(j)
          mycoords(2)=farrayPtrY(j)
          if (dimcount .eq. 3) mycoords(3)=farrayPtrZ(j)

          call c_ESMC_PointListSph2CartCoord(coordSysLocal, dimcount, mycoords, &
                                             cart_coords, localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return

          call ESMF_PointListAdd(pointlist=pointlist,id=seqInd(j-cl+1), &
                                 loc_coords=cart_coords, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return
        endif
      enddo
      deallocate(seqInd, stat=localrc)
      if (ESMF_LogFoundAllocError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
    enddo

    deallocate(mycoords, stat=localrc)
    if (ESMF_LogFoundAllocError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    deallocate(cart_coords, stat=localrc)
    if (ESMF_LogFoundAllocError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Set return values
    ESMF_PointListCreateFrmLocStream = pointlist

    ESMF_INIT_SET_CREATED(ESMF_PointListCreateFrmLocStream)

    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function ESMF_PointListCreateFrmLocStream
!------------------------------------------------------------------------------


#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_PointListCreateFrmInput"
!BOPI
! !IROUTINE: ESMF_PointListCreateFrmInput - Create a new PointList from user input

! !INTERFACE:
  function ESMF_PointListCreateFrmInput(maxpts, numdims, rc)
!
! !RETURN VALUE:
    type(ESMF_PointList) :: ESMF_PointListCreateFrmInput
!
! !ARGUMENTS:
    integer, intent(in)                             :: maxpts, numdims
    integer, intent(out), optional                  :: rc
!
! !DESCRIPTION:
!   Allocates memory for a new {\tt ESMF\_PointList} object and
!   constructs its internals from test input
!
!   The arguments are:
!   \begin{description}
!   \item[{maxpts}]
!     The maximum number of points to hold in the PointList.
!   \item[{numdims}]
!     The number of dimensions for points in the PointList.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    integer             :: localrc      ! local return code
    type(ESMF_PointList)  :: pointlist

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    pointlist%this = ESMF_NULL_POINTER

    ! Call C++ create code

    call c_ESMC_PointListCreateFrmInput(maxpts, numdims, pointlist, localrc)

    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Set return values
    ESMF_PointListCreateFrmInput = pointlist

    ESMF_INIT_SET_CREATED(ESMF_PointListCreateFrmInput)

    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function ESMF_PointListCreateFrmInput
!------------------------------------------------------------------------------



!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_PointListDestroy"
!BOPI
! !IROUTINE: ESMF_PointListDestroy - Release resources associated with a PointList

! !INTERFACE:
  subroutine ESMF_PointListDestroy(pointlist, rc)
!
! !ARGUMENTS:
    type(ESMF_PointList), intent(inout) :: pointlist
    integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!   Destroys an {\tt ESMF\_PointList}, releaseing the resources associated
!   with the object.
!
!   The arguments are:
!   \begin{description}
!   \item[pointlist]
!     The {\tt ESMF\_PointList} to be destroyed.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! check input variable
    ESMF_INIT_CHECK_DEEP(ESMF_PointListGetInit,pointlist,rc)

    ! was pointlist already destroyed?
    if (pointlist%this .eq. ESMF_NULL_POINTER) then
      if (present(rc)) rc = ESMF_SUCCESS
      return
    endif

    ! Call C++ destroy code

    call c_ESMC_PointListDestroy(pointlist, localrc)
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! nullify pointer
    pointlist%this = ESMF_NULL_POINTER
    ESMF_INIT_SET_DELETED(pointlist)

    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_PointListDestroy
!------------------------------------------------------------------------------



!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_PointListGet"
!BOPI
! !IROUTINE: ESMF_PointListGet - Get information from a PointList

! !INTERFACE:
  subroutine ESMF_PointListGet(pointlist, numpts, maxpts, dims, rc)
!
! !ARGUMENTS:
    type(ESMF_PointList), intent(in) :: pointlist
    integer, intent(out), optional :: numpts
    integer, intent(out), optional :: maxpts
    integer, intent(out), optional :: dims
    integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Returns information about an {\tt ESMF\_PointList}.
!
!     The arguments are:
!     \begin{description}
!     \item[pointlist]
!          {\tt ESMF\_PointList} to be queried.
!     \item[{[numpts]}]
!          Returns current number of points.
!     \item[{[maxpts]}]
!          Returns maximum number of points allowed.
!     \item[{[dims]}]
!          Returns number of dimensions for coordinates.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ESMF_INIT_CHECK_DEEP(ESMF_PointListGetInit,pointlist,rc)

    if (present(numpts)) then
      call c_ESMC_PointListGetNumPts(pointlist, numpts, localrc)
      if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    if (present(maxpts)) then
      call c_ESMC_PointListGetMaxPts(pointlist, maxpts, localrc)
      if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    if (present(dims)) then
      call c_ESMC_PointListGetDims(pointlist, dims, localrc)
      if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif


    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_PointListGet
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_PointListGetForLoc"
!BOPI
! !IROUTINE: ESMF_PointListGetForLoc - Get information given location in a PointList

! !INTERFACE:
  subroutine ESMF_PointListGetForLoc(pointlist, loc, id, loc_coords, rc)
!
! !ARGUMENTS:
    type(ESMF_PointList), intent(in)                          :: pointlist
    integer,            intent(in)                          :: loc
    integer,            intent(out), optional               :: id
    real(ESMF_KIND_R8), intent(out), optional, dimension(:) :: loc_coords
    integer,            intent(out), optional               :: rc
!
! !DESCRIPTION:
!     Returns information about an {\tt ESMF\_PointList}.
!
!     The arguments are:
!     \begin{description}
!     \item[pointlist]
!          {\tt ESMF\_PointList} to be queried.
!     \item[loc]
!          Location within Pointlist to be queried. Locations values begin with zero.
!     \item[{[id]}]
!          Returns the id associated with location.
!     \item[{[loc_coords]}]
!          Returns array of coordinates associated with location.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ESMF_INIT_CHECK_DEEP(ESMF_PointListGetInit,pointlist,rc)

    if (present(id)) then
      call c_ESMC_PointListGetId(pointlist, loc, id, localrc)
      if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    if (present(loc_coords)) then
      call c_ESMC_PointListGetCoords(pointlist, loc, loc_coords, localrc)

      if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_PointListGetForLoc
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_PointListAdd"
!BOPI
! !IROUTINE: ESMF_PointListAdd - Adds a point to a PointList

! !INTERFACE:
  subroutine ESMF_PointListAdd(pointlist, id, loc_coords, rc)
!
! !ARGUMENTS:
    type(ESMF_PointList), intent(in)               :: pointlist
    integer,            intent(in)               :: id
    real(ESMF_KIND_R8), intent(in), dimension(:) :: loc_coords
    integer,            intent(out), optional    :: rc
!
! !DESCRIPTION:
!   Add a point to an {\tt ESMF\_PointList} with the given values.
!
!     The arguments are:
!     \begin{description}
!     \item[pointlist]
!          {\tt ESMF\_PointList} to be queried.
!     \item[{[id]}]
!          The id associated with point to add.
!     \item[{[loc_coords]}]
!          The array of coordinates associated with point to add.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ESMF_INIT_CHECK_DEEP(ESMF_PointListGetInit,pointlist,rc)

    call c_ESMC_PointListAdd(pointlist, id, loc_coords, localrc)
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_PointListAdd
!------------------------------------------------------------------------------



!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_PointListPrint"
!BOPI
! !IROUTINE: ESMF_PointListPrint - Print the contents of a PointList

! !INTERFACE:
  subroutine ESMF_PointListPrint(pointlist, rc)
!
! !ARGUMENTS:
    type(ESMF_PointList),     intent(in)            :: pointlist
    integer,                intent(out), optional :: rc
!
! !DESCRIPTION:
!   Print information about an {\tt ESMF\_PointList}.
!
!   The arguments are:
!   \begin{description}
!   \item[pointlist]
!     {\tt ESMF\_PointList} to print contents of.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ESMF_INIT_CHECK_DEEP(ESMF_PointListGetInit,pointlist,rc)

    call ESMF_UtilIOUnitFlush (ESMF_UtilIOStdout, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call c_ESMC_PointListPrint(pointlist, localrc)
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Set return values
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_PointListPrint


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_PointListWriteVTK"
!BOPI
! !IROUTINE: ESMF_PointListWriteVTK - Print the contents of a PointList to a VTK file

! !INTERFACE:
  subroutine ESMF_PointListWriteVTK(pointlist, filename, rc)
!
! !ARGUMENTS:
    type(ESMF_PointList), intent(in)            :: pointlist
    character(*),         intent(in)            :: filename
    integer,              intent(out), optional :: rc
!
! !DESCRIPTION:
!   Print information about an {\tt ESMF\_PointList} into a VTK file.
!
!   The arguments are:
!   \begin{description}
!   \item[pointlist]
!     {\tt ESMF\_PointList} to print contents of.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ESMF_INIT_CHECK_DEEP(ESMF_PointListGetInit,pointlist,rc)

    call ESMF_UtilIOUnitFlush (ESMF_UtilIOStdout, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call c_ESMC_PointListWriteVTK(pointlist, filename, localrc)
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Set return values
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_PointListWriteVTK
!------------------------------------------------------------------------------

end module ESMF_PointListMod
