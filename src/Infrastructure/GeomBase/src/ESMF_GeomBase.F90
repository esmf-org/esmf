! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2016, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
!
#define ESMF_FILENAME "ESMF_GeomBase.F90"
!
!     ESMF GeomBase Module
      module ESMF_GeomBaseMod
!
!==============================================================================
!
! This file contains the GeomBase class definition.
!
!------------------------------------------------------------------------------
! INCLUDES
#include "ESMF.h"
!==============================================================================
!BOPI
! !MODULE: ESMF_GeomBaseMod - GeomBase class
!
! !DESCRIPTION:
!
! The code in this file implements the {\tt ESMF\_GeomBase} class.  
!
!------------------------------------------------------------------------------
! !USES:
      use ESMF_UtilTypesMod   
      use ESMF_InitMacrosMod    ! ESMF initializer macros
      use ESMF_LogErrMod        ! ESMF error handling
      use ESMF_StaggerLocMod
      use ESMF_DistGridMod
      use ESMF_GridMod
      use ESMF_MeshMod
      use ESMF_LocStreamMod
      use ESMF_XGridMod
      use ESMF_XGridGeomBaseMod
      use ESMF_XGridGetMod

!     NEED TO ADD MORE HERE
      implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
      private


!------------------------------------------------------------------------------
! ! ESMF_GeomType_Flag
!
!------------------------------------------------------------------------------
  type ESMF_GeomType_Flag
#ifndef ESMF_NO_SEQUENCE
  sequence
#endif
     integer :: type
  end type

  type(ESMF_GeomType_Flag), parameter :: &
                      ESMF_GEOMTYPE_INVALID=ESMF_GeomType_Flag(-1), &
                      ESMF_GEOMTYPE_UNINIT=ESMF_GeomType_Flag(0), &
                      ESMF_GEOMTYPE_GRID=ESMF_GeomType_Flag(1), &
                      ESMF_GEOMTYPE_MESH=ESMF_GeomType_Flag(2), &
                      ESMF_GEOMTYPE_LOCSTREAM=ESMF_GeomType_Flag(3), &
                      ESMF_GEOMTYPE_XGRID=ESMF_GeomType_Flag(4)

!------------------------------------------------------------------------------
! ! ESMF_GeomBaseClass
!
!------------------------------------------------------------------------------

  ! F90 class type to hold pointer to object
  type ESMF_GeomBaseClass
#ifndef ESMF_NO_SEQUENCE
  sequence
#endif
    type(ESMF_GeomType_Flag) :: type
    type(ESMF_StaggerLoc) :: staggerloc
    type(ESMF_Grid) :: grid
    type(ESMF_MeshLoc) :: meshloc ! either nodes or elements
    type(ESMF_Mesh) :: mesh
    type(ESMF_LocStream) :: locstream
    type(ESMF_XGrid) :: xgrid
    type(ESMF_XGridSide_Flag) :: xgridside
    integer :: xgridIndex 

  end type


!------------------------------------------------------------------------------
! ! ESMF_GeomBase
!
!------------------------------------------------------------------------------

  ! F90 class type to hold pointer to C++ object
  type ESMF_GeomBase
#ifndef ESMF_NO_SEQUENCE
  sequence
#endif
    
    type(ESMF_GeomBaseClass),pointer :: gbcp

    ESMF_INIT_DECLARE
  end type

!------------------------------------------------------------------------------
!
! !PUBLIC TYPES:
!
public ESMF_GeomBase
public ESMF_GeomBaseClass ! for internal use only
public ESMF_GeomType_Flag,  ESMF_GEOMTYPE_INVALID, ESMF_GEOMTYPE_UNINIT, &
                       ESMF_GEOMTYPE_GRID,  ESMF_GEOMTYPE_MESH, &
                       ESMF_GEOMTYPE_LOCSTREAM, ESMF_GEOMTYPE_XGRID

!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:
!
!
! - ESMF-public methods:

  public operator(==)
  public operator(/=)

  public ESMF_GeomBaseCreate
  public ESMF_GeomBaseDestroy

  public ESMF_GeomBaseGet
  public ESMF_GeomBaseGetPlocalDE

  public ESMF_GeomBaseSerialize
  public ESMF_GeomBaseDeserialize

  public ESMF_GeomBaseValidate

  public ESMF_GeomBaseGetArrayInfo


!  public ESMF_GeomBaseGetMesh

! - ESMF-internal methods:
  public ESMF_GeomBaseGetInit  

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



! -------------------------- ESMF-public method -------------------------------
!BOPI
! !IROUTINE: ESMF_GeomBaseCreate -- Generic interface

! !INTERFACE:
interface ESMF_GeomBaseCreate

! !PRIVATE MEMBER FUNCTIONS:
!
      module procedure ESMF_GeomBaseCreateGrid
      module procedure ESMF_GeomBaseCreateMesh
      module procedure ESMF_GeomBaseCreateLocStream
      module procedure ESMF_GeomBaseCreateXGrid
      
! !DESCRIPTION: 
! This interface provides a single entry point for the various 
!  types of {\tt ESMF\_GeomBaseCreate} functions.   
!EOPI 
end interface



!==============================================================================
!BOPI
! !INTERFACE:
      interface operator (==)

! !PRIVATE MEMBER FUNCTIONS:
         module procedure ESMF_GeomTypeEqual

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
         module procedure ESMF_GeomTypeNotEqual

! !DESCRIPTION:
!     This interface overloads the inequality operator for the specific
!     ESMF GridConn.  It is provided for easy comparisons of 
!     these types with defined values.
!
!EOPI
      end interface


!==============================================================================

      contains

!==============================================================================


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GeomBaseGetArrayInfo"

!BOPI
! !IROUTINE: ESMF_GeomBaseGetArrayInfo" - get information to make an Array from a GeomBase

! !INTERFACE:
      subroutine ESMF_GeomBaseGetArrayInfo(geombase, &
                           gridToFieldMap, ungriddedLBound, ungriddedUBound, &
                           distgrid, distgridToArrayMap, undistLBound, undistUBound,   &
                           rc)
!
! !ARGUMENTS:
       type(ESMF_GeomBase),   intent(in)            :: geombase
       integer,               intent(in),  optional :: gridToFieldMap(:)
       integer,               intent(in),  optional :: ungriddedLBound(:)
       integer,               intent(in),  optional :: ungriddedUBound(:)
       type(ESMF_DistGrid),   intent(out), optional :: distgrid
       integer,               intent(out)           :: distgridToArrayMap(:)
       integer,               intent(out), optional :: undistLBound(:)
       integer,               intent(out), optional :: undistUBound(:)
       integer,               intent(out), optional :: rc

!
! !DESCRIPTION:
!  
! This subroutine gets information from a GeomBase which is useful in creating an
! Array. This subroutine takes as input {\tt gridToFieldMap} which gives for each
! geombase object dimension which dimension in the eventual Array it should be
! mapped to. It also takes {\tt ungriddedLBound} and {\tt ungriddedUBound} which
! describes the dimensions of the Array not associated with the geombase object. 
! From these it produces a mapping from the distgrid to the Array, the undistributed
! bounds of the Array in the correct order. (For everything besides {\tt Grid}  the 
! gridToFieldMap and distgridToArrayMap will be single element
! arrays describing which dimension in the Array the geombase object (e.g. Mesh)
! is mapped to.
!
! The arguments are:
! \begin{description}
!\item[{geombase}]
!     The geombase to get the information from to create the Array.
!\item[{[distgrid]}]
!     The distgrid to create the Array on
!\item[{[gridToFieldMap]}]
!     Indicates where each grid dimension goes in the newly created Array.
!     {\tt The array gridToFieldMap} should be at least of size equal to the geombase object's
!     Array dimension (e.g. Mesh = 1)
!     If not set defaults to (1,2,3,....,geombase objects dim).
!\item[{[ungriddedLBound]}]
!     The lower bounds of the non-grid Array dimensions.
!\item[{[ungriddedUBound]}]
!     The upper bounds of the non-grid array dimensions.  
!\item[{distgridToArrayMap}]
!     The distgrid to Array dimension map (must be allocated to at least
!     the number of dimensions of the distGrid).
!\item[{undistLBound}]
!     Undistributed lower bounds (must be of size grid undistDimCount+size(ungriddedUBound))
!\item[{undistUBound}]
!     Undistributed upper bounds (must be of size grid undistDimCount+size(ungriddedUBound))
! \item[{[rc]}]
!      Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOPI
    integer :: localrc
    type(ESMF_GeomBaseClass),pointer :: gbcp

    ! Initialize return code; assume failure until success is certain
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP_SHORT(ESMF_GeomBaseGetInit, geombase, rc)

    ! Get GeomBaseClass
    gbcp=>geombase%gbcp

    ! Get info depending on type
    select case(gbcp%type%type)
       case (ESMF_GEOMTYPE_GRID%type) ! Grid 
         call ESMF_GridGetArrayInfo(gbcp%grid, gbcp%staggerloc, &
                           gridToFieldMap, ungriddedLBound, ungriddedUBound,  &
                           distgrid, distgridToArrayMap, undistLBound, undistUBound,       &
                           rc=localrc)
         if (ESMF_LogFoundError(localrc, &
                                 ESMF_ERR_PASSTHRU, &
                                 ESMF_CONTEXT, rcToReturn=rc)) return

       case (ESMF_GEOMTYPE_MESH%type) ! Mesh
          if (present(gridToFieldMap)) then
            distgridToArrayMap = gridToFieldMap
          else
            distgridToArrayMap = 1
          endif

          if (present(ungriddedLBound) .and. present (undistLBound)) then
              if (size(ungriddedLBound) .gt. 0) undistLBound = ungriddedLBound
          endif
          if (present(ungriddedUBound) .and. present (undistUBound)) then
              if (size(ungriddedUBound) .gt. 0) undistUBound = ungriddedUBound
          endif

           ! Distgrid
	   if (present(distgrid)) then
	    if (gbcp%meshloc == ESMF_MESHLOC_NODE) then
               call ESMF_MeshGet(mesh=gbcp%mesh, &
                                 nodalDistgrid=distgrid, &
                                  rc=localrc)
               if (ESMF_LogFoundError(localrc, &
                                 ESMF_ERR_PASSTHRU, &
                                 ESMF_CONTEXT, rcToReturn=rc)) return
	    else if (gbcp%meshloc == ESMF_MESHLOC_ELEMENT) then
               call ESMF_MeshGet(mesh=gbcp%mesh, &
                                 elementDistgrid=distgrid, &
                                  rc=localrc)
               if (ESMF_LogFoundError(localrc, &
                                 ESMF_ERR_PASSTHRU, &
                                 ESMF_CONTEXT, rcToReturn=rc)) return
            else
              if (ESMF_LogFoundError(ESMF_RC_ARG_VALUE, &
                               msg=" Bad Mesh Location value", &
                               ESMF_CONTEXT, rcToReturn=rc)) return
            endif
           endif


       case (ESMF_GEOMTYPE_LOCSTREAM%type) ! LocStream
          if (present(gridToFieldMap)) then
             distgridToArrayMap = gridToFieldMap
          else
             distgridToArrayMap = 1
          endif

          if (present(ungriddedLBound) .and. present (undistLBound)) then
              if (size(ungriddedLBound) .gt. 0) undistLBound = ungriddedLBound
          endif
          if (present(ungriddedUBound) .and. present (undistUBound)) then
              if (size(ungriddedUBound) .gt. 0) undistUBound = ungriddedUBound
          endif

          ! Get distgrid
	  if (present(distgrid)) then
               call ESMF_LocStreamGet(gbcp%locstream, distgrid=distgrid, &
                       rc=localrc)
                if (ESMF_LogFoundError(localrc, &
                                 ESMF_ERR_PASSTHRU, &
                                 ESMF_CONTEXT, rcToReturn=rc)) return
           endif

       case (ESMF_GEOMTYPE_XGRID%type) ! Xgrid
          if (present(gridToFieldMap)) then
             distgridToArrayMap = gridToFieldMap
          else
             distgridToArrayMap = 1
          endif

          if (present(ungriddedLBound) .and. present (undistLBound)) then
              if (size(ungriddedLBound) .gt. 0) undistLBound = ungriddedLBound
          endif
          if (present(ungriddedUBound) .and. present (undistUBound)) then
              if (size(ungriddedUBound) .gt. 0) undistUBound = ungriddedUBound
          endif

          ! Get distgrid
	  if (present(distgrid)) then
              call ESMF_XGridGet(gbcp%xgrid, gridIndex=gbcp%xgridindex, &
                                 xgridSide=gbcp%xgridSide, &
                                 distgrid=distgrid, rc=localrc)
              if (ESMF_LogFoundError(localrc, &
                                 ESMF_ERR_PASSTHRU, &
                                 ESMF_CONTEXT, rcToReturn=rc)) return
          endif

       case default
         if (ESMF_LogFoundError(ESMF_RC_ARG_VALUE, &
                               msg=" Bad type value", &
                               ESMF_CONTEXT, rcToReturn=rc)) return
    end select


    ! Set return value
    if (present(rc)) rc = ESMF_SUCCESS

    end  subroutine ESMF_GeomBaseGetArrayInfo

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GeomBaseCreate"
!BOPI
! !IROUTINE: ESMF_GeomBaseCreate - Create a GeomBase from a Grid

! !INTERFACE:
  ! Private name; call using ESMF_GeomBaseCreate()
      function ESMF_GeomBaseCreateGrid(grid,staggerloc, rc)
!
! !RETURN VALUE:
      type(ESMF_GeomBase) :: ESMF_GeomBaseCreateGrid
!
! !ARGUMENTS:
       type(ESMF_Grid),       intent(in)              :: grid
       type(ESMF_StaggerLoc), intent(in)              :: staggerloc
       integer,               intent(out),  optional  :: rc
!
! !DESCRIPTION:
! Create an {\tt ESMF\_GeomBase} object from an {\tt ESMF\_Grid} object. 
! This will be overloaded for each type of object a GeomBase can represent. 
!
! The arguments are:
! \begin{description}
! \item[grid]
!      {\tt ESMF\_Grid} object to create the Grid Base from.
! \item[staggerloc] 
!       StaggerLoc on the Grid to create the Grid Base object on. 
! \item[{[rc]}]
!      Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOPI
    type(ESMF_GeomBaseClass),pointer :: gbcp
    integer :: localrc ! local error status

    ! Initialize return code; assume failure until success is certain
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP_SHORT(ESMF_GridGetInit, grid, rc)

    ! initialize pointers
    nullify(gbcp)
    nullify( ESMF_GeomBaseCreateGrid%gbcp)

    ! allocate GeomBase type
    allocate(gbcp, stat=localrc)
    if (ESMF_LogFoundAllocError(localrc, msg="Allocating GeomBase type object", &
                                     ESMF_CONTEXT, rcToReturn=rc)) return

    ! Set values in GeomBase
    gbcp%type = ESMF_GEOMTYPE_GRID
    gbcp%grid = grid
    gbcp%staggerloc = staggerloc

    ! Set GeomBase Type into GeomBase
     ESMF_GeomBaseCreateGrid%gbcp=>gbcp

    ! Add reference to this object into ESMF garbage collection table
    call c_ESMC_VMAddFObject(ESMF_GeomBaseCreateGrid, ESMF_ID_GEOMBASE%objectID)
      
    ! Set init status
    ESMF_INIT_SET_CREATED(ESMF_GeomBaseCreateGrid)

    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS

    end function ESMF_GeomBaseCreateGrid

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GeomBaseCreate"
!BOPI
! !IROUTINE: ESMF_GeomBaseCreate - Create a GeomBase from a Mesh

! !INTERFACE:
  ! Private name; call using ESMF_GeomBaseCreate()
      function ESMF_GeomBaseCreateMesh(mesh, loc, rc)
!
! !RETURN VALUE:
      type(ESMF_GeomBase) :: ESMF_GeomBaseCreateMesh
!
! !ARGUMENTS:
       type(ESMF_Mesh),       intent(in)              :: mesh
       type(ESMF_MeshLoc),    intent(in), optional    :: loc
       integer,               intent(out),  optional  :: rc
!
! !DESCRIPTION:
! Create an {\tt ESMF\_GeomBase} object from an {\tt ESMF\_Mesh} object. 
! This will be overloaded for each type of object a GeomBase can represent. 
!
! The arguments are:
! \begin{description}
! \item[mesh]
!      {\tt ESMF\_Mesh} object to create the Mesh Base from.
! \item[{[rc]}]
!      Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOPI
    type(ESMF_GeomBaseClass),pointer :: gbcp
    integer :: localrc ! local error status
    type(ESMF_MeshLoc) :: localLoc

    ! Initialize return code; assume failure until success is certain
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP_SHORT(ESMF_MeshGetInit, mesh, rc)

    ! initialize pointers
    nullify(gbcp)
    nullify( ESMF_GeomBaseCreateMesh%gbcp)

    ! allocate GeomBase type
    allocate(gbcp, stat=localrc)
    if (ESMF_LogFoundAllocError(localrc, msg="Allocating GeomBase type object", &
                                     ESMF_CONTEXT, rcToReturn=rc)) return

    ! Set default
    if (present(loc)) then
       localLoc=loc
    else
       localLoc=ESMF_MESHLOC_NODE
    endif

    ! Set values in GeomBase
    gbcp%type = ESMF_GEOMTYPE_MESH
    gbcp%mesh = mesh
    gbcp%meshloc = localLoc

    ! Set GeomBase Type into GeomBase
     ESMF_GeomBaseCreateMesh%gbcp=>gbcp

    ! Add reference to this object into ESMF garbage collection table
    call c_ESMC_VMAddFObject(ESMF_GeomBaseCreateMesh, ESMF_ID_GEOMBASE%objectID)
      
    ! Set init status
    ESMF_INIT_SET_CREATED(ESMF_GeomBaseCreateMesh)

    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS

    end function ESMF_GeomBaseCreateMesh


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GeomBaseCreate"
!BOPI
! !IROUTINE: ESMF_GeomBaseCreate - Create a GeomBase from a LocStream

! !INTERFACE:
  ! Private name; call using ESMF_GeomBaseCreate()
      function ESMF_GeomBaseCreateLocStream(locstream, rc)
!
! !RETURN VALUE:
      type(ESMF_GeomBase) :: ESMF_GeomBaseCreateLocStream
!
! !ARGUMENTS:
       type(ESMF_LocStream),  intent(in)              :: locstream
       integer,               intent(out),  optional  :: rc
!
! !DESCRIPTION:
! Create an {\tt ESMF\_GeomBase} object from an {\tt ESMF\_LocStream} object. 
! This will be overloaded for each type of object a GeomBase can represent. 
!
! The arguments are:
! \begin{description}
! \item[locstream]
!      {\tt ESMF\_LocStream} object to create the GeomBase from.
! \item[{[rc]}]
!      Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOPI
    type(ESMF_GeomBaseClass),pointer :: gbcp
    integer :: localrc ! local error status

    ! Initialize return code; assume failure until success is certain
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP_SHORT(ESMF_LocStreamGetInit, locstream, rc)

    ! initialize pointers
    nullify(gbcp)
    nullify( ESMF_GeomBaseCreateLocStream%gbcp)

    ! allocate GeomBase type
    allocate(gbcp, stat=localrc)
    if (ESMF_LogFoundAllocError(localrc, msg="Allocating GeomBase type object", &
                                     ESMF_CONTEXT, rcToReturn=rc)) return

    ! Set values in GeomBase
    gbcp%type = ESMF_GEOMTYPE_LOCSTREAM
    gbcp%locstream = locstream

    ! Set GeomBase Type into GeomBase
     ESMF_GeomBaseCreateLocStream%gbcp=>gbcp

    ! Add reference to this object into ESMF garbage collection table
    call c_ESMC_VMAddFObject(ESMF_GeomBaseCreateLocStream, ESMF_ID_GEOMBASE%objectID)
    
    ! Set init status
    ESMF_INIT_SET_CREATED(ESMF_GeomBaseCreateLocStream)

    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS

    end function ESMF_GeomBaseCreateLocStream



!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GeomBaseCreate"
!BOPI
! !IROUTINE: ESMF_GeomBaseCreate - Create a GeomBase from an XGrid

! !INTERFACE:
  ! Private name; call using ESMF_GeomBaseCreate()
      function ESMF_GeomBaseCreateXGrid(xgrid, xgridside, gridIndex, rc)
!
! !RETURN VALUE:
      type(ESMF_GeomBase) :: ESMF_GeomBaseCreateXGrid
!
! !ARGUMENTS:
       type(ESMF_XGrid),     intent(in)            :: xgrid
       type(ESMF_XGridSide_Flag), intent(in), optional  :: XGridSide
       integer,              intent(in), optional  :: gridIndex
       integer,              intent(out),optional  :: rc
!
! !DESCRIPTION:
! Create an {\tt ESMF\_GeomBase} object from an {\tt ESMF\_XGrid} object. 
! This will be overloaded for each type of object a GeomBase can represent. 
!
! The arguments are:
! \begin{description}
! \item[xgrid]
!      {\tt ESMF\_XGrid} object to create the GeomBase from.
! \item[{[XGridSide]}]
!      Which side to create the Field on. 
! \item[{[gridIndex]}]
!      Index to specify which distgrid when on side A or side B
! \item[{[rc]}]
!      Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOPI
    type(ESMF_GeomBaseClass),pointer :: gbcp
    integer :: localrc ! local error status
    integer :: localGridIndex
    type(ESMF_XGridSide_Flag) :: localXGridSide

    ! Initialize return code; assume failure until success is certain
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP_SHORT(ESMF_XGridGetInit, xgrid, rc)

    ! Set defaults
    if (present(XGridSide)) then
       localXGridSide=xgridSide
    else
       localXGridSide=ESMF_XGRIDSIDE_BALANCED
    endif

    if (present(gridIndex)) then
       localGridIndex=gridIndex
    else
       localGridIndex=1
    endif

    ! initialize pointers
    nullify(gbcp)
    nullify( ESMF_GeomBaseCreateXGrid%gbcp)

    ! allocate GeomBase type
    allocate(gbcp, stat=localrc)
    if (ESMF_LogFoundAllocError(localrc, msg="Allocating GeomBase type object", &
                                     ESMF_CONTEXT, rcToReturn=rc)) return

    ! Set values in GeomBase
    gbcp%type = ESMF_GEOMTYPE_XGRID
    gbcp%xgrid = xgrid
    gbcp%xgridside = localXGridSide
    gbcp%xgridIndex = localGridIndex

    ! Set GeomBase Type into GeomBase
    ESMF_GeomBaseCreateXGrid%gbcp=>gbcp

    ! Add reference to this object into ESMF garbage collection table
    call c_ESMC_VMAddFObject(ESMF_GeomBaseCreateXGrid, ESMF_ID_GEOMBASE%objectID)
    
    ! Set init status
    ESMF_INIT_SET_CREATED(ESMF_GeomBaseCreateXGrid)

    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS

    end function ESMF_GeomBaseCreateXGrid


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GeomBaseDestroy"
!BOPI
! !IROUTINE: ESMF_GeomBaseDestroy - Release resources associated with a GeomBase

! !INTERFACE:
      subroutine ESMF_GeomBaseDestroy(geombase, noGarbage, rc)
!
! !ARGUMENTS:
      type(ESMF_GeomBase)            :: geombase
      logical, intent(in),  optional :: noGarbage
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!   Destroys an {\tt ESMF\_GeomBase} object. This call does not destroy wrapped
!   Grid, LocStream, or other Grid objects. 
!
!     The arguments are:
!     \begin{description}
!     \item[geombase]
!          {\tt ESMF\_GeomBase} to be destroyed.
! \item[{[noGarbage]}]
!      If set to {\tt .TRUE.} the object will be fully destroyed and removed
!      from the ESMF garbage collection system. Note however that under this 
!      condition ESMF cannot protect against accessing the destroyed object 
!      through dangling aliases -- a situation which may lead to hard to debug 
!      application crashes.
! 
!      It is generally recommended to leave the {\tt noGarbage} argument
!      set to {\tt .FALSE.} (the default), and to take advantage of the ESMF 
!      garbage collection system which will prevent problems with dangling
!      aliases or incorrect sequences of destroy calls. However this level of
!      support requires that a small remnant of the object is kept in memory
!      past the destroy call. This can lead to an unexpected increase in memory
!      consumption over the course of execution in applications that use 
!      temporary ESMF objects. For situations where the repeated creation and 
!      destruction of temporary objects leads to memory issues, it is 
!      recommended to call with {\tt noGarbage} set to {\tt .TRUE.}, fully 
!      removing the entire temporary object from memory.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI

    ! Initialize return code; assume failure until success is certain
    integer                 :: localrc ! local error status
    logical                 :: opt_noGarbage  ! helper variable
    
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_GeomBaseGetInit, geombase, rc)

    ! Set default flags
    opt_noGarbage = .false.
    if (present(noGarbage)) opt_noGarbage = noGarbage

    ! Conditionally remove object from memory
    if (opt_noGarbage) then
      ! remove reference to this object from ESMF garbage collection table
      call c_ESMC_VMRmFObject(geombase)
      ! deallocate GeomBase type
      deallocate(geombase%gbcp, stat=localrc)
      if (ESMF_LogFoundDeallocError(localrc, &
        msg="Deallocating GeomBase type object", &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    ! Set init code
    ESMF_INIT_SET_DELETED(geombase)

    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS

 end subroutine ESMF_GeomBaseDestroy


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GeomBaseGet"
!BOPI
! !IROUTINE: ESMF_GeomBaseGet - Get information about a Grid

! !INTERFACE:
      subroutine ESMF_GeomBaseGet(geombase, &
          dimCount, localDECount, distgrid, &
          distgridToGridMap, indexFlag, geomtype, &
          grid, staggerloc, mesh, meshloc, locstream, &
          xgrid, xgridside, gridIndex,rc)
!
! !ARGUMENTS:
      type(ESMF_GeomBase),   intent(in)            :: geombase
      integer,               intent(out), optional :: dimCount
      integer,               intent(out), optional :: localDECount
      type(ESMF_DistGrid),   intent(out), optional :: distgrid
      integer,               intent(out), optional :: distgridToGridMap(:)
      type(ESMF_Index_Flag),  intent(out), optional :: indexflag
      type(ESMF_GeomType_Flag),   intent(out), optional :: geomtype
      type(ESMF_Grid),       intent(out), optional :: grid      
      type(ESMF_StaggerLoc), intent(out), optional :: staggerloc
      type(ESMF_Mesh),       intent(out), optional :: mesh      
      type(ESMF_MeshLoc),    intent(out), optional :: meshloc
      type(ESMF_LocStream),  intent(out), optional :: locstream
      type(ESMF_XGrid),      intent(out), optional :: xgrid
      type(ESMF_XGridSide_Flag),  intent(out), optional :: xgridside
      integer,               intent(out), optional :: gridIndex
      integer,               intent(out), optional :: rc

!
! !DESCRIPTION:
!    Gets various types of information about a grid. 
!
!The arguments are:
!\begin{description}
!\item[{geombase}]
!   GeomBase to get the information from.
!\item[{[dimCount]}]
!   DimCount of the GeomBase object (e.g. the number of dimensions in the Array
!   it will be mapped to (e.g. Mesh=1)). 
!\item[{[localDECount]}]
!   The number of DEs in this grid on this PET.
!\item[{[distgrid]}]
!   The structure describing the distribution of the grid. 
!\item[{[distgridToGridMap]}]
!   List that has as many elements as the distgrid dimCount. This array describes
!   mapping between the grids dimensions and the distgrid.
! \item[{[indexflag]}]
!    Flag that indicates how the DE-local indices are to be defined.
! \item[{[geomtype]}]
!    Flag that indicates what type of object this geombase holds. 
!    Can be {\tt ESMF_GEOMTYPE_GRID}, {\tt ESMF_GEOMTYPE_MESH},...
! \item[{[grid]}]
!    The Grid object that this geombase object holds. 
! \item[{[staggerloc]}]
!    The Grid stagger location.
! \item[{[mesh]}]
!    The Mesh object that this geombase object holds. 
! \item[{[meshloc]}]
!    The part of the mesh that the field is on
! \item[{[locstream]}]
!    The LocStream object that this geombase object holds. 
! \item[{[XGridSide]}]
!      The xgrid side that the Fiels was created on. 
! \item[{[gridIndex]}]
!      The Xgrid index to specify which distgrid the Field was created on when on side A or side B.
!\item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!\end{description}
!
!EOPI
    type(ESMF_GeomBaseClass),pointer :: gbcp
    type(ESMF_XGridGeomBase)         :: xgrid_geombase
    integer                          :: localrc

    ! Initialize return code; assume failure until success is certain
    if (present(rc)) rc = ESMF_RC_NOT_IMPL


    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP_SHORT(ESMF_GeomBaseGetInit, geombase, rc)

    ! Get GeomBaseClass
    gbcp=>geombase%gbcp

    ! get type
    if (present(geomtype)) then
       geomtype=gbcp%type
    endif

    ! Get grid object plus error checking
    if (present(grid)) then
       if (gbcp%type==ESMF_GEOMTYPE_GRID) then
          grid=gbcp%grid
       else
          if (ESMF_LogFoundError(ESMF_RC_ARG_VALUE, &
                               msg=" Grid not geometry type", &
                               ESMF_CONTEXT, rcToReturn=rc)) return
       endif
    endif

    ! Get mesh object plus error checking
    if (present(mesh)) then
       if (gbcp%type==ESMF_GEOMTYPE_Mesh) then
          mesh=gbcp%mesh
       else
          if (ESMF_LogFoundError(ESMF_RC_ARG_VALUE, &
                               msg=" Mesh not geometry type", &
                               ESMF_CONTEXT, rcToReturn=rc)) return
       endif
    endif

    ! Get locstream object plus error checking
    if (present(locstream)) then
       if (gbcp%type==ESMF_GEOMTYPE_LOCSTREAM) then
          locstream=gbcp%locstream
       else
          if (ESMF_LogFoundError(ESMF_RC_ARG_VALUE, &
                               msg=" LocStream not geometry type", &
                               ESMF_CONTEXT, rcToReturn=rc)) return
       endif
    endif


    ! Get xgrid object plus error checking
    if (present(xgrid)) then
       if (gbcp%type==ESMF_GEOMTYPE_XGRID) then
          xgrid=gbcp%xgrid
       else
          if (ESMF_LogFoundError(ESMF_RC_ARG_VALUE, &
                               msg=" XGrid not geometry type", &
                               ESMF_CONTEXT, rcToReturn=rc)) return
       endif
    endif


    ! Get objects plus error checking
    if (present(staggerloc)) then
       if (gbcp%type==ESMF_GEOMTYPE_GRID) then
          staggerloc=gbcp%staggerloc
       else
          if (ESMF_LogFoundError(ESMF_RC_ARG_VALUE, &
                               msg=" Grid not geometry type", &
                               ESMF_CONTEXT, rcToReturn=rc)) return
       endif
    endif

    ! Get objects plus error checking
    if (present(meshloc)) then
       if (gbcp%type==ESMF_GEOMTYPE_MESH) then
          meshloc=gbcp%meshloc
       else
          if (ESMF_LogFoundError(ESMF_RC_ARG_VALUE, &
                               msg=" Grid not geometry type", &
                               ESMF_CONTEXT, rcToReturn=rc)) return
       endif
    endif




    ! Get info depending on type
    select case(gbcp%type%type)
       case (ESMF_GEOMTYPE_GRID%type) ! Grid 
            call ESMF_GridGet(grid=gbcp%grid,  &
                      dimCount=dimCount,  localDECount=localDECount, &
                      distgridToGridMap=distgridToGridMap, &
                      indexflag=indexFlag, rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                                 ESMF_ERR_PASSTHRU, &
                                 ESMF_CONTEXT, rcToReturn=rc)) return
            call ESMF_GridGet(grid=gbcp%grid, staggerloc=gbcp%staggerloc, &
                      distgrid=distgrid, rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                                 ESMF_ERR_PASSTHRU, &
                                 ESMF_CONTEXT, rcToReturn=rc)) return

       case (ESMF_GEOMTYPE_MESH%type) ! Mesh
            if (present(dimCount)) dimCount = 1
            if (present(localDECount)) localDECount = 1
            if (present(distgridToGridMap)) distgridToGridMap = 1
            ! Distgrid
 	    if (present(distgrid)) then
	       if (gbcp%meshloc == ESMF_MESHLOC_NODE) then
                  call ESMF_MeshGet(mesh=gbcp%mesh, &
                                    nodalDistgrid=distgrid, &
                                    rc=localrc)
                  if (ESMF_LogFoundError(localrc, &
                                   ESMF_ERR_PASSTHRU, &
                                   ESMF_CONTEXT, rcToReturn=rc)) return
	       else if (gbcp%meshloc == ESMF_MESHLOC_ELEMENT) then
                  call ESMF_MeshGet(mesh=gbcp%mesh, &
                                 elementDistgrid=distgrid, &
                                  rc=localrc)
                  if (ESMF_LogFoundError(localrc, &
                                 ESMF_ERR_PASSTHRU, &
                                 ESMF_CONTEXT, rcToReturn=rc)) return
               else
                  if (ESMF_LogFoundError(ESMF_RC_ARG_VALUE, &
                               msg=" Bad Mesh Location value", &
                               ESMF_CONTEXT, rcToReturn=rc)) return
               endif
            endif
            if (present(indexFlag)) indexFlag = ESMF_INDEX_DELOCAL



       case (ESMF_GEOMTYPE_LOCSTREAM%type) ! LocStream
            if (present(dimCount)) dimCount = 1
            if (present(distgridToGridMap)) distgridToGridMap = 1
            call ESMF_LocStreamGet(gbcp%locstream, distgrid=distgrid, &
                   localDECount=localDECount, indexflag=indexflag, rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                                 ESMF_ERR_PASSTHRU, &
                                 ESMF_CONTEXT, rcToReturn=rc)) return

       case (ESMF_GEOMTYPE_XGRID%type) ! XGrid

         if(gbcp%xgridside == ESMF_XGRIDSIDE_BALANCED) then
           if (present(dimCount)) dimCount = 1
           if (present(distgridToGridMap)) distgridToGridMap = 1
           if (present(localDECount)) then
               call ESMF_XGridGet(gbcp%xgrid, localDECount=localDECount, rc=localrc)
               if (ESMF_LogFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rcToReturn=rc)) return
           endif
         else
           call ESMF_XGridGet(gbcp%xgrid, xgrid_geombase, xgridSide=gbcp%xgridSide, &
                              gridindex=gbcp%xgridindex, rc=localrc)
           if (ESMF_LogFoundError(localrc, &
                            ESMF_ERR_PASSTHRU, &
                            ESMF_CONTEXT, rcToReturn=rc)) return
           call ESMF_XGridGeomBaseGet(xgrid_geombase, &
              dimCount=dimCount, localDeCount=localDECount, distgrid=distgrid, &
              distgridToGridMap=distgridToGridMap, indexFlag=indexFlag, &
              grid=grid, staggerloc=staggerloc, mesh=mesh, meshloc=meshloc, &
              rc=localrc)
           if (ESMF_LogFoundError(localrc, &
              ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return
         endif

         ! Get distgrid
         if (present(distgrid)) then
             call ESMF_XGridGet(gbcp%xgrid, gridIndex=gbcp%xgridindex, &
                                xgridSide=gbcp%xgridSide, &
                                distgrid=distgrid, rc=localrc)
             if (ESMF_LogFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rcToReturn=rc)) return
         endif
         if (present(indexFlag)) indexFlag = ESMF_INDEX_DELOCAL
         if (present(xgridside)) xgridside=gbcp%xgridside
         if (present(gridIndex)) gridIndex=gbcp%xgridIndex
             
       case default
         if (ESMF_LogFoundError(ESMF_RC_ARG_VALUE, &
                               msg=" Bad type value", &
                               ESMF_CONTEXT, rcToReturn=rc)) return
    end select


    ! Set return value
    if (present(rc)) rc = ESMF_SUCCESS

end subroutine ESMF_GeomBaseGet

!------------------------------------------------------------------------------
#undef  ESMF_METHOD  
#define ESMF_METHOD "ESMF_GeomBaseGetPLocalDe"
!BOPI
! !IROUTINE: ESMF_GeomBaseGetPLocalDE - Get information about a particular DE 

! !INTERFACE:
      subroutine ESMF_GeomBaseGetPLocalDe(geombase, localDe, &
          exclusiveLBound, exclusiveUBound, exclusiveCount,  rc)

!
! !ARGUMENTS:
      type(ESMF_GeomBase),        intent(in)            :: geombase
      integer,                intent(in)            :: localDe
       integer,                intent(out), optional :: exclusiveLBound(:)
      integer,                intent(out), optional :: exclusiveUBound(:)
      integer,                intent(out), optional :: exclusiveCount(:)
      integer,                intent(out), optional :: rc
!
! !DESCRIPTION:
!  This method gets information about the range of the index space which a 
!  localDe occupies.
!
!The arguments are:
!\begin{description}
!\item[{geombase}]
!    Grid Base to get the information from.
!\item[{[localDe]}]
!     The local DE from which to get the information.  
!\item[{[exclusiveLBound]}]
!     Upon return this holds the lower bounds of the exclusive region.
!     {\tt exclusiveLBound} must be allocated to be of size equal to the Grid dimCount.
!     Please see Section~\ref{sec:grid:usage:bounds} for a description
!     of the regions and their associated bounds and counts. 
!\item[{[exclusiveUBound]}]
!     Upon return this holds the upper bounds of the exclusive region.
!     {\tt exclusiveUBound} must be allocated to be of size equal to the Grid dimCount.
!     Please see Section~\ref{sec:grid:usage:bounds} for a description
!     of the regions and their associated bounds and counts. 
!\item[{[exclusiveCount]}]
!     Upon return this holds the number of items in the exclusive region per dimension
!     (i.e. {\tt exclusiveUBound-exclusiveLBound+1}). {\tt exclusiveCount} must
!      be allocated to be of size equal to the Grid dimCount.
!     Please see Section~\ref{sec:grid:usage:bounds} for a description
!     of the regions and their associated bounds and counts. 
!\item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!\end{description}
!
!EOPI
    integer :: localrc
    type(ESMF_GeomBaseClass),pointer :: gbcp
    integer :: cl,cu,cc,el,eu,ec

    ! Initialize return code; assume failure until success is certain
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP_SHORT(ESMF_GeomBaseGetInit, geombase, rc)

    ! Get GeomBaseClass
    gbcp=>geombase%gbcp

    ! Get info depending on type
    select case(gbcp%type%type)

       case (ESMF_GEOMTYPE_GRID%type) ! Grid 
          call ESMF_GridGet(grid=gbcp%grid, localDE=localDE, &
          staggerloc=gbcp%staggerloc,  &
          exclusiveLBound=exclusiveLBound, exclusiveUBound=exclusiveUBound, &
          exclusiveCount=exclusiveCount, rc=localrc) 
          if (ESMF_LogFoundError(localrc, &
                                 ESMF_ERR_PASSTHRU, &
                                 ESMF_CONTEXT, rcToReturn=rc)) return

       case  (ESMF_GEOMTYPE_MESH%type) ! Mesh
          if (present(exclusiveLBound)) exclusiveLBound(1) = 1
          if (present(exclusiveUBound)) then
	       if (gbcp%meshloc == ESMF_MESHLOC_NODE) then
                  call ESMF_MeshGet(mesh=gbcp%mesh, &
                                    numOwnedNodes=exclusiveUBound(1), &
                                    rc=localrc)
                  if (ESMF_LogFoundError(localrc, &
                                   ESMF_ERR_PASSTHRU, &
                                   ESMF_CONTEXT, rcToReturn=rc)) return
	       else if (gbcp%meshloc == ESMF_MESHLOC_ELEMENT) then
                  call ESMF_MeshGet(mesh=gbcp%mesh, &
                                    numOwnedElements=exclusiveUBound(1), &
                                    rc=localrc)
                  if (ESMF_LogFoundError(localrc, &
                                 ESMF_ERR_PASSTHRU, &
                                 ESMF_CONTEXT, rcToReturn=rc)) return
               else
                  if (ESMF_LogFoundError(ESMF_RC_ARG_VALUE, &
                               msg=" Bad Mesh Location value", &
                               ESMF_CONTEXT, rcToReturn=rc)) return
               endif
            endif
          if (present(exclusiveCount)) then
	       if (gbcp%meshloc == ESMF_MESHLOC_NODE) then
                  call ESMF_MeshGet(mesh=gbcp%mesh, &
                                    numOwnedNodes=exclusiveCount(1), &
                                    rc=localrc)
                  if (ESMF_LogFoundError(localrc, &
                                   ESMF_ERR_PASSTHRU, &
                                   ESMF_CONTEXT, rcToReturn=rc)) return
	       else if (gbcp%meshloc == ESMF_MESHLOC_ELEMENT) then
                  call ESMF_MeshGet(mesh=gbcp%mesh, &
                                    numOwnedElements=exclusiveCount(1), &
                                    rc=localrc)
                  if (ESMF_LogFoundError(localrc, &
                                 ESMF_ERR_PASSTHRU, &
                                 ESMF_CONTEXT, rcToReturn=rc)) return
               else
                  if (ESMF_LogFoundError(ESMF_RC_ARG_VALUE, &
                               msg=" Bad Mesh Location value", &
                               ESMF_CONTEXT, rcToReturn=rc)) return
               endif
            endif

       case  (ESMF_GEOMTYPE_LOCSTREAM%type) ! LocStream
          call ESMF_LocStreamGetBounds(gbcp%locstream, &
               localDE=localDE, &   
               exclusiveLBound=el, &
               exclusiveUBound=eu, &
               exclusiveCount=ec,  &
               rc=localrc)
          if (ESMF_LogFoundError(localrc, &
                                 ESMF_ERR_PASSTHRU, &
                                 ESMF_CONTEXT, rcToReturn=rc)) return
          if (present(exclusiveLBound)) exclusiveLBound(1)=el
          if (present(exclusiveUBound)) exclusiveUBound(1)=eu
          if (present(exclusiveCount)) exclusiveCount(1)=ec


       case  (ESMF_GEOMTYPE_XGRID%type) ! Xgrid
          call ESMF_XGridGet(gbcp%xgrid, localDE=localDE, &   
              exclusiveLBound=el, &
               exclusiveUBound=eu, &
               exclusiveCount=ec,  &
               rc=localrc)
          if (ESMF_LogFoundError(localrc, &
                                 ESMF_ERR_PASSTHRU, &
                                 ESMF_CONTEXT, rcToReturn=rc)) return

         if (present(exclusiveLBound)) exclusiveLBound(1)=el
         if (present(exclusiveUBound)) exclusiveUBound(1)=eu
         if (present(exclusiveCount)) exclusiveCount(1)=ec

       case default
         if (ESMF_LogFoundError(ESMF_RC_ARG_VALUE, &
                               msg=" Bad type value", &
                               ESMF_CONTEXT, rcToReturn=rc)) return
    end select

    ! Set return value
    if (present(rc)) rc = ESMF_SUCCESS

 end subroutine ESMF_GeomBaseGetPLocalDe

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GeomBaseSerialize"

!BOPI
! !IROUTINE: ESMF_GeomBaseSerialize - Serialize geombase info into a byte stream
!
! !INTERFACE:
      subroutine ESMF_GeomBaseSerialize(geombase, buffer, length, offset, &
                                        attreconflag, inquireflag, rc) 
!
! !ARGUMENTS:
      type(ESMF_GeomBase), intent(inout) :: geombase 
      character, pointer, dimension(:) :: buffer
      integer, intent(inout) :: length
      integer, intent(inout) :: offset
      type(ESMF_AttReconcileFlag), intent(in), optional :: attreconflag
      type(ESMF_InquireFlag), intent(in), optional :: inquireflag
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!      Takes an {\tt ESMF\_GeomBase} object and adds all the information needed
!      to  recreate the object based on this information.  
!      Expected to be used by {\tt ESMF\_StateReconcile()}.
!
!     The arguments are:
!     \begin{description}
!     \item [geombase]
!           {\tt ESMF\_GeomBase} object to be serialized.
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
    type(ESMF_GeomBaseClass),pointer :: gbcp
    integer :: localrc
    type(ESMF_AttReconcileFlag) :: lattreconflag
    type(ESMF_InquireFlag) :: linquireflag

    ! Initialize return code; assume failure until success is certain
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP_SHORT(ESMF_GeomBaseGetInit, geombase, rc)

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

    ! Get GeomBaseClass
    gbcp=>geombase%gbcp

    ! serialize GeomBase info
    call c_ESMC_GeomBaseSerialize(gbcp%type%type, &
                                  gbcp%staggerloc%staggerloc, &
                                  gbcp%meshloc%meshloc, &
                                  gbcp%xgridside, &
                                  gbcp%xgridIndex, &
                                  buffer, length, offset, linquireflag, &
                                  localrc)
    if (ESMF_LogFoundError(localrc, &
                                 ESMF_ERR_PASSTHRU, &
                                 ESMF_CONTEXT, rcToReturn=rc)) return

    ! Get info depending on type
    select case(gbcp%type%type)

       case (ESMF_GEOMTYPE_GRID%type) ! Grid 
          call ESMF_GridSerialize(grid=gbcp%grid, buffer=buffer, &
                     length=length, offset=offset, &
                     attreconflag=lattreconflag, inquireflag=linquireflag, &
                     rc=localrc) 
          if (ESMF_LogFoundError(localrc, &
                                 ESMF_ERR_PASSTHRU, &
                                 ESMF_CONTEXT, rcToReturn=rc)) return

       case  (ESMF_GEOMTYPE_MESH%type)
          call ESMF_MeshSerialize(mesh=gbcp%mesh, buffer=buffer, &
                     length=length, offset=offset, &
                     inquireflag=linquireflag, &
                     rc=localrc) 
          if (ESMF_LogFoundError(localrc, &
                                 ESMF_ERR_PASSTHRU, &
                                 ESMF_CONTEXT, rcToReturn=rc)) return

       case  (ESMF_GEOMTYPE_LOCSTREAM%type)
          call ESMF_LocStreamSerialize(locstream=gbcp%locstream, &
                      buffer=buffer,length=length, offset=offset, &
                      inquireflag=linquireflag, &
                      rc=localrc) 
          if (ESMF_LogFoundError(localrc, &
                                 ESMF_ERR_PASSTHRU, &
                                 ESMF_CONTEXT, rcToReturn=rc)) return

       case  (ESMF_GEOMTYPE_XGRID%type)
          call ESMF_XGridSerialize(xgrid=gbcp%xgrid, &
                      buffer=buffer,length=length, offset=offset, &
                      inquireflag=linquireflag, &
                      rc=localrc) 
          if (ESMF_LogFoundError(localrc, &
                                 ESMF_ERR_PASSTHRU, &
                                 ESMF_CONTEXT, rcToReturn=rc)) return

       case default
         if (ESMF_LogFoundError(ESMF_RC_ARG_VALUE, &
                               msg=" Bad type value", &
                               ESMF_CONTEXT, rcToReturn=rc)) return
    end select


    ! Set return value
    if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_GeomBaseSerialize

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GeomBaseDeserialize"

!BOPI
! !IROUTINE: ESMF_GeomBaseDeserialize - Deserialize a byte stream into a GeomBase
!
! !INTERFACE:
      function ESMF_GeomBaseDeserialize(buffer, offset, attreconflag, rc) 
!
! !RETURN VALUE:
      type(ESMF_GeomBase) :: ESMF_GeomBaseDeserialize   
!
! !ARGUMENTS:
      character, pointer, dimension(:) :: buffer
      integer, intent(inout) :: offset
      type(ESMF_AttReconcileFlag), optional :: attreconflag
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!      Takes a byte-stream buffer and reads the information needed to
!      recreate a Grid object.  Recursively calls the deserialize routines
!      needed to recreate the subobjects.
!      Expected to be used by {\tt ESMF\_StateReconcile()}.
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
    type(ESMF_GeomBaseClass),pointer :: gbcp
    integer :: localrc
    type(ESMF_AttReconcileFlag) :: lattreconflag

    ! Initialize return code; assume failure until success is certain
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! deal with optional attreconflag
    if (present(attreconflag)) then
      lattreconflag = attreconflag
    else
      lattreconflag = ESMF_ATTRECONCILE_OFF
    endif

    ! allocate GeomBase type
    allocate(gbcp, stat=localrc)
    if (ESMF_LogFoundAllocError(localrc, msg="Allocating GeomBase type object", &
                                     ESMF_CONTEXT, rcToReturn=rc)) return

    ! serialize GeomBase info
    call c_ESMC_GeomBaseDeserialize(gbcp%type%type,	&
				    gbcp%staggerloc%staggerloc, &
                                    gbcp%meshloc%meshloc, &
                                    gbcp%xgridside, &
                                    gbcp%xgridIndex, &
                                    buffer, offset, localrc)
    if (ESMF_LogFoundError(localrc, &
                                 ESMF_ERR_PASSTHRU, &
                                 ESMF_CONTEXT, rcToReturn=rc)) return

    ! Get info depending on type
    select case(gbcp%type%type)

       case (ESMF_GEOMTYPE_GRID%type) ! Grid
          gbcp%grid=ESMF_GridDeserialize(buffer=buffer, &
              offset=offset, attreconflag=lattreconflag, rc=localrc)
          if (ESMF_LogFoundError(localrc, &
                                 ESMF_ERR_PASSTHRU, &
                                 ESMF_CONTEXT, rcToReturn=rc)) return  

       case  (ESMF_GEOMTYPE_MESH%type)
          gbcp%mesh=ESMF_MeshDeserialize(buffer=buffer, &
              offset=offset, rc=localrc)
          if (ESMF_LogFoundError(localrc, &
                                 ESMF_ERR_PASSTHRU, &
                                 ESMF_CONTEXT, rcToReturn=rc)) return  


       case  (ESMF_GEOMTYPE_LOCSTREAM%type)
          gbcp%locstream=ESMF_LocStreamDeserialize(buffer=buffer, &
              offset=offset, rc=localrc)
          if (ESMF_LogFoundError(localrc, &
                                 ESMF_ERR_PASSTHRU, &
                                 ESMF_CONTEXT, rcToReturn=rc)) return  

       case  (ESMF_GEOMTYPE_XGRID%type)
          gbcp%xgrid=ESMF_XGridDeserialize(buffer=buffer, &
              offset=offset, rc=localrc)
          if (ESMF_LogFoundError(localrc, &
                                 ESMF_ERR_PASSTHRU, &
                                 ESMF_CONTEXT, rcToReturn=rc)) return  

       case default
         if (ESMF_LogFoundError(ESMF_RC_ARG_VALUE, &
                               msg=" Bad type value", &
                               ESMF_CONTEXT, rcToReturn=rc)) return
    end select


      ! Set pointer 
      ESMF_GeomBaseDeserialize%gbcp=>gbcp

      ! Add reference to this object into ESMF garbage collection table
      call c_ESMC_VMAddFObject(ESMF_GeomBaseDeserialize, ESMF_ID_GEOMBASE%objectID)
    
     ! Set init status
      ESMF_INIT_SET_CREATED(ESMF_GeomBaseDeserialize)

      if  (present(rc)) rc = ESMF_SUCCESS

      end function ESMF_GeomBaseDeserialize




! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GeomBaseValidate()"
!BOPI
! !IROUTINE: ESMF_GeomBaseValidate - Validate GeomBase internals

! !INTERFACE:
  subroutine ESMF_GeomBaseValidate(geombase, rc)
!
! !ARGUMENTS:
    type(ESMF_GeomBase), intent(in)              :: geombase
    integer,             intent(out),  optional  :: rc  
!         
!
! !DESCRIPTION:
!      Validates that the {\tt GeomBase} is internally consistent.
!
!     The arguments are:
!     \begin{description}
!     \item[geombase] 
!          Specified {\tt ESMF\_GeomBase} object.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    integer :: localrc
    type(ESMF_GeomBaseClass),pointer :: gbcp

    ! Initialize return code; assume failure until success is certain
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP_SHORT(ESMF_GeomBaseGetInit, geombase, rc)

    ! Get GeomBaseClass
    gbcp=>geombase%gbcp

    ! Get info depending on type
    select case(gbcp%type%type)

       case (ESMF_GEOMTYPE_GRID%type) ! Grid 
         call ESMF_GridValidate(grid=gbcp%grid, rc=localrc)
          if (ESMF_LogFoundError(localrc, &
                                 ESMF_ERR_PASSTHRU, &
                                 ESMF_CONTEXT, rcToReturn=rc)) return

       case (ESMF_GEOMTYPE_MESH%type) ! Mesh
         !call ESMF_MeshValidate(mesh=gbcp%mesh, rc=localrc)
         ! if (ESMF_LogFoundError(localrc, &
         !                        ESMF_ERR_PASSTHRU, &
         !                        ESMF_CONTEXT, rcToReturn=rc)) return

       case (ESMF_GEOMTYPE_LOCSTREAM%type) ! LocStream
          call ESMF_LocStreamValidate(locstream=gbcp%locstream, rc=localrc)
          if (ESMF_LogFoundError(localrc, &
                                 ESMF_ERR_PASSTHRU, &
                                 ESMF_CONTEXT, rcToReturn=rc)) return

       case (ESMF_GEOMTYPE_XGRID%type) ! XGrid
          call ESMF_XGridValidate(xgrid=gbcp%xgrid, rc=localrc)
          if (ESMF_LogFoundError(localrc, &
                                 ESMF_ERR_PASSTHRU, &
                                 ESMF_CONTEXT, rcToReturn=rc)) return

       case default
         if (ESMF_LogFoundError(ESMF_RC_ARG_VALUE, &
                               msg=" Bad type value", &
                               ESMF_CONTEXT, rcToReturn=rc)) return
    end select


    ! Set return value
    if (present(rc)) rc = ESMF_SUCCESS
    
  end subroutine ESMF_GeomBaseValidate
!------------------------------------------------------------------------------

! -------------------------- ESMF-internal method -----------------------------
#undef ESMF_METHOD
#define ESMF_METHOD "ESMF_GeomBaseGetInit"
!BOPI
! !IROUTINE: ESMF_GeomBaseGetInit - Internal access routine for init code
!
! !INTERFACE:
      function ESMF_GeomBaseGetInit(geombase)
!
! !RETURN VALUE:
      ESMF_INIT_TYPE :: ESMF_GeomBaseGetInit
!
! !ARGUMENTS:
      type(ESMF_GeomBase), intent(in), optional :: geombase
!
! !DESCRIPTION:
! Access deep object init code.
!
! The arguments are:
! \begin{description}
! \item [geombase]
! Grid Base object.
! \end{description}
!
!EOPI

    if (present(geombase)) then
      ESMF_GeomBaseGetInit = ESMF_INIT_GET(geombase)
    else
      ESMF_GeomBaseGetInit = ESMF_INIT_CREATED
    endif

    end function ESMF_GeomBaseGetInit

!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GeomTypeEqual"
!BOPI
! !IROUTINE: ESMF_GeomTypeEqual - Equality of GeomTypes
!
! !INTERFACE:
      function ESMF_GeomTypeEqual(GeomType1, GeomType2)

! !RETURN VALUE:
      logical :: ESMF_GeomTypeEqual

! !ARGUMENTS:

      type (ESMF_GeomType_Flag), intent(in) :: &
         GeomType1,      &! Two igrid statuses to compare for
         GeomType2        ! equality

! !DESCRIPTION:
!     This routine compares two ESMF GeomType statuses to see if
!     they are equivalent.
!
!     The arguments are:
!     \begin{description}
!     \item[GeomType1, GeomType2]
!          Two igrid statuses to compare for equality
!     \end{description}
!
!EOPI

      ESMF_GeomTypeEqual = (GeomType1%type == &
                              GeomType2%type)

      end function ESMF_GeomTypeEqual
!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GeomTypeNotEqual"
!BOPI
! !IROUTINE: ESMF_GeomTypeNotEqual - Non-equality of GeomTypes
!
! !INTERFACE:
      function ESMF_GeomTypeNotEqual(GeomType1, GeomType2)

! !RETURN VALUE:
      logical :: ESMF_GeomTypeNotEqual

! !ARGUMENTS:

      type (ESMF_GeomType_Flag), intent(in) :: &
         GeomType1,      &! Two GeomType Statuses to compare for
         GeomType2        ! inequality

! !DESCRIPTION:
!     This routine compares two ESMF GeomType statuses to see if
!     they are unequal.
!
!     The arguments are:
!     \begin{description}
!     \item[GeomType1, GeomType2]
!          Two statuses of GeomTypes to compare for inequality
!     \end{description}
!
!EOPI

      ESMF_GeomTypeNotEqual = (GeomType1%type /= &
                                 GeomType2%type)

      end function ESMF_GeomTypeNotEqual


#undef  ESMF_METHOD

end module ESMF_GeomBaseMod


  subroutine f_esmf_geombasecollectgarbage(gb, rc)
#undef  ESMF_METHOD
#define ESMF_METHOD "f_esmf_geombasecollectgarbage()"
    use ESMF_UtilTypesMod
    use ESMF_LogErrMod
    use ESMF_GeomBaseMod

    implicit none

    type(ESMF_GeomBase)  :: gb
    integer, intent(out) :: rc     
  
    integer :: localrc              
  
    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    rc = ESMF_RC_NOT_IMPL
    
    !print *, "collecting GeomBase garbage"
    
    ! deallocate actual GeomBaseClass allocation      
    if (associated(gb%gbcp)) then
      deallocate(gb%gbcp, stat=localrc)
      if (ESMF_LogFoundAllocError(localrc, msg="Deallocating GeomBase", &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif
    nullify(gb%gbcp)

    ! return successfully  
    rc = ESMF_SUCCESS

  end subroutine f_esmf_geombasecollectgarbage
