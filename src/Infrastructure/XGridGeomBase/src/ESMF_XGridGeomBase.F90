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
!
#define ESMF_FILENAME "ESMF_XGridGeomBase.F90"
!
!     ESMF GeomBase Module
      module ESMF_XGridGeomBaseMod
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
! !MODULE: ESMF_XGridGeomBaseMod - GeomBase class
!
! !DESCRIPTION:
!
! The code in this file implements the {\tt ESMF\_XGridGeomBase} class.
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

!     NEED TO ADD MORE HERE
      implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
      private


!------------------------------------------------------------------------------
! ! ESMF_XGridGeomType_Flag
!
!------------------------------------------------------------------------------
  type ESMF_XGridGeomType_Flag
#ifndef ESMF_NO_SEQUENCE
    sequence
#endif
     integer :: type
  end type

  type(ESMF_XGridGeomType_Flag), parameter :: &
                      ESMF_XGridGEOMTYPE_INVALID=ESMF_XGridGeomType_Flag(-1), &
                      ESMF_XGridGEOMTYPE_UNINIT=ESMF_XGridGeomType_Flag(0), &
                      ESMF_XGridGEOMTYPE_GRID=ESMF_XGridGeomType_Flag(1), &
                      ESMF_XGridGEOMTYPE_MESH=ESMF_XGridGeomType_Flag(2)

!------------------------------------------------------------------------------
! ! ESMF_XGridGeomBaseClass
!
!------------------------------------------------------------------------------

  ! F90 class type to hold pointer to object
  type ESMF_XGridGeomBaseClass
#ifndef ESMF_NO_SEQUENCE
  sequence
#endif
    type(ESMF_XGridGeomType_Flag) :: type
    type(ESMF_StaggerLoc) :: staggerloc
    type(ESMF_Grid) :: grid
    type(ESMF_MeshLoc) :: meshloc ! either nodes or elements
    type(ESMF_Mesh) :: mesh
  end type


!------------------------------------------------------------------------------
! ! ESMF_XGridGeomBase
!
!------------------------------------------------------------------------------

  ! F90 class type to hold pointer to C++ object
  type ESMF_XGridGeomBase
#ifndef ESMF_NO_SEQUENCE
  sequence
#endif

    type(ESMF_XGridGeomBaseClass),pointer :: gbcp

    ESMF_INIT_DECLARE
  end type

!------------------------------------------------------------------------------
!
! !PUBLIC TYPES:
!
public ESMF_XGridGeomBase
public ESMF_XGridGeomBaseClass ! for internal use only
public ESMF_XGridGeomType_Flag,  ESMF_XGRIDGEOMTYPE_INVALID, ESMF_XGRIDGEOMTYPE_UNINIT, &
                       ESMF_XGRIDGEOMTYPE_GRID,  ESMF_XGRIDGEOMTYPE_MESH

!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:
!
!
! - ESMF-public methods:

  public operator(==)
  public operator(/=)

  public ESMF_XGridGeomBaseCreate
  public ESMF_XGridGeomBaseDestroy

  public ESMF_XGridGeomBaseGet
  public ESMF_XGridGeomBaseGetPlocalDE
  public ESMF_XGridGeomBaseMatch

  public ESMF_XGridGeomBaseSerialize
  public ESMF_XGridGeomBaseDeserialize

  public ESMF_XGridGeomBaseValidate

  public ESMF_XGridGeomBaseGetArrayInfo


!  public ESMF_XGridGeomBaseGetMesh

! - ESMF-internal methods:
  public ESMF_XGridGeomBaseGetInit

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
! !IROUTINE: ESMF_XGridGeomBaseCreate -- Generic interface

! !INTERFACE:
interface ESMF_XGridGeomBaseCreate

! !PRIVATE MEMBER FUNCTIONS:
!
      module procedure ESMF_XGridGeomBaseCreateGrid
      module procedure ESMF_XGridGeomBaseCreateMesh

! !DESCRIPTION:
! This interface provides a single entry point for the various
!  types of {\tt ESMF\_XGridGeomBaseCreate} functions.
!EOPI
end interface



!==============================================================================
!BOPI
! !INTERFACE:
      interface operator (==)

! !PRIVATE MEMBER FUNCTIONS:
         module procedure ESMF_XGridGeomTypeEqual

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
         module procedure ESMF_XGridGeomTypeNotEqual

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
#define ESMF_METHOD "ESMF_XGridGeomBaseGetArrayInfo"

!BOPI
! !IROUTINE: ESMF_XGridGeomBaseGetArrayInfo" - get information to make an Array from a GeomBase

! !INTERFACE:
      subroutine ESMF_XGridGeomBaseGetArrayInfo(gridbase, &
                           gridToFieldMap, ungriddedLBound, ungriddedUBound, &
                           distgrid, distgridToArrayMap, undistLBound, undistUBound,   &
                           rc)
!
! !ARGUMENTS:
       type(ESMF_XGridGeomBase),   intent(in)       :: gridbase
       integer,               intent(in),  optional :: gridToFieldMap(:)
       integer,               intent(in),  optional :: ungriddedLBound(:)
       integer,               intent(in),  optional :: ungriddedUBound(:)
       type(ESMF_DistGrid),   intent(out), optional :: distgrid
       integer,               intent(out)           :: distgridToArrayMap(:)
       integer,               intent(out)           :: undistLBound(:)
       integer,               intent(out)           :: undistUBound(:)
       integer,               intent(out), optional :: rc

!
! !DESCRIPTION:
!
! This subroutine gets information from a GeomBase which is useful in creating an
! Array. This subroutine takes as input {\tt gridToFieldMap} which gives for each
! gridbase object dimension which dimension in the eventual Array it should be
! mapped to. It also takes {\tt ungriddedLBound} and {\tt ungriddedUBound} which
! describes the dimensions of the Array not associated with the gridbase object.
! From these it produces a mapping from the distgrid to the Array, the undistributed
! bounds of the Array in the correct order. (For everything besides {\tt Grid}  the
! gridToFieldMap and distgridToArrayMap will be single element
! arrays describing which dimension in the Array the gridbase object (e.g. Mesh)
! is mapped to.
!
! The arguments are:
! \begin{description}
!\item[{gridbase}]
!     The gridbase to get the information from to create the Array.
!\item[{[distgrid]}]
!     The distgrid to create the Array on
!\item[{[gridToFieldMap]}]
!     Indicates where each grid dimension goes in the newly created Array.
!     {\tt The array gridToFieldMap} should be at least of size equal to the gridbase object's
!     Array dimension (e.g. Mesh = 1)
!     If not set defaults to (1,2,3,....,gridbase objects dim).
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
    type(ESMF_XGridGeomBaseClass),pointer :: gbcp

    ! Initialize return code; assume failure until success is certain
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP_SHORT(ESMF_XGridGeomBaseGetInit, gridbase, rc)

    ! Get GeomBaseClass
    gbcp=>gridbase%gbcp

    ! Get info depending on type
    select case(gbcp%type%type)
     case (ESMF_XGRIDGEOMTYPE_GRID%type) ! Grid
       call ESMF_GridGetArrayInfo(gbcp%grid, gbcp%staggerloc, &
         gridToFieldMap, ungriddedLBound, ungriddedUBound,  &
         distgrid, distgridToArrayMap, undistLBound, undistUBound,       &
         rc=localrc)
       if (ESMF_LogFoundError(localrc, &
         ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return

     case (ESMF_XGRIDGEOMTYPE_MESH%type) ! Mesh
        if (present(gridToFieldMap)) then
          distgridToArrayMap = gridToFieldMap
        else
          distgridToArrayMap = 1
        endif

        if (present(ungriddedLBound)) then
            if (size(ungriddedLBound) .gt. 0) undistLBound = ungriddedLBound
        endif
        if (present(ungriddedUBound)) then
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



     case default
       if (ESMF_LogFoundError(ESMF_RC_ARG_VALUE, &
         msg=" Bad type value", &
         ESMF_CONTEXT, rcToReturn=rc)) return
    end select


    ! Set return value
    if (present(rc)) rc = ESMF_SUCCESS

    end  subroutine ESMF_XGridGeomBaseGetArrayInfo

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_XGridGeomBaseCreate"
!BOPI
! !IROUTINE: ESMF_XGridGeomBaseCreate - Create a GeomBase from a Grid

! !INTERFACE:
  ! Private name; call using ESMF_XGridGeomBaseCreate()
      function ESMF_XGridGeomBaseCreateGrid(grid,staggerloc, rc)
!
! !RETURN VALUE:
      type(ESMF_XGridGeomBase) :: ESMF_XGridGeomBaseCreateGrid
!
! !ARGUMENTS:
       type(ESMF_Grid),       intent(in)              :: grid
       type(ESMF_StaggerLoc), intent(in)              :: staggerloc
       integer,               intent(out),  optional  :: rc
!
! !DESCRIPTION:
! Create an {\tt ESMF\_XGridGeomBase} object from an {\tt ESMF\_Grid} object.
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
    type(ESMF_XGridGeomBaseClass),pointer :: gbcp
    integer :: localrc ! local error status

    ! Initialize return code; assume failure until success is certain
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP_SHORT(ESMF_GridGetInit, grid, rc)

    ! initialize pointers
    nullify(gbcp)
    nullify( ESMF_XGridGeomBaseCreateGrid%gbcp)

    ! allocate GeomBase type
    allocate(gbcp, stat=localrc)
    if (ESMF_LogFoundAllocError(localrc, msg="Allocating GeomBase type object", &
     ESMF_CONTEXT, rcToReturn=rc)) return

    ! Set values in GeomBase
    gbcp%type = ESMF_XGRIDGEOMTYPE_GRID
    gbcp%grid = grid
    gbcp%staggerloc = staggerloc

    ! Set GeomBase Type into GeomBase
     ESMF_XGridGeomBaseCreateGrid%gbcp=>gbcp

    ! Add reference to this object into ESMF garbage collection table
    call c_ESMC_VMAddFObject(ESMF_XGridGeomBaseCreateGrid, ESMF_ID_GEOMBASE%objectID)

    ! Set init status
    ESMF_INIT_SET_CREATED(ESMF_XGridGeomBaseCreateGrid)

    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS

    end function ESMF_XGridGeomBaseCreateGrid

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_XGridGeomBaseCreate"
!BOPI
! !IROUTINE: ESMF_XGridGeomBaseCreate - Create a GeomBase from a Mesh

! !INTERFACE:
  ! Private name; call using ESMF_XGridGeomBaseCreate()
      function ESMF_XGridGeomBaseCreateMesh(mesh, loc, rc)
!
! !RETURN VALUE:
      type(ESMF_XGridGeomBase) :: ESMF_XGridGeomBaseCreateMesh
!
! !ARGUMENTS:
       type(ESMF_Mesh),       intent(in)              :: mesh
       type(ESMF_MeshLoc),    intent(in), optional    :: loc
       integer,               intent(out),  optional  :: rc
!
! !DESCRIPTION:
! Create an {\tt ESMF\_XGridGeomBase} object from an {\tt ESMF\_Mesh} object.
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
    type(ESMF_XGridGeomBaseClass),pointer :: gbcp
    integer :: localrc ! local error status
    type(ESMF_MeshLoc) :: localLoc

    ! Initialize return code; assume failure until success is certain
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP_SHORT(ESMF_MeshGetInit, mesh, rc)

    ! initialize pointers
    nullify(gbcp)
    nullify( ESMF_XGridGeomBaseCreateMesh%gbcp)

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
    gbcp%type = ESMF_XGRIDGEOMTYPE_MESH
    gbcp%mesh = mesh
    gbcp%meshloc = localLoc

    ! Set GeomBase Type into GeomBase
     ESMF_XGridGeomBaseCreateMesh%gbcp=>gbcp

    ! Add reference to this object into ESMF garbage collection table
    call c_ESMC_VMAddFObject(ESMF_XGridGeomBaseCreateMesh, ESMF_ID_GEOMBASE%objectID)

    ! Set init status
    ESMF_INIT_SET_CREATED(ESMF_XGridGeomBaseCreateMesh)

    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS

    end function ESMF_XGridGeomBaseCreateMesh


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_XGridGeomBaseDestroy"
!BOPI
! !IROUTINE: ESMF_XGridGeomBaseDestroy - Release resources associated with a GeomBase

! !INTERFACE:
      subroutine ESMF_XGridGeomBaseDestroy(gridbase, rc)
!
! !ARGUMENTS:
      type(ESMF_XGridGeomBase) :: gridbase
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!   Destroys an {\tt ESMF\_GridBase} object. This call does not destroy wrapped
!   Grid, or other Grid objects.
!
!     The arguments are:
!     \begin{description}
!     \item[gridbase]
!          {\tt ESMF\_XGridGeomBase} to be destroyed.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI

    ! Initialize return code; assume failure until success is certain
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_XGridGeomBaseGetInit, gridbase, rc)

    ! do _not_ deallocate/nullify GeomBase memory here because ESMF
    ! garbage collection will handle cleaning up GeomBase allocations

    ! Set init code
    ESMF_INIT_SET_DELETED(gridbase)

    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS

 end subroutine ESMF_XGridGeomBaseDestroy


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_XGridGeomBaseGet"
!BOPI
! !IROUTINE: ESMF_XGridGeomBaseGet - Get information about a Grid

! !INTERFACE:
      subroutine ESMF_XGridGeomBaseGet(gridbase, &
          dimCount, localDECount, distgrid, &
          distgridToGridMap, indexFlag, geomtype, &
          grid, staggerloc, mesh, meshloc, rc)
!
! !ARGUMENTS:
      type(ESMF_XGridGeomBase),   intent(in)       :: gridbase
      integer,               intent(out), optional :: dimCount
      integer,               intent(out), optional :: localDECount
      type(ESMF_DistGrid),   intent(out), optional :: distgrid
      integer,               intent(out), optional :: distgridToGridMap(:)
      type(ESMF_Index_Flag), intent(out), optional :: indexflag
      type(ESMF_XGridGeomType_Flag),   intent(out), optional :: geomtype
      type(ESMF_Grid),       intent(out), optional :: grid
      type(ESMF_StaggerLoc), intent(out), optional :: staggerloc
      type(ESMF_Mesh),       intent(out), optional :: mesh
      type(ESMF_MeshLoc),    intent(out), optional :: meshloc
      integer,               intent(out), optional :: rc

!
! !DESCRIPTION:
!    Gets various types of information about a grid.
!
!The arguments are:
!\begin{description}
!\item[{gridbase}]
!   Gridbase to get the information from.
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
!    Flag that indicates what type of object this gridbase holds.
!    Can be {\tt ESMF_XGRIDGEOMTYPE_GRID}, {\tt ESMF_XGRIDGEOMTYPE_MESH},...
! \item[{[grid]}]
!    The Grid object that this gridbase object holds.
! \item[{[staggerloc]}]
!    The Grid stagger location.
! \item[{[mesh]}]
!    The Mesh object that this gridbase object holds.
! \item[{[meshloc]}]
!    The part of the mesh that the field is on
!\item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!\end{description}
!
!EOPI
    type(ESMF_XGridGeomBaseClass),pointer :: gbcp
    integer                          :: localrc

    ! Initialize return code; assume failure until success is certain
    if (present(rc)) rc = ESMF_RC_NOT_IMPL


    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP_SHORT(ESMF_XGridGeomBaseGetInit, gridbase, rc)

    ! Get GeomBaseClass
    gbcp=>gridbase%gbcp

    ! get type
    if (present(geomtype)) then
       geomtype=gbcp%type
    endif

    ! Get grid object plus error checking
    if (present(grid)) then
       if (gbcp%type==ESMF_XGRIDGEOMTYPE_GRID) then
          grid=gbcp%grid
       else
          if (ESMF_LogFoundError(ESMF_RC_ARG_VALUE, &
           msg=" Grid not geometry type", &
           ESMF_CONTEXT, rcToReturn=rc)) return
       endif
    endif

    ! Get mesh object plus error checking
    if (present(mesh)) then
       if (gbcp%type==ESMF_XGRIDGEOMTYPE_Mesh) then
          mesh=gbcp%mesh
       else
        if (ESMF_LogFoundError(ESMF_RC_ARG_VALUE, &
         msg=" Mesh not geometry type", &
         ESMF_CONTEXT, rcToReturn=rc)) return
       endif
    endif

    ! Get objects plus error checking
    if (present(staggerloc)) then
       if (gbcp%type==ESMF_XGRIDGEOMTYPE_GRID) then
          staggerloc=gbcp%staggerloc
       else
          if (ESMF_LogFoundError(ESMF_RC_ARG_VALUE, &
           msg=" Grid not geometry type", &
           ESMF_CONTEXT, rcToReturn=rc)) return
       endif
    endif

    ! Get objects plus error checking
    if (present(meshloc)) then
       if (gbcp%type==ESMF_XGRIDGEOMTYPE_MESH) then
          meshloc=gbcp%meshloc
       else
          if (ESMF_LogFoundError(ESMF_RC_ARG_VALUE, &
           msg=" Grid not geometry type", &
           ESMF_CONTEXT, rcToReturn=rc)) return
       endif
    endif




    ! Get info depending on type
    select case(gbcp%type%type)
       case (ESMF_XGRIDGEOMTYPE_GRID%type) ! Grid
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

       case (ESMF_XGRIDGEOMTYPE_MESH%type) ! Mesh
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

       case default
         if (ESMF_LogFoundError(ESMF_RC_ARG_VALUE, &
           msg=" Bad type value", &
           ESMF_CONTEXT, rcToReturn=rc)) return
    end select


    ! Set return value
    if (present(rc)) rc = ESMF_SUCCESS

end subroutine ESMF_XGridGeomBaseGet

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_XGridGeomBaseGetPLocalDe"
!BOPI
! !IROUTINE: ESMF_XGridGeomBaseGetPLocalDE - Get information about a particular DE

! !INTERFACE:
      subroutine ESMF_XGridGeomBaseGetPLocalDe(gridbase, localDe, &
          exclusiveLBound, exclusiveUBound, exclusiveCount,  rc)

!
! !ARGUMENTS:
      type(ESMF_XGridGeomBase),   intent(in)        :: gridbase
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
!\item[{gridbase}]
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
    type(ESMF_XGridGeomBaseClass),pointer :: gbcp
    integer :: cl,cu,cc,el,eu,ec

    ! Initialize return code; assume failure until success is certain
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP_SHORT(ESMF_XGridGeomBaseGetInit, gridbase, rc)

    ! Get GeomBaseClass
    gbcp=>gridbase%gbcp

    ! Get info depending on type
    select case(gbcp%type%type)

       case (ESMF_XGRIDGEOMTYPE_GRID%type) ! Grid
          call ESMF_GridGet(grid=gbcp%grid, localDE=localDE, &
          staggerloc=gbcp%staggerloc,  &
          exclusiveLBound=exclusiveLBound, exclusiveUBound=exclusiveUBound, &
          exclusiveCount=exclusiveCount, rc=localrc)
          if (ESMF_LogFoundError(localrc, &
     ESMF_ERR_PASSTHRU, &
     ESMF_CONTEXT, rcToReturn=rc)) return

       case  (ESMF_XGRIDGEOMTYPE_MESH%type) ! Mesh
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

       case default
         if (ESMF_LogFoundError(ESMF_RC_ARG_VALUE, &
    msg=" Bad type value", &
    ESMF_CONTEXT, rcToReturn=rc)) return
      end select

    ! Set return value
    if (present(rc)) rc = ESMF_SUCCESS

 end subroutine ESMF_XGridGeomBaseGetPLocalDe

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_XGridGeomBaseMatch"
!BOPI
! !IROUTINE: ESMF_XGridGeomBaseMatch - Match geometric object

! !INTERFACE:
      function ESMF_XGridGeomBaseMatch(xgb1, xgb2, rc)

      logical :: ESMF_XGridGeomBaseMatch
!
! !ARGUMENTS:
      type(ESMF_XGridGeomBase),   intent(in)       :: xgb1
      type(ESMF_XGridGeomBase),   intent(in)       :: xgb2
      integer,               intent(out), optional :: rc

!
! !DESCRIPTION:
!    Match geometric object
!
!The arguments are:
!\begin{description}
!\item[{xgb1}]
!   first xgrid geombase object
!\item[{xgb2}]
!   second xgrid geombase object
!\item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!\end{description}
!
!EOPI
    type(ESMF_XGridGeomBaseClass),pointer :: gbcp1, gbcp2
    integer                          :: localrc

    ! Initialize return code; assume failure until success is certain
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP_SHORT(ESMF_XGridGeomBaseGetInit, xgb1, rc)
    ESMF_INIT_CHECK_DEEP_SHORT(ESMF_XGridGeomBaseGetInit, xgb2, rc)

    ! Get GeomBaseClass
    gbcp1=>xgb1%gbcp
    gbcp2=>xgb2%gbcp

    ESMF_XGridGeomBaseMatch = .false.

    if(gbcp1%type%type /= gbcp2%type%type) then
      call ESMF_LogSetError(rcToCheck=ESMF_RC_OBJ_BAD, &
         msg="xgrid geombase object type does not match", &
          ESMF_CONTEXT, rcToReturn=rc)
      return
    endif

    ! Get info depending on type
    select case(gbcp1%type%type)
       case (ESMF_XGRIDGEOMTYPE_GRID%type) ! Grid

         if(ESMF_GridMatch(gbcp1%grid, gbcp2%grid, rc=localrc) >= ESMF_GRIDMATCH_EXACT) &
          ESMF_XGridGeomBaseMatch = .true.
         if (ESMF_LogFoundError(ESMF_RC_ARG_VALUE, &
     msg=" Bad type value", &
     ESMF_CONTEXT, rcToReturn=rc)) return

       case (ESMF_XGRIDGEOMTYPE_MESH%type) ! Mesh
         ESMF_XGridGeomBaseMatch = ESMF_MeshMatch(gbcp1%mesh, gbcp2%mesh, rc=localrc)
         if (ESMF_LogFoundError(ESMF_RC_ARG_VALUE, &
     msg=" Bad type value", &
     ESMF_CONTEXT, rcToReturn=rc)) return

       case default
         if (ESMF_LogFoundError(ESMF_RC_ARG_VALUE, &
     msg=" Bad type value", &
     ESMF_CONTEXT, rcToReturn=rc)) return
    end select


    ! Set return value
    if (present(rc)) rc = ESMF_SUCCESS

end function ESMF_XGridGeomBaseMatch

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_XGridGeomBaseSerialize"

!BOPI
! !IROUTINE: ESMF_XGridGeomBaseSerialize - Serialize gridbase info into a byte stream
!
! !INTERFACE:
      subroutine ESMF_XGridGeomBaseSerialize(gridbase, buffer, length, offset, &
                                        attreconflag, inquireflag, rc)
!
! !ARGUMENTS:
      type(ESMF_XGridGeomBase), intent(inout)           :: gridbase
      character, pointer, dimension(:)                  :: buffer
      integer, intent(inout)                            :: length
      integer, intent(inout)                            :: offset
      type(ESMF_AttReconcileFlag), intent(in), optional :: attreconflag
      type(ESMF_InquireFlag), intent(in), optional      :: inquireflag
      integer, intent(out), optional                    :: rc
!
! !DESCRIPTION:
!      Takes an {\tt ESMF\_XGridGeomBase} object and adds all the information needed
!      to  recreate the object based on this information.
!      Expected to be used by {\tt ESMF\_StateReconcile()}.
!
!     The arguments are:
!     \begin{description}
!     \item [gridbase]
!           {\tt ESMF\_XGridGeomBase} object to be serialized.
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
    type(ESMF_XGridGeomBaseClass),pointer :: gbcp
    integer :: localrc
    type(ESMF_AttReconcileFlag) :: lattreconflag
    type(ESMF_InquireFlag) :: linquireflag

    ! Initialize return code; assume failure until success is certain
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP_SHORT(ESMF_XGridGeomBaseGetInit, gridbase, rc)

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
    gbcp=>gridbase%gbcp

    ! serialize GeomBase info
    call c_ESMC_XGridGeomBaseSerialize(gbcp%type%type, &
                                  gbcp%staggerloc%staggerloc, &
                                  gbcp%meshloc%meshloc, &
                                  buffer, length, offset, linquireflag, &
                                  localrc)
    if (ESMF_LogFoundError(localrc, &
     ESMF_ERR_PASSTHRU, &
     ESMF_CONTEXT, rcToReturn=rc)) return

    ! Get info depending on type
    select case(gbcp%type%type)

       case (ESMF_XGRIDGEOMTYPE_GRID%type) ! Grid
          call ESMF_GridSerialize(grid=gbcp%grid, buffer=buffer, &
                     length=length, offset=offset, &
                     attreconflag=lattreconflag, inquireflag=linquireflag, &
                     rc=localrc)
          if (ESMF_LogFoundError(localrc, &
   ESMF_ERR_PASSTHRU, &
   ESMF_CONTEXT, rcToReturn=rc)) return

       case  (ESMF_XGRIDGEOMTYPE_MESH%type)
          call ESMF_MeshSerialize(mesh=gbcp%mesh, buffer=buffer, &
                     length=length, offset=offset, &
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

      end subroutine ESMF_XGridGeomBaseSerialize

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_XGridGeomBaseDeserialize"

!BOPI
! !IROUTINE: ESMF_XGridGeomBaseDeserialize - Deserialize a byte stream into a GeomBase
!
! !INTERFACE:
      function ESMF_XGridGeomBaseDeserialize(buffer, offset, attreconflag, rc)
!
! !RETURN VALUE:
      type(ESMF_XGridGeomBase) :: ESMF_XGridGeomBaseDeserialize
!
! !ARGUMENTS:
      character, pointer, dimension(:)      :: buffer
      integer, intent(inout)                :: offset
      type(ESMF_AttReconcileFlag), optional :: attreconflag
      integer, intent(out), optional        :: rc
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
    type(ESMF_XGridGeomBaseClass),pointer :: gbcp
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
                                     ESMF_CONTEXT,  &
                                     rcToReturn=rc)) return

    ! serialize GeomBase info
    call c_ESMC_XGridGeomBaseDeserialize(gbcp%type%type,        &
                                    gbcp%staggerloc%staggerloc, &
                                    gbcp%meshloc%meshloc, &
                                    buffer, offset, localrc)
    if (ESMF_LogFoundError(localrc, &
   ESMF_ERR_PASSTHRU, &
   ESMF_CONTEXT, rcToReturn=rc)) return

    ! Get info depending on type
    select case(gbcp%type%type)

     case (ESMF_XGRIDGEOMTYPE_GRID%type) ! Grid
        gbcp%grid=ESMF_GridDeserialize(buffer=buffer, &
            offset=offset, attreconflag=lattreconflag, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
           ESMF_ERR_PASSTHRU, &
           ESMF_CONTEXT, rcToReturn=rc)) return

     case (ESMF_XGRIDGEOMTYPE_MESH%type)
        gbcp%mesh=ESMF_MeshDeserialize(buffer=buffer, &
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
    ESMF_XGridGeomBaseDeserialize%gbcp=>gbcp

    ! Add reference to this object into ESMF garbage collection table
    call c_ESMC_VMAddFObject(ESMF_XGridGeomBaseDeserialize, ESMF_ID_GEOMBASE%objectID)

   ! Set init status
    ESMF_INIT_SET_CREATED(ESMF_XGridGeomBaseDeserialize)

    if  (present(rc)) rc = ESMF_SUCCESS

    end function ESMF_XGridGeomBaseDeserialize




! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_XGridGeomBaseValidate()"
!BOPI
! !IROUTINE: ESMF_XGridGeomBaseValidate - Validate GeomBase internals

! !INTERFACE:
  subroutine ESMF_XGridGeomBaseValidate(gridbase, rc)
!
! !ARGUMENTS:
    type(ESMF_XGridGeomBase), intent(in)         :: gridbase
    integer,             intent(out),  optional  :: rc
!
!
! !DESCRIPTION:
!      Validates that the {\tt GeomBase} is internally consistent.
!
!     The arguments are:
!     \begin{description}
!     \item[gridbase]
!          Specified {\tt ESMF\_XGridGeomBase} object.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    integer :: localrc
    type(ESMF_XGridGeomBaseClass),pointer :: gbcp

    ! Initialize return code; assume failure until success is certain
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP_SHORT(ESMF_XGridGeomBaseGetInit, gridbase, rc)

    ! Get GeomBaseClass
    gbcp=>gridbase%gbcp

    ! Get info depending on type
    select case(gbcp%type%type)

       case (ESMF_XGRIDGEOMTYPE_GRID%type) ! Grid
         call ESMF_GridValidate(grid=gbcp%grid, rc=localrc)
          if (ESMF_LogFoundError(localrc, &
     ESMF_ERR_PASSTHRU, &
     ESMF_CONTEXT, rcToReturn=rc)) return

       case (ESMF_XGRIDGEOMTYPE_MESH%type) ! Mesh
         !call ESMF_MeshValidate(mesh=gbcp%mesh, rc=localrc)
         ! if (ESMF_LogFoundError(localrc, &
         !                        ESMF_ERR_PASSTHRU, &
         !                        ESMF_CONTEXT, rcToReturn=rc)) return

       case default
         if (ESMF_LogFoundError(ESMF_RC_ARG_VALUE, &
     msg=" Bad type value", &
     ESMF_CONTEXT, rcToReturn=rc)) return
    end select


    ! Set return value
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_XGridGeomBaseValidate
!------------------------------------------------------------------------------

! -------------------------- ESMF-internal method -----------------------------
#undef ESMF_METHOD
#define ESMF_METHOD "ESMF_XGridGeomBaseGetInit"
!BOPI
! !IROUTINE: ESMF_XGridGeomBaseGetInit - Internal access routine for init code
!
! !INTERFACE:
      function ESMF_XGridGeomBaseGetInit(gridbase)
!
! !RETURN VALUE:
      ESMF_INIT_TYPE :: ESMF_XGridGeomBaseGetInit
!
! !ARGUMENTS:
      type(ESMF_XGridGeomBase), intent(in), optional :: gridbase
!
! !DESCRIPTION:
! Access deep object init code.
!
! The arguments are:
! \begin{description}
! \item [gridbase]
! Grid Base object.
! \end{description}
!
!EOPI

    if (present(gridbase)) then
      ESMF_XGridGeomBaseGetInit = ESMF_INIT_GET(gridbase)
    else
      ESMF_XGridGeomBaseGetInit = ESMF_INIT_CREATED
    endif

    end function ESMF_XGridGeomBaseGetInit

!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_XGridGeomTypeEqual"
!BOPI
! !IROUTINE: ESMF_XGridGeomTypeEqual - Equality of GeomTypes
!
! !INTERFACE:
      function ESMF_XGridGeomTypeEqual(GeomType1, GeomType2)

! !RETURN VALUE:
      logical :: ESMF_XGridGeomTypeEqual

! !ARGUMENTS:

      type (ESMF_XGridGeomType_Flag), intent(in) :: &
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

      ESMF_XGridGeomTypeEqual = (GeomType1%type == &
                              GeomType2%type)

      end function ESMF_XGridGeomTypeEqual
!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_XGridGeomTypeNotEqual"
!BOPI
! !IROUTINE: ESMF_XGridGeomTypeNotEqual - Non-equality of GeomTypes
!
! !INTERFACE:
      function ESMF_XGridGeomTypeNotEqual(GeomType1, GeomType2)

! !RETURN VALUE:
      logical :: ESMF_XGridGeomTypeNotEqual

! !ARGUMENTS:

      type (ESMF_XGridGeomType_Flag), intent(in) :: &
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

      ESMF_XGridGeomTypeNotEqual = (GeomType1%type /= &
                                 GeomType2%type)

      end function ESMF_XGridGeomTypeNotEqual


#undef  ESMF_METHOD

end module ESMF_XGridGeomBaseMod


  subroutine f_esmf_xgridgeombasecolgarbage(gb, rc)
#undef  ESMF_METHOD
#define ESMF_METHOD "f_esmf_geombasecolgarbage()"
    use ESMF_UtilTypesMod
    use ESMF_LogErrMod
    use ESMF_XGridGeomBaseMod

    implicit none

    type(ESMF_XGridGeomBase)  :: gb
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

  end subroutine f_esmf_xgridgeombasecolgarbage
