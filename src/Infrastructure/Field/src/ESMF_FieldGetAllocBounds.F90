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
#define ESMF_FILENAME "ESMF_FieldGetAllocBounds.F90"
!==============================================================================
!
!     ESMF FieldGetAllocBounds module
module ESMF_FieldGetAllocBoundsMod
!
!==============================================================================
!
! This file contains the Field Get Allocation Bounds methods.
!
!------------------------------------------------------------------------------
! INCLUDES
#include "ESMF.h"

!------------------------------------------------------------------------------
!
!BOPI
! !MODULE: ESMF_FieldGetAllocBoundsMod - Get Field allocation bounds
!
! !DESCRIPTION:
! The code in this file implements the {\tt ESMF\_Field} allocation bounds 
! computation methods.
!
!------------------------------------------------------------------------------
! !USES:
  use ESMF_UtilTypesMod
  use ESMF_BaseMod
  use ESMF_LogErrMod
  use ESMF_ArraySpecMod
  use ESMF_LocalArrayMod
  use ESMF_ArrayMod
  use ESMF_ArrayGetMod
  use ESMF_GridMod
  use ESMF_XGridMod
  use ESMF_MeshMod
  use ESMF_LocStreamMod
  use ESMF_GeomBaseMod
  use ESMF_StaggerLocMod
  use ESMF_InitMacrosMod
  
  use ESMF_FieldMod

  implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
  private

!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:
!
! - ESMF-public methods:
   public ESMF_GridGetFieldBounds
   public ESMF_MeshGetFieldBounds
   public ESMF_LocStreamGetFieldBounds
   public ESMF_XGridGetFieldBounds
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

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridGetFieldBounds"
!BOP
! !IROUTINE: ESMF_GridGetFieldBounds -  Get precomputed DE-local Fortran data array bounds for creating a Field from a Grid and Fortran array

! !INTERFACE:
    subroutine ESMF_GridGetFieldBounds(grid, keywordEnforcer, &
        localDe, staggerloc, gridToFieldMap, &
        ungriddedLBound, ungriddedUBound, &
        totalLWidth, totalUWidth, &
        totalLBound, totalUBound, totalCount, rc)
    
! !ARGUMENTS:
    type(ESMF_Grid),       intent(in)            :: grid     
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,               intent(in),  optional :: localDe
    type(ESMF_StaggerLoc), intent(in),  optional :: staggerloc 
    integer,               intent(in),  optional :: gridToFieldMap(:)    
    integer,               intent(in),  optional :: ungriddedLBound(:)
    integer,               intent(in),  optional :: ungriddedUBound(:)
    integer,               intent(in),  optional :: totalLWidth(:)
    integer,               intent(in),  optional :: totalUWidth(:)
    integer,               intent(out), optional :: totalLBound(:)
    integer,               intent(out), optional :: totalUBound(:)
    integer,               intent(out), optional :: totalCount(:)
    integer,               intent(out), optional :: rc     

!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
! Compute the lower and upper bounds of Fortran data array that can later
! be used in FieldCreate interface to create a {\tt ESMF\_Field} from a
! {\tt ESMF\_Grid} and the Fortran data array. For an example and
! associated documentation using this method see section 
! \ref{sec:field:usage:create_5dgrid_7dptr_2dungridded}.
!
! The arguments are:
! \begin{description}
! \item [grid]
!       {\tt ESMF\_Grid}.
! \item [{[localDe]}]
!       Local DE for which information is requested. {\tt [0,..,localDeCount-1]}.
!       For {\tt localDeCount==1} the {\tt localDe} argument may be omitted,
!       in which case it will default to {\tt localDe=0}.
! \item [{[staggerloc]}]
!       Stagger location of data in grid cells.  For valid
!       predefined values and interpretation of results see
!       section \ref{const:staggerloc}.
! \item [{[gridToFieldMap]}]
!       List with number of elements equal to the
!       {\tt grid}|s dimCount.  The list elements map each dimension
!       of the {\tt grid} to a dimension in the {\tt field} by
!       specifying the appropriate {\tt field} dimension index. The default is to
!       map all of the {\tt grid}|s dimensions against the lowest dimensions of
!       the {\tt field} in sequence, i.e. {\tt gridToFieldMap} = (/1,2,3,.../).
!       The values of all {\tt gridToFieldMap} entries must be greater than or equal
!       to one and smaller than or equal to the {\tt field} rank.
!       It is erroneous to specify the same {\tt gridToFieldMap} entry
!       multiple times. The total ungridded dimensions in the {\tt field}
!       are the total {\tt field} dimensions less
!       the dimensions in
!       the {\tt grid}.  Ungridded dimensions must be in the same order they are
!       stored in the {\tt field}.  
! \item [{[ungriddedLBound]}]
!       Lower bounds of the ungridded dimensions of the {\tt field}.
!       The number of elements in the {\tt ungriddedLBound} is equal to the number of ungridded
!       dimensions in the {\tt field}.  All ungridded dimensions of the
!       {\tt field} are also undistributed. When field dimension count is
!       greater than grid dimension count, both ungriddedLBound and ungriddedUBound
!       must be specified. When both are specified the values are checked
!       for consistency.  Note that the the ordering of
!       these ungridded dimensions is the same as their order in the {\tt field}.
! \item [{[ungriddedUBound]}]
!       Upper bounds of the ungridded dimensions of the {\tt field}.
!       The number of elements in the {\tt ungriddedUBound} is equal to the number of ungridded
!       dimensions in the {\tt field}.  All ungridded dimensions of the
!       {\tt field} are also undistributed. When field dimension count is
!       greater than grid dimension count, both ungriddedLBound and ungriddedUBound
!       must be specified. When both are specified the values are checked
!       for consistency.  Note that the the ordering of
!       these ungridded dimensions is the same as their order in the {\tt field}.
! \item [{[totalLWidth]}]
!       Lower bound of halo region.  The size of this array is the number
!       of dimensions in the {\tt grid}.  However, ordering of the elements
!       needs to be the same as they appear in the {\tt field}.  Values default
!       to 0.  If values for totalLWidth are specified they must be reflected in
!       the size of the {\tt field}.  That is, for each gridded dimension the
!       {\tt field} size should be max( {\tt totalLWidth} + {\tt totalUWidth}
!       + {\tt computationalCount}, {\tt exclusiveCount} ).
! \item [{[totalUWidth]}]
!       Upper bound of halo region.  The size of this array is the number
!       of dimensions in the {\tt grid}.  However, ordering of the elements
!       needs to be the same as they appear in the {\tt field}.  Values default
!       to 0.  If values for totalUWidth are specified they must be reflected in
!       the size of the {\tt field}.  That is, for each gridded dimension the
!       {\tt field} size should max( {\tt totalLWidth} + {\tt totalUWidth}
!       + {\tt computationalCount}, {\tt exclusiveCount} ).
! \item [{[totalLBound]}]
!       \begin{sloppypar}
!       The relative lower bounds of Fortran data array to be used
!       later in {\tt ESMF\_FieldCreate} from {\tt ESMF\_Grid} and Fortran data array.
!       This is an output variable from this user interface.
!       \end{sloppypar}
!       The relative lower bounds of Fortran data array to be used
! \item [{[totalUBound]}]
!       \begin{sloppypar}
!       The relative upper bounds of Fortran data array to be used
!       later in {\tt ESMF\_FieldCreate} from {\tt ESMF\_Grid} and Fortran data array.
!       This is an output variable from this user interface.
!       \end{sloppypar}
! \item [{[totalCount]}]
!       Number of elements need to be allocated for Fortran data array to be used
!       later in {\tt ESMF\_FieldCreate} from {\tt ESMF\_Grid} and Fortran data array.
!       This is an output variable from this user interface.
!
! \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!EOP

!  !Local Variables
    integer :: localrc
    type(ESMF_STAGGERLOC)                  :: l_staggerloc
    type(ESMF_GeomBase) :: geombase

    ! Initialize
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ESMF_INIT_CHECK_DEEP(ESMF_GridGetInit,grid,rc)

    ! default staggerloc setup
    if(present(staggerloc)) then
        l_staggerloc = staggerloc
    else
        l_staggerloc = ESMF_STAGGERLOC_CENTER
    endif

     ! Create GeomBase from Grid
    geombase=ESMF_GeomBaseCreate(grid,l_staggerloc, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
 

    ! call into generic alloc bound calculation subroutine
    call ESMF_FieldGetGBAllocBounds(geombase, localDe=localDe, &
        gridToFieldMap=gridToFieldMap, &
        ungriddedLBound=ungriddedLBound, ungriddedUBound=ungriddedUBound, &
        totalLWidth=totalLWidth, totalUWidth=totalUWidth, &
        totalLBound=totalLBound, totalUBound=totalUBound, &
        totalCount=totalCount, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    ! Destroy GeomBase
    call ESMF_GeomBaseDestroy(geombase, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    if (present(rc)) rc = ESMF_SUCCESS
    end subroutine ESMF_GridGetFieldBounds

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_LocStreamGetFieldBounds"
!BOP
! !IROUTINE: ESMF_LocStreamGetFieldBounds -  Get precomputed DE-local Fortran data array bounds for creating a Field from a LocStream and Fortran array

! !INTERFACE:
    subroutine ESMF_LocStreamGetFieldBounds(locstream, keywordEnforcer, &
        localDe, gridToFieldMap, &
        ungriddedLBound, ungriddedUBound, &
        totalLBound, totalUBound, totalCount, rc)
    
! !ARGUMENTS:
    type(ESMF_LocStream), intent(in)            :: locstream     
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,              intent(in),  optional :: localDe
    integer,              intent(in),  optional :: gridToFieldMap(:)    
    integer,              intent(in),  optional :: ungriddedLBound(:)
    integer,              intent(in),  optional :: ungriddedUBound(:)
    integer,              intent(out), optional :: totalLBound(:)
    integer,              intent(out), optional :: totalUBound(:)
    integer,              intent(out), optional :: totalCount(:)
    integer,              intent(out), optional :: rc     

!
! !DESCRIPTION:
! Compute the lower and upper bounds of Fortran data array that can later
! be used in FieldCreate interface to create a {\tt ESMF\_Field} from a
! {\tt ESMF\_LocStream} and the Fortran data array.  For an example and
! associated documentation using this method see section 
! \ref{sec:field:usage:create_5dgrid_7dptr_2dungridded}.
!
! The arguments are:
! \begin{description}
! \item [locstream]
!       {\tt ESMF\_LocStream}.
! \item [{[localDe]}]
!       Local DE for which information is requested. {\tt [0,..,localDeCount-1]}.
!       For {\tt localDeCount==1} the {\tt localDe} argument may be omitted,
!       in which case it will default to {\tt localDe=0}.
! \item [{[gridToFieldMap]}]
!       List with number of elements equal to 1.
!       The list elements map the dimension
!       of the {\tt locstream} to a dimension in the {\tt field} by
!       specifying the appropriate {\tt field} dimension index. The default is to
!       map the {\tt locstream}|s dimension against the lowest dimension of
!       the {\tt field} in sequence, i.e. {\tt gridToFieldMap} = (/1/).
!       The values of all {\tt gridToFieldMap} entries must be greater than or equal
!       to one and smaller than or equal to the {\tt field} rank.
!       The total ungridded dimensions in the {\tt field}
!       are the total {\tt field} dimensions less
!       the dimensions in
!       the {\tt grid}.  Ungridded dimensions must be in the same order they are
!       stored in the {\t field}.  
! \item [{[ungriddedLBound]}]
!       Lower bounds of the ungridded dimensions of the {\tt field}.
!       The number of elements in the {\tt ungriddedLBound} is equal to the number of ungridded
!       dimensions in the {\tt field}.  All ungridded dimensions of the
!       {\tt field} are also undistributed. When field dimension count is
!       greater than 1, both ungriddedLBound and ungriddedUBound
!       must be specified. When both are specified the values are checked
!       for consistency.  Note that the the ordering of
!       these ungridded dimensions is the same as their order in the {\tt field}.
! \item [{[ungriddedUBound]}]
!       Upper bounds of the ungridded dimensions of the {\tt field}.
!       The number of elements in the {\tt ungriddedUBound} is equal to the number of ungridded
!       dimensions in the {\tt field}.  All ungridded dimensions of the
!       {\tt field} are also undistributed. When field dimension count is
!       greater than 1, both ungriddedLBound and ungriddedUBound
!       must be specified. When both are specified the values are checked
!       for consistency.  Note that the the ordering of
!       these ungridded dimensions is the same as their order in the {\tt field}.
! \item [{[totalLBound]}]
!       \begin{sloppypar}
!       The relative lower bounds of Fortran data array to be used
!       later in {\tt ESMF\_FieldCreate} from {\tt ESMF\_LocStream} and Fortran data array.
!       This is an output variable from this user interface.
!       \end{sloppypar}
! \item [{[totalUBound]}]
!       \begin{sloppypar}
!       The relative upper bounds of Fortran data array to be used
!       later in {\tt ESMF\_FieldCreate} from {\tt ESMF\_LocStream} and Fortran data array.
!       This is an output variable from this user interface.
!       \end{sloppypar}
! \item [{[totalCount]}]
!       Number of elements need to be allocated for Fortran data array to be used
!       later in {\tt ESMF\_FieldCreate} from {\tt ESMF\_LocStream} and Fortran data array.
!       This is an output variable from this user interface.
!
! \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!EOP

!  !Local Variables
    integer :: localrc
    type(ESMF_GeomBase) :: geombase

    ! Initialize
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ESMF_INIT_CHECK_DEEP(ESMF_LocStreamGetInit,locstream,rc)

     ! Create GeomBase from LocStream
    geombase=ESMF_GeomBaseCreate(locstream,rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
 

    ! call into generic alloc bound calculation subroutine
    call ESMF_FieldGetGBAllocBounds(geombase, localDe=localDe, &
        gridToFieldMap=gridToFieldMap, &
        ungriddedLBound=ungriddedLBound, ungriddedUBound=ungriddedUBound, &
        totalLBound=totalLBound, totalUBound=totalUBound, &
        totalCount=totalCount, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    ! Destroy GeomBase
    call ESMF_GeomBaseDestroy(geombase, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    if (present(rc)) rc = ESMF_SUCCESS
    end subroutine ESMF_LocStreamGetFieldBounds


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_MeshGetFieldBounds"
!BOP
! !IROUTINE: ESMF_MeshGetFieldBounds -  Get precomputed DE-local Fortran data array bounds for creating a Field from a Mesh and a Fortran array

! !INTERFACE:
    subroutine ESMF_MeshGetFieldBounds(mesh, keywordEnforcer, &
        localDe, gridToFieldMap, &
        ungriddedLBound, ungriddedUBound, &
        totalLBound, totalUBound, totalCount, rc)
    
! !ARGUMENTS:
    type(ESMF_Mesh), intent(in)            :: mesh     
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,         intent(in),  optional :: localDe
    integer,         intent(in),  optional :: gridToFieldMap(:)    
    integer,         intent(in),  optional :: ungriddedLBound(:)
    integer,         intent(in),  optional :: ungriddedUBound(:)
    integer,         intent(out), optional :: totalLBound(:)
    integer,         intent(out), optional :: totalUBound(:)
    integer,         intent(out), optional :: totalCount(:)
    integer,         intent(out), optional :: rc     

!
! !DESCRIPTION:
! Compute the lower and upper bounds of Fortran data array that can later
! be used in FieldCreate interface to create a {\tt ESMF\_Field} from a
! {\tt ESMF\_Mesh} and the Fortran data array. For an example and
! associated documentation using this method see section 
! \ref{sec:field:usage:create_5dgrid_7dptr_2dungridded}.
!
! The arguments are:
! \begin{description}
! \item [mesh]
!       {\tt ESMF\_Mesh}.
! \item [{[localDe]}]
!       Local DE for which information is requested. {\tt [0,..,localDeCount-1]}.
!       For {\tt localDeCount==1} the {\tt localDe} argument may be omitted,
!       in which case it will default to {\tt localDe=0}.
! \item [{[gridToFieldMap]}]
!       List with number of elements equal to the
!       {\tt grid}|s dimCount.  The list elements map each dimension
!       of the {\tt grid} to a dimension in the {\tt field} by
!       specifying the appropriate {\tt field} dimension index. The default is to
!       map all of the {\tt grid}|s dimensions against the lowest dimensions of
!       the {\tt field} in sequence, i.e. {\tt gridToFieldMap} = (/1,2,3,.../).
!       The values of all {\tt gridToFieldMap} entries must be greater than or equal
!       to one and smaller than or equal to the {\tt field} rank.
!       It is erroneous to specify the same {\tt gridToFieldMap} entry
!       multiple times. The total ungridded dimensions in the {\tt field}
!       are the total {\tt field} dimensions less
!       the dimensions in
!       the {\tt grid}.  Ungridded dimensions must be in the same order they are
!       stored in the {\t field}.  
! \item [{[ungriddedLBound]}]
!       Lower bounds of the ungridded dimensions of the {\tt field}.
!       The number of elements in the {\tt ungriddedLBound} is equal to the number of ungridded
!       dimensions in the {\tt field}.  All ungridded dimensions of the
!       {\tt field} are also undistributed. When field dimension count is
!       greater than grid dimension count, both ungriddedLBound and ungriddedUBound
!       must be specified. When both are specified the values are checked
!       for consistency.  Note that the the ordering of
!       these ungridded dimensions is the same as their order in the {\tt field}.
! \item [{[ungriddedUBound]}]
!       Upper bounds of the ungridded dimensions of the {\tt field}.
!       The number of elements in the {\tt ungriddedUBound} is equal to the number of ungridded
!       dimensions in the {\tt field}.  All ungridded dimensions of the
!       {\tt field} are also undistributed. When field dimension count is
!       greater than grid dimension count, both ungriddedLBound and ungriddedUBound
!       must be specified. When both are specified the values are checked
!       for consistency.  Note that the the ordering of
!       these ungridded dimensions is the same as their order in the {\tt field}.
! \item [{[totalLBound]}]
!       \begin{sloppypar}
!       The relative lower bounds of Fortran data array to be used
!       later in {\tt ESMF\_FieldCreate} from {\tt ESMF\_Mesh} and Fortran data array.
!       This is an output variable from this user interface.
!       \end{sloppypar}
! \item [{[totalUBound]}]
!       \begin{sloppypar}
!       The relative upper bounds of Fortran data array to be used
!       later in {\tt ESMF\_FieldCreate} from {\tt ESMF\_Mesh} and Fortran data array.
!       This is an output variable from this user interface.
!       \end{sloppypar}
! \item [{[totalCount]}]
!       Number of elements need to be allocated for Fortran data array to be used
!       later in {\tt ESMF\_FieldCreate} from {\tt ESMF\_Mesh} and Fortran data array.
!       This is an output variable from this user interface.
!
! \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!EOP

!  !Local Variables
    integer :: localrc
    type(ESMF_GeomBase) :: geombase

    ! Initialize
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ESMF_INIT_CHECK_DEEP(ESMF_MeshGetInit,mesh,rc)

     ! Create GeomBase from Mesh
    geombase=ESMF_GeomBaseCreate(mesh,rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
 

    ! call into generic alloc bound calculation subroutine
    call ESMF_FieldGetGBAllocBounds(geombase, localDe=localDe, &
        gridToFieldMap=gridToFieldMap, &
        ungriddedLBound=ungriddedLBound, ungriddedUBound=ungriddedUBound, &
        totalLBound=totalLBound, totalUBound=totalUBound, &
        totalCount=totalCount, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    ! Destroy GeomBase
    call ESMF_GeomBaseDestroy(geombase, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    if (present(rc)) rc = ESMF_SUCCESS
    end subroutine ESMF_MeshGetFieldBounds

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_XGridGetFieldBounds"
!BOP
! !IROUTINE: ESMF_XGridGetFieldBounds -  Get precomputed DE-local Fortran data array bounds for creating a Field from an XGrid and a Fortran array

! !INTERFACE:
    subroutine ESMF_XGridGetFieldBounds(xgrid, keywordEnforcer, &
        xgridside, gridindex, localDe, gridToFieldMap, &
        ungriddedLBound, ungriddedUBound, &
        totalLBound, totalUBound, totalCount, rc)
    
! !ARGUMENTS:
    type(ESMF_XGrid),          intent(in)            :: xgrid
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_XGridSide_Flag), intent(in),  optional :: xgridside
    integer,                   intent(in),  optional :: gridindex
    integer,                   intent(in),  optional :: localDe
    integer,                   intent(in),  optional :: gridToFieldMap(:)    
    integer,                   intent(in),  optional :: ungriddedLBound(:)
    integer,                   intent(in),  optional :: ungriddedUBound(:)
    integer,                   intent(out), optional :: totalLBound(:)
    integer,                   intent(out), optional :: totalUBound(:)
    integer,                   intent(out), optional :: totalCount(:)
    integer,                   intent(out), optional :: rc     

!
! !DESCRIPTION:
! Compute the lower and upper bounds of Fortran data array that can later
! be used in FieldCreate interface to create a {\tt ESMF\_Field} from a
! {\tt ESMF\_XGrid} and the Fortran data array.  For an example and
! associated documentation using this method see section 
! \ref{sec:field:usage:create_5dgrid_7dptr_2dungridded}.
!
! The arguments are:
! \begin{description}
! \item [xgrid]
!        {\tt ESMF\_XGrid} object.
! \item [{[xgridside]}]
!       Which side of the XGrid to create the Field on (either ESMF\_XGRIDSIDE\_A,
!       ESMF\_XGRIDSIDE\_B, or ESMF\_XGRIDSIDE\_BALANCED). If not passed in then
!       defaults to ESMF\_XGRIDSIDE\_BALANCED.
! \item [{[gridindex]}]
!       If xgridside is ESMF\_XGRIDSIDE\_A or ESMF\_XGRIDSIDE\_B then this index tells which Grid on
!       that side to create the Field on. If not provided, defaults to 1.
! \item [{[localDe]}]
!       Local DE for which information is requested. {\tt [0,..,localDeCount-1]}.
!       For {\tt localDeCount==1} the {\tt localDe} argument may be omitted,
!       in which case it will default to {\tt localDe=0}.
! \item [{[gridToFieldMap]}]
!       List with number of elements equal to 1.
!       The list elements map the dimension
!       of the {\tt locstream} to a dimension in the {\tt field} by
!       specifying the appropriate {\tt field} dimension index. The default is to
!       map the {\tt locstream}|s dimension against the lowest dimension of
!       the {\tt field} in sequence, i.e. {\tt gridToFieldMap} = (/1/).
!       The values of all {\tt gridToFieldMap} entries must be greater than or equal
!       to one and smaller than or equal to the {\tt field} rank.
!       The total ungridded dimensions in the {\tt field}
!       are the total {\tt field} dimensions less
!       the dimensions in
!       the {\tt grid}.  Ungridded dimensions must be in the same order they are
!       stored in the {\t field}.  
! \item [{[ungriddedLBound]}]
!       Lower bounds of the ungridded dimensions of the {\tt field}.
!       The number of elements in the {\tt ungriddedLBound} is equal to the number of ungridded
!       dimensions in the {\tt field}.  All ungridded dimensions of the
!       {\tt field} are also undistributed. When field dimension count is
!       greater than 1, both ungriddedLBound and ungriddedUBound
!       must be specified. When both are specified the values are checked
!       for consistency.  Note that the the ordering of
!       these ungridded dimensions is the same as their order in the {\tt field}.
! \item [{[ungriddedUBound]}]
!       Upper bounds of the ungridded dimensions of the {\tt field}.
!       The number of elements in the {\tt ungriddedUBound} is equal to the number of ungridded
!       dimensions in the {\tt field}.  All ungridded dimensions of the
!       {\tt field} are also undistributed. When field dimension count is
!       greater than 1, both ungriddedLBound and ungriddedUBound
!       must be specified. When both are specified the values are checked
!       for consistency.  Note that the the ordering of
!       these ungridded dimensions is the same as their order in the {\tt field}.
! \item [{[totalLBound]}]
!       \begin{sloppypar}
!       The relative lower bounds of Fortran data array to be used
!       later in {\tt ESMF\_FieldCreate} from {\tt ESMF\_LocStream} and Fortran data array.
!       This is an output variable from this user interface.
!       \end{sloppypar}
! \item [{[totalUBound]}]
!       \begin{sloppypar}
!       The relative upper bounds of Fortran data array to be used
!       later in {\tt ESMF\_FieldCreate} from {\tt ESMF\_LocStream} and Fortran data array.
!       This is an output variable from this user interface.
!       \end{sloppypar}
! \item [{[totalCount]}]
!       Number of elements need to be allocated for Fortran data array to be used
!       later in {\tt ESMF\_FieldCreate} from {\tt ESMF\_LocStream} and Fortran data array.
!       This is an output variable from this user interface.
!
! \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!EOP

!  !Local Variables
    integer :: localrc
    type(ESMF_GeomBase) :: geombase

    ! Initialize
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ESMF_INIT_CHECK_DEEP(ESMF_XGridGetInit, xgrid, rc)

     ! Create GeomBase from LocStream
    geombase=ESMF_GeomBaseCreate(xgrid, xgridSide, gridIndex, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
 

    ! call into generic alloc bound calculation subroutine
    call ESMF_FieldGetGBAllocBounds(geombase, localDe=localDe, &
        gridToFieldMap=gridToFieldMap, &
        ungriddedLBound=ungriddedLBound, ungriddedUBound=ungriddedUBound, &
        totalLBound=totalLBound, totalUBound=totalUBound, &
        totalCount=totalCount, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    ! Destroy GeomBase
    call ESMF_GeomBaseDestroy(geombase, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    if (present(rc)) rc = ESMF_SUCCESS
    end subroutine ESMF_XGridGetFieldBounds

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldGetGBAllocBounds"
!BOPI
! !IROUTINE: ESMF_FieldGet -  Get precomputed Fortran data array bounds

! !INTERFACE:
  ! Private name; call using ESMF_FieldGet()
    subroutine ESMF_FieldGetGBAllocBounds(geombase, &
        localDe, gridToFieldMap, &
        ungriddedLBound, ungriddedUBound, &
        totalLWidth, totalUWidth, &
        totalLBound, totalUBound, totalCount, rc)
    
! !ARGUMENTS:
    type(ESMF_GeomBase), intent(inout)         :: geombase     
    integer,             intent(in),  optional :: localDe
    integer,             intent(in),  optional :: gridToFieldMap(:)    
    integer,             intent(in),  optional :: ungriddedLBound(:)
    integer,             intent(in),  optional :: ungriddedUBound(:)
    integer,             intent(in),  optional :: totalLWidth(:)
    integer,             intent(in),  optional :: totalUWidth(:)
    integer,             intent(out), optional :: totalLBound(:)
    integer,             intent(out), optional :: totalUBound(:)
    integer,             intent(out), optional :: totalCount(:)
    integer,             intent(out), optional :: rc     

!
! !DESCRIPTION:
! Compute the lower and upper bounds of Fortran data array that can later
! be used in FieldCreate interface to create a {\tt ESMF\_Field} from a
! {\tt ESMF\_Grid} and the Fortran data array. For an example and
! associated documentation using this method see section 
! \ref{sec:field:usage:create_5dgrid_7dptr_2dungridded}.
!
! The arguments are:
! \begin{description}
! \item [geombase]
!       {\tt ESMF\_GeomBase}.
! \item [localDe]
!       The local DE number in its PET context to compute the bounds and counts
!       information based on the computational and exclusive bounds and counts 
!       information of the grid from that local DE in its PET context.
! \item [{[gridToFieldMap]}]
!       List with number of elements equal to the
!       {\tt grid}|s dimCount.  The list elements map each dimension
!       of the {\tt grid} to a dimension in the {\tt field} by
!       specifying the appropriate {\tt field} dimension index. The default is to
!       map all of the {\tt grid}|s dimensions against the lowest dimensions of
!       the {\tt field} in sequence, i.e. {\tt gridToFieldMap} = (/1,2,3,.../).
!       The total ungridded dimensions in the {\tt field}
!       are the total {\tt field} dimensions less
!       the dimensions in
!       the {\tt grid}.  Ungridded dimensions must be in the same order they are
!       stored in the {\t field}.  
! \item [{[ungriddedLBound]}]
!       Lower bounds of the ungridded dimensions of the {\tt field}.
!       The number of elements in the {\tt ungriddedLBound} is equal to the number of ungridded
!       dimensions in the {\tt field}.  All ungridded dimensions of the
!       {\tt field} are also undistributed. When field dimension count is
!       greater than grid dimension count, both ungriddedLBound and ungriddedUBound
!       must be specified. When both are specified the values are checked
!       for consistency.  Note that the the ordering of
!       these ungridded dimensions is the same as their order in the {\tt field}.
! \item [{[ungriddedUBound]}]
!       Upper bounds of the ungridded dimensions of the {\tt field}.
!       The number of elements in the {\tt ungriddedUBound} is equal to the number of ungridded
!       dimensions in the {\tt field}.  All ungridded dimensions of the
!       {\tt field} are also undistributed. When field dimension count is
!       greater than grid dimension count, both ungriddedLBound and ungriddedUBound
!       must be specified. When both are specified the values are checked
!       for consistency.  Note that the the ordering of
!       these ungridded dimensions is the same as their order in the {\tt field}.
! \item [{[totalLWidth]}]
!       Lower bound of halo region.  The size of this array is the number
!       of gridded dimensions in the {\tt field}.  However, ordering of the elements
!       needs to be the same as they appear in the {\tt field}.  Values default
!       to 0.  If values for totalLWidth are specified they must be reflected in
!       the size of the {\tt field}.  That is, for each gridded dimension the
!       {\tt field} size should be max( {\tt totalLWidth} + {\tt totalUWidth}
!       + {\tt computationalCount}, {\tt exclusiveCount} ).
! \item [{[totalUWidth]}]
!       Upper bound of halo region.  The size of this array is the number
!       of gridded dimensions in the {\tt field}.  However, ordering of the elements
!       needs to be the same as they appear in the {\tt field}.  Values default
!       to 0.  If values for totalUWidth are specified they must be reflected in
!       the size of the {\tt field}.  That is, for each gridded dimension the
!       {\tt field} size should max( {\tt totalLWidth} + {\tt totalUWidth}
!       + {\tt computationalCount}, {\tt exclusiveCount} ).
! \item [{[totalLBound]}]
!       \begin{sloppypar}
!       The relative lower bounds of Fortran data array to be used
!       later in {\tt ESMF\_FieldCreate} from {\tt ESMF\_Grid} and Fortran data array.
!       This is an output variable from this user interface.
!       \end{sloppypar}
! \item [{[totalUBound]}]
!       \begin{sloppypar}
!       The relative upper bounds of Fortran data array to be used
!       later in {\tt ESMF\_FieldCreate} from {\tt ESMF\_Grid} and Fortran data array.
!       This is an output variable from this user interface.
!       \end{sloppypar}
! \item [{[totalCount]}]
!       Number of elements need to be allocated for Fortran data array to be used
!       later in {\tt ESMF\_FieldCreate} from {\tt ESMF\_Grid} and Fortran data array.
!       This is an output variable from this user interface.
!
! \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!EOPI

!  !Local Variables
    integer :: localrc

!   temporary local variables corresponding to input/output arguments
    integer, dimension(ESMF_MAXDIM)        :: l_g2fm, l_mhlw, l_mhuw
    integer, dimension(:), allocatable     :: l_uglb, l_ugub
    integer, dimension(:), allocatable     :: l_alb, l_aub, l_ac

!   internal local variables 
    integer, dimension(ESMF_MAXDIM)        :: ec, dg2gm
    integer, dimension(ESMF_MAXDIM)        :: f2gm, gelb, geub
    logical, dimension(ESMF_MAXDIM)        :: flipflop
    integer                                :: forderIndex, i
    integer                                :: gridrank, arrayrank, uglb_size, ugub_size
    integer                                :: grid_repdimcount, gridrank_norep
    integer                                :: localDeCount, l_localDe


    ! Initialize
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ESMF_INIT_CHECK_DEEP(ESMF_GeomBaseGetInit,geombase,rc)

    call ESMF_GeomBaseGet(geombase, localDeCount=localDeCount, &
      dimCount=gridrank, distgridToGridMap=dg2gm, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    ! default localDe    
    if(localDeCount .gt. 1 .and. (.not. present(localDe))) then
       call ESMF_LogSetError(ESMF_RC_ARG_VALUE, &
          msg="localDe must be present when localDeCount is greater than 1", &
           ESMF_CONTEXT, rcToReturn=rc)
       return
    endif 
    if(present(localDe)) then
        l_localDe = localDe
    else 
        l_localDe = 0
    endif

    call ESMF_GeomBaseGetPLocalDE(geombase, localDe=l_localDe, &
       exclusiveLBound=gelb, exclusiveUBound=geub, exclusiveCount=ec, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    ! Validate input arguments
    if(present(gridToFieldMap) ) then
        if(size(gridToFieldMap) .ne. gridrank) then
           call ESMF_LogSetError(ESMF_RC_ARG_VALUE, &
              msg="gridToFieldMap size must equal to grid dimension count", &
               ESMF_CONTEXT, rcToReturn=rc)
           return
        endif 
    endif

    ! set up local gridToFieldMap
    grid_repdimcount = 0
    if(present(gridToFieldMap)) then
        l_g2fm(1:size(gridToFieldMap)) = gridToFieldMap
        do i = 1, size(gridToFieldMap)
            if(gridToFieldMap(i) == 0) grid_repdimcount = grid_repdimcount + 1
        enddo
    else
        do i = 1, ESMF_MAXDIM
            l_g2fm(i) = i
        enddo
    endif
    gridrank_norep = gridrank - grid_repdimcount
    ! gridToFieldMap elements must be in range 0...fieldRank and unique  
    ! algorithm to check element uniqueness:  
    !   run time: O(ESMF_MAXDIM)  
    !   memory:   O(2*ESMF_MAXDIM)  
    !          or O(ESMF_MAXDIM+ESMF_MAXDIM/sizeof(integer)) with bitvector  
    flipflop = .false.  
    do i = 1, gridrank
       if(l_g2fm(i) .lt. 0 .and. l_g2fm(i) .gt. arrayrank) then  
           call ESMF_LogSetError(ESMF_RC_ARG_VALUE, &   
                 msg="- gridToFieldMap element must be within range 0...array rank", &  
                   ESMF_CONTEXT, rcToReturn=rc)   
           return  
       endif  
       if(l_g2fm(i) /= 0) then
           if(flipflop(l_g2fm(i))) then  
               call ESMF_LogSetError(ESMF_RC_ARG_VALUE, &   
                     msg="- gridToFieldMap element must be unique", &  
                       ESMF_CONTEXT, rcToReturn=rc)   
               return  
           endif  
           flipflop(l_g2fm(i)) = .true.  
       endif
    enddo  

    ! User must either provide both ungriddedLBound and ungriddedUBound
    ! with same size or do not specify either one of them. There is no
    ! suitable default value for unbounded variables, especially when
    ! the intent is to create a Field with a greater rank than Grid
    if(present(ungriddedLBound)) then
        uglb_size = size(ungriddedLBound)
    else 
        uglb_size = 0
    endif
    if(present(ungriddedUBound)) then
        ugub_size = size(ungriddedUBound)
    else
        ugub_size = 0
    endif

    if(uglb_size .ne. ugub_size) then
       call ESMF_LogSetError(ESMF_RC_ARG_VALUE, &
          msg="ungriddedLBound and ungriddedUBound must have same size", &
           ESMF_CONTEXT, rcToReturn=rc)
       return
    endif 
    if(uglb_size .ne. 0) then
        allocate(l_uglb(uglb_size), l_ugub(ugub_size))
        l_uglb(1:uglb_size) = ungriddedLBound(1:uglb_size)
        l_ugub(1:ugub_size) = ungriddedUBound(1:ugub_size)
    endif

    ! the result Field/array rank
    arrayrank = gridrank + uglb_size
    arrayrank = arrayrank - grid_repdimcount

    ! check argument validity
    if(present(totalLBound)) then
        if(size(totalLBound) .ne. arrayrank) then
           call ESMF_LogSetError(ESMF_RC_ARG_VALUE, &
              msg="totalLBound size must equal to the desired array rank", &
               ESMF_CONTEXT, rcToReturn=rc)
           return
        endif 
    endif
    if(present(totalUBound)) then
        if(size(totalUBound) .ne. arrayrank) then
           call ESMF_LogSetError(ESMF_RC_ARG_VALUE, &
              msg="totalUBound size must equal to the desired array rank", &
               ESMF_CONTEXT, rcToReturn=rc)
           return
        endif 
    endif
    if(present(totalCount)) then
        if(size(totalCount) .ne. arrayrank) then
           call ESMF_LogSetError(ESMF_RC_ARG_VALUE, &
              msg="totalCount size must equal to the desired array rank", &
               ESMF_CONTEXT, rcToReturn=rc)
           return
        endif 
    endif

    if(present(totalLWidth) ) then
        if(size(totalLWidth) .ne. gridrank_norep) then
           call ESMF_LogSetError(ESMF_RC_ARG_VALUE, &
              msg="totalLWidth size must equal to gridded dimension count", &
               ESMF_CONTEXT, rcToReturn=rc)
           return
        endif 
    endif

    if(present(totalUWidth) ) then
        if(size(totalUWidth) .ne. gridrank_norep) then
           call ESMF_LogSetError(ESMF_RC_ARG_VALUE, &
              msg="totalUWidth size must equal to gridded dimension count", &
               ESMF_CONTEXT, rcToReturn=rc)
           return
        endif 
    endif

    ! At this point input arguments are validated
    ! allocate the return value arrays
    allocate(l_alb(arrayrank), l_aub(arrayrank), l_ac(arrayrank))
    l_mhlw = 0
    if(present(totalLWidth)) then
        l_mhlw(1:size(totalLWidth)) = totalLWidth
    endif
    l_mhuw = 0
    if(present(totalUWidth)) then
        l_mhuw(1:size(totalUWidth)) = totalUWidth
    endif

    ! First we compute the ungridded bounds:
    ! compute a reverse mapping from Field to Grid then
    ! compute ungridded Fortran array bounds
    f2gm = 0
    do i = 1, gridrank
        if(l_g2fm(i) /= 0) f2gm(l_g2fm(i)) = i
    enddo
    forderIndex = 1
    ! ungridded bounds info present indicates field has ungridded dimension
    ! otherwise we do not have to worry about this.
    if(uglb_size /= 0) then
        do i = 1, arrayrank
            ! if the i-th dimension is ungridded
            if(f2gm(i) .eq. 0) then
                l_alb(i) = l_uglb(forderIndex)
                l_aub(i) = l_ugub(forderIndex)
                l_ac(i)  = l_aub(i) - l_alb(i) + 1
                forderIndex = forderIndex + 1
            endif
        enddo
    endif
!XXX
    ! Next compute the gridded bounds using the mapping
    ! from Field to Grid computed in last step
    forderIndex = 1
    do i = 1, arrayrank
        ! if i-th dimension is gridded
        if(f2gm(i) .gt. 0) then
            l_ac(i) = ec(f2gm(i))+l_mhlw(forderIndex)+l_mhuw(forderIndex)
            l_alb(i) = gelb(f2gm(i)) - l_mhlw(forderIndex)
            l_aub(i) = l_alb(i) + l_ac(i) - 1
            forderIndex = forderIndex + 1
        endif
    enddo

    ! Prepare the return values
    if(present(totalLBound)) totalLBound(1:arrayrank) = l_alb(1:arrayrank)
    if(present(totalUBound)) totalUBound(1:arrayrank) = l_aub(1:arrayrank)
    if(present(totalCount))  totalCount(1:arrayrank)  = l_ac(1:arrayrank)

    ! deallocate temporary arrays
    if(uglb_size .ne. 0) then
        deallocate(l_uglb, l_ugub)
    endif
    deallocate(l_alb, l_aub, l_ac)

    if (present(rc)) rc = ESMF_SUCCESS
    end subroutine ESMF_FieldGetGBAllocBounds

end module ESMF_FieldGetAllocBoundsMod
