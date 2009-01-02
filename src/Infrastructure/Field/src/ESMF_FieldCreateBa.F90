! $Id: ESMF_FieldCreateBa.F90,v 1.1 2009/01/02 19:48:56 feiliu Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2008, University Corporation for Atmospheric Research, 
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
! Laboratory, University of Michigan, National Centers for Environmental 
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
#define ESMF_FILENAME "ESMF_FieldCreateBa.F90"
!==============================================================================
!
! ESMF FieldCreateBa module
module ESMF_FieldCreateBaMod
!
!==============================================================================
!
! This file contains the Basic FieldCreate() methods.
!
!------------------------------------------------------------------------------
! INCLUDES
! < ignore blank lines below.  they are created by the files which
!   define various macros. >
#include "ESMF.h"

!------------------------------------------------------------------------------
! !USES:
  use ESMF_UtilTypesMod
  use ESMF_BaseMod
  use ESMF_LogErrMod
  use ESMF_IOSpecMod
  use ESMF_ArraySpecMod
  use ESMF_LocalArrayMod
  use ESMF_DELayoutMod
  use ESMF_StaggerLocMod
  use ESMF_GridMod
  use ESMF_MeshMod
  use ESMF_LocStreamMod
  use ESMF_GeomBaseMod
  use ESMF_ArrayMod
  use ESMF_ArrayGetMod
  use ESMF_ArrayCreateMod
  
  use ESMF_FieldMod
  
  implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:

!------------------------------------------------------------------------------
! !PUBLIC MEMBER FUNCTIONS:

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
  character(*), parameter, private :: version = &
    '$Id: ESMF_FieldCreateBa.F90,v 1.1 2009/01/02 19:48:56 feiliu Exp $'

!==============================================================================
! 
! INTERFACE BLOCKS
!
!==============================================================================


! -------------------------- ESMF-public method -------------------------------
!------------------------------------------------------------------------------
  public ESMF_FieldCreateEmpty
  public ESMF_FieldCreateNoData       ! Create a new Field without data
  public ESMF_FieldDestroy            ! Destroy a Field
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_FieldCreateNoData - Create a new Field without data
!
! !INTERFACE:
  interface ESMF_FieldCreateNoData
   
! !PRIVATE MEMBER FUNCTIONS:
    module procedure ESMF_FieldCreateNoDataPtr
    module procedure ESMF_FieldCreateNoDataArray
! !DESCRIPTION:
!   This interface provides an entry point for methods that create 
!   an {\tt ESMF\_Field} without allocating or referencing any associated data.
!   The variations allow an {\tt ESMF\_Grid} to be specified or not, and for
!   the data description to be specified or not.
  end interface
!EOPI
!------------------------------------------------------------------------------

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldCreateEmpty"
!BOP
! !IROUTINE: ESMF_FieldCreateEmpty - Create an empty Field (no Grid)

! !INTERFACE:
  function ESMF_FieldCreateEmpty(name, iospec, rc)
!
! !RETURN VALUE:
    type(ESMF_Field) :: ESMF_FieldCreateEmpty 
!
! !ARGUMENTS:
    character (len = *), intent(in), optional :: name  
    type(ESMF_IOSpec), intent(in), optional :: iospec  
    integer, intent(out), optional :: rc               
!
! !DESCRIPTION:
! This version of {\tt ESMF\_FieldCreate} builds an empty {\tt ESMF\_Field} 
! and depends on later calls to add an {\tt ESMF\_Grid} and {\tt ESMF\_Array} to 
! it. Attributes can be added to an empty Field object. For an example and
! associated documentation using this method see Section 
! \ref{sec:field:usage:create_empty_setcommit}.
!
!
! The arguments are:
! \begin{description}
! \item [{[name]}] 
!       {\tt Field} name. 
! \item [{[iospec]}] 
!       I/O specification. ! NOT IMPLEMENTED 
! \item [{[rc]}] 
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOP
!------------------------------------------------------------------------------
    type(ESMF_FieldType), pointer :: ftype  ! Pointer to new field
    integer :: localrc                     
    
    ! Initialize
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    nullify(ftype)
    nullify(ESMF_FieldCreateEmpty%ftypep)

    allocate(ftype, stat=localrc)
    if (ESMF_LogMsgFoundAllocError(localrc, "Allocating Field information", &
                                     ESMF_CONTEXT, rc)) return
    call ESMF_FieldInitialize(ftype, rc=localrc) 
    if (ESMF_LogMsgFoundAllocError(localrc, "Default initialize Field", &
                                     ESMF_CONTEXT, rc)) return

    ! Call field construction method
    call ESMF_FieldConstructEmpty(ftype, name, iospec, localrc)
    if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

    ! Set return values.
    ESMF_FieldCreateEmpty%ftypep => ftype

    ESMF_INIT_SET_CREATED(ESMF_FieldCreateEmpty)

    call ESMF_FieldValidate(ESMF_FieldCreateEmpty, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

    if (present(rc)) rc = ESMF_SUCCESS

  end function ESMF_FieldCreateEmpty
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
! TODO:FIELDINTEGRATION Decide if FieldCreateNoArray is still needed
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldCreateNoDataPtr"
!BOPI
! !IROUTINE: ESMF_FieldCreateNoData - Create a Field with no associated data buffer
! !INTERFACE:
  ! Private name; call using ESMF_FieldCreateNoData()
  function ESMF_FieldCreateNoDataPtr(grid, arrayspec, staggerloc, &
    gridToFieldMap, ungriddedLBound, ungriddedUBound, maxHaloLWidth, &
    maxHaloUWidth, name, iospec, rc)
!
! !RETURN VALUE:
    type(ESMF_Field) :: ESMF_FieldCreateNoDataPtr   
!
! !ARGUMENTS:
   type(ESMF_Grid) :: grid                 
   type(ESMF_ArraySpec), intent(inout) :: arrayspec    
   type(ESMF_StaggerLoc), intent(in), optional ::staggerloc 
   integer, intent(in), optional :: gridToFieldMap(:)    
   integer, intent(in), optional :: ungriddedLBound(:)
   integer, intent(in), optional :: ungriddedUBound(:)
   integer, intent(in), optional :: maxHaloLWidth(:)
   integer, intent(in), optional :: maxHaloUWidth(:)
   character (len=*), intent(in), optional :: name    
   type(ESMF_IOSpec), intent(in), optional :: iospec  
   integer, intent(out), optional :: rc               
!
! !DESCRIPTION:
! An interface function to {\tt ESMF\_FieldCreateNoData()}.
! Creates an {\tt ESMF\_Field} in its entirety except for the assignment
! or allocation of an associated raw data buffer.
!
! The arguments are:
! \begin{description}
! \item [grid] 
!       Pointer to an {\tt ESMF\_Grid} object. 
! \item [arrayspec]
!       Data specification. 
! \item [{[staggerloc]}]
!       Stagger location of data in grid cells.  For valid 
!       predefined values see Section \ref{sec:opt:staggerloc}.
!       To create a custom stagger location see Section
!       \ref{sec:usage:staggerloc:adv}.The default
!       value is ESMF\_STAGGERLOC\_CENTER.
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
! \item [{[maxHaloLWidth]}]
!       Lower bound of halo region.  The size of this array is the number
!       of dimensions in the {\tt grid}.  However, ordering of the elements
!       needs to be the same as they appear in the {\tt field}.  Values default
!       to 0.  If values for maxHaloLWidth are specified they must be reflected in
!       the size of the {\tt field}.  That is, for each gridded dimension the
!       {\tt field} size should be max( {\tt maxHaloLWidth} + {\tt maxHaloUWidth}
!       + {\tt computationalCount}, {\tt exclusiveCount} ). Although the halo operation is not
!       implemented, the {\tt minHaloLWidth} is checked for validity and stored
!       in preparation for the implementation of the halo method.
!       HALO OPERATION NOT IMPLEMENTED
! \item [{[maxHaloUWidth]}]
!       Upper bound of halo region.  The size of this array is the number
!       of dimensions in the {\tt grid}.  However, ordering of the elements
!       needs to be the same as they appear in the {\tt field}.  Values default
!       to 0.  If values for maxHaloUWidth are specified they must be reflected in
!       the size of the {\tt field}.  That is, for each gridded dimension the
!       {\tt field} size should max( {\tt maxHaloLWidth} + {\tt maxHaloUWidth}
!       + {\tt computationalCount}, {\tt exclusiveCount} ).  Although the halo operation is not
!       implemented, the {\tt maxHaloUWidth} is checked for validity and stored
!       in preparation for the implementation of the halo method. 
!       HALO OPERATION NOT IMPLEMENTED
! \item [{[name]}] 
!       {\tt Field} name. 
! \item [{[iospec]}] 
!       I/O specification. ! NOT IMPLEMENTED
! \item [{[rc]}] 
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    type(ESMF_FieldType), pointer :: ftype      ! Pointer to new field
    integer :: localrc                         
   
    ! Initialize
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    nullify(ftype)
    nullify(ESMF_FieldCreateNoDataPtr%ftypep)

#if 0 
    allocate(ftype, stat=localrc)
    if (ESMF_LogMsgFoundAllocError(localrc, "Allocating Field information", &
                                     ESMF_CONTEXT, rc)) return

    call ESMF_FieldInitialize(ftype, rc=localrc) 
    if (ESMF_LogMsgFoundAllocError(localrc, "Default initialize Field", &
                                     ESMF_CONTEXT, rc)) return

    ! Call construction method to build field internals.
    call ESMF_FieldConstructNoDataPtr(ftype, grid, arrayspec, staggerloc, &
                                     gridToFieldMap, ungriddedLBound, &
                                     ungriddedUBound, maxHaloLWidth, &
                                     maxHaloUWidth, name, iospec, localrc)
    if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

    ! Set return values.
    ESMF_FieldCreateNoDataPtr%ftypep => ftype

    ESMF_INIT_SET_CREATED(ESMF_FieldCreateNoDataPtr)
      
    call ESMF_FieldValidate(ESMF_FieldCreateNoDataPtr, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

    if (present(rc)) rc = ESMF_SUCCESS
#endif

  end function ESMF_FieldCreateNoDataPtr
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
! TODO:FIELDINTEGRATION Decide if FieldCreateNoDataArray is still needed
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldCreateNoDataArray"
!BOPI
! !IROUTINE: ESMF_FieldCreateNoData - Create a Field with no associated Array object
! !INTERFACE:
  ! Private name; call using ESMF_FieldCreateNoData()
  function ESMF_FieldCreateNoDataArray(grid, staggerloc, name, iospec, rc)
!
! !RETURN VALUE:
    type(ESMF_Field) :: ESMF_FieldCreateNoDataArray 
!
! !ARGUMENTS:
    type(ESMF_Grid) :: grid                 
    type(ESMF_StaggerLoc), intent(in), optional :: staggerloc 
    character (len=*), intent(in), optional :: name    
    type(ESMF_IOSpec), intent(in), optional :: iospec  
    integer, intent(out), optional :: rc               
!
! !DESCRIPTION:
! An interface function to {\tt ESMF\_FieldCreateNoData()}.
! This version of {\tt ESMF\_FieldCreate} builds an {\tt ESMF\_Field} 
! and depends on a later call to add an {\tt ESMF\_Array} to it.  
!
! The arguments are:
! \begin{description}
! \item [grid] 
!       Pointer to an {\tt ESMF\_Grid} object. 
! \item [{[staggerloc]}]
!       Stagger location of data in grid cells.  For valid 
!       predefined values see Section \ref{sec:opt:staggerloc}.
!       To create a custom stagger location see Section
!       \ref{sec:usage:staggerloc:adv}. The default
!       value is ESMF\_STAGGERLOC\_CENTER.
! \item [{[name]}] 
!       {\tt Field} name. 
! \item [{[iospec]}] 
!       I/O specification. ! NOT IMPLEMENTED 
! \item [{[rc]}] 
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    type(ESMF_FieldType), pointer :: ftype  ! Pointer to new field
    integer :: localrc                    
    
    ! Initialize
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    nullify(ftype)
    nullify(ESMF_FieldCreateNoDataArray%ftypep)

#if 0
    allocate(ftype, stat=localrc)
    if (ESMF_LogMsgFoundAllocError(localrc, "Allocating Field information", &
                                     ESMF_CONTEXT, rc)) return
    call ESMF_FieldInitialize(ftype, rc=localrc) 
    if (ESMF_LogMsgFoundAllocError(localrc, "Default initialize Field", &
                                     ESMF_CONTEXT, rc)) return

    ! Call field construction method
    call ESMF_FieldConstructNoArray(ftype, grid, staggerloc, &
                                    name, &
                                    iospec, localrc)
    if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

    ! Set return values.
    ESMF_FieldCreateNoDataArray%ftypep => ftype

    ESMF_INIT_SET_CREATED(ESMF_FieldCreateNoDataArray)

    call ESMF_FieldValidate(ESMF_FieldCreateNoDataArray, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

    if (present(rc)) rc = ESMF_SUCCESS
#endif

  end function ESMF_FieldCreateNoDataArray
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldDestroy"
!BOP
! !IROUTINE: ESMF_FieldDestroy - Free all resources associated with a Field
! !INTERFACE:
  subroutine ESMF_FieldDestroy(field, rc)
!
! !ARGUMENTS:
    type(ESMF_Field) :: field       
    integer, intent(out), optional :: rc     
!
! !DESCRIPTION:
! Releases all resources associated with the {\tt ESMF\_Field}.
! 
! The arguments are:
! \begin{description}
! \item [field]
!       {\tt ESMF\_Field} object.
! \item [{[rc]}] 
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOP
!------------------------------------------------------------------------------
    ! Local variables
    integer :: localrc                         

    ! Initialize
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! check input variables
    ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit,field,rc)

    ! TODO: If already destroyed or never created, return ok?
    ! (should it be ok to destroy the same object twice without complaint?)
    ! for now, no, you cannot delete an object twice 
    call ESMF_FieldValidate(field, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, &
                               ESMF_ERR_PASSTHRU, &
                               ESMF_CONTEXT, rc)) return

    ! Destruct all field internals and then free field memory.
    call ESMF_FieldDestruct(field%ftypep, localrc)
    if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
         
    if (associated(field%ftypep)) then
       deallocate(field%ftypep, stat=localrc)
       if (ESMF_LogMsgFoundAllocError(localrc, "Deallocating Field", &
                                     ESMF_CONTEXT, rc)) return
    endif 
    ESMF_INIT_SET_DELETED(field)

    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_FieldDestroy
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldConstructIANew"
!BOPI
! !IROUTINE: ESMF_FieldConstructIANew - Construct the internals of a Field

! !INTERFACE:
  subroutine ESMF_FieldConstructIANew(ftype, geombase, arrayspec, &
    gridToFieldMap, ungriddedLBound, ungriddedUBound, &
    maxHaloLWidth, maxHaloUWidth, name, iospec, rc)
!
! !ARGUMENTS:
    type(ESMF_FieldType), pointer :: ftype 
    type(ESMF_GeomBase) :: geombase  
    type(ESMF_ArraySpec), intent(inout)         :: arrayspec
    integer, intent(in), optional :: gridToFieldMap(:)
    integer, intent(in), optional :: ungriddedLBound(:)
    integer, intent(in), optional :: ungriddedUBound(:)      
    integer, intent(in), optional :: maxHaloLWidth(:)
    integer, intent(in), optional :: maxHaloUWidth(:)
    character (len=*), intent(in), optional :: name
    type(ESMF_IOSpec), intent(in), optional :: iospec 
    integer, intent(out), optional :: rc              
!
! !DESCRIPTION:
! Constructs all {\tt ESMF\_Field} internals, including the allocation
! of a data {\tt ESMF\_Array}.  TODO: this is missing a counts argument,
! which is required if the arrayspec rank is greater than the {\tt grid} rank.
! Either that, or we must enforce that a datamap comes in, and it
! contains the counts for non-grid dims.
!
! The arguments are:
! \begin{description}
! \item [ftype]
!       Pointer to an {\tt ESMF\_Field} object.
! \item [grid] 
!       {\tt ESMF\_GeomBase} object. 
! \item [arrayspec]
!       Data specification.
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
! \item [{[maxHaloLWidth]}]
!       Lower bound of halo region.  The size of this array is the number
!       of dimensions in the {\tt grid}.  However, ordering of the elements
!       needs to be the same as they appear in the {\tt field}.  Values default
!       to 0.  If values for maxHaloLWidth are specified they must be reflected in
!       the size of the {\tt field}.  That is, for each gridded dimension the
!       {\tt field} size should be max( {\tt maxHaloLWidth} + {\tt maxHaloUWidth}
!       + {\tt computationalCount}, {\tt exclusiveCount} ). Although the halo operation is not
!       implemented, the {\tt minHaloLWidth} is checked for validity and stored
!       in preparation for the implementation of the halo method.
!       HALO OPERATION NOT IMPLEMENTED
! \item [{[maxHaloUWidth]}]
!       Upper bound of halo region.  The size of this array is the number
!       of dimensions in the {\tt grid}.  However, ordering of the elements
!       needs to be the same as they appear in the {\tt field}.  Values default
!       to 0.  If values for maxHaloUWidth are specified they must be reflected in
!       the size of the {\tt field}.  That is, for each gridded dimension the
!       {\tt field} size should max( {\tt maxHaloLWidth} + {\tt maxHaloUWidth}
!       + {\tt computationalCount}, {\tt exclusiveCount} ).  Although the halo operation is not
!       implemented, the {\tt maxHaloUWidth} is checked for validity and stored
!       in preparation for the implementation of the halo method. 
!       HALO OPERATION NOT IMPLEMENTED
! \item [{[name]}] 
!       {\tt ESMF\_Field} name. 
! \item [{[iospec]}] 
!       I/O specification. ! NOT IMPLEMENTED 
! \item [{[rc]}] 
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!       
! \end{description}
!EOPI
!------------------------------------------------------------------------------
    integer :: localrc
    type(ESMF_Array) :: array                  
    integer :: i, arrayRank, gridDimCount
    type(ESMF_DistGrid) :: distgrid
    type(ESMF_IndexFlag) :: indexflag    
    integer, pointer :: distgridToArrayMap(:)
    integer, pointer :: arrayLBound(:),arrayUBound(:)
    integer              :: compEUWidth(ESMF_MAXDIM),compELWidth(ESMF_MAXDIM)
    integer              :: ungriddedDimCount

    ! Initialize return code   
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    call ESMF_BaseCreate(ftype%base, "Field", name, 0, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return


    call ESMF_ArraySpecGet(arrayspec, rank=arrayRank, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return


    call ESMF_GeomBaseGet(geombase, distgrid=distgrid, dimCount=gridDimCount, &
                      indexflag=indexflag, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

    if (arrayRank .lt. gridDimCount) then
       call ESMF_LogMsgSetError(ESMF_RC_ARG_RANK, &
                               "Array rank must be equal to or greater than GeomBase rank", &
                                ESMF_CONTEXT, rc) 
       return
    endif

   ! Get the ungridded dimCount
   ungriddedDimCount=0
   if (present(ungriddedUBound)) then
      ungriddedDimCount=size(ungriddedUBound)
   endif


! TODO:FIELDINTEGRATION Check that Field halo is same rank as distgrid dim count
    
    ! allocate distgridToArrayMap
    allocate(distgridToArrayMap(gridDimCount) , stat=localrc)
    if (ESMF_LogMsgFoundAllocError(localrc, "Allocating distgridToArrayMap", &
                                     ESMF_CONTEXT, rc)) return   

    ! allocate undistributed Bounds
    allocate(arrayLBound(ungriddedDimCount) , stat=localrc)
    if (ESMF_LogMsgFoundAllocError(localrc, "Allocating gridLBound", &
                                     ESMF_CONTEXT, rc)) return   
    allocate(arrayUBound(ungriddedDimCount) , stat=localrc)
    if (ESMF_LogMsgFoundAllocError(localrc, "Allocating gridUBound", &
                                     ESMF_CONTEXT, rc)) return   

    ! Get dimmap and undistibuted bounds
    call ESMF_GeomBaseGetArrayInfo(geombase,                                            &
                            gridToFieldMap, ungriddedLBound, ungriddedUBound, &
                            distgridToArrayMap, arrayLBound, arrayUBound,   &
                            computationalEdgeLWidth=compELWidth, computationalEdgeUWidth=compEUWidth, &
                            rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return

    ! create Array
    array=ESMF_ArrayCreate(arrayspec=arrayspec, &                     
              distgrid=distgrid, &                                                         
              distgridToArrayMap=distgridToArrayMap, &                   
              computationalEdgeLWidth=compELWidth(1:gridDimCount), & 
              computationalEdgeUWidth=compEUWidth(1:gridDimCount), & 
              totalLWidth=maxHaloLWidth, totalUWidth=maxHaloUWidth, & 
              indexflag=indexflag, &                                                              
              staggerLoc=0,        & 
              undistLBound=arrayLBound, undistUBound=arrayUBound, &
              name=name, &
              rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return

    ! Default of gridToFieldMap should be {1,2,3...}
    if (.not. present(gridToFieldMap)) then
        do i = 1, ESMF_MAXDIM
          ftype%gridToFieldMap(i) = i
        enddo
    else
       ftype%gridToFieldMap(1:size(gridToFieldMap)) = gridToFieldMap
    endif

    if(present(ungriddedLBound)) &
       ftype%ungriddedLBound(1:size(ungriddedLBound)) = ungriddedLBound
    if(present(ungriddedUBound)) &
       ftype%ungriddedUBound(1:size(ungriddedUBound)) = ungriddedUBound
    if(present(maxHaloLWidth)) &
       ftype%maxHaloLWidth(1:size(maxHaloLWidth)) = maxHaloLWidth
    if(present(maxHaloUWidth)) &
       ftype%maxHaloUWidth(1:size(maxHaloUWidth)) = maxHaloUWidth

    ftype%array = array
    ftype%array_internal = .true.
    ftype%datastatus = ESMF_STATUS_READY
    ftype%geombase  = geombase
    ftype%gridstatus = ESMF_STATUS_READY
    ftype%fieldstatus = ESMF_STATUS_READY 

    ! cleanup
    deallocate(distgridToArrayMap)
    deallocate(arrayLBound)
    deallocate(arrayUBound)

    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_FieldConstructIANew
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldConstructIANewArray"
!BOPI
! !IROUTINE: ESMF_FieldConstructIANewArray - Construct the internals of a Field

! !INTERFACE:
  subroutine ESMF_FieldConstructIANewArray(ftype, geombase, array, copyflag, &
    gridToFieldMap, ungriddedLBound, ungriddedUBound, &
    maxHaloLWidth, maxHaloUWidth, name, iospec, rc)
!
! !ARGUMENTS:
    type(ESMF_FieldType), pointer :: ftype 
    type(ESMF_GeomBase) :: geombase 
    type(ESMF_Array), intent(in) :: array     
    type(ESMF_CopyFlag), intent(in) :: copyflag
    integer, intent(in) :: gridToFieldMap(:)
    integer, intent(in) :: ungriddedLBound(:)
    integer, intent(in) :: ungriddedUBound(:)      
    integer, intent(in) :: maxHaloLWidth(:)
    integer, intent(in) :: maxHaloUWidth(:)
    character (len=*), intent(in), optional :: name
    type(ESMF_IOSpec), intent(in), optional :: iospec 
    integer, intent(out), optional :: rc              
!
! !DESCRIPTION:
! Constructs all {\tt ESMF\_Field} internals, including the allocation
! of a data {\tt ESMF\_Array}.  
!
! The arguments are:
! \begin{description}
! \item [ftype]
!       Pointer to an {\tt ESMF\_Field} object.
! \item [geombase] 
!       {\tt ESMF\_GeomBase} object. 
! \item [array]
!       Data. 
! \item [copyflag]
!       Whether to copy the existing data space or reference directly. Valid
!       values are {\tt ESMF\_DATA\_COPY} or {\tt ESMF\_DATA\_REF} (default).
! \item [staggerloc] 
!       Stagger location of data in grid cells.  For valid 
!       predefined values see Section \ref{sec:opt:staggerloc}.
!       To create a custom stagger location see Section
!       \ref{sec:usage:staggerloc:adv}. The default
!       value is ESMF\_STAGGERLOC\_CENTER.
! \item [gridToFieldMap]
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
! \item [ungriddedLBound]
!       Lower bounds of the ungridded dimensions of the {\tt field}.
!       The number of elements in the {\tt ungriddedLBound} is equal to the number of ungridded
!       dimensions in the {\tt field}.  All ungridded dimensions of the
!       {\tt field} are also undistributed. When field dimension count is
!       greater than grid dimension count, both ungriddedLBound and ungriddedUBound
!       must be specified. When both are specified the values are checked
!       for consistency.  Note that the the ordering of
!       these ungridded dimensions is the same as their order in the {\tt field}.
! \item [ungriddedUBound]
!       Upper bounds of the ungridded dimensions of the {\tt field}.
!       The number of elements in the {\tt ungriddedUBound} is equal to the number of ungridded
!       dimensions in the {\tt field}.  All ungridded dimensions of the
!       {\tt field} are also undistributed. When field dimension count is
!       greater than grid dimension count, both ungriddedLBound and ungriddedUBound
!       must be specified. When both are specified the values are checked
!       for consistency.  Note that the the ordering of
!       these ungridded dimensions is the same as their order in the {\tt field}.
! \item [maxHaloLWidth]
!       Lower bound of halo region.  The size of this array is the number
!       of dimensions in the {\tt grid}.  However, ordering of the elements
!       needs to be the same as they appear in the {\tt field}.  Values default
!       to 0.  If values for maxHaloLWidth are specified they must be reflected in
!       the size of the {\tt field}.  That is, for each gridded dimension the
!       {\tt field} size should be max( {\tt maxHaloLWidth} + {\tt maxHaloUWidth}
!       + {\tt computationalCount}, {\tt exclusiveCount} ). Although the halo operation is not
!       implemented, the {\tt minHaloLWidth} is checked for validity and stored
!       in preparation for the implementation of the halo method.
!       HALO OPERATION NOT IMPLEMENTED
! \item [maxHaloUWidth]
!       Upper bound of halo region.  The size of this array is the number
!       of dimensions in the {\tt grid}.  However, ordering of the elements
!       needs to be the same as they appear in the {\tt field}.  Values default
!       to 0.  If values for maxHaloUWidth are specified they must be reflected in
!       the size of the {\tt field}.  That is, for each gridded dimension the
!       {\tt field} size should max( {\tt maxHaloLWidth} + {\tt maxHaloUWidth}
!       + {\tt computationalCount}, {\tt exclusiveCount} ).  Although the halo operation is not
!       implemented, the {\tt maxHaloUWidth} is checked for validity and stored
!       in preparation for the implementation of the halo method. 
!       HALO OPERATION NOT IMPLEMENTED
! \item [{[name]}] 
!       {\tt ESMF\_Field} name. 
! \item [{[iospec]}] 
!       I/O specification. ! NOT IMPLEMENTED 
! \item [{[rc]}] 
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!EOPI
!------------------------------------------------------------------------------
    integer :: localrc 
    type(ESMF_ArraySpec) :: arrayspec
    type(ESMF_Array)     :: newarray
    type(ESMF_TypeKind)  :: typekind
    integer              :: arrayrank

    ! Initialize
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! this validates the grid already, no need to validate it first.
    call ESMF_FieldConstructNoArray(ftype, geombase, &
                                    name=name, &
                                    iospec=iospec, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

    ! make sure the array is a valid object first.
    call ESMF_ArrayValidate(array, localrc)
    if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
    call ESMF_ArrayGet(array, typekind=typekind, &
        rank=arrayrank, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
    call ESMF_ArraySpecSet(arrayspec, typekind=typekind, &
        rank=arrayrank, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

    ftype%gridToFieldMap(1:size(gridToFieldMap)) = gridToFieldMap
 
    ftype%ungriddedLBound(1:size(ungriddedLBound)) = ungriddedLBound
    ftype%ungriddedUBound(1:size(ungriddedUBound)) = ungriddedUBound
    ftype%maxHaloLWidth(1:size(maxHaloLWidth)) = maxHaloLWidth
    ftype%maxHaloUWidth(1:size(maxHaloUWidth)) = maxHaloUWidth

    ! default copyflag value is ESMF_DATA_REF
    ftype%array_internal = .false.
    if(copyflag == ESMF_DATA_REF) then
        ftype%array = array
    else
        newarray = ESMF_ArrayCreate(array, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
        ftype%array = newarray
        ftype%array_internal = .true.
    endif
        
    ftype%datastatus = ESMF_STATUS_READY
    ftype%geombase  = geombase
    ftype%gridstatus = ESMF_STATUS_READY
    ftype%fieldstatus = ESMF_STATUS_READY 

    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_FieldConstructIANewArray
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldConstructNoDataPtr"
!BOPI
! !IROUTINE: ESMF_FieldConstructNoDataPtr - Construct a Field with no associated buffer

! !INTERFACE:
  subroutine ESMF_FieldConstructNoDataPtr(ftype, geombase, arrayspec, &
    gridToFieldMap, ungriddedLBound, ungriddedUBound, maxHaloLWidth, &
    maxHaloUWidth, name, iospec, rc)
!
! !ARGUMENTS:     
    type(ESMF_FieldType), pointer :: ftype                
    type(ESMF_GeomBase), intent(inout) :: geombase
    type(ESMF_ArraySpec), intent(inout) :: arrayspec     
    integer, intent(in), optional :: gridToFieldMap(:)    
    integer, intent(in), optional :: ungriddedLBound(:)
    integer, intent(in), optional :: ungriddedUBound(:)
    integer, intent(in), optional :: maxHaloLWidth(:)
    integer, intent(in), optional :: maxHaloUWidth(:)
    character (len=*), intent(in), optional :: name
    type(ESMF_IOSpec), intent(in), optional :: iospec 
    integer, intent(out), optional :: rc              
!
! !DESCRIPTION:
! Constructs all {\tt ESMF\_Field} internals except for the assignment of 
! an associated data buffer.
!
! The arguments are:
! \begin{description}
! \item [ftype]
!       Pointer to an {\tt ESMF\_Field} object.
! \item [geombase] 
!       {\tt ESMF\_GeomBase} object. 
! \item [arrayspec]
!       Data specification. 
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
! \item [{[maxHaloLWidth]}]
!       Lower bound of halo region.  The size of this array is the number
!       of dimensions in the {\tt grid}.  However, ordering of the elements
!       needs to be the same as they appear in the {\tt field}.  Values default
!       to 0.  If values for maxHaloLWidth are specified they must be reflected in
!       the size of the {\tt field}.  That is, for each gridded dimension the
!       {\tt field} size should be max( {\tt maxHaloLWidth} + {\tt maxHaloUWidth}
!       + {\tt computationalCount}, {\tt exclusiveCount} ). Although the halo operation is not
!       implemented, the {\tt minHaloLWidth} is checked for validity and stored
!       in preparation for the implementation of the halo method.
!       HALO OPERATION NOT IMPLEMENTED
! \item [{[maxHaloUWidth]}]
!       Upper bound of halo region.  The size of this array is the number
!       of dimensions in the {\tt grid}.  However, ordering of the elements
!       needs to be the same as they appear in the {\tt field}.  Values default
!       to 0.  If values for maxHaloUWidth are specified they must be reflected in
!       the size of the {\tt field}.  That is, for each gridded dimension the
!       {\tt field} size should max( {\tt maxHaloLWidth} + {\tt maxHaloUWidth}
!       + {\tt computationalCount}, {\tt exclusiveCount} ).  Although the halo operation is not
!       implemented, the {\tt maxHaloUWidth} is checked for validity and stored
!       in preparation for the implementation of the halo method. 
!       HALO OPERATION NOT IMPLEMENTED
! \item [{[name]}] 
!       {\tt ESMF\_Field} name. 
! \item [{[iospec]}] 
!       I/O specification.  ! NOT IMPLEMENTED
! \item [{[rc]}] 
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!EOPI
!------------------------------------------------------------------------------
  integer :: localrc

  ! Initialize
  localrc = ESMF_RC_NOT_IMPL
  if (present(rc)) rc = ESMF_RC_NOT_IMPL

  !TODO:FIELDINTEGRATION Complete implementation of FieldConstructNoDataPtr
#if 0
  if (present(staggerloc)) then
      ftype%staggerloc = staggerloc
  else
      ftype%staggerloc = ESMF_STAGGERLOC_CENTER
  endif

  ! Default of gridToFieldMap should be {1,2,3...}
  if (.not. present(gridToFieldMap)) then
      do i = 1, ESMF_MAXDIM
        ftype%gridToFieldMap(i) = i
      enddo
  else
     ftype%gridToFieldMap = gridToFieldMap
  end if

  if(present(ungriddedLBound)) &
     ftype%ungriddedLBound = ungriddedLBound
  if(present(ungriddedUBound)) &
     ftype%ungriddedUBound = ungriddedUBound
  if(present(maxHaloLWidth)) &
     ftype%maxHaloLWidth = maxHaloLWidth
  if(present(maxHaloUWidth)) &
     ftype%maxHaloUWidth = maxHaloUWidth

  ! Construct a default name if one is not given
  call ESMF_BaseCreate(ftype%base, "Field", name, 0, localrc)
  if (ESMF_LogMsgFoundError(localrc, &
                              ESMF_ERR_PASSTHRU, &
                              ESMF_CONTEXT, rc)) return

  ! Check to see grid is valid first.

  call ESMF_GeomBaseValidate(grid, localrc)
  if (ESMF_LogMsgFoundError(localrc, &
                              ESMF_ERR_PASSTHRU, &
                              ESMF_CONTEXT, rc)) return
  ftype%grid = grid
  ftype%gridstatus = ESMF_STATUS_READY

  call ESMF_GeomBaseGet(grid, dimCount=gridRank, rc=localrc)

  ! construct the array here - but TODO: we are missing the counts
  ! in case there are non-grid axes.  there has to be an additional
  ! counts array which contains counts for any data axes which is
  ! not associated with the grid.  e.g. for a 3d data array on a 2d grid,
  ! there would be counts(1).  for 4d data, counts(2).
  

  ! If I/O spec is present, copy it into the field object; otherwise just 
  ! initialize the I/O spec in the field object.
  if(present(iospec)) then
    !ESMF_IOSpecCopyInit(ftype%iospec, iospec, localrc)
    !if (ESMF_LogMsgFoundError(localrc, &
    !                          ESMF_ERR_PASSTHRU, &
    !                          ESMF_CONTEXT, rc)) return
  else 
    !ESMF_IOSpecInit(ftype%iospec, localrc)
    !if (ESMF_LogMsgFoundError(localrc, &
    !                          ESMF_ERR_PASSTHRU, &
    !                          ESMF_CONTEXT, rc)) return
  endif

  ftype%fieldstatus = ESMF_STATUS_READY

  if (present(rc)) rc = ESMF_SUCCESS
#endif
end subroutine ESMF_FieldConstructNoDataPtr
!------------------------------------------------------------------------------


! TODO:FIELDINTEGRATION Restore FieldConstructNoArray method
!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldConstructNoArray"
!BOPI
! !IROUTINE: ESMF_FieldConstructNoArray - Construct a Field with no associated Array

! !INTERFACE:
  subroutine ESMF_FieldConstructNoArray(ftype, geombase, &
                                            name, iospec, rc)
!
! !ARGUMENTS:     
    type(ESMF_FieldType), pointer :: ftype   
    type(ESMF_GeomBase), intent(inout) :: geombase                 
    character (len=*), intent(in), optional :: name    
    type(ESMF_IOSpec), intent(in), optional :: iospec  
    integer, intent(out), optional :: rc               
!
! !DESCRIPTION:
! Constructs an {\tt ESMF\_Field} except for its internal data {\tt ESMF\_Array}.
!
! The arguments are:
! \begin{description}
! \item [ftype]
!       Pointer to an {\tt ESMF\_Field} object.
! \item [geombase] 
!       {\tt ESMF\_GeomBase} object. 
! \item [{[datamap]}]
!       An {\tt ESMF\_FieldDataMap} which describes the mapping of 
!       data to the {\tt ESMF\_GeomBase}.
! \item [{[name]}] 
!       {\tt ESMF\_Field} name. 
! \item [{[iospec]}] 
!       I/O specification. ! NOT IMPLEMENTED 
! \item [{[rc]}] 
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!EOPI
!------------------------------------------------------------------------------
    integer :: localrc
    integer :: gridRank

    ! Initialize
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Construct a default name if one is not given
    call ESMF_BaseCreate(ftype%base, "Field", name, 0, localrc)
    if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

    ! Attach GeomBase
    call ESMF_GeomBaseValidate(geombase, localrc)
    if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
    ftype%geombase = geombase
    ftype%gridstatus = ESMF_STATUS_READY

    call ESMF_GeomBaseGet(ftype%geombase, dimCount=gridRank, rc=localrc)
    
    ftype%fieldstatus = ESMF_STATUS_READY

    if  (present(rc)) rc = ESMF_SUCCESS
    
  end subroutine ESMF_FieldConstructNoArray
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldConstructEmpty"
!BOPI
! !IROUTINE: ESMF_FieldConstructEmpty - Construct a Field with no GeomBase or Array
!
! !INTERFACE:
  subroutine ESMF_FieldConstructEmpty(ftypep, name, iospec, rc)
!
! !ARGUMENTS:     
    type(ESMF_FieldType), pointer :: ftypep
    character (len = *), intent(in), optional :: name  
    type(ESMF_IOSpec), intent(in), optional :: iospec  
    integer, intent(out), optional :: rc               
!
! !DESCRIPTION:
! Constructs an empty {\tt ESMF\_Field}.
!
! The arguments are:
! \begin{description}
! \item [ftypep]
!       Pointer to an {\tt ESMF\_Field} object.
! \item [{[name]}]
!       {\tt ESMF\_Field} name.
! \item [{[iospec]}]
!       {\tt ESMF\_Field} I/O specification. ! NOT IMPLEMENTED
! \item [{[rc]}]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!EOPI
!------------------------------------------------------------------------------
    ! Local variables
    integer :: localrc

    ! Initialize
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Construct a default name if one is not given
    call ESMF_BaseCreate(ftypep%base, "Field", name, 0, localrc)
    if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

    ! Initialize field contents
    ftypep%gridstatus = ESMF_STATUS_UNINIT
    ftypep%datastatus = ESMF_STATUS_UNINIT

    ftypep%fieldstatus = ESMF_STATUS_READY

    if (present(rc)) rc = ESMF_SUCCESS
    
  end subroutine ESMF_FieldConstructEmpty
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldDestruct"
!BOPI
! !IROUTINE:   ESMF_FieldDestruct - Free any Field memory allocated internally
!
! !INTERFACE:
  subroutine ESMF_FieldDestruct(ftype, rc)
!
! !ARGUMENTS:
    type(ESMF_FieldType), pointer :: ftype        
    integer, intent(out), optional :: rc         
!
! !DESCRIPTION:
! Releases all resources except the {\tt ESMF\_Field} itself.
!
! The arguments are:
! \begin{description}
! \item [ftype]
!       Pointer to an {\tt ESMF\_Field} object.
! \item [{[rc]}] 
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!EOPI
!------------------------------------------------------------------------------
    integer :: localrc

    ! Initialize
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL


    ! release the base class resources
    call ESMF_BaseDestroy(ftype%base, localrc)
    if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
!
! TODO: more code goes here
!
    if((ftype%is_proxy .or. ftype%array_internal) .and. &
      ftype%datastatus .eq. ESMF_STATUS_READY) then
        call ESMF_ArrayDestroy(ftype%array, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
    endif

    if (ftype%gridstatus .eq. ESMF_STATUS_READY) then
        call ESMF_GeomBaseDestroy(ftype%geombase, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
    endif

    if  (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_FieldDestruct
!------------------------------------------------------------------------------

end module ESMF_FieldCreateBaMod
