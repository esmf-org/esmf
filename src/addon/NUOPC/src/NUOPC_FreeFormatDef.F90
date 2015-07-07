! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2015, University Corporation for Atmospheric Research, 
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
! Laboratory, University of Michigan, National Centers for Environmental 
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
#define FILENAME "src/addon/NUOPC/src/NUOPC_FreeFormatDef.F90"
!==============================================================================

module NUOPC_FreeFormatDef

  use ESMF

  implicit none
  
  private
  
  public NUOPC_FreeFormat
  public NUOPC_FreeFormatCreate, NUOPC_FreeFormatDestroy
  
  public NUOPC_FreeFormatLen
  
!==============================================================================
! 
! Constants
!
!==============================================================================
  
  integer, parameter              :: NUOPC_FreeFormatLen = 160

!==============================================================================
! 
! DERIVED TYPES
!
!==============================================================================
  
  type NUOPC_FreeFormat
    character(len=NUOPC_FreeFormatLen), pointer   :: stringList(:)
    integer                                       :: size
  end type

!==============================================================================
! 
! INTERFACE BLOCKS
!
!==============================================================================

#if 0
  interface NUOPC_FreeFormatBla
    module procedure NUOPC_FreeFormatBla1
    module procedure NUOPC_FreeFormatBla2
  end interface
#endif

  !-----------------------------------------------------------------------------
  contains
  !-----------------------------------------------------------------------------
  
  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_FreeFormatCreate - Create a FreeFormat object
! !INTERFACE:
  function NUOPC_FreeFormatCreate(stringList, capacity, rc)
! !RETURN VALUE:
    type(NUOPC_FreeFormat) :: NUOPC_FreeFormatCreate
! !ARGUMENTS:
    character(len=NUOPC_FreeFormatLen), optional, intent(in)  :: stringList(:)
    integer,                            optional, intent(in)  :: capacity
    integer,                            optional, intent(out) :: rc
! !DESCRIPTION:
!   Create a new FreeFormat object. If {\tt stringList} is provided, then the
!   newly created object will hold the provided strings and the size is that 
!   of {\tt size(stringList)}. If {\tt capacity} is provided, it is used to set
!   the capacity of the newly created FreeFormat object. Providing a
!   {\tt capacity} that is smaller than {\tt size(stringList)} triggers an
!   error.
!EOP
  !-----------------------------------------------------------------------------
    integer                                :: stat
    
    if (present(rc)) rc = ESMF_SUCCESS

#if 0    
    ! allocate a new run element
    allocate(runElement, stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg="Allocation of RunElement in NUOPC_RunElementAdd.", &
      line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
#endif

  end function
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_FreeFormatDestroy - Destroy a FreeFormat object
! !INTERFACE:
  subroutine NUOPC_FreeFormatDestroy(freeFormat, rc)
! !ARGUMENTS:
    type(NUOPC_FreeFormat),           intent(inout) :: freeFormat
    integer,                optional, intent(out)   :: rc
! !DESCRIPTION:
!   Destroy a FreeFormat object. All internal memory is deallocated.
!EOP
  !-----------------------------------------------------------------------------
    integer                                :: stat
    
    if (present(rc)) rc = ESMF_SUCCESS

#if 0    
    ! allocate a new run element
    allocate(runElement, stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg="Allocation of RunElement in NUOPC_RunElementAdd.", &
      line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
#endif

  end subroutine
  !-----------------------------------------------------------------------------


end module NUOPC_FreeFormatDef
