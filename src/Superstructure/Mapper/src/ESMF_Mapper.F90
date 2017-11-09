! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2017, University Corporation for Atmospheric Research, 
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
! Laboratory, University of Michigan, National Centers for Environmental 
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
#define ESMF_FILENAME "ESMF_Mapper.F90"
!==============================================================================
!
!     ESMF Mapper module
module ESMF_MapperMod
!
!==============================================================================
!
! This file contains the Mapper class definition and Mapper class methods.
!
!------------------------------------------------------------------------------
! INCLUDES
#include "ESMF.h"

!------------------------------------------------------------------------------
!
!BOPI
! !MODULE: ESMF_MapperMod - Mapper class
!
! !DESCRIPTION:
! The code in this file implements the {\tt ESMF\_Mapper} class
!
! This type is implemented in Fortran 90.
!
!------------------------------------------------------------------------------
! !USES:
  use ESMF_UtilTypesMod
  use ESMF_LogErrMod
  use ESMF_StateTypesMod
  use ESMF_CompMod
  use ESMF_GridCompMod
  use ESMF_CplCompMod
  use ESMF_VMMod
  use ESMF_InitMacrosMod


  implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
  private

!------------------------------------------------------------------------------
! ! ESMF_Mapper class

  ! Mapper optimization info for a component
  type ESMF_MapperCompOptimizeInfo
    ! The PET list used by the component
    integer, dimension(:), pointer :: curPetList => null()
    ! The predicted optimal PET list for the component
    integer, dimension(:), pointer :: optPetList => null()
    ! The elapsed wallclock time for the latest run of this component
    real(ESMF_KIND_R8) :: curElapsedWallClockTime
    ! The predicted wallclock time for the next run of this component
    real(ESMF_KIND_R8) :: optElapsedWallClockTime
    ! Saved wallclock times for previous predictions
    real(ESMF_KIND_R8), dimension(:), pointer :: savedOptElapsedWallClockTimes
  end type

  ! Information specific to a component optimized by the mapper
  type ESMF_MapperCompInfo
    ! Name of the component - used as key to match to a component (since
    ! component object references can be transient)
    character(len=ESMF_MAXSTR) :: name
    ! Component constraints provided to the mapper
    ! Min/Max number of PETs for the component
    integer :: minNumPet
    integer :: maxNumPet
    ! Optimization info for this component
    type(ESMF_MapperCompOptimizeInfo) :: optInfo
  end type

  ! Mapper optimization info for an execution block
  type ESMF_MapperExecutionBlockOptimizeInfo
    ! The elapsed wallclock time for the latest run of this execution block
    real(ESMF_KIND_R8) :: curElapsedWallClockTime
    ! The predicted wallclock time for the next run of this execution block
    real(ESMF_KIND_R8) :: optElapsedWallClockTime
  end type

  ! A Mapper execution block - consists of multiple components that
  ! need to run sequentially
  ! A component can only be part of a single execution block
  type ESMF_MapperExecutionBlock
    ! Min and Max number of PETs for the Execution block
    ! - derived from Min/Max PETs for each comp in the execution block
    integer :: minNumPet
    integer :: maxNumPet
    ! Optimize information (non component specific) for the execution block
    type(ESMF_MapperExecutionBlockOptimizeInfo) :: optInfo
    ! Info on components in the execution block
    type(ESMF_MapperCompInfo), dimension(:), pointer :: compInfo => null()
  end type

  ! Mapper global (not component specific) optimization info
  type ESMF_MapperOptimizeInfo
    ! True if we reached an optimal layout
    logical :: hasOptimalLayout
    ! Threshold that indicates that the mapper has reached optimal layout
    ! - Min percentage of time (wallclock time) the execution time is
    !   expected to be reduced for a more optimal layout
    real(ESMF_KIND_R4) :: optThreshold
    ! The elapsed wallclock time for the latest run of the application
    real(ESMF_KIND_R8) :: curElapsedWallClockTime
    ! The predicted wallclock time for the next run of the application
    real(ESMF_KIND_R8) :: optElapsedWallClockTime
    ! Saved wallclock times for previous predictions
    real(ESMF_KIND_R8), dimension(:), pointer :: savedOptElapsedWallClockTimes
  end type

  ! The ESMF Mapper
  type ESMF_Mapper
    ! The VM on which the mapper optimizes
    type(ESMF_VM) :: vm
    ! The global PET list that the mapper works on - from mapper VM
    integer, dimension(:), pointer :: petList => null()
    ! Total number of components in the system - derived from the
    ! execution blocks provided by the user
    integer :: numComponents
    ! Global optimization information for the application
    type(ESMF_MapperOptimizeInfo) :: optInfo
    ! The execution blocks in the application, provided as constraints to 
    ! the mapper
    type(ESMF_MapperExecutionBlock), dimension(:), pointer :: execBlocks => null()
  end type

!------------------------------------------------------------------------------
! !PUBLIC TYPES:
  public ESMF_Mapper, ESMF_MapperCompInfo, ESMF_MapperExecutionBlock

!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:
!
! - ESMF-public methods:
   public ESMF_MapperCreate          ! Create a mapper
   public ESMF_MapperSetConstraints  ! Set constraints for the mapper
   public ESMF_MapperSetCompConstraints  ! Set constraints for the components
   public ESMF_MapperOptimize  ! Optimize based on set constraints
   public ESMF_MapperGet  ! Get info about components from the mapper
   public ESMF_MapperPrint  ! Print Mapper details
   public ESMF_MapperDestroy          ! Destroy a mapper

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
  character(*), parameter, private :: version = &
    '$Id$'


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! -------------------------- ESMF-public method -------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_MapperCreate()"
!BOP
! !IROUTINE: ESMF_MapperCreate - Create a mapper

! !INTERFACE:
  function ESMF_MapperCreate(vm, keywordEnforcer, configFile, rc)
! !RETURN VALUE:
    type(ESMF_Mapper) :: ESMF_MapperCreate
!
! !ARGUMENTS:
    type(ESMF_VM), intent(inout) :: vm
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    character(len=*), intent(in), optional :: configFile
    integer,             intent(out), optional :: rc

! !DESCRIPTION:
!   Returns the ESMF\_Mapper that  has been created.
!
! The arguments are:
!   \begin{description}
!   \item[{[configFile]}]
!     Configuration file for the ESMF\_Mapper
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
  !-----------------------------------------------------------------------------    
    ESMF_MapperCreate%vm = vm
    ! Read config file, if present, and initialize the mapper
    if (present(rc)) rc = ESMF_SUCCESS
  end function
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_MapperDestroy()"
!BOP
! !IROUTINE: ESMF_MapperDestroy - Destroy a mapper, release resources

!!INTERFACE:
  subroutine ESMF_MapperDestroy(mapper, keywordEnforcer, rc)
!
! !ARGUMENTS:
    type(ESMF_Mapper), intent(inout) :: mapper
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,             intent(out), optional :: rc

! !DESCRIPTION:
!   Destroys the ESMF\_Mapper
!
! The arguments are:
!   \begin{description}
!   \item[{[mapper]}]
!     Mapper class; 
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
  !-----------------------------------------------------------------------------    
    integer :: nblocks, ncomps, i, j

    if(associated(mapper%petList)) then
      deallocate(mapper%petList)
      nullify(mapper%petList)
    end if
    if(associated(mapper%optInfo%savedOptElapsedWallClockTimes)) then
      deallocate(mapper%optInfo%savedOptElapsedWallClockTimes)
      nullify(mapper%optInfo%savedOptElapsedWallClockTimes)
    end if
    if(associated(mapper%execBlocks)) then
      nblocks = size(mapper%execBlocks)
      do i=1,nblocks
        if(associated(mapper%execBlocks(i)%compInfo)) then
          ncomps = size(mapper%execBlocks(i)%compInfo)
          do j=1, ncomps
            if(associated(mapper%execBlocks(i)%compInfo(j)%optInfo%curPetList)) then
              deallocate(mapper%execBlocks(i)%compInfo(j)%optInfo%curPetList)
              nullify(mapper%execBlocks(i)%compInfo(j)%optInfo%curPetList)
            end if
            if(associated(mapper%execBlocks(i)%compInfo(j)%optInfo%optPetList)) then
              deallocate(mapper%execBlocks(i)%compInfo(j)%optInfo%optPetList)
              nullify(mapper%execBlocks(i)%compInfo(j)%optInfo%optPetList)
            end if
            if(associated(mapper%execBlocks(i)%compInfo(j)%optInfo%savedOptElapsedWallClockTimes)) then
              deallocate(mapper%execBlocks(i)%compInfo(j)%optInfo%savedOptElapsedWallClockTimes)
              nullify(mapper%execBlocks(i)%compInfo(j)%optInfo%savedOptElapsedWallClockTimes)
            end if
          end do
          deallocate(mapper%execBlocks(i)%compInfo)
          nullify(mapper%execBlocks(i)%compInfo)
        end if
      end do
      deallocate(mapper%execBlocks)
      nullify(mapper%execBlocks)
    end if
    if (present(rc)) rc = ESMF_SUCCESS
  end subroutine
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_MapperSetConstraints()"
!BOP
! !IROUTINE: ESMF_MapperSetConstraints - Set constraints for the mapper

! !INTERFACE:
  subroutine ESMF_MapperSetConstraints(mapper, keywordEnforcer, &
    execBlock, rc)
!
! !ARGUMENTS:
    type(ESMF_Mapper), intent(inout) :: mapper
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_GridComp), dimension(:), intent(in), optional :: execBlock
    integer,             intent(out), optional :: rc

! !DESCRIPTION:
!   Set constraints on the ESMF\_Mapper
!
! The arguments are:
!   \begin{description}
!   \item[{[mapper]}]
!     Mapper class; 
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
  !-----------------------------------------------------------------------------    
    if (present(rc)) rc = ESMF_SUCCESS
  end subroutine
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_MapperSetCompConstraints()"
!BOP
! !IROUTINE: ESMF_MapperSetCompConstraints - Set constraints for each component

! !INTERFACE:
  subroutine ESMF_MapperSetCompConstraints(mapper, gComp, keywordEnforcer, &
    minNumPet, maxNumPet, rc)
!
! !ARGUMENTS:
    type(ESMF_Mapper), intent(inout) :: mapper
    type(ESMF_GridComp), intent(in) :: gComp
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,             intent(in), optional :: minNumPet
    integer,             intent(in), optional :: maxNumPet
    integer,             intent(out), optional :: rc

! !DESCRIPTION:
!   Set constraints on the ESMF\_Mapper
!
! The arguments are:
!   \begin{description}
!   \item[{[mapper]}]
!     Mapper class; 
!   \item[{[gComp]}]
!     Grid Component
!   \item[{[minNumPet]}]
!     Grid Component
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
  !-----------------------------------------------------------------------------    
    if (present(rc)) rc = ESMF_SUCCESS
  end subroutine
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_MapperOptimize()"
!BOP
! !IROUTINE: ESMF_MapperOptimize - Optimize using the mapper

! !INTERFACE:
  subroutine ESMF_MapperOptimize(mapper, keywordEnforcer, rc)
!
! !ARGUMENTS:
    type(ESMF_Mapper), intent(inout) :: mapper
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,             intent(out), optional :: rc

! !DESCRIPTION:
!   Optimize using the mapper based on the set constraints
!
! The arguments are:
!   \begin{description}
!   \item[{[mapper]}]
!     Mapper class; 
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
  !-----------------------------------------------------------------------------    
    if (present(rc)) rc = ESMF_SUCCESS
  end subroutine
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_MapperGet()"
!BOP
! !IROUTINE: ESMF_MapperGet - Get info from the mapper

! !INTERFACE:
  subroutine ESMF_MapperGet(mapper, gComp, keywordEnforcer, npets, petList, rc)
!
! !ARGUMENTS:
    type(ESMF_Mapper), intent(in) :: mapper
    type(ESMF_GridComp), intent(in) :: gComp
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,              intent(out), optional :: npets
    integer, dimension(:),   intent(out), optional :: petList
    integer,             intent(out), optional :: rc

! !DESCRIPTION:
!   Get info from the mapper
!
! The arguments are:
!   \begin{description}
!   \item[{[mapper]}]
!     Mapper class; 
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
  !-----------------------------------------------------------------------------    
    if (present(rc)) rc = ESMF_SUCCESS
  end subroutine
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_MapperPrint()"
!BOP
! !IROUTINE: ESMF_MapperPrint - Print mapper details

! !INTERFACE:
  subroutine ESMF_MapperPrint(mapper, keywordEnforcer, rc)
!
! !ARGUMENTS:
    type(ESMF_Mapper), intent(inout) :: mapper
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,             intent(out), optional :: rc

! !DESCRIPTION:
!   Print mapper details
!
! The arguments are:
!   \begin{description}
!   \item[{[mapper]}]
!     Mapper class; 
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
  !-----------------------------------------------------------------------------    
    if (present(rc)) rc = ESMF_SUCCESS
  end subroutine
!------------------------------------------------------------------------------

end module ESMF_MapperMod
