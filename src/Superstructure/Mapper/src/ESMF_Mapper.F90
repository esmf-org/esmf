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
    real(ESMF_KIND_R8), dimension(:), pointer :: savedOptElapsedWallClockTimes => null()
  end type

  ! Information specific to a component optimized by the mapper
  ! The component info could also represent a super component,
  ! consisting of components that run sequentially
  type ESMF_MapperCompInfoClass
    ! Name of the component - used as key to match to a component (since
    ! component object references can be transient)
    ! In the case of a super component, concatenation of the names of 
    ! all components
    character(len=ESMF_MAXSTR) :: name
    ! Component constraints provided to the mapper
    ! Min/Max number of PETs for the component
    integer :: minNumPet
    integer :: maxNumPet
    ! Optimization info for this component
    type(ESMF_MapperCompOptimizeInfo) :: optInfo
    ! A list of sequential components
    type(ESMF_MapperCompInfo), dimension(:), pointer :: seqCompInfos => null()
  end type

  type ESMF_MapperCompInfo
    type(ESMF_MapperCompInfoClass), pointer :: compInfop => null()
  end type

  ! Mapper optimization info for an execution block
  type ESMF_MapperExecutionBlockOptimizeInfo
    ! The elapsed wallclock time for the latest run of this execution block
    real(ESMF_KIND_R8) :: curElapsedWallClockTime
    ! The predicted wallclock time for the next run of this execution block
    real(ESMF_KIND_R8) :: optElapsedWallClockTime
  end type

  ! A Mapper execution block - consists of multiple components and
  ! execution blocks that need to run concurrently
  type ESMF_MapperExecutionBlockClass
    ! Min and Max number of PETs for the Execution block
    ! - derived from Min/Max PETs for each comp in the execution block
    integer :: minNumPet
    integer :: maxNumPet
    ! Optimize information (non component specific) for the execution block
    type(ESMF_MapperExecutionBlockOptimizeInfo) :: optInfo
    ! Info on components in the execution block
    type(ESMF_MapperCompInfo), dimension(:), pointer :: compInfos => null()
    ! Execution blocks that are part of this execution block
    type(ESMF_MapperExecutionBlock), dimension(:), pointer :: execBlocks => null()
  end type

  type ESMF_MapperExecutionBlock
    type(ESMF_MapperExecutionBlockClass), pointer :: execBlockp => null()
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
    real(ESMF_KIND_R8), dimension(:), pointer :: savedOptElapsedWallClockTimes => null()
  end type

  ! The ESMF Mapper
  type ESMF_MapperClass
    ! The VM on which the mapper optimizes
    type(ESMF_VM) :: vm
    ! The global PET list that the mapper works on - from mapper VM
    integer, dimension(:), pointer :: petList => null()
    ! Total number of components in the system - derived from the
    ! execution blocks provided by the user
    integer :: numComponents
    ! Global optimization information for the application
    type(ESMF_MapperOptimizeInfo) :: optInfo
    ! The root execution block
    type(ESMF_MapperExecutionBlock) :: rootExecBlock
  end type

  type ESMF_Mapper
    type(ESMF_MapperClass), pointer :: mapperp => null()
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

  public ESMF_MapperCompInfoCreate  ! Create a comp info associated with a comp
  public ESMF_MapperCompInfoDestroy ! Destroy a comp info
  public ESMF_MapperExecutionBlockCreate ! Create an execution block
  public ESMF_MapperExecutionBlockDestroy ! Destroy an execution block

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

    type(ESMF_MapperClass), pointer :: mapperp
    integer :: localrc
    integer :: npets, i

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
    nullify(ESMF_MapperCreate%mapperp)

    allocate(mapperp, stat=localrc)
    if (ESMF_LogFoundAllocError(localrc, msg="MapperClass", &
      ESMF_CONTEXT, rcToReturn=rc)) return

    mapperp%vm = vm

    call ESMF_VMGet(vm, petCount=npets, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    allocate(mapperp%petList(npets), stat=localrc)
    if (ESMF_LogFoundAllocError(localrc, msg="MapperClassPetList", &
      ESMF_CONTEXT, rcToReturn=rc)) return
    do i=1, npets
      mapperp%petList(i) = i-1
    end do

    ! FIXME: Read config file, if present, and initialize the mapper

    ESMF_MapperCreate%mapperp => mapperp

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

    if(associated(mapper%mapperp)) then
      if(associated(mapper%mapperp%petList)) then
        deallocate(mapper%mapperp%petList)
        nullify(mapper%mapperp%petList)
      end if
      if(associated(mapper%mapperp%optInfo%savedOptElapsedWallClockTimes)) then
        deallocate(mapper%mapperp%optInfo%savedOptElapsedWallClockTimes)
        nullify(mapper%mapperp%optInfo%savedOptElapsedWallClockTimes)
      end if
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
    rootExecBlock, rc)
!
! !ARGUMENTS:
    type(ESMF_Mapper), intent(inout) :: mapper
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_MapperExecutionBlock), intent(in), optional :: rootExecBlock
    integer,             intent(out), optional :: rc

! !DESCRIPTION:
!   Set constraints on the ESMF\_Mapper
!
! The arguments are:
!   \begin{description}
!   \item[{[mapper]}]
!     Mapper class; 
!   \item[{[rootExecBlock]}]
!     Root execution block for this mapper;
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
  !-----------------------------------------------------------------------------    
    if(present(rootExecBlock)) then
      mapper%mapperp%rootExecBlock = rootExecBlock
    end if
    if (present(rc)) rc = ESMF_SUCCESS
  end subroutine
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_MapperSetCompConstraints()"
!BOP
! !IROUTINE: ESMF_MapperSetCompConstraints - Set constraints for each component

! !INTERFACE:
  subroutine ESMF_MapperSetCompConstraints(mapper, gComp, gCompInfo, keywordEnforcer, &
    minNumPet, maxNumPet, rc)
!
! !ARGUMENTS:
    type(ESMF_Mapper), intent(inout) :: mapper
    type(ESMF_GridComp), intent(in) :: gComp
    type(ESMF_MapperCompInfo), intent(inout) :: gCompInfo
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
!   \item[{[gCompInfo]}]
!     Mapper Component Info associated with this component
!   \item[{[minNumPet]}]
!     Min number of PETs
!   \item[{[maxNumPet]}]
!     Max number of PETs
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
  !-----------------------------------------------------------------------------    
    if(present(minNumPet)) then
      gCompInfo%compInfop%minNumPet = minNumPet
    end if
    if(present(maxNumPet)) then
      gCompInfo%compInfop%maxNumPet = maxNumPet
    end if
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
! !IROUTINE: ESMF_MapperGet - Get info from the mapper regarding a component

! !INTERFACE:
  subroutine ESMF_MapperGet(mapper, gCompInfo, keywordEnforcer, npets, petList, rc)
!
! !ARGUMENTS:
    type(ESMF_Mapper), intent(in) :: mapper
    type(ESMF_MapperCompInfo), intent(in) :: gCompInfo
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
!   \item[{[gCompInfo]}]
!     Mapper component info associated with the ESMF component being queried; 
!   \item[{[npets]}]
!     Number of PETs (optimized by the mapper) to use for the component
!   \item[{[petList]}]
!     PET list (optimized by the mapper) to use for the component
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
  !-----------------------------------------------------------------------------    

    if(present(npets)) then
      npets = size(gCompInfo%compInfop%optInfo%optPetList)
    end if
    if(present(petList)) then
      if(size(petList) /= size(gCompInfo%compInfop%optInfo%optPetList)) then
        call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
          msg="The size of petlist is not equal to number of pets", &
          ESMF_CONTEXT, rcToReturn=rc)
      end if
      petList = gCompInfo%compInfop%optInfo%optPetList
    end if
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

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_MapperCompInfoCreate()"
!BOP
! !IROUTINE: ESMF_MapperCompInfoCreate - Create a mapper component info object
! associated with an ESMF component or multiple sequential components

! !INTERFACE:
  function ESMF_MapperCompInfoCreate(mapper, gComps, keywordEnforcer, minNumPet, maxNumPet, rc)
! !RETURN VALUE:
    type(ESMF_MapperCompInfo) :: ESMF_MapperCompInfoCreate
!
! !ARGUMENTS:
    type(ESMF_Mapper), intent(inout) :: mapper
    type(ESMF_GridComp), dimension(:), intent(in) :: gComps
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,              intent(in), optional :: minNumPet
    integer,              intent(in), optional :: maxNumPet
    integer,             intent(out), optional :: rc

    type(ESMF_MapperCompInfoClass), pointer :: compInfop
    integer :: localrc
    integer :: ncomps, i
    character(len=ESMF_MAXSTR) :: cname, compInfoName
! !DESCRIPTION:
!   Create an ESMF Mapper Comp Info object associated with an ESMF component
!   or multiple sequential components
!
! The arguments are:
!   \begin{description}
!   \item[{[mapper]}]
!     Mapper class; 
!   \item[{[gComps]}]
!     Array of sequential ESMF components;
!   \item[{[gCompInfo]}]
!     Mapper component info associated with the sequential ESMF components
!   \item[{[minNumPet]}]
!     Minimum number of PETs that can be used for this component
!   \item[{[maxNumPet]}]
!     Maximum number of PETs that can be used for this component
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
  !-----------------------------------------------------------------------------    
    nullify(ESMF_MapperCompInfoCreate%compInfop)

    ncomps = size(gComps)
  
    compInfoName = ""
    do i=1,ncomps  
      call ESMF_GridCompGet(gComps(i), name=cname, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      compInfoName = trim(compInfoName) // "_" // trim(cname)
    end do

    allocate(compInfop, stat=localrc)
    if (ESMF_LogFoundAllocError(localrc, msg="CompInfoClass", &
      ESMF_CONTEXT, rcToReturn=rc)) return

    compInfop%name = trim(compInfoName)

    ! FIXME: Decide on how to store seqCompInfos

    if(present(minNumPet)) then
      compInfop%minNumPet = minNumPet
    end if
    if(present(maxNumPet)) then
      compInfop%maxNumPet = maxNumPet
    end if

    ESMF_MapperCompInfoCreate%compInfop => compInfop

    if (present(rc)) rc = ESMF_SUCCESS
  end function
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_MapperCompInfoDestroy()"
!BOP
! !IROUTINE: ESMF_MapperCompInfoDestroy - Destroy a mapper component info object

! !INTERFACE:
  subroutine ESMF_MapperCompInfoDestroy(mapper, gCompInfo, keywordEnforcer, rc)
!
! !ARGUMENTS:
    type(ESMF_Mapper), intent(inout) :: mapper
    type(ESMF_MapperCompInfo), intent(inout) :: gCompInfo
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,             intent(out), optional :: rc

! !DESCRIPTION:
!   Destroy an ESMF Mapper Comp Info object
!
! The arguments are:
!   \begin{description}
!   \item[{[mapper]}]
!     Mapper class; 
!   \item[{[gCompInfo]}]
!     Mapper component info object that needs to be destroyed; 
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
  !-----------------------------------------------------------------------------    
    if(associated(gCompInfo%compInfop)) then
      if(associated(gCompInfo%compInfop%seqCompInfos)) then
        deallocate(gCompInfo%compInfop%seqCompInfos)
        nullify(gCompInfo%compInfop%seqCompInfos)
      end if

      if(associated(gCompInfo%compInfop%optInfo%savedOptElapsedWallClockTimes)) then
        deallocate(gCompInfo%compInfop%optInfo%savedOptElapsedWallClockTimes)
        nullify(gCompInfo%compInfop%optInfo%savedOptElapsedWallClockTimes)
      end if
    end if
    if (present(rc)) rc = ESMF_SUCCESS
  end subroutine
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_MapperExecutionBlockCreate()"
!BOP
! !IROUTINE: ESMF_MapperExecutionBlockCreate - Create a mapper execution block
! An execution block consists of components or other execution blocks that
! run concurrently

! !INTERFACE:
  function ESMF_MapperExecutionBlockCreate(mapper, gCompInfos, execBlocks, keywordEnforcer, rc)
! !RETURN VALUE:
    type(ESMF_MapperExecutionBlock) :: ESMF_MapperExecutionBlockCreate
!
! !ARGUMENTS:
    type(ESMF_Mapper), intent(inout) :: mapper
    type(ESMF_MapperCompInfo), dimension(:), intent(in) :: gCompInfos
    type(ESMF_MapperExecutionBlock), dimension(:), intent(in) :: execBlocks
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,             intent(out), optional :: rc

    type(ESMF_MapperExecutionBlockClass), pointer :: parentExecBlockp
    integer :: minNumPet, maxNumPet
    integer :: i, nCompInfos, nExecBlocks
    integer :: localrc
! !DESCRIPTION:
!   Create a mapper execution block
!
! The arguments are:
!   \begin{description}
!   \item[{[mapper]}]
!     Mapper class; 
!   \item[{[gCompInfos]}]
!     Mapper component info objects that are part of this execution block
!   \item[{[execBlocks]}]
!     Mapper execution blocks that are part of this execution block
!   \item[{[parentExecBlock]}]
!     Mapper execution block that is created
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
  !-----------------------------------------------------------------------------    

    nullify(ESMF_MapperExecutionBlockCreate%execBlockp)

    nCompInfos = size(gCompInfos)
    nExecBlocks = size(execBlocks)
    minNumPet = 0
    maxNumPet = size(mapper%mapperp%petList)
    do i=1, nCompInfos
      if(minNumPet < gCompInfos(i)%compInfop%minNumPet) then
        minNumPet = gCompInfos(i)%compInfop%minNumPet
      end if
      if(maxNumPet > gCompInfos(i)%compInfop%maxNumPet) then
        maxNumPet = gCompInfos(i)%compInfop%maxNumPet
      end if
    end do
    do i=1, nExecBlocks
      if(minNumPet < execBlocks(i)%execBlockp%minNumPet) then
        minNumPet = execBlocks(i)%execBlockp%minNumPet
      end if
      if(maxNumPet > execBlocks(i)%execBlockp%maxNumPet) then
        maxNumPet = execBlocks(i)%execBlockp%maxNumPet
      end if
    end do

    allocate(parentExecBlockp, stat=localrc)
    if (ESMF_LogFoundAllocError(localrc, msg="ExecBlock", &
      ESMF_CONTEXT, rcToReturn=rc)) return

    parentExecBlockp%minNumPet = minNumPet
    parentExecBlockp%maxNumPet = maxNumPet

    allocate(parentExecBlockp%compInfos(nCompInfos))
    if (ESMF_LogFoundAllocError(localrc, msg="ExecBlockCompInfos", &
      ESMF_CONTEXT, rcToReturn=rc)) return

    parentExecBlockp%compInfos = gCompInfos
    parentExecBlockp%execBlocks = execBlocks

    ESMF_MapperExecutionBlockCreate%execBlockp => parentExecBlockp
    if (present(rc)) rc = ESMF_SUCCESS
  end function
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_MapperExecutionBlockDestroy()"
!BOP
! !IROUTINE: ESMF_MapperExecutionBlockDestroy - destroy a mapper execution block
! The execution block is recursively destroyed

! !INTERFACE:
  subroutine ESMF_MapperExecutionBlockDestroy(mapper, execBlock, keywordEnforcer, rc)
!
! !ARGUMENTS:
    type(ESMF_Mapper), intent(inout) :: mapper
    type(ESMF_MapperExecutionBlock), intent(inout) :: execBlock
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,             intent(out), optional :: rc

    integer :: localrc
    integer :: i, sz
! !DESCRIPTION:
!   Destroy a mapper execution block
!
! The arguments are:
!   \begin{description}
!   \item[{[mapper]}]
!     Mapper class; 
!   \item[{[execBlock]}]
!     Mapper execution block that needs to be destroyed;
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
  !-----------------------------------------------------------------------------    
    if(associated(execBlock%execBlockp)) then
      if(associated(execBlock%execBlockp%compInfos)) then
        sz = size(execBlock%execBlockp%compInfos)
        do i=1,sz
          call ESMF_MapperCompInfoDestroy(mapper, &
                execBlock%execBlockp%compInfos(i), rc=localrc)
          ! FIXME: Should we ignore errors while destroying ?
        end do
        deallocate(execBlock%execBlockp%compInfos)
      end if
      if(associated(execBlock%execBlockp%execBlocks)) then
        sz = size(execBlock%execBlockp%execBlocks)
        do i=1,sz
          call ESMF_MapperExecutionBlockDestroy(mapper, &
                execBlock%execBlockp%execBlocks(i), rc=localrc)
          ! FIXME: Should we ignore errors while destroying ?
        end do
        deallocate(execBlock%execBlockp%execBlocks)
      end if
    end if
    if (present(rc)) rc = ESMF_SUCCESS
  end subroutine
!------------------------------------------------------------------------------

end module ESMF_MapperMod
