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
    !integer, dimension(:), pointer :: curPetList => null()
    integer, dimension(:), allocatable :: curPetList
    ! The predicted optimal PET list for the component
    !integer, dimension(:), pointer :: optPetList => null()
    integer, dimension(:), allocatable :: optPetList
    ! The elapsed wallclock time for the latest run of this component
    real(ESMF_KIND_R8) :: curElapsedWallClockTime
    ! The predicted wallclock time for the next run of this component
    real(ESMF_KIND_R8) :: optElapsedWallClockTime
    ! Saved wallclock times for previous predictions
    !real(ESMF_KIND_R8), dimension(:), pointer :: savedOptElapsedWallClockTimes => null()
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
  end type

  type ESMF_MapperCompInfo
    type(ESMF_MapperCompInfoClass), pointer :: compInfop => null()
  end type

  ! ESMF_MAPPER_WGT_EXEC_TIME_OPT_METHOD => Uses the execution time of a
  ! component to partition pets
  integer, parameter ::&
    ESMF_MAPPER_WGT_EXEC_TIME_OPT_METHOD=1

  ! The ESMF Mapper
  type ESMF_Mapper
#ifndef ESMF_NO_SEQUENCE
  sequence
#endif
  private
    type(ESMF_Pointer) :: this
  end type

!------------------------------------------------------------------------------
! !PUBLIC TYPES:
  public ESMF_Mapper, ESMF_MapperCompInfo

  public ESMF_MAPPER_WGT_EXEC_TIME_OPT_METHOD

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

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
  character(*), parameter, private :: version = &
    '$Id$'


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

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
    integer,          intent(out), optional :: rc

    integer :: nameLen, localrc
    localrc = ESMF_RC_NOT_IMPL

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
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    localrc = ESMF_RC_NOT_IMPL

    nameLen = 0
    if (present(configFile)) then
      nameLen = len_trim(configFile)
    end if

    ! Call the C entry point
    call c_ESMC_MapperCreate(ESMF_MapperCreate, vm, nameLen, &
          configFile, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT,&
          rcToReturn=rc)) return

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
    integer :: localrc

    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    localrc = ESMF_RC_NOT_IMPL

    ! Call the C entry point
    call c_ESMC_MapperDestroy(mapper, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT,&
          rcToReturn=rc)) return

    if (present(rc)) rc = ESMF_SUCCESS
  end subroutine
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_MapperSetConstraints()"
!BOP
! !IROUTINE: ESMF_MapperSetConstraints - Set constraints for the mapper

! !INTERFACE:
  subroutine ESMF_MapperSetConstraints(mapper, keywordEnforcer, rc)
!
! !ARGUMENTS:
    type(ESMF_Mapper), intent(inout) :: mapper
    type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
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

    if (present(rc)) rc = ESMF_RC_NOT_IMPL
  end subroutine
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_MapperSetCompConstraints()"
!BOP
! !IROUTINE: ESMF_MapperSetCompConstraints - Set constraints for each component

! !INTERFACE:
  subroutine ESMF_MapperSetCompConstraints(mapper, gComp, gCompInfo, keywordEnforcer, rc)
!
! !ARGUMENTS:
    type(ESMF_Mapper), intent(inout) :: mapper
    type(ESMF_GridComp), intent(in) :: gComp
    type(ESMF_MapperCompInfo), intent(inout) :: gCompInfo
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
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
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
  end subroutine
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_MapperOptimize()"
!BOP
! !IROUTINE: ESMF_MapperOptimize - Optimize using the mapper

! !INTERFACE:
  subroutine ESMF_MapperOptimize(mapper, keywordEnforcer, optThresholdReached, rc)
!
! !ARGUMENTS:
    type(ESMF_Mapper), intent(inout) :: mapper
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    logical,              intent(out), optional :: optThresholdReached
    integer,             intent(out), optional :: rc

    integer :: localrc
    logical :: loptThresholdReached
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

    if (present(rc)) rc = ESMF_RC_NOT_IMPL
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
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
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
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
  end subroutine
!------------------------------------------------------------------------------

end module ESMF_MapperMod
