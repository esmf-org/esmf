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
  public ESMF_Mapper

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
   public ESMF_MapperGetCompInfo  ! Get info about components from the mapper
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

    if (present(rc)) rc = localrc
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

    if (present(rc)) rc = localrc
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
    integer :: localrc

    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    localrc = ESMF_RC_NOT_IMPL

    ! Call the C entry point
    call c_ESMC_MapperSetConstraints(mapper, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT,&
          rcToReturn=rc)) return

    if (present(rc)) rc = localrc
  end subroutine
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_MapperSetCompConstraints()"
!BOPI
! !IROUTINE: ESMF_MapperSetCompConstraints - Set constraints for each component

! !INTERFACE:
  subroutine ESMF_MapperSetCompConstraints(mapper, compNameLen, compName, phaseNameLen, phaseName, keywordEnforcer, rc)
!
! !ARGUMENTS:
    type(ESMF_Mapper), intent(inout) :: mapper
    integer, intent(in) :: compNameLen
    character(len=*), intent(in) :: compName
    integer, intent(in) :: phaseNameLen
    character(len=*), intent(in) :: phaseName
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
!EOPI
  !-----------------------------------------------------------------------------    
    integer :: localrc

    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    localrc = ESMF_RC_NOT_IMPL

    ! Call the C entry point
    call c_ESMC_MapperSetCompConstraints(mapper,&
          compNameLen, compName, phaseNameLen, phaseName, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT,&
          rcToReturn=rc)) return

    if (present(rc)) rc = localrc
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
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    localrc = ESMF_RC_NOT_IMPL

    ! Call the C entry point
    call c_ESMC_MapperOptimize(mapper, loptThresholdReached, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT,&
          rcToReturn=rc)) return

    if(present(optThresholdReached)) optThresholdReached = loptThresholdReached

    if (present(rc)) rc = localrc
  end subroutine
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_MapperGet()"
!BOP
! !IROUTINE: ESMF_MapperGet - Get info from the mapper regarding a component

! !INTERFACE:
  subroutine ESMF_MapperGetCompInfo(mapper, compNameLen, compName, phaseNameLen, phaseName, keywordEnforcer, npets, startPet, endPet, rc)
!
! !ARGUMENTS:
    type(ESMF_Mapper), intent(in) :: mapper
    integer, intent(in) :: compNameLen
    character(len=*), intent(in) :: compName
    integer, intent(in) :: phaseNameLen
    character(len=*), intent(in) :: phaseName
    type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,              intent(out), optional :: npets
    integer,              intent(out), optional :: startPet
    integer,              intent(out), optional :: endPet
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
    integer :: localrc, lnpets, lstartPet, lendPet
    ! Call the C entry point
    call c_ESMC_MapperGetCompInfo(mapper,&
          compNameLen, compName, phaseNameLen, phaseName,&
          lstartPet, lendPet,&
          localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT,&
          rcToReturn=rc)) return

    if(present(startPet)) startPet = lstartPet
    if(present(endPet)) endPet = lendPet

    if (present(rc)) rc = localrc
  end subroutine
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_MapperPrint()"
!BOPI
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
!EOPI
  !-----------------------------------------------------------------------------    
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
  end subroutine
!------------------------------------------------------------------------------

end module ESMF_MapperMod
