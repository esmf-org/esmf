! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2014, University Corporation for Atmospheric Research, 
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
! Laboratory, University of Michigan, National Centers for Environmental 
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
#define FILENAME "src/addon/NUOPC/src/NUOPC_Comp.F90"
!==============================================================================

module NUOPC_Comp

  use ESMF
  use NUOPC_Base

  implicit none
  
  private
  
  ! public module interfaces
  public NUOPC_CompDerive
  public NUOPC_CompSpecialize
  
  ! interface blocks
  interface NUOPC_CompDerive
    module procedure NUOPC_CompDeriveGridComp
    module procedure NUOPC_CompDeriveCplComp
  end interface
  !---------------------------------------------
  interface NUOPC_CompSpecialize
    module procedure NUOPC_CompSpecializeGridComp
    module procedure NUOPC_CompSpecializeCplComp
  end interface

  !-----------------------------------------------------------------------------
  contains
  !-----------------------------------------------------------------------------
  
  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_CompDerive - Derive a GridComp from a generic component
! !INTERFACE:
  ! Private name; call using NUOPC_CompDerive() 
  subroutine NUOPC_CompDeriveGridComp(comp, genericSetServicesRoutine, rc)
! !ARGUMENTS:
    type(ESMF_GridComp), intent(in)            :: comp
    interface
      subroutine genericSetServicesRoutine(gridcomp, rc)
        use ESMF
        implicit none
        type(ESMF_GridComp)        :: gridcomp ! must not be optional
        integer, intent(out)       :: rc       ! must not be optional
      end subroutine
    end interface
    integer,             intent(out), optional :: rc
! !DESCRIPTION:
!   Derive a GridComp (i.e. Model, Mediator, or Driver) from a generic component.
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    integer                                   :: localrc

    if (present(rc)) rc = ESMF_SUCCESS
    
    call genericSetServicesRoutine(comp, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME, &
      rcToReturn=rc)) &
      return  ! bail out
    
  end subroutine
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_CompDerive - Derive a CplComp from a generic component
! !INTERFACE:
  ! Private name; call using NUOPC_CompDerive() 
  subroutine NUOPC_CompDeriveCplComp(comp, genericSetServicesRoutine, rc)
! !ARGUMENTS:
    type(ESMF_CplComp),  intent(in)            :: comp
    interface
      subroutine genericSetServicesRoutine(cplcomp, rc)
        use ESMF
        implicit none
        type(ESMF_CplComp)         :: cplcomp  ! must not be optional
        integer, intent(out)       :: rc       ! must not be optional
      end subroutine
    end interface
    integer,             intent(out), optional :: rc
! !DESCRIPTION:
!   Derive an CplComp (i.e. Connector) from a generic component.
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    integer                                   :: localrc

    if (present(rc)) rc = ESMF_SUCCESS
    
    call genericSetServicesRoutine(comp, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME, &
      rcToReturn=rc)) &
      return  ! bail out
    
  end subroutine
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_CompSpecialize - Specialize a derived GridComp
!
! !INTERFACE:
  ! Private name; call using NUOPC_CompSpecialize()
  subroutine NUOPC_CompSpecializeGridComp(comp, specLabel, specIndex, &
    specRoutine, rc)
! !ARGUMENTS:
    type(ESMF_GridComp)                     :: comp
    character(len=*), intent(in)            :: specLabel
    integer,          intent(in),  optional :: specIndex
    interface
      subroutine specRoutine(gridcomp, rc)
        use ESMF_CompMod
        implicit none
        type(ESMF_GridComp)        :: gridcomp ! must not be optional
        integer, intent(out)       :: rc       ! must not be optional
      end subroutine
    end interface
    integer,          intent(out), optional :: rc 
!
! !DESCRIPTION:
! Specialize a derived GridComp (i.e. Model, Mediator, or Driver).
!EOP
  !-----------------------------------------------------------------------------
    if (present(rc)) rc = ESMF_SUCCESS

    call ESMF_MethodAdd(comp, label=specLabel, index=specIndex, &
      userRoutine=specRoutine, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    
  end subroutine
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_CompSpecialize - Specialize a derived CplComp
!
! !INTERFACE:
  ! Private name; call using NUOPC_CompSpecialize()
  subroutine NUOPC_CompSpecializeCplComp(comp, specLabel, specIndex, &
    specRoutine, rc)
! !ARGUMENTS:
    type(ESMF_CplComp)                      :: comp
    character(len=*), intent(in)            :: specLabel
    integer,          intent(in),  optional :: specIndex
    interface
      subroutine specRoutine(cplcomp, rc)
        use ESMF_CompMod
        implicit none
        type(ESMF_CplComp)         :: cplcomp  ! must not be optional
        integer, intent(out)       :: rc       ! must not be optional
      end subroutine
    end interface
    integer,          intent(out), optional :: rc 
!
! !DESCRIPTION:
! Specialize a derived CplComp (i.e. Model, Mediator, or Driver).
!EOP
  !-----------------------------------------------------------------------------
    if (present(rc)) rc = ESMF_SUCCESS

    call ESMF_MethodAdd(comp, label=specLabel, index=specIndex, &
      userRoutine=specRoutine, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    
  end subroutine
  !-----------------------------------------------------------------------------

end module
