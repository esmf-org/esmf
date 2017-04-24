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
#define ESMF_FILENAME "ESMF_Trace.F90"
!==============================================================================
!
!     ESMF Trace module
module ESMF_TraceMod

#include "ESMF.h"

  use ESMF_UtilTypesMod
  use ESMF_LogErrMod 
  use ESMF_CompMod
  use ESMF_GridCompMod
  use ESMF_CplCompMod
  use ESMF_BaseMod
  use ESMF_VMMod
  use ESMF_AttributeMod
  use ESMF_TimeMod
  use ESMF_ClockMod
  
  implicit none
  private

  public ESMF_TraceOpen
  public ESMF_TraceClose
  public ESMF_TraceEventPhaseEnter
  public ESMF_TraceEventPhaseExit
  public ESMF_TraceEventPhasePrologueEnter
  public ESMF_TraceEventPhasePrologueExit
  public ESMF_TraceEventPhaseEpilogueEnter
  public ESMF_TraceEventPhaseEpilogueExit
  public ESMF_TraceEventComponentInfo
  public ESMF_TraceEventRegionEnter
  public ESMF_TraceEventRegionExit
  public ESMF_TraceEventMemInfo
  public ESMF_TraceEventClock
  
  interface ESMF_TraceEventPhaseEnter
     module procedure ESMF_TraceEventGridCompPhaseEnter
     module procedure ESMF_TraceEventCplCompPhaseEnter
  end interface ESMF_TraceEventPhaseEnter

  interface ESMF_TraceEventPhaseExit
     module procedure ESMF_TraceEventGridCompPhaseExit
     module procedure ESMF_TraceEventCplCompPhaseExit
  end interface ESMF_TraceEventPhaseExit

  interface ESMF_TraceEventPhasePrologueEnter
     module procedure ESMF_TraceEventGridCompPhasePrologueEnter
     module procedure ESMF_TraceEventCplCompPhasePrologueEnter
  end interface ESMF_TraceEventPhasePrologueEnter

  interface ESMF_TraceEventPhaseEpilogueExit
     module procedure ESMF_TraceEventGridCompPhaseEpilogueExit
     module procedure ESMF_TraceEventCplCompPhaseEpilogueExit
  end interface ESMF_TraceEventPhaseEpilogueExit

  interface ESMF_TraceEventComponentInfo
     module procedure ESMF_TraceEventGridComponentInfo
     module procedure ESMF_TraceEventCplComponentInfo
  end interface ESMF_TraceEventComponentInfo
  
  interface ESMF_TraceGetCompID
     module procedure ESMF_TraceGetGridCompID
     module procedure ESMF_TraceGetCplCompID
  end interface ESMF_TraceGetCompID
  
contains

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_TraceOpen()"
  subroutine ESMF_TraceOpen(traceDir, rc)
    character(len=*), intent(in)             :: traceDir
    integer,          intent(out), optional  :: rc
    
    call c_esmftrace_open(traceDir, rc)
    if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return
  end subroutine ESMF_TraceOpen
  
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_TraceClose()"
  subroutine ESMF_TraceClose(rc)
    integer,          intent(out), optional :: rc    
    
    call c_esmftrace_close(rc)
    if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return
  end subroutine ESMF_TraceClose

!!$#undef  ESMF_METHOD
!!$#define ESMF_METHOD "ESMF_TraceLocalPet()"
!!$  function ESMF_TraceLocalPet(rc)
!!$    logical :: ESMF_TraceLocalPet
!!$    integer, intent(out), optional :: rc
!!$
!!$    ! locals
!!$    integer :: check, localrc
!!$
!!$    if (present(rc)) rc = ESMF_SUCCESS
!!$    
!!$    call c_esmftrace_checkpetlist(check, localrc)
!!$    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
!!$         ESMF_CONTEXT, rcToReturn=rc)) return
!!$    
!!$    if (check == 1) then
!!$       ESMF_TraceLocalPet = .true.
!!$    else
!!$       ESMF_TraceLocalPet = .false.
!!$    endif 
!!$  end function ESMF_TraceLocalPet

  
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_TraceEventGridCompPhaseEnter()"
  subroutine ESMF_TraceEventGridCompPhaseEnter(comp, rc)
    type(ESMF_GridComp), intent(in) :: comp
    integer, intent(out), optional  :: rc

    ! locals
    integer :: vmid
    integer :: baseid
    type(ESMF_Method_Flag) :: method
    integer :: method_enum
    integer :: phase

    rc = ESMF_SUCCESS 

    call ESMF_TraceGetCompID(comp, vmid, baseid, rc=rc)
    if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_GridCompGet(comp, currentMethod=method, &
         currentPhase=phase, rc=rc)
    if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return

    method_enum = ESMF_TraceMethodToEnum(method)

    call c_esmftrace_phase_enter(vmid, baseid, method_enum, phase, rc)
    if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return

  end subroutine ESMF_TraceEventGridCompPhaseEnter

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_TraceEventCplCompPhaseEnter()"
  subroutine ESMF_TraceEventCplCompPhaseEnter(comp, rc)
    type(ESMF_CplComp), intent(in) :: comp
    integer, intent(out), optional  :: rc

    ! locals
    integer :: vmid
    integer :: baseid
    type(ESMF_Method_Flag) :: method
    integer :: method_enum
    integer :: phase

    rc = ESMF_SUCCESS 

    call ESMF_TraceGetCompID(comp, vmid, baseid, rc=rc)
    if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_CplCompGet(comp, currentMethod=method, &
         currentPhase=phase, rc=rc)
    if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return

    method_enum = ESMF_TraceMethodToEnum(method)

    call c_esmftrace_phase_enter(vmid, baseid, method_enum, phase, rc)
    if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return

  end subroutine ESMF_TraceEventCplCompPhaseEnter


  
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_TraceEventGridCompPhaseExit()"
  subroutine ESMF_TraceEventGridCompPhaseExit(comp, rc)
    type(ESMF_GridComp), intent(in) :: comp
    integer, intent(out), optional  :: rc

    ! locals
    integer :: vmid
    integer :: baseid
    type(ESMF_Method_Flag) :: method
    integer :: method_enum
    integer :: phase

    rc = ESMF_SUCCESS

    call ESMF_TraceGetCompID(comp, vmid, baseid, rc=rc)
    if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_GridCompGet(comp, currentMethod=method, &
         currentPhase=phase, rc=rc)
    if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return

    method_enum = ESMF_TraceMethodToEnum(method)

    call c_esmftrace_phase_exit(vmid, baseid, method_enum, phase, rc)
    if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return
  end subroutine ESMF_TraceEventGridCompPhaseExit

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_TraceEventCplCompPhaseExit()"
  subroutine ESMF_TraceEventCplCompPhaseExit(comp, rc)
    type(ESMF_CplComp), intent(in) :: comp
    integer, intent(out), optional  :: rc

    ! locals
    integer :: vmid
    integer :: baseid
    type(ESMF_Method_Flag) :: method
    integer :: method_enum
    integer :: phase

    rc = ESMF_SUCCESS

    call ESMF_TraceGetCompID(comp, vmid, baseid, rc=rc)
    if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_CplCompGet(comp, currentMethod=method, &
         currentPhase=phase, rc=rc)
    if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return

    method_enum = ESMF_TraceMethodToEnum(method)

    call c_esmftrace_phase_exit(vmid, baseid, method_enum, phase, rc)
    if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return
  end subroutine ESMF_TraceEventCplCompPhaseExit
  
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_TraceEventGridCompPhasePrologueEnter()"
  subroutine ESMF_TraceEventGridCompPhasePrologueEnter(comp, rc)
    type(ESMF_GridComp), intent(in) :: comp
    integer, intent(out), optional  :: rc

    ! locals
    integer :: vmid
    integer :: baseid
    type(ESMF_Method_Flag) :: method
    integer :: method_enum
    integer :: phase

    rc = ESMF_SUCCESS 

    call ESMF_TraceGetCompID(comp, vmid, baseid, rc=rc)
    if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_GridCompGet(comp, currentMethod=method, &
         currentPhase=phase, rc=rc)
    if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return

    method_enum = ESMF_TraceMethodToEnum(method)

    call c_esmftrace_phase_prologue_enter(vmid, baseid, method_enum, phase, rc)
    if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return

  end subroutine ESMF_TraceEventGridCompPhasePrologueEnter

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_TraceEventCplCompPhasePrologueEnter()"
  subroutine ESMF_TraceEventCplCompPhasePrologueEnter(comp, rc)
    type(ESMF_CplComp), intent(in) :: comp
    integer, intent(out), optional  :: rc
    
    ! locals
    integer :: vmid
    integer :: baseid
    type(ESMF_Method_Flag) :: method
    integer :: method_enum
    integer :: phase

    rc = ESMF_SUCCESS 

    call ESMF_TraceGetCompID(comp, vmid, baseid, rc=rc)
    if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_CplCompGet(comp, currentMethod=method, &
         currentPhase=phase, rc=rc)
    if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return

    method_enum = ESMF_TraceMethodToEnum(method)

    call c_esmftrace_phase_prologue_enter(vmid, baseid, method_enum, phase, rc)
    if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return

  end subroutine ESMF_TraceEventCplCompPhasePrologueEnter

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_TraceEventPhasePrologueExit()"
  subroutine ESMF_TraceEventPhasePrologueExit(comp, rc)
    type(ESMF_GridComp), intent(in) :: comp
    integer, intent(out), optional  :: rc

    ! locals
    integer :: vmid
    integer :: baseid
    type(ESMF_Method_Flag) :: method
    integer :: method_enum
    integer :: phase

    rc = ESMF_SUCCESS

    call ESMF_TraceGetCompID(comp, vmid, baseid, rc=rc)
    if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_GridCompGet(comp, currentMethod=method, &
         currentPhase=phase, rc=rc)
    if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return

    method_enum = ESMF_TraceMethodToEnum(method)

    call c_esmftrace_phase_prologue_exit(vmid, baseid, method_enum, phase, rc)
    if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return
  end subroutine ESMF_TraceEventPhasePrologueExit

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_TraceEventPhaseEpilogueEnter()"
  subroutine ESMF_TraceEventPhaseEpilogueEnter(comp, rc)
    type(ESMF_GridComp), intent(in) :: comp
    integer, intent(out), optional  :: rc

    ! locals
    integer :: vmid
    integer :: baseid
    type(ESMF_Method_Flag) :: method
    integer :: method_enum
    integer :: phase

    rc = ESMF_SUCCESS 

    call ESMF_TraceGetCompID(comp, vmid, baseid, rc=rc)
    if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_GridCompGet(comp, currentMethod=method, &
         currentPhase=phase, rc=rc)
    if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return

    method_enum = ESMF_TraceMethodToEnum(method)

    call c_esmftrace_phase_epilogue_enter(vmid, baseid, method_enum, phase, rc)
    if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return

  end subroutine ESMF_TraceEventPhaseEpilogueEnter

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_TraceEventGridCompPhaseEpilogueExit()"
  subroutine ESMF_TraceEventGridCompPhaseEpilogueExit(comp, rc)
    type(ESMF_GridComp), intent(in) :: comp
    integer, intent(out), optional  :: rc

    ! locals
    integer :: vmid
    integer :: baseid
    type(ESMF_Method_Flag) :: method
    integer :: method_enum
    integer :: phase

    rc = ESMF_SUCCESS

    call ESMF_TraceGetCompID(comp, vmid, baseid, rc=rc)
    if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_GridCompGet(comp, currentMethod=method, &
         currentPhase=phase, rc=rc)
    if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return

    method_enum = ESMF_TraceMethodToEnum(method)

    call c_esmftrace_phase_epilogue_exit(vmid, baseid, method_enum, phase, rc)
    if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return
  end subroutine ESMF_TraceEventGridCompPhaseEpilogueExit
  
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_TraceEventCplCompPhaseEpilogueExit()"
  subroutine ESMF_TraceEventCplCompPhaseEpilogueExit(comp, rc)
    type(ESMF_CplComp), intent(in) :: comp
    integer, intent(out), optional  :: rc
    
    ! locals
    integer :: vmid
    integer :: baseid
    type(ESMF_Method_Flag) :: method
    integer :: method_enum
    integer :: phase

    rc = ESMF_SUCCESS

    call ESMF_TraceGetCompID(comp, vmid, baseid, rc=rc)
    if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_CplCompGet(comp, currentMethod=method, &
         currentPhase=phase, rc=rc)
    if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return

    method_enum = ESMF_TraceMethodToEnum(method)

    call c_esmftrace_phase_epilogue_exit(vmid, baseid, method_enum, phase, rc)
    if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return
  end subroutine ESMF_TraceEventCplCompPhaseEpilogueExit


#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_TraceEventGridComponentInfo()"
  subroutine ESMF_TraceEventGridComponentInfo(comp, attrConv, attrPurp, attrName, attrKey, rc)
    type(ESMF_GridComp), intent(in) :: comp
    character(len=*),    intent(in), optional :: attrConv(:)
    character(len=*),    intent(in), optional :: attrPurp(:)
    character(len=*),    intent(in), optional :: attrName(:)
    character(len=*),    intent(in), optional :: attrKey(:)
    integer, intent(out), optional  :: rc    

    ! local
    integer                    :: baseid
    integer                    :: vmid
    character(len=ESMF_MAXSTR) :: name
    logical                    :: isPresent
    integer                    :: i, j
    integer                    :: itemCount
    character(len=ESMF_MAXSTR), allocatable :: attrValList(:)
    character(len=ESMF_MAXSTR*10) :: attributeKeys
    character(len=ESMF_MAXSTR*10) :: attributeVals
    character(len=ESMF_MAXSTR*10) :: attributeList
    character(2) :: delim, delim2
    
    if (present(rc)) rc = ESMF_SUCCESS
         
    call ESMF_TraceGetCompID(comp, vmid, baseid, rc=rc)
    if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_GridCompGet(comp, name=name, rc=rc)
    if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return

    attributeKeys = ""
    attributeVals = ""

    if (present(attrConv) .and. present(attrPurp) &
         .and. present(attrName) .and. present(attrKey)) then
    
       if (size(attrConv) /= size(attrPurp) .or. &
            size(attrConv) /= size(attrName)) then
          call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
               msg="Attribute vectors must have same length", &
               ESMF_CONTEXT, rcToReturn=rc)
          return
       endif
       
       delim = ""
       do i=1, size(attrConv)      
          
          call ESMF_AttributeGet(comp, name=trim(attrName(i)), &
               convention=trim(attrConv(i)), purpose=trim(attrPurp(i)), &
               attnestflag=ESMF_ATTNEST_ON, isPresent=isPresent, itemCount=itemCount, rc=rc)
          if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
               ESMF_CONTEXT, rcToReturn=rc)) return
          
          if (isPresent) then
             allocate(attrValList(itemCount))
             call ESMF_AttributeGet(comp, name=trim(attrName(i)), valueList=attrValList, &
                  convention=trim(attrConv(i)), purpose=trim(attrPurp(i)), &
                  attnestflag=ESMF_ATTNEST_ON, isPresent=isPresent, rc=rc)
             if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
                  ESMF_CONTEXT, rcToReturn=rc)) return
             
             !attributeKeys = trim(attributeKeys)//trim(delim)// &
             !     trim(attrConv(i)) //"$"//trim(attrPurp(i))//"$"//trim(attrName(i))
             attributeKeys = trim(attributeKeys)//trim(delim)// &
                  trim(attrKey(i))
             
             attributeList = ""
             delim2 = ""
             do j=1, itemCount
                attributeList = trim(attributeList)//trim(delim2)//trim(attrValList(j))
                delim2 = "||"
             enddo
             attributeVals = trim(attributeVals)//trim(delim)//trim(attributeList)
             delim = "::"
             
             deallocate(attrValList)
          endif
       enddo
    endif
       
    !print *, "attributeKeys = ", trim(attributeKeys)
    !print *, "attributeVals = ", trim(attributeVals)
        
    call c_esmftrace_component_info(comp, vmid, baseid, trim(name), &
         trim(attributeKeys), trim(attributeVals), rc)
    if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return

  end subroutine ESMF_TraceEventGridComponentInfo


#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_TraceEventCplComponentInfo()"
  subroutine ESMF_TraceEventCplComponentInfo(comp, attrConv, attrPurp, attrName, attrKey, rc)
    type(ESMF_CplComp), intent(in) :: comp
    character(len=*),    intent(in) :: attrConv(:)
    character(len=*),    intent(in) :: attrPurp(:)
    character(len=*),    intent(in) :: attrName(:)
    character(len=*),    intent(in) :: attrKey(:)
    integer, intent(out), optional  :: rc    

    ! local
    integer                    :: baseid
    integer                    :: vmid
    character(len=ESMF_MAXSTR) :: name
    logical                    :: isPresent
    integer                    :: i, j
    integer                    :: itemCount
    character(len=ESMF_MAXSTR), allocatable :: attrValList(:)
    character(len=ESMF_MAXSTR*10) :: attributeKeys
    character(len=ESMF_MAXSTR*10) :: attributeVals
    character(len=ESMF_MAXSTR*10) :: attributeList
    character(2) :: delim, delim2
    
    if (present(rc)) rc = ESMF_SUCCESS
         
    call ESMF_TraceGetCompID(comp, vmid, baseid, rc=rc)
    if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_CplCompGet(comp, name=name, rc=rc)
    if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return

    if (size(attrConv) /= size(attrPurp) .or. &
         size(attrConv) /= size(attrName)) then
       call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
            msg="Attribute vectors must have same length", &
            ESMF_CONTEXT, rcToReturn=rc)
       return
    endif

    attributeKeys = ""
    attributeVals = ""
    delim = ""
    do i=1, size(attrConv)      

       call ESMF_AttributeGet(comp, name=trim(attrName(i)), &
            convention=trim(attrConv(i)), purpose=trim(attrPurp(i)), &
            attnestflag=ESMF_ATTNEST_ON, isPresent=isPresent, itemCount=itemCount, rc=rc)
       if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

       if (isPresent) then
          allocate(attrValList(itemCount))
          call ESMF_AttributeGet(comp, name=trim(attrName(i)), valueList=attrValList, &
               convention=trim(attrConv(i)), purpose=trim(attrPurp(i)), &
               attnestflag=ESMF_ATTNEST_ON, isPresent=isPresent, rc=rc)
          if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
               ESMF_CONTEXT, rcToReturn=rc)) then
             print *, "incorrect tk = ", attrName(i)
             return
          endif
          !attributeKeys = trim(attributeKeys)//trim(delim)// &
          !     trim(attrConv(i)) //"$"//trim(attrPurp(i))//"$"//trim(attrName(i))
          attributeKeys = trim(attributeKeys)//trim(delim)// &
               trim(attrKey(i))
          
          attributeList = ""
          delim2 = ""
          do j=1, itemCount
             attributeList = trim(attributeList)//trim(delim2)//trim(attrValList(j))
             delim2 = "||"
          enddo
          attributeVals = trim(attributeVals)//trim(delim)//trim(attributeList)
          delim = "::"

          deallocate(attrValList)
       endif
    enddo
    
    !print *, "attributeKeys = ", trim(attributeKeys)
    !print *, "attributeVals = ", trim(attributeVals)
        
    call c_esmftrace_component_info(comp, vmid, baseid, trim(name), &
         trim(attributeKeys), trim(attributeVals), rc)
    if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return

  end subroutine ESMF_TraceEventCplComponentInfo


  
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_TraceEventGetGridCompID()"
  subroutine ESMF_TraceGetGridCompID(comp, vmid, baseid, rc)
    type(ESMF_GridComp), intent(in)            :: comp
    integer,             intent(out)           :: vmid
    integer,             intent(out)           :: baseid
    integer,             intent(out), optional :: rc    
    
    ! local
    type(ESMF_VMId), pointer   :: vmidptr(:)
    character                  :: vmkey
    
    rc = ESMF_SUCCESS
    
    call ESMF_BaseGetID(comp%compp%base, baseid, rc=rc)
    if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    
    allocate(vmidptr(1))
    call ESMF_BaseGetVMId(comp%compp%base, vmidptr(1), rc=rc)
    if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return
    
    call c_ESMCI_VMIdGet (vmidptr(1), vmid, vmkey, rc)
    if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return
    
    deallocate(vmidptr)
    
  end subroutine ESMF_TraceGetGridCompID

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_TraceEventGetCplCompID()"
  subroutine ESMF_TraceGetCplCompID(comp, vmid, baseid, rc)
    type(ESMF_CplComp),  intent(in)            :: comp
    integer,             intent(out)           :: vmid
    integer,             intent(out)           :: baseid
    integer,             intent(out), optional :: rc    
    
    ! local
    type(ESMF_VMId), pointer   :: vmidptr(:)
    character                  :: vmkey
    
    rc = ESMF_SUCCESS
    
    call ESMF_BaseGetID(comp%compp%base, baseid, rc=rc)
    if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    
    allocate(vmidptr(1))
    call ESMF_BaseGetVMId(comp%compp%base, vmidptr(1), rc=rc)
    if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return
    
    call c_ESMCI_VMIdGet (vmidptr(1), vmid, vmkey, rc)
    if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return
    
    deallocate(vmidptr)
    
  end subroutine ESMF_TraceGetCplCompID

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_TraceEventRegionEnter()"
  subroutine ESMF_TraceEventRegionEnter(name, rc)
    character(len=*), intent(in) :: name
    integer, intent(out), optional  :: rc

    rc = ESMF_SUCCESS 
    
    call c_esmftrace_region_enter(name, rc)
    if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return

  end subroutine ESMF_TraceEventRegionEnter

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_TraceEventRegionExit()"
  subroutine ESMF_TraceEventRegionExit(name, rc)
    character(len=*), intent(in) :: name
    integer, intent(out), optional  :: rc

    rc = ESMF_SUCCESS 
    
    call c_esmftrace_region_exit(name, rc)
    if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return

  end subroutine ESMF_TraceEventRegionExit

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_TraceEventMemInfo()"
  subroutine ESMF_TraceEventMemInfo(rc)
    integer, intent(out), optional  :: rc
    
    rc = ESMF_SUCCESS 
    
    call c_esmftrace_mem_info(rc)
    if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return
    
  end subroutine ESMF_TraceEventMemInfo


#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_TraceEventClock()"
  subroutine ESMF_TraceEventClock(clock, rc)
    type(ESMF_Clock) :: clock
    integer, intent(out), optional  :: rc

    ! locals
    type(ESMF_Time) :: currTime
    integer :: yy, mm, dd, h, m, s
    
    rc = ESMF_SUCCESS 

    call ESMF_ClockGet(clock, currTime=currTime, rc=rc)
    if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_TimeGet(currTime, yy=yy, mm=mm, dd=dd, h=h, m=m, s=s, rc=rc)
    if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return
    call c_esmftrace_clock(yy, mm, dd, h, m, s, rc)
    if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return
    
  end subroutine ESMF_TraceEventClock
  
  
  
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_TraceMethodToEnum()"
  function ESMF_TraceMethodToEnum(method_flag)
    integer :: ESMF_TraceMethodToEnum
    type(ESMF_Method_Flag), intent(in) :: method_flag
    
    if (method_flag == ESMF_METHOD_INITIALIZE .or. &
         method_flag == ESMF_METHOD_INITIALIZEIC) then
       ESMF_TraceMethodToEnum = 0
    elseif (method_flag == ESMF_METHOD_RUN .or. &
         method_flag == ESMF_METHOD_RUNIC) then
       ESMF_TraceMethodToEnum = 1
    elseif (method_flag == ESMF_METHOD_FINALIZE .or. &
         method_flag ==  ESMF_METHOD_FINALIZEIC) then
       ESMF_TraceMethodToEnum = 2
    else
       ESMF_TraceMethodToEnum = -1  ! not supported
    endif

  end function ESMF_TraceMethodToEnum

end module
