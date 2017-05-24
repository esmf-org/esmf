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

!BOPI
! !MODULE: ESMF_TraceMod - Tracing module
!
! !DESCRIPTION:
!
! The code in this file implements the Fortran interfaces to
! ESMF's tracing capability.  
!
!
! !USES:
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

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
  private

!------------------------------------------------------------------------------
! !PUBLIC MEMBER FUNCTIONS:
  public ESMF_TraceRegionEnter
  public ESMF_TraceRegionExit

! - ESMF-internal methods:
  public ESMF_TraceOpen
  public ESMF_TraceClose
  public ESMF_TracePhaseEnter
  public ESMF_TracePhaseExit
  public ESMF_TracePhasePrologueEnter
  public ESMF_TracePhaseEpilogueExit
  public ESMF_TraceComponentInfo
  public ESMF_TraceMemInfo
  public ESMF_TraceClock
!EOPI
  
  interface ESMF_TracePhaseEnter
     module procedure ESMF_TraceGridCompPhaseEnter
     module procedure ESMF_TraceCplCompPhaseEnter
  end interface ESMF_TracePhaseEnter

  interface ESMF_TracePhaseExit
     module procedure ESMF_TraceGridCompPhaseExit
     module procedure ESMF_TraceCplCompPhaseExit
  end interface ESMF_TracePhaseExit

  interface ESMF_TracePhasePrologueEnter
     module procedure ESMF_TraceGridCompPhasePrologueEnter
     module procedure ESMF_TraceCplCompPhasePrologueEnter
  end interface ESMF_TracePhasePrologueEnter

  interface ESMF_TracePhaseEpilogueExit
     module procedure ESMF_TraceGridCompPhaseEpilogueExit
     module procedure ESMF_TraceCplCompPhaseEpilogueExit
  end interface ESMF_TracePhaseEpilogueExit

  interface ESMF_TraceComponentInfo
     module procedure ESMF_TraceGridComponentInfo
     module procedure ESMF_TraceCplComponentInfo
  end interface ESMF_TraceComponentInfo
  
  interface ESMF_TraceGetCompID
     module procedure ESMF_TraceGetGridCompID
     module procedure ESMF_TraceGetCplCompID
  end interface ESMF_TraceGetCompID
  
contains

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_TraceOpen()"
!BOPI 
! !IROUTINE: ESMF_TraceOpen - Initialize tracing infrastructure
! 
! !INTERFACE: 
  subroutine ESMF_TraceOpen(traceDir, rc)
! !ARGUMENTS: 
    character(len=*), intent(in)             :: traceDir
    integer,          intent(out), optional  :: rc
!
! !DESCRIPTION:
!   Initialize tracing infrastructure including creating output directory
!   and opening files for trace output.
!
!EOPI
!-------------------------------------------------------------------------------

    call c_esmftrace_open(traceDir, rc)
    if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return
  end subroutine ESMF_TraceOpen
  
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_TraceClose()"
!BOPI 
! !IROUTINE: ESMF_TraceClose - Finalize tracing infrastructure
! 
! !INTERFACE: 
  subroutine ESMF_TraceClose(rc)
! !ARGUMENTS:
    integer,          intent(out), optional :: rc    
!
! !DESCRIPTION:
!   Close the tracing infrastructure, flushing any
!   outstanding events to disk.  Once closed, a trace
!   cannot be re-opened.
!
!EOPI
!-------------------------------------------------------------------------------
    call c_esmftrace_close(rc)
    if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return
  end subroutine ESMF_TraceClose

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_TraceGridCompPhaseEnter()"
  subroutine ESMF_TraceGridCompPhaseEnter(comp, rc)
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

  end subroutine ESMF_TraceGridCompPhaseEnter

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_TraceCplCompPhaseEnter()"
  subroutine ESMF_TraceCplCompPhaseEnter(comp, rc)
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

  end subroutine ESMF_TraceCplCompPhaseEnter

  
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_TraceGridCompPhaseExit()"
  subroutine ESMF_TraceGridCompPhaseExit(comp, rc)
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
  end subroutine ESMF_TraceGridCompPhaseExit

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_TraceCplCompPhaseExit()"
  subroutine ESMF_TraceCplCompPhaseExit(comp, rc)
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
  end subroutine ESMF_TraceCplCompPhaseExit
  
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_TraceGridCompPhasePrologueEnter()"
  subroutine ESMF_TraceGridCompPhasePrologueEnter(comp, rc)
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

  end subroutine ESMF_TraceGridCompPhasePrologueEnter

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_TraceCplCompPhasePrologueEnter()"
  subroutine ESMF_TraceCplCompPhasePrologueEnter(comp, rc)
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

  end subroutine ESMF_TraceCplCompPhasePrologueEnter

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_TraceGridCompPhaseEpilogueExit()"
  subroutine ESMF_TraceGridCompPhaseEpilogueExit(comp, rc)
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
  end subroutine ESMF_TraceGridCompPhaseEpilogueExit
  
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_TraceCplCompPhaseEpilogueExit()"
  subroutine ESMF_TraceCplCompPhaseEpilogueExit(comp, rc)
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
  end subroutine ESMF_TraceCplCompPhaseEpilogueExit


#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_TraceGridComponentInfo()"
  subroutine ESMF_TraceGridComponentInfo(comp, attrConv, attrPurp, attrName, attrKey, rc)
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

  end subroutine ESMF_TraceGridComponentInfo


#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_TraceCplComponentInfo()"
  subroutine ESMF_TraceCplComponentInfo(comp, attrConv, attrPurp, attrName, attrKey, rc)
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

  end subroutine ESMF_TraceCplComponentInfo


  
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_TraceGetGridCompID()"
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
#define ESMF_METHOD "ESMF_TraceGetCplCompID()"
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
#define ESMF_METHOD "ESMF_TraceRegionEnter()"
!BOP 
! !IROUTINE: ESMF_TraceRegionEnter - Trace user-defined region entry event
! 
! !INTERFACE: 
  subroutine ESMF_TraceRegionEnter(name, rc)
! !ARGUMENTS: 
    character(len=*), intent(in) :: name
    integer, intent(out), optional  :: rc
!
! !DESCRIPTION:
!   Record an event in the trace for this PET indicating entry
!   into a user-defined region with the given name.  This call
!   must be paired with a call to {\tt ESMF\_TraceRegionExit()}
!   with a matching {\tt name} parameter.  User-defined regions may be
!   nested.
!   If tracing is disabled on the calling PET or for the application
!   as a whole, no event will be recorded and
!   the call will return immediately.
!
! The arguments are:
! \begin{description}
! \item[{name}]
!   A user-defined name for the region of code being entered
! \item[{[rc]}]
!   Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}    
!EOP
!-------------------------------------------------------------------------------
    rc = ESMF_SUCCESS 
    
    call c_esmftrace_region_enter(name, rc)
    if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return

  end subroutine ESMF_TraceRegionEnter
  
  
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_TraceRegionExit()"
!BOP 
! !IROUTINE: ESMF_TraceRegionExit - Trace user-defined region exit event
! 
! !INTERFACE: 
  subroutine ESMF_TraceRegionExit(name, rc)
! !ARGUMENTS: 
    character(len=*), intent(in) :: name
    integer, intent(out), optional  :: rc
!
! !DESCRIPTION:
!   Record an event in the trace for this PET indicating exit
!   from a user-defined region with the given name.  This call
!   must appear after a call to {\tt ESMF\_TraceRegionEnter()}
!   with a matching {\tt name} parameter.
!   If tracing is disabled on the calling PET or for the application
!   as a whole, no event will be recorded and
!   the call will return immediately.
!
! The arguments are:
! \begin{description}
! \item[{name}]
!   A user-defined name for the region of code being exited
! \item[{[rc]}]
!   Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}    
!EOP
!-------------------------------------------------------------------------------
    rc = ESMF_SUCCESS 
    
    call c_esmftrace_region_exit(name, rc)
    if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return

  end subroutine ESMF_TraceRegionExit

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_TraceMemInfo()"
  subroutine ESMF_TraceMemInfo(rc)
    integer, intent(out), optional  :: rc
    
    rc = ESMF_SUCCESS 
    
    call c_esmftrace_mem_info(rc)
    if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return
    
  end subroutine ESMF_TraceMemInfo


#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_TraceClock()"
  subroutine ESMF_TraceClock(clock, rc)
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
    
  end subroutine ESMF_TraceClock
  
  
  
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
