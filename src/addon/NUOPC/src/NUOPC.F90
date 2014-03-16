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
#define FILENAME "src/addon/NUOPC/src/NUOPC.F90"
!==============================================================================

!TODO: make this macros available through ESMF as parameter or find other way
#define ESMF_INIT_CREATED 82949521

module NUOPC

  !-----------------------------------------------------------------------------
  ! Generic code collection
  !-----------------------------------------------------------------------------

  use ESMF
  use NUOPC_Base
  use NUOPC_RunSequenceDef
  use NUOPC_Comp

  implicit none
  
  private

  ! public module variables  
  public NUOPC_FieldDictionary
  public NUOPC_PhaseMapStringLength
  
  ! public module interfaces
  public NUOPC_ClockCheckSetClock
  public NUOPC_ClockInitialize
  public NUOPC_ClockPrintCurrTime
  public NUOPC_ClockPrintStartTime
  public NUOPC_ClockPrintStopTime
  public NUOPC_CplCompAreServicesSet
  public NUOPC_CplCompAttributeAdd
  public NUOPC_CplCompAttributeGet
  public NUOPC_CplCompAttributeSet
  public NUOPC_FieldAttributeAdd
  public NUOPC_FieldAttributeGet
  public NUOPC_FieldAttributeSet
  public NUOPC_FieldBundleUpdateTime
  public NUOPC_FieldDictionaryAddEntry  
  public NUOPC_FieldDictionaryGetEntry  
  public NUOPC_FieldDictionaryHasEntry  
  public NUOPC_FieldDictionarySetup
  public NUOPC_FieldIsAtTime
  public NUOPC_FieldWrite
  public NUOPC_FillCplList
  public NUOPC_GridCompAreServicesSet  
  public NUOPC_GridCompAttributeAdd
  public NUOPC_GridCompCheckSetClock
  public NUOPC_GridCompSetClock
  public NUOPC_GridCompSetServices
  public NUOPC_GridCreateSimpleSph
  public NUOPC_GridCreateSimpleXY
  public NUOPC_IsCreated
  public NUOPC_StateAdvertiseField
  public NUOPC_StateAdvertiseFields
  public NUOPC_StateBuildStdList
  public NUOPC_StateIsAllConnected
  public NUOPC_StateIsAtTime
  public NUOPC_StateIsFieldConnected
  public NUOPC_StateIsUpdated
  public NUOPC_StateRealizeField
  public NUOPC_StateSetTimestamp
  public NUOPC_StateUpdateTimestamp
  public NUOPC_StateWrite
  public NUOPC_TimePrint
  
  ! defined in NUOPC_RunSequenceDef
  public NUOPC_RunElement
  public NUOPC_RunElementAdd, NUOPC_RunElementAddComp, NUOPC_RunElementAddLink
  public NUOPC_RunElementPrint
  public NUOPC_RunSequence
  public NUOPC_RunSequenceAdd, NUOPC_RunSequenceSet, NUOPC_RunSequencePrint
  public NUOPC_RunSequenceDeallocate
  public NUOPC_RunSequenceIterate
  
  ! defined in NUOPC_Comp
  public NUOPC_CompDerive
  public NUOPC_CompSpecialize

end module
