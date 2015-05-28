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
  public NUOPC_FieldAttributeAdd
  public NUOPC_FieldAttributeGet
  public NUOPC_FieldAttributeSet
  public NUOPC_FieldBundleUpdateTime
  public NUOPC_FieldDictionaryAddEntry
  public NUOPC_FieldDictionaryGetEntry
  public NUOPC_FieldDictionaryHasEntry
  public NUOPC_FieldDictionaryMatchSyno  
  public NUOPC_FieldDictionarySetSyno  
  public NUOPC_FieldDictionarySetup
  public NUOPC_FieldDictionarySetAutoAdd
  public NUOPC_FieldIsAtTime
  public NUOPC_FieldWrite
  public NUOPC_GridCreateSimpleSph
  public NUOPC_GridCreateSimpleXY
  public NUOPC_IsCreated
  public NUOPC_Nop
  public NUOPC_StateAdvertiseField
  public NUOPC_StateAdvertiseFields
  public NUOPC_StateAttributeAdd
  public NUOPC_StateAttributeGet
  public NUOPC_StateAttributeSet
  public NUOPC_StateBuildStdList
  public NUOPC_StateIsAllConnected
  public NUOPC_StateIsAtTime
  public NUOPC_StateIsFieldConnected
  public NUOPC_StateIsUpdated
  public NUOPC_StateNamespaceAdd
  public NUOPC_StateRealizeField
  public NUOPC_StateReconcile
  public NUOPC_StateSetTimestamp
  public NUOPC_StateUpdateTimestamp
  public NUOPC_StateWrite
  public NUOPC_TimePrint
  public NUOPC_Write
  
  ! defined in NUOPC_RunSequenceDef
  public NUOPC_RunElement
  public NUOPC_RunElementAdd, NUOPC_RunElementAddComp, NUOPC_RunElementAddLink
  public NUOPC_RunElementPrint
  public NUOPC_RunSequence
  public NUOPC_RunSequenceAdd, NUOPC_RunSequenceSet, NUOPC_RunSequencePrint
  public NUOPC_RunSequenceDeallocate
  public NUOPC_RunSequenceIterate
  
  ! defined in NUOPC_Comp
  public NUOPC_CompAreServicesSet  
  public NUOPC_CompAttributeAdd
  public NUOPC_CompAttributeGet
  public NUOPC_CompAttributeSet
  public NUOPC_CompCheckSetClock
  public NUOPC_CompDerive
  public NUOPC_CompFilterPhaseMap
  public NUOPC_CompSearchPhaseMap
  public NUOPC_CompSetClock
  public NUOPC_CompSetEntryPoint
  public NUOPC_CompSetInternalEntryPoint
  public NUOPC_CompSetServices
  public NUOPC_CompSpecialize

end module
