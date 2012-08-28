! $Id: NUOPC.F90,v 1.29 2012/08/28 23:04:37 theurich Exp $

#define FILENAME "src/addon/NUOPC/NUOPC.F90"

!TODO: make this macros available through ESMF as parameter or find other way
#define ESMF_INIT_CREATED 82949521

module NUOPC

  !-----------------------------------------------------------------------------
  ! Generic code collection
  !-----------------------------------------------------------------------------

  use ESMF
  use NUOPC_Base
  use NUOPC_RunSequenceDef

  implicit none
  
  private

  ! public module variables  
  public NUOPC_FieldDictionary

  ! public module interfaces
  public NUOPC_FieldDictionarySetup
  public NUOPC_FieldDictionaryAddEntry  
  public NUOPC_FieldAttributeAdd
  public NUOPC_FieldAttributeGet
  public NUOPC_CplCompAreServicesSet
  public NUOPC_CplCompAttributeAdd
  public NUOPC_CplCompAttributeGet
  public NUOPC_CplCompAttributeSet
  public NUOPC_GridCompAttributeAdd
  public NUOPC_TimePrint
  public NUOPC_ClockCheckSetClock
  public NUOPC_ClockPrintCurrTime
  public NUOPC_ClockPrintStartTime
  public NUOPC_ClockPrintStopTime
  public NUOPC_ClockInitialize
  public NUOPC_GridCompAreServicesSet  
  public NUOPC_GridCompSetClock
  public NUOPC_GridCompCheckSetClock
  public NUOPC_StateAdvertiseField
  public NUOPC_StateBuildStdList
  public NUOPC_StateIsAllConnected
  public NUOPC_StateIsFieldConnected
  public NUOPC_StateIsAtTime
  public NUOPC_StateRealizeField
  public NUOPC_StateSetTimestamp
  public NUOPC_StateUpdateTimestamp
  public NUOPC_FieldBundleUpdateTime
  public NUOPC_GridCreateSimpleXY
  
  public NUOPC_IsCreated
  
  ! defined in NUOPC_RunSequenceDef
  public NUOPC_RunElement, NUOPC_RunSequence
  public NUOPC_RunElementAdd, NUOPC_RunSequenceAdd
  public NUOPC_RunElementPrint, NUOPC_RunSequencePrint
  public NUOPC_RunSequenceSet
  public NUOPC_RunSequenceDeallocate
  public NUOPC_RunSequenceIterate
  
end module
