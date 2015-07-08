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
  use NUOPC_FreeFormatDef
  use NUOPC_Base
!TODO: completely remove after CSC okay:  use NUOPC_RunSequenceDef
  use NUOPC_Comp

  implicit none
  
  private

  ! public
  public NUOPC_PhaseMapStringLength       ! parameter
  
  ! public FreeFormat API
  public NUOPC_FreeFormat                 ! type
  public NUOPC_FreeFormatLen              ! parameter
  public NUOPC_FreeFormatCreate
  public NUOPC_FreeFormatDestroy
  public NUOPC_FreeFormatGet
  public NUOPC_FreeFormatPrint

  ! public FieldDictionary API
  public NUOPC_FieldDictionary            ! variable
  public NUOPC_FieldDictionaryAddEntry
  public NUOPC_FieldDictionaryGetEntry
  public NUOPC_FieldDictionaryHasEntry
  public NUOPC_FieldDictionaryMatchSyno  
  public NUOPC_FieldDictionarySetSyno  
  public NUOPC_FieldDictionarySetup
  public NUOPC_FieldDictionarySetAutoAdd

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
  public NUOPC_FieldIsAtTime
  public NUOPC_FieldWrite
  public NUOPC_GridCreateSimpleSph
  public NUOPC_GridCreateSimpleXY
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
  ! -- utility methods following the new v7 scheme
  public NUOPC_Add
  public NUOPC_Advertise
  public NUOPC_AttributeAdd
  public NUOPC_AttributeGet
  public NUOPC_AttributeSet
  public NUOPC_CheckSet
  public NUOPC_Create
  public NUOPC_FillData
  public NUOPC_Get
  public NUOPC_IsAtTime
  public NUOPC_IsConnected
  public NUOPC_IsCreated
  public NUOPC_IsUpdated
  public NUOPC_Nop
  public NUOPC_Print
  public NUOPC_Realize
  public NUOPC_Reconcile
  public NUOPC_UpdateTimestamp
  public NUOPC_Write
  
!TODO: completely remove after CSC okay:  ! defined in NUOPC_RunSequenceDef
!TODO: completely remove after CSC okay:  public NUOPC_RunElement
!TODO: completely remove after CSC okay:  public NUOPC_RunElementAdd, NUOPC_RunElementAddComp, NUOPC_RunElementAddLink
!TODO: completely remove after CSC okay:  public NUOPC_RunElementPrint
!TODO: completely remove after CSC okay:  public NUOPC_RunSequence
!TODO: completely remove after CSC okay:  public NUOPC_RunSequenceAdd, NUOPC_RunSequenceSet, NUOPC_RunSequencePrint
!TODO: completely remove after CSC okay:  public NUOPC_RunSequenceDeallocate
!TODO: completely remove after CSC okay:  public NUOPC_RunSequenceIterate
  
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
