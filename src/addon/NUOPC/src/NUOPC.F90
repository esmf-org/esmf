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

module NUOPC

  !-----------------------------------------------------------------------------
  ! Generic code collection
  !-----------------------------------------------------------------------------

  use ESMF
  use NUOPC_FreeFormatDef
  use NUOPC_Base
  use NUOPC_Auxiliary
  use NUOPC_Comp

  implicit none
  
  private

  ! public
  public NUOPC_PhaseMapStringLength       ! parameter
  
  ! public FreeFormat API
  public NUOPC_FreeFormat                 ! type
  public NUOPC_FreeFormatLen              ! parameter
  public NUOPC_FreeFormatCreate           ! method
  public NUOPC_FreeFormatDestroy          ! method
  public NUOPC_FreeFormatGet              ! method
  public NUOPC_FreeFormatGetLine          ! method
  public NUOPC_FreeFormatPrint            ! method

  ! public FieldDictionary API
  public NUOPC_FieldDictionary            ! variable
  public NUOPC_FieldDictionaryAddEntry    ! method
  public NUOPC_FieldDictionaryGetEntry    ! method
  public NUOPC_FieldDictionaryHasEntry    ! method
  public NUOPC_FieldDictionaryMatchSyno   ! method
  public NUOPC_FieldDictionarySetSyno     ! method
  public NUOPC_FieldDictionarySetup       ! method
  public NUOPC_FieldDictionarySetAutoAdd  ! method

  ! public generic Comp API
  public NUOPC_CompAreServicesSet         ! method
  public NUOPC_CompAttributeAdd           ! method
  public NUOPC_CompAttributeEgest         ! method
  public NUOPC_CompAttributeGet           ! method
  public NUOPC_CompAttributeIngest        ! method
  public NUOPC_CompAttributeInit          ! method
  public NUOPC_CompAttributeSet           ! method
  public NUOPC_CompCheckSetClock          ! method
  public NUOPC_CompDerive                 ! method
  public NUOPC_CompFilterPhaseMap         ! method
  public NUOPC_CompSearchPhaseMap         ! method
  public NUOPC_CompSetClock               ! method
  public NUOPC_CompSetEntryPoint          ! method
  public NUOPC_CompSetInternalEntryPoint  ! method
  public NUOPC_CompSetServices            ! method
  public NUOPC_CompSpecialize             ! method

  ! public Utility API
  public NUOPC_ClockCheckSetClock         ! method
  public NUOPC_ClockInitialize            ! method
  public NUOPC_ClockPrintCurrTime         ! method
  public NUOPC_ClockPrintStartTime        ! method
  public NUOPC_ClockPrintStopTime         ! method
  public NUOPC_FieldAttributeInit         ! method
  public NUOPC_FieldAttributeGet          ! method
  public NUOPC_FieldAttributeSet          ! method
  public NUOPC_FieldBundleUpdateTime      ! method
  public NUOPC_FieldIsAtTime              ! method
  public NUOPC_FieldWrite                 ! method
  public NUOPC_StateAdvertiseField        ! method
  public NUOPC_StateAdvertiseFields       ! method
  public NUOPC_StateAttributeInit         ! method
  public NUOPC_StateAttributeGet          ! method
  public NUOPC_StateAttributeSet          ! method
  public NUOPC_StateBuildStdList          ! method
  public NUOPC_StateIsAtTime              ! method
  public NUOPC_StateIsFieldConnected      ! method
  public NUOPC_StateIsUpdated             ! method
  public NUOPC_StateNamespaceAdd          ! method
  public NUOPC_StateRealizeField          ! method
  public NUOPC_StateReconcile             ! method
  public NUOPC_StateSetTimestamp          ! method
  public NUOPC_StateUpdateTimestamp       ! method
  public NUOPC_StateWrite                 ! method
  public NUOPC_TimePrint                  ! method
  ! -- v7 DONE

  ! -- PRELIM. NEED REVIEW utility methods following the new v7 scheme
  public NUOPC_Add
  public NUOPC_Advertise
  public NUOPC_AttributeGet
  public NUOPC_AttributeSet
  public NUOPC_CheckSet
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
  
  ! public Auxiliary API
  public NUOPC_ConvertStringToInt         ! method
  public NUOPC_CreateSimpleSphGrid        ! method
  public NUOPC_CreateSimpleXYGrid         ! method
  public NUOPC_FillField                  ! method

end module
