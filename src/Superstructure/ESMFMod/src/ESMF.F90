! $Id: ESMF.F90,v 1.48.2.9 2009/01/21 21:25:24 cdeluca Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2009, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
!


module ESMF_Mod

    use ESMF_UtilTypesMod
    use ESMF_UtilMod

    use ESMF_LogErrMod
    use ESMF_InitMacrosMod
    
    use ESMF_F90InterfaceMod
    use ESMF_FortranWordsizeMod
    use ESMF_TypeKindGetMod
    
    use ESMF_BaseMod
    use ESMF_IOSpecMod

    use ESMF_FractionMod
    use ESMF_BaseTimeMod
    use ESMF_CalendarMod
    use ESMF_TimeIntervalMod
    use ESMF_TimeMod
    use ESMF_AlarmMod
    use ESMF_ClockMod

    use ESMF_ArraySpecMod
    use ESMF_LocalArrayMod
    use ESMF_InternArrayDataMapMod
    
    use ESMF_VMMod 
    use ESMF_DELayoutMod
    use ESMF_DistGridMod
    use ESMF_ArrayMod
    use ESMF_ArrayBundleMod

    use ESMF_ConfigMod

    use ESMF_InternArrayMod
    use ESMF_InternArrayCreateMod
    use ESMF_InternArrayGetMod

    use ESMF_InternDGMod
    use ESMF_GridMod
    use ESMF_StaggerLocMod

    use ESMF_XPacketMod
    use ESMF_CommTableMod
    use ESMF_RTableMod
    use ESMF_RouteMod
    use ESMF_RHandleMod

 !   use ESMF_FieldDataMapMod
 !   use ESMF_InternArrayCommMod

    use ESMF_FieldMod
    use ESMF_FieldPrMod
    use ESMF_FieldGetMod
    use ESMF_FieldSetMod
    use ESMF_FieldCreateMod
    use ESMF_FieldBundleMod
!    use ESMF_FieldBundleGetMod

    use ESMF_AttributeMod

!    use ESMF_RegridTypesMod
!    use ESMF_RegridMod

!    use ESMF_FieldCommMod
!    use ESMF_FieldBundleCommMod

    use ESMF_StateTypesMod
    use ESMF_StateMod
!    use ESMF_StateGetMod
    use ESMF_StateReconcileMod
    use ESMF_CompMod
    use ESMF_GridCompMod
    use ESMF_CplCompMod
    
    use ESMF_InitMod

end module ESMF_Mod
