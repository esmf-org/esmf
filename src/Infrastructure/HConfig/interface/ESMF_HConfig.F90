! $Id$
!
! Earth System Modeling Framework
! Copyright (c) 2002-2023, University Corporation for Atmospheric Research, 
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
! Laboratory, University of Michigan, National Centers for Environmental 
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
#define ESMF_FILENAME "ESMF_HConfig.F90"
!==============================================================================
!
! ESMF HConfig Module
module ESMF_HConfigMod
!
!==============================================================================
!
! This file contains the F90 wrapper code for the C++ implementation of
!  the HConfig class.
!
!------------------------------------------------------------------------------
! INCLUDES
#include "ESMF.h"

!==============================================================================
!BOPI
! !MODULE: ESMF_HConfigMod
!

!   F90 API wrapper of C++ implementation of HConfig
!
!------------------------------------------------------------------------------

! !USES:
  use ESMF_UtilTypesMod           ! ESMF utility types
  use ESMF_InitMacrosMod          ! ESMF initializer macros
  use ESMF_LogErrMod              ! ESMF error handling
  use ESMF_VMMod                  ! ESMF VM
  use ESMF_IOUtilMod              ! ESMF I/O utility layer

  implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
  private

!------------------------------------------------------------------------------
!     ! ESMF_HConfig
!
!------------------------------------------------------------------------------

  !TODO: Currently both ESMF_HConfig and ESMF_HConfigIter are identical
  !TODO: types. The implementation simply mimics a difference on the user API
  !TODO: level. The current way matches the deep C++ implementation of HConfig,
  !TODO: however, in the long run it would be nice to clean this up.
  !TODO: What really should happen is that there is a separate deep C++
  !TODO: implementation for ESMF_HConfig and ESMF_HConfigIter, for nodes and
  !TODO: iterators, respectively. At that point the Fortran derived types would
  !TODO: also change to reflect this.
  !TODO: As a consquence the inernal ESMF_HConfigIterAsHConfig() would go away,
  !TODO: and the HConfig vs. HConfigIter implementations of overloaded
  !TODO: interfaces would look different, depending on which type it is.

  ! Fortran type to hold pointer to C++ object
  type ESMF_HConfig
#ifndef ESMF_NO_SEQUENCE
  sequence
#endif
  private
    ! 20 x 4-byte to safely store a few items on the C++ side. This size has
    ! been emperically determined, and might need to increase if the C++ side
    ! changes!
    ! Keeping this memory as shallow on the stack eliminates the need for
    ! complicated garbage collection around heap memory.
#ifndef ESMF_NO_INITIALIZERS
    integer(ESMF_KIND_I4), dimension(20) :: shallowMemory = 0
#else
    integer(ESMF_KIND_I4), dimension(20) :: shallowMemory
#endif
    logical ::  loopFirst = .true.
    ESMF_INIT_DECLARE
  end type

  ! Fortran type to hold pointer to C++ object
  type ESMF_HConfigIter
#ifndef ESMF_NO_SEQUENCE
  sequence
#endif
  private
    ! 20 x 4-byte to safely store a few items on the C++ side. This size has
    ! been emperically determined, and might need to increase if the C++ side
    ! changes!
    ! Keeping this memory as shallow on the stack eliminates the need for
    ! complicated garbage collection around heap memory.
#ifndef ESMF_NO_INITIALIZERS
    integer(ESMF_KIND_I4), dimension(20) :: shallowMemory = 0
#else
    integer(ESMF_KIND_I4), dimension(20) :: shallowMemory
#endif
    logical ::  loopFirst = .true.
    ESMF_INIT_DECLARE
  end type

!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
! !PUBLIC TYPES:
  public ESMF_HConfig
  public ESMF_HConfigIter

!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:

! - ESMF-public methods:
  public operator(==)
  public operator(/=)

  public ESMF_HConfigAdd
  public ESMF_HConfigAddMapKey
  public ESMF_HConfigAddMapVal

  public ESMF_HConfigAsI4
  public ESMF_HConfigAsI4MapKey
  public ESMF_HConfigAsI4MapVal

  public ESMF_HConfigAsI4Seq
  public ESMF_HConfigAsI4SeqMapKey
  public ESMF_HConfigAsI4SeqMapVal

  public ESMF_HConfigAsI8
  public ESMF_HConfigAsI8MapKey
  public ESMF_HConfigAsI8MapVal

  public ESMF_HConfigAsI8Seq
  public ESMF_HConfigAsI8SeqMapKey
  public ESMF_HConfigAsI8SeqMapVal

  public ESMF_HConfigAsLogical
  public ESMF_HConfigAsLogicalMapKey
  public ESMF_HConfigAsLogicalMapVal

  public ESMF_HConfigAsLogicalSeq
  public ESMF_HConfigAsLogicalSeqMapKey
  public ESMF_HConfigAsLogicalSeqMapVal

  public ESMF_HConfigAsR4
  public ESMF_HConfigAsR4MapKey
  public ESMF_HConfigAsR4MapVal

  public ESMF_HConfigAsR4Seq
  public ESMF_HConfigAsR4SeqMapKey
  public ESMF_HConfigAsR4SeqMapVal

  public ESMF_HConfigAsR8
  public ESMF_HConfigAsR8MapKey
  public ESMF_HConfigAsR8MapVal

  public ESMF_HConfigAsR8Seq
  public ESMF_HConfigAsR8SeqMapKey
  public ESMF_HConfigAsR8SeqMapVal

  public ESMF_HConfigAsString
  public ESMF_HConfigAsStringMapKey
  public ESMF_HConfigAsStringMapVal

  public ESMF_HConfigAsStringSeq
  public ESMF_HConfigAsStringSeqMapKey
  public ESMF_HConfigAsStringSeqMapVal

  public ESMF_HConfigCreate
  public ESMF_HConfigCreateAt
  public ESMF_HConfigCreateAtMapKey
  public ESMF_HConfigCreateAtMapVal

  public ESMF_HConfigDestroy

  public ESMF_HConfigFileLoad
  public ESMF_HConfigFileSave

  public ESMF_HConfigGetDocCount

  public ESMF_HConfigGetSize
  public ESMF_HConfigGetSizeMapKey
  public ESMF_HConfigGetSizeMapVal

  public ESMF_HConfigGetTag
  public ESMF_HConfigGetTagMapKey
  public ESMF_HConfigGetTagMapVal

  public ESMF_HConfigIsDefined
  public ESMF_HConfigIsDefinedMapKey
  public ESMF_HConfigIsDefinedMapVal

  public ESMF_HConfigIsNull
  public ESMF_HConfigIsNullMapKey
  public ESMF_HConfigIsNullMapVal

  public ESMF_HConfigIsMap
  public ESMF_HConfigIsMapMapKey
  public ESMF_HConfigIsMapMapVal

  public ESMF_HConfigIsScalar
  public ESMF_HConfigIsScalarMapKey
  public ESMF_HConfigIsScalarMapVal

  public ESMF_HConfigIsSequence
  public ESMF_HConfigIsSequenceMapKey
  public ESMF_HConfigIsSequenceMapVal

  public ESMF_HConfigIterAsHConfig

  public ESMF_HConfigIterBegin
  public ESMF_HConfigIterBeginMapKey
  public ESMF_HConfigIterBeginMapVal

  public ESMF_HConfigIterEnd
  public ESMF_HConfigIterEndMapKey
  public ESMF_HConfigIterEndMapVal

  public ESMF_HConfigIterIsMap
  public ESMF_HConfigIterIsSequence

  public ESMF_HConfigIterLoop
  public ESMF_HConfigIterNext

  public ESMF_HConfigLoad

  public ESMF_HConfigRemove

  public ESMF_HConfigSet
  public ESMF_HConfigSetMapKey
  public ESMF_HConfigSetMapVal

! - ESMF-internal methods:
  public ESMF_HConfigGetInit
!EOPI
!------------------------------------------------------------------------------

!==============================================================================
!
! INTERFACE BLOCKS
!
!==============================================================================

  interface ESMF_HConfigAdd
    module procedure ESMF_HConfigAddHConfig
    module procedure ESMF_HConfigIterAddHConfig
    module procedure ESMF_HConfigAddI4
    module procedure ESMF_HConfigIterAddI4
    module procedure ESMF_HConfigAddI4Seq
    module procedure ESMF_HConfigIterAddI4Seq
    module procedure ESMF_HConfigAddI8
    module procedure ESMF_HConfigIterAddI8
    module procedure ESMF_HConfigAddI8Seq
    module procedure ESMF_HConfigIterAddI8Seq
    module procedure ESMF_HConfigAddLogical
    module procedure ESMF_HConfigIterAddLogical
    module procedure ESMF_HConfigAddLogicalSeq
    module procedure ESMF_HConfigIterAddLogicalSeq
    module procedure ESMF_HConfigAddR4
    module procedure ESMF_HConfigIterAddR4
    module procedure ESMF_HConfigAddR4Seq
    module procedure ESMF_HConfigIterAddR4Seq
    module procedure ESMF_HConfigAddR8
    module procedure ESMF_HConfigIterAddR8
    module procedure ESMF_HConfigAddR8Seq
    module procedure ESMF_HConfigIterAddR8Seq
    module procedure ESMF_HConfigAddString
    module procedure ESMF_HConfigIterAddString
    module procedure ESMF_HConfigAddStringSeq
    module procedure ESMF_HConfigIterAddStringSeq
  end interface

  interface ESMF_HConfigAddMapKey
    module procedure ESMF_HConfigAddMapKeyHConfig
    module procedure ESMF_HConfigAddMapKeyI4
    module procedure ESMF_HConfigAddMapKeyI4Seq
    module procedure ESMF_HConfigAddMapKeyI8
    module procedure ESMF_HConfigAddMapKeyI8Seq
    module procedure ESMF_HConfigAddMapKeyLogical
    module procedure ESMF_HConfigAddMapKeyLogicalSeq
    module procedure ESMF_HConfigAddMapKeyR4
    module procedure ESMF_HConfigAddMapKeyR4Seq
    module procedure ESMF_HConfigAddMapKeyR8
    module procedure ESMF_HConfigAddMapKeyR8Seq
    module procedure ESMF_HConfigAddMapKeyString
    module procedure ESMF_HConfigAddMapKeyStringSeq
  end interface

  interface ESMF_HConfigAddMapVal
    module procedure ESMF_HConfigAddMapValHConfig
    module procedure ESMF_HConfigAddMapValI4
    module procedure ESMF_HConfigAddMapValI4Seq
    module procedure ESMF_HConfigAddMapValI8
    module procedure ESMF_HConfigAddMapValI8Seq
    module procedure ESMF_HConfigAddMapValLogical
    module procedure ESMF_HConfigAddMapValLogicalSeq
    module procedure ESMF_HConfigAddMapValR4
    module procedure ESMF_HConfigAddMapValR4Seq
    module procedure ESMF_HConfigAddMapValR8
    module procedure ESMF_HConfigAddMapValR8Seq
    module procedure ESMF_HConfigAddMapValString
    module procedure ESMF_HConfigAddMapValStringSeq
  end interface

  interface ESMF_HConfigAsI4
    module procedure ESMF_HConfigAsI4
    module procedure ESMF_HConfigIterAsI4
  end interface

  interface ESMF_HConfigAsI8
    module procedure ESMF_HConfigAsI8
    module procedure ESMF_HConfigIterAsI8
  end interface

  interface ESMF_HConfigAsLogical
    module procedure ESMF_HConfigAsLogical
    module procedure ESMF_HConfigIterAsLogical
  end interface

  interface ESMF_HConfigAsR4
    module procedure ESMF_HConfigAsR4
    module procedure ESMF_HConfigIterAsR4
  end interface

  interface ESMF_HConfigAsR8
    module procedure ESMF_HConfigAsR8
    module procedure ESMF_HConfigIterAsR8
  end interface

  interface ESMF_HConfigAsString
    module procedure ESMF_HConfigAsString
    module procedure ESMF_HConfigIterAsString
  end interface

  interface ESMF_HConfigAsI4Seq
    module procedure ESMF_HConfigAsI4Seq
    module procedure ESMF_HConfigIterAsI4Seq
  end interface

  interface ESMF_HConfigAsI8Seq
    module procedure ESMF_HConfigAsI8Seq
    module procedure ESMF_HConfigIterAsI8Seq
  end interface

  interface ESMF_HConfigAsLogicalSeq
    module procedure ESMF_HConfigAsLogicalSeq
    module procedure ESMF_HConfigIterAsLogicalSeq
  end interface

  interface ESMF_HConfigAsR4Seq
    module procedure ESMF_HConfigAsR4Seq
    module procedure ESMF_HConfigIterAsR4Seq
  end interface

  interface ESMF_HConfigAsR8Seq
    module procedure ESMF_HConfigAsR8Seq
    module procedure ESMF_HConfigIterAsR8Seq
  end interface

  interface ESMF_HConfigAsStringSeq
    module procedure ESMF_HConfigAsStringSeq
    module procedure ESMF_HConfigIterAsStringSeq
  end interface

  interface ESMF_HConfigCreate
    module procedure ESMF_HConfigCreateDefault
    module procedure ESMF_HConfigCreateHConfig
    module procedure ESMF_HConfigCreateI4
    module procedure ESMF_HConfigCreateI4Seq
    module procedure ESMF_HConfigCreateI8
    module procedure ESMF_HConfigCreateI8Seq
    module procedure ESMF_HConfigCreateLogical
    module procedure ESMF_HConfigCreateLogicalSeq
    module procedure ESMF_HConfigCreateR4
    module procedure ESMF_HConfigCreateR4Seq
    module procedure ESMF_HConfigCreateR8
    module procedure ESMF_HConfigCreateR8Seq
    module procedure ESMF_HConfigCreateStringSeq
  end interface

  interface ESMF_HConfigCreateAt
    module procedure ESMF_HConfigCreateAt
    module procedure ESMF_HConfigIterCreateAt
  end interface

  interface ESMF_HConfigGetSize
    module procedure ESMF_HConfigGetSize
    module procedure ESMF_HConfigIterGetSize
  end interface

  interface ESMF_HConfigGetTag
    module procedure ESMF_HConfigGetTag
    module procedure ESMF_HConfigIterGetTag
  end interface

  interface ESMF_HConfigIsDefined
    module procedure ESMF_HConfigIsDefined
    module procedure ESMF_HConfigItrIsDefined
  end interface

  interface ESMF_HConfigIsNull
    module procedure ESMF_HConfigIsNull
    module procedure ESMF_HConfigItrIsNull
  end interface

  interface ESMF_HConfigIsMap
    module procedure ESMF_HConfigIsMap
    module procedure ESMF_HConfigItrIsMap
  end interface

  interface ESMF_HConfigIsScalar
    module procedure ESMF_HConfigIsScalar
    module procedure ESMF_HConfigItrIsScalar
  end interface

  interface ESMF_HConfigIsSequence
    module procedure ESMF_HConfigIsSequence
    module procedure ESMF_HConfigItrIsSequence
  end interface

  interface ESMF_HConfigIterBegin
    module procedure ESMF_HConfigIterBegin
    module procedure ESMF_HConfigItrIterBegin
  end interface

  interface ESMF_HConfigIterEnd
    module procedure ESMF_HConfigIterEnd
    module procedure ESMF_HConfigItrIterEnd
  end interface

  interface ESMF_HConfigRemove
    module procedure ESMF_HConfigRemove
    module procedure ESMF_HConfigIterRemove
  end interface

  interface ESMF_HConfigSet
    module procedure ESMF_HConfigSetHConfig
    module procedure ESMF_HConfigIterSetHConfig
    module procedure ESMF_HConfigSetI4
    module procedure ESMF_HConfigIterSetI4
    module procedure ESMF_HConfigSetI4Seq
    module procedure ESMF_HConfigIterSetI4Seq
    module procedure ESMF_HConfigSetI8
    module procedure ESMF_HConfigIterSetI8
    module procedure ESMF_HConfigSetI8Seq
    module procedure ESMF_HConfigIterSetI8Seq
    module procedure ESMF_HConfigSetLogical
    module procedure ESMF_HConfigIterSetLogical
    module procedure ESMF_HConfigSetLogicalSeq
    module procedure ESMF_HConfigIterSetLogicalSeq
    module procedure ESMF_HConfigSetR4
    module procedure ESMF_HConfigIterSetR4
    module procedure ESMF_HConfigSetR4Seq
    module procedure ESMF_HConfigIterSetR4Seq
    module procedure ESMF_HConfigSetR8
    module procedure ESMF_HConfigIterSetR8
    module procedure ESMF_HConfigSetR8Seq
    module procedure ESMF_HConfigIterSetR8Seq
    module procedure ESMF_HConfigSetString
    module procedure ESMF_HConfigIterSetString
    module procedure ESMF_HConfigSetStringSeq
    module procedure ESMF_HConfigIterSetStringSeq
  end interface

  interface ESMF_HConfigSetMapKey
    module procedure ESMF_HConfigSetMapKeyHConfig
    module procedure ESMF_HConfigSetMapKeyI4
    module procedure ESMF_HConfigSetMapKeyI4Seq
    module procedure ESMF_HConfigSetMapKeyI8
    module procedure ESMF_HConfigSetMapKeyI8Seq
    module procedure ESMF_HConfigSetMapKeyLogical
    module procedure ESMF_HConfigSetMapKeyLogicalSeq
    module procedure ESMF_HConfigSetMapKeyR4
    module procedure ESMF_HConfigSetMapKeyR4Seq
    module procedure ESMF_HConfigSetMapKeyR8
    module procedure ESMF_HConfigSetMapKeyR8Seq
    module procedure ESMF_HConfigSetMapKeyString
    module procedure ESMF_HConfigSetMapKeyStringSeq
  end interface

  interface ESMF_HConfigSetMapVal
    module procedure ESMF_HConfigSetMapValHConfig
    module procedure ESMF_HConfigSetMapValI4
    module procedure ESMF_HConfigSetMapValI4Seq
    module procedure ESMF_HConfigSetMapValI8
    module procedure ESMF_HConfigSetMapValI8Seq
    module procedure ESMF_HConfigSetMapValLogical
    module procedure ESMF_HConfigSetMapValLogicalSeq
    module procedure ESMF_HConfigSetMapValR4
    module procedure ESMF_HConfigSetMapValR4Seq
    module procedure ESMF_HConfigSetMapValR8
    module procedure ESMF_HConfigSetMapValR8Seq
    module procedure ESMF_HConfigSetMapValString
    module procedure ESMF_HConfigSetMapValStringSeq
  end interface

! -------------------------- ESMF-public interface ----------------------------
!BOP
! !IROUTINE: ESMF_HConfigOperator(==) - HConfig equality operator
!
! !INTERFACE:
  interface operator(==)
!   if (hconfig1 == hconfig2) then ... endif
!             OR
!   result = (hconfig1 == hconfig2)
! !RETURN VALUE:
!   logical :: result
!
! !ARGUMENTS:
!   type(ESMF_HConfig), intent(in) :: hconfig1
!   type(ESMF_HConfig), intent(in) :: hconfig2
!
! !DESCRIPTION:
!   Test whether hconfig1 and hconfig2 are valid aliases to the same ESMF
!   HConfig object in memory. For a more general comparison of two 
!   ESMF HConfigs, going beyond the simple alias test, the 
!   {\tt ESMF\_HConfigMatch()} function (not yet fully implemented) must 
!   be used.
!
!   The arguments are:
!   \begin{description}
!   \item[hconfig1]
!     The {\tt ESMF\_HConfig} object on the left hand side of the equality
!     operation.
!   \item[hconfig2]
!     The {\tt ESMF\_HConfig} object on the right hand side of the equality
!     operation.
!   \end{description}
!
!EOP
    module procedure ESMF_HConfigEQ
    module procedure ESMF_HConfigIterEQ

  end interface
!------------------------------------------------------------------------------

! -------------------------- ESMF-public interface ----------------------------
!BOP
! !IROUTINE: ESMF_HConfigOperator(/=) - HConfig not equal operator
!
! !INTERFACE:
  interface operator(/=)
!   if (hconfig1 /= hconfig2) then ... endif
!             OR
!   result = (hconfig1 /= hconfig2)
! !RETURN VALUE:
!   logical :: result
!
! !ARGUMENTS:
!   type(ESMF_HConfig), intent(in) :: hconfig1
!   type(ESMF_HConfig), intent(in) :: hconfig2
!
! !DESCRIPTION:
!   Test whether hconfig1 and hconfig2 are {\it not} valid aliases to the
!   same ESMF HConfig object in memory. For a more general comparison of two
!   ESMF HConfigs, going beyond the simple alias test, the
!   {\tt ESMF\_HConfigMatch()} function (not yet fully implemented) must 
!   be used.
!
!   The arguments are:
!   \begin{description}
!   \item[hconfig1]
!     The {\tt ESMF\_HConfig} object on the left hand side of the non-equality
!     operation.
!   \item[hconfig2]
!     The {\tt ESMF\_HConfig} object on the right hand side of the non-equality
!     operation.
!   \end{description}
!
!EOP
    module procedure ESMF_HConfigNE
    module procedure ESMF_HConfigIterNE

  end interface
!------------------------------------------------------------------------------


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


! -------------------------- ESMF-internal method -----------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigEQ()"
!BOPI
! !IROUTINE:  ESMF_HConfigEQ - Compare two HConfigs for equality
!
! !INTERFACE:
  function ESMF_HConfigEQ(HConfig1, HConfig2)
!
! !RETURN VALUE:
    logical :: ESMF_HConfigEQ

! !ARGUMENTS:
    type(ESMF_HConfig), intent(in) :: HConfig1
    type(ESMF_HConfig), intent(in) :: HConfig2

! !DESCRIPTION:
!   Test if both {\tt HConfig1} and {\tt HConfig2} alias the same
!   ESMF HConfig object.
!
!EOPI
!-------------------------------------------------------------------------------

    ESMF_INIT_TYPE init1, init2
    integer :: localrc1, localrc2
    logical :: lval1, lval2

    ! Use the following logic, rather than "ESMF-INIT-CHECK-DEEP", to gain 
    ! init checks on both args, and in the case where both are uninitialized,
    ! to distinguish equality based on uninitialized type (uncreated,
    ! deleted).

    ! check inputs
    init1 = ESMF_HConfigGetInit(HConfig1)
    init2 = ESMF_HConfigGetInit(HConfig2)

    ! TODO: this line must remain split in two for SunOS f90 8.3 127000-03
    if (init1 .eq. ESMF_INIT_CREATED .and. &
      init2 .eq. ESMF_INIT_CREATED) then
      ESMF_HConfigEQ = all(HConfig1%shallowMemory .eq. HConfig2%shallowMemory)
    else
      ESMF_HConfigEQ = .false.
    endif

  end function ESMF_HConfigEQ
!-------------------------------------------------------------------------------


! -------------------------- ESMF-internal method -----------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigIterEQ()"
!BOPI
! !IROUTINE:  ESMF_HConfigIterEQ - Compare two HConfigIters for equality
!
! !INTERFACE:
  function ESMF_HConfigIterEQ(HConfig1, HConfig2)
!
! !RETURN VALUE:
    logical :: ESMF_HConfigIterEQ

! !ARGUMENTS:
    type(ESMF_HConfigIter), intent(in) :: HConfig1
    type(ESMF_HConfigIter), intent(in) :: HConfig2

! !DESCRIPTION:
!   Test if both {\tt HConfig1} and {\tt HConfig2} alias the same
!   ESMF HConfigIter object.
!
!EOPI
!-------------------------------------------------------------------------------

    ESMF_INIT_TYPE init1, init2
    integer :: localrc1, localrc2
    logical :: lval1, lval2

    ! Use the following logic, rather than "ESMF-INIT-CHECK-DEEP", to gain 
    ! init checks on both args, and in the case where both are uninitialized,
    ! to distinguish equality based on uninitialized type (uncreated,
    ! deleted).

    ! check inputs
    init1 = ESMF_HConfigIterGetInit(HConfig1)
    init2 = ESMF_HConfigIterGetInit(HConfig2)

    ! TODO: this line must remain split in two for SunOS f90 8.3 127000-03
    if (init1 .eq. ESMF_INIT_CREATED .and. &
      init2 .eq. ESMF_INIT_CREATED) then
      ESMF_HConfigIterEQ = all(HConfig1%shallowMemory .eq. HConfig2%shallowMemory)
    else
      ESMF_HConfigIterEQ = .false.
    endif

  end function ESMF_HConfigIterEQ
!-------------------------------------------------------------------------------


! -------------------------- ESMF-internal method -----------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigNE()"
!BOPI
! !IROUTINE:  ESMF_HConfigNE - Compare two HConfigs for non-equality
!
! !INTERFACE:
  function ESMF_HConfigNE(HConfig1, HConfig2)
!
! !RETURN VALUE:
    logical :: ESMF_HConfigNE

! !ARGUMENTS:
    type(ESMF_HConfig), intent(in) :: HConfig1
    type(ESMF_HConfig), intent(in) :: HConfig2

! !DESCRIPTION:
!   Test if both {\tt HConfig1} and {\tt HConfig2} alias the same
!   ESMF HConfig object.
!
!EOPI
!-------------------------------------------------------------------------------

    ESMF_HConfigNE = .not.ESMF_HConfigEQ(HConfig1, HConfig2)

  end function ESMF_HConfigNE
!-------------------------------------------------------------------------------


! -------------------------- ESMF-internal method -----------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigIterNE()"
!BOPI
! !IROUTINE:  ESMF_HConfigIterNE - Compare two HConfigIters for non-equality
!
! !INTERFACE:
  function ESMF_HConfigIterNE(HConfig1, HConfig2)
!
! !RETURN VALUE:
    logical :: ESMF_HConfigIterNE

! !ARGUMENTS:
    type(ESMF_HConfigIter), intent(in) :: HConfig1
    type(ESMF_HConfigIter), intent(in) :: HConfig2

! !DESCRIPTION:
!   Test if both {\tt HConfig1} and {\tt HConfig2} alias the same
!   ESMF HConfigIter object.
!
!EOPI
!-------------------------------------------------------------------------------

    ESMF_HConfigIterNE = .not.ESMF_HConfigIterEQ(HConfig1, HConfig2)

  end function ESMF_HConfigIterNE
!-------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
!BOP
! !IROUTINE: ESMF_HConfigAdd - Add <Type> content to HConfig object

! !INTERFACE:
!  subroutine ESMF_HConfigAdd(hconfig, content, keywordEnforcer, &
!    addKey, addKeyString, index, keyString, doc, rc)
!
! !ARGUMENTS:
!    type(ESMF_HConfig[Iter}), intent(in)      :: hconfig
!    <Type>,             intent(in)            :: content[(:)]
!type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
!    type(ESMF_HConfig), intent(in),  optional :: addKey
!    character(*),       intent(in),  optional :: addKeyString
!    integer,            intent(in),  optional :: index
!    character(*),       intent(in),  optional :: keyString
!    integer,            intent(in),  optional :: doc
!    integer,            intent(out), optional :: rc
!
! !DESCRIPTION:
!   Add the content of type <Type> to the {\tt hconfig},
!   at the current location, or as specified by {\tt index} or {\tt keyString}
!   (mutually exclusive!).
!   Most <Type> options support the sequence array variant {\tt (:)} in
!   addition to the scalar variant.
!
!   If either {\tt addKey} or {\tt addKeyString} (mutually exclusive!) is
!   specified, then add a new map element with the respective {\em key}.
!   Otherwise add a new list element at the end of the list. Error checking
!   is implemented to ensure respective conditions are met.
!
!   The supported <Type> options are:
!   \begin{itemize}
!   \item {\tt type(HConfig)} (scalar only variant!)
!   \item {\tt integer(ESMF\_KIND\_I4)}
!   \item {\tt integer(ESMF\_KIND\_I8)}
!   \item {\tt logical}
!   \item {\tt real(ESMF\_KIND\_R4)}
!   \item {\tt real(ESMF\_KIND\_R8)}
!   \item {\tt character(*)}
!   \end{itemize}
!
!   The arguments are:
!   \begin{description}
!   \item[hconfig]
!     {\tt ESMF\_HConfig} or {\tt ESMF\_HConfigIter} object.
!   \item[content]
!     The content to be added.
!   \item[{[addKey]}]
!     The key under which to add the new map item.
!     Mutural exclusive with {\tt addKeyString}.
!   \item[{[addKeyString]}]
!     The key string under which to add the new map item.
!     Mutural exclusive with {\tt addKey}.
!   \item[{[index]}]
!     Attempt to access by index if specified.
!     Mutural exclusive with {\tt keyString}.
!   \item[{[keyString]}]
!     Attempt to access by key string if specified.
!     Mutural exclusive with {\tt index}.
!   \item[{[doc]}]
!     The doc index. Defaults to the first document.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigAddHConfig()"

  subroutine ESMF_HConfigAddHConfig(hconfig, content, keywordEnforcer, &
    addKey, addKeyString, index, keyString, doc, rc)

    type(ESMF_HConfig), intent(in)            :: hconfig
    type(ESMF_HConfig), intent(in)            :: content
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_HConfig), intent(in),  optional :: addKey
    character(*),       intent(in),  optional :: addKeyString
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    integer,            intent(out), optional :: rc

    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hconfigTemp, hKey

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    if (present(addKey).or.present(addKeyString)) then
      if (present(addKeyString)) then
        hkey = ESMF_HConfigCreate(content=addKeyString, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      else
        ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, addKey, rc)
        hkey = addKey
      endif
    endif

    if (present(index).or.present(keyString).or.present(doc)) then
      hconfigTemp = ESMF_HConfigCreateAt(hconfig, index=index, &
        keyString=keyString, doc=doc, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      ! Call into the C++ interface to add content
      if (present(addKey).or.present(addKeyString)) then
        call c_ESMC_HConfigAddKey(hconfigTemp, content, hkey, localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      else
        call c_ESMC_HConfigAdd(hconfigTemp, content, localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      endif
      ! clean up
      call ESMF_HConfigDestroy(hconfigTemp, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    else
      ! Call into the C++ interface to add content
      if (present(addKey).or.present(addKeyString)) then
        call c_ESMC_HConfigAddKey(hconfig, content, hkey, localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      else
        call c_ESMC_HConfigAdd(hconfig, content, localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      endif
    endif

    if (present(addKeyString)) then
      call ESMF_HConfigDestroy(hkey, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigIterAddHConfig()"

  subroutine ESMF_HConfigIterAddHConfig(hconfig, content, keywordEnforcer, &
    addKey, addKeyString, index, keyString, doc, rc)

    type(ESMF_HConfigIter), intent(in)        :: hconfig
    type(ESMF_HConfig), intent(in)            :: content
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_HConfig), intent(in),  optional :: addKey
    character(*),       intent(in),  optional :: addKeyString
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    integer,            intent(out), optional :: rc

    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hconfigTemp

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    hconfigTemp = ESMF_HConfigIterAsHConfig(hconfig, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_HConfigAdd(hconfigTemp, content=content, addKey=addKey, &
      addKeyString=addKeyString, index=index, keyString=keyString, doc=doc, &
      rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigAddI4()"

  subroutine ESMF_HConfigAddI4(hconfig, content, keywordEnforcer, &
    addKey, addKeyString, index, keyString, doc, rc)

    type(ESMF_HConfig),     intent(in)            :: hconfig
    integer(ESMF_KIND_I4),  intent(in)            :: content
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_HConfig),     intent(in),  optional :: addKey
    character(*),           intent(in),  optional :: addKeyString
    integer,                intent(in),  optional :: index
    character(*),           intent(in),  optional :: keyString
    integer,                intent(in),  optional :: doc
    integer,                intent(out), optional :: rc

    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hcontent

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    hcontent = ESMF_HConfigCreate(content=content, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_HConfigAdd(hconfig, hcontent, &
      addKey=addKey, addKeyString=addKeyString, &
      index=index, keyString=keyString, doc=doc, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! clean up
    call ESMF_HConfigDestroy(hcontent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigIterAddI4()"

  subroutine ESMF_HConfigIterAddI4(hconfig, content, keywordEnforcer, &
    addKey, addKeyString, index, keyString, doc, rc)

    type(ESMF_HConfigIter), intent(in)        :: hconfig
    integer(ESMF_KIND_I4),  intent(in)        :: content
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_HConfig), intent(in),  optional :: addKey
    character(*),       intent(in),  optional :: addKeyString
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    integer,            intent(out), optional :: rc

    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hconfigTemp

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    hconfigTemp = ESMF_HConfigIterAsHConfig(hconfig, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_HConfigAdd(hconfigTemp, content=content, addKey=addKey, &
      addKeyString=addKeyString, index=index, keyString=keyString, doc=doc, &
      rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigAddI4Seq()"

  subroutine ESMF_HConfigAddI4Seq(hconfig, content, keywordEnforcer, &
    addKey, addKeyString, index, keyString, doc, rc)

    type(ESMF_HConfig),     intent(in)            :: hconfig
    integer(ESMF_KIND_I4),  intent(in)            :: content(:)
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_HConfig),     intent(in),  optional :: addKey
    character(*),           intent(in),  optional :: addKeyString
    integer,                intent(in),  optional :: index
    character(*),           intent(in),  optional :: keyString
    integer,                intent(in),  optional :: doc
    integer,                intent(out), optional :: rc

    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hcontent

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    hcontent = ESMF_HConfigCreate(content=content, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_HConfigAdd(hconfig, hcontent, &
      addKey=addKey, addKeyString=addKeyString, &
      index=index, keyString=keyString, doc=doc, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! clean up
    call ESMF_HConfigDestroy(hcontent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigIterAddI4Seq()"

  subroutine ESMF_HConfigIterAddI4Seq(hconfig, content, keywordEnforcer, &
    addKey, addKeyString, index, keyString, doc, rc)

    type(ESMF_HConfigIter), intent(in)        :: hconfig
    integer(ESMF_KIND_I4),  intent(in)        :: content(:)
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_HConfig), intent(in),  optional :: addKey
    character(*),       intent(in),  optional :: addKeyString
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    integer,            intent(out), optional :: rc

    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hconfigTemp

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    hconfigTemp = ESMF_HConfigIterAsHConfig(hconfig, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_HConfigAdd(hconfigTemp, content=content, addKey=addKey, &
      addKeyString=addKeyString, index=index, keyString=keyString, doc=doc, &
      rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigAddI8()"

  subroutine ESMF_HConfigAddI8(hconfig, content, keywordEnforcer, &
    addKey, addKeyString, index, keyString, doc, rc)

    type(ESMF_HConfig),     intent(in)            :: hconfig
    integer(ESMF_KIND_I8),  intent(in)            :: content
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_HConfig),     intent(in),  optional :: addKey
    character(*),           intent(in),  optional :: addKeyString
    integer,                intent(in),  optional :: index
    character(*),           intent(in),  optional :: keyString
    integer,                intent(in),  optional :: doc
    integer,                intent(out), optional :: rc

    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hcontent

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    hcontent = ESMF_HConfigCreate(content=content, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_HConfigAdd(hconfig, hcontent, &
      addKey=addKey, addKeyString=addKeyString, &
      index=index, keyString=keyString, doc=doc, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! clean up
    call ESMF_HConfigDestroy(hcontent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigIterAddI8()"

  subroutine ESMF_HConfigIterAddI8(hconfig, content, keywordEnforcer, &
    addKey, addKeyString, index, keyString, doc, rc)

    type(ESMF_HConfigIter), intent(in)        :: hconfig
    integer(ESMF_KIND_I8),  intent(in)        :: content
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_HConfig), intent(in),  optional :: addKey
    character(*),       intent(in),  optional :: addKeyString
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    integer,            intent(out), optional :: rc

    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hconfigTemp

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    hconfigTemp = ESMF_HConfigIterAsHConfig(hconfig, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_HConfigAdd(hconfigTemp, content=content, addKey=addKey, &
      addKeyString=addKeyString, index=index, keyString=keyString, doc=doc, &
      rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigAddI8Seq()"

  subroutine ESMF_HConfigAddI8Seq(hconfig, content, keywordEnforcer, &
    addKey, addKeyString, index, keyString, doc, rc)

    type(ESMF_HConfig),     intent(in)            :: hconfig
    integer(ESMF_KIND_I8),  intent(in)            :: content(:)
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_HConfig),     intent(in),  optional :: addKey
    character(*),           intent(in),  optional :: addKeyString
    integer,                intent(in),  optional :: index
    character(*),           intent(in),  optional :: keyString
    integer,                intent(in),  optional :: doc
    integer,                intent(out), optional :: rc

    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hcontent

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    hcontent = ESMF_HConfigCreate(content=content, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_HConfigAdd(hconfig, hcontent, &
      addKey=addKey, addKeyString=addKeyString, &
      index=index, keyString=keyString, doc=doc, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! clean up
    call ESMF_HConfigDestroy(hcontent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigIterAddI8Seq()"

  subroutine ESMF_HConfigIterAddI8Seq(hconfig, content, keywordEnforcer, &
    addKey, addKeyString, index, keyString, doc, rc)

    type(ESMF_HConfigIter), intent(in)        :: hconfig
    integer(ESMF_KIND_I8),  intent(in)        :: content(:)
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_HConfig), intent(in),  optional :: addKey
    character(*),       intent(in),  optional :: addKeyString
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    integer,            intent(out), optional :: rc

    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hconfigTemp

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    hconfigTemp = ESMF_HConfigIterAsHConfig(hconfig, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_HConfigAdd(hconfigTemp, content=content, addKey=addKey, &
      addKeyString=addKeyString, index=index, keyString=keyString, doc=doc, &
      rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigAddLogical()"

  subroutine ESMF_HConfigAddLogical(hconfig, content, keywordEnforcer, &
    addKey, addKeyString, index, keyString, doc, rc)

    type(ESMF_HConfig), intent(in)            :: hconfig
    logical,            intent(in)            :: content
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_HConfig), intent(in),  optional :: addKey
    character(*),       intent(in),  optional :: addKeyString
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    integer,            intent(out), optional :: rc

    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hcontent

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    hcontent = ESMF_HConfigCreate(content=content, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_HConfigAdd(hconfig, hcontent, &
      addKey=addKey, addKeyString=addKeyString, &
      index=index, keyString=keyString, doc=doc, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! clean up
    call ESMF_HConfigDestroy(hcontent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigIterAddLogical()"

  subroutine ESMF_HConfigIterAddLogical(hconfig, content, keywordEnforcer, &
    addKey, addKeyString, index, keyString, doc, rc)

    type(ESMF_HConfigIter), intent(in)        :: hconfig
    logical,            intent(in)            :: content
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_HConfig), intent(in),  optional :: addKey
    character(*),       intent(in),  optional :: addKeyString
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    integer,            intent(out), optional :: rc

    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hconfigTemp

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    hconfigTemp = ESMF_HConfigIterAsHConfig(hconfig, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_HConfigAdd(hconfigTemp, content=content, addKey=addKey, &
      addKeyString=addKeyString, index=index, keyString=keyString, doc=doc, &
      rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigAddLogicalSeq()"

  subroutine ESMF_HConfigAddLogicalSeq(hconfig, content, keywordEnforcer, &
    addKey, addKeyString, index, keyString, doc, rc)

    type(ESMF_HConfig),     intent(in)            :: hconfig
    logical,                intent(in)            :: content(:)
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_HConfig),     intent(in),  optional :: addKey
    character(*),           intent(in),  optional :: addKeyString
    integer,                intent(in),  optional :: index
    character(*),           intent(in),  optional :: keyString
    integer,                intent(in),  optional :: doc
    integer,                intent(out), optional :: rc

    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hcontent

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    hcontent = ESMF_HConfigCreate(content=content, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_HConfigAdd(hconfig, hcontent, &
      addKey=addKey, addKeyString=addKeyString, &
      index=index, keyString=keyString, doc=doc, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! clean up
    call ESMF_HConfigDestroy(hcontent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigIterAddLogicalSeq()"

  subroutine ESMF_HConfigIterAddLogicalSeq(hconfig, content, keywordEnforcer, &
    addKey, addKeyString, index, keyString, doc, rc)

    type(ESMF_HConfigIter), intent(in)        :: hconfig
    logical,                intent(in)        :: content(:)
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_HConfig), intent(in),  optional :: addKey
    character(*),       intent(in),  optional :: addKeyString
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    integer,            intent(out), optional :: rc

    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hconfigTemp

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    hconfigTemp = ESMF_HConfigIterAsHConfig(hconfig, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_HConfigAdd(hconfigTemp, content=content, addKey=addKey, &
      addKeyString=addKeyString, index=index, keyString=keyString, doc=doc, &
      rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigAddR4()"

  subroutine ESMF_HConfigAddR4(hconfig, content, keywordEnforcer, &
    addKey, addKeyString, index, keyString, doc, rc)

    type(ESMF_HConfig), intent(in)            :: hconfig
    real(ESMF_KIND_R4), intent(in)            :: content
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_HConfig), intent(in),  optional :: addKey
    character(*),       intent(in),  optional :: addKeyString
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    integer,            intent(out), optional :: rc

    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hcontent

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    hcontent = ESMF_HConfigCreate(content=content, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_HConfigAdd(hconfig, hcontent, &
      addKey=addKey, addKeyString=addKeyString, &
      index=index, keyString=keyString, doc=doc, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! clean up
    call ESMF_HConfigDestroy(hcontent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigIterAddR4()"

  subroutine ESMF_HConfigIterAddR4(hconfig, content, keywordEnforcer, &
    addKey, addKeyString, index, keyString, doc, rc)

    type(ESMF_HConfigIter), intent(in)        :: hconfig
    real(ESMF_KIND_R4), intent(in)            :: content
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_HConfig), intent(in),  optional :: addKey
    character(*),       intent(in),  optional :: addKeyString
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    integer,            intent(out), optional :: rc

    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hconfigTemp

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    hconfigTemp = ESMF_HConfigIterAsHConfig(hconfig, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_HConfigAdd(hconfigTemp, content=content, addKey=addKey, &
      addKeyString=addKeyString, index=index, keyString=keyString, doc=doc, &
      rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigAddR4Seq()"

  subroutine ESMF_HConfigAddR4Seq(hconfig, content, keywordEnforcer, &
    addKey, addKeyString, index, keyString, doc, rc)

    type(ESMF_HConfig), intent(in)            :: hconfig
    real(ESMF_KIND_R4), intent(in)            :: content(:)
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_HConfig), intent(in),  optional :: addKey
    character(*),       intent(in),  optional :: addKeyString
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    integer,            intent(out), optional :: rc

    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hcontent

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    hcontent = ESMF_HConfigCreate(content=content, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_HConfigAdd(hconfig, hcontent, &
      addKey=addKey, addKeyString=addKeyString, &
      index=index, keyString=keyString, doc=doc, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! clean up
    call ESMF_HConfigDestroy(hcontent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigIterAddR4Seq()"

  subroutine ESMF_HConfigIterAddR4Seq(hconfig, content, keywordEnforcer, &
    addKey, addKeyString, index, keyString, doc, rc)

    type(ESMF_HConfigIter), intent(in)        :: hconfig
    real(ESMF_KIND_R4), intent(in)            :: content(:)
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_HConfig), intent(in),  optional :: addKey
    character(*),       intent(in),  optional :: addKeyString
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    integer,            intent(out), optional :: rc

    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hconfigTemp

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    hconfigTemp = ESMF_HConfigIterAsHConfig(hconfig, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_HConfigAdd(hconfigTemp, content=content, addKey=addKey, &
      addKeyString=addKeyString, index=index, keyString=keyString, doc=doc, &
      rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigAddR8()"

  subroutine ESMF_HConfigAddR8(hconfig, content, keywordEnforcer, &
    addKey, addKeyString, index, keyString, doc, rc)

    type(ESMF_HConfig), intent(in)            :: hconfig
    real(ESMF_KIND_R8), intent(in)            :: content
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_HConfig), intent(in),  optional :: addKey
    character(*),       intent(in),  optional :: addKeyString
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    integer,            intent(out), optional :: rc

    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hcontent

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    hcontent = ESMF_HConfigCreate(content=content, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_HConfigAdd(hconfig, hcontent, &
      addKey=addKey, addKeyString=addKeyString, &
      index=index, keyString=keyString, doc=doc, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! clean up
    call ESMF_HConfigDestroy(hcontent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigIterAddR8()"

  subroutine ESMF_HConfigIterAddR8(hconfig, content, keywordEnforcer, &
    addKey, addKeyString, index, keyString, doc, rc)

    type(ESMF_HConfigIter), intent(in)        :: hconfig
    real(ESMF_KIND_R8), intent(in)            :: content
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_HConfig), intent(in),  optional :: addKey
    character(*),       intent(in),  optional :: addKeyString
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    integer,            intent(out), optional :: rc

    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hconfigTemp

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    hconfigTemp = ESMF_HConfigIterAsHConfig(hconfig, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_HConfigAdd(hconfigTemp, content=content, addKey=addKey, &
      addKeyString=addKeyString, index=index, keyString=keyString, doc=doc, &
      rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigAddR8Seq()"

  subroutine ESMF_HConfigAddR8Seq(hconfig, content, keywordEnforcer, &
    addKey, addKeyString, index, keyString, doc, rc)

    type(ESMF_HConfig), intent(in)            :: hconfig
    real(ESMF_KIND_R8), intent(in)            :: content(:)
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_HConfig), intent(in),  optional :: addKey
    character(*),       intent(in),  optional :: addKeyString
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    integer,            intent(out), optional :: rc

    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hcontent

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    hcontent = ESMF_HConfigCreate(content=content, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_HConfigAdd(hconfig, hcontent, &
      addKey=addKey, addKeyString=addKeyString, &
      index=index, keyString=keyString, doc=doc, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! clean up
    call ESMF_HConfigDestroy(hcontent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigIterAddR8Seq()"

  subroutine ESMF_HConfigIterAddR8Seq(hconfig, content, keywordEnforcer, &
    addKey, addKeyString, index, keyString, doc, rc)

    type(ESMF_HConfigIter), intent(in)        :: hconfig
    real(ESMF_KIND_R8), intent(in)            :: content(:)
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_HConfig), intent(in),  optional :: addKey
    character(*),       intent(in),  optional :: addKeyString
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    integer,            intent(out), optional :: rc

    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hconfigTemp

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    hconfigTemp = ESMF_HConfigIterAsHConfig(hconfig, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_HConfigAdd(hconfigTemp, content=content, addKey=addKey, &
      addKeyString=addKeyString, index=index, keyString=keyString, doc=doc, &
      rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigAddString()"

  subroutine ESMF_HConfigAddString(hconfig, content, keywordEnforcer, &
    addKey, addKeyString, index, keyString, doc, rc)

    type(ESMF_HConfig), intent(in)            :: hconfig
    character(*),       intent(in)            :: content
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_HConfig), intent(in),  optional :: addKey
    character(*),       intent(in),  optional :: addKeyString
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    integer,            intent(out), optional :: rc

    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hcontent

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    hcontent = ESMF_HConfigCreate(content=content, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_HConfigAdd(hconfig, hcontent, &
      addKey=addKey, addKeyString=addKeyString, &
      index=index, keyString=keyString, doc=doc, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! clean up
    call ESMF_HConfigDestroy(hcontent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigIterAddString()"

  subroutine ESMF_HConfigIterAddString(hconfig, content, keywordEnforcer, &
    addKey, addKeyString, index, keyString, doc, rc)

    type(ESMF_HConfigIter), intent(in)        :: hconfig
    character(*),       intent(in)            :: content
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_HConfig), intent(in),  optional :: addKey
    character(*),       intent(in),  optional :: addKeyString
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    integer,            intent(out), optional :: rc

    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hconfigTemp

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    hconfigTemp = ESMF_HConfigIterAsHConfig(hconfig, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_HConfigAdd(hconfigTemp, content=content, addKey=addKey, &
      addKeyString=addKeyString, index=index, keyString=keyString, doc=doc, &
      rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigAddStringSeq()"

  subroutine ESMF_HConfigAddStringSeq(hconfig, content, keywordEnforcer, &
    addKey, addKeyString, index, keyString, doc, rc)

    type(ESMF_HConfig),     intent(in)            :: hconfig
    character(len=*),       intent(in)            :: content(:)
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_HConfig),     intent(in),  optional :: addKey
    character(*),           intent(in),  optional :: addKeyString
    integer,                intent(in),  optional :: index
    character(*),           intent(in),  optional :: keyString
    integer,                intent(in),  optional :: doc
    integer,                intent(out), optional :: rc

    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hcontent

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    hcontent = ESMF_HConfigCreate(content=content, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_HConfigAdd(hconfig, hcontent, &
      addKey=addKey, addKeyString=addKeyString, &
      index=index, keyString=keyString, doc=doc, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! clean up
    call ESMF_HConfigDestroy(hcontent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigIterAddStringSeq()"

  subroutine ESMF_HConfigIterAddStringSeq(hconfig, content, keywordEnforcer, &
    addKey, addKeyString, index, keyString, doc, rc)

    type(ESMF_HConfigIter), intent(in)        :: hconfig
    character(len=*),       intent(in)        :: content(:)
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_HConfig), intent(in),  optional :: addKey
    character(*),       intent(in),  optional :: addKeyString
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    integer,            intent(out), optional :: rc

    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hconfigTemp

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    hconfigTemp = ESMF_HConfigIterAsHConfig(hconfig, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_HConfigAdd(hconfigTemp, content=content, addKey=addKey, &
      addKeyString=addKeyString, index=index, keyString=keyString, doc=doc, &
      rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
!BOP
! !IROUTINE: ESMF_HConfigAddMapKey - Add <Type> content to HConfig MapKey object

! !INTERFACE:
!  subroutine ESMF_HConfigAdd(hconfig, content, keywordEnforcer, &
!    addKey, addKeyString, index, keyString, doc, rc)
!
! !ARGUMENTS:
!    type(ESMF_HConfigIter), intent(in)        :: hconfig
!    <Type>,             intent(in)            :: content[(:)]
!type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
!    type(ESMF_HConfig), intent(in),  optional :: addKey
!    character(*),       intent(in),  optional :: addKeyString
!    integer,            intent(in),  optional :: index
!    character(*),       intent(in),  optional :: keyString
!    integer,            intent(in),  optional :: doc
!    integer,            intent(out), optional :: rc
!
! !DESCRIPTION:
!   Add the content of type <Type> to the {\tt hconfig} map key,
!   at the current location, or as specified by {\tt index} or {\tt keyString}
!   (mutually exclusive!).
!   Most <Type> options support the sequence array variant {\tt (:)} in
!   addition to the scalar variant.
!
!   If either {\tt addKey} or {\tt addKeyString} (mutually exclusive!) is
!   specified, then add a new map element with the respective {\em key}.
!   Otherwise add a new list element at the end of the list. Error checking
!   is implemented to ensure respective conditions are met.
!
!   The supported <Type> options are:
!   \begin{itemize}
!   \item {\tt type(HConfig)} (scalar only variant!)
!   \item {\tt integer(ESMF\_KIND\_I4)}
!   \item {\tt integer(ESMF\_KIND\_I8)}
!   \item {\tt logical}
!   \item {\tt real(ESMF\_KIND\_R4)}
!   \item {\tt real(ESMF\_KIND\_R8)}
!   \item {\tt character(*)}
!   \end{itemize}
!
!   The arguments are:
!   \begin{description}
!   \item[hconfig]
!     {\tt ESMF\_HConfigIter} object.
!   \item[content]
!     The content to be added.
!   \item[{[addKey]}]
!     The key under which to add the new map item.
!     Mutural exclusive with {\tt addKeyString}.
!   \item[{[addKeyString]}]
!     The key string under which to add the new map item.
!     Mutural exclusive with {\tt addKey}.
!   \item[{[index]}]
!     Attempt to access by index if specified.
!     Mutural exclusive with {\tt keyString}.
!   \item[{[keyString]}]
!     Attempt to access by key string if specified.
!     Mutural exclusive with {\tt index}.
!   \item[{[doc]}]
!     The doc index. Defaults to the first document.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigAddMapKeyHConfig()"

  subroutine ESMF_HConfigAddMapKeyHConfig(hconfig, content, keywordEnforcer, &
    addKey, addKeyString, index, keyString, doc, rc)

    type(ESMF_HConfigIter), intent(in)        :: hconfig
    type(ESMF_HConfig), intent(in)            :: content
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_HConfig), intent(in),  optional :: addKey
    character(*),       intent(in),  optional :: addKeyString
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    integer,            intent(out), optional :: rc

    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hconfigTemp, hKey

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigIterGetInit, hconfig, rc)

    if (present(addKey).or.present(addKeyString)) then
      if (present(addKeyString)) then
        hkey = ESMF_HConfigCreate(content=addKeyString, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      else
        ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, addKey, rc)
        hkey = addKey
      endif
    endif

    if (present(index).or.present(keyString).or.present(doc)) then
      hconfigTemp = ESMF_HConfigCreateAtMapKey(hconfig, index=index, &
        keyString=keyString, doc=doc, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      ! Call into the C++ interface to add content
      if (present(addKey).or.present(addKeyString)) then
        call c_ESMC_HConfigAddKey(hconfigTemp, content, hkey, localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      else
        call c_ESMC_HConfigAdd(hconfigTemp, content, localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      endif
      ! clean up
      call ESMF_HConfigDestroy(hconfigTemp, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    else
      ! Call into the C++ interface to add content
      if (present(addKey).or.present(addKeyString)) then
        call c_ESMC_HConfigAddKeyMapKey(hconfig, content, hkey, localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      else
        call c_ESMC_HConfigAddMapKey(hconfig, content, localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      endif
    endif

    if (present(addKeyString)) then
      call ESMF_HConfigDestroy(hkey, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigAddMapKeyI4()"

  subroutine ESMF_HConfigAddMapKeyI4(hconfig, content, keywordEnforcer, &
    addKey, addKeyString, index, keyString, rc)

    type(ESMF_HConfigIter),     intent(in)            :: hconfig
    integer(ESMF_KIND_I4),  intent(in)            :: content
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_HConfig),     intent(in),  optional :: addKey
    character(*),           intent(in),  optional :: addKeyString
    integer,                intent(in),  optional :: index
    character(*),           intent(in),  optional :: keyString
    integer,                intent(out), optional :: rc

    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hcontent

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigIterGetInit, hconfig, rc)

    hcontent = ESMF_HConfigCreate(content=content, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_HConfigAddMapKey(hconfig, hcontent, &
      addKey=addKey, addKeyString=addKeyString, &
      index=index, keyString=keyString, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! clean up
    call ESMF_HConfigDestroy(hcontent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigAddMapKeyI4Seq()"

  subroutine ESMF_HConfigAddMapKeyI4Seq(hconfig, content, keywordEnforcer, &
    addKey, addKeyString, index, keyString, rc)

    type(ESMF_HConfigIter), intent(in)            :: hconfig
    integer(ESMF_KIND_I4),  intent(in)            :: content(:)
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_HConfig),     intent(in),  optional :: addKey
    character(*),           intent(in),  optional :: addKeyString
    integer,                intent(in),  optional :: index
    character(*),           intent(in),  optional :: keyString
    integer,                intent(out), optional :: rc

    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hcontent

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigIterGetInit, hconfig, rc)

    hcontent = ESMF_HConfigCreate(content=content, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_HConfigAddMapKey(hconfig, hcontent, &
      addKey=addKey, addKeyString=addKeyString, &
      index=index, keyString=keyString, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! clean up
    call ESMF_HConfigDestroy(hcontent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigAddMapKeyI8()"

  subroutine ESMF_HConfigAddMapKeyI8(hconfig, content, keywordEnforcer, &
    addKey, addKeyString, index, keyString, rc)

    type(ESMF_HConfigIter),     intent(in)            :: hconfig
    integer(ESMF_KIND_I8),  intent(in)            :: content
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_HConfig),     intent(in),  optional :: addKey
    character(*),           intent(in),  optional :: addKeyString
    integer,                intent(in),  optional :: index
    character(*),           intent(in),  optional :: keyString
    integer,                intent(out), optional :: rc

    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hcontent

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigIterGetInit, hconfig, rc)

    hcontent = ESMF_HConfigCreate(content=content, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_HConfigAddMapKey(hconfig, hcontent, &
      addKey=addKey, addKeyString=addKeyString, &
      index=index, keyString=keyString, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! clean up
    call ESMF_HConfigDestroy(hcontent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigAddMapKeyI8Seq()"

  subroutine ESMF_HConfigAddMapKeyI8Seq(hconfig, content, keywordEnforcer, &
    addKey, addKeyString, index, keyString, rc)

    type(ESMF_HConfigIter), intent(in)            :: hconfig
    integer(ESMF_KIND_I8),  intent(in)            :: content(:)
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_HConfig),     intent(in),  optional :: addKey
    character(*),           intent(in),  optional :: addKeyString
    integer,                intent(in),  optional :: index
    character(*),           intent(in),  optional :: keyString
    integer,                intent(out), optional :: rc

    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hcontent

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigIterGetInit, hconfig, rc)

    hcontent = ESMF_HConfigCreate(content=content, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_HConfigAddMapKey(hconfig, hcontent, &
      addKey=addKey, addKeyString=addKeyString, &
      index=index, keyString=keyString, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! clean up
    call ESMF_HConfigDestroy(hcontent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigAddMapKeyLogical()"

  subroutine ESMF_HConfigAddMapKeyLogical(hconfig, content, keywordEnforcer, &
    addKey, addKeyString, index, keyString, rc)

    type(ESMF_HConfigIter), intent(in)        :: hconfig
    logical,            intent(in)            :: content
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_HConfig), intent(in),  optional :: addKey
    character(*),       intent(in),  optional :: addKeyString
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(out), optional :: rc

    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hcontent

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigIterGetInit, hconfig, rc)

    hcontent = ESMF_HConfigCreate(content=content, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_HConfigAddMapKey(hconfig, hcontent, &
      addKey=addKey, addKeyString=addKeyString, &
      index=index, keyString=keyString, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! clean up
    call ESMF_HConfigDestroy(hcontent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigAddMapKeyLogicalSeq()"

  subroutine ESMF_HConfigAddMapKeyLogicalSeq(hconfig, content, keywordEnforcer, &
    addKey, addKeyString, index, keyString, rc)

    type(ESMF_HConfigIter), intent(in)        :: hconfig
    logical,            intent(in)            :: content(:)
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_HConfig), intent(in),  optional :: addKey
    character(*),       intent(in),  optional :: addKeyString
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(out), optional :: rc

    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hcontent

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigIterGetInit, hconfig, rc)

    hcontent = ESMF_HConfigCreate(content=content, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_HConfigAddMapKey(hconfig, hcontent, &
      addKey=addKey, addKeyString=addKeyString, &
      index=index, keyString=keyString, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! clean up
    call ESMF_HConfigDestroy(hcontent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigAddMapKeyR4()"

  subroutine ESMF_HConfigAddMapKeyR4(hconfig, content, keywordEnforcer, &
    addKey, addKeyString, index, keyString, rc)

    type(ESMF_HConfigIter), intent(in)        :: hconfig
    real(ESMF_KIND_R4), intent(in)            :: content
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_HConfig), intent(in),  optional :: addKey
    character(*),       intent(in),  optional :: addKeyString
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(out), optional :: rc

    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hcontent

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigIterGetInit, hconfig, rc)

    hcontent = ESMF_HConfigCreate(content=content, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_HConfigAddMapKey(hconfig, hcontent, &
      addKey=addKey, addKeyString=addKeyString, &
      index=index, keyString=keyString, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! clean up
    call ESMF_HConfigDestroy(hcontent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigAddMapKeyR4Seq()"

  subroutine ESMF_HConfigAddMapKeyR4Seq(hconfig, content, keywordEnforcer, &
    addKey, addKeyString, index, keyString, rc)

    type(ESMF_HConfigIter), intent(in)        :: hconfig
    real(ESMF_KIND_R4), intent(in)            :: content(:)
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_HConfig), intent(in),  optional :: addKey
    character(*),       intent(in),  optional :: addKeyString
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(out), optional :: rc

    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hcontent

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigIterGetInit, hconfig, rc)

    hcontent = ESMF_HConfigCreate(content=content, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_HConfigAddMapKey(hconfig, hcontent, &
      addKey=addKey, addKeyString=addKeyString, &
      index=index, keyString=keyString, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! clean up
    call ESMF_HConfigDestroy(hcontent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigAddMapKeyR8()"

  subroutine ESMF_HConfigAddMapKeyR8(hconfig, content, keywordEnforcer, &
    addKey, addKeyString, index, keyString, rc)

    type(ESMF_HConfigIter), intent(in)        :: hconfig
    real(ESMF_KIND_R8), intent(in)            :: content
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_HConfig), intent(in),  optional :: addKey
    character(*),       intent(in),  optional :: addKeyString
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(out), optional :: rc

    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hcontent

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigIterGetInit, hconfig, rc)

    hcontent = ESMF_HConfigCreate(content=content, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_HConfigAddMapKey(hconfig, hcontent, &
      addKey=addKey, addKeyString=addKeyString, &
      index=index, keyString=keyString, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! clean up
    call ESMF_HConfigDestroy(hcontent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigAddMapKeyR8Seq()"

  subroutine ESMF_HConfigAddMapKeyR8Seq(hconfig, content, keywordEnforcer, &
    addKey, addKeyString, index, keyString, rc)

    type(ESMF_HConfigIter), intent(in)        :: hconfig
    real(ESMF_KIND_R8), intent(in)            :: content(:)
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_HConfig), intent(in),  optional :: addKey
    character(*),       intent(in),  optional :: addKeyString
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(out), optional :: rc

    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hcontent

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigIterGetInit, hconfig, rc)

    hcontent = ESMF_HConfigCreate(content=content, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_HConfigAddMapKey(hconfig, hcontent, &
      addKey=addKey, addKeyString=addKeyString, &
      index=index, keyString=keyString, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! clean up
    call ESMF_HConfigDestroy(hcontent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigAddMapKeyString()"

  subroutine ESMF_HConfigAddMapKeyString(hconfig, content, keywordEnforcer, &
    addKey, addKeyString, index, keyString, rc)

    type(ESMF_HConfigIter), intent(in)        :: hconfig
    character(*),       intent(in)            :: content
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_HConfig), intent(in),  optional :: addKey
    character(*),       intent(in),  optional :: addKeyString
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(out), optional :: rc

    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hcontent

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigIterGetInit, hconfig, rc)

    hcontent = ESMF_HConfigCreate(content=content, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_HConfigAddMapKey(hconfig, hcontent, &
      addKey=addKey, addKeyString=addKeyString, &
      index=index, keyString=keyString, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! clean up
    call ESMF_HConfigDestroy(hcontent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigAddMapKeyStringSeq()"

  subroutine ESMF_HConfigAddMapKeyStringSeq(hconfig, content, keywordEnforcer, &
    addKey, addKeyString, index, keyString, rc)

    type(ESMF_HConfigIter), intent(in)        :: hconfig
    character(*),       intent(in)            :: content(:)
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_HConfig), intent(in),  optional :: addKey
    character(*),       intent(in),  optional :: addKeyString
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(out), optional :: rc

    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hcontent

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigIterGetInit, hconfig, rc)

    hcontent = ESMF_HConfigCreate(content=content, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_HConfigAddMapKey(hconfig, hcontent, &
      addKey=addKey, addKeyString=addKeyString, &
      index=index, keyString=keyString, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! clean up
    call ESMF_HConfigDestroy(hcontent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
!BOP
! !IROUTINE: ESMF_HConfigAddMapVal - Add <Type> content to HConfig MapVal object

! !INTERFACE:
!  subroutine ESMF_HConfigAdd(hconfig, content, keywordEnforcer, &
!    addKey, addKeyString, index, keyString, doc, rc)
!
! !ARGUMENTS:
!    type(ESMF_HConfigIter), intent(in)        :: hconfig
!    <Type>,             intent(in)            :: content[(:)]
!type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
!    type(ESMF_HConfig), intent(in),  optional :: addKey
!    character(*),       intent(in),  optional :: addKeyString
!    integer,            intent(in),  optional :: index
!    character(*),       intent(in),  optional :: keyString
!    integer,            intent(in),  optional :: doc
!    integer,            intent(out), optional :: rc
!
! !DESCRIPTION:
!   Add the content of type <Type> to the {\tt hconfig} map value,
!   at the current location, or as specified by {\tt index} or {\tt keyString}
!   (mutually exclusive!).
!   Most <Type> options support the sequence array variant {\tt (:)} in
!   addition to the scalar variant.
!
!   If either {\tt addKey} or {\tt addKeyString} (mutually exclusive!) is
!   specified, then add a new map element with the respective {\em key}.
!   Otherwise add a new list element at the end of the list. Error checking
!   is implemented to ensure respective conditions are met.
!
!   The supported <Type> options are:
!   \begin{itemize}
!   \item {\tt type(HConfig)} (scalar only variant!)
!   \item {\tt integer(ESMF\_KIND\_I4)}
!   \item {\tt integer(ESMF\_KIND\_I8)}
!   \item {\tt logical}
!   \item {\tt real(ESMF\_KIND\_R4)}
!   \item {\tt real(ESMF\_KIND\_R8)}
!   \item {\tt character(*)}
!   \end{itemize}
!
!   The arguments are:
!   \begin{description}
!   \item[hconfig]
!     {\tt ESMF\_HConfigIter} object.
!   \item[content]
!     The content to be added.
!   \item[{[addKey]}]
!     The key under which to add the new map item.
!     Mutural exclusive with {\tt addKeyString}.
!   \item[{[addKeyString]}]
!     The key string under which to add the new map item.
!     Mutural exclusive with {\tt addKey}.
!   \item[{[index]}]
!     Attempt to access by index if specified.
!     Mutural exclusive with {\tt keyString}.
!   \item[{[keyString]}]
!     Attempt to access by key string if specified.
!     Mutural exclusive with {\tt index}.
!   \item[{[doc]}]
!     The doc index. Defaults to the first document.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigAddMapValHConfig()"

  subroutine ESMF_HConfigAddMapValHConfig(hconfig, content, keywordEnforcer, &
    addKey, addKeyString, index, keyString, doc, rc)

    type(ESMF_HConfigIter), intent(in)        :: hconfig
    type(ESMF_HConfig), intent(in)            :: content
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_HConfig), intent(in),  optional :: addKey
    character(*),       intent(in),  optional :: addKeyString
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    integer,            intent(out), optional :: rc

    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hconfigTemp, hKey

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigIterGetInit, hconfig, rc)

    if (present(addKey).or.present(addKeyString)) then
      if (present(addKeyString)) then
        hkey = ESMF_HConfigCreate(content=addKeyString, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      else
        ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, addKey, rc)
        hkey = addKey
      endif
    endif

    if (present(index).or.present(keyString).or.present(doc)) then
      hconfigTemp = ESMF_HConfigCreateAtMapVal(hconfig, index=index, &
        keyString=keyString, doc=doc, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      ! Call into the C++ interface to add content
      if (present(addKey).or.present(addKeyString)) then
        call c_ESMC_HConfigAddKey(hconfigTemp, content, hkey, localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      else
        call c_ESMC_HConfigAdd(hconfigTemp, content, localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      endif
      ! clean up
      call ESMF_HConfigDestroy(hconfigTemp, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    else
      ! Call into the C++ interface to add content
      if (present(addKey).or.present(addKeyString)) then
        call c_ESMC_HConfigAddKeyMapVal(hconfig, content, hkey, localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      else
        call c_ESMC_HConfigAddMapVal(hconfig, content, localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      endif
    endif

    if (present(addKeyString)) then
      call ESMF_HConfigDestroy(hkey, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigAddMapValI4()"

  subroutine ESMF_HConfigAddMapValI4(hconfig, content, keywordEnforcer, &
    addKey, addKeyString, index, keyString, rc)

    type(ESMF_HConfigIter),     intent(in)            :: hconfig
    integer(ESMF_KIND_I4),  intent(in)            :: content
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_HConfig),     intent(in),  optional :: addKey
    character(*),           intent(in),  optional :: addKeyString
    integer,                intent(in),  optional :: index
    character(*),           intent(in),  optional :: keyString
    integer,                intent(out), optional :: rc

    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hcontent

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigIterGetInit, hconfig, rc)

    hcontent = ESMF_HConfigCreate(content=content, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_HConfigAddMapVal(hconfig, hcontent, &
      addKey=addKey, addKeyString=addKeyString, &
      index=index, keyString=keyString, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! clean up
    call ESMF_HConfigDestroy(hcontent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigAddMapValI4Seq()"

  subroutine ESMF_HConfigAddMapValI4Seq(hconfig, content, keywordEnforcer, &
    addKey, addKeyString, index, keyString, rc)

    type(ESMF_HConfigIter), intent(in)            :: hconfig
    integer(ESMF_KIND_I4),  intent(in)            :: content(:)
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_HConfig),     intent(in),  optional :: addKey
    character(*),           intent(in),  optional :: addKeyString
    integer,                intent(in),  optional :: index
    character(*),           intent(in),  optional :: keyString
    integer,                intent(out), optional :: rc

    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hcontent

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigIterGetInit, hconfig, rc)

    hcontent = ESMF_HConfigCreate(content=content, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_HConfigAddMapVal(hconfig, hcontent, &
      addKey=addKey, addKeyString=addKeyString, &
      index=index, keyString=keyString, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! clean up
    call ESMF_HConfigDestroy(hcontent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigAddMapValI8()"

  subroutine ESMF_HConfigAddMapValI8(hconfig, content, keywordEnforcer, &
    addKey, addKeyString, index, keyString, rc)

    type(ESMF_HConfigIter),     intent(in)            :: hconfig
    integer(ESMF_KIND_I8),  intent(in)            :: content
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_HConfig),     intent(in),  optional :: addKey
    character(*),           intent(in),  optional :: addKeyString
    integer,                intent(in),  optional :: index
    character(*),           intent(in),  optional :: keyString
    integer,                intent(out), optional :: rc

    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hcontent

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigIterGetInit, hconfig, rc)

    hcontent = ESMF_HConfigCreate(content=content, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_HConfigAddMapVal(hconfig, hcontent, &
      addKey=addKey, addKeyString=addKeyString, &
      index=index, keyString=keyString, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! clean up
    call ESMF_HConfigDestroy(hcontent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigAddMapValI8Seq()"

  subroutine ESMF_HConfigAddMapValI8Seq(hconfig, content, keywordEnforcer, &
    addKey, addKeyString, index, keyString, rc)

    type(ESMF_HConfigIter), intent(in)            :: hconfig
    integer(ESMF_KIND_I8),  intent(in)            :: content(:)
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_HConfig),     intent(in),  optional :: addKey
    character(*),           intent(in),  optional :: addKeyString
    integer,                intent(in),  optional :: index
    character(*),           intent(in),  optional :: keyString
    integer,                intent(out), optional :: rc

    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hcontent

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigIterGetInit, hconfig, rc)

    hcontent = ESMF_HConfigCreate(content=content, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_HConfigAddMapVal(hconfig, hcontent, &
      addKey=addKey, addKeyString=addKeyString, &
      index=index, keyString=keyString, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! clean up
    call ESMF_HConfigDestroy(hcontent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigAddMapValLogical()"

  subroutine ESMF_HConfigAddMapValLogical(hconfig, content, keywordEnforcer, &
    addKey, addKeyString, index, keyString, rc)

    type(ESMF_HConfigIter), intent(in)        :: hconfig
    logical,            intent(in)            :: content
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_HConfig), intent(in),  optional :: addKey
    character(*),       intent(in),  optional :: addKeyString
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(out), optional :: rc

    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hcontent

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigIterGetInit, hconfig, rc)

    hcontent = ESMF_HConfigCreate(content=content, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_HConfigAddMapVal(hconfig, hcontent, &
      addKey=addKey, addKeyString=addKeyString, &
      index=index, keyString=keyString, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! clean up
    call ESMF_HConfigDestroy(hcontent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigAddMapValLogicalSeq()"

  subroutine ESMF_HConfigAddMapValLogicalSeq(hconfig, content, keywordEnforcer, &
    addKey, addKeyString, index, keyString, rc)

    type(ESMF_HConfigIter), intent(in)        :: hconfig
    logical,            intent(in)            :: content(:)
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_HConfig), intent(in),  optional :: addKey
    character(*),       intent(in),  optional :: addKeyString
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(out), optional :: rc

    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hcontent

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigIterGetInit, hconfig, rc)

    hcontent = ESMF_HConfigCreate(content=content, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_HConfigAddMapVal(hconfig, hcontent, &
      addKey=addKey, addKeyString=addKeyString, &
      index=index, keyString=keyString, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! clean up
    call ESMF_HConfigDestroy(hcontent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigAddMapValR4()"

  subroutine ESMF_HConfigAddMapValR4(hconfig, content, keywordEnforcer, &
    addKey, addKeyString, index, keyString, rc)

    type(ESMF_HConfigIter), intent(in)        :: hconfig
    real(ESMF_KIND_R4), intent(in)            :: content
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_HConfig), intent(in),  optional :: addKey
    character(*),       intent(in),  optional :: addKeyString
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(out), optional :: rc

    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hcontent

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigIterGetInit, hconfig, rc)

    hcontent = ESMF_HConfigCreate(content=content, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_HConfigAddMapVal(hconfig, hcontent, &
      addKey=addKey, addKeyString=addKeyString, &
      index=index, keyString=keyString, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! clean up
    call ESMF_HConfigDestroy(hcontent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigAddMapValR4Seq()"

  subroutine ESMF_HConfigAddMapValR4Seq(hconfig, content, keywordEnforcer, &
    addKey, addKeyString, index, keyString, rc)

    type(ESMF_HConfigIter), intent(in)        :: hconfig
    real(ESMF_KIND_R4), intent(in)            :: content(:)
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_HConfig), intent(in),  optional :: addKey
    character(*),       intent(in),  optional :: addKeyString
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(out), optional :: rc

    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hcontent

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigIterGetInit, hconfig, rc)

    hcontent = ESMF_HConfigCreate(content=content, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_HConfigAddMapVal(hconfig, hcontent, &
      addKey=addKey, addKeyString=addKeyString, &
      index=index, keyString=keyString, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! clean up
    call ESMF_HConfigDestroy(hcontent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigAddMapValR8()"

  subroutine ESMF_HConfigAddMapValR8(hconfig, content, keywordEnforcer, &
    addKey, addKeyString, index, keyString, rc)

    type(ESMF_HConfigIter), intent(in)        :: hconfig
    real(ESMF_KIND_R8), intent(in)            :: content
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_HConfig), intent(in),  optional :: addKey
    character(*),       intent(in),  optional :: addKeyString
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(out), optional :: rc

    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hcontent

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigIterGetInit, hconfig, rc)

    hcontent = ESMF_HConfigCreate(content=content, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_HConfigAddMapVal(hconfig, hcontent, &
      addKey=addKey, addKeyString=addKeyString, &
      index=index, keyString=keyString, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! clean up
    call ESMF_HConfigDestroy(hcontent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigAddMapValR8Seq()"

  subroutine ESMF_HConfigAddMapValR8Seq(hconfig, content, keywordEnforcer, &
    addKey, addKeyString, index, keyString, rc)

    type(ESMF_HConfigIter), intent(in)        :: hconfig
    real(ESMF_KIND_R8), intent(in)            :: content(:)
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_HConfig), intent(in),  optional :: addKey
    character(*),       intent(in),  optional :: addKeyString
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(out), optional :: rc

    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hcontent

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigIterGetInit, hconfig, rc)

    hcontent = ESMF_HConfigCreate(content=content, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_HConfigAddMapVal(hconfig, hcontent, &
      addKey=addKey, addKeyString=addKeyString, &
      index=index, keyString=keyString, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! clean up
    call ESMF_HConfigDestroy(hcontent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigAddMapValString()"

  subroutine ESMF_HConfigAddMapValString(hconfig, content, keywordEnforcer, &
    addKey, addKeyString, index, keyString, rc)

    type(ESMF_HConfigIter), intent(in)        :: hconfig
    character(*),       intent(in)            :: content
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_HConfig), intent(in),  optional :: addKey
    character(*),       intent(in),  optional :: addKeyString
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(out), optional :: rc

    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hcontent

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigIterGetInit, hconfig, rc)

    hcontent = ESMF_HConfigCreate(content=content, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_HConfigAddMapVal(hconfig, hcontent, &
      addKey=addKey, addKeyString=addKeyString, &
      index=index, keyString=keyString, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! clean up
    call ESMF_HConfigDestroy(hcontent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigAddMapValStringSeq()"

  subroutine ESMF_HConfigAddMapValStringSeq(hconfig, content, keywordEnforcer, &
    addKey, addKeyString, index, keyString, rc)

    type(ESMF_HConfigIter), intent(in)        :: hconfig
    character(*),       intent(in)            :: content(:)
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_HConfig), intent(in),  optional :: addKey
    character(*),       intent(in),  optional :: addKeyString
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(out), optional :: rc

    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hcontent

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigIterGetInit, hconfig, rc)

    hcontent = ESMF_HConfigCreate(content=content, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_HConfigAddMapVal(hconfig, hcontent, &
      addKey=addKey, addKeyString=addKeyString, &
      index=index, keyString=keyString, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! clean up
    call ESMF_HConfigDestroy(hcontent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
!BOP
! !IROUTINE: ESMF_HConfigAs<TypeSpec> - Return value as <Type>

! !INTERFACE:
!  function ESMF_HConfigAs<TypeSpec>(hconfig, keywordEnforcer, index, keyString, &
!    doc, asOkay, rc)
! !RETURN VALUE:
!    <Type> :: ESMF_HConfigAs<TypeSpec>
!
! !ARGUMENTS:
!    type(ESMF_HConfig[Iter]) , intent(in)     :: hconfig
!type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
!    integer,            intent(in),  optional :: index
!    character(*),       intent(in),  optional :: keyString
!    integer,            intent(in),  optional :: doc
!    logical,            intent(out), optional :: asOkay
!    integer,            intent(out), optional :: rc

! !DESCRIPTION:
!   Return the value of item {\tt hconfig} interpreted as <Type>.
!   The returned value is only valid if {\tt rc == ESMF\_SUCCESS}, and, if
!   provided, {\tt asOkay == .true.}.
!
!   The supported <Type> / <TypeSpec> options are:
!   \begin{itemize}
!   \item {\tt integer(ESMF\_KIND\_I4)} / {\tt I4}
!   \item {\tt integer(ESMF\_KIND\_I8)} / {\tt I8}
!   \item {\tt logical}                 / {\tt Logical}
!   \item {\tt real(ESMF\_KIND\_R4)}    / {\tt R4}
!   \item {\tt real(ESMF\_KIND\_R8)}    / {\tt R8}
!   \item {\tt character(len=:), allocatable} / {\tt String}
!   \end{itemize}
!
! The arguments are:
!   \begin{description}
!   \item[hconfig]
!     {\tt ESMF\_HConfig} or {\tt ESMF\_HConfigIter} object.
!   \item[{[index]}]
!     Attempt to access by index if specified. Mutural exclusive with {\tt keyString}.
!   \item[{[keyString]}]
!     Attempt to access by key string if specified. Mutural exclusive with {\tt index}.
!   \item[{[doc]}]
!     The doc index. Defaults to the first document.
!   \item[{[asOkay]}]
!     Set to {\tt .true.} for successful convertion to the requested typekind.
!     Set to {\tt .false.} otherwise. By default, i.e. without {\tt asOkay},
!     the latter condition leads to {\tt rc /= ESMF\_SUCCESS}.
!     Providing {\tt asOkay} returns {\tt rc == ESMF\_SUCCESS} in either case,
!     and the validity of the returned converted value is determined by
!     {\tt asOkay}.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigAsI4()"
  function ESMF_HConfigAsI4(hconfig, keywordEnforcer, index, keyString, &
    doc, asOkay, rc)

    integer(ESMF_KIND_I4) :: ESMF_HConfigAsI4

    type(ESMF_HConfig), intent(in)            :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    logical,            intent(out), optional :: asOkay
    integer,            intent(out), optional :: rc

    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hconfigTemp
    type(ESMF_Logical)    :: flag

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    if (present(index).or.present(keyString).or.present(doc)) then
      hconfigTemp = ESMF_HConfigCreateAt(hconfig, index=index, &
        keyString=keyString, doc=doc, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      ! Call into the C++ interface to get the I4
      call c_ESMC_HConfigAsI4(hconfigTemp, ESMF_HConfigAsI4, flag, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      ! clean up
      call ESMF_HConfigDestroy(hconfigTemp, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    else
      ! Call into the C++ interface to get the I4
      call c_ESMC_HConfigAsI4(hconfig, ESMF_HConfigAsI4, flag, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    ! handle asOkay
    if (present(asOkay)) then
      asOkay = flag
    else if (flag == ESMF_FALSE) then
      call ESMF_LogSetError(ESMF_RC_ARG_INCOMP, &
        msg="Conversion to the requested typekind is not supported", &
        ESMF_CONTEXT, rcToReturn=rc)
      return
    endif

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigIterAsI4()"
  function ESMF_HConfigIterAsI4(hconfig, keywordEnforcer, index, keyString, &
    doc, asOkay, rc)

    integer(ESMF_KIND_I4) :: ESMF_HConfigIterAsI4

    type(ESMF_HConfigIter), intent(in)        :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    logical,            intent(out), optional :: asOkay
    integer,            intent(out), optional :: rc

    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hconfigTemp

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    hconfigTemp = ESMF_HConfigIterAsHConfig(hconfig, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ESMF_HConfigIterAsI4 = ESMF_HConfigAsI4(hconfigTemp, &
      index=index, keyString=keyString, doc=doc, asOkay=asOkay, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigAsI8()"

  function ESMF_HConfigAsI8(hconfig, keywordEnforcer, index, keyString, &
    doc, asOkay, rc)

    integer(ESMF_KIND_I8) :: ESMF_HConfigAsI8

    type(ESMF_HConfig), intent(in)            :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    logical,            intent(out), optional :: asOkay
    integer,            intent(out), optional :: rc

    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hconfigTemp
    type(ESMF_Logical)    :: flag

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    if (present(index).or.present(keyString).or.present(doc)) then
      hconfigTemp = ESMF_HConfigCreateAt(hconfig, index=index, &
        keyString=keyString, doc=doc, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      ! Call into the C++ interface to get the I8
      call c_ESMC_HConfigAsI8(hconfigTemp, ESMF_HConfigAsI8, flag, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      ! clean up
      call ESMF_HConfigDestroy(hconfigTemp, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    else
      ! Call into the C++ interface to get the I8
      call c_ESMC_HConfigAsI8(hconfig, ESMF_HConfigAsI8, flag, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    ! handle asOkay
    if (present(asOkay)) then
      asOkay = flag
    else if (flag == ESMF_FALSE) then
      call ESMF_LogSetError(ESMF_RC_ARG_INCOMP, &
        msg="Conversion to the requested typekind is not supported", &
        ESMF_CONTEXT, rcToReturn=rc)
      return
    endif

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigIterAsI8()"
  function ESMF_HConfigIterAsI8(hconfig, keywordEnforcer, index, keyString, &
    doc, asOkay, rc)

    integer(ESMF_KIND_I8) :: ESMF_HConfigIterAsI8

    type(ESMF_HConfigIter), intent(in)        :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    logical,            intent(out), optional :: asOkay
    integer,            intent(out), optional :: rc

    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hconfigTemp

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    hconfigTemp = ESMF_HConfigIterAsHConfig(hconfig, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ESMF_HConfigIterAsI8 = ESMF_HConfigAsI8(hconfigTemp, &
      index=index, keyString=keyString, doc=doc, asOkay=asOkay, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigAsLogical()"

  function ESMF_HConfigAsLogical(hconfig, keywordEnforcer, index, keyString, &
    doc, asOkay, rc)

    logical :: ESMF_HConfigAsLogical

    type(ESMF_HConfig), intent(in)            :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    logical,            intent(out), optional :: asOkay
    integer,            intent(out), optional :: rc

    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hconfigTemp
    type(ESMF_Logical)    :: value
    type(ESMF_Logical)    :: flag

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    if (present(index).or.present(keyString).or.present(doc)) then
      hconfigTemp = ESMF_HConfigCreateAt(hconfig, index=index, &
        keyString=keyString, doc=doc, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      ! Call into the C++ interface to get the Logical
      call c_ESMC_HConfigAsLogical(hconfigTemp, value, flag, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      ! clean up
      call ESMF_HConfigDestroy(hconfigTemp, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    else
      ! Call into the C++ interface to get the Logical
      call c_ESMC_HConfigAsLogical(hconfig, value, flag, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    ! handle asOkay
    if (present(asOkay)) then
      asOkay = flag
    else if (flag == ESMF_FALSE) then
      call ESMF_LogSetError(ESMF_RC_ARG_INCOMP, &
        msg="Conversion to the requested typekind is not supported", &
        ESMF_CONTEXT, rcToReturn=rc)
      return
    endif

    ! convert ESMF_Logical -> logical
    ESMF_HConfigAsLogical = value

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigIterAsLogical()"
  function ESMF_HConfigIterAsLogical(hconfig, keywordEnforcer, index, keyString, &
    doc, asOkay, rc)

    logical :: ESMF_HConfigIterAsLogical

    type(ESMF_HConfigIter), intent(in)        :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    logical,            intent(out), optional :: asOkay
    integer,            intent(out), optional :: rc

    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hconfigTemp

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    hconfigTemp = ESMF_HConfigIterAsHConfig(hconfig, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ESMF_HConfigIterAsLogical = ESMF_HConfigAsLogical(hconfigTemp, &
      index=index, keyString=keyString, doc=doc, asOkay=asOkay, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigAsR4()"

  function ESMF_HConfigAsR4(hconfig, keywordEnforcer, index, keyString, &
    doc, asOkay, rc)

    real(ESMF_KIND_R4) :: ESMF_HConfigAsR4

    type(ESMF_HConfig), intent(in)            :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    logical,            intent(out), optional :: asOkay
    integer,            intent(out), optional :: rc

    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hconfigTemp
    type(ESMF_Logical)    :: flag

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    if (present(index).or.present(keyString).or.present(doc)) then
      hconfigTemp = ESMF_HConfigCreateAt(hconfig, index=index, &
        keyString=keyString, doc=doc, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      ! Call into the C++ interface to get the R4
      call c_ESMC_HConfigAsR4(hconfigTemp, ESMF_HConfigAsR4, flag, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      ! clean up
      call ESMF_HConfigDestroy(hconfigTemp, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    else
      ! Call into the C++ interface to get the R4
      call c_ESMC_HConfigAsR4(hconfig, ESMF_HConfigAsR4, flag, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    ! handle asOkay
    if (present(asOkay)) then
      asOkay = flag
    else if (flag == ESMF_FALSE) then
      call ESMF_LogSetError(ESMF_RC_ARG_INCOMP, &
        msg="Conversion to the requested typekind is not supported", &
        ESMF_CONTEXT, rcToReturn=rc)
      return
    endif

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigIterAsR4()"
  function ESMF_HConfigIterAsR4(hconfig, keywordEnforcer, index, keyString, &
    doc, asOkay, rc)

    real(ESMF_KIND_R4) :: ESMF_HConfigIterAsR4

    type(ESMF_HConfigIter), intent(in)        :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    logical,            intent(out), optional :: asOkay
    integer,            intent(out), optional :: rc

    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hconfigTemp

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    hconfigTemp = ESMF_HConfigIterAsHConfig(hconfig, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ESMF_HConfigIterAsR4 = ESMF_HConfigAsR4(hconfigTemp, &
      index=index, keyString=keyString, doc=doc, asOkay=asOkay, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigAsR8()"

  function ESMF_HConfigAsR8(hconfig, keywordEnforcer, index, keyString, &
    doc, asOkay, rc)

    real(ESMF_KIND_R8) :: ESMF_HConfigAsR8

    type(ESMF_HConfig), intent(in)            :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    logical,            intent(out), optional :: asOkay
    integer,            intent(out), optional :: rc

    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hconfigTemp
    type(ESMF_Logical)    :: flag

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    if (present(index).or.present(keyString).or.present(doc)) then
      hconfigTemp = ESMF_HConfigCreateAt(hconfig, index=index, &
        keyString=keyString, doc=doc, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      ! Call into the C++ interface to get the R8
      call c_ESMC_HConfigAsR8(hconfigTemp, ESMF_HConfigAsR8, flag, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      ! clean up
      call ESMF_HConfigDestroy(hconfigTemp, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    else
      ! Call into the C++ interface to get the R8
      call c_ESMC_HConfigAsR8(hconfig, ESMF_HConfigAsR8, flag, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    ! handle asOkay
    if (present(asOkay)) then
      asOkay = flag
    else if (flag == ESMF_FALSE) then
      call ESMF_LogSetError(ESMF_RC_ARG_INCOMP, &
        msg="Conversion to the requested typekind is not supported", &
        ESMF_CONTEXT, rcToReturn=rc)
      return
    endif

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigIterAsR8()"
  function ESMF_HConfigIterAsR8(hconfig, keywordEnforcer, index, keyString, &
    doc, asOkay, rc)

    real(ESMF_KIND_R8) :: ESMF_HConfigIterAsR8

    type(ESMF_HConfigIter), intent(in)        :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    logical,            intent(out), optional :: asOkay
    integer,            intent(out), optional :: rc

    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hconfigTemp

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    hconfigTemp = ESMF_HConfigIterAsHConfig(hconfig, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ESMF_HConfigIterAsR8 = ESMF_HConfigAsR8(hconfigTemp, &
      index=index, keyString=keyString, doc=doc, asOkay=asOkay, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigAsString()"

  function ESMF_HConfigAsString(hconfig, keywordEnforcer, index, keyString, &
    doc, asOkay, rc)

    character(len=:), allocatable :: ESMF_HConfigAsString

    type(ESMF_HConfig), intent(in)            :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    logical,            intent(out), optional :: asOkay
    integer,            intent(out), optional :: rc

    integer               :: localrc                ! local return code
    integer               :: len
    type(ESMF_HConfig)    :: hconfigTemp
    type(ESMF_Logical)    :: flag

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    if (present(index).or.present(keyString).or.present(doc)) then
      hconfigTemp = ESMF_HConfigCreateAt(hconfig, index=index, &
        keyString=keyString, doc=doc, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! Call into the C++ interface to get length
      call c_ESMC_HConfigAsStringLen(hconfigTemp, len, flag, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! correctly size the character allocation
      allocate(character(len=len)::ESMF_HConfigAsString)

      ! Call into the C++ interface to get the string
      call c_ESMC_HConfigAsString(hconfigTemp, ESMF_HConfigAsString, &
        flag, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! clean up
      call ESMF_HConfigDestroy(hconfigTemp, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    else
      ! Call into the C++ interface to get length
      call c_ESMC_HConfigAsStringLen(hconfig, len, flag, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! correctly size the character allocation
      allocate(character(len=len)::ESMF_HConfigAsString)

      ! Call into the C++ interface to get the string
      call c_ESMC_HConfigAsString(hconfig, ESMF_HConfigAsString, &
        flag, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    ! handle asOkay
    if (present(asOkay)) then
      asOkay = flag
    else if (flag == ESMF_FALSE) then
      call ESMF_LogSetError(ESMF_RC_ARG_INCOMP, &
        msg="Conversion to the requested typekind is not supported", &
        ESMF_CONTEXT, rcToReturn=rc)
      return
    endif

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigIterAsString()"
  function ESMF_HConfigIterAsString(hconfig, keywordEnforcer, index, keyString, &
    doc, asOkay, rc)

    character(len=:), allocatable :: ESMF_HConfigIterAsString

    type(ESMF_HConfigIter), intent(in)        :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    logical,            intent(out), optional :: asOkay
    integer,            intent(out), optional :: rc

    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hconfigTemp

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    hconfigTemp = ESMF_HConfigIterAsHConfig(hconfig, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ESMF_HConfigIterAsString = ESMF_HConfigAsString(hconfigTemp, &
      index=index, keyString=keyString, doc=doc, asOkay=asOkay, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
!BOP
! !IROUTINE: ESMF_HConfigAs<TypeSpec>MapKey - Return map key as <Type>

! !INTERFACE:
!  function ESMF_HConfigAs<TypeSpec>MapKey(hconfig, keywordEnforcer, index, keyString, &
!    doc, asOkay, rc)
! !RETURN VALUE:
!    <Type> :: ESMF_HConfigAs<TypeSpec>MapKey
!
! !ARGUMENTS:
!    type(ESMF_HConfigIter), intent(in)        :: hconfig
!type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
!    integer,            intent(in),  optional :: index
!    character(*),       intent(in),  optional :: keyString
!    integer,            intent(in),  optional :: doc
!    logical,            intent(out), optional :: asOkay
!    integer,            intent(out), optional :: rc

! !DESCRIPTION:
!   Return the map key of item {\tt hconfig} interpreted as <Type>.
!   The returned value is only valid if {\tt rc == ESMF\_SUCCESS}, and, if
!   provided, {\tt asOkay == .true.}.
!
!   The supported <Type> / <TypeSpec> options are:
!   \begin{itemize}
!   \item {\tt integer(ESMF\_KIND\_I4)} / {\tt I4}
!   \item {\tt integer(ESMF\_KIND\_I8)} / {\tt I8}
!   \item {\tt logical}                 / {\tt Logical}
!   \item {\tt real(ESMF\_KIND\_R4)}    / {\tt R4}
!   \item {\tt real(ESMF\_KIND\_R8)}    / {\tt R8}
!   \item {\tt character(len=:), allocatable} / {\tt String}
!   \end{itemize}
!
! The arguments are:
!   \begin{description}
!   \item[hconfig]
!     {\tt ESMF\_HConfigIter} object.
!   \item[{[index]}]
!     Attempt to access by index if specified. Mutural exclusive with {\tt keyString}.
!   \item[{[keyString]}]
!     Attempt to access by key string if specified. Mutural exclusive with {\tt index}.
!   \item[{[doc]}]
!     The doc index. Defaults to the first document.
!   \item[{[asOkay]}]
!     Set to {\tt .true.} for successful convertion to the requested typekind.
!     Set to {\tt .false.} otherwise. By default, i.e. without {\tt asOkay},
!     the latter condition leads to {\tt rc /= ESMF\_SUCCESS}.
!     Providing {\tt asOkay} returns {\tt rc == ESMF\_SUCCESS} in either case,
!     and the validity of the returned converted value is determined by
!     {\tt asOkay}.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigAsI4MapKey()"

  function ESMF_HConfigAsI4MapKey(hconfig, keywordEnforcer, index, keyString, &
    doc, asOkay, rc)

    integer(ESMF_KIND_I4) :: ESMF_HConfigAsI4MapKey

    type(ESMF_HConfigIter), intent(in)        :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    logical,            intent(out), optional :: asOkay
    integer,            intent(out), optional :: rc

    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hconfigTemp
    type(ESMF_Logical)    :: flag

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigIterGetInit, hconfig, rc)

    if (present(index).or.present(keyString).or.present(doc)) then
      hconfigTemp = ESMF_HConfigCreateAtMapKey(hconfig, index=index, &
        keyString=keyString, doc=doc, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      ! Call into the C++ interface to get the I4
      call c_ESMC_HConfigAsI4(hconfigTemp, ESMF_HConfigAsI4MapKey, &
        flag, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      ! clean up
      call ESMF_HConfigDestroy(hconfigTemp, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    else
      ! Call into the C++ interface to get the I4
      call c_ESMC_HConfigAsI4MapKey(hconfig, ESMF_HConfigAsI4MapKey, &
        flag, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    ! handle asOkay
    if (present(asOkay)) then
      asOkay = flag
    else if (flag == ESMF_FALSE) then
      call ESMF_LogSetError(ESMF_RC_ARG_INCOMP, &
        msg="Conversion to the requested typekind is not supported", &
        ESMF_CONTEXT, rcToReturn=rc)
      return
    endif

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigAsI8MapKey()"

  function ESMF_HConfigAsI8MapKey(hconfig, keywordEnforcer, index, keyString, &
    doc, asOkay, rc)

    integer(ESMF_KIND_I8) :: ESMF_HConfigAsI8MapKey

    type(ESMF_HConfigIter), intent(in)        :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    logical,            intent(out), optional :: asOkay
    integer,            intent(out), optional :: rc

    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hconfigTemp
    type(ESMF_Logical)    :: flag

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigIterGetInit, hconfig, rc)

    if (present(index).or.present(keyString).or.present(doc)) then
      hconfigTemp = ESMF_HConfigCreateAtMapKey(hconfig, index=index, &
        keyString=keyString, doc=doc, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      ! Call into the C++ interface to get the I8
      call c_ESMC_HConfigAsI8(hconfigTemp, ESMF_HConfigAsI8MapKey, &
        flag, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      ! clean up
      call ESMF_HConfigDestroy(hconfigTemp, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    else
      ! Call into the C++ interface to get the I8
      call c_ESMC_HConfigAsI8MapKey(hconfig, ESMF_HConfigAsI8MapKey, &
        flag, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    ! handle asOkay
    if (present(asOkay)) then
      asOkay = flag
    else if (flag == ESMF_FALSE) then
      call ESMF_LogSetError(ESMF_RC_ARG_INCOMP, &
        msg="Conversion to the requested typekind is not supported", &
        ESMF_CONTEXT, rcToReturn=rc)
      return
    endif

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigAsLogicalMapKey()"

  function ESMF_HConfigAsLogicalMapKey(hconfig, keywordEnforcer, index, keyString, &
    doc, asOkay, rc)

    logical :: ESMF_HConfigAsLogicalMapKey

    type(ESMF_HConfigIter), intent(in)        :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    logical,            intent(out), optional :: asOkay
    integer,            intent(out), optional :: rc

    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hconfigTemp
    type(ESMF_Logical)    :: value
    type(ESMF_Logical)    :: flag

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigIterGetInit, hconfig, rc)

    if (present(index).or.present(keyString).or.present(doc)) then
      hconfigTemp = ESMF_HConfigCreateAtMapKey(hconfig, index=index, &
        keyString=keyString, doc=doc, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      ! Call into the C++ interface to get the Logical
      call c_ESMC_HConfigAsLogical(hconfigTemp, value, flag, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      ! clean up
      call ESMF_HConfigDestroy(hconfigTemp, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    else
      ! Call into the C++ interface to get the Logical
      call c_ESMC_HConfigAsLogicalMapKey(hconfig, value, flag, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    ! handle asOkay
    if (present(asOkay)) then
      asOkay = flag
    else if (flag == ESMF_FALSE) then
      call ESMF_LogSetError(ESMF_RC_ARG_INCOMP, &
        msg="Conversion to the requested typekind is not supported", &
        ESMF_CONTEXT, rcToReturn=rc)
      return
    endif

    ! convert ESMF_Logical -> logical
    ESMF_HConfigAsLogicalMapKey = value

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigAsR4MapKey()"

  function ESMF_HConfigAsR4MapKey(hconfig, keywordEnforcer, index, keyString, &
    doc, asOkay, rc)

    real(ESMF_KIND_R4) :: ESMF_HConfigAsR4MapKey

    type(ESMF_HConfigIter), intent(in)        :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    logical,            intent(out), optional :: asOkay
    integer,            intent(out), optional :: rc

    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hconfigTemp
    type(ESMF_Logical)    :: flag

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigIterGetInit, hconfig, rc)

    if (present(index).or.present(keyString).or.present(doc)) then
      hconfigTemp = ESMF_HConfigCreateAtMapKey(hconfig, index=index, &
        keyString=keyString, doc=doc, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      ! Call into the C++ interface to get the R4
      call c_ESMC_HConfigAsR4(hconfigTemp, ESMF_HConfigAsR4MapKey, &
        flag, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      ! clean up
      call ESMF_HConfigDestroy(hconfigTemp, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    else
      ! Call into the C++ interface to get the R4
      call c_ESMC_HConfigAsR4MapKey(hconfig, ESMF_HConfigAsR4MapKey, &
        flag, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    ! handle asOkay
    if (present(asOkay)) then
      asOkay = flag
    else if (flag == ESMF_FALSE) then
      call ESMF_LogSetError(ESMF_RC_ARG_INCOMP, &
        msg="Conversion to the requested typekind is not supported", &
        ESMF_CONTEXT, rcToReturn=rc)
      return
    endif

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigAsR8MapKey()"

  function ESMF_HConfigAsR8MapKey(hconfig, keywordEnforcer, index, keyString, &
    doc, asOkay, rc)

    real(ESMF_KIND_R8) :: ESMF_HConfigAsR8MapKey

    type(ESMF_HConfigIter), intent(in)        :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    logical,            intent(out), optional :: asOkay
    integer,            intent(out), optional :: rc

    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hconfigTemp
    type(ESMF_Logical)    :: flag

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigIterGetInit, hconfig, rc)

    if (present(index).or.present(keyString).or.present(doc)) then
      hconfigTemp = ESMF_HConfigCreateAtMapKey(hconfig, index=index, &
        keyString=keyString, doc=doc, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      ! Call into the C++ interface to get the R8
      call c_ESMC_HConfigAsR8(hconfigTemp, ESMF_HConfigAsR8MapKey, &
        flag, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      ! clean up
      call ESMF_HConfigDestroy(hconfigTemp, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    else
      ! Call into the C++ interface to get the R8
      call c_ESMC_HConfigAsR8MapKey(hconfig, ESMF_HConfigAsR8MapKey, &
        flag, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    ! handle asOkay
    if (present(asOkay)) then
      asOkay = flag
    else if (flag == ESMF_FALSE) then
      call ESMF_LogSetError(ESMF_RC_ARG_INCOMP, &
        msg="Conversion to the requested typekind is not supported", &
        ESMF_CONTEXT, rcToReturn=rc)
      return
    endif

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigAsStringMapKey()"

  function ESMF_HConfigAsStringMapKey(hconfig, keywordEnforcer, index, keyString, &
    doc, asOkay, rc)

    character(len=:), allocatable :: ESMF_HConfigAsStringMapKey

    type(ESMF_HConfigIter), intent(in)        :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    logical,            intent(out), optional :: asOkay
    integer,            intent(out), optional :: rc

    integer               :: localrc                ! local return code
    integer               :: len
    type(ESMF_HConfig)    :: hconfigTemp
    type(ESMF_Logical)    :: flag

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigIterGetInit, hconfig, rc)

    if (present(index).or.present(keyString).or.present(doc)) then
      hconfigTemp = ESMF_HConfigCreateAtMapKey(hconfig, index=index, &
        keyString=keyString, doc=doc, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! Call into the C++ interface to get length
      call c_ESMC_HConfigAsStringLen(hconfigTemp, len, flag, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! correctly size the character allocation
      allocate(character(len=len)::ESMF_HConfigAsStringMapKey)

      ! Call into the C++ interface to get the string
      call c_ESMC_HConfigAsString(hconfigTemp, ESMF_HConfigAsStringMapKey, &
        flag, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! clean up
      call ESMF_HConfigDestroy(hconfigTemp, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    else
      ! Call into the C++ interface to get length
      call c_ESMC_HConfigAsStringMapKeyLen(hconfig, len, flag, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! correctly size the character allocation
      allocate(character(len=len)::ESMF_HConfigAsStringMapKey)

      ! Call into the C++ interface to get the string
      call c_ESMC_HConfigAsStringMapKey(hconfig, ESMF_HConfigAsStringMapKey, &
        flag, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    ! handle asOkay
    if (present(asOkay)) then
      asOkay = flag
    else if (flag == ESMF_FALSE) then
      call ESMF_LogSetError(ESMF_RC_ARG_INCOMP, &
        msg="Conversion to the requested typekind is not supported", &
        ESMF_CONTEXT, rcToReturn=rc)
      return
    endif

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
!BOP
! !IROUTINE: ESMF_HConfigAs<TypeSpec>MapVal - Return map value as <Type>

! !INTERFACE:
!  function ESMF_HConfigAs<TypeSpec>MapVal(hconfig, keywordEnforcer, index, keyString, &
!    doc, asOkay, rc)
! !RETURN VALUE:
!    <Type> :: ESMF_HConfigAs<TypeSpec>MapVal
!
! !ARGUMENTS:
!    type(ESMF_HConfigIter), intent(in)        :: hconfig
!type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
!    integer,            intent(in),  optional :: index
!    character(*),       intent(in),  optional :: keyString
!    integer,            intent(in),  optional :: doc
!    logical,            intent(out), optional :: asOkay
!    integer,            intent(out), optional :: rc

! !DESCRIPTION:
!   Return the map value of item {\tt hconfig} interpreted as <Type>.
!   The returned value is only valid if {\tt rc == ESMF\_SUCCESS}, and, if
!   provided, {\tt asOkay == .true.}.
!
!   The supported <Type> / <TypeSpec> options are:
!   \begin{itemize}
!   \item {\tt integer(ESMF\_KIND\_I4)} / {\tt I4}
!   \item {\tt integer(ESMF\_KIND\_I8)} / {\tt I8}
!   \item {\tt logical}                 / {\tt Logical}
!   \item {\tt real(ESMF\_KIND\_R4)}    / {\tt R4}
!   \item {\tt real(ESMF\_KIND\_R8)}    / {\tt R8}
!   \item {\tt character(len=:), allocatable} / {\tt String}
!   \end{itemize}
!
! The arguments are:
!   \begin{description}
!   \item[hconfig]
!     {\tt ESMF\_HConfigIter} object.
!   \item[{[index]}]
!     Attempt to access by index if specified. Mutural exclusive with {\tt keyString}.
!   \item[{[keyString]}]
!     Attempt to access by key string if specified. Mutural exclusive with {\tt index}.
!   \item[{[doc]}]
!     The doc index. Defaults to the first document.
!   \item[{[asOkay]}]
!     Set to {\tt .true.} for successful convertion to the requested typekind.
!     Set to {\tt .false.} otherwise. By default, i.e. without {\tt asOkay},
!     the latter condition leads to {\tt rc /= ESMF\_SUCCESS}.
!     Providing {\tt asOkay} returns {\tt rc == ESMF\_SUCCESS} in either case,
!     and the validity of the returned converted value is determined by
!     {\tt asOkay}.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigAsI4MapVal()"

  function ESMF_HConfigAsI4MapVal(hconfig, keywordEnforcer, index, keyString, &
    doc, asOkay, rc)

    integer(ESMF_KIND_I4) :: ESMF_HConfigAsI4MapVal

    type(ESMF_HConfigIter), intent(in)        :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    logical,            intent(out), optional :: asOkay
    integer,            intent(out), optional :: rc

    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hconfigTemp
    type(ESMF_Logical)    :: flag

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigIterGetInit, hconfig, rc)

    if (present(index).or.present(keyString).or.present(doc)) then
      hconfigTemp = ESMF_HConfigCreateAtMapVal(hconfig, index=index, &
        keyString=keyString, doc=doc, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      ! Call into the C++ interface to get the I4
      call c_ESMC_HConfigAsI4(hconfigTemp, ESMF_HConfigAsI4MapVal, &
        flag, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      ! clean up
      call ESMF_HConfigDestroy(hconfigTemp, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    else
      ! Call into the C++ interface to get the I4
      call c_ESMC_HConfigAsI4MapVal(hconfig, ESMF_HConfigAsI4MapVal, &
        flag, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    ! handle asOkay
    if (present(asOkay)) then
      asOkay = flag
    else if (flag == ESMF_FALSE) then
      call ESMF_LogSetError(ESMF_RC_ARG_INCOMP, &
        msg="Conversion to the requested typekind is not supported", &
        ESMF_CONTEXT, rcToReturn=rc)
      return
    endif

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigAsI8MapVal()"

  function ESMF_HConfigAsI8MapVal(hconfig, keywordEnforcer, index, keyString, &
    doc, asOkay, rc)

    integer(ESMF_KIND_I8) :: ESMF_HConfigAsI8MapVal

    type(ESMF_HConfigIter), intent(in)        :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    logical,            intent(out), optional :: asOkay
    integer,            intent(out), optional :: rc

    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hconfigTemp
    type(ESMF_Logical)    :: flag

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigIterGetInit, hconfig, rc)

    if (present(index).or.present(keyString).or.present(doc)) then
      hconfigTemp = ESMF_HConfigCreateAtMapVal(hconfig, index=index, &
        keyString=keyString, doc=doc, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      ! Call into the C++ interface to get the I8
      call c_ESMC_HConfigAsI8(hconfigTemp, ESMF_HConfigAsI8MapVal, &
        flag, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      ! clean up
      call ESMF_HConfigDestroy(hconfigTemp, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    else
      ! Call into the C++ interface to get the I8
      call c_ESMC_HConfigAsI8MapVal(hconfig, ESMF_HConfigAsI8MapVal, &
        flag, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    ! handle asOkay
    if (present(asOkay)) then
      asOkay = flag
    else if (flag == ESMF_FALSE) then
      call ESMF_LogSetError(ESMF_RC_ARG_INCOMP, &
        msg="Conversion to the requested typekind is not supported", &
        ESMF_CONTEXT, rcToReturn=rc)
      return
    endif

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigAsLogicalMapVal()"

  function ESMF_HConfigAsLogicalMapVal(hconfig, keywordEnforcer, index, keyString, &
    doc, asOkay, rc)

    logical :: ESMF_HConfigAsLogicalMapVal

    type(ESMF_HConfigIter), intent(in)        :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    logical,            intent(out), optional :: asOkay
    integer,            intent(out), optional :: rc

    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hconfigTemp
    type(ESMF_Logical)    :: value
    type(ESMF_Logical)    :: flag

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigIterGetInit, hconfig, rc)

    if (present(index).or.present(keyString).or.present(doc)) then
      hconfigTemp = ESMF_HConfigCreateAtMapVal(hconfig, index=index, &
        keyString=keyString, doc=doc, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      ! Call into the C++ interface to get the Logical
      call c_ESMC_HConfigAsLogical(hconfigTemp, value, flag, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      ! clean up
      call ESMF_HConfigDestroy(hconfigTemp, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    else
      ! Call into the C++ interface to get the Logical
      call c_ESMC_HConfigAsLogicalMapVal(hconfig, value, flag, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    ! handle asOkay
    if (present(asOkay)) then
      asOkay = flag
    else if (flag == ESMF_FALSE) then
      call ESMF_LogSetError(ESMF_RC_ARG_INCOMP, &
        msg="Conversion to the requested typekind is not supported", &
        ESMF_CONTEXT, rcToReturn=rc)
      return
    endif

    ! convert ESMF_Logical -> logical
    ESMF_HConfigAsLogicalMapVal = value

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigAsR4MapVal()"

  function ESMF_HConfigAsR4MapVal(hconfig, keywordEnforcer, index, keyString, &
    doc, asOkay, rc)

    real(ESMF_KIND_R4) :: ESMF_HConfigAsR4MapVal

    type(ESMF_HConfigIter), intent(in)        :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    logical,            intent(out), optional :: asOkay
    integer,            intent(out), optional :: rc

    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hconfigTemp
    type(ESMF_Logical)    :: flag

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigIterGetInit, hconfig, rc)

    if (present(index).or.present(keyString).or.present(doc)) then
      hconfigTemp = ESMF_HConfigCreateAtMapVal(hconfig, index=index, &
        keyString=keyString, doc=doc, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      ! Call into the C++ interface to get the R4
      call c_ESMC_HConfigAsR4(hconfigTemp, ESMF_HConfigAsR4MapVal, &
        flag, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      ! clean up
      call ESMF_HConfigDestroy(hconfigTemp, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    else
      ! Call into the C++ interface to get the R4
      call c_ESMC_HConfigAsR4MapVal(hconfig, ESMF_HConfigAsR4MapVal, &
        flag, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    ! handle asOkay
    if (present(asOkay)) then
      asOkay = flag
    else if (flag == ESMF_FALSE) then
      call ESMF_LogSetError(ESMF_RC_ARG_INCOMP, &
        msg="Conversion to the requested typekind is not supported", &
        ESMF_CONTEXT, rcToReturn=rc)
      return
    endif

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigAsR8MapVal()"

  function ESMF_HConfigAsR8MapVal(hconfig, keywordEnforcer, index, keyString, &
    doc, asOkay, rc)

    real(ESMF_KIND_R8) :: ESMF_HConfigAsR8MapVal

    type(ESMF_HConfigIter), intent(in)        :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    logical,            intent(out), optional :: asOkay
    integer,            intent(out), optional :: rc

    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hconfigTemp
    type(ESMF_Logical)    :: flag

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigIterGetInit, hconfig, rc)

    if (present(index).or.present(keyString).or.present(doc)) then
      hconfigTemp = ESMF_HConfigCreateAtMapVal(hconfig, index=index, &
        keyString=keyString, doc=doc, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      ! Call into the C++ interface to get the R8
      call c_ESMC_HConfigAsR8(hconfigTemp, ESMF_HConfigAsR8MapVal, &
        flag, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      ! clean up
      call ESMF_HConfigDestroy(hconfigTemp, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    else
      ! Call into the C++ interface to get the R8
      call c_ESMC_HConfigAsR8MapVal(hconfig, ESMF_HConfigAsR8MapVal, &
        flag, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    ! handle asOkay
    if (present(asOkay)) then
      asOkay = flag
    else if (flag == ESMF_FALSE) then
      call ESMF_LogSetError(ESMF_RC_ARG_INCOMP, &
        msg="Conversion to the requested typekind is not supported", &
        ESMF_CONTEXT, rcToReturn=rc)
      return
    endif

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigAsStringMapVal()"

  function ESMF_HConfigAsStringMapVal(hconfig, keywordEnforcer, index, keyString, &
    doc, asOkay, rc)

    character(len=:), allocatable :: ESMF_HConfigAsStringMapVal

    type(ESMF_HConfigIter), intent(in)        :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    logical,            intent(out), optional :: asOkay
    integer,            intent(out), optional :: rc

    integer               :: localrc                ! local return code
    integer               :: len
    type(ESMF_HConfig)    :: hconfigTemp
    type(ESMF_Logical)    :: flag

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigIterGetInit, hconfig, rc)

    if (present(index).or.present(keyString).or.present(doc)) then
      hconfigTemp = ESMF_HConfigCreateAtMapVal(hconfig, index=index, &
        keyString=keyString, doc=doc, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! Call into the C++ interface to get length
      call c_ESMC_HConfigAsStringLen(hconfigTemp, len, flag, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! correctly size the character allocation
      allocate(character(len=len)::ESMF_HConfigAsStringMapVal)

      ! Call into the C++ interface to get the string
      call c_ESMC_HConfigAsString(hconfigTemp, ESMF_HConfigAsStringMapVal, &
        flag, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! clean up
      call ESMF_HConfigDestroy(hconfigTemp, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    else
      ! Call into the C++ interface to get length
      call c_ESMC_HConfigAsStringMapValLen(hconfig, len, flag, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! correctly size the character allocation
      allocate(character(len=len)::ESMF_HConfigAsStringMapVal)

      ! Call into the C++ interface to get the string
      call c_ESMC_HConfigAsStringMapVal(hconfig, ESMF_HConfigAsStringMapVal, &
        flag, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    ! handle asOkay
    if (present(asOkay)) then
      asOkay = flag
    else if (flag == ESMF_FALSE) then
      call ESMF_LogSetError(ESMF_RC_ARG_INCOMP, &
        msg="Conversion to the requested typekind is not supported", &
        ESMF_CONTEXT, rcToReturn=rc)
      return
    endif

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
!BOP
! !IROUTINE: ESMF_HConfigAs<TypeSpec>Seq - Return value as sequence array of <Type>

! !INTERFACE:
!  function ESMF_HConfigAs<TypeSpec>Seq(hconfig, keywordEnforcer, index, keyString, &
!    doc, asOkay, rc)
! !RETURN VALUE:
!    <Type>, allocatable :: ESMF_HConfigAs<TypeSpec>Seq(:)
!
! !ARGUMENTS:
!    type(ESMF_HConfig[Iter]), intent(in)      :: hconfig
!type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
!    integer,            intent(in),  optional :: index
!    character(*),       intent(in),  optional :: keyString
!    integer,            intent(in),  optional :: doc
!    logical,            intent(out), optional :: asOkay
!    integer,            intent(out), optional :: rc

! !DESCRIPTION:
!   Return the value of item {\tt hconfig} interpreted as sequence of <Type>.
!   The returned value is only valid if {\tt rc == ESMF\_SUCCESS}, and, if
!   provided, {\tt asOkay == .true.}.
!
!   The supported <Type> / <TypeSpec> options are:
!   \begin{itemize}
!   \item {\tt integer(ESMF\_KIND\_I4)} / {\tt I4}
!   \item {\tt integer(ESMF\_KIND\_I8)} / {\tt I8}
!   \item {\tt logical}                 / {\tt Logical}
!   \item {\tt real(ESMF\_KIND\_R4)}    / {\tt R4}
!   \item {\tt real(ESMF\_KIND\_R8)}    / {\tt R8}
!   \item {\tt character(len=:), allocatable} / {\tt String} (See note about {\tt stringLen} argument below!)
!   \end{itemize}
!
!   An extra non-optional argument {\tt stringLen} must be provided for the
!   {\bf String} option. This argument specifies the number of characters in
!   each of the output strings. Longer actual string values are tuncated, while
!   shorter actual string values are padded with white space.
!
! The arguments are:
!   \begin{description}
!   \item[hconfig]
!     {\tt ESMF\_HConfig} or {\tt ESMF\_HConfigIter} object.
!   \item[{[index]}]
!     Attempt to access by index if specified. Mutural exclusive with {\tt keyString}.
!   \item[{[keyString]}]
!     Attempt to access by key string if specified. Mutural exclusive with {\tt index}.
!   \item[{[doc]}]
!     The doc index. Defaults to the first document.
!   \item[{[asOkay]}]
!     Set to {\tt .true.} for successful convertion to the requested typekind.
!     Set to {\tt .false.} otherwise. By default, i.e. without {\tt asOkay},
!     the latter condition leads to {\tt rc /= ESMF\_SUCCESS}.
!     Providing {\tt asOkay} returns {\tt rc == ESMF\_SUCCESS} in either case,
!     and the validity of the returned converted value is determined by
!     {\tt asOkay}.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigAsI4Seq()"

  function ESMF_HConfigAsI4Seq(hconfig, keywordEnforcer, index, keyString, &
    doc, asOkay, rc)

    integer(ESMF_KIND_I4), allocatable :: ESMF_HConfigAsI4Seq(:)

    type(ESMF_HConfig), intent(in)            :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    logical,            intent(out), optional :: asOkay
    integer,            intent(out), optional :: rc

    integer               :: localrc                ! local return code
    logical               :: isScalar, isSequence
    type(ESMF_HConfig)    :: hconfigTemp
    integer               :: i, size

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    isScalar = ESMF_HConfigIsScalar(hconfig, index=index, keyString=keyString, &
      doc=doc, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    if (isScalar) then
      allocate(ESMF_HConfigAsI4Seq(1))
      ESMF_HConfigAsI4Seq(1) = ESMF_HConfigAsI4(hconfig, index=index, &
        keyString=keyString, doc=doc, asOkay=asOkay, rc=localrc)
    else
      isSequence = ESMF_HConfigIsSequence(hconfig, index=index, &
        keyString=keyString, doc=doc, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      if (isSequence) then
        ! access the sequence
        hconfigTemp = ESMF_HConfigCreateAt(hconfig, index=index, &
          keyString=keyString, doc=doc, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
        ! loop through the sequence
        size = ESMF_HConfigGetSize(hconfigTemp, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
        allocate(ESMF_HConfigAsI4Seq(size))
        do i=1, size
          ESMF_HConfigAsI4Seq(i) = ESMF_HConfigAsI4(hconfigTemp, &
            index=i, asOkay=asOkay, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
          if (present(asOkay)) then
            if (.not.asOkay) exit
          endif
        enddo
        ! clean up
        call ESMF_HConfigDestroy(hconfigTemp, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      else
        ! this is an error condition... neither scalar and nor sequence
        call ESMF_LogSetError(ESMF_RC_ARG_INCOMP, &
          msg="Must either be scalar or sequence to use Seq interface", &
          ESMF_CONTEXT, rcToReturn=rc)
        return
      endif
    endif

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigIterAsI4Seq()"
  function ESMF_HConfigIterAsI4Seq(hconfig, keywordEnforcer, index, keyString, &
    doc, asOkay, rc)

    integer(ESMF_KIND_I4), allocatable :: ESMF_HConfigIterAsI4Seq(:)

    type(ESMF_HConfigIter), intent(in)        :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    logical,            intent(out), optional :: asOkay
    integer,            intent(out), optional :: rc

    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hconfigTemp

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    hconfigTemp = ESMF_HConfigIterAsHConfig(hconfig, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ESMF_HConfigIterAsI4Seq = ESMF_HConfigAsI4Seq(hconfigTemp, &
      index=index, keyString=keyString, doc=doc, asOkay=asOkay, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigAsI8Seq()"

  function ESMF_HConfigAsI8Seq(hconfig, keywordEnforcer, index, keyString, &
    doc, asOkay, rc)

    integer(ESMF_KIND_I8), allocatable :: ESMF_HConfigAsI8Seq(:)

    type(ESMF_HConfig), intent(in)            :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    logical,            intent(out), optional :: asOkay
    integer,            intent(out), optional :: rc

    integer               :: localrc                ! local return code
    logical               :: isScalar, isSequence
    type(ESMF_HConfig)    :: hconfigTemp
    integer               :: i, size

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    isScalar = ESMF_HConfigIsScalar(hconfig, index=index, keyString=keyString, &
      doc=doc, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    if (isScalar) then
      allocate(ESMF_HConfigAsI8Seq(1))
      ESMF_HConfigAsI8Seq(1) = ESMF_HConfigAsI8(hconfig, index=index, &
        keyString=keyString, doc=doc, asOkay=asOkay, rc=localrc)
    else
      isSequence = ESMF_HConfigIsSequence(hconfig, index=index, &
        keyString=keyString, doc=doc, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      if (isSequence) then
        ! access the sequence
        hconfigTemp = ESMF_HConfigCreateAt(hconfig, index=index, &
          keyString=keyString, doc=doc, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
        ! loop through the sequence
        size = ESMF_HConfigGetSize(hconfigTemp, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
        allocate(ESMF_HConfigAsI8Seq(size))
        do i=1, size
          ESMF_HConfigAsI8Seq(i) = ESMF_HConfigAsI8(hconfigTemp, &
            index=i, asOkay=asOkay, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
          if (present(asOkay)) then
            if (.not.asOkay) exit
          endif
        enddo
        ! clean up
        call ESMF_HConfigDestroy(hconfigTemp, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      else
        ! this is an error condition... neither scalar and nor sequence
        call ESMF_LogSetError(ESMF_RC_ARG_INCOMP, &
          msg="Must either be scalar or sequence to use Seq interface", &
          ESMF_CONTEXT, rcToReturn=rc)
        return
      endif
    endif

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigIterAsI8Seq()"
  function ESMF_HConfigIterAsI8Seq(hconfig, keywordEnforcer, index, keyString, &
    doc, asOkay, rc)

    integer(ESMF_KIND_I8), allocatable :: ESMF_HConfigIterAsI8Seq(:)

    type(ESMF_HConfigIter), intent(in)        :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    logical,            intent(out), optional :: asOkay
    integer,            intent(out), optional :: rc

    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hconfigTemp

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    hconfigTemp = ESMF_HConfigIterAsHConfig(hconfig, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ESMF_HConfigIterAsI8Seq = ESMF_HConfigAsI8Seq(hconfigTemp, &
      index=index, keyString=keyString, doc=doc, asOkay=asOkay, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigAsLogicalSeq()"

  function ESMF_HConfigAsLogicalSeq(hconfig, keywordEnforcer, index, keyString, &
    doc, asOkay, rc)

    logical, allocatable :: ESMF_HConfigAsLogicalSeq(:)

    type(ESMF_HConfig), intent(in)            :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    logical,            intent(out), optional :: asOkay
    integer,            intent(out), optional :: rc

    integer               :: localrc                ! local return code
    logical               :: isScalar, isSequence
    type(ESMF_HConfig)    :: hconfigTemp
    integer               :: i, size

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    isScalar = ESMF_HConfigIsScalar(hconfig, index=index, keyString=keyString, &
      doc=doc, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    if (isScalar) then
      allocate(ESMF_HConfigAsLogicalSeq(1))
      ESMF_HConfigAsLogicalSeq(1) = ESMF_HConfigAsLogical(hconfig, index=index, &
        keyString=keyString, doc=doc, asOkay=asOkay, rc=localrc)
    else
      isSequence = ESMF_HConfigIsSequence(hconfig, index=index, &
        keyString=keyString, doc=doc, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      if (isSequence) then
        ! access the sequence
        hconfigTemp = ESMF_HConfigCreateAt(hconfig, index=index, &
          keyString=keyString, doc=doc, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
        ! loop through the sequence
        size = ESMF_HConfigGetSize(hconfigTemp, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
        allocate(ESMF_HConfigAsLogicalSeq(size))
        do i=1, size
          ESMF_HConfigAsLogicalSeq(i) = ESMF_HConfigAsLogical(hconfigTemp, &
            index=i, asOkay=asOkay, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
          if (present(asOkay)) then
            if (.not.asOkay) exit
          endif
        enddo
        ! clean up
        call ESMF_HConfigDestroy(hconfigTemp, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      else
        ! this is an error condition... neither scalar and nor sequence
        call ESMF_LogSetError(ESMF_RC_ARG_INCOMP, &
          msg="Must either be scalar or sequence to use Seq interface", &
          ESMF_CONTEXT, rcToReturn=rc)
        return
      endif
    endif

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigIterAsLogicalSeq()"
  function ESMF_HConfigIterAsLogicalSeq(hconfig, keywordEnforcer, index, keyString, &
    doc, asOkay, rc)

    logical, allocatable :: ESMF_HConfigIterAsLogicalSeq(:)

    type(ESMF_HConfigIter), intent(in)        :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    logical,            intent(out), optional :: asOkay
    integer,            intent(out), optional :: rc

    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hconfigTemp

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    hconfigTemp = ESMF_HConfigIterAsHConfig(hconfig, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ESMF_HConfigIterAsLogicalSeq = ESMF_HConfigAsLogicalSeq(hconfigTemp, &
      index=index, keyString=keyString, doc=doc, asOkay=asOkay, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigAsR4Seq()"

  function ESMF_HConfigAsR4Seq(hconfig, keywordEnforcer, index, keyString, &
    doc, asOkay, rc)

    real(ESMF_KIND_R4), allocatable :: ESMF_HConfigAsR4Seq(:)

    type(ESMF_HConfig), intent(in)            :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    logical,            intent(out), optional :: asOkay
    integer,            intent(out), optional :: rc

    integer               :: localrc                ! local return code
    logical               :: isScalar, isSequence
    type(ESMF_HConfig)    :: hconfigTemp
    integer               :: i, size

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    isScalar = ESMF_HConfigIsScalar(hconfig, index=index, keyString=keyString, &
      doc=doc, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    if (isScalar) then
      allocate(ESMF_HConfigAsR4Seq(1))
      ESMF_HConfigAsR4Seq(1) = ESMF_HConfigAsR4(hconfig, index=index, &
        keyString=keyString, doc=doc, asOkay=asOkay, rc=localrc)
    else
      isSequence = ESMF_HConfigIsSequence(hconfig, index=index, &
        keyString=keyString, doc=doc, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      if (isSequence) then
        ! access the sequence
        hconfigTemp = ESMF_HConfigCreateAt(hconfig, index=index, &
          keyString=keyString, doc=doc, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
        ! loop through the sequence
        size = ESMF_HConfigGetSize(hconfigTemp, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
        allocate(ESMF_HConfigAsR4Seq(size))
        do i=1, size
          ESMF_HConfigAsR4Seq(i) = ESMF_HConfigAsR4(hconfigTemp, &
            index=i, asOkay=asOkay, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
          if (present(asOkay)) then
            if (.not.asOkay) exit
          endif
        enddo
        ! clean up
        call ESMF_HConfigDestroy(hconfigTemp, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      else
        ! this is an error condition... neither scalar and nor sequence
        call ESMF_LogSetError(ESMF_RC_ARG_INCOMP, &
          msg="Must either be scalar or sequence to use Seq interface", &
          ESMF_CONTEXT, rcToReturn=rc)
        return
      endif
    endif

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigIterAsR4Seq()"
  function ESMF_HConfigIterAsR4Seq(hconfig, keywordEnforcer, index, keyString, &
    doc, asOkay, rc)

    real(ESMF_KIND_R4), allocatable :: ESMF_HConfigIterAsR4Seq(:)

    type(ESMF_HConfigIter), intent(in)        :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    logical,            intent(out), optional :: asOkay
    integer,            intent(out), optional :: rc

    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hconfigTemp

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    hconfigTemp = ESMF_HConfigIterAsHConfig(hconfig, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ESMF_HConfigIterAsR4Seq = ESMF_HConfigAsR4Seq(hconfigTemp, &
      index=index, keyString=keyString, doc=doc, asOkay=asOkay, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigAsR8Seq()"

  function ESMF_HConfigAsR8Seq(hconfig, keywordEnforcer, index, keyString, &
    doc, asOkay, rc)

    real(ESMF_KIND_R8), allocatable :: ESMF_HConfigAsR8Seq(:)

    type(ESMF_HConfig), intent(in)            :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    logical,            intent(out), optional :: asOkay
    integer,            intent(out), optional :: rc

    integer               :: localrc                ! local return code
    logical               :: isScalar, isSequence
    type(ESMF_HConfig)    :: hconfigTemp
    integer               :: i, size

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    isScalar = ESMF_HConfigIsScalar(hconfig, index=index, keyString=keyString, &
      doc=doc, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    if (isScalar) then
      allocate(ESMF_HConfigAsR8Seq(1))
      ESMF_HConfigAsR8Seq(1) = ESMF_HConfigAsR8(hconfig, index=index, &
        keyString=keyString, doc=doc, asOkay=asOkay, rc=localrc)
    else
      isSequence = ESMF_HConfigIsSequence(hconfig, index=index, &
        keyString=keyString, doc=doc, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      if (isSequence) then
        ! access the sequence
        hconfigTemp = ESMF_HConfigCreateAt(hconfig, index=index, &
          keyString=keyString, doc=doc, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
        ! loop through the sequence
        size = ESMF_HConfigGetSize(hconfigTemp, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
        allocate(ESMF_HConfigAsR8Seq(size))
        do i=1, size
          ESMF_HConfigAsR8Seq(i) = ESMF_HConfigAsR8(hconfigTemp, &
            index=i, asOkay=asOkay, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
          if (present(asOkay)) then
            if (.not.asOkay) exit
          endif
        enddo
        ! clean up
        call ESMF_HConfigDestroy(hconfigTemp, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      else
        ! this is an error condition... neither scalar and nor sequence
        call ESMF_LogSetError(ESMF_RC_ARG_INCOMP, &
          msg="Must either be scalar or sequence to use Seq interface", &
          ESMF_CONTEXT, rcToReturn=rc)
        return
      endif
    endif

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigIterAsR8Seq()"
  function ESMF_HConfigIterAsR8Seq(hconfig, keywordEnforcer, index, keyString, &
    doc, asOkay, rc)

    real(ESMF_KIND_R8), allocatable :: ESMF_HConfigIterAsR8Seq(:)

    type(ESMF_HConfigIter), intent(in)        :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    logical,            intent(out), optional :: asOkay
    integer,            intent(out), optional :: rc

    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hconfigTemp

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    hconfigTemp = ESMF_HConfigIterAsHConfig(hconfig, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ESMF_HConfigIterAsR8Seq = ESMF_HConfigAsR8Seq(hconfigTemp, &
      index=index, keyString=keyString, doc=doc, asOkay=asOkay, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigAsStringSeq()"

  function ESMF_HConfigAsStringSeq(hconfig, stringLen, keywordEnforcer, index, keyString, &
    doc, asOkay, rc)

    character(len=:), allocatable :: ESMF_HConfigAsStringSeq(:)

    type(ESMF_HConfig), intent(in)            :: hconfig
    integer,            intent(in)            :: stringLen
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    logical,            intent(out), optional :: asOkay
    integer,            intent(out), optional :: rc

    integer               :: localrc                ! local return code
    logical               :: isScalar, isSequence
    type(ESMF_HConfig)    :: hconfigTemp
    integer               :: i, size
    character(len=:), allocatable :: tempString(:)

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    isScalar = ESMF_HConfigIsScalar(hconfig, index=index, keyString=keyString, &
      doc=doc, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    if (isScalar) then
      allocate(character(len=stringLen)::tempString(1))
      tempString(1) = ESMF_HConfigAsString(hconfig, index=index, &
        keyString=keyString, doc=doc, asOkay=asOkay, rc=localrc)
      call move_alloc(tempString, ESMF_HConfigAsStringSeq)
    else
      isSequence = ESMF_HConfigIsSequence(hconfig, index=index, &
        keyString=keyString, doc=doc, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      if (isSequence) then
        ! access the sequence
        hconfigTemp = ESMF_HConfigCreateAt(hconfig, index=index, &
          keyString=keyString, doc=doc, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
        ! loop through the sequence
        size = ESMF_HConfigGetSize(hconfigTemp, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
        allocate(character(len=stringLen)::tempString(size))
        do i=1, size
          tempString(i) = ESMF_HConfigAsString(hconfigTemp, &
            index=i, asOkay=asOkay, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
          if (present(asOkay)) then
            if (.not.asOkay) exit
          endif
        enddo
        call move_alloc(tempString, ESMF_HConfigAsStringSeq)
        ! clean up
        call ESMF_HConfigDestroy(hconfigTemp, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      else
        ! this is an error condition... neither scalar and nor sequence
        call ESMF_LogSetError(ESMF_RC_ARG_INCOMP, &
          msg="Must either be scalar or sequence to use Seq interface", &
          ESMF_CONTEXT, rcToReturn=rc)
        return
      endif
    endif

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigIterAsStringSeq()"
  function ESMF_HConfigIterAsStringSeq(hconfig, stringLen, keywordEnforcer, index, keyString, &
    doc, asOkay, rc)

    character(len=:), allocatable :: ESMF_HConfigIterAsStringSeq(:)

    type(ESMF_HConfigIter), intent(in)        :: hconfig
    integer,            intent(in)            :: stringLen
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    logical,            intent(out), optional :: asOkay
    integer,            intent(out), optional :: rc

    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hconfigTemp

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    hconfigTemp = ESMF_HConfigIterAsHConfig(hconfig, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ESMF_HConfigIterAsStringSeq = ESMF_HConfigAsStringSeq(hconfigTemp, &
      stringLen=stringLen, &
      index=index, keyString=keyString, doc=doc, asOkay=asOkay, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
!BOP
! !IROUTINE: ESMF_HConfigAs<TypeSpec>SeqMapKey - Return map key value as sequence array of <Type>

! !INTERFACE:
!  function ESMF_HConfigAs<TypeSpec>SeqMapKey(hconfig, keywordEnforcer, index, keyString, &
!    doc, asOkay, rc)
! !RETURN VALUE:
!    <Type>, allocatable :: ESMF_HConfigAs<TypeSpec>SeqMapKey(:)
!
! !ARGUMENTS:
!    type(ESMF_HConfigIter), intent(in)        :: hconfig
!type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
!    integer,            intent(in),  optional :: index
!    character(*),       intent(in),  optional :: keyString
!    integer,            intent(in),  optional :: doc
!    logical,            intent(out), optional :: asOkay
!    integer,            intent(out), optional :: rc

! !DESCRIPTION:
!   Return the map key of item {\tt hconfig} interpreted as sequence of <Type>.
!   The returned value is only valid if {\tt rc == ESMF\_SUCCESS}, and, if
!   provided, {\tt asOkay == .true.}.
!
!   The supported <Type> / <TypeSpec> options are:
!   \begin{itemize}
!   \item {\tt integer(ESMF\_KIND\_I4)} / {\tt I4}
!   \item {\tt integer(ESMF\_KIND\_I8)} / {\tt I8}
!   \item {\tt logical}                 / {\tt Logical}
!   \item {\tt real(ESMF\_KIND\_R4)}    / {\tt R4}
!   \item {\tt real(ESMF\_KIND\_R8)}    / {\tt R8}
!   \item {\tt character(len=:), allocatable} / {\tt String} (See note about {\tt stringLen} argument below!)
!   \end{itemize}
!
!   An extra non-optional argument {\tt stringLen} must be provided for the
!   {\bf String} option. This argument specifies the number of characters in
!   each of the output strings. Longer actual string values are tuncated, while
!   shorter actual string values are padded with white space.
!
! The arguments are:
!   \begin{description}
!   \item[hconfig]
!     {\tt ESMF\_HConfig} object.
!   \item[{[index]}]
!     Attempt to access by index if specified. Mutural exclusive with {\tt keyString}.
!   \item[{[keyString]}]
!     Attempt to access by key string if specified. Mutural exclusive with {\tt index}.
!   \item[{[doc]}]
!     The doc index. Defaults to the first document.
!   \item[{[asOkay]}]
!     Set to {\tt .true.} for successful convertion to the requested typekind.
!     Set to {\tt .false.} otherwise. By default, i.e. without {\tt asOkay},
!     the latter condition leads to {\tt rc /= ESMF\_SUCCESS}.
!     Providing {\tt asOkay} returns {\tt rc == ESMF\_SUCCESS} in either case,
!     and the validity of the returned converted value is determined by
!     {\tt asOkay}.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigAsI4SeqMapKey()"

  function ESMF_HConfigAsI4SeqMapKey(hconfig, keywordEnforcer, index, keyString, &
    doc, asOkay, rc)

    integer(ESMF_KIND_I4), allocatable :: ESMF_HConfigAsI4SeqMapKey(:)

    type(ESMF_HConfigIter), intent(in)        :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    logical,            intent(out), optional :: asOkay
    integer,            intent(out), optional :: rc

    integer               :: localrc                ! local return code
    logical               :: isScalar, isSequence
    type(ESMF_HConfig)    :: hconfigTemp
    integer               :: i, size

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigIterGetInit, hconfig, rc)

    isScalar = ESMF_HConfigIsScalarMapKey(hconfig, index=index, &
      keyString=keyString, doc=doc, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    if (isScalar) then
      allocate(ESMF_HConfigAsI4SeqMapKey(1))
      ESMF_HConfigAsI4SeqMapKey(1) = ESMF_HConfigAsI4MapKey(hconfig, &
        index=index, keyString=keyString, doc=doc, asOkay=asOkay, rc=localrc)
    else
      isSequence = ESMF_HConfigIsSequenceMapKey(hconfig, index=index, &
        keyString=keyString, doc=doc, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      if (isSequence) then
        ! access the sequence
        hconfigTemp = ESMF_HConfigCreateAtMapKey(hconfig, index=index, &
          keyString=keyString, doc=doc, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
        ! loop through the sequence
        size = ESMF_HConfigGetSize(hconfigTemp, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
        allocate(ESMF_HConfigAsI4SeqMapKey(size))
        do i=1, size
          ESMF_HConfigAsI4SeqMapKey(i) = ESMF_HConfigAsI4(hconfigTemp, &
            index=i, asOkay=asOkay, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
          if (present(asOkay)) then
            if (.not.asOkay) exit
          endif
        enddo
        ! clean up
        call ESMF_HConfigDestroy(hconfigTemp, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      else
        ! this is an error condition... neither scalar and nor sequence
        call ESMF_LogSetError(ESMF_RC_ARG_INCOMP, &
          msg="Must either be scalar or sequence to use Seq interface", &
          ESMF_CONTEXT, rcToReturn=rc)
        return
      endif
    endif

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigAsI8SeqMapKey()"

  function ESMF_HConfigAsI8SeqMapKey(hconfig, keywordEnforcer, index, keyString, &
    doc, asOkay, rc)

    integer(ESMF_KIND_I8), allocatable :: ESMF_HConfigAsI8SeqMapKey(:)
    type(ESMF_HConfigIter), intent(in)        :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    logical,            intent(out), optional :: asOkay
    integer,            intent(out), optional :: rc

    integer               :: localrc                ! local return code
    logical               :: isScalar, isSequence
    type(ESMF_HConfig)    :: hconfigTemp
    integer               :: i, size

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigIterGetInit, hconfig, rc)

    isScalar = ESMF_HConfigIsScalarMapKey(hconfig, index=index, &
      keyString=keyString, doc=doc, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    if (isScalar) then
      allocate(ESMF_HConfigAsI8SeqMapKey(1))
      ESMF_HConfigAsI8SeqMapKey(1) = ESMF_HConfigAsI8MapKey(hconfig, &
        index=index, keyString=keyString, doc=doc, asOkay=asOkay, rc=localrc)
    else
      isSequence = ESMF_HConfigIsSequenceMapKey(hconfig, index=index, &
        keyString=keyString, doc=doc, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      if (isSequence) then
        ! access the sequence
        hconfigTemp = ESMF_HConfigCreateAtMapKey(hconfig, index=index, &
          keyString=keyString, doc=doc, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
        ! loop through the sequence
        size = ESMF_HConfigGetSize(hconfigTemp, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
        allocate(ESMF_HConfigAsI8SeqMapKey(size))
        do i=1, size
          ESMF_HConfigAsI8SeqMapKey(i) = ESMF_HConfigAsI8(hconfigTemp, &
            index=i, asOkay=asOkay, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
          if (present(asOkay)) then
            if (.not.asOkay) exit
          endif
        enddo
        ! clean up
        call ESMF_HConfigDestroy(hconfigTemp, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      else
        ! this is an error condition... neither scalar and nor sequence
        call ESMF_LogSetError(ESMF_RC_ARG_INCOMP, &
          msg="Must either be scalar or sequence to use Seq interface", &
          ESMF_CONTEXT, rcToReturn=rc)
        return
      endif
    endif

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigAsLogicalSeqMapKey()"

  function ESMF_HConfigAsLogicalSeqMapKey(hconfig, keywordEnforcer, index, keyString, &
    doc, asOkay, rc)

    logical, allocatable :: ESMF_HConfigAsLogicalSeqMapKey(:)

    type(ESMF_HConfigIter), intent(in)        :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    logical,            intent(out), optional :: asOkay
    integer,            intent(out), optional :: rc

    integer               :: localrc                ! local return code
    logical               :: isScalar, isSequence
    type(ESMF_HConfig)    :: hconfigTemp
    integer               :: i, size

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigIterGetInit, hconfig, rc)

    isScalar = ESMF_HConfigIsScalarMapKey(hconfig, index=index, &
      keyString=keyString, doc=doc, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    if (isScalar) then
      allocate(ESMF_HConfigAsLogicalSeqMapKey(1))
      ESMF_HConfigAsLogicalSeqMapKey(1) = ESMF_HConfigAsLogicalMapKey(hconfig, &
        index=index, keyString=keyString, doc=doc, asOkay=asOkay, rc=localrc)
    else
      isSequence = ESMF_HConfigIsSequenceMapKey(hconfig, index=index, &
        keyString=keyString, doc=doc, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      if (isSequence) then
        ! access the sequence
        hconfigTemp = ESMF_HConfigCreateAtMapKey(hconfig, index=index, &
          keyString=keyString, doc=doc, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
        ! loop through the sequence
        size = ESMF_HConfigGetSize(hconfigTemp, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
        allocate(ESMF_HConfigAsLogicalSeqMapKey(size))
        do i=1, size
          ESMF_HConfigAsLogicalSeqMapKey(i) = ESMF_HConfigAsLogical(hconfigTemp, &
            index=i, asOkay=asOkay, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
          if (present(asOkay)) then
            if (.not.asOkay) exit
          endif
        enddo
        ! clean up
        call ESMF_HConfigDestroy(hconfigTemp, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      else
        ! this is an error condition... neither scalar and nor sequence
        call ESMF_LogSetError(ESMF_RC_ARG_INCOMP, &
          msg="Must either be scalar or sequence to use Seq interface", &
          ESMF_CONTEXT, rcToReturn=rc)
        return
      endif
    endif

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigAsR4SeqMapKey()"

  function ESMF_HConfigAsR4SeqMapKey(hconfig, keywordEnforcer, index, keyString, &
    doc, asOkay, rc)

    real(ESMF_KIND_R4), allocatable :: ESMF_HConfigAsR4SeqMapKey(:)

    type(ESMF_HConfigIter), intent(in)        :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    logical,            intent(out), optional :: asOkay
    integer,            intent(out), optional :: rc

    integer               :: localrc                ! local return code
    logical               :: isScalar, isSequence
    type(ESMF_HConfig)    :: hconfigTemp
    integer               :: i, size

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigIterGetInit, hconfig, rc)

    isScalar = ESMF_HConfigIsScalarMapKey(hconfig, index=index, &
      keyString=keyString, doc=doc, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    if (isScalar) then
      allocate(ESMF_HConfigAsR4SeqMapKey(1))
      ESMF_HConfigAsR4SeqMapKey(1) = ESMF_HConfigAsR4MapKey(hconfig, &
        index=index, keyString=keyString, doc=doc, asOkay=asOkay, rc=localrc)
    else
      isSequence = ESMF_HConfigIsSequenceMapKey(hconfig, index=index, &
        keyString=keyString, doc=doc, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      if (isSequence) then
        ! access the sequence
        hconfigTemp = ESMF_HConfigCreateAtMapKey(hconfig, index=index, &
          keyString=keyString, doc=doc, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
        ! loop through the sequence
        size = ESMF_HConfigGetSize(hconfigTemp, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
        allocate(ESMF_HConfigAsR4SeqMapKey(size))
        do i=1, size
          ESMF_HConfigAsR4SeqMapKey(i) = ESMF_HConfigAsR4(hconfigTemp, &
            index=i, asOkay=asOkay, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
          if (present(asOkay)) then
            if (.not.asOkay) exit
          endif
        enddo
        ! clean up
        call ESMF_HConfigDestroy(hconfigTemp, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      else
        ! this is an error condition... neither scalar and nor sequence
        call ESMF_LogSetError(ESMF_RC_ARG_INCOMP, &
          msg="Must either be scalar or sequence to use Seq interface", &
          ESMF_CONTEXT, rcToReturn=rc)
        return
      endif
    endif

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigAsR8SeqMapKey()"

  function ESMF_HConfigAsR8SeqMapKey(hconfig, keywordEnforcer, index, keyString, &
    doc, asOkay, rc)

    real(ESMF_KIND_R8), allocatable :: ESMF_HConfigAsR8SeqMapKey(:)

    type(ESMF_HConfigIter), intent(in)        :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    logical,            intent(out), optional :: asOkay
    integer,            intent(out), optional :: rc

    integer               :: localrc                ! local return code
    logical               :: isScalar, isSequence
    type(ESMF_HConfig)    :: hconfigTemp
    integer               :: i, size

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigIterGetInit, hconfig, rc)

    isScalar = ESMF_HConfigIsScalarMapKey(hconfig, index=index, &
      keyString=keyString, doc=doc, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    if (isScalar) then
      allocate(ESMF_HConfigAsR8SeqMapKey(1))
      ESMF_HConfigAsR8SeqMapKey(1) = ESMF_HConfigAsR8MapKey(hconfig, &
        index=index, keyString=keyString, doc=doc, asOkay=asOkay, rc=localrc)
    else
      isSequence = ESMF_HConfigIsSequenceMapKey(hconfig, index=index, &
        keyString=keyString, doc=doc, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      if (isSequence) then
        ! access the sequence
        hconfigTemp = ESMF_HConfigCreateAtMapKey(hconfig, index=index, &
          keyString=keyString, doc=doc, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
        ! loop through the sequence
        size = ESMF_HConfigGetSize(hconfigTemp, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
        allocate(ESMF_HConfigAsR8SeqMapKey(size))
        do i=1, size
          ESMF_HConfigAsR8SeqMapKey(i) = ESMF_HConfigAsR8(hconfigTemp, &
            index=i, asOkay=asOkay, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
          if (present(asOkay)) then
            if (.not.asOkay) exit
          endif
        enddo
        ! clean up
        call ESMF_HConfigDestroy(hconfigTemp, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      else
        ! this is an error condition... neither scalar and nor sequence
        call ESMF_LogSetError(ESMF_RC_ARG_INCOMP, &
          msg="Must either be scalar or sequence to use Seq interface", &
          ESMF_CONTEXT, rcToReturn=rc)
        return
      endif
    endif

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigAsStringSeqMapKey()"

  function ESMF_HConfigAsStringSeqMapKey(hconfig, stringLen, keywordEnforcer, index, keyString, &
    doc, asOkay, rc)

    character(len=:), allocatable :: ESMF_HConfigAsStringSeqMapKey(:)

    type(ESMF_HConfigIter), intent(in)        :: hconfig
    integer,            intent(in)            :: stringLen
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    logical,            intent(out), optional :: asOkay
    integer,            intent(out), optional :: rc

    integer               :: localrc                ! local return code
    logical               :: isScalar, isSequence
    type(ESMF_HConfig)    :: hconfigTemp
    integer               :: i, size
    character(len=:), allocatable :: tempString(:)

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigIterGetInit, hconfig, rc)

    isScalar = ESMF_HConfigIsScalarMapKey(hconfig, index=index, &
      keyString=keyString, doc=doc, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    if (isScalar) then
      allocate(character(len=stringLen)::tempString(1))
      tempString(1) = ESMF_HConfigAsStringMapKey(hconfig, &
        index=index, keyString=keyString, doc=doc, asOkay=asOkay, rc=localrc)
      call move_alloc(tempString, ESMF_HConfigAsStringSeqMapKey)
    else
      isSequence = ESMF_HConfigIsSequenceMapKey(hconfig, index=index, &
        keyString=keyString, doc=doc, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      if (isSequence) then
        ! access the sequence
        hconfigTemp = ESMF_HConfigCreateAtMapKey(hconfig, index=index, &
          keyString=keyString, doc=doc, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
        ! loop through the sequence
        size = ESMF_HConfigGetSize(hconfigTemp, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
        allocate(character(len=stringLen)::tempString(size))
        do i=1, size
          tempString(i) = ESMF_HConfigAsString(hconfigTemp, &
            index=i, asOkay=asOkay, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
          if (present(asOkay)) then
            if (.not.asOkay) exit
          endif
        enddo
        call move_alloc(tempString, ESMF_HConfigAsStringSeqMapKey)
        ! clean up
        call ESMF_HConfigDestroy(hconfigTemp, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      else
        ! this is an error condition... neither scalar and nor sequence
        call ESMF_LogSetError(ESMF_RC_ARG_INCOMP, &
          msg="Must either be scalar or sequence to use Seq interface", &
          ESMF_CONTEXT, rcToReturn=rc)
        return
      endif
    endif

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
!BOP
! !IROUTINE: ESMF_HConfigAs<TypeSpec>SeqMapVal - Return map value as sequence array of <Type>

! !INTERFACE:
!  function ESMF_HConfigAs<TypeSpec>SeqMapVal(hconfig, keywordEnforcer, index, keyString, &
!    doc, asOkay, rc)
! !RETURN VALUE:
!    <Type>, allocatable :: ESMF_HConfigAs<TypeSpec>SeqMapVal(:)
!
! !ARGUMENTS:
!    type(ESMF_HConfigIter), intent(in)        :: hconfig
!type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
!    integer,            intent(in),  optional :: index
!    character(*),       intent(in),  optional :: keyString
!    integer,            intent(in),  optional :: doc
!    logical,            intent(out), optional :: asOkay
!    integer,            intent(out), optional :: rc

! !DESCRIPTION:
!   Return the map value of item {\tt hconfig} interpreted as sequence of <Type>.
!   The returned value is only valid if {\tt rc == ESMF\_SUCCESS}, and, if
!   provided, {\tt asOkay == .true.}.
!
!   The supported <Type> / <TypeSpec> options are:
!   \begin{itemize}
!   \item {\tt integer(ESMF\_KIND\_I4)} / {\tt I4}
!   \item {\tt integer(ESMF\_KIND\_I8)} / {\tt I8}
!   \item {\tt logical}                 / {\tt Logical}
!   \item {\tt real(ESMF\_KIND\_R4)}    / {\tt R4}
!   \item {\tt real(ESMF\_KIND\_R8)}    / {\tt R8}
!   \item {\tt character(len=:), allocatable} / {\tt String} (See note about {\tt stringLen} argument below!)
!   \end{itemize}
!
!   An extra non-optional argument {\tt stringLen} must be provided for the
!   {\bf String} option. This argument specifies the number of characters in
!   each of the output strings. Longer actual string values are tuncated, while
!   shorter actual string values are padded with white space.
!
! The arguments are:
!   \begin{description}
!   \item[hconfig]
!     {\tt ESMF\_HConfigIter} object.
!   \item[{[index]}]
!     Attempt to access by index if specified. Mutural exclusive with {\tt keyString}.
!   \item[{[keyString]}]
!     Attempt to access by key string if specified. Mutural exclusive with {\tt index}.
!   \item[{[doc]}]
!     The doc index. Defaults to the first document.
!   \item[{[asOkay]}]
!     Set to {\tt .true.} for successful convertion to the requested typekind.
!     Set to {\tt .false.} otherwise. By default, i.e. without {\tt asOkay},
!     the latter condition leads to {\tt rc /= ESMF\_SUCCESS}.
!     Providing {\tt asOkay} returns {\tt rc == ESMF\_SUCCESS} in either case,
!     and the validity of the returned converted value is determined by
!     {\tt asOkay}.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigAsI4SeqMapVal()"

  function ESMF_HConfigAsI4SeqMapVal(hconfig, keywordEnforcer, index, keyString, &
    doc, asOkay, rc)

    integer(ESMF_KIND_I4), allocatable :: ESMF_HConfigAsI4SeqMapVal(:)

    type(ESMF_HConfigIter), intent(in)        :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    logical,            intent(out), optional :: asOkay
    integer,            intent(out), optional :: rc

    integer               :: localrc                ! local return code
    logical               :: isScalar, isSequence
    type(ESMF_HConfig)    :: hconfigTemp
    integer               :: i, size

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigIterGetInit, hconfig, rc)

    isScalar = ESMF_HConfigIsScalarMapVal(hconfig, index=index, &
      keyString=keyString, doc=doc, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    if (isScalar) then
      allocate(ESMF_HConfigAsI4SeqMapVal(1))
      ESMF_HConfigAsI4SeqMapVal(1) = ESMF_HConfigAsI4MapVal(hconfig, &
        index=index, keyString=keyString, doc=doc, asOkay=asOkay, rc=localrc)
    else
      isSequence = ESMF_HConfigIsSequenceMapVal(hconfig, index=index, &
        keyString=keyString, doc=doc, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      if (isSequence) then
        ! access the sequence
        hconfigTemp = ESMF_HConfigCreateAtMapVal(hconfig, index=index, &
          keyString=keyString, doc=doc, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
        ! loop through the sequence
        size = ESMF_HConfigGetSize(hconfigTemp, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
        allocate(ESMF_HConfigAsI4SeqMapVal(size))
        do i=1, size
          ESMF_HConfigAsI4SeqMapVal(i) = ESMF_HConfigAsI4(hconfigTemp, &
            index=i, asOkay=asOkay, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
          if (present(asOkay)) then
            if (.not.asOkay) exit
          endif
        enddo
        ! clean up
        call ESMF_HConfigDestroy(hconfigTemp, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      else
        ! this is an error condition... neither scalar and nor sequence
        call ESMF_LogSetError(ESMF_RC_ARG_INCOMP, &
          msg="Must either be scalar or sequence to use Seq interface", &
          ESMF_CONTEXT, rcToReturn=rc)
        return
      endif
    endif

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigAsI8SeqMapVal()"

  function ESMF_HConfigAsI8SeqMapVal(hconfig, keywordEnforcer, index, keyString, &
    doc, asOkay, rc)

    integer(ESMF_KIND_I8), allocatable :: ESMF_HConfigAsI8SeqMapVal(:)

    type(ESMF_HConfigIter), intent(in)        :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    logical,            intent(out), optional :: asOkay
    integer,            intent(out), optional :: rc

    integer               :: localrc                ! local return code
    logical               :: isScalar, isSequence
    type(ESMF_HConfig)    :: hconfigTemp
    integer               :: i, size

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigIterGetInit, hconfig, rc)

    isScalar = ESMF_HConfigIsScalarMapVal(hconfig, index=index, &
      keyString=keyString, doc=doc, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    if (isScalar) then
      allocate(ESMF_HConfigAsI8SeqMapVal(1))
      ESMF_HConfigAsI8SeqMapVal(1) = ESMF_HConfigAsI8MapVal(hconfig, &
        index=index, keyString=keyString, doc=doc, asOkay=asOkay, rc=localrc)
    else
      isSequence = ESMF_HConfigIsSequenceMapVal(hconfig, index=index, &
        keyString=keyString, doc=doc, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      if (isSequence) then
        ! access the sequence
        hconfigTemp = ESMF_HConfigCreateAtMapVal(hconfig, index=index, &
          keyString=keyString, doc=doc, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
        ! loop through the sequence
        size = ESMF_HConfigGetSize(hconfigTemp, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
        allocate(ESMF_HConfigAsI8SeqMapVal(size))
        do i=1, size
          ESMF_HConfigAsI8SeqMapVal(i) = ESMF_HConfigAsI8(hconfigTemp, &
            index=i, asOkay=asOkay, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
          if (present(asOkay)) then
            if (.not.asOkay) exit
          endif
        enddo
        ! clean up
        call ESMF_HConfigDestroy(hconfigTemp, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      else
        ! this is an error condition... neither scalar and nor sequence
        call ESMF_LogSetError(ESMF_RC_ARG_INCOMP, &
          msg="Must either be scalar or sequence to use Seq interface", &
          ESMF_CONTEXT, rcToReturn=rc)
        return
      endif
    endif

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigAsLogicalSeqMapVal()"

  function ESMF_HConfigAsLogicalSeqMapVal(hconfig, keywordEnforcer, index, keyString, &
    doc, asOkay, rc)

    logical, allocatable :: ESMF_HConfigAsLogicalSeqMapVal(:)

    type(ESMF_HConfigIter), intent(in)        :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    logical,            intent(out), optional :: asOkay
    integer,            intent(out), optional :: rc

    integer               :: localrc                ! local return code
    logical               :: isScalar, isSequence
    type(ESMF_HConfig)    :: hconfigTemp
    integer               :: i, size

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigIterGetInit, hconfig, rc)

    isScalar = ESMF_HConfigIsScalarMapVal(hconfig, index=index, &
      keyString=keyString, doc=doc, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    if (isScalar) then
      allocate(ESMF_HConfigAsLogicalSeqMapVal(1))
      ESMF_HConfigAsLogicalSeqMapVal(1) = ESMF_HConfigAsLogicalMapVal(hconfig, &
        index=index, keyString=keyString, doc=doc, asOkay=asOkay, rc=localrc)
    else
      isSequence = ESMF_HConfigIsSequenceMapVal(hconfig, index=index, &
        keyString=keyString, doc=doc, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      if (isSequence) then
        ! access the sequence
        hconfigTemp = ESMF_HConfigCreateAtMapVal(hconfig, index=index, &
          keyString=keyString, doc=doc, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
        ! loop through the sequence
        size = ESMF_HConfigGetSize(hconfigTemp, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
        allocate(ESMF_HConfigAsLogicalSeqMapVal(size))
        do i=1, size
          ESMF_HConfigAsLogicalSeqMapVal(i) = ESMF_HConfigAsLogical(hconfigTemp, &
            index=i, asOkay=asOkay, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
          if (present(asOkay)) then
            if (.not.asOkay) exit
          endif
        enddo
        ! clean up
        call ESMF_HConfigDestroy(hconfigTemp, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      else
        ! this is an error condition... neither scalar and nor sequence
        call ESMF_LogSetError(ESMF_RC_ARG_INCOMP, &
          msg="Must either be scalar or sequence to use Seq interface", &
          ESMF_CONTEXT, rcToReturn=rc)
        return
      endif
    endif

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigAsR4SeqMapVal()"

  function ESMF_HConfigAsR4SeqMapVal(hconfig, keywordEnforcer, index, keyString, &
    doc, asOkay, rc)

    real(ESMF_KIND_R4), allocatable :: ESMF_HConfigAsR4SeqMapVal(:)

    type(ESMF_HConfigIter), intent(in)        :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    logical,            intent(out), optional :: asOkay
    integer,            intent(out), optional :: rc

    integer               :: localrc                ! local return code
    logical               :: isScalar, isSequence
    type(ESMF_HConfig)    :: hconfigTemp
    integer               :: i, size

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigIterGetInit, hconfig, rc)

    isScalar = ESMF_HConfigIsScalarMapVal(hconfig, index=index, &
      keyString=keyString, doc=doc, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    if (isScalar) then
      allocate(ESMF_HConfigAsR4SeqMapVal(1))
      ESMF_HConfigAsR4SeqMapVal(1) = ESMF_HConfigAsR4MapVal(hconfig, &
        index=index, keyString=keyString, doc=doc, asOkay=asOkay, rc=localrc)
    else
      isSequence = ESMF_HConfigIsSequenceMapVal(hconfig, index=index, &
        keyString=keyString, doc=doc, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      if (isSequence) then
        ! access the sequence
        hconfigTemp = ESMF_HConfigCreateAtMapVal(hconfig, index=index, &
          keyString=keyString, doc=doc, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
        ! loop through the sequence
        size = ESMF_HConfigGetSize(hconfigTemp, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
        allocate(ESMF_HConfigAsR4SeqMapVal(size))
        do i=1, size
          ESMF_HConfigAsR4SeqMapVal(i) = ESMF_HConfigAsR4(hconfigTemp, &
            index=i, asOkay=asOkay, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
          if (present(asOkay)) then
            if (.not.asOkay) exit
          endif
        enddo
        ! clean up
        call ESMF_HConfigDestroy(hconfigTemp, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      else
        ! this is an error condition... neither scalar and nor sequence
        call ESMF_LogSetError(ESMF_RC_ARG_INCOMP, &
          msg="Must either be scalar or sequence to use Seq interface", &
          ESMF_CONTEXT, rcToReturn=rc)
        return
      endif
    endif

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigAsR8SeqMapVal()"

  function ESMF_HConfigAsR8SeqMapVal(hconfig, keywordEnforcer, index, keyString, &
    doc, asOkay, rc)

    real(ESMF_KIND_R8), allocatable :: ESMF_HConfigAsR8SeqMapVal(:)

    type(ESMF_HConfigIter), intent(in)        :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    logical,            intent(out), optional :: asOkay
    integer,            intent(out), optional :: rc

    integer               :: localrc                ! local return code
    logical               :: isScalar, isSequence
    type(ESMF_HConfig)    :: hconfigTemp
    integer               :: i, size

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigIterGetInit, hconfig, rc)

    isScalar = ESMF_HConfigIsScalarMapVal(hconfig, index=index, &
      keyString=keyString, doc=doc, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    if (isScalar) then
      allocate(ESMF_HConfigAsR8SeqMapVal(1))
      ESMF_HConfigAsR8SeqMapVal(1) = ESMF_HConfigAsR8MapVal(hconfig, &
        index=index, keyString=keyString, doc=doc, asOkay=asOkay, rc=localrc)
    else
      isSequence = ESMF_HConfigIsSequenceMapVal(hconfig, index=index, &
        keyString=keyString, doc=doc, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      if (isSequence) then
        ! access the sequence
        hconfigTemp = ESMF_HConfigCreateAtMapVal(hconfig, index=index, &
          keyString=keyString, doc=doc, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
        ! loop through the sequence
        size = ESMF_HConfigGetSize(hconfigTemp, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
        allocate(ESMF_HConfigAsR8SeqMapVal(size))
        do i=1, size
          ESMF_HConfigAsR8SeqMapVal(i) = ESMF_HConfigAsR8(hconfigTemp, &
            index=i, asOkay=asOkay, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
          if (present(asOkay)) then
            if (.not.asOkay) exit
          endif
        enddo
        ! clean up
        call ESMF_HConfigDestroy(hconfigTemp, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      else
        ! this is an error condition... neither scalar and nor sequence
        call ESMF_LogSetError(ESMF_RC_ARG_INCOMP, &
          msg="Must either be scalar or sequence to use Seq interface", &
          ESMF_CONTEXT, rcToReturn=rc)
        return
      endif
    endif

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigAsStringSeqMapVal()"

  function ESMF_HConfigAsStringSeqMapVal(hconfig, stringLen, keywordEnforcer, index, keyString, &
    doc, asOkay, rc)

    character(len=:), allocatable :: ESMF_HConfigAsStringSeqMapVal(:)

    type(ESMF_HConfigIter), intent(in)        :: hconfig
    integer,            intent(in)            :: stringLen
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    logical,            intent(out), optional :: asOkay
    integer,            intent(out), optional :: rc

    integer               :: localrc                ! local return code
    logical               :: isScalar, isSequence
    type(ESMF_HConfig)    :: hconfigTemp
    integer               :: i, size
    character(len=:), allocatable :: tempString(:)

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigIterGetInit, hconfig, rc)

    isScalar = ESMF_HConfigIsScalarMapVal(hconfig, index=index, &
      keyString=keyString, doc=doc, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    if (isScalar) then
      allocate(character(len=stringLen)::tempString(1))
      tempString(1) = ESMF_HConfigAsStringMapVal(hconfig, &
        index=index, keyString=keyString, doc=doc, asOkay=asOkay, rc=localrc)
      call move_alloc(tempString, ESMF_HConfigAsStringSeqMapVal)
    else
      isSequence = ESMF_HConfigIsSequenceMapVal(hconfig, index=index, &
        keyString=keyString, doc=doc, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      if (isSequence) then
        ! access the sequence
        hconfigTemp = ESMF_HConfigCreateAtMapVal(hconfig, index=index, &
          keyString=keyString, doc=doc, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
        ! loop through the sequence
        size = ESMF_HConfigGetSize(hconfigTemp, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
        allocate(character(len=stringLen)::tempString(size))
        do i=1, size
          tempString(i) = ESMF_HConfigAsString(hconfigTemp, &
            index=i, asOkay=asOkay, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
          if (present(asOkay)) then
            if (.not.asOkay) exit
          endif
        enddo
        call move_alloc(tempString, ESMF_HConfigAsStringSeqMapVal)
        ! clean up
        call ESMF_HConfigDestroy(hconfigTemp, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      else
        ! this is an error condition... neither scalar and nor sequence
        call ESMF_LogSetError(ESMF_RC_ARG_INCOMP, &
          msg="Must either be scalar or sequence to use Seq interface", &
          ESMF_CONTEXT, rcToReturn=rc)
        return
      endif
    endif

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigCreateDefault()"
!BOP
! !IROUTINE: ESMF_HConfigCreate - Create HConfig object from YAML string or file

! !INTERFACE:
  ! Private name; call using ESMF_HConfigCreate()
  function ESMF_HConfigCreateDefault(keywordEnforcer, content, filename, rc)
!
! !RETURN VALUE:
    type(ESMF_HConfig) :: ESMF_HConfigCreateDefault
!
! !ARGUMENTS:
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    character(len=*),   intent(in),  optional :: content
    character(len=*),   intent(in),  optional :: filename
    integer,            intent(out), optional :: rc
!
! !DESCRIPTION:
!   Create a new HConfig object. The object is empty unless either the
!   {\tt content} or {\tt filename} argument is specified.
!
!   The arguments are:
!   \begin{description}
!   \item[{[content]}]
!     String containing the YAML text. Mutually exclusive with
!     {\tt filename} argument.
!   \item[{[filename]}]
!     Name of the YAML file to be loaded. Mutually exclusive with
!     {\tt content} argument.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc        ! local return code
    type(ESMF_HConfig)    :: hconfig        ! opaque pointer to new C++ HConfig

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! invalidate return value
    hconfig%shallowMemory = 0
    ESMF_HConfigCreateDefault = hconfig

    if (present(content) .and. present(filename)) then
      call ESMF_LogSetError(ESMF_RC_ARG_INCOMP, &
        msg="The 'content' and 'filename' arguments are mutual exclusive", &
        ESMF_CONTEXT, rcToReturn=rc)
      return
    endif

    ! call into the C++ interface, which will sort out optional arguments
    call c_ESMC_HConfigCreate(hconfig, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Set return value
    ESMF_HConfigCreateDefault = hconfig

    ! Set init code
    ESMF_INIT_SET_CREATED(ESMF_HConfigCreateDefault)

    ! handle content and filename
    if (present(content)) then
      ! load content
      call ESMF_HConfigLoad(ESMF_HConfigCreateDefault, content=content, &
        rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    else if (present(filename)) then
      ! load filename
      call ESMF_HConfigFileLoad(ESMF_HConfigCreateDefault, filename=filename, &
        rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function ESMF_HConfigCreateDefault
!------------------------------------------------------------------------------



! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigCreateHConfig()"
!BOP
! !IROUTINE: ESMF_HConfigCreate - Create HConfig object from HConfig object

! !INTERFACE:
  ! Private name; call using ESMF_HConfigCreate()
  function ESMF_HConfigCreateHConfig(content, keywordEnforcer, rc)
!
! !RETURN VALUE:
    type(ESMF_HConfig) :: ESMF_HConfigCreateHConfig
!
! !ARGUMENTS:
    type(ESMF_HConfig), intent(in)            :: content
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(out), optional :: rc
!
! !DESCRIPTION:
!   Create a new HConfig object from existing HConfig object as a deep copy.
!
!   The arguments are:
!   \begin{description}
!   \item[content]
!     HConfig content.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc        ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ESMF_HConfigCreateHConfig = ESMF_HConfigCreate(rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_HConfigSet(ESMF_HConfigCreateHConfig, content, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function ESMF_HConfigCreateHConfig
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
!BOP
! !IROUTINE: ESMF_HConfigCreate - Create HConfig object from <Type> content

! !INTERFACE:
!  function ESMF_HConfigCreate(content, keywordEnforcer, rc)
!
! !RETURN VALUE:
!    type(ESMF_HConfig) :: ESMF_HConfigCreate
!
! !ARGUMENTS:
!    <Type>,  intent(in)            :: content[(:)]
!type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
!    integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!   Create a new HConfig object from content of type <Type>. All <Type> options
!   support the sequence array variant {\tt (:)} in addition to the scalar
!   variant.
!
!   The supported <Type> options are:
!   \begin{itemize}
!   \item {\tt integer(ESMF\_KIND\_I4)}
!   \item {\tt integer(ESMF\_KIND\_I8)}
!   \item {\tt logical}
!   \item {\tt real(ESMF\_KIND\_R4)}
!   \item {\tt real(ESMF\_KIND\_R8)}
!   \end{itemize}
!
!   The arguments are:
!   \begin{description}
!   \item[content]
!     Content of type <Type>.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigCreateI4()"

  function ESMF_HConfigCreateI4(content, keywordEnforcer, rc)

    type(ESMF_HConfig) :: ESMF_HConfigCreateI4

    integer(ESMF_KIND_I4),  intent(in)            :: content
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,                intent(out), optional :: rc

    integer               :: localrc        ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! invalidate return value
    ESMF_HConfigCreateI4%shallowMemory = 0

    ! call into the C++ interface, which will sort out optional arguments
    call c_ESMC_HConfigCreateI4(ESMF_HConfigCreateI4, content, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Set init code
    ESMF_INIT_SET_CREATED(ESMF_HConfigCreateI4)

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function ESMF_HConfigCreateI4
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigCreateI4Seq()"

  function ESMF_HConfigCreateI4Seq(content, keywordEnforcer, rc)

    type(ESMF_HConfig) :: ESMF_HConfigCreateI4Seq

    integer(ESMF_KIND_I4),  intent(in)            :: content(:)
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,                intent(out), optional :: rc

    integer               :: localrc        ! local return code
    integer               :: count

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! invalidate return value
    ESMF_HConfigCreateI4Seq%shallowMemory = 0

    count = size(content)
    if (count>0) then
      ! call into the C++ interface, which will sort out optional arguments
      call c_ESMC_HConfigCreateI4Seq(ESMF_HConfigCreateI4Seq, content(1), &
        count, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      ! Set init code
      ESMF_INIT_SET_CREATED(ESMF_HConfigCreateI4Seq)
    else
      ! empty hconfig
      ESMF_HConfigCreateI4Seq = ESMF_HConfigCreate(rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function ESMF_HConfigCreateI4Seq
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigCreateI8()"

  function ESMF_HConfigCreateI8(content, keywordEnforcer, rc)

    type(ESMF_HConfig) :: ESMF_HConfigCreateI8

    integer(ESMF_KIND_I8),  intent(in)            :: content
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,                intent(out), optional :: rc

    integer               :: localrc        ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! invalidate return value
    ESMF_HConfigCreateI8%shallowMemory = 0

    ! call into the C++ interface, which will sort out optional arguments
    call c_ESMC_HConfigCreateI8(ESMF_HConfigCreateI8, content, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Set init code
    ESMF_INIT_SET_CREATED(ESMF_HConfigCreateI8)

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function ESMF_HConfigCreateI8
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigCreateI8Seq()"

  function ESMF_HConfigCreateI8Seq(content, keywordEnforcer, rc)

    type(ESMF_HConfig) :: ESMF_HConfigCreateI8Seq

    integer(ESMF_KIND_I8),  intent(in)            :: content(:)
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,                intent(out), optional :: rc

    integer               :: localrc        ! local return code
    integer               :: count

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! invalidate return value
    ESMF_HConfigCreateI8Seq%shallowMemory = 0

    count = size(content)
    if (count>0) then
      ! call into the C++ interface, which will sort out optional arguments
      call c_ESMC_HConfigCreateI8Seq(ESMF_HConfigCreateI8Seq, content(1), &
        count, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      ! Set init code
      ESMF_INIT_SET_CREATED(ESMF_HConfigCreateI8Seq)
    else
      ! empty hconfig
      ESMF_HConfigCreateI8Seq = ESMF_HConfigCreate(rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function ESMF_HConfigCreateI8Seq
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigCreateLogical()"

  function ESMF_HConfigCreateLogical(content, keywordEnforcer, rc)

    type(ESMF_HConfig) :: ESMF_HConfigCreateLogical

    logical,            intent(in)            :: content
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(out), optional :: rc

    integer                       :: localrc        ! local return code
    character(len=:), allocatable :: sContent

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    sContent = "False"
    if (content) sContent = "True"

    ESMF_HConfigCreateLogical = ESMF_HConfigCreate(content=sContent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function ESMF_HConfigCreateLogical
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigCreateLogicalSeq()"

  function ESMF_HConfigCreateLogicalSeq(content, keywordEnforcer, rc)

    type(ESMF_HConfig) :: ESMF_HConfigCreateLogicalSeq

    logical,            intent(in)            :: content(:)
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(out), optional :: rc

    integer                       :: localrc        ! local return code
    character(len=:), allocatable :: sContent
    integer                       :: count, i

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! start with empty hconfig
    ESMF_HConfigCreateLogicalSeq = ESMF_HConfigCreate(rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    count = size(content)
    do i=1, count
      ! add this element of the sequence
      sContent = "False"
      if (content(i)) sContent = "True"
      call ESMF_HConfigAdd(ESMF_HConfigCreateLogicalSeq, sContent, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    enddo

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function ESMF_HConfigCreateLogicalSeq
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigCreateR4()"

  function ESMF_HConfigCreateR4(content, keywordEnforcer, rc)

    type(ESMF_HConfig) :: ESMF_HConfigCreateR4

    real(ESMF_KIND_R4),  intent(in)            :: content
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,             intent(out), optional :: rc

    integer               :: localrc        ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! invalidate return value
    ESMF_HConfigCreateR4%shallowMemory = 0

    ! call into the C++ interface, which will sort out optional arguments
    call c_ESMC_HConfigCreateR4(ESMF_HConfigCreateR4, content, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Set init code
    ESMF_INIT_SET_CREATED(ESMF_HConfigCreateR4)

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function ESMF_HConfigCreateR4
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigCreateR4Seq()"

  function ESMF_HConfigCreateR4Seq(content, keywordEnforcer, rc)

    type(ESMF_HConfig) :: ESMF_HConfigCreateR4Seq

    real(ESMF_KIND_R4),     intent(in)            :: content(:)
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,                intent(out), optional :: rc

    integer               :: localrc        ! local return code
    integer               :: count

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! invalidate return value
    ESMF_HConfigCreateR4Seq%shallowMemory = 0

    count = size(content)
    if (count>0) then
      ! call into the C++ interface, which will sort out optional arguments
      call c_ESMC_HConfigCreateR4Seq(ESMF_HConfigCreateR4Seq, content(1), &
        count, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      ! Set init code
      ESMF_INIT_SET_CREATED(ESMF_HConfigCreateR4Seq)
    else
      ! empty hconfig
      ESMF_HConfigCreateR4Seq = ESMF_HConfigCreate(rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function ESMF_HConfigCreateR4Seq
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigCreateR8()"

  function ESMF_HConfigCreateR8(content, keywordEnforcer, rc)

    type(ESMF_HConfig) :: ESMF_HConfigCreateR8

    real(ESMF_KIND_R8),  intent(in)            :: content
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,             intent(out), optional :: rc

    integer               :: localrc        ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! invalidate return value
    ESMF_HConfigCreateR8%shallowMemory = 0

    ! call into the C++ interface, which will sort out optional arguments
    call c_ESMC_HConfigCreateR8(ESMF_HConfigCreateR8, content, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Set init code
    ESMF_INIT_SET_CREATED(ESMF_HConfigCreateR8)

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function ESMF_HConfigCreateR8
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigCreateR8Seq()"

  function ESMF_HConfigCreateR8Seq(content, keywordEnforcer, rc)

    type(ESMF_HConfig) :: ESMF_HConfigCreateR8Seq

    real(ESMF_KIND_R8),     intent(in)            :: content(:)
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,                intent(out), optional :: rc

    integer               :: localrc        ! local return code
    integer               :: count

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! invalidate return value
    ESMF_HConfigCreateR8Seq%shallowMemory = 0

    count = size(content)
    if (count>0) then
      ! call into the C++ interface, which will sort out optional arguments
      call c_ESMC_HConfigCreateR8Seq(ESMF_HConfigCreateR8Seq, content(1), &
        count, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      ! Set init code
      ESMF_INIT_SET_CREATED(ESMF_HConfigCreateR8Seq)
    else
      ! empty hconfig
      ESMF_HConfigCreateR8Seq = ESMF_HConfigCreate(rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function ESMF_HConfigCreateR8Seq
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigCreateStringSeq()"
!BOP
! !IROUTINE: ESMF_HConfigCreate - Create HConfig object from String sequence array

! !INTERFACE:
  ! Private name; call using ESMF_HConfigCreate()
  function ESMF_HConfigCreateStringSeq(content, keywordEnforcer, rc)
!
! !RETURN VALUE:
    type(ESMF_HConfig) :: ESMF_HConfigCreateStringSeq
!
! !ARGUMENTS:
    character(len=*),   intent(in)            :: content(:)
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(out), optional :: rc
!
! !DESCRIPTION:
!   Create a new HConfig object.
!
!   The arguments are:
!   \begin{description}
!   \item[content]
!     String content.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer                       :: localrc        ! local return code
    integer                       :: count, i

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! start with empty hconfig
    ESMF_HConfigCreateStringSeq = ESMF_HConfigCreate(rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    count = size(content)
    do i=1, count
      ! add this element of the sequence
      call ESMF_HConfigAdd(ESMF_HConfigCreateStringSeq, content(i), rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    enddo

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function ESMF_HConfigCreateStringSeq
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
!BOP
! !IROUTINE: ESMF_HConfigCreateAt - Return HConfig object at location

! !INTERFACE:
!  function ESMF_HConfigCreateAt(hconfig, keywordEnforcer, index, key, &
!    keyString, doc, rc)
!
! !RETURN VALUE:
!    type(ESMF_HConfig) :: ESMF_HConfigCreateAt
!
! !ARGUMENTS:
!    type(ESMF_HConfig[Iter]), intent(in)      :: hconfig
!type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
!    integer,            intent(in),  optional :: index
!    type(ESMF_HConfig), intent(in),  optional :: key
!    character(*),       intent(in),  optional :: keyString
!    integer,            intent(in),  optional :: doc
!    integer,            intent(out), optional :: rc
!
! !DESCRIPTION:
!   Create a new HConfig object at the current iteration, or
!   as specified by {\tt index}, {\tt key} or {\tt keyString}.
!   The {\tt hconfig} must {\em not} be a map iterator.
!
!   The arguments are:
!   \begin{description}
!   \item[hconfig]
!     {\tt ESMF\_HConfig} or {\tt ESMF\_HConfigIter} object.
!   \item[{[index]}]
!     Attempt to access by index if specified. Mutural exclusive with
!     {\tt key} and {\tt keyString}.
!   \item[{[key]}]
!     Attempt to access by key if specified. Mutural exclusive with
!     {\tt index} and {\tt keyString},
!   \item[{[keyString]}]
!     Attempt to access by key string if specified. Mutural exclusive with
!     {\tt index} and {\tt key}.
!   \item[{[doc]}]
!     The doc index. Defaults to the first document.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigCreateAt()"

  function ESMF_HConfigCreateAt(hconfig, keywordEnforcer, index, key, &
    keyString, doc, rc)

    type(ESMF_HConfig) :: ESMF_HConfigCreateAt

    type(ESMF_HConfig), intent(in)            :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    type(ESMF_HConfig), intent(in),  optional :: key
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    integer,            intent(out), optional :: rc

    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hKey

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! invalidate return value
    ESMF_HConfigCreateAt%shallowMemory = 0

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    ! Check mutual exclusions
    if ((present(index) .and. (present(key) .or. present(keyString))) &
      .or. (present(key) .and. present(keyString))) then
      call ESMF_LogSetError(ESMF_RC_ARG_INCOMP, &
        msg="The 'index', 'key', and 'keyString' arguments are mutual exclusive", &
        ESMF_CONTEXT, rcToReturn=rc)
      return
    endif

    if (present(key) .or. present(keyString)) then
      if (present(keyString)) then
        hkey = ESMF_HConfigCreate(content=keyString, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      else
        ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, key, rc)
        hkey = key
      endif
      ! Call into the C++ interface, which will sort out optional arguments.
      call c_ESMC_HConfigCreateAtKey(hconfig, ESMF_HConfigCreateAt, &
        hkey, doc, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      if (present(keyString)) then
        call ESMF_HConfigDestroy(hkey, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      endif
    else
      ! Call into the C++ interface, which will sort out optional arguments.
      call c_ESMC_HConfigCreateAt(hconfig, ESMF_HConfigCreateAt, &
        index, doc, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    ! Set init code
    ESMF_INIT_SET_CREATED(ESMF_HConfigCreateAt)

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigIterCreateAt()"

  function ESMF_HConfigIterCreateAt(hconfig, keywordEnforcer, index, key, &
    keyString, doc, rc)

    type(ESMF_HConfig) :: ESMF_HConfigIterCreateAt

    type(ESMF_HConfigIter), intent(in)        :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    type(ESMF_HConfig), intent(in),  optional :: key
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    integer,            intent(out), optional :: rc

    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hconfigTemp

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    hconfigTemp = ESMF_HConfigIterAsHConfig(hconfig, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ESMF_HConfigIterCreateAt = ESMF_HConfigCreateAt(hconfigTemp, &
      index=index, key=key, keyString=keyString, doc=doc, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigCreateAtMapKey()"
!BOP
! !IROUTINE: ESMF_HConfigCreateAtMapKey - Return HConfig object at location

! !INTERFACE:
  function ESMF_HConfigCreateAtMapKey(hconfig, keywordEnforcer, index, key, &
    keyString, doc, rc)
!
! !RETURN VALUE:
    type(ESMF_HConfig) :: ESMF_HConfigCreateAtMapKey
!
! !ARGUMENTS:
    type(ESMF_HConfigIter), intent(in)        :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    type(ESMF_HConfig), intent(in),  optional :: key
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    integer,            intent(out), optional :: rc
!
! !DESCRIPTION:
!   Create a new HConfig object for a map key at the current iteration, or
!   as specified by {\tt index}, {\tt key} or {\tt keyString}.
!   The {\tt hconfig} {\em must} be a map iterator.
!
!   The arguments are:
!   \begin{description}
!   \item[hconfig]
!     {\tt ESMF\_HConfigIter} object.
!   \item[{[index]}]
!     Attempt to access by index if specified. Mutural exclusive with
!     {\tt key} and {\tt keyString}.
!   \item[{[key]}]
!     Attempt to access by key if specified. Mutural exclusive with
!     {\tt index} and {\tt keyString},
!   \item[{[keyString]}]
!     Attempt to access by key string if specified. Mutural exclusive with
!     {\tt index} and {\tt key}.
!   \item[{[doc]}]
!     The doc index. Defaults to the first document.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hKey

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! invalidate return value
    ESMF_HConfigCreateAtMapKey%shallowMemory = 0

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigIterGetInit, hconfig, rc)

    ! Check mutual exclusions
    if ((present(index) .and. (present(key) .or. present(keyString))) &
      .or. (present(key) .and. present(keyString))) then
      call ESMF_LogSetError(ESMF_RC_ARG_INCOMP, &
        msg="The 'index', 'key', and 'keyString' arguments are mutual exclusive", &
        ESMF_CONTEXT, rcToReturn=rc)
      return
    endif

    if (present(key) .or. present(keyString)) then
      if (present(keyString)) then
        hkey = ESMF_HConfigCreate(content=keyString, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      else
        ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, key, rc)
        hkey = key
      endif
      ! Call into the C++ interface, which will sort out optional arguments.
      call c_ESMC_HConfigCreateAtMapKeyKey(hconfig, ESMF_HConfigCreateAtMapKey, &
        hkey, doc, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      if (present(keyString)) then
        call ESMF_HConfigDestroy(hkey, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      endif
    else
      ! Call into the C++ interface, which will sort out optional arguments.
      call c_ESMC_HConfigCreateAtMapKey(hconfig, ESMF_HConfigCreateAtMapKey, &
        index, doc, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    ! Set init code
    ESMF_INIT_SET_CREATED(ESMF_HConfigCreateAtMapKey)

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigCreateAtMapVal()"
!BOP
! !IROUTINE: ESMF_HConfigCreateAtMapVal - Return HConfig object at location

! !INTERFACE:
  function ESMF_HConfigCreateAtMapVal(hconfig, keywordEnforcer, index, key, &
    keyString, doc, rc)
!
! !RETURN VALUE:
    type(ESMF_HConfig) :: ESMF_HConfigCreateAtMapVal
!
! !ARGUMENTS:
    type(ESMF_HConfigIter), intent(in)        :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    type(ESMF_HConfig), intent(in),  optional :: key
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    integer,            intent(out), optional :: rc
!
! !DESCRIPTION:
!   Create a new HConfig object for a map value at the current iteration, or
!   as specified by {\tt index}, {\tt key} or {\tt keyString}.
!   The {\tt hconfig} {\em must} be a map iterator.
!
!   The arguments are:
!   \begin{description}
!   \item[hconfig]
!     {\tt ESMF\_HConfigIter} object.
!   \item[{[index]}]
!     Attempt to access by index if specified. Mutural exclusive with
!     {\tt key} and {\tt keyString}.
!   \item[{[key]}]
!     Attempt to access by key if specified. Mutural exclusive with
!     {\tt index} and {\tt keyString},
!   \item[{[keyString]}]
!     Attempt to access by key string if specified. Mutural exclusive with
!     {\tt index} and {\tt key}.
!   \item[{[doc]}]
!     The doc index. Defaults to the first document.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hKey

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! invalidate return value
    ESMF_HConfigCreateAtMapVal%shallowMemory = 0

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigIterGetInit, hconfig, rc)

    ! Check mutual exclusions
    if ((present(index) .and. (present(key) .or. present(keyString))) &
      .or. (present(key) .and. present(keyString))) then
      call ESMF_LogSetError(ESMF_RC_ARG_INCOMP, &
        msg="The 'index', 'key', and 'keyString' arguments are mutual exclusive", &
        ESMF_CONTEXT, rcToReturn=rc)
      return
    endif

    if (present(key) .or. present(keyString)) then
      if (present(keyString)) then
        hkey = ESMF_HConfigCreate(content=keyString, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      else
        ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, key, rc)
        hkey = key
      endif
      ! Call into the C++ interface, which will sort out optional arguments.
      call c_ESMC_HConfigCreateAtMapValKey(hconfig, ESMF_HConfigCreateAtMapVal, &
        hkey, doc, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      if (present(keyString)) then
        call ESMF_HConfigDestroy(hkey, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      endif
    else
      ! Call into the C++ interface, which will sort out optional arguments.
      call c_ESMC_HConfigCreateAtMapVal(hconfig, ESMF_HConfigCreateAtMapVal, &
        index, doc, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    ! Set init code
    ESMF_INIT_SET_CREATED(ESMF_HConfigCreateAtMapVal)

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigDestroy()"
!BOP
! !IROUTINE: ESMF_HConfigDestroy - Release resources associated with a HConfig 

! !INTERFACE:
  subroutine ESMF_HConfigDestroy(hconfig, keywordEnforcer, rc)
!
! !ARGUMENTS:
    type(ESMF_HConfig), intent(inout)          :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
     integer,            intent(out), optional :: rc

! !DESCRIPTION:
!   Destroys an {\tt ESMF\_HConfig}, releasing the resources associated
!   with the object.
!
!   By default a small remnant of the object is kept in memory in order to 
!   prevent problems with dangling aliases. The default garbage collection
!   mechanism can be overridden with the {\tt noGarbage} argument.
!
! The arguments are:
! \begin{description}
! \item[hconfig]
!      {\tt ESMF\_HConfig} object to be destroyed.
! \item[{[rc]}]
!      Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer                 :: localrc        ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_HConfigDestroy(hconfig, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
 
    ! Mark this HConfig as invalid
    hconfig%shallowMemory = 0

    ! Set init code
    ESMF_INIT_SET_DELETED(hconfig)
 
    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS
 
  end subroutine ESMF_HConfigDestroy
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigFileLoad()"
!BOP
! !IROUTINE: ESMF_HConfigFileLoad - Load file into HConfig

! !INTERFACE:
  subroutine ESMF_HConfigFileLoad(hconfig, filename, keywordEnforcer, doc, rc)
!
! !ARGUMENTS:
    type(ESMF_HConfig), intent(in)            :: hconfig
    character(len=*),   intent(in)            :: filename
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: doc
    integer,            intent(out), optional :: rc
!
! !DESCRIPTION:
!   Load YAML file into {\tt hconfig}.
!
!   The arguments are:
!   \begin{description}
!   \item[hconfig]
!     {\tt ESMF\_HConfig} object.
!   \item[filename]
!     Name of the YAML file to be loaded.
!   \item[{[doc]}]
!     The doc index inside the file. If specified, only this single document
!     is loaded from file, resulting in a single document {\tt hconfig} object.
!     Defaults to {\em all} docs.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc                ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_HConfigFileLoad(hconfig, filename, doc, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_HConfigFileLoad
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigFileSave()"
!BOP
! !IROUTINE: ESMF_HConfigFileSave - Save HConfig to file

! !INTERFACE:
  subroutine ESMF_HConfigFileSave(hconfig, filename, keywordEnforcer, doc, rc)
!
! !ARGUMENTS:
    type(ESMF_HConfig), intent(in)            :: hconfig
    character(len=*),   intent(in)            :: filename
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: doc
    integer,            intent(out), optional :: rc
!
! !DESCRIPTION:
!   Save HConfig into YAML file. Only {\tt localPet == 0} does the writing.
!   The {\tt hconfig} must {\em not} be a map iterator.
!
!   The arguments are:
!   \begin{description}
!   \item[hconfig]
!     {\tt ESMF\_HConfig} object.
!   \item[filename]
!     Name of the YAML file into which to save.
!   \item[{[doc]}]
!     The doc index inside {\tt hconfig}. Defaults to {\em all} docs.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc                ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_HConfigFileSave(hconfig, filename, doc, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_HConfigFileSave
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigGetDocCount()"
!BOP
! !IROUTINE: ESMF_HConfigGetDocCount - Get number of docs in HConfig

! !INTERFACE:
  function ESMF_HConfigGetDocCount(hconfig, keywordEnforcer, rc)
! !RETURN VALUE:
    integer :: ESMF_HConfigGetDocCount
!
! !ARGUMENTS:
    type(ESMF_HConfig), intent(in)            :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(out), optional :: rc

! !DESCRIPTION:
!   Return the number of documents held by {\tt hconfig}.
!
! The arguments are:
!   \begin{description}
!   \item[hconfig]
!     {\tt ESMF\_HConfig} object.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc                ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ESMF_HConfigGetDocCount = 0   ! initialize

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_HConfigGetDocCount(hconfig, ESMF_HConfigGetDocCount, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
!BOP
! !IROUTINE: ESMF_HConfigGetSize - Get size of HConfig node

! !INTERFACE:
!  function ESMF_HConfigGetSize(hconfig, keywordEnforcer, index, keyString, doc, rc)
! !RETURN VALUE:
!    integer :: ESMF_HConfigGetSize
!
! !ARGUMENTS:
!    type(ESMF_HConfig[Iter]), intent(in)      :: hconfig
!type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
!    integer,            intent(in),  optional :: index
!    character(*),       intent(in),  optional :: keyString
!    integer,            intent(in),  optional :: doc
!    integer,            intent(out), optional :: rc

! !DESCRIPTION:
!   Return the number of elements in collection {\tt hconfig}.
!
! The arguments are:
!   \begin{description}
!   \item[hconfig]
!     {\tt ESMF\_HConfig} or {\tt ESMF\_HConfigIter} object.
!   \item[{[index]}]
!     Attempt to access by index if specified. Mutural exclusive with {\tt keyString}.
!   \item[{[keyString]}]
!     Attempt to access by key string if specified. Mutural exclusive with {\tt index}.
!   \item[{[doc]}]
!     The doc index. Defaults to the first document.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigGetSize()"

  function ESMF_HConfigGetSize(hconfig, keywordEnforcer, index, keyString, doc, rc)

    integer :: ESMF_HConfigGetSize

    type(ESMF_HConfig), intent(in)            :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    integer,            intent(out), optional :: rc

    integer               :: localrc                ! local return code
    integer               :: size
    type(ESMF_HConfig)    :: hconfigTemp

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ESMF_HConfigGetSize = 0   ! initialize

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    if (present(index).or.present(keyString).or.present(doc)) then
      hconfigTemp = ESMF_HConfigCreateAt(hconfig, index=index, &
        keyString=keyString, doc=doc, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! Call into the C++ interface, which will sort out optional arguments.
      call c_ESMC_HConfigGetSize(hconfigTemp, ESMF_HConfigGetSize, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! clean up
      call ESMF_HConfigDestroy(hconfigTemp, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    else
      ! Call into the C++ interface, which will sort out optional arguments.
      call c_ESMC_HConfigGetSize(hconfig, ESMF_HConfigGetSize, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigIterGetSize()"

  function ESMF_HConfigIterGetSize(hconfig, keywordEnforcer, index, keyString, doc, rc)

    integer :: ESMF_HConfigIterGetSize

    type(ESMF_HConfigIter), intent(in)        :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    integer,            intent(out), optional :: rc

    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hconfigTemp

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    hconfigTemp = ESMF_HConfigIterAsHConfig(hconfig, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ESMF_HConfigIterGetSize = ESMF_HConfigGetSize(hconfigTemp, &
      index=index, keyString=keyString, doc=doc, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigGetSizeMapKey()"
!BOP
! !IROUTINE: ESMF_HConfigGetSizeMapKey - Get size of HConfig node

! !INTERFACE:
  function ESMF_HConfigGetSizeMapKey(hconfig, keywordEnforcer, index, keyString, &
    doc, rc)
! !RETURN VALUE:
    integer :: ESMF_HConfigGetSizeMapKey
!
! !ARGUMENTS:
    type(ESMF_HConfigIter), intent(in)        :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    integer,            intent(out), optional :: rc

! !DESCRIPTION:
!   Return size of the {\tt hconfig} node.
!
! The arguments are:
!   \begin{description}
!   \item[hconfig]
!     {\tt ESMF\_HConfigIter} object.
!   \item[{[index]}]
!     Attempt to access by index if specified. Mutural exclusive with {\tt keyString}.
!   \item[{[keyString]}]
!     Attempt to access by key string if specified. Mutural exclusive with {\tt index}.
!   \item[{[doc]}]
!     The doc index. Defaults to the first document.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc                ! local return code
    integer               :: size
    type(ESMF_HConfig)    :: hconfigTemp

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ESMF_HConfigGetSizeMapKey = 0   ! initialize

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigIterGetInit, hconfig, rc)

    if (present(index).or.present(keyString).or.present(doc)) then
      hconfigTemp = ESMF_HConfigCreateAtMapKey(hconfig, index=index, &
        keyString=keyString, doc=doc, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! Call into the C++ interface, which will sort out optional arguments.
      call c_ESMC_HConfigGetSize(hconfigTemp, ESMF_HConfigGetSizeMapKey, &
        localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! clean up
      call ESMF_HConfigDestroy(hconfigTemp, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    else
      ! Call into the C++ interface, which will sort out optional arguments.
      call c_ESMC_HConfigGetSizeMapKey(hconfig, ESMF_HConfigGetSizeMapKey, &
        localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigGetSizeMapVal()"
!BOP
! !IROUTINE: ESMF_HConfigGetSizeMapVal - Get size of HConfig node

! !INTERFACE:
  function ESMF_HConfigGetSizeMapVal(hconfig, keywordEnforcer, index, keyString, &
    doc, rc)
! !RETURN VALUE:
    integer :: ESMF_HConfigGetSizeMapVal
!
! !ARGUMENTS:
    type(ESMF_HConfigIter), intent(in)        :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    integer,            intent(out), optional :: rc

! !DESCRIPTION:
!   Return size of the {\tt hconfig} node.
!
! The arguments are:
!   \begin{description}
!   \item[hconfig]
!     {\tt ESMF\_HConfigIter} object.
!   \item[{[index]}]
!     Attempt to access by index if specified. Mutural exclusive with {\tt keyString}.
!   \item[{[keyString]}]
!     Attempt to access by key string if specified. Mutural exclusive with {\tt index}.
!   \item[{[doc]}]
!     The doc index. Defaults to the first document.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc                ! local return code
    integer               :: size
    type(ESMF_HConfig)    :: hconfigTemp

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ESMF_HConfigGetSizeMapVal = 0   ! initialize

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigIterGetInit, hconfig, rc)

    if (present(index).or.present(keyString).or.present(doc)) then
      hconfigTemp = ESMF_HConfigCreateAtMapVal(hconfig, index=index, &
        keyString=keyString, doc=doc, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! Call into the C++ interface, which will sort out optional arguments.
      call c_ESMC_HConfigGetSize(hconfigTemp, ESMF_HConfigGetSizeMapVal, &
        localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! clean up
      call ESMF_HConfigDestroy(hconfigTemp, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    else
      ! Call into the C++ interface, which will sort out optional arguments.
      call c_ESMC_HConfigGetSizeMapVal(hconfig, ESMF_HConfigGetSizeMapVal, &
        localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
!BOP
! !IROUTINE: ESMF_HConfigGetTag - Get tag of HConfig node

! !INTERFACE:
!  function ESMF_HConfigGetTag(hconfig, keywordEnforcer, index, keyString, doc, rc)
! !RETURN VALUE:
!    character(len=:), allocatable :: ESMF_HConfigGetTag
!
! !ARGUMENTS:
!    type(ESMF_HConfig[Iter]), intent(in)      :: hconfig
!type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
!    integer,            intent(in),  optional :: index
!    character(*),       intent(in),  optional :: keyString
!    integer,            intent(in),  optional :: doc
!    integer,            intent(out), optional :: rc

! !DESCRIPTION:
!   Return tag string of the {\tt hconfig} node.
!
! The arguments are:
!   \begin{description}
!   \item[hconfig]
!     {\tt ESMF\_HConfig} or {\tt ESMF\_HConfigIter} object.
!   \item[{[index]}]
!     Attempt to access by index if specified. Mutural exclusive with {\tt keyString}.
!   \item[{[keyString]}]
!     Attempt to access by key string if specified. Mutural exclusive with {\tt index}.
!   \item[{[doc]}]
!     The doc index. Defaults to the first document.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigGetTag()"

  function ESMF_HConfigGetTag(hconfig, keywordEnforcer, index, keyString, doc, rc)

    character(len=:), allocatable :: ESMF_HConfigGetTag

    type(ESMF_HConfig), intent(in)            :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    integer,            intent(out), optional :: rc

    integer               :: localrc                ! local return code
    integer               :: len
    type(ESMF_HConfig)    :: hconfigTemp

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    if (present(index).or.present(keyString).or.present(doc)) then
      hconfigTemp = ESMF_HConfigCreateAt(hconfig, index=index, &
        keyString=keyString, doc=doc, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! Call into the C++ interface to get length
      call c_ESMC_HConfigGetTagLen(hconfigTemp, len, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! correctly size the character allocation
      allocate(character(len=len)::ESMF_HConfigGetTag)

      ! Call into the C++ interface to get the string
      call c_ESMC_HConfigGetTag(hconfigTemp, ESMF_HConfigGetTag, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! clean up
      call ESMF_HConfigDestroy(hconfigTemp, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    else
      ! Call into the C++ interface to get length
      call c_ESMC_HConfigGetTagLen(hconfig, len, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! correctly size the character allocation
      allocate(character(len=len)::ESMF_HConfigGetTag)

      ! Call into the C++ interface to get the string
      call c_ESMC_HConfigGetTag(hconfig, ESMF_HConfigGetTag, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigIterGetTag()"

  function ESMF_HConfigIterGetTag(hconfig, keywordEnforcer, index, keyString, doc, rc)

    character(len=:), allocatable :: ESMF_HConfigIterGetTag

    type(ESMF_HConfigIter), intent(in)        :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    integer,            intent(out), optional :: rc

    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hconfigTemp

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    hconfigTemp = ESMF_HConfigIterAsHConfig(hconfig, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ESMF_HConfigIterGetTag = ESMF_HConfigGetTag(hconfigTemp, &
      index=index, keyString=keyString, doc=doc, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigGetTagMapKey()"
!BOP
! !IROUTINE: ESMF_HConfigGetTagMapKey - Get tag of map key node

! !INTERFACE:
  function ESMF_HConfigGetTagMapKey(hconfig, keywordEnforcer, index, keyString, &
    doc, rc)
! !RETURN VALUE:
    character(len=:), allocatable :: ESMF_HConfigGetTagMapKey
!
! !ARGUMENTS:
    type(ESMF_HConfigIter), intent(in)        :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    integer,            intent(out), optional :: rc

! !DESCRIPTION:
!   Return tag string of map key of the {\tt hconfig} node.
!
! The arguments are:
!   \begin{description}
!   \item[hconfig]
!     {\tt ESMF\_HConfigIter} object.
!   \item[{[index]}]
!     Attempt to access by index if specified. Mutural exclusive with {\tt keyString}.
!   \item[{[keyString]}]
!     Attempt to access by key string if specified. Mutural exclusive with {\tt index}.
!   \item[{[doc]}]
!     The doc index. Defaults to the first document.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc                ! local return code
    integer               :: len
    type(ESMF_HConfig)    :: hconfigTemp

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigIterGetInit, hconfig, rc)

    if (present(index).or.present(keyString).or.present(doc)) then
      hconfigTemp = ESMF_HConfigCreateAtMapKey(hconfig, index=index, &
        keyString=keyString, doc=doc, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! Call into the C++ interface to get length
      call c_ESMC_HConfigGetTagLen(hconfigTemp, len, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! correctly size the character allocation
      allocate(character(len=len)::ESMF_HConfigGetTagMapKey)

      ! Call into the C++ interface to get the string
      call c_ESMC_HConfigGetTag(hconfigTemp, ESMF_HConfigGetTagMapKey, &
        localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! clean up
      call ESMF_HConfigDestroy(hconfigTemp, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    else
      ! Call into the C++ interface to get length
      call c_ESMC_HConfigGetTagMapKeyLen(hconfig, len, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! correctly size the character allocation
      allocate(character(len=len)::ESMF_HConfigGetTagMapKey)

      ! Call into the C++ interface to get the string
      call c_ESMC_HConfigGetTagMapKey(hconfig, ESMF_HConfigGetTagMapKey, &
        localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigGetTagMapVal()"
!BOP
! !IROUTINE: ESMF_HConfigGetTagMapVal - Get tag of map key node

! !INTERFACE:
  function ESMF_HConfigGetTagMapVal(hconfig, keywordEnforcer, index, keyString, &
    doc, rc)
! !RETURN VALUE:
    character(len=:), allocatable :: ESMF_HConfigGetTagMapVal
!
! !ARGUMENTS:
    type(ESMF_HConfigIter), intent(in)        :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    integer,            intent(out), optional :: rc

! !DESCRIPTION:
!   Return tag string of map key of the {\tt hconfig} node.
!
! The arguments are:
!   \begin{description}
!   \item[hconfig]
!     {\tt ESMF\_HConfigIter} object.
!   \item[{[index]}]
!     Attempt to access by index if specified. Mutural exclusive with {\tt keyString}.
!   \item[{[keyString]}]
!     Attempt to access by key string if specified. Mutural exclusive with {\tt index}.
!   \item[{[doc]}]
!     The doc index. Defaults to the first document.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc                ! local return code
    integer               :: len
    type(ESMF_HConfig)    :: hconfigTemp

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigIterGetInit, hconfig, rc)

    if (present(index).or.present(keyString).or.present(doc)) then
      hconfigTemp = ESMF_HConfigCreateAtMapVal(hconfig, index=index, &
        keyString=keyString, doc=doc, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! Call into the C++ interface to get length
      call c_ESMC_HConfigGetTagLen(hconfigTemp, len, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! correctly size the character allocation
      allocate(character(len=len)::ESMF_HConfigGetTagMapVal)

      ! Call into the C++ interface to get the string
      call c_ESMC_HConfigGetTag(hconfigTemp, ESMF_HConfigGetTagMapVal, &
        localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! clean up
      call ESMF_HConfigDestroy(hconfigTemp, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    else
      ! Call into the C++ interface to get length
      call c_ESMC_HConfigGetTagMapValLen(hconfig, len, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! correctly size the character allocation
      allocate(character(len=len)::ESMF_HConfigGetTagMapVal)

      ! Call into the C++ interface to get the string
      call c_ESMC_HConfigGetTagMapVal(hconfig, ESMF_HConfigGetTagMapVal, &
        localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
!BOP
! !IROUTINE: ESMF_HConfigIs<NodeType> - Check for HConfig node type

! !INTERFACE:
!  function ESMF_HConfigIs<NodeType>(hconfig, keywordEnforcer, index, keyString, &
!    doc, rc)
! !RETURN VALUE:
!    logical :: ESMF_HConfigIs<NodeType>
!
! !ARGUMENTS:
!    type(ESMF_HConfig[Iter]), intent(in)      :: hconfig
!type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
!    integer,            intent(in),  optional :: index
!    character(*),       intent(in),  optional :: keyString
!    integer,            intent(in),  optional :: doc
!    integer,            intent(out), optional :: rc

! !DESCRIPTION:
!   Return {\tt .true.} if the {\tt hconfig} node is of node type
!   <NodeType>. Otherwise return {\tt .false.}. If an error occurs, i.e.
!   {\tt rc /= ESMF\_SUCCESS} is returned, the return value of the function
!   will be {\tt .false.}.
!
!   The supported <NodeType> options are:
!   \begin{itemize}
!   \item Defined
!   \item Null
!   \item Map
!   \item Scalar
!   \item Sequence
!   \end{itemize}
!
!   The arguments are:
!   \begin{description}
!   \item[hconfig]
!     {\tt ESMF\_HConfig} or {\tt ESMF\_HConfigIter} object.
!   \item[{[index]}]
!     Attempt to access by index if specified. Mutural exclusive with {\tt keyString}.
!   \item[{[keyString]}]
!     Attempt to access by key string if specified. Mutural exclusive with {\tt index}.
!   \item[{[doc]}]
!     The doc index. Defaults to the first document.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigIsDefined()"

  function ESMF_HConfigIsDefined(hconfig, keywordEnforcer, index, keyString, &
    doc, rc)

    logical :: ESMF_HConfigIsDefined

    type(ESMF_HConfig), intent(in)            :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    integer,            intent(out), optional :: rc

    integer               :: localrc                ! local return code
    type(ESMF_Logical)    :: flag
    type(ESMF_HConfig)    :: hconfigTemp

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ESMF_HConfigIsDefined = .false.   ! initialize

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    if (present(index).or.present(keyString).or.present(doc)) then
      hconfigTemp = ESMF_HConfigCreateAt(hconfig, index=index, &
        keyString=keyString, doc=doc, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! Call into the C++ interface, which will sort out optional arguments.
      call c_ESMC_HConfigIsDefined(hconfigTemp, flag, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! clean up
      call ESMF_HConfigDestroy(hconfigTemp, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    else
      ! Call into the C++ interface, which will sort out optional arguments.
      call c_ESMC_HConfigIsDefined(hconfig, flag, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    ! convert ESMF_Logical -> logical
    ESMF_HConfigIsDefined = flag

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigItrIsDefined()"

  function ESMF_HConfigItrIsDefined(hconfig, keywordEnforcer, index, keyString, &
    doc, rc)

    logical :: ESMF_HConfigItrIsDefined

    type(ESMF_HConfigIter), intent(in)        :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    integer,            intent(out), optional :: rc

    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hconfigTemp

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    hconfigTemp = ESMF_HConfigIterAsHConfig(hconfig, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ESMF_HConfigItrIsDefined = ESMF_HConfigIsDefined(hconfigTemp, &
      index=index, keyString=keyString, doc=doc, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigIsNull()"

  function ESMF_HConfigIsNull(hconfig, keywordEnforcer, index, keyString, doc, rc)

    logical :: ESMF_HConfigIsNull

    type(ESMF_HConfig), intent(in)            :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    integer,            intent(out), optional :: rc

    integer               :: localrc                ! local return code
    type(ESMF_Logical)    :: flag
    type(ESMF_HConfig)    :: hconfigTemp

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ESMF_HConfigIsNull = .false.   ! initialize

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    if (present(index).or.present(keyString).or.present(doc)) then
      hconfigTemp = ESMF_HConfigCreateAt(hconfig, index=index, &
        keyString=keyString, doc=doc, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! Call into the C++ interface, which will sort out optional arguments.
      call c_ESMC_HConfigIsNull(hconfigTemp, flag, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! clean up
      call ESMF_HConfigDestroy(hconfigTemp, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    else
      ! Call into the C++ interface, which will sort out optional arguments.
      call c_ESMC_HConfigIsNull(hconfig, flag, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    ! convert ESMF_Logical -> logical
    ESMF_HConfigIsNull = flag

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigItrIsNull()"

  function ESMF_HConfigItrIsNull(hconfig, keywordEnforcer, index, keyString, doc, rc)

    logical :: ESMF_HConfigItrIsNull

    type(ESMF_HConfigIter), intent(in)        :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    integer,            intent(out), optional :: rc

    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hconfigTemp

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    hconfigTemp = ESMF_HConfigIterAsHConfig(hconfig, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ESMF_HConfigItrIsNull = ESMF_HConfigIsNull(hconfigTemp, &
      index=index, keyString=keyString, doc=doc, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigIsMap()"

  function ESMF_HConfigIsMap(hconfig, keywordEnforcer, index, keyString, doc, rc)

    logical :: ESMF_HConfigIsMap

    type(ESMF_HConfig), intent(in)            :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    integer,            intent(out), optional :: rc

    integer               :: localrc                ! local return code
    type(ESMF_Logical)    :: flag
    type(ESMF_HConfig)    :: hconfigTemp

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ESMF_HConfigIsMap = .false.   ! initialize

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    if (present(index).or.present(keyString).or.present(doc)) then
      hconfigTemp = ESMF_HConfigCreateAt(hconfig, index=index, &
        keyString=keyString, doc=doc, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! Call into the C++ interface, which will sort out optional arguments.
      call c_ESMC_HConfigIsMap(hconfigTemp, flag, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! clean up
      call ESMF_HConfigDestroy(hconfigTemp, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    else
      ! Call into the C++ interface, which will sort out optional arguments.
      call c_ESMC_HConfigIsMap(hconfig, flag, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    ! convert ESMF_Logical -> logical
    ESMF_HConfigIsMap = flag

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigItrIsMap()"

  function ESMF_HConfigItrIsMap(hconfig, keywordEnforcer, index, keyString, doc, rc)

    logical :: ESMF_HConfigItrIsMap

    type(ESMF_HConfigIter), intent(in)        :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    integer,            intent(out), optional :: rc

    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hconfigTemp

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    hconfigTemp = ESMF_HConfigIterAsHConfig(hconfig, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ESMF_HConfigItrIsMap = ESMF_HConfigIsMap(hconfigTemp, &
      index=index, keyString=keyString, doc=doc, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigIsScalar()"

  function ESMF_HConfigIsScalar(hconfig, keywordEnforcer, index, keyString, doc, rc)

    logical :: ESMF_HConfigIsScalar

    type(ESMF_HConfig), intent(in)            :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    integer,            intent(out), optional :: rc

    integer               :: localrc                ! local return code
    type(ESMF_Logical)    :: flag
    type(ESMF_HConfig)    :: hconfigTemp

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ESMF_HConfigIsScalar = .false.   ! initialize

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    if (present(index).or.present(keyString).or.present(doc)) then
      hconfigTemp = ESMF_HConfigCreateAt(hconfig, index=index, &
        keyString=keyString, doc=doc, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! Call into the C++ interface, which will sort out optional arguments.
      call c_ESMC_HConfigIsScalar(hconfigTemp, flag, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! clean up
      call ESMF_HConfigDestroy(hconfigTemp, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    else
      ! Call into the C++ interface, which will sort out optional arguments.
      call c_ESMC_HConfigIsScalar(hconfig, flag, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    ! convert ESMF_Logical -> logical
    ESMF_HConfigIsScalar = flag

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigItrIsScalar()"

  function ESMF_HConfigItrIsScalar(hconfig, keywordEnforcer, index, keyString, doc, rc)

    logical :: ESMF_HConfigItrIsScalar

    type(ESMF_HConfigIter), intent(in)        :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    integer,            intent(out), optional :: rc

    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hconfigTemp

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    hconfigTemp = ESMF_HConfigIterAsHConfig(hconfig, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ESMF_HConfigItrIsScalar = ESMF_HConfigIsScalar(hconfigTemp, &
      index=index, keyString=keyString, doc=doc, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigIsSequence()"

  function ESMF_HConfigIsSequence(hconfig, keywordEnforcer, index, keyString, doc, rc)

    logical :: ESMF_HConfigIsSequence

    type(ESMF_HConfig), intent(in)            :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    integer,            intent(out), optional :: rc

    integer               :: localrc                ! local return code
    type(ESMF_Logical)    :: flag
    type(ESMF_HConfig)    :: hconfigTemp

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ESMF_HConfigIsSequence = .false.   ! initialize

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    if (present(index).or.present(keyString).or.present(doc)) then
      hconfigTemp = ESMF_HConfigCreateAt(hconfig, index=index, &
        keyString=keyString, doc=doc, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! Call into the C++ interface, which will sort out optional arguments.
      call c_ESMC_HConfigIsSequence(hconfigTemp, flag, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! clean up
      call ESMF_HConfigDestroy(hconfigTemp, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    else
      ! Call into the C++ interface, which will sort out optional arguments.
      call c_ESMC_HConfigIsSequence(hconfig, flag, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    ! convert ESMF_Logical -> logical
    ESMF_HConfigIsSequence = flag

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigItrIsSequence()"

  function ESMF_HConfigItrIsSequence(hconfig, keywordEnforcer, index, keyString, doc, rc)

    logical :: ESMF_HConfigItrIsSequence

    type(ESMF_HConfigIter), intent(in)        :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    integer,            intent(out), optional :: rc

    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hconfigTemp

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    hconfigTemp = ESMF_HConfigIterAsHConfig(hconfig, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ESMF_HConfigItrIsSequence = ESMF_HConfigIsSequence(hconfigTemp, &
      index=index, keyString=keyString, doc=doc, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
!BOP
! !IROUTINE: ESMF_HConfigIs<NodeType>MapKey - Check for HConfig MapKey node type

! !INTERFACE:
!  function ESMF_HConfigIs<NodeType>MapKey(hconfig, keywordEnforcer, index, keyString, &
!    doc, rc)
! !RETURN VALUE:
!    logical :: ESMF_HConfigIs<NodeType>MapKey
!
! !ARGUMENTS:
!    type(ESMF_HConfigIter), intent(in)       :: hconfig
!type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
!    integer,            intent(in),  optional :: index
!    character(*),       intent(in),  optional :: keyString
!    integer,            intent(in),  optional :: doc
!    integer,            intent(out), optional :: rc

! !DESCRIPTION:
!   Return {\tt .true.} if the {\tt hconfig} MapKey node is of node type
!   <NodeType>. Otherwise return {\tt .false.}. If an error occurs, i.e.
!   {\tt rc /= ESMF\_SUCCESS} is returned, the return value of the function
!   will be {\tt .false.}.
!
!   The supported <NodeType> options are:
!   \begin{itemize}
!   \item Defined
!   \item Null
!   \item Map
!   \item Scalar
!   \item Sequence
!   \end{itemize}
!
!   The arguments are:
!   \begin{description}
!   \item[hconfig]
!     {\tt ESMF\_HConfigIter} object.
!   \item[{[index]}]
!     Attempt to access by index if specified. Mutural exclusive with {\tt keyString}.
!   \item[{[keyString]}]
!     Attempt to access by key string if specified. Mutural exclusive with {\tt index}.
!   \item[{[doc]}]
!     The doc index. Defaults to the first document.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigIsDefinedMapKey()"

  function ESMF_HConfigIsDefinedMapKey(hconfig, keywordEnforcer, index, keyString, &
    doc, rc)

    logical :: ESMF_HConfigIsDefinedMapKey

    type(ESMF_HConfigIter), intent(in)        :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    integer,            intent(out), optional :: rc

    integer               :: localrc                ! local return code
    type(ESMF_Logical)    :: flag
    type(ESMF_HConfig)    :: hconfigTemp

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ESMF_HConfigIsDefinedMapKey = .false.   ! initialize

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigIterGetInit, hconfig, rc)

    if (present(index).or.present(keyString).or.present(doc)) then
      hconfigTemp = ESMF_HConfigCreateAtMapKey(hconfig, index=index, &
        keyString=keyString, doc=doc, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! Call into the C++ interface, which will sort out optional arguments.
      call c_ESMC_HConfigIsDefined(hconfigTemp, flag, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! clean up
      call ESMF_HConfigDestroy(hconfigTemp, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    else
      ! Call into the C++ interface, which will sort out optional arguments.
      call c_ESMC_HConfigIsDefinedMapKey(hconfig, flag, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    ! convert ESMF_Logical -> logical
    ESMF_HConfigIsDefinedMapKey = flag

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------



! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigIsNullMapKey()"

  function ESMF_HConfigIsNullMapKey(hconfig, keywordEnforcer, index, keyString, &
    doc, rc)

    logical :: ESMF_HConfigIsNullMapKey

    type(ESMF_HConfigIter), intent(in)        :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    integer,            intent(out), optional :: rc

    integer               :: localrc                ! local return code
    type(ESMF_Logical)    :: flag
    type(ESMF_HConfig)    :: hconfigTemp

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ESMF_HConfigIsNullMapKey = .false.   ! initialize

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigIterGetInit, hconfig, rc)

    if (present(index).or.present(keyString).or.present(doc)) then
      hconfigTemp = ESMF_HConfigCreateAtMapKey(hconfig, index=index, &
        keyString=keyString, doc=doc, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! Call into the C++ interface, which will sort out optional arguments.
      call c_ESMC_HConfigIsNull(hconfigTemp, flag, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! clean up
      call ESMF_HConfigDestroy(hconfigTemp, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    else
      ! Call into the C++ interface, which will sort out optional arguments.
      call c_ESMC_HConfigIsNullMapKey(hconfig, flag, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    ! convert ESMF_Logical -> logical
    ESMF_HConfigIsNullMapKey = flag

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigIsMapMapKey()"

  function ESMF_HConfigIsMapMapKey(hconfig, keywordEnforcer, index, keyString, &
    doc, rc)

    logical :: ESMF_HConfigIsMapMapKey

    type(ESMF_HConfigIter), intent(in)        :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    integer,            intent(out), optional :: rc

    integer               :: localrc                ! local return code
    type(ESMF_Logical)    :: flag
    type(ESMF_HConfig)    :: hconfigTemp

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ESMF_HConfigIsMapMapKey = .false.   ! initialize

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigIterGetInit, hconfig, rc)

    if (present(index).or.present(keyString).or.present(doc)) then
      hconfigTemp = ESMF_HConfigCreateAtMapKey(hconfig, index=index, &
        keyString=keyString, doc=doc, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! Call into the C++ interface, which will sort out optional arguments.
      call c_ESMC_HConfigIsMap(hconfigTemp, flag, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! clean up
      call ESMF_HConfigDestroy(hconfigTemp, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    else
      ! Call into the C++ interface, which will sort out optional arguments.
      call c_ESMC_HConfigIsMapMapKey(hconfig, flag, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    ! convert ESMF_Logical -> logical
    ESMF_HConfigIsMapMapKey = flag

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigIsScalarMapKey()"

  function ESMF_HConfigIsScalarMapKey(hconfig, keywordEnforcer, index, keyString, &
    doc, rc)

    logical :: ESMF_HConfigIsScalarMapKey

    type(ESMF_HConfigIter), intent(in)        :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    integer,            intent(out), optional :: rc

    integer               :: localrc                ! local return code
    type(ESMF_Logical)    :: flag
    type(ESMF_HConfig)    :: hconfigTemp

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ESMF_HConfigIsScalarMapKey = .false.   ! initialize

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigIterGetInit, hconfig, rc)

    if (present(index).or.present(keyString).or.present(doc)) then
      hconfigTemp = ESMF_HConfigCreateAtMapKey(hconfig, index=index, &
        keyString=keyString, doc=doc, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! Call into the C++ interface, which will sort out optional arguments.
      call c_ESMC_HConfigIsScalar(hconfigTemp, flag, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! clean up
      call ESMF_HConfigDestroy(hconfigTemp, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    else
      ! Call into the C++ interface, which will sort out optional arguments.
      call c_ESMC_HConfigIsScalarMapKey(hconfig, flag, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    ! convert ESMF_Logical -> logical
    ESMF_HConfigIsScalarMapKey = flag

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigIsSequenceMapKey()"

  function ESMF_HConfigIsSequenceMapKey(hconfig, keywordEnforcer, index, keyString, &
    doc, rc)

    logical :: ESMF_HConfigIsSequenceMapKey

    type(ESMF_HConfigIter), intent(in)        :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    integer,            intent(out), optional :: rc

    integer               :: localrc                ! local return code
    type(ESMF_Logical)    :: flag
    type(ESMF_HConfig)    :: hconfigTemp

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ESMF_HConfigIsSequenceMapKey = .false.   ! initialize

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigIterGetInit, hconfig, rc)

    if (present(index).or.present(keyString).or.present(doc)) then
      hconfigTemp = ESMF_HConfigCreateAtMapKey(hconfig, index=index, &
        keyString=keyString, doc=doc, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! Call into the C++ interface, which will sort out optional arguments.
      call c_ESMC_HConfigIsSequence(hconfigTemp, flag, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! clean up
      call ESMF_HConfigDestroy(hconfigTemp, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    else
      ! Call into the C++ interface, which will sort out optional arguments.
      call c_ESMC_HConfigIsSequenceMapKey(hconfig, flag, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    ! convert ESMF_Logical -> logical
    ESMF_HConfigIsSequenceMapKey = flag

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
!BOP
! !IROUTINE: ESMF_HConfigIs<NodeType>MapVal - Check for HConfig MapVal node type

! !INTERFACE:
!  function ESMF_HConfigIs<NodeType>MapVal(hconfig, keywordEnforcer, index, keyString, &
!    doc, rc)
! !RETURN VALUE:
!    logical :: ESMF_HConfigIs<NodeType>MapVal
!
! !ARGUMENTS:
!    type(ESMF_HConfigIter), intent(in)       :: hconfig
!type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
!    integer,            intent(in),  optional :: index
!    character(*),       intent(in),  optional :: keyString
!    integer,            intent(in),  optional :: doc
!    integer,            intent(out), optional :: rc

! !DESCRIPTION:
!   Return {\tt .true.} if the {\tt hconfig} MapVal node is of node type
!   <NodeType>. Otherwise return {\tt .false.}. If an error occurs, i.e.
!   {\tt rc /= ESMF\_SUCCESS} is returned, the return value of the function
!   will be {\tt .false.}.
!
!   The supported <NodeType> options are:
!   \begin{itemize}
!   \item Defined
!   \item Null
!   \item Map
!   \item Scalar
!   \item Sequence
!   \end{itemize}
!
!   The arguments are:
!   \begin{description}
!   \item[hconfig]
!     {\tt ESMF\_HConfigIter} object.
!   \item[{[index]}]
!     Attempt to access by index if specified. Mutural exclusive with {\tt keyString}.
!   \item[{[keyString]}]
!     Attempt to access by key string if specified. Mutural exclusive with {\tt index}.
!   \item[{[doc]}]
!     The doc index. Defaults to the first document.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigIsDefinedMapVal()"

  function ESMF_HConfigIsDefinedMapVal(hconfig, keywordEnforcer, index, keyString, &
    doc, rc)

    logical :: ESMF_HConfigIsDefinedMapVal

    type(ESMF_HConfigIter), intent(in)        :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    integer,            intent(out), optional :: rc

    integer               :: localrc                ! local return code
    type(ESMF_Logical)    :: flag
    type(ESMF_HConfig)    :: hconfigTemp

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ESMF_HConfigIsDefinedMapVal = .false.   ! initialize

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigIterGetInit, hconfig, rc)

    if (present(index).or.present(keyString).or.present(doc)) then
      hconfigTemp = ESMF_HConfigCreateAtMapVal(hconfig, index=index, &
        keyString=keyString, doc=doc, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! Call into the C++ interface, which will sort out optional arguments.
      call c_ESMC_HConfigIsDefined(hconfigTemp, flag, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! clean up
      call ESMF_HConfigDestroy(hconfigTemp, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    else
      ! Call into the C++ interface, which will sort out optional arguments.
      call c_ESMC_HConfigIsDefinedMapVal(hconfig, flag, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    ! convert ESMF_Logical -> logical
    ESMF_HConfigIsDefinedMapVal = flag

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigIsNullMapVal()"

  function ESMF_HConfigIsNullMapVal(hconfig, keywordEnforcer, index, keyString, &
    doc, rc)

    logical :: ESMF_HConfigIsNullMapVal

    type(ESMF_HConfigIter), intent(in)        :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    integer,            intent(out), optional :: rc

    integer               :: localrc                ! local return code
    type(ESMF_Logical)    :: flag
    type(ESMF_HConfig)    :: hconfigTemp

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ESMF_HConfigIsNullMapVal = .false.   ! initialize

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigIterGetInit, hconfig, rc)

    if (present(index).or.present(keyString).or.present(doc)) then
      hconfigTemp = ESMF_HConfigCreateAtMapVal(hconfig, index=index, &
        keyString=keyString, doc=doc, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! Call into the C++ interface, which will sort out optional arguments.
      call c_ESMC_HConfigIsNull(hconfigTemp, flag, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! clean up
      call ESMF_HConfigDestroy(hconfigTemp, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    else
      ! Call into the C++ interface, which will sort out optional arguments.
      call c_ESMC_HConfigIsNullMapVal(hconfig, flag, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    ! convert ESMF_Logical -> logical
    ESMF_HConfigIsNullMapVal = flag

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigIsMapMapVal()"

  function ESMF_HConfigIsMapMapVal(hconfig, keywordEnforcer, index, keyString, &
    doc, rc)

    logical :: ESMF_HConfigIsMapMapVal

    type(ESMF_HConfigIter), intent(in)        :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    integer,            intent(out), optional :: rc

    integer               :: localrc                ! local return code
    type(ESMF_Logical)    :: flag
    type(ESMF_HConfig)    :: hconfigTemp

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ESMF_HConfigIsMapMapVal = .false.   ! initialize

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigIterGetInit, hconfig, rc)

    if (present(index).or.present(keyString).or.present(doc)) then
      hconfigTemp = ESMF_HConfigCreateAtMapVal(hconfig, index=index, &
        keyString=keyString, doc=doc, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! Call into the C++ interface, which will sort out optional arguments.
      call c_ESMC_HConfigIsMap(hconfigTemp, flag, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! clean up
      call ESMF_HConfigDestroy(hconfigTemp, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    else
      ! Call into the C++ interface, which will sort out optional arguments.
      call c_ESMC_HConfigIsMapMapVal(hconfig, flag, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    ! convert ESMF_Logical -> logical
    ESMF_HConfigIsMapMapVal = flag

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigIsScalarMapVal()"

  function ESMF_HConfigIsScalarMapVal(hconfig, keywordEnforcer, index, keyString, &
    doc, rc)

    logical :: ESMF_HConfigIsScalarMapVal

    type(ESMF_HConfigIter), intent(in)        :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    integer,            intent(out), optional :: rc

    integer               :: localrc                ! local return code
    type(ESMF_Logical)    :: flag
    type(ESMF_HConfig)    :: hconfigTemp

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ESMF_HConfigIsScalarMapVal = .false.   ! initialize

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigIterGetInit, hconfig, rc)

    if (present(index).or.present(keyString).or.present(doc)) then
      hconfigTemp = ESMF_HConfigCreateAtMapVal(hconfig, index=index, &
        keyString=keyString, doc=doc, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! Call into the C++ interface, which will sort out optional arguments.
      call c_ESMC_HConfigIsScalar(hconfigTemp, flag, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! clean up
      call ESMF_HConfigDestroy(hconfigTemp, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    else
      ! Call into the C++ interface, which will sort out optional arguments.
      call c_ESMC_HConfigIsScalarMapVal(hconfig, flag, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    ! convert ESMF_Logical -> logical
    ESMF_HConfigIsScalarMapVal = flag

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigIsSequenceMapVal()"

  function ESMF_HConfigIsSequenceMapVal(hconfig, keywordEnforcer, index, keyString, &
    doc, rc)

    logical :: ESMF_HConfigIsSequenceMapVal

    type(ESMF_HConfigIter), intent(in)        :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    integer,            intent(out), optional :: rc

    integer               :: localrc                ! local return code
    type(ESMF_Logical)    :: flag
    type(ESMF_HConfig)    :: hconfigTemp

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ESMF_HConfigIsSequenceMapVal = .false.   ! initialize

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigIterGetInit, hconfig, rc)

    if (present(index).or.present(keyString).or.present(doc)) then
      hconfigTemp = ESMF_HConfigCreateAtMapVal(hconfig, index=index, &
        keyString=keyString, doc=doc, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! Call into the C++ interface, which will sort out optional arguments.
      call c_ESMC_HConfigIsSequence(hconfigTemp, flag, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! clean up
      call ESMF_HConfigDestroy(hconfigTemp, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    else
      ! Call into the C++ interface, which will sort out optional arguments.
      call c_ESMC_HConfigIsSequenceMapVal(hconfig, flag, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    ! convert ESMF_Logical -> logical
    ESMF_HConfigIsSequenceMapVal = flag

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-internal method -----------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigIterAsHConfig()"
!BOPI
! !IROUTINE: ESMF_HConfigIterAsHConfig - Interpret an iterator as hconfig

! !INTERFACE:
  function ESMF_HConfigIterAsHConfig(hconfig, keywordEnforcer, rc)
! !RETURN VALUE:
    type(ESMF_HConfig) :: ESMF_HConfigIterAsHConfig
!
! !ARGUMENTS:
    type(ESMF_HConfigIter), intent(in)        :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(out), optional :: rc

! !DESCRIPTION:
!   Return an {\tt ESMF\_HConfig} object
!
! The arguments are:
!   \begin{description}
!   \item[hconfig]
!     {\tt ESMF\_HConfigIter} object.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    integer               :: localrc                ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigIterGetInit, hconfig, rc)

    ! Copy the shallow memory
    ESMF_HConfigIterAsHConfig%shallowMemory = hconfig%shallowMemory
    ESMF_HConfigIterAsHConfig%loopFirst = hconfig%loopFirst

    ! Set init code
    ESMF_INIT_SET_CREATED(ESMF_HConfigIterAsHConfig)

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
!BOP
! !IROUTINE: ESMF_HConfigIterBegin - Iterator at the beginning
!
! !INTERFACE:
!  function ESMF_HConfigIterBegin(hconfig, keywordEnforcer, rc)
! !RETURN VALUE:
!    type(ESMF_HConfigIter) :: ESMF_HConfigIterBegin
!
! !ARGUMENTS:
!    type(ESMF_HConfig[Iter]), intent(in)      :: hconfig
!type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
!    integer,            intent(out), optional :: rc
!
! !DESCRIPTION:
!   Return an iterator that points to the first item in {\tt hconfig}.
!
! The arguments are:
!   \begin{description}
!   \item[hconfig]
!     {\tt ESMF\_HConfig} or {\tt ESMF\_HConfigIter} object.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigIterBegin()"

  function ESMF_HConfigIterBegin(hconfig, keywordEnforcer, rc)

    type(ESMF_HConfigIter) :: ESMF_HConfigIterBegin

    type(ESMF_HConfig), intent(in)            :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(out), optional :: rc

    integer               :: localrc                ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! invalidate return value
    ESMF_HConfigIterBegin%shallowMemory = 0

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_HConfigIterBegin(hconfig, ESMF_HConfigIterBegin, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Set init code
    ESMF_INIT_SET_CREATED(ESMF_HConfigIterBegin)

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigItrIterBegin()"

  function ESMF_HConfigItrIterBegin(hconfig, keywordEnforcer, rc)

    type(ESMF_HConfigIter) :: ESMF_HConfigItrIterBegin

    type(ESMF_HConfigIter), intent(in)        :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(out), optional :: rc

    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hconfigTemp

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    hconfigTemp = ESMF_HConfigIterAsHConfig(hconfig, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ESMF_HConfigItrIterBegin = ESMF_HConfigIterBegin(hconfigTemp, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigIterBeginMapKey()"
!BOP
! !IROUTINE: ESMF_HConfigIterBeginMapKey - Iterator at the beginning

! !INTERFACE:
  function ESMF_HConfigIterBeginMapKey(hconfig, keywordEnforcer, rc)
! !RETURN VALUE:
    type(ESMF_HConfigIter) :: ESMF_HConfigIterBeginMapKey
!
! !ARGUMENTS:
    type(ESMF_HConfigIter), intent(in)        :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(out), optional :: rc

! !DESCRIPTION:
!   Return an iterator that points to the first item in {\tt hconfig}.
!
! The arguments are:
!   \begin{description}
!   \item[hconfig]
!     {\tt ESMF\_HConfigIter} object.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc                ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! invalidate return value
    ESMF_HConfigIterBeginMapKey%shallowMemory = 0

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigIterGetInit, hconfig, rc)

    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_HConfigIterBeginMapKey(hconfig, ESMF_HConfigIterBeginMapKey, &
      localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Set init code
    ESMF_INIT_SET_CREATED(ESMF_HConfigIterBeginMapKey)

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigIterBeginMapVal()"
!BOP
! !IROUTINE: ESMF_HConfigIterBeginMapVal - Iterator at the beginning

! !INTERFACE:
  function ESMF_HConfigIterBeginMapVal(hconfig, keywordEnforcer, rc)
! !RETURN VALUE:
    type(ESMF_HConfigIter) :: ESMF_HConfigIterBeginMapVal
!
! !ARGUMENTS:
    type(ESMF_HConfigIter), intent(in)        :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(out), optional :: rc

! !DESCRIPTION:
!   Return an iterator that points to the first item in {\tt hconfig}.
!
! The arguments are:
!   \begin{description}
!   \item[hconfig]
!     {\tt ESMF\_HConfigIter} object.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc                ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! invalidate return value
    ESMF_HConfigIterBeginMapVal%shallowMemory = 0

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigIterGetInit, hconfig, rc)

    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_HConfigIterBeginMapVal(hconfig, ESMF_HConfigIterBeginMapVal, &
      localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Set init code
    ESMF_INIT_SET_CREATED(ESMF_HConfigIterBeginMapVal)

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
!BOP
! !IROUTINE: ESMF_HConfigIterEnd - Iterator at the end
!
! !INTERFACE:
!  function ESMF_HConfigIterEnd(hconfig, keywordEnforcer, rc)
! !RETURN VALUE:
!    type(ESMF_HConfigIter) :: ESMF_HConfigIterEnd
!
! !ARGUMENTS:
!    type(ESMF_HConfig[Iter]), intent(in)      :: hconfig
!type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
!    integer,            intent(out), optional :: rc
!
! !DESCRIPTION:
!   Return an iterator that points to one past the last item in {\tt hconfig}.
!
! The arguments are:
!   \begin{description}
!   \item[hconfig]
!     {\tt ESMF\_HConfig} or {\tt ESMF\_HConfigIter} object.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigIterEnd()"

  function ESMF_HConfigIterEnd(hconfig, keywordEnforcer, rc)

    type(ESMF_HConfigIter) :: ESMF_HConfigIterEnd

    type(ESMF_HConfig), intent(in)            :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(out), optional :: rc

    integer               :: localrc                ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! invalidate return value
    ESMF_HConfigIterEnd%shallowMemory = 0

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_HConfigIterEnd(hconfig, ESMF_HConfigIterEnd, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Set init code
    ESMF_INIT_SET_CREATED(ESMF_HConfigIterEnd)

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigItrIterEnd()"

  function ESMF_HConfigItrIterEnd(hconfig, keywordEnforcer, rc)

    type(ESMF_HConfigIter) :: ESMF_HConfigItrIterEnd

    type(ESMF_HConfigIter), intent(in)        :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(out), optional :: rc

    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hconfigTemp

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    hconfigTemp = ESMF_HConfigIterAsHConfig(hconfig, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ESMF_HConfigItrIterEnd = ESMF_HConfigIterEnd(hconfigTemp, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigIterEndMapKey()"
!BOP
! !IROUTINE: ESMF_HConfigIterEndMapKey - Iterator at the end

! !INTERFACE:
  function ESMF_HConfigIterEndMapKey(hconfig, keywordEnforcer, rc)
! !RETURN VALUE:
    type(ESMF_HConfigIter) :: ESMF_HConfigIterEndMapKey
!
! !ARGUMENTS:
    type(ESMF_HConfigIter), intent(in)        :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(out), optional :: rc

! !DESCRIPTION:
!   Return an iterator that points to one past the last item in {\tt hconfig}.
!
! The arguments are:
!   \begin{description}
!   \item[hconfig]
!     {\tt ESMF\_HConfigIter} object.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc                ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! invalidate return value
    ESMF_HConfigIterEndMapKey%shallowMemory = 0

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigIterGetInit, hconfig, rc)

    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_HConfigIterEndMapKey(hconfig, ESMF_HConfigIterEndMapKey, &
      localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Set init code
    ESMF_INIT_SET_CREATED(ESMF_HConfigIterEndMapKey)

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigIterEndMapVal()"
!BOP
! !IROUTINE: ESMF_HConfigIterEndMapVal - Iterator at the end

! !INTERFACE:
  function ESMF_HConfigIterEndMapVal(hconfig, keywordEnforcer, rc)
! !RETURN VALUE:
    type(ESMF_HConfigIter) :: ESMF_HConfigIterEndMapVal
!
! !ARGUMENTS:
    type(ESMF_HConfigIter), intent(in)        :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(out), optional :: rc

! !DESCRIPTION:
!   Return an iterator that points to one past the last item in {\tt hconfig}.
!
! The arguments are:
!   \begin{description}
!   \item[hconfig]
!     {\tt ESMF\_HConfigIter} object.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc                ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! invalidate return value
    ESMF_HConfigIterEndMapVal%shallowMemory = 0

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigIterGetInit, hconfig, rc)

    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_HConfigIterEndMapVal(hconfig, ESMF_HConfigIterEndMapVal, &
      localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Set init code
    ESMF_INIT_SET_CREATED(ESMF_HConfigIterEndMapVal)

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigIterIsMap()"
!BOP
! !IROUTINE: ESMF_HConfigIterIsMap - Check whether HConfig iterator is Map

! !INTERFACE:
  function ESMF_HConfigIterIsMap(hconfig, keywordEnforcer, rc)
! !RETURN VALUE:
    logical :: ESMF_HConfigIterIsMap
!
! !ARGUMENTS:
    type(ESMF_HConfigIter), intent(in)        :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(out), optional :: rc

! !DESCRIPTION:
!   Return {\tt .true.} if the {\tt hconfig} node is Null. Otherwise return
!   {\tt .false.}. If an error occurs, i.e. {\tt rc /= ESMF\_SUCCESS} is
!   returned, the return value of the function will also be {\tt .false.}.
!
! The arguments are:
!   \begin{description}
!   \item[hconfig]
!     {\tt ESMF\_HConfigIter} object.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc                ! local return code
    type(ESMF_Logical)    :: flag

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ESMF_HConfigIterIsMap = .false.   ! initialize

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigIterGetInit, hconfig, rc)

    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_HConfigIsMapIterator(hconfig, flag, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    ESMF_HConfigIterIsMap = flag

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigIterIsSequence()"
!BOP
! !IROUTINE: ESMF_HConfigIterIsSequence - Check whether HConfig iterator is Sequence

! !INTERFACE:
  function ESMF_HConfigIterIsSequence(hconfig, keywordEnforcer, rc)
! !RETURN VALUE:
    logical :: ESMF_HConfigIterIsSequence
!
! !ARGUMENTS:
    type(ESMF_HConfigIter), intent(in)        :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(out), optional :: rc

! !DESCRIPTION:
!   Return {\tt .true.} if the {\tt hconfig} node is Null. Otherwise return
!   {\tt .false.}. If an error occurs, i.e. {\tt rc /= ESMF\_SUCCESS} is
!   returned, the return value of the function will also be {\tt .false.}.
!
! The arguments are:
!   \begin{description}
!   \item[hconfig]
!     {\tt ESMF\_HConfigIter} object.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc                ! local return code
    type(ESMF_Logical)    :: flag

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ESMF_HConfigIterIsSequence = .false.   ! initialize

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigIterGetInit, hconfig, rc)

    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_HConfigIsSeqIterator(hconfig, flag, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    ESMF_HConfigIterIsSequence = flag

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigIterLoop()"
!BOP
! !IROUTINE: ESMF_HConfigIterLoop - Step iterator forward

! !INTERFACE:
  function ESMF_HConfigIterLoop(hconfig, hconfigBegin, hconfigEnd, keywordEnforcer, rc)
! !RETURN VALUE:
    logical :: ESMF_HConfigIterLoop
!
! !ARGUMENTS:
    type(ESMF_HConfigIter), intent(inout)     :: hconfig
    type(ESMF_HConfigIter), intent(in)        :: hconfigBegin
    type(ESMF_HConfigIter), intent(in)        :: hconfigEnd
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(out), optional :: rc

! !DESCRIPTION:
!   Step the iterator {\tt hconfig} forward. starting at {\tt hconfigBegin}
!   until {\tt hconfigEnd} is reached. Returns {\tt .true.} as long as
!   {\tt hconfig} has not reached {\tt hconfigEnd}. Once this condition has
!   been reached, returns {\tt .false.}.
!
!   The intended usage of {\tt ESMF\_HConfigIterLoop()} is as the conditional
!   in a {\tt do while} loop, iterating over the elements of a HConfig object.
!
! The arguments are:
!   \begin{description}
!   \item[hconfig]
!     The {\tt ESMF\_HConfigIter} object. Must enter equal to
!     {\tt hconfigBegin} on the first loop step.
!   \item[hconfigBegin]
!     The {\tt ESMF\_HConfigIter} to begin at.
!   \item[hconfigEnd]
!     The {\tt ESMF\_HConfigIter} that marks the end of the loop.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc                ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! default to returning .false.
    ESMF_HConfigIterLoop = .false.

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigIterGetInit, hconfig, rc)
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigIterGetInit, hconfigBegin, rc)
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigIterGetInit, hconfigEnd, rc)

    ! detect hconfigEnd condition early
    if (hconfig == hconfigEnd) return

    ! take the conditional next step
    if (hconfig /= hconfigBegin .or. .not.hconfig%loopFirst) then
      ! reset for next time this iterator is used from the beginning of loop
      if (hconfig == hconfigBegin) hconfig%loopFirst = .true.
      ! next
      call ESMF_HConfigIterNext(hconfig, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    else
      ! prepare for next loop call on this iterator
      hconfig%loopFirst = .false.
    endif

    ! Set the return value
    ESMF_HConfigIterLoop = (hconfig /= hconfigEnd)

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigIterNext()"
!BOP
! !IROUTINE: ESMF_HConfigIterNext - Step iterator forward

! !INTERFACE:
  subroutine ESMF_HConfigIterNext(hconfig, keywordEnforcer, rc)
!
! !ARGUMENTS:
    type(ESMF_HConfigIter), intent(inout)         :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(out), optional :: rc

! !DESCRIPTION:
!   Step the iterator {\tt hconfig} one step forward.
!
! The arguments are:
!   \begin{description}
!   \item[hconfig]
!     {\tt ESMF\_HConfigIter} object.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc                ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigIterGetInit, hconfig, rc)

    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_HConfigIterNext(hconfig, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine
!------------------------------------------------------------------------------


! -------------------------- ESMF-internal method -----------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigLoad()"
!BOPI
! !IROUTINE: ESMF_HConfigLoad - Load string into HConfig

! !INTERFACE:
  subroutine ESMF_HConfigLoad(hconfig, content, keywordEnforcer, rc)
!
! !ARGUMENTS:
    type(ESMF_HConfig), intent(in)            :: hconfig
    character(len=*),   intent(in)            :: content
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(out), optional :: rc
!
! !DESCRIPTION:
!   Load YAML syntax string into HConfig.
!
!   The arguments are:
!   \begin{description}
!   \item[hconfig]
!     {\tt ESMF\_HConfig} object.
!   \item[content]
!     String containing the YAML text.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    integer               :: localrc                ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_HConfigLoad(hconfig, content, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_HConfigLoad
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
!BOP
! !IROUTINE: ESMF_HConfigRemove - Remove element from HConfig object
!
! !INTERFACE:
!  subroutine ESMF_HConfigRemove(hconfig, keywordEnforcer, index, keyString, rc)
!
! !ARGUMENTS:
!    type(ESMF_HConfig[Iter]), intent(in)      :: hconfigIter
!type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
!    integer,            intent(in),  optional :: index
!    character(*),       intent(in),  optional :: keyString
!    integer,            intent(out), optional :: rc
!
! !DESCRIPTION:
!   Remove an element from a squence or map HConfig object. Either {\tt index}
!   (for sequence) or {\tt keyString} (for map) must be provided. An error is
!   flagged if neither optional argument is specified.
!
!   The arguments are:
!   \begin{description}
!   \item[hconfig]
!     {\tt ESMF\_HConfig} object.
!   \item[{[index]}]
!     Attempt to access by index if specified.
!     Mutural exclusive with {\tt keyString}.
!   \item[{[keyString]}]
!     Attempt to access by key string if specified.
!     Mutural exclusive with {\tt index}.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigRemove()"

  subroutine ESMF_HConfigRemove(hconfig, keywordEnforcer, index, keyString, rc)

    type(ESMF_HConfig), intent(in)            :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(out), optional :: rc

    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hconfigTemp

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    ! Check mutual exclusions
    if (present(index) .and. present(keyString)) then
      call ESMF_LogSetError(ESMF_RC_ARG_INCOMP, &
        msg="The 'index' and 'keyString' arguments are mutual exclusive", &
        ESMF_CONTEXT, rcToReturn=rc)
      return
    endif

    if (present(index)) then
      ! Call into the C++ interface to set remove element by index
      call c_ESMC_HConfigRemoveIndex(hconfig, index, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    else if (present(keyString)) then
      ! Call into the C++ interface to set remove element by keyString
      call c_ESMC_HConfigRemoveKeyString(hconfig, keyString, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    else
      call ESMF_LogSetError(ESMF_RC_ARG_INCOMP, &
        msg="Either 'index' or 'keyString' MUST be sepcified", &
        ESMF_CONTEXT, rcToReturn=rc)
      return
    endif

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigIterRemove()"

  subroutine ESMF_HConfigIterRemove(hconfig, keywordEnforcer, index, keyString, rc)

    type(ESMF_HConfigIter), intent(in)        :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(out), optional :: rc

    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hconfigTemp

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    hconfigTemp = ESMF_HConfigIterAsHConfig(hconfig, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_HConfigRemove(hconfigTemp, &
      index=index, keyString=keyString, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
!BOP
! !IROUTINE: ESMF_HConfigSet - Set <Type> content in HConfig object

! !INTERFACE:
!  subroutine ESMF_HConfigSet(hconfig, content, keywordEnforcer, &
!    index, keyString, doc, rc)
!
! !ARGUMENTS:
!    type(ESMF_HConfig[Iter]), intent(in)      :: hconfig
!    <Type>,             intent(in)            :: content[(:}]
!type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
!    integer,            intent(in),  optional :: index
!    character(*),       intent(in),  optional :: keyString
!    integer,            intent(in),  optional :: doc
!    integer,            intent(out), optional :: rc
!
! !DESCRIPTION:
!   Set the content of type <Type> to {\tt hconfig},
!   at the current location, or as specified by {\tt index} or {\tt keyString}
!   (mutually exclusive!).
!   Most <Type> options support the sequence array variant {\tt (:)} in
!   addition to the scalar variant.
!
!   The supported <Type> options are:
!   \begin{itemize}
!   \item {\tt type(HConfig)} (scalar only variant!)
!   \item {\tt integer(ESMF\_KIND\_I4)}
!   \item {\tt integer(ESMF\_KIND\_I8)}
!   \item {\tt logical}
!   \item {\tt real(ESMF\_KIND\_R4)}
!   \item {\tt real(ESMF\_KIND\_R8)}
!   \item {\tt character(*)}
!   \end{itemize}
!
!   The arguments are:
!   \begin{description}
!   \item[hconfig]
!     {\tt ESMF\_HConfig} or {\tt ESMF\_HConfigIter} object.
!   \item[content]
!     The content to be set.
!   \item[{[index]}]
!     Attempt to access by index if specified. Mutural exclusive with {\tt keyString}.
!   \item[{[keyString]}]
!     Attempt to access by key string if specified. Mutural exclusive with {\tt index}.
!   \item[{[doc]}]
!     The doc index. Defaults to the first document.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigSetHConfig()"

  subroutine ESMF_HConfigSetHConfig(hconfig, content, keywordEnforcer, &
    index, keyString, doc, rc)

    type(ESMF_HConfig), intent(in)            :: hconfig
    type(ESMF_HConfig), intent(in)            :: content
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    integer,            intent(out), optional :: rc

    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hconfigTemp

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    if (present(index).or.present(keyString).or.present(doc)) then
      hconfigTemp = ESMF_HConfigCreateAt(hconfig, index=index, &
        keyString=keyString, doc=doc, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      ! Call into the C++ interface to set content
      call c_ESMC_HConfigSet(hconfigTemp, content, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      ! clean up
      call ESMF_HConfigDestroy(hconfigTemp, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    else
      ! Call into the C++ interface to set content
      call c_ESMC_HConfigSet(hconfig, content, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigIterSetHConfig()"

  subroutine ESMF_HConfigIterSetHConfig(hconfig, content, keywordEnforcer, &
    index, keyString, doc, rc)

    type(ESMF_HConfigIter), intent(in)        :: hconfig
    type(ESMF_HConfig), intent(in)            :: content
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    integer,            intent(out), optional :: rc

    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hconfigTemp

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    hconfigTemp = ESMF_HConfigIterAsHConfig(hconfig, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_HConfigSet(hconfigTemp, content=content, &
      index=index, keyString=keyString, doc=doc, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigSetI4()"

  subroutine ESMF_HConfigSetI4(hconfig, content, keywordEnforcer, &
    index, keyString, doc, rc)

    type(ESMF_HConfig),     intent(in)            :: hconfig
    integer(ESMF_KIND_I4),  intent(in)            :: content
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,                intent(in),  optional :: index
    character(*),           intent(in),  optional :: keyString
    integer,                intent(in),  optional :: doc
    integer,                intent(out), optional :: rc

    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hcontent

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    hcontent = ESMF_HConfigCreate(content=content, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_HConfigSet(hconfig, hcontent, index=index, keyString=keyString, &
      doc=doc, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! clean up
    call ESMF_HConfigDestroy(hcontent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigIterSetI4()"

  subroutine ESMF_HConfigIterSetI4(hconfig, content, keywordEnforcer, &
    index, keyString, doc, rc)

    type(ESMF_HConfigIter), intent(in)            :: hconfig
    integer(ESMF_KIND_I4),  intent(in)            :: content
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,                intent(in),  optional :: index
    character(*),           intent(in),  optional :: keyString
    integer,                intent(in),  optional :: doc
    integer,                intent(out), optional :: rc

    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hconfigTemp

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    hconfigTemp = ESMF_HConfigIterAsHConfig(hconfig, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_HConfigSet(hconfigTemp, content=content, &
      index=index, keyString=keyString, doc=doc, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigSetI4Seq()"

  subroutine ESMF_HConfigSetI4Seq(hconfig, content, keywordEnforcer, &
    index, keyString, doc, rc)

    type(ESMF_HConfig),     intent(in)            :: hconfig
    integer(ESMF_KIND_I4),  intent(in)            :: content(:)
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,                intent(in),  optional :: index
    character(*),           intent(in),  optional :: keyString
    integer,                intent(in),  optional :: doc
    integer,                intent(out), optional :: rc

    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hcontent

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    hcontent = ESMF_HConfigCreate(content=content, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_HConfigSet(hconfig, hcontent, index=index, keyString=keyString, &
      doc=doc, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! clean up
    call ESMF_HConfigDestroy(hcontent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigIterSetI4Seq()"

  subroutine ESMF_HConfigIterSetI4Seq(hconfig, content, keywordEnforcer, &
    index, keyString, doc, rc)

    type(ESMF_HConfigIter), intent(in)            :: hconfig
    integer(ESMF_KIND_I4),  intent(in)            :: content(:)
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,                intent(in),  optional :: index
    character(*),           intent(in),  optional :: keyString
    integer,                intent(in),  optional :: doc
    integer,                intent(out), optional :: rc
    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hconfigTemp

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    hconfigTemp = ESMF_HConfigIterAsHConfig(hconfig, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_HConfigSet(hconfigTemp, content=content, &
      index=index, keyString=keyString, doc=doc, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigSetI8()"

  subroutine ESMF_HConfigSetI8(hconfig, content, keywordEnforcer, &
    index, keyString, doc, rc)

    type(ESMF_HConfig),     intent(in)            :: hconfig
    integer(ESMF_KIND_I8),  intent(in)            :: content
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,                intent(in),  optional :: index
    character(*),           intent(in),  optional :: keyString
    integer,                intent(in),  optional :: doc
    integer,                intent(out), optional :: rc

    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hcontent

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    hcontent = ESMF_HConfigCreate(content=content, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_HConfigSet(hconfig, hcontent, index=index, keyString=keyString, &
      doc=doc, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! clean up
    call ESMF_HConfigDestroy(hcontent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigIterSetI8()"

  subroutine ESMF_HConfigIterSetI8(hconfig, content, keywordEnforcer, &
    index, keyString, doc, rc)

    type(ESMF_HConfigIter), intent(in)            :: hconfig
    integer(ESMF_KIND_I8),  intent(in)            :: content
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,                intent(in),  optional :: index
    character(*),           intent(in),  optional :: keyString
    integer,                intent(in),  optional :: doc
    integer,                intent(out), optional :: rc

    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hconfigTemp

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    hconfigTemp = ESMF_HConfigIterAsHConfig(hconfig, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_HConfigSet(hconfigTemp, content=content, &
      index=index, keyString=keyString, doc=doc, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigSetI8Seq()"

  subroutine ESMF_HConfigSetI8Seq(hconfig, content, keywordEnforcer, &
    index, keyString, doc, rc)

    type(ESMF_HConfig),     intent(in)            :: hconfig
    integer(ESMF_KIND_I8),  intent(in)            :: content(:)
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,                intent(in),  optional :: index
    character(*),           intent(in),  optional :: keyString
    integer,                intent(in),  optional :: doc
    integer,                intent(out), optional :: rc

    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hcontent

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    hcontent = ESMF_HConfigCreate(content=content, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_HConfigSet(hconfig, hcontent, index=index, keyString=keyString, &
      doc=doc, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! clean up
    call ESMF_HConfigDestroy(hcontent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigIterSetI8Seq()"

  subroutine ESMF_HConfigIterSetI8Seq(hconfig, content, keywordEnforcer, &
    index, keyString, doc, rc)

    type(ESMF_HConfigIter), intent(in)            :: hconfig
    integer(ESMF_KIND_I8),  intent(in)            :: content(:)
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,                intent(in),  optional :: index
    character(*),           intent(in),  optional :: keyString
    integer,                intent(in),  optional :: doc
    integer,                intent(out), optional :: rc

    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hconfigTemp

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    hconfigTemp = ESMF_HConfigIterAsHConfig(hconfig, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_HConfigSet(hconfigTemp, content=content, &
      index=index, keyString=keyString, doc=doc, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigSetLogical()"

  subroutine ESMF_HConfigSetLogical(hconfig, content, keywordEnforcer, &
    index, keyString, doc, rc)

    type(ESMF_HConfig), intent(in)            :: hconfig
    logical,            intent(in)            :: content
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    integer,            intent(out), optional :: rc

    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hcontent

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    hcontent = ESMF_HConfigCreate(content=content, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_HConfigSet(hconfig, hcontent, index=index, keyString=keyString, &
      doc=doc, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! clean up
    call ESMF_HConfigDestroy(hcontent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigIterSetLogical()"

  subroutine ESMF_HConfigIterSetLogical(hconfig, content, keywordEnforcer, &
    index, keyString, doc, rc)

    type(ESMF_HConfigIter), intent(in)        :: hconfig
    logical,            intent(in)            :: content
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    integer,            intent(out), optional :: rc

    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hconfigTemp

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    hconfigTemp = ESMF_HConfigIterAsHConfig(hconfig, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_HConfigSet(hconfigTemp, content=content, &
      index=index, keyString=keyString, doc=doc, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigSetLogicalSeq()"

  subroutine ESMF_HConfigSetLogicalSeq(hconfig, content, keywordEnforcer, &
    index, keyString, doc, rc)

    type(ESMF_HConfig),     intent(in)            :: hconfig
    logical,                intent(in)            :: content(:)
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,                intent(in),  optional :: index
    character(*),           intent(in),  optional :: keyString
    integer,                intent(in),  optional :: doc
    integer,                intent(out), optional :: rc

    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hcontent

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    hcontent = ESMF_HConfigCreate(content=content, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_HConfigSet(hconfig, hcontent, index=index, keyString=keyString, &
      doc=doc, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! clean up
    call ESMF_HConfigDestroy(hcontent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigIterSetLogicalSeq()"

  subroutine ESMF_HConfigIterSetLogicalSeq(hconfig, content, keywordEnforcer, &
    index, keyString, doc, rc)

    type(ESMF_HConfigIter), intent(in)            :: hconfig
    logical,                intent(in)            :: content(:)
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,                intent(in),  optional :: index
    character(*),           intent(in),  optional :: keyString
    integer,                intent(in),  optional :: doc
    integer,                intent(out), optional :: rc

    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hconfigTemp

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    hconfigTemp = ESMF_HConfigIterAsHConfig(hconfig, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_HConfigSet(hconfigTemp, content=content, &
      index=index, keyString=keyString, doc=doc, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine
!------------------------------------------------------------------------------

! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigSetR4()"

  subroutine ESMF_HConfigSetR4(hconfig, content, keywordEnforcer, &
    index, keyString, doc, rc)

    type(ESMF_HConfig), intent(in)            :: hconfig
    real(ESMF_KIND_R4), intent(in)            :: content
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    integer,            intent(out), optional :: rc

    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hcontent

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    hcontent = ESMF_HConfigCreate(content=content, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_HConfigSet(hconfig, hcontent, index=index, keyString=keyString, &
      doc=doc, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! clean up
    call ESMF_HConfigDestroy(hcontent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigIterSetR4()"

  subroutine ESMF_HConfigIterSetR4(hconfig, content, keywordEnforcer, &
    index, keyString, doc, rc)

    type(ESMF_HConfigIter), intent(in)        :: hconfig
    real(ESMF_KIND_R4), intent(in)            :: content
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    integer,            intent(out), optional :: rc

    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hconfigTemp

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    hconfigTemp = ESMF_HConfigIterAsHConfig(hconfig, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_HConfigSet(hconfigTemp, content=content, &
      index=index, keyString=keyString, doc=doc, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigSetR4Seq()"

  subroutine ESMF_HConfigSetR4Seq(hconfig, content, keywordEnforcer, &
    index, keyString, doc, rc)

    type(ESMF_HConfig), intent(in)            :: hconfig
    real(ESMF_KIND_R4), intent(in)            :: content(:)
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    integer,            intent(out), optional :: rc

    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hcontent

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    hcontent = ESMF_HConfigCreate(content=content, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_HConfigSet(hconfig, hcontent, index=index, keyString=keyString, &
      doc=doc, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! clean up
    call ESMF_HConfigDestroy(hcontent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigIterSetR4Seq()"

  subroutine ESMF_HConfigIterSetR4Seq(hconfig, content, keywordEnforcer, &
    index, keyString, doc, rc)

    type(ESMF_HConfigIter), intent(in)        :: hconfig
    real(ESMF_KIND_R4), intent(in)            :: content(:)
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    integer,            intent(out), optional :: rc

    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hconfigTemp

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    hconfigTemp = ESMF_HConfigIterAsHConfig(hconfig, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_HConfigSet(hconfigTemp, content=content, &
      index=index, keyString=keyString, doc=doc, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigSetR8()"

  subroutine ESMF_HConfigSetR8(hconfig, content, keywordEnforcer, &
    index, keyString, doc, rc)

    type(ESMF_HConfig), intent(in)            :: hconfig
    real(ESMF_KIND_R8), intent(in)            :: content
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    integer,            intent(out), optional :: rc

    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hcontent

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    hcontent = ESMF_HConfigCreate(content=content, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_HConfigSet(hconfig, hcontent, index=index, keyString=keyString, &
      doc=doc, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! clean up
    call ESMF_HConfigDestroy(hcontent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigIterSetR8()"

  subroutine ESMF_HConfigIterSetR8(hconfig, content, keywordEnforcer, &
    index, keyString, doc, rc)

    type(ESMF_HConfigIter), intent(in)        :: hconfig
    real(ESMF_KIND_R8), intent(in)            :: content
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    integer,            intent(out), optional :: rc

    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hconfigTemp

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    hconfigTemp = ESMF_HConfigIterAsHConfig(hconfig, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_HConfigSet(hconfigTemp, content=content, &
      index=index, keyString=keyString, doc=doc, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigSetR8Seq()"

  subroutine ESMF_HConfigSetR8Seq(hconfig, content, keywordEnforcer, &
    index, keyString, doc, rc)
!
! !ARGUMENTS:
    type(ESMF_HConfig), intent(in)            :: hconfig
    real(ESMF_KIND_R8), intent(in)            :: content(:)
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    integer,            intent(out), optional :: rc

    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hcontent

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    hcontent = ESMF_HConfigCreate(content=content, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_HConfigSet(hconfig, hcontent, index=index, keyString=keyString, &
      doc=doc, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! clean up
    call ESMF_HConfigDestroy(hcontent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigIterSetR8Seq()"

  subroutine ESMF_HConfigIterSetR8Seq(hconfig, content, keywordEnforcer, &
    index, keyString, doc, rc)
!
! !ARGUMENTS:
    type(ESMF_HConfigIter), intent(in)        :: hconfig
    real(ESMF_KIND_R8), intent(in)            :: content(:)
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    integer,            intent(out), optional :: rc

    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hconfigTemp

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    hconfigTemp = ESMF_HConfigIterAsHConfig(hconfig, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_HConfigSet(hconfigTemp, content=content, &
      index=index, keyString=keyString, doc=doc, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigSetString()"

  subroutine ESMF_HConfigSetString(hconfig, content, keywordEnforcer, &
    index, keyString, doc, rc)

    type(ESMF_HConfig), intent(in)            :: hconfig
    character(*),       intent(in)            :: content
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    integer,            intent(out), optional :: rc

    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hcontent

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    hcontent = ESMF_HConfigCreate(content=content, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_HConfigSet(hconfig, hcontent, index=index, keyString=keyString, &
      doc=doc, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! clean up
    call ESMF_HConfigDestroy(hcontent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigIterSetString()"

  subroutine ESMF_HConfigIterSetString(hconfig, content, keywordEnforcer, &
    index, keyString, doc, rc)

    type(ESMF_HConfigIter), intent(in)        :: hconfig
    character(*),       intent(in)            :: content
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    integer,            intent(out), optional :: rc

    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hconfigTemp

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    hconfigTemp = ESMF_HConfigIterAsHConfig(hconfig, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_HConfigSet(hconfigTemp, content=content, &
      index=index, keyString=keyString, doc=doc, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigSetStringSeq()"

  subroutine ESMF_HConfigSetStringSeq(hconfig, content, keywordEnforcer, &
    index, keyString, doc, rc)

    type(ESMF_HConfig),     intent(in)            :: hconfig
    character(len=*),       intent(in)            :: content(:)
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,                intent(in),  optional :: index
    character(*),           intent(in),  optional :: keyString
    integer,                intent(in),  optional :: doc
    integer,                intent(out), optional :: rc

    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hcontent

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    hcontent = ESMF_HConfigCreate(content=content, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_HConfigSet(hconfig, hcontent, index=index, keyString=keyString, &
      doc=doc, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! clean up
    call ESMF_HConfigDestroy(hcontent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigIterSetStringSeq()"

  subroutine ESMF_HConfigIterSetStringSeq(hconfig, content, keywordEnforcer, &
    index, keyString, doc, rc)

    type(ESMF_HConfigIter), intent(in)            :: hconfig
    character(len=*),       intent(in)            :: content(:)
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,                intent(in),  optional :: index
    character(*),           intent(in),  optional :: keyString
    integer,                intent(in),  optional :: doc
    integer,                intent(out), optional :: rc

    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hconfigTemp

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    hconfigTemp = ESMF_HConfigIterAsHConfig(hconfig, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_HConfigSet(hconfigTemp, content=content, &
      index=index, keyString=keyString, doc=doc, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
!BOP
! !IROUTINE: ESMF_HConfigSetMapKey - Set <Type> content in HConfig MapKey object

! !INTERFACE:
!  subroutine ESMF_HConfigSet(hconfig, content, keywordEnforcer, &
!    index, keyString, doc, rc)
!
! !ARGUMENTS:
!    type(ESMF_HConfigIter), intent(in)        :: hconfig
!    <Type>,             intent(in)            :: content[(:}]
!type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
!    integer,            intent(in),  optional :: index
!    character(*),       intent(in),  optional :: keyString
!    integer,            intent(in),  optional :: doc
!    integer,            intent(out), optional :: rc
!
! !DESCRIPTION:
!   Set the content of type <Type> to the {\tt hconfig} map key,
!   at the current location, or as specified by {\tt index} or {\tt keyString}
!   (mutually exclusive!).
!   Most <Type> options support the sequence array variant {\tt (:)} in
!   addition to the scalar variant.
!
!   The supported <Type> options are:
!   \begin{itemize}
!   \item {\tt type(HConfig)} (scalar only variant!)
!   \item {\tt integer(ESMF\_KIND\_I4)}
!   \item {\tt integer(ESMF\_KIND\_I8)}
!   \item {\tt logical}
!   \item {\tt real(ESMF\_KIND\_R4)}
!   \item {\tt real(ESMF\_KIND\_R8)}
!   \item {\tt character(*)}
!   \end{itemize}
!
!   The arguments are:
!   \begin{description}
!   \item[hconfig]
!     {\tt ESMF\_HConfig} or {\tt ESMF\_HConfigIter} object.
!   \item[content]
!     The content to be set.
!   \item[{[index]}]
!     Attempt to access by index if specified. Mutural exclusive with {\tt keyString}.
!   \item[{[keyString]}]
!     Attempt to access by key string if specified. Mutural exclusive with {\tt index}.
!   \item[{[doc]}]
!     The doc index. Defaults to the first document.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigSetMapKeyHConfig()"

  subroutine ESMF_HConfigSetMapKeyHConfig(hconfig, content, keywordEnforcer, &
    index, keyString, doc, rc)

    type(ESMF_HConfigIter), intent(in)        :: hconfig
    type(ESMF_HConfig), intent(in)            :: content
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    integer,            intent(out), optional :: rc

    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hconfigTemp

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigIterGetInit, hconfig, rc)

    if (present(index).or.present(keyString).or.present(doc)) then
      hconfigTemp = ESMF_HConfigCreateAtMapKey(hconfig, index=index, &
        keyString=keyString, doc=doc, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      ! Call into the C++ interface to set content
      call c_ESMC_HConfigSet(hconfigTemp, content, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      ! clean up
      call ESMF_HConfigDestroy(hconfigTemp, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    else
      ! Call into the C++ interface to set content
      call c_ESMC_HConfigSetMapKey(hconfig, content, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigSetMapKeyI4()"

  subroutine ESMF_HConfigSetMapKeyI4(hconfig, content, keywordEnforcer, &
    index, keyString, doc, rc)

    type(ESMF_HConfigIter), intent(in)            :: hconfig
    integer(ESMF_KIND_I4),  intent(in)            :: content
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,                intent(in),  optional :: index
    character(*),           intent(in),  optional :: keyString
    integer,                intent(in),  optional :: doc
    integer,                intent(out), optional :: rc

    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hcontent

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigIterGetInit, hconfig, rc)

    hcontent = ESMF_HConfigCreate(content=content, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_HConfigSetMapKey(hconfig, hcontent, index=index, keyString=keyString, &
      doc=doc, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! clean up
    call ESMF_HConfigDestroy(hcontent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigSetMapKeyI4Seq()"

  subroutine ESMF_HConfigSetMapKeyI4Seq(hconfig, content, keywordEnforcer, &
    index, keyString, doc, rc)

    type(ESMF_HConfigIter), intent(in)            :: hconfig
    integer(ESMF_KIND_I4),  intent(in)            :: content(:)
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,                intent(in),  optional :: index
    character(*),           intent(in),  optional :: keyString
    integer,                intent(in),  optional :: doc
    integer,                intent(out), optional :: rc

    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hcontent

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigIterGetInit, hconfig, rc)

    hcontent = ESMF_HConfigCreate(content=content, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_HConfigSetMapKey(hconfig, hcontent, index=index, keyString=keyString, &
      doc=doc, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! clean up
    call ESMF_HConfigDestroy(hcontent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigSetMapKeyI8()"

  subroutine ESMF_HConfigSetMapKeyI8(hconfig, content, keywordEnforcer, &
    index, keyString, doc, rc)

    type(ESMF_HConfigIter), intent(in)            :: hconfig
    integer(ESMF_KIND_I8),  intent(in)            :: content
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,                intent(in),  optional :: index
    character(*),           intent(in),  optional :: keyString
    integer,                intent(in),  optional :: doc
    integer,                intent(out), optional :: rc

    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hcontent

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigIterGetInit, hconfig, rc)

    hcontent = ESMF_HConfigCreate(content=content, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_HConfigSetMapKey(hconfig, hcontent, index=index, keyString=keyString, &
      rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! clean up
    call ESMF_HConfigDestroy(hcontent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigSetMapKeyI8Seq()"

  subroutine ESMF_HConfigSetMapKeyI8Seq(hconfig, content, keywordEnforcer, &
    index, keyString, doc, rc)

    type(ESMF_HConfigIter), intent(in)            :: hconfig
    integer(ESMF_KIND_I8),  intent(in)            :: content(:)
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,                intent(in),  optional :: index
    character(*),           intent(in),  optional :: keyString
    integer,                intent(in),  optional :: doc
    integer,                intent(out), optional :: rc

    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hcontent

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigIterGetInit, hconfig, rc)

    hcontent = ESMF_HConfigCreate(content=content, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_HConfigSetMapKey(hconfig, hcontent, index=index, keyString=keyString, &
      doc=doc, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! clean up
    call ESMF_HConfigDestroy(hcontent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigSetMapKeyLogical()"

  subroutine ESMF_HConfigSetMapKeyLogical(hconfig, content, keywordEnforcer, &
    index, keyString, doc, rc)

    type(ESMF_HConfigIter), intent(in)        :: hconfig
    logical,            intent(in)            :: content
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    integer,            intent(out), optional :: rc

    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hcontent

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigIterGetInit, hconfig, rc)

    hcontent = ESMF_HConfigCreate(content=content, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_HConfigSetMapKey(hconfig, hcontent, index=index, keyString=keyString, &
      doc=doc, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! clean up
    call ESMF_HConfigDestroy(hcontent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigSetMapKeyLogicalSeq()"
  subroutine ESMF_HConfigSetMapKeyLogicalSeq(hconfig, content, keywordEnforcer, &
    index, keyString, doc, rc)

    type(ESMF_HConfigIter), intent(in)        :: hconfig
    logical,            intent(in)            :: content(:)
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    integer,            intent(out), optional :: rc

    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hcontent

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigIterGetInit, hconfig, rc)

    hcontent = ESMF_HConfigCreate(content=content, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_HConfigSetMapKey(hconfig, hcontent, index=index, keyString=keyString, &
      doc=doc, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! clean up
    call ESMF_HConfigDestroy(hcontent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigSetMapKeyR4()"

  subroutine ESMF_HConfigSetMapKeyR4(hconfig, content, keywordEnforcer, &
    index, keyString, doc, rc)

    type(ESMF_HConfigIter), intent(in)        :: hconfig
    real(ESMF_KIND_R4), intent(in)            :: content
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    integer,            intent(out), optional :: rc

    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hcontent

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigIterGetInit, hconfig, rc)

    hcontent = ESMF_HConfigCreate(content=content, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_HConfigSetMapKey(hconfig, hcontent, index=index, keyString=keyString, &
      doc=doc, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! clean up
    call ESMF_HConfigDestroy(hcontent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigSetMapKeyR4Seq()"

  subroutine ESMF_HConfigSetMapKeyR4Seq(hconfig, content, keywordEnforcer, &
    index, keyString, doc, rc)

    type(ESMF_HConfigIter), intent(in)        :: hconfig
    real(ESMF_KIND_R4), intent(in)            :: content(:)
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    integer,            intent(out), optional :: rc

    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hcontent

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigIterGetInit, hconfig, rc)

    hcontent = ESMF_HConfigCreate(content=content, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_HConfigSetMapKey(hconfig, hcontent, index=index, keyString=keyString, &
      doc=doc, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! clean up
    call ESMF_HConfigDestroy(hcontent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigSetMapKeyR8()"

  subroutine ESMF_HConfigSetMapKeyR8(hconfig, content, keywordEnforcer, &
    index, keyString, doc, rc)

    type(ESMF_HConfigIter), intent(in)        :: hconfig
    real(ESMF_KIND_R8), intent(in)            :: content
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    integer,            intent(out), optional :: rc

    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hcontent

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigIterGetInit, hconfig, rc)

    hcontent = ESMF_HConfigCreate(content=content, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_HConfigSetMapKey(hconfig, hcontent, index=index, keyString=keyString, &
      doc=doc, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! clean up
    call ESMF_HConfigDestroy(hcontent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigSetMapKeyR8Seq()"

  subroutine ESMF_HConfigSetMapKeyR8Seq(hconfig, content, keywordEnforcer, &
    index, keyString, doc, rc)

    type(ESMF_HConfigIter), intent(in)        :: hconfig
    real(ESMF_KIND_R8), intent(in)            :: content(:)
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    integer,            intent(out), optional :: rc

    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hcontent

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigIterGetInit, hconfig, rc)

    hcontent = ESMF_HConfigCreate(content=content, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_HConfigSetMapKey(hconfig, hcontent, index=index, keyString=keyString, &
      doc=doc, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! clean up
    call ESMF_HConfigDestroy(hcontent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigSetMapKeyString()"

  subroutine ESMF_HConfigSetMapKeyString(hconfig, content, keywordEnforcer, &
    index, keyString, doc, rc)

    type(ESMF_HConfigIter), intent(in)        :: hconfig
    character(*),       intent(in)            :: content
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    integer,            intent(out), optional :: rc

    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hcontent

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigIterGetInit, hconfig, rc)

    hcontent = ESMF_HConfigCreate(content=content, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_HConfigSetMapKey(hconfig, hcontent, index=index, keyString=keyString, &
      doc=doc, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! clean up
    call ESMF_HConfigDestroy(hcontent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigSetMapKeyStringSeq()"

  subroutine ESMF_HConfigSetMapKeyStringSeq(hconfig, content, keywordEnforcer, &
    index, keyString, doc, rc)
!
! !ARGUMENTS:
    type(ESMF_HConfigIter), intent(in)        :: hconfig
    character(*),       intent(in)            :: content(:)
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    integer,            intent(out), optional :: rc

    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hcontent

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigIterGetInit, hconfig, rc)

    hcontent = ESMF_HConfigCreate(content=content, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_HConfigSetMapKey(hconfig, hcontent, index=index, keyString=keyString, &
      doc=doc, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! clean up
    call ESMF_HConfigDestroy(hcontent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
!BOP
! !IROUTINE: ESMF_HConfigSetMapVal - Set <Type> content in HConfig MapVal object

! !INTERFACE:
!  subroutine ESMF_HConfigSet(hconfig, content, keywordEnforcer, &
!    index, keyString, doc, rc)
!
! !ARGUMENTS:
!    type(ESMF_HConfigIter), intent(in)        :: hconfig
!    <Type>,             intent(in)            :: content[(:}]
!type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
!    integer,            intent(in),  optional :: index
!    character(*),       intent(in),  optional :: keyString
!    integer,            intent(in),  optional :: doc
!    integer,            intent(out), optional :: rc
!
! !DESCRIPTION:
!   Set the content of type <Type> to the {\tt hconfig} map value,
!   at the current location, or as specified by {\tt index} or {\tt keyString}
!   (mutually exclusive!).
!   Most <Type> options support the sequence array variant {\tt (:)} in
!   addition to the scalar variant.
!
!   The supported <Type> options are:
!   \begin{itemize}
!   \item {\tt type(HConfig)} (scalar only variant!)
!   \item {\tt integer(ESMF\_KIND\_I4)}
!   \item {\tt integer(ESMF\_KIND\_I8)}
!   \item {\tt logical}
!   \item {\tt real(ESMF\_KIND\_R4)}
!   \item {\tt real(ESMF\_KIND\_R8)}
!   \item {\tt character(*)}
!   \end{itemize}
!
!   The arguments are:
!   \begin{description}
!   \item[hconfig]
!     {\tt ESMF\_HConfig} or {\tt ESMF\_HConfigIter} object.
!   \item[content]
!     The content to be set.
!   \item[{[index]}]
!     Attempt to access by index if specified. Mutural exclusive with {\tt keyString}.
!   \item[{[keyString]}]
!     Attempt to access by key string if specified. Mutural exclusive with {\tt index}.
!   \item[{[doc]}]
!     The doc index. Defaults to the first document.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigSetMapValHConfig()"

  subroutine ESMF_HConfigSetMapValHConfig(hconfig, content, keywordEnforcer, &
    index, keyString, doc, rc)

    type(ESMF_HConfigIter), intent(in)        :: hconfig
    type(ESMF_HConfig), intent(in)            :: content
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    integer,            intent(out), optional :: rc

    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hconfigTemp

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigIterGetInit, hconfig, rc)

    if (present(index).or.present(keyString).or.present(doc)) then
      hconfigTemp = ESMF_HConfigCreateAtMapVal(hconfig, index=index, &
        keyString=keyString, doc=doc, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      ! Call into the C++ interface to set content
      call c_ESMC_HConfigSet(hconfigTemp, content, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      ! clean up
      call ESMF_HConfigDestroy(hconfigTemp, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    else
      ! Call into the C++ interface to set content
      call c_ESMC_HConfigSetMapVal(hconfig, content, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigSetMapValI4()"

  subroutine ESMF_HConfigSetMapValI4(hconfig, content, keywordEnforcer, &
    index, keyString, doc, rc)

    type(ESMF_HConfigIter),     intent(in)            :: hconfig
    integer(ESMF_KIND_I4),  intent(in)            :: content
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,                intent(in),  optional :: index
    character(*),           intent(in),  optional :: keyString
    integer,                intent(in),  optional :: doc
    integer,                intent(out), optional :: rc

    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hcontent

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigIterGetInit, hconfig, rc)

    hcontent = ESMF_HConfigCreate(content=content, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_HConfigSetMapVal(hconfig, hcontent, index=index, keyString=keyString, &
      doc=doc, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! clean up
    call ESMF_HConfigDestroy(hcontent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigSetMapValI4Seq()"

  subroutine ESMF_HConfigSetMapValI4Seq(hconfig, content, keywordEnforcer, &
    index, keyString, doc, rc)

    type(ESMF_HConfigIter), intent(in)            :: hconfig
    integer(ESMF_KIND_I4),  intent(in)            :: content(:)
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,                intent(in),  optional :: index
    character(*),           intent(in),  optional :: keyString
    integer,                intent(in),  optional :: doc
    integer,                intent(out), optional :: rc

    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hcontent

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigIterGetInit, hconfig, rc)

    hcontent = ESMF_HConfigCreate(content=content, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_HConfigSetMapVal(hconfig, hcontent, index=index, keyString=keyString, &
      doc=doc, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! clean up
    call ESMF_HConfigDestroy(hcontent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigSetMapValI8()"

  subroutine ESMF_HConfigSetMapValI8(hconfig, content, keywordEnforcer, &
    index, keyString, doc, rc)
!
! !ARGUMENTS:
    type(ESMF_HConfigIter),     intent(in)            :: hconfig
    integer(ESMF_KIND_I8),  intent(in)            :: content
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,                intent(in),  optional :: index
    character(*),           intent(in),  optional :: keyString
    integer,                intent(in),  optional :: doc
    integer,                intent(out), optional :: rc

    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hcontent

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigIterGetInit, hconfig, rc)

    hcontent = ESMF_HConfigCreate(content=content, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_HConfigSetMapVal(hconfig, hcontent, index=index, keyString=keyString, &
      doc=doc, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! clean up
    call ESMF_HConfigDestroy(hcontent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigSetMapValI8Seq()"

  subroutine ESMF_HConfigSetMapValI8Seq(hconfig, content, keywordEnforcer, &
    index, keyString, doc, rc)

    type(ESMF_HConfigIter), intent(in)            :: hconfig
    integer(ESMF_KIND_I8),  intent(in)            :: content(:)
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,                intent(in),  optional :: index
    character(*),           intent(in),  optional :: keyString
    integer,                intent(in),  optional :: doc
    integer,                intent(out), optional :: rc

    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hcontent

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigIterGetInit, hconfig, rc)

    hcontent = ESMF_HConfigCreate(content=content, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_HConfigSetMapVal(hconfig, hcontent, index=index, keyString=keyString, &
      doc=doc, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! clean up
    call ESMF_HConfigDestroy(hcontent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigSetMapValLogical()"

  subroutine ESMF_HConfigSetMapValLogical(hconfig, content, keywordEnforcer, &
    index, keyString, doc, rc)

    type(ESMF_HConfigIter), intent(in)        :: hconfig
    logical,            intent(in)            :: content
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    integer,            intent(out), optional :: rc

    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hcontent

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigIterGetInit, hconfig, rc)

    hcontent = ESMF_HConfigCreate(content=content, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_HConfigSetMapVal(hconfig, hcontent, index=index, keyString=keyString, &
      doc=doc, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! clean up
    call ESMF_HConfigDestroy(hcontent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigSetMapValLogicalSeq()"

  subroutine ESMF_HConfigSetMapValLogicalSeq(hconfig, content, keywordEnforcer, &
    index, keyString, doc, rc)
!
! !ARGUMENTS:
    type(ESMF_HConfigIter), intent(in)        :: hconfig
    logical,            intent(in)            :: content(:)
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    integer,            intent(out), optional :: rc

    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hcontent

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigIterGetInit, hconfig, rc)

    hcontent = ESMF_HConfigCreate(content=content, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_HConfigSetMapVal(hconfig, hcontent, index=index, keyString=keyString, &
      doc=doc, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! clean up
    call ESMF_HConfigDestroy(hcontent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigSetMapValR4()"

  subroutine ESMF_HConfigSetMapValR4(hconfig, content, keywordEnforcer, &
    index, keyString, doc, rc)

    type(ESMF_HConfigIter), intent(in)        :: hconfig
    real(ESMF_KIND_R4), intent(in)            :: content
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    integer,            intent(out), optional :: rc

    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hcontent

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigIterGetInit, hconfig, rc)

    hcontent = ESMF_HConfigCreate(content=content, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_HConfigSetMapVal(hconfig, hcontent, index=index, keyString=keyString, &
      doc=doc, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! clean up
    call ESMF_HConfigDestroy(hcontent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigSetMapValR4Seq()"

  subroutine ESMF_HConfigSetMapValR4Seq(hconfig, content, keywordEnforcer, &
    index, keyString, doc, rc)

    type(ESMF_HConfigIter), intent(in)        :: hconfig
    real(ESMF_KIND_R4), intent(in)            :: content(:)
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    integer,            intent(out), optional :: rc

    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hcontent

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigIterGetInit, hconfig, rc)

    hcontent = ESMF_HConfigCreate(content=content, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_HConfigSetMapVal(hconfig, hcontent, index=index, keyString=keyString, &
      doc=doc, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! clean up
    call ESMF_HConfigDestroy(hcontent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigSetMapValR8()"

  subroutine ESMF_HConfigSetMapValR8(hconfig, content, keywordEnforcer, &
    index, keyString, doc, rc)

    type(ESMF_HConfigIter), intent(in)        :: hconfig
    real(ESMF_KIND_R8), intent(in)            :: content
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    integer,            intent(out), optional :: rc

    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hcontent

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigIterGetInit, hconfig, rc)

    hcontent = ESMF_HConfigCreate(content=content, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_HConfigSetMapVal(hconfig, hcontent, index=index, keyString=keyString, &
      doc=doc, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! clean up
    call ESMF_HConfigDestroy(hcontent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigSetMapValR8Seq()"

  subroutine ESMF_HConfigSetMapValR8Seq(hconfig, content, keywordEnforcer, &
    index, keyString, doc, rc)

    type(ESMF_HConfigIter), intent(in)        :: hconfig
    real(ESMF_KIND_R8), intent(in)            :: content(:)
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    integer,            intent(out), optional :: rc

    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hcontent

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigIterGetInit, hconfig, rc)

    hcontent = ESMF_HConfigCreate(content=content, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_HConfigSetMapVal(hconfig, hcontent, index=index, keyString=keyString, &
      doc=doc, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! clean up
    call ESMF_HConfigDestroy(hcontent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigSetMapValString()"

  subroutine ESMF_HConfigSetMapValString(hconfig, content, keywordEnforcer, &
    index, keyString, doc, rc)

    type(ESMF_HConfigIter), intent(in)        :: hconfig
    character(*),       intent(in)            :: content
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    integer,            intent(out), optional :: rc

    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hcontent

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigIterGetInit, hconfig, rc)

    hcontent = ESMF_HConfigCreate(content=content, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_HConfigSetMapVal(hconfig, hcontent, index=index, keyString=keyString, &
      doc=doc, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! clean up
    call ESMF_HConfigDestroy(hcontent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigSetMapValStringSeq()"

  subroutine ESMF_HConfigSetMapValStringSeq(hconfig, content, keywordEnforcer, &
    index, keyString, doc, rc)

    type(ESMF_HConfigIter), intent(in)        :: hconfig
    character(*),       intent(in)            :: content(:)
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    integer,            intent(out), optional :: rc

    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hcontent

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigIterGetInit, hconfig, rc)

    hcontent = ESMF_HConfigCreate(content=content, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_HConfigSetMapVal(hconfig, hcontent, index=index, keyString=keyString, &
      doc=doc, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! clean up
    call ESMF_HConfigDestroy(hcontent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine
!------------------------------------------------------------------------------


! -------------------------- ESMF-internal method -----------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigGetInit"
!BOPI
! !IROUTINE: ESMF_HConfigGetInit - Internal access routine for init code
!
! !INTERFACE:
  function ESMF_HConfigGetInit(hconfig)
!
! !RETURN VALUE:
    ESMF_INIT_TYPE :: ESMF_HConfigGetInit
!
! !ARGUMENTS:
    type(ESMF_HConfig), intent(in), optional :: hconfig
!
! !DESCRIPTION:
!   Access deep object init code.
!
!   The arguments are:
!   \begin{description}
!   \item [hconfig]
!     HConfig object.
!   \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    if (present(hconfig)) then
      ESMF_HConfigGetInit = ESMF_INIT_GET(hconfig)
    else
      ESMF_HConfigGetInit = ESMF_INIT_CREATED
    endif

  end function ESMF_HConfigGetInit
!------------------------------------------------------------------------------

! -------------------------- ESMF-internal method -----------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigIterGetInit"
!BOPI
! !IROUTINE: ESMF_HConfigIterGetInit - Internal access routine for init code
!
! !INTERFACE:
  function ESMF_HConfigIterGetInit(hconfig)
!
! !RETURN VALUE:
    ESMF_INIT_TYPE :: ESMF_HConfigIterGetInit
!
! !ARGUMENTS:
    type(ESMF_HConfigIter), intent(in), optional :: hconfig
!
! !DESCRIPTION:
!   Access deep object init code.
!
!   The arguments are:
!   \begin{description}
!   \item [hconfig]
!     HConfig object.
!   \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    if (present(hconfig)) then
      ESMF_HConfigIterGetInit = ESMF_INIT_GET(hconfig)
    else
      ESMF_HConfigIterGetInit = ESMF_INIT_CREATED
    endif

  end function ESMF_HConfigIterGetInit
!------------------------------------------------------------------------------

end module ESMF_HConfigMod
