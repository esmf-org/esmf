! $Id: ESMF_RegridBilinear.F90,v 1.1 2003/04/08 16:53:53 pwjones Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2003, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the GPL.
!
!==============================================================================
!
!     ESMF Bilinear Regrid Module
      module ESMF_RegridBilinearMod
!
!==============================================================================
!
! This file contains the Regrid class methods for bilinear regridding.
!
!------------------------------------------------------------------------------
! INCLUDES
#include <ESMF_Macros.inc>
!==============================================================================
!BOP
! !MODULE: ESMF_RegridBilinearMod - Bilinear interpolation
!
! !DESCRIPTION:
!
! The code in this file implements the bilinear methods for the ESMF Regrid
! class.
!
!------------------------------------------------------------------------------
! !USES:
      use ESMF_BaseMod      ! ESMF base   class
      use ESMF_ArrayMod     ! ESMF array  class
      use ESMF_FieldMod     ! ESMF field  class
      use ESMF_BundleMod    ! ESMF bundle class
      use ESMF_GridMod      ! ESMF grid   class
      use ESMF_PhysGridMod  ! ESMF physical grid class
      use ESMF_DistGridMod  ! ESMF distributed grid class
      use ESMF_RegridTypesMod ! ESMF regrid data structures
      implicit none

!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:
!

    public ESMF_RegridCreateBilinear ! create and fill a regrid object
                                     ! for a bilinear regridding

!
!EOP

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_RegridBilinear.F90,v 1.1 2003/04/08 16:53:53 pwjones Exp $'

!==============================================================================
!
! INTERFACE BLOCKS
!
!==============================================================================
!BOP
! !INTERFACE:
      interface ESMF_RegridCreateBilinear

! !PRIVATE MEMBER FUNCTIONS:
         module procedure ESMF_RegridCreateFromFieldBilinear
         module procedure ESMF_RegridCreateFromBundleBilinear

! !DESCRIPTION:
!     This interface provides a single entry to the Regrid create methods 
!     specifically for a bilinear regridding. 
!
!EOP
      end interface
!
!==============================================================================

      contains

!==============================================================================
!
! This section includes the bilinear Regrid Create methods.
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE:
!     ESMF_RegridCreateFromFieldBilinear - Creates Regrid structure for a field pair

! !INTERFACE:
      function ESMF_RegridCreateFromFieldBilinear(src_field, dst_field, &
                                                  name, rc)
!
! !RETURN VALUE:
      type(ESMF_Regrid) :: ESMF_RegridCreateFromFieldBilinear
!
! !ARGUMENTS:
      character (len = *), intent(in) :: name

      type (ESMF_Field), intent(in) :: &
         src_field,          &! field to be regridded
         dst_field            ! destination (incl grid) of resulting regridded field

      integer, intent(out) :: rc  ! return code to flag errors
!
! !DESCRIPTION:
!     Given a source field and destination field (and their attached
!     grids), this routine allocates memory for a new {\tt Regrid} object
!     and fills it with information necessary for regridding the source
!     field to the destination field using a bilinear interpolation.  
!     Returns a pointer to a new {\tt Regrid}.
!
!     The arguments are:
!     \begin{description}
!     \item[name]
!          {\tt Regrid} name.
!     \item[src\_field]
!          Field to be regridded.
!     \item[dst\_field]
!          Resultant field where regridded source field will be stored.
!     \item[[rc]]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
! !REQUIREMENTS:  TODO
!EOP

      type(ESMF_Regrid), pointer :: regrid  ! pointer to new regrid object

!     Initialize pointers
      nullify(regrid)
      nullify(ESMF_RegridCreateFromFieldBilinear%ptr)

!     Initialize return code
      rc = ESMF_FAILURE

!     No allocation here - construct must perform all necessary allocation
!     Call construction method to allocate and initialize grid internals.

      call ESMF_RegridConstructFromFieldBilinear(regrid, src_field, dst_field, &
                                                 name, rc)

!     Set return values.
      if (rc /= ESMF_SUCCESS) then
         ! Use error function eventually...
         print *, "ERROR in ESMF_RegridCreateFromFieldBilinear: Regrid construct"
      else
         ESMF_RegridCreateFromFieldBilinear%ptr => regrid
      endif

      end function ESMF_RegridCreateFromFieldBilinear

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:
!     ESMF_RegridCreateFromBundleBilinear - Creates bilinear Regrid structure for a bundle pair

! !INTERFACE:
      function ESMF_RegridCreateFromBundle(src_bundle, dst_bundle, name, rc)
!
! !RETURN VALUE:
      type(ESMF_Regrid) :: ESMF_RegridCreateFromBundleBilinear
!
! !ARGUMENTS:
      type (ESMF_Bundle), intent(in) :: &
         src_bundle,          &! bundle of fields to be regridded
         dst_bundle            ! destination (incl grid) of resulting regridded field

      character (len = *), intent(in) :: name

      integer, intent(out) :: rc
!
! !DESCRIPTION:
!     Given a source field bundle and destination field bundle (and attached
!     grids), this routine allocates memory for a new {\tt Regrid} object
!     and fills it with information necessary for regridding the source
!     bundle to the destination bundle using bilinear interpolation.  
!     Returns a pointer to a new {\tt Regrid}.
!
!     The arguments are:
!     \begin{description}
!     \item[name]
!          {\tt Regrid} name.
!     \item[src\_bundle]
!          Field bundle to be regridded.
!     \item[dst\_bundle]
!          Resultant field bundle where regridded source bundle will be stored.
!     \item[[rc]]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
! !REQUIREMENTS:  TODO
!EOP

      type(ESMF_Regrid), pointer :: regrid  ! pointer to new regrid object

!     Initialize pointers
      nullify(regrid)
      nullify(ESMF_RegridCreateFromBundleBilinear%ptr)

!     Initialize return code
      rc = ESMF_FAILURE

!     No allocation here - construct must perform all necessary allocation
!     Call construction method to allocate and initialize grid internals.

      call ESMF_RegridConstructFromBundleBilinear(regrid, src_bundle, dst_bundle, &
                                                  name, rc)

!     Set return values.
      if (rc /= ESMF_SUCCESS) then
         ! Use error function eventually...
         print *, "ERROR in ESMF_RegridCreateFromBundle: Regrid construct"
      else
         ESMF_RegridCreateFromBundle%ptr => regrid
      endif

      end function ESMF_RegridCreateFromBundleBilinear

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:
!     ESMF_RegridConstructFromFieldBilinear - Constructs bilinear Regrid structure for a field pair

! !INTERFACE:
      subroutine ESMF_RegridConstructFromField(regrid, src_field, dst_field, &
                                               name, rc)
!
! !ARGUMENTS:

      type(ESMF_RegridType), intent(out) :: regrid  ! newly created regrid

      type (ESMF_Field), intent(in) :: &
         src_field,          &! field to be regridded
         dst_field            ! destination (incl grid) of resulting regridded field

      character (len = *), intent(in) :: name

      integer, intent(out) :: rc
!
! !DESCRIPTION:
!     Given a source field and destination field (and their attached
!     grids), this routine constructs a new {\tt Regrid} object
!     and fills it with information necessary for regridding the source
!     field to the destination field using a bilinear interpolation.  
!     Returns a pointer to a new {\tt Regrid}.
!
!     The arguments are:
!     \begin{description}
!     \item[src\_field]
!          Field to be regridded.
!     \item[dst\_field]
!          Resultant field where regridded source field will be stored.
!     \item[[name]]
!          {\tt Regrid} name.
!     \item[[rc]]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
! !REQUIREMENTS:  TODO
!EOP

!     Initialize pointers
      nullify(regrid%src_bundle)
      nullify(regrid%dst_bundle)
      nullify(regrid%src_field)
      nullify(regrid%dst_field)
      nullify(regrid%src_address)
      nullify(regrid%dst_address)
      nullify(regrid%weights)
      nullify(regrid%gather)

!     Initialize return code
      rc = ESMF_FAILURE

!     set name
      regrid%name = name

!     set other pointers
      regrid%src_field => src_field
      regrid%dst_field => dst_field

!     Set method
      regrid%method = ESMF_RegridMethod_Bilinear

      !TODO
      ! calculate the actual regridding here including:
      !    src_address,      &! addresses of source field for regrid operation
      !    dst_address       &! addresses of destination field for regrid op
      !    weights            ! array of weights for performing the
      !    comm_before,      &! pre-stored communication patterns necessary for
      !    comm_after         ! doing any data motion required for

      !if (status /= ESMF_SUCCESS) then
      !  ! Use error function eventually...
      !  print *, "ERROR in ESMF_RegridCreateFromField: Regrid construct"
      !  return
      !endif

!     Set return values.
      rc = ESMF_SUCCESS

      end subroutine ESMF_RegridConstructFromFieldBilinear

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:
!     ESMF_RegridConstructFromBundleBilinear - Constructs bilinear Regrid structure for a bundle pair

! !INTERFACE:
      subroutine ESMF_RegridConstructFromBundle(regrid, src_bundle, dst_bundle, &
                                                name, rc)
!
! !ARGUMENTS:

      type(ESMF_RegridType), intent(out) :: regrid  ! newly created regrid

      type (ESMF_Bundle), intent(in) :: &
         src_bundle,          &! field bundle to be regridded
         dst_bundle            ! destination (incl grid) of resulting regridded bundle

      character (len = *), intent(in) :: name

      integer, intent(out) :: rc
!
! !DESCRIPTION:
!     Given a source field bundle and destination field bundle (and their attached
!     grids), this routine constructs a new {\tt Regrid} object
!     and fills it with information necessary for regridding the source
!     bundle to the destination bundle using bilinear interpolation.  Returns a 
!     pointer to a new {\tt Regrid}.
!
!     The arguments are:
!     \begin{description}
!     \item[src\_bundle]
!          Field to be regridded.
!     \item[dst\_bundle]
!          Resultant field where regridded source field will be stored.
!     \item[[name]]
!          {\tt Regrid} name.
!     \item[[rc]]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
! !REQUIREMENTS:  TODO
!EOP


!     Initialize pointers
      nullify(regrid%src_bundle)
      nullify(regrid%dst_bundle)
      nullify(regrid%src_field)
      nullify(regrid%dst_field)
      nullify(regrid%src_address)
      nullify(regrid%dst_address)
      nullify(regrid%weights)
      nullify(regrid%gather)

!     Initialize return code
      rc = ESMF_FAILURE

!     Set name
      regrid%name = name

!     Set pointers to bundles
      !TODO - check consistency of bundle pair
      regrid%src_bundle => src_bundle
      regrid%dst_bundle => dst_bundle

!     Set method
      regrid%method = ESMF_RegridMethod_Bilinear

      !TODO
      ! calculate the actual regridding here including:
      !    src_address,      &! addresses of source field for regrid operation
      !    dst_address       &! addresses of destination field for regrid op
      !    weights            ! array of weights for performing the
      !    comm_before,      &! pre-stored communication patterns necessary for
      !    comm_after         ! doing any data motion required for

      !if (status /= ESMF_SUCCESS) then
      !  ! Use error function eventually...
      !  print *, "ERROR in ESMF_RegridCreateFromField: Regrid construct"
      !  return
      !endif

!     Set return values.
      rc = ESMF_SUCCESS

      end subroutine ESMF_RegridConstructFromBundleBilinear

!------------------------------------------------------------------------------

   end module ESMF_RegridBilinearMod
