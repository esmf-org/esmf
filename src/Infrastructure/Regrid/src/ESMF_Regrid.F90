! $Id: ESMF_Regrid.F90,v 1.11 2003/04/08 16:53:52 pwjones Exp $
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
!     ESMF Regrid Module
      module ESMF_RegridMod
!
!==============================================================================
!
! This file contains most of the Regrid class methods.  The Regrid data type
! and some utility functions are contained in RegridTypesMod.
!
!------------------------------------------------------------------------------
! INCLUDES
#include <ESMF_Macros.inc>
!==============================================================================
!BOP
! !MODULE: ESMF_RegridMod - Regridding and interpolation
!
! !DESCRIPTION:
!
! The code in this file implements most of the Regrid class methods.  Regrid 
! is responsible for any regridding and interpolation required for ESMF 
! applications.
! Regridding includes any process that transforms a field from one ESMF
! grid to another, including:
! \begin{itemize}
! \item bilinear or bicubic interpolation
! \item conservative remapping
! \item spectral or other functional transforms
! \item sub-sampling, super-sampling or shifting.
! \end{itemize}
!
!------------------------------------------------------------------------------
! !USES:
      use ESMF_BaseMod         ! ESMF base  class
      use ESMF_ArrayMod        ! ESMF array class
      use ESMF_FieldMod        ! ESMF field class
      use ESMF_BundleMod       ! ESMF bundle class
      use ESMF_GridMod         ! ESMF grid  class
      use ESMF_PhysGridMod     ! ESMF physical grid class
      use ESMF_DistGridMod     ! ESMF distributed grid class
      use ESMF_RegridTypesMod  ! ESMF regrid data types and utilities
      use ESMF_RegridBilinMod  ! ESMF regrid methods related to bilinear regrid

      implicit none

!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:
!

    public ESMF_RegridCreate      ! create and fill a regrid object
    public ESMF_RegridDo          ! perform a regrid operation

!
!EOP
!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_Regrid.F90,v 1.11 2003/04/08 16:53:52 pwjones Exp $'

!==============================================================================
!
! INTERFACE BLOCKS
!
!==============================================================================
!BOP
! !INTERFACE:
      interface ESMF_RegridCreate

! !PRIVATE MEMBER FUNCTIONS:
         module procedure ESMF_RegridCreateFromField
         module procedure ESMF_RegridCreateFromBundle
         
         !From ESMF_RegridTypesMod
         function ESMF_RegridCreateFromRegrid(old_regrid, method, &
                                              name, index_shift, rc)
            type(ESMF_Regrid) :: ESMF_RegridCreateFromRegrid
            type (ESMF_Regrid), intent(in) :: old_regrid 
            integer, intent(in) :: method
            character (len = *), intent(in), optional :: name
            type (ESMF_Array), intent(in), optional :: index_shift
            integer, intent(out), optional :: rc
         end
!
! !DESCRIPTION:
!     This interface provides a single entry point for various Regrid create
!     methods, including a regrid copied from an existing regrid, a regrid
!     created from a field pair, a regrid created from input field bundles
!     and a regrid created by shifting addresses of an existing regrid.
!
!EOP
      end interface
!
!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      interface ESMF_RegridDo

! !PRIVATE MEMBER FUNCTIONS:
         module procedure ESMF_RegridDoField
         module procedure ESMF_RegridDoBundle
         !module procedure ESMF_RegridDoArray

! !DESCRIPTION:
!     This interface provides a single entry point for performing the
!     actual regridding operation.
!
!EOP
      end interface
!
!==============================================================================

      contains

!==============================================================================
!
! This section includes some of the Regrid Create methods.
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE:
!     ESMF_RegridCreateFromField - Creates Regrid structure for a field pair

! !INTERFACE:
      function ESMF_RegridCreateFromField(src_field, dst_field, method, name, rc)
!
! !RETURN VALUE:
      type(ESMF_Regrid) :: ESMF_RegridCreateFromField
!
! !ARGUMENTS:
      character (len = *), intent(in), optional :: name

      type (ESMF_Field), intent(in) :: &
         src_field,          &! field to be regridded
         dst_field            ! destination (incl grid) of resulting regridded field

      integer, intent(in) :: method   ! method to use for regridding

      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Given a source field and destination field (and their attached
!     grids), this routine allocates memory for a new {\tt Regrid} object
!     and fills it with information necessary for regridding the source
!     field to the destination field.  Returns a pointer to a new {\tt Regrid}.
!     This routine is actually a shell which calls individual create routines
!     based on regrid method choice.
!
!     The arguments are:
!     \begin{description}
!     \item[name]
!          {\tt Regrid} name.
!     \item[src\_field]
!          Field to be regridded.
!     \item[dst\_field]
!          Resultant field where regridded source field will be stored.
!     \item[method]
!          Method to use for regridding.
!     \item[[rc]]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!   The supported regridding methods are currently:
!   \begin{description}
!   \item[ESMF\_RegridMethod\_Bilinear] bilinear (logically-rectangular grids)
!   \item[ESMF\_RegridMethod\_Bicubic ] bicubic  (logically-rectangular grids)
!   \item[ESMF\_RegridMethod\_Conserv1] first-order conservative
!   \item[ESMF\_RegridMethod\_Conserv2] second-order conservative
!   \item[ESMF\_RegridMethod\_Raster  ] regrid by rasterizing domain
!   \item[ESMF\_RegridMethod\_NearNbr ] nearest-neighbor distance-weighted average
!   \item[ESMF\_RegridMethod\_Fourier ] Fourier transform
!   \item[ESMF\_RegridMethod\_Legendre] Legendre transform
!   \item[ESMF\_RegridMethod\_Index   ] index-space regridding (shift, stencil)
!   \item[ESMF\_RegridMethod\_Linear  ] linear for 1-d regridding
!   \item[ESMF\_RegridMethod\_Spline  ] cubic spline for 1-d regridding
!   \item[ESMF\_RegridMethod\_User    ] user-supplied method
!   \end{description}
!
! !REQUIREMENTS:  TODO
!EOP

      integer :: status=ESMF_FAILURE              ! Error status
      logical :: rcpresent=.FALSE.                ! Return code present
      character (*) :: regrid_name

!     Initialize return code
      if(present(rc)) then
        rcpresent=.TRUE.
        rc = ESMF_FAILURE
      endif

!     You have a right to define a name.  If you do not choose a name,
!     one will be appointed for you.

      if(.not. present(name)) then
         regrid_name = 'MyRegrid'  ! TODO: create a name here
      else
         regrid_name = name
      endif

!     Initialize pointers
      nullify(ESMF_RegridCreateFromField%ptr)

!     Call the appropriate create routine based on method choice
      select case(method)
      case(ESMF_RegridMethod_Bilinear) ! bilinear
         ESMF_RegridCreateFromField = &
         ESMF_RegridCreateBilin(src_field, dst_field, regrid_name, status)
      case(ESMF_RegridMethod_Bicubic)  ! bicubic
         print *, "ERROR in ESMF_RegridCreateFromField: ", &
                  "Bicubic not yet supported"
         return
      case(ESMF_RegridMethod_Conserv1)
         print *, "ERROR in ESMF_RegridCreateFromField: ", &
                  "1st-order conservative not yet supported"
         return
      case(ESMF_RegridMethod_Conserv2) ! 2nd-order conservative
         print *, "ERROR in ESMF_RegridCreateFromField: ", &
                  "2nd-order conservative not yet supported"
         return
      case(ESMF_RegridMethod_Raster) ! regrid by rasterizing domain
         print *, "ERROR in ESMF_RegridCreateFromField: ", &
                  "Raster method not yet supported"
         return
      case(ESMF_RegridMethod_NearNbr) ! nearest-neighbor dist-weighted avg
         print *, "ERROR in ESMF_RegridCreateFromField: ", &
                  "Nearest-neighbor method not yet supported"
         return
      case(ESMF_RegridMethod_Fourier) ! Fourier transform
         print *, "ERROR in ESMF_RegridCreateFromField: ", &
                  "Fourier transforms not yet supported"
         return
      case(ESMF_RegridMethod_Legendre) ! Legendre transform
         print *, "ERROR in ESMF_RegridCreateFromField: ", &
                  "Legendre transforms not yet supported"
         return
      case(ESMF_RegridMethod_Index) ! index-space regridding (shift, stencil)
         print *, "ERROR in ESMF_RegridCreateFromField: ", &
                  "Index-space methods not yet supported"
         return
      case(ESMF_RegridMethod_Linear) ! linear for 1-d regridding
         print *, "ERROR in ESMF_RegridCreateFromField: ", &
                  "1-d linear methods not yet supported"
         return
      case(ESMF_RegridMethod_Spline) ! cubic spline for 1-d regridding
         print *, "ERROR in ESMF_RegridCreateFromField: ", &
                  "1-d cubic splines not yet supported"
         return
      case(ESMF_RegridMethod_User) ! cubic spline for 1-d regridding
         print *, "ERROR in ESMF_RegridCreateFromField: ", &
                  "User-defined regridding not yet supported"
         return
      case default
         print *, "ERROR in ESMF_RegridCreateFromField: Invalid method"
         return
      end select

      if (status /= ESMF_SUCCESS) then
         ! Use error function eventually...
         print *, "ERROR in ESMF_RegridCreateFromField: error in creation"
         if (rcpresent) rc = status
      else
         if (rcpresent) rc = ESMF_SUCCESS
      endif

      end function ESMF_RegridCreateFromField

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:
!     ESMF_RegridCreateFromBundle - Creates Regrid structure for a bundle pair

! !INTERFACE:
      function ESMF_RegridCreateFromBundle(src_bundle, dst_bundle, method, name, rc)
!
! !RETURN VALUE:
      type(ESMF_Regrid) :: ESMF_RegridCreateFromBundle
!
! !ARGUMENTS:

      type (ESMF_Bundle), intent(in) :: &
         src_bundle,      &! field bundle to be regridded
         dst_bundle        ! destination (incl grid) of resulting regridded field

      integer, intent(in) :: method   ! method to use for regridding

      character (len = *), intent(in), optional :: name

      integer, intent(out), optional :: rc

!
! !DESCRIPTION:
!     Given a source field bundle and destination field bundle (and attached
!     grids), this routine allocates memory for a new {\tt Regrid} object
!     and fills it with information necessary for regridding the source
!     bundle to the destination bundle.  Returns a pointer to a new {\tt Regrid}.
!     This routine is actually a shell which calls individual create routines
!     based on the input regrid method choice.
!
!     The arguments are:
!     \begin{description}
!     \item[name]
!          {\tt Regrid} name.
!     \item[src\_bundle]
!          Field bundle to be regridded.
!     \item[dst\_bundle]
!          Resultant field bundle where regridded source bundle will be stored.
!     \item[method]
!          Method to use for regridding.
!     \item[[rc]]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!   The supported regridding methods for this create routine are currently:
!   \begin{description}
!   \item[ESMF\_RegridMethod\_Bilinear] bilinear (logically-rectangular grids)
!   \item[ESMF\_RegridMethod\_Bicubic ] bicubic  (logically-rectangular grids)
!   \item[ESMF\_RegridMethod\_Conserv1] first-order conservative
!   \item[ESMF\_RegridMethod\_Conserv2] second-order conservative
!   \item[ESMF\_RegridMethod\_Raster  ] regrid by rasterizing domain
!   \item[ESMF\_RegridMethod\_NearNbr ] nearest-neighbor distance-weighted average
!   \item[ESMF\_RegridMethod\_Fourier ] Fourier transform
!   \item[ESMF\_RegridMethod\_Legendre] Legendre transform
!   \item[ESMF\_RegridMethod\_Index   ] index-space regridding (shift, stencil)
!   \item[ESMF\_RegridMethod\_Linear  ] linear for 1-d regridding
!   \item[ESMF\_RegridMethod\_Spline  ] cubic spline for 1-d regridding
!   \item[ESMF\_RegridMethod\_User    ] user-supplied method
!   \end{description}
!
! !REQUIREMENTS:  TODO
!EOP

      integer :: status=ESMF_FAILURE              ! Error status
      logical :: rcpresent=.FALSE.                ! Return code present
      character (*) :: regrid_name

!     Initialize return code
      if(present(rc)) then
        rcpresent=.TRUE.
        rc = ESMF_FAILURE
      endif

!     You have a right to define a name.  If you do not choose a name,
!     one will be appointed for you.

      if(.not. present(name)) then
         regrid_name = 'MyRegrid'  ! TODO: create a name here
      else
         regrid_name = name
      endif

!     Initialize pointers
      nullify(ESMF_RegridCreateFromBundle%ptr)

!     Call the appropriate create routine based on method choice

      select case(method)
      case(ESMF_RegridMethod_Bilinear) ! bilinear
         ESMF_RegridCreateFromBundle = &
         ESMF_RegridCreateBilin(src_bundle, dst_bundle, regrid_name, status)
      case(ESMF_RegridMethod_Bicubic)  ! bicubic
         print *, "ERROR in ESMF_RegridCreateFromBundle: ", &
                  "Bicubic not yet supported"
         return
      case(ESMF_RegridMethod_Conserv1)
         print *, "ERROR in ESMF_RegridCreateFromBundle: ", &
                  "1st-order conservative not yet supported"
         return
      case(ESMF_RegridMethod_Conserv2) ! 2nd-order conservative
         print *, "ERROR in ESMF_RegridCreateFromBundle: ", &
                  "2nd-order conservative not yet supported"
         return
      case(ESMF_RegridMethod_Raster) ! regrid by rasterizing domain
         print *, "ERROR in ESMF_RegridCreateFromBundle: ", &
                  "Raster method not yet supported"
         return
      case(ESMF_RegridMethod_NearNbr) ! nearest-neighbor dist-weighted avg
         print *, "ERROR in ESMF_RegridCreateFromBundle: ", &
                  "Nearest-neighbor method not yet supported"
         return
      case(ESMF_RegridMethod_Fourier) ! Fourier transform
         print *, "ERROR in ESMF_RegridCreateFromBundle: ", &
                  "Fourier transforms not yet supported"
         return
      case(ESMF_RegridMethod_Legendre) ! Legendre transform
         print *, "ERROR in ESMF_RegridCreateFromBundle: ", &
                  "Legendre transforms not yet supported"
         return
      case(ESMF_RegridMethod_Index) ! index-space regridding (shift, stencil)
         print *, "ERROR in ESMF_RegridCreateFromBundle: ", &
                  "Index-space methods not yet supported"
         return
      case(ESMF_RegridMethod_Linear) ! linear for 1-d regridding
         print *, "ERROR in ESMF_RegridCreateFromBundle: ", &
                  "1-d linear methods not yet supported"
         return
      case(ESMF_RegridMethod_Spline) ! cubic spline for 1-d regridding
         print *, "ERROR in ESMF_RegridCreateFromBundle: ", &
                  "1-d cubic splines not yet supported"
         return
      case(ESMF_RegridMethod_User) ! cubic spline for 1-d regridding
         print *, "ERROR in ESMF_RegridCreateFromBundle: ", &
                  "User-defined regridding not yet supported"
         return
      case default
         print *, "ERROR in ESMF_RegridCreateFromBundle: Invalid method"
         return
      end select

!     Set return values.
!     ESMF_RegridCreateFromBundle set by above calls

      if (status /= ESMF_SUCCESS) then
         ! Use error function eventually...
         print *, "ERROR in ESMF_RegridCreateFromBundle: create functions"
         if (rcpresent) rc = status
      else
         if (rcpresent) rc = ESMF_SUCCESS
      endif

      end function ESMF_RegridCreateFromBundle

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:
!     ESMF_RegridDoField - Performs a regridding between two fields

! !INTERFACE:
      subroutine ESMF_RegridDoField(src_field, dst_field, regrid, rc)
!
! !ARGUMENTS:

      type (ESMF_Field), intent(in) :: &
         src_field,           ! field to be regridded
         
      type (ESMF_Regrid), intent(in) :: &
         regrid               ! precomputed regrid structure with
                              !  regridding info

      type (ESMF_Field), intent(out) :: &
         dst_field            ! resulting regridded field

      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Given a source field and precomputed regridding information, this 
!     routine regrids the source field to a new field on the destination
!     grid.  
!
! !REQUIREMENTS:  TODO
!EOP

   !type (ESMF_Array) ::
   !   src_data   ! source data necessary for regrid gathered
   !              !  from potentially distributed source field

   !*** gather remote data to local array

   ! allocate local gathered source array
   ! use route to gather data
      
   !*** initialize dest field to zero
   
   !dst_field = zero

   !*** do the regrid

   ! will look something like   
   !do n=1,num_links
   !   dst_field(regrid%dst_add(n)) = dst_field(regrid%dst_add(n)) + &
   !                                  src_field(regrid%src_add(n))* &
   !                                  regrid%weights(n)
   !end do

   ! set return codes

   end subroutine ESMF_RegridDoField

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:
!     ESMF_RegridDoBundle - Performs a regridding between two bundles of fields

! !INTERFACE:
      subroutine ESMF_RegridDoBundle(src_bundle, dst_bundle, regrid, rc)
!
! !ARGUMENTS:

      type (ESMF_Bundle), intent(in) :: &
         src_bundle,           ! bundle of fields to be regridded
         
      type (ESMF_Regrid), intent(in) :: &
         regrid               ! precomputed regrid structure with
                              !  regridding info

      type (ESMF_Bundle), intent(out) :: &
         dst_bundle            ! resulting regridded bundle of fields

      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Given a bundle of source fields and precomputed regridding information, 
!     this routine regrids the bundle of source fields to a bundle of new 
!     fields on the destination grid.  
!
! !REQUIREMENTS:  TODO
!EOP

   !type (ESMF_Array) ::
   !   src_data   ! source data necessary for regrid gathered
   !              !  from potentially distributed source field

   !*** gather remote data to local array

   ! allocate local gathered source array
   ! must check for packed bundles, etc.
   ! use route to gather data


   !*** regrid every field in the bundle
   ! this will look something like
   !do ifield = 1,num_fields
   
      !*** initialize dest field to zero
   
      !dst_field = zero

      !*** do the regrid
   
      !do n=1,num_links
      !   dst_field(regrid%dst_add(n)) = dst_field(regrid%dst_add(n)) + &
      !                                  src_field(regrid%src_add(n))* &
      !                                  regrid%weights(n)
      !end do
   !end do ! ifield

!EOC
   end subroutine ESMF_RegridDoBundle

!------------------------------------------------------------------------------

   end module ESMF_RegridMod
