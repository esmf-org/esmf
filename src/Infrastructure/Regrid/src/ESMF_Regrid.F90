! $Id: ESMF_Regrid.F90,v 1.13 2003/05/07 04:34:31 cdeluca Exp $
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
!BOPI
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
      !use ESMF_RegridConservMod ! ESMF regrid methods related to conservative regrid

      implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
      private

!------------------------------------------------------------------------------
!     ! ESMF_RegridConfig
!
!     ! Description of ESMF_RegridConfig

      type ESMF_RegridConfig
      sequence
      private
        integer :: dummy
      end type

!------------------------------------------------------------------------------
!     !  ESMF_Regrid
!
!     !  The Regrid data structure that is passed between languages.

      type ESMF_Regrid
      sequence
      private
        type (ESMF_RegridType), pointer :: ptr     ! pointer to a regrid type
      end type

!------------------------------------------------------------------------------
! !PUBLIC TYPES:
!
      public ESMF_RegridConfig
      public ESMF_Regrid

!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:
!
      public ESMF_RegridCreate     ! create and fill a regrid object
      public ESMF_RegridRun        ! perform a regrid operation

      public ESMF_RegridGet        ! returns value of a regrid attribute
      public ESMF_RegridDestroy    ! deallocate memory associated with a regrid
      public ESMF_RegridValidate   ! Error checking and validation
      public ESMF_RegridPrint      ! Prints various regrid info

!------------------------------------------------------------------------------
! !PUBLIC DATA MEMBERS:
!
      integer, parameter, public ::       &! supported regrid methods
         ESMF_RegridMethod_none     =  0, &! no regridding or undefined regridding
         ESMF_RegridMethod_Bilinear =  1, &! bilinear (logically-rect grids)
         ESMF_RegridMethod_Bicubic  =  2, &! bicubic  (logically-rect grids)
         ESMF_RegridMethod_Conserv1 =  3, &! 1st-order conservative
         ESMF_RegridMethod_Conserv2 =  4, &! 2nd-order conservative
         ESMF_RegridMethod_Raster   =  5, &! regrid by rasterizing domain
         ESMF_RegridMethod_NearNbr  =  6, &! nearest-neighbor dist-weighted avg
         ESMF_RegridMethod_Fourier  =  7, &! Fourier transform
         ESMF_RegridMethod_Legendre =  8, &! Legendre transform
         ESMF_RegridMethod_Index    =  9, &! index-space regridding (shift, stencil)
         ESMF_RegridMethod_Linear   = 10, &! linear for 1-d regridding
         ESMF_RegridMethod_Spline   = 11, &! cubic spline for 1-d regridding
         ESMF_RegridMethod_Copy     = 51, &! copy existing regrid
         ESMF_RegridMethod_Shift    = 52, &! shift addresses of existing regrid
         ESMF_RegridMethod_Adjoint  = 53, &! create adjoint of existing regrid
         ESMF_RegridMethod_File     = 89, &! read a regrid from a file
         ESMF_RegridMethod_User     = 90   ! user-supplied method

!
!EOPI
!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
         '$Id: ESMF_Regrid.F90,v 1.13 2003/05/07 04:34:31 cdeluca Exp $'

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
         module procedure ESMF_RegridCreateFromRegrid
         
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
      interface ESMF_RegridRun

! !PRIVATE MEMBER FUNCTIONS:
         module procedure ESMF_RegridRunField
         module procedure ESMF_RegridRunBundle
         !module procedure ESMF_RegridRunArray

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
! !IROUTINE: ESMF_RegridCreateFromField - Creates Regrid structure for a field pair

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
!   The supported regridding methods for this create function are currently:
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

      integer :: stat = ESMF_FAILURE              ! Error status
      logical :: rcpresent=.FALSE.                ! Return code present
      type (ESMF_RegridType) :: regrid
      character (*) :: regrid_name

!     Initialize return code
      if(present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_FAILURE
      endif

!     You have the right to define a name.  If you do not choose a name,
!     one will be appointed for you.

      if(.not. present(name)) then
         regrid_name = 'WeDontNeedNoStinkinRegridName'  ! TODO: create a name here
      else
         regrid_name = name
      endif

!     Initialize pointers
      nullify(ESMF_RegridCreateFromField%ptr)

!     Call the appropriate create routine based on method choice
      select case(method)
      case(ESMF_RegridMethod_Bilinear) ! bilinear
         regrid = ESMF_RegridConstructBilin(src_field, dst_field, &
                                            regrid_name, stat)
      case(ESMF_RegridMethod_Bicubic)  ! bicubic
         print *, "ERROR in ESMF_RegridCreateFromField: ", &
                  "Bicubic not yet supported"
         stat = ESMF_FAILURE
      case(ESMF_RegridMethod_Conserv1)
         print *, "ERROR in ESMF_RegridCreateFromField: ", &
                  "1st-order conservative not yet supported"
         stat = ESMF_FAILURE
      case(ESMF_RegridMethod_Conserv2) ! 2nd-order conservative
         print *, "ERROR in ESMF_RegridCreateFromField: ", &
                  "2nd-order conservative not yet supported"
         stat = ESMF_FAILURE
      case(ESMF_RegridMethod_Raster) ! regrid by rasterizing domain
         print *, "ERROR in ESMF_RegridCreateFromField: ", &
                  "Raster method not yet supported"
         stat = ESMF_FAILURE
      case(ESMF_RegridMethod_NearNbr) ! nearest-neighbor dist-weighted avg
         print *, "ERROR in ESMF_RegridCreateFromField: ", &
                  "Nearest-neighbor method not yet supported"
         stat = ESMF_FAILURE
      case(ESMF_RegridMethod_Fourier) ! Fourier transform
         print *, "ERROR in ESMF_RegridCreateFromField: ", &
                  "Fourier transforms not yet supported"
         stat = ESMF_FAILURE
      case(ESMF_RegridMethod_Legendre) ! Legendre transform
         print *, "ERROR in ESMF_RegridCreateFromField: ", &
                  "Legendre transforms not yet supported"
         stat = ESMF_FAILURE
      case(ESMF_RegridMethod_Index) ! index-space regridding (shift, stencil)
         print *, "ERROR in ESMF_RegridCreateFromField: ", &
                  "Index-space methods not yet supported"
         stat = ESMF_FAILURE
      case(ESMF_RegridMethod_Linear) ! linear for 1-d regridding
         print *, "ERROR in ESMF_RegridCreateFromField: ", &
                  "1-d linear methods not yet supported"
         stat = ESMF_FAILURE
      case(ESMF_RegridMethod_Spline) ! cubic spline for 1-d regridding
         print *, "ERROR in ESMF_RegridCreateFromField: ", &
                  "1-d cubic splines not yet supported"
         stat = ESMF_FAILURE
      case(ESMF_RegridMethod_User) ! cubic spline for 1-d regridding
         print *, "ERROR in ESMF_RegridCreateFromField: ", &
                  "User-defined regridding not yet supported"
         stat = ESMF_FAILURE
      case default
         print *, "ERROR in ESMF_RegridCreateFromField: Invalid method"
         stat = ESMF_FAILURE
      end select

      if (stat /= ESMF_SUCCESS) then
         ! Use error function eventually...
         print *, 'ERROR in ESMF_RegridCreateFromField: error in creation'
         if (rcpresent) rc = stat
      else
         ESMF_RegridCreateFromField%ptr => regrid
         if (rcpresent) rc = ESMF_SUCCESS
      endif

      end function ESMF_RegridCreateFromField

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_RegridCreateFromBundle - Creates Regrid structure for a bundle pair

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

      integer :: stat = ESMF_FAILURE              ! Error status
      logical :: rcpresent=.FALSE.                ! Return code present
      type (ESMF_RegridType) :: regrid
      character (*) :: regrid_name

!     Initialize return code
      if(present(rc)) then
        rcpresent=.TRUE.
        rc = ESMF_FAILURE
      endif

!     You have the right to define a name.  If you do not choose a name,
!     one will be appointed for you.

      if(.not. present(name)) then
         regrid_name = 'WeDontNeedNoStinkinRegridName'  ! TODO: create a name here
      else
         regrid_name = name
      endif

!     Initialize pointers
      nullify(ESMF_RegridCreateFromBundle%ptr)

!     Call the appropriate create routine based on method choice

      select case(method)
      case(ESMF_RegridMethod_Bilinear) ! bilinear
         regrid = ESMF_RegridConstructBilin(src_bundle, dst_bundle, &
                                            regrid_name, stat)
      case(ESMF_RegridMethod_Bicubic)  ! bicubic
         print *, "ERROR in ESMF_RegridCreateFromBundle: ", &
                  "Bicubic not yet supported"
         stat = ESMF_FAILURE
      case(ESMF_RegridMethod_Conserv1)
         print *, "ERROR in ESMF_RegridCreateFromBundle: ", &
                  "1st-order conservative not yet supported"
         stat = ESMF_FAILURE
      case(ESMF_RegridMethod_Conserv2) ! 2nd-order conservative
         print *, "ERROR in ESMF_RegridCreateFromBundle: ", &
                  "2nd-order conservative not yet supported"
         stat = ESMF_FAILURE
      case(ESMF_RegridMethod_Raster) ! regrid by rasterizing domain
         print *, "ERROR in ESMF_RegridCreateFromBundle: ", &
                  "Raster method not yet supported"
         stat = ESMF_FAILURE
      case(ESMF_RegridMethod_NearNbr) ! nearest-neighbor dist-weighted avg
         print *, "ERROR in ESMF_RegridCreateFromBundle: ", &
                  "Nearest-neighbor method not yet supported"
         stat = ESMF_FAILURE
      case(ESMF_RegridMethod_Fourier) ! Fourier transform
         print *, "ERROR in ESMF_RegridCreateFromBundle: ", &
                  "Fourier transforms not yet supported"
         stat = ESMF_FAILURE
      case(ESMF_RegridMethod_Legendre) ! Legendre transform
         print *, "ERROR in ESMF_RegridCreateFromBundle: ", &
                  "Legendre transforms not yet supported"
         stat = ESMF_FAILURE
      case(ESMF_RegridMethod_Index) ! index-space regridding (shift, stencil)
         print *, "ERROR in ESMF_RegridCreateFromBundle: ", &
                  "Index-space methods not yet supported"
         stat = ESMF_FAILURE
      case(ESMF_RegridMethod_Linear) ! linear for 1-d regridding
         print *, "ERROR in ESMF_RegridCreateFromBundle: ", &
                  "1-d linear methods not yet supported"
         stat = ESMF_FAILURE
      case(ESMF_RegridMethod_Spline) ! cubic spline for 1-d regridding
         print *, "ERROR in ESMF_RegridCreateFromBundle: ", &
                  "1-d cubic splines not yet supported"
         stat = ESMF_FAILURE
      case(ESMF_RegridMethod_User) ! cubic spline for 1-d regridding
         print *, "ERROR in ESMF_RegridCreateFromBundle: ", &
                  "User-defined regridding not yet supported"
         stat = ESMF_FAILURE
      case default
         print *, "ERROR in ESMF_RegridCreateFromBundle: Invalid method"
         stat = ESMF_FAILURE
      end select

!     Set return values.
!     ESMF_RegridCreateFromBundle set by above calls

      if (stat /= ESMF_SUCCESS) then
         ! Use error function eventually...
         print *, "ERROR in ESMF_RegridCreateFromBundle: create functions"
         if (rcpresent) rc = stat
      else
         ESMF_RegridCreateFromBundle%ptr => regrid
         if (rcpresent) rc = ESMF_SUCCESS
      endif

      end function ESMF_RegridCreateFromBundle

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_RegridCreateFromRegrid - Creates Regrid structure from existing regrid

! !INTERFACE:
      function ESMF_RegridCreateFromRegrid(old_regrid, method, &
                                           name, index_shift, rc)
!
! !RETURN VALUE:
      type(ESMF_Regrid) :: ESMF_RegridCreateFromRegrid
!
! !ARGUMENTS:
      type (ESMF_Regrid), intent(in) :: &
         old_regrid          ! old regrid object to use in creating new regrid

      integer, intent(in) :: method   ! method to use for regridding

      character (len = *), intent(in), optional :: name

      type (ESMF_Array), intent(in), optional :: &
         index_shift        ! index shifts if shift method is chosen

      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Given an existing regrid structure, this routine allocates memory for a
!     new {\tt Regrid} object and fills it with new regridding obtained by
!     simple manipulations of the existing regrid.  Returns a pointer to a new
!     {\tt Regrid}.
!
!     The arguments are:
!     \begin{description}
!     \item[[name]]
!          {\tt Regrid} name.
!     \item[old\_regrid]
!          Existing regrid structure to be manipulated to form new regrid.
!     \item[method]
!          Method to use for regridding.
!     \item[[index\_shift]]
!          Array indicating shift in each logical direction for creating
!          a new regrid by index shift of an old regrid.
!     \item[[rc]]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!   The supported methods for creating a regridding from an existing regrid are:
!   \begin{description}
!   \item[ESMF\_RegridMethod\_Copy   ] simple copy of old regrid
!   \item[ESMF\_RegridMethod\_Shift  ] simple shift of addresses
!   \item[ESMF\_RegridMethod\_Adjoint] creates adjoint of existing regrid
!   \end{description}
!
! !REQUIREMENTS:  TODO
!EOP

      type(ESMF_RegridType) :: regrid     ! New regrid
      integer :: status=ESMF_FAILURE      ! Error status
      logical :: rcpresent=.FALSE.        ! Return code present

!     Initialize pointers
      nullify(ESMF_RegridCreateFromRegrid%ptr)

!     Initialize return code
      if(present(rc)) then
        rcpresent=.TRUE.
        rc = ESMF_FAILURE
      endif

!     No allocation here - construct must perform all necessary allocation
!     Call construction method to allocate and initialize grid internals.

      !if (method == ESMF_RegridMethod_Shift) then
      !   call ESMF_RegridConstructFromRegrid(regrid, old_regrid%ptr, method, &
      !                                       name=name, index_shift=index_shift, &
      !                                       rc=status)
      !else
      !   call ESMF_RegridConstructFromRegrid(regrid, old_regrid%ptr, method, &
      !                                       name=name, rc=status)
      !endif

!     Set return values.

      if (status /= ESMF_SUCCESS) then
         ! Use error function eventually...
         print *, "ERROR in ESMF_RegridCreateFromRegrid: Regrid construct"
      else
         ESMF_RegridCreateFromRegrid%ptr => regrid
         if(rcpresent) rc = ESMF_SUCCESS
      endif

      end function ESMF_RegridCreateFromRegrid

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_RegridRunField - Performs a regridding between two fields

! !INTERFACE:
      subroutine ESMF_RegridRunField(src_field, dst_field, regrid, rc)
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

   end subroutine ESMF_RegridRunField

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_RegridRunBundle - Performs a regridding between two bundles of fields

! !INTERFACE:
      subroutine ESMF_RegridRunBundle(src_bundle, dst_bundle, regrid, rc)
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
   end subroutine ESMF_RegridRunBundle

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_RegridGetConfig - Get configuration information from a Regrid

! !INTERFACE:
      subroutine ESMF_RegridGetConfig(regrid, config, rc)
!
! !ARGUMENTS:
      type(ESMF_Regrid), intent(in) :: regrid
      integer, intent(out) :: config
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Returns the set of resources the Regrid object was configured with.
!
!     The arguments are:
!     \begin{description}
!     \item[regrid]
!          Class to be queried.
!     \item[config]
!          Configuration information.
!     \item[[rc]]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:

!
!  code goes here
!
      end subroutine ESMF_RegridGetConfig

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_RegridSetConfig - Set configuration information for a Regrid

! !INTERFACE:
      subroutine ESMF_RegridSetConfig(regrid, config, rc)
!
! !ARGUMENTS:
      type(ESMF_Regrid), intent(in) :: regrid
      integer, intent(in) :: config
      integer, intent(out), optional :: rc

!
! !DESCRIPTION:
!     Configures the Regrid object with set of resources given.
!
!     The arguments are:
!     \begin{description}
!     \item[regrid]
!          Class to be configured.
!     \item[config]
!          Configuration information.
!     \item[[rc]]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:

!
!  code goes here
!
      end subroutine ESMF_RegridSetConfig

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_RegridGet - Get an attribute of a Regrid

! !INTERFACE:
      subroutine ESMF_RegridGet(regrid, name,            &
                                src_bundle, dst_bundle,  &
                                src_field,  dst_field,   &
                                method, num_links , gather, rc)
!
! !ARGUMENTS:

      type(ESMF_Regrid), intent(in) :: regrid

      character (*),      intent(out), optional :: name
      type (ESMF_Bundle), intent(out), optional :: src_bundle, dst_bundle
      type (ESMF_Field),  intent(out), optional :: src_field,  dst_field
      integer,            intent(out), optional :: method
      integer,            intent(out), optional :: num_links
      type (ESMF_Route),  intent(out), optional :: gather

      integer,            intent(out), optional :: rc

!
! !DESCRIPTION:
!     Returns the value of a Regrid attribute.  The
!     attribute is specified through optional arguments.
!
!     The arguments are:
!     \begin{description}
!     \item[regrid]
!          Regrid to be queried.
!     \item[[name]]
!          Name for this regrid.
!     \item[[src\_bundle]]
!          If created with bundles, the source bundle associated
!          with this regrid. 
!     \item[[dst\_bundle]]
!          If created with bundles, the destination bundle associated
!          with this regrid. 
!     \item[[src\_field]]
!          If created with fields, the source field associated
!          with this regrid. 
!     \item[[dst\_field]]
!          If created with fields, the destination field associated
!          with this regrid. 
!     \item[[method]]
!          Integer enum of method used in this regrid.
!     \item[[num\_links]]
!          Number of unique links between grids for this regrid.
!     \item[[gather]]
!          Route used to gather non-local information to perform regrid.
!     \item[[rc]]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:
!  TODO

      logical :: rcpresent = .FALSE.
      integer :: stat, status = ESMF_SUCCESS

      if (present(rc)) rcpresent = .TRUE.
      
      ! Get name if requested
      if (present(name)) then
         call ESMF_RegridTypeGet(regrid%ptr, name=name, stat)
         if (stat /= ESMF_SUCCESS) status = ESMF_FAILURE
      endif

      ! Get bundles if requested
      if (present(src_bundle)) then
         call ESMF_RegridTypeGet(regrid%ptr, src_bundle=src_bundle, stat)
         if (stat /= ESMF_SUCCESS) status = ESMF_FAILURE
      endif
      if (present(dst_bundle)) then
         call ESMF_RegridTypeGet(regrid%ptr, dst_bundle=dst_bundle, stat)
         if (stat /= ESMF_SUCCESS) status = ESMF_FAILURE
      endif

      ! Get fields if requested
      if (present(src_field)) then
         call ESMF_RegridTypeGet(regrid%ptr, src_field=src_field, stat)
         if (stat /= ESMF_SUCCESS) status = ESMF_FAILURE
      endif
      if (present(dst_field)) then
         call ESMF_RegridTypeGet(regrid%ptr, dst_field=dst_field, stat)
         if (stat /= ESMF_SUCCESS) status = ESMF_FAILURE
      endif

      ! get method or number of links or gather route
      if (present(method)) then
         call ESMF_RegridTypeGet(regrid%ptr, method=method, stat)
         if (stat /= ESMF_SUCCESS) status = ESMF_FAILURE
      endif
      if (present(num_links)) then
         call ESMF_RegridTypeGet(regrid%ptr, num_links=num_links, stat)
         if (stat /= ESMF_SUCCESS) status = ESMF_FAILURE
      endif
      if (present(gather)) then
         call ESMF_RegridTypeGet(regrid%ptr, gather=gather, stat)
         if (stat /= ESMF_SUCCESS) status = ESMF_FAILURE
      endif

      if (rcpresent) rc = status

      end subroutine ESMF_RegridGet

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_RegridDestroy - Free all resources associated with a Regrid

! !INTERFACE:
      subroutine ESMF_RegridDestroy(regrid, rc)
!
! !ARGUMENTS:
      type(ESMF_Regrid), intent(in) :: regrid
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Destroys a {\tt Regrid} object previously allocated
!     via an {\tt ESMF\_RegridCreate routine}.
!
!     The arguments are:
!     \begin{description}
!     \item[regrid]
!          The regrid object to be destroyed.
!     \item[[rc]]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS: TODO
!EOP

      integer :: status=ESMF_FAILURE              ! Error status
      logical :: rcpresent=.FALSE.                ! Return code present

!     Initialize return code
      if(present(rc)) then
        rcpresent=.TRUE.
        rc = ESMF_FAILURE
      endif

!     Call destruct method to free up internally-allocated memory

      call ESMF_RegridDestruct(regrid%ptr, status)
      if (status /= ESMF_SUCCESS) then
        ! Use error function eventually...
        print *, "ERROR in ESMF_RegridDestroy: Regrid destruct"
        return
      endif

!     Set return values.
      nullify(regrid%ptr)
      if (rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_RegridDestroy

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_RegridValidate - Check internal consistency of a Regrid

! !INTERFACE:
      subroutine ESMF_RegridValidate(regrid, opt, rc)
!
! !ARGUMENTS:
      type(ESMF_Regrid), intent(in) :: regrid
      character (len=*), intent(in), optional :: opt
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Validates that a Regrid is internally consistent.
!
!     The arguments are:
!     \begin{description}
!     \item[regrid]
!          Class to be queried.
!     \item[[opt]]
!          Validation options.
!     \item[[rc]]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:  XXXn.n, YYYn.n

!
!  code goes here
!
      end subroutine ESMF_RegridValidate

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_RegridPrint - Print the contents of a Regrid

! !INTERFACE:
      subroutine ESMF_RegridPrint(regrid, opt, rc)
!
! !ARGUMENTS:
      type(ESMF_Regrid), intent(in) :: regrid
      character (len=*), intent(in) :: opt
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!      Print information about a Regrid.
!
!     The arguments are:
!     \begin{description}
!     \item[regrid]
!          Class to be queried.
!     \item[[opt]]
!          Print ptions that control the type of information and level of
!          detail.
!     \item[[rc]]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:  SSSn.n, GGGn.n

!
!  code goes here
!
      end subroutine ESMF_RegridPrint

!------------------------------------------------------------------------------

   end module ESMF_RegridMod
