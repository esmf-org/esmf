! $Id: ESMF_Regrid.F90,v 1.7 2003/01/16 17:13:49 pwjones Exp $
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
! This file contains the Regrid class definition and all Regrid class
! methods.
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
! The code in this file implements the Regrid class.  Regrid is responsible
! for any regridding and interpolation required for ESMF applications.
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
      use ESMF_BaseMod      ! ESMF base  class
      use ESMF_ArrayMod     ! ESMF array class
      use ESMF_FieldMod     ! ESMF field class
      use ESMF_GridMod      ! ESMF grid  class
      use ESMF_PhysGridMod  ! ESMF physical grid class
      use ESMF_DistGridMod  ! ESMF distributed grid class

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
!     !  ESMF_RegridType
!
!     ! Description of ESMF_Regrid.

      type ESMF_RegridType
      sequence
      private
        type (ESMF_Base) :: base

        !character (???) :: name

        type (ESMF_Bundle), pointer :: & ! if created with bundle pair
           src_bundle,          &! pointer to source field bundle
           dst_bundle            ! pointer to destination field bundle

        type (ESMF_Field), pointer :: & ! if created with field pair
           src_field,          &! pointer to source grid
           dst_field            ! pointer to destination grid

        integer :: &
           regrid_method       ! method used for this regridding

        type (ESMF_Array), pointer :: &
           src_address,      &! addresses of source field for regrid operation
           dst_address       &! addresses of destination field for regrid op

        type (ESMF_Array), pointer :: &
           weights            ! array of weights for performing the
                              ! regridding transformation

        !type (????), pointer :: &
        !  comm_before,      &! pre-stored communication patterns necessary for
        !  comm_after         ! doing any data motion required for
        !                     ! performing transform
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
      public ESMF_RegridConfig
      public ESMF_Regrid

!------------------------------------------------------------------------------
! !PUBLIC DATA MEMBERS:

      integer, parameter, public ::       &! supported regrid methods
         ESMF_RegridMethod_none     =  0, &! no regridding or undefined method
         ESMF_RegridMethod_Bilinear =  1, &! bilinear (logically-rect grids)
         ESMF_RegridMethod_Bicubic  =  2, &! bicubic  (logically-rect grids)
         ESMF_RegridMethod_Conserv1 =  3, &! 1st-order conservative
         ESMF_RegridMethod_Conserv2 =  4, &! 2nd-order conservative
         ESMF_RegridMethod_Raster   =  5, &! regrid by rasterizing domain
         ESMF_RegridMethod_NearNbr  =  6, &! nearest-neighbor dist-weighted avg
         ESMF_RegridMethod_Fourier  =  7, &! Fourier transform
         ESMF_RegridMethod_Legendre =  8, &! Legendre transform
         ESMF_RegridMethod_Stencil  =  9, &! index-space regridding (stencil)
         ESMF_RegridMethod_Linear   = 10, &! linear for 1-d regridding
         ESMF_RegridMethod_Spline   = 11, &! cubic spline for 1-d regridding
         ESMF_RegridMethod_Copy     = 51, &! copy existing regrid
         ESMF_RegridMethod_Shift    = 52, &! shift addresses of existing regrid
         ESMF_RegridMethod_Adjoint  = 53, &! create adjoint of existing regrid
         ESMF_RegridMethod_File     = 89, &! read a regrid from a file
         ESMF_RegridMethod_User     = 90   ! user-supplied method

!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:
!

    public ESMF_RegridCreate      ! create and fill a regrid object
    public ESMF_RegridDestroy     ! deallocate memory associated with a regrid

    public ESMF_RegridDo          ! perform a regrid operation

    public ESMF_RegridGetName     ! Get name of regrid
    !public ESMF_RegridGetField    ! Get fields associated with this regrid
    !public ESMF_RegridGetBundle   ! Get bundles associated with this regrid

    public ESMF_RegridValidate
    public ESMF_RegridPrint

!
!EOP

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_Regrid.F90,v 1.7 2003/01/16 17:13:49 pwjones Exp $'

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

! !DESCRIPTION:
!     This interface provides a single entry point for various Regrid create
!     methods, including a regrid copied from an existing regrid, a regrid
!     created from an field pair, a regrid created from input field bundles
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
         module procedure ESMF_RegridDoArray

! !DESCRIPTION:
!     This interface provides a single entry point for performing the
!     actual regridding operation.
!
!EOP
      end interface
!
!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      interface ESMF_RegridConstruct

! !PRIVATE MEMBER FUNCTIONS:
         module procedure ESMF_RegridConstructFromField
         module procedure ESMF_RegridConstructFromBundle
         module procedure ESMF_RegridConstructFromRegrid

! !DESCRIPTION:
!     This interface provides a single entry point for methods that construct a
!     complete {\tt Regrid}.  It follows the available create methods.
!
!EOP
      end interface
!
!==============================================================================

      contains

!==============================================================================
!
! This section includes the Regrid Create and Destroy methods.
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

      type(ESMF_RegridType), pointer :: regrid    ! Pointer to new regrid
      integer :: status=ESMF_FAILURE              ! Error status
      logical :: rcpresent=.FALSE.                ! Return code present

!     Initialize pointers
      nullify(regrid)
      nullify(ESMF_RegridCreateFromField%ptr)

!     Initialize return code
      if(present(rc)) then
        rcpresent=.TRUE.
        rc = ESMF_FAILURE
      endif

!     No allocation here - construct must perform all necessary allocation
!     Call construction method to allocate and initialize grid internals.

      call ESMF_RegridConstructFromField(regrid, src_field, dst_field, &
                                         method, name, status)
      if (status /= ESMF_SUCCESS) then
        ! Use error function eventually...
        print *, "ERROR in ESMF_RegridCreateFromField: Regrid construct"
        return
      endif

!     Set return values.
      ESMF_RegridCreateFromField%ptr => regrid
      if(rcpresent) rc = ESMF_SUCCESS

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
      type (ESMF_Field), intent(in) :: &
         src_field,          &! field to be regridded
         dst_field            ! destination (incl grid) of resulting regridded field

      integer, intent(in) :: method   ! method to use for regridding

      character (len = *), intent(in), optional :: name

      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Given a source field bundle and destination field bundle (and attached
!     grids), this routine allocates memory for a new {\tt Regrid} object
!     and fills it with information necessary for regridding the source
!     bundle to the destination bundle.  Returns a pointer to a new {\tt Regrid}.
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

      type(ESMF_RegridType), pointer :: regrid    ! Pointer to new regrid
      integer :: status=ESMF_FAILURE              ! Error status
      logical :: rcpresent=.FALSE.                ! Return code present

!     Initialize pointers
      nullify(regrid)
      nullify(ESMF_RegridCreateFromBundle%ptr)

!     Initialize return code
      if(present(rc)) then
        rcpresent=.TRUE.
        rc = ESMF_FAILURE
      endif

!     No allocation here - construct must perform all necessary allocation
!     Call construction method to allocate and initialize grid internals.

      call ESMF_RegridConstructFromBundle(regrid, src_bundle, dst_bundle, &
                                          method, name, status)
      if (status /= ESMF_SUCCESS) then
        ! Use error function eventually...
        print *, "ERROR in ESMF_RegridCreateFromBundle: Regrid construct"
        return
      endif

!     Set return values.
      ESMF_RegridCreateFromBundle%ptr => regrid
      if(rcpresent) rc = ESMF_SUCCESS

      end function ESMF_RegridCreateFromBundle

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:
!     ESMF_RegridCreateFromRegrid - Creates Regrid structure from existing regrid

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

      type(ESMF_RegridType), pointer :: regrid    ! Pointer to new regrid
      integer :: status=ESMF_FAILURE              ! Error status
      logical :: rcpresent=.FALSE.                ! Return code present

!     Initialize pointers
      nullify(regrid)
      nullify(ESMF_RegridCreateFromRegrid%ptr)

!     Initialize return code
      if(present(rc)) then
        rcpresent=.TRUE.
        rc = ESMF_FAILURE
      endif

!     No allocation here - construct must perform all necessary allocation
!     Call construction method to allocate and initialize grid internals.

      if (method == ESMF_RegridMethod_Shift) then
         call ESMF_RegridConstructFromRegrid(regrid, old_regrid%ptr, method, &
                                             name=name, index_shift=index_shift, &
                                             rc=status)
      else
         call ESMF_RegridConstructFromRegrid(regrid, old_regrid%ptr, method, &
                                             name=name, rc=status)
      endif

      if (status /= ESMF_SUCCESS) then
        ! Use error function eventually...
        print *, "ERROR in ESMF_RegridCreateFromRegrid: Regrid construct"
        return
      endif

!     Set return values.
      ESMF_RegridCreateFromRegrid%ptr => regrid
      if(rcpresent) rc = ESMF_SUCCESS

      end function ESMF_RegridCreateFromRegrid

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:
!     ESMF_RegridDestroy - Free all resources associated with a Regrid

! !INTERFACE:
      subroutine ESMF_RegridDestroy(regrid, rc)
!
! !ARGUMENTS:
      type(ESMF_Regrid), intent(in) :: regrid
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Destroys a {\tt Regrid} object previously allocated
!     via an {\tt ESMF_RegridCreate routine}.
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
! !IROUTINE:
!     ESMF_RegridConstructFromField - Constructs Regrid structure for a field pair

! !INTERFACE:
      subroutine ESMF_RegridConstructFromField(regrid, src_field, dst_field, &
                                               method, name, rc)
!
! !ARGUMENTS:

      type(ESMF_RegridType), intent(out) :: regrid  ! newly created regrid

      type (ESMF_Field), intent(in) :: &
         src_field,          &! field to be regridded
         dst_field            ! destination (incl grid) of resulting regridded field

      integer, intent(in) :: method   ! method to use for regridding

      character (len = *), intent(in), optional :: name

      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Given a source field and destination field (and their attached
!     grids), this routine constructs a new {\tt Regrid} object
!     and fills it with information necessary for regridding the source
!     field to the destination field.  Returns a pointer to a new {\tt Regrid}.
!
!     The arguments are:
!     \begin{description}
!     \item[src\_field]
!          Field to be regridded.
!     \item[dst\_field]
!          Resultant field where regridded source field will be stored.
!     \item[method]
!          Method to use for regridding.
!     \item[[name]]
!          {\tt Regrid} name.
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

!     Initialize pointers
      nullify(regrid%src_bundle)
      nullify(regrid%dst_bundle)
      nullify(regrid%src_field)
      nullify(regrid%dst_field)
      nullify(regrid%src_address)
      nullify(regrid%dst_address)
      nullify(regrid%weights)
      !nullify(regrid%comm_before)
      !nullify(regrid%comm_after)

!     Initialize return code
      if(present(rc)) then
        rcpresent=.TRUE.
        rc = ESMF_FAILURE
      endif

      if (present(name)) then
         !regrid%name = name
      else
         !assign name
      endif

      regrid%src_field => src_field
      regrid%dst_field => dst_field

      regrid%method = method

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
      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_RegridConstructFromField

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:
!     ESMF_RegridConstructFromBundle - Constructs Regrid structure for a bundle pair

! !INTERFACE:
      subroutine ESMF_RegridConstructFromBundle(regrid, src_bundle, dst_bundle, &
                                                method, name, rc)
!
! !ARGUMENTS:

      type(ESMF_RegridType), intent(out) :: regrid  ! newly created regrid

      type (ESMF_Bundle), intent(in) :: &
         src_bundle,          &! field bundle to be regridded
         dst_bundle            ! destination (incl grid) of resulting regridded bundle

      integer, intent(in) :: method   ! method to use for regridding

      character (len = *), intent(in), optional :: name

      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Given a source field bundle and destination field bundle (and their attached
!     grids), this routine constructs a new {\tt Regrid} object
!     and fills it with information necessary for regridding the source
!     bundle to the destination bundle.  Returns a pointer to a new {\tt Regrid}.
!
!     The arguments are:
!     \begin{description}
!     \item[src\_bundle]
!          Field to be regridded.
!     \item[dst\_bundle]
!          Resultant field where regridded source field will be stored.
!     \item[method]
!          Method to use for regridding.
!     \item[[name]]
!          {\tt Regrid} name.
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

!     Initialize pointers
      nullify(regrid%src_bundle)
      nullify(regrid%dst_bundle)
      nullify(regrid%src_field)
      nullify(regrid%dst_field)
      nullify(regrid%src_address)
      nullify(regrid%dst_address)
      nullify(regrid%weights)
      !nullify(regrid%comm_before)
      !nullify(regrid%comm_after)

!     Initialize return code
      if(present(rc)) then
        rcpresent=.TRUE.
        rc = ESMF_FAILURE
      endif

      if (present(name)) then
         !regrid%name = name
      else
         !assign name
      endif

      !TODO - check consistency of bundle pair
      regrid%src_bundle => src_bundle
      regrid%dst_bundle => dst_bundle

      regrid%method = method

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
      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_RegridConstructFromBundle

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:
!     ESMF_RegridConstructFromRegrid - Constructs Regrid structure from existing Regrid

! !INTERFACE:
      subroutine ESMF_RegridConstructFromRegrid(regrid, src_field, dst_field, &
                                                method, name, index_shift, rc)
!
! !ARGUMENTS:

      type(ESMF_RegridType), intent(out) :: regrid  ! newly created regrid

      type (ESMF_Field), intent(in) :: &
         src_field,          &! field to be regridded
         dst_field            ! destination (incl grid) of resulting regridded field

      integer, intent(in) :: method   ! method to use for regridding

      character (len = *), intent(in), optional :: name

      type (ESMF_Array), intent(in), optional :: &
         index_shift      ! shifts in each logical direction for shift method

      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Given an existing regrid structure, this routine constructs a
!     new {\tt Regrid} object and fills it with new info obtained by
!     simple manipulations of the existing regrid.  Returns a pointer to a new
!     {\tt Regrid}.
!
!     The arguments are:
!     \begin{description}
!     \item[regrid]
!          Newly-constructed regrid object to be returned.
!     \item[old\_regrid]
!          Existing regrid structure to be manipulated to form new regrid.
!     \item[method]
!          Method to use for regridding.
!     \item[[name]]
!          {\tt Regrid} name.
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

      integer :: status=ESMF_FAILURE              ! Error status
      logical :: rcpresent=.FALSE.                ! Return code present

!     Initialize pointers

      if (associated(old_regrid%src_bundle)) then
         regrid%src_bundle => old_regrid%src_bundle
         regrid%dst_bundle => old_regrid%dst_bundle
      else
         nullify(regrid%src_bundle)
         nullify(regrid%dst_bundle)
      endif

      if (associated(old_regrid%src_field)) then
         regrid%src_field => old_regrid%src_field
         regrid%dst_field => old_regrid%dst_field
      else
         nullify(regrid%src_field)
         nullify(regrid%dst_field)
      endif

      nullify(regrid%src_address)
      nullify(regrid%dst_address)
      nullify(regrid%weights)
      !nullify(regrid%comm_before)
      !nullify(regrid%comm_after)

!     Initialize return code
      if(present(rc)) then
        rcpresent=.TRUE.
        rc = ESMF_FAILURE
      endif

      if (present(name)) then
         !regrid%name = name
      else
         !assign name
      endif

      regrid%method = method

      select case (method)
      case (ESMF_RegridMethod_Copy)
      case (ESMF_RegridMethod_Shift)
      case (ESMF_RegridMethod_Adjoint)
      case default
      end select
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
      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_RegridConstructFromRegrid

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:
!     ESMF_RegridDestruct - Free any Regrid memory allocated internally

! !INTERFACE:
      subroutine ESMF_RegridDestruct(regrid, rc)
!
! !ARGUMENTS:
      type(ESMF_RegridType), intent(inout) :: regrid
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     ESMF routine which deallocates any space allocated by
!    {\tt  ESMF\_RegridConstruct}, does any additional cleanup before the
!     original Regrid object is freed.  Intended for internal ESMF
!     use only; end-users use {\tt ESMF\_RegridDestroy}, which calls
!     {\tt ESMF_RegridDestruct}.
!
!     The arguments are:
!     \begin{description}
!     \item[regrid]
!          The regrid object to be destructed.
!     \item[[rc]]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:

!       clear name
        !name = blank character string

!       if created with bundles, nullify bundle pointer
        if (associated(src_bundle)) nullify(src_bundle)
        if (associated(dst_bundle)) nullify(dst_bundle)

!       if created with fields, nullify field pointers
        if (associated(src_field)) nullify(src_field)
        if (associated(dst_field)) nullify(dst_field)

        regrid_method = ESMF_RegridMethod_none

!       deallocate regrid transform
        deallocate(src_address, dst_address, weights)

!       nullify communication structures
        !if (associated(comm_before)) nullify(comm_before)
        !if (associated(comm_after )) nullify(comm_after )

      end subroutine ESMF_RegridDestruct

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:
!     ESMF_RegridGetConfig - Get configuration information from a Regrid

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
! !IROUTINE:
!     ESMF_RegridSetConfig - Set configuration information for a Regrid

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
! !IROUTINE:
!     ESMF_RegridGetValue - Get <Value> for a Regrid

! !INTERFACE:
      subroutine ESMF_RegridGetValue(regrid, value, rc)
!
! !ARGUMENTS:
      type(ESMF_Regrid), intent(in) :: regrid
      integer, intent(out) :: value
      integer, intent(out), optional :: rc

!
! !DESCRIPTION:
!     Returns the value of Regrid attribute <Value>.
!     May be multiple routines, one per attribute.
!
!     The arguments are:
!     \begin{description}
!     \item[regrid]
!          Class to be queried.
!     \item[value]
!          Value to be retrieved.
!     \item[[rc]]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:

!
!  code goes here
!
      end subroutine ESMF_RegridGetValue

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:
!     ESMF_RegridSetValue - Set <Value> for a Regrid

! !INTERFACE:
      subroutine ESMF_RegridSetValue(Regrid, value, rc)
!
! !ARGUMENTS:
      type(ESMF_Regrid), intent(in) :: regrid
      integer, intent(in) :: value
      integer, intent(out), optional :: rc

!
! !DESCRIPTION:
!     Set a Regrid attribute with the given value.
!     May be multiple routines, one per attribute.
!
!     The arguments are:
!     \begin{description}
!     \item[regrid]
!          Class to be modified.
!     \item[value]
!          Value to be set.
!     \item[[rc]]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:

!
!  code goes here
!
      end subroutine ESMF_RegridSetValue

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:
!     ESMF_RegridValidate - Check internal consistency of a Regrid

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
! !IROUTINE:
!     ESMF_RegridPrint - Print the contents of a Regrid

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
