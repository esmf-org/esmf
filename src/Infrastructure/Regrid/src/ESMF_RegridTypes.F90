! $Id: ESMF_RegridTypes.F90,v 1.1 2003/04/08 16:53:54 pwjones Exp $
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
!     ESMF Regrid Types Module
      module ESMF_RegridTypesMod
!
!==============================================================================
!
! This file contains the Regrid class definition and a few simple Regrid class
! methods.  The remaining class methods are included in another module in
! order to facilitate easy branching of Regrid creation based on regrid method.
!
!------------------------------------------------------------------------------
! INCLUDES
#include <ESMF_Macros.inc>
!==============================================================================
!BOP
! !MODULE: ESMF_RegridTypesMod - Regridding and interpolation data types
!
! !DESCRIPTION:
!
! The code in this file implements parts of the Regrid class.  Regrid is 
! responsible for any regridding and interpolation required for ESMF 
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
      use ESMF_BaseMod      ! ESMF base   class
      use ESMF_ArrayMod     ! ESMF array  class
      use ESMF_RouteMod     ! ESMF route  class
      use ESMF_FieldMod     ! ESMF field  class
      use ESMF_BundleMod    ! ESMF bundle class
      use ESMF_GridMod      ! ESMF grid   class
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

        type (ESMF_Route), pointer :: &
           gather             ! pre-stored communication patterns necessary for
                              ! doing any data motion required for
                              ! performing transform
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

!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:
!

    public ESMF_RegridDestroy     ! deallocate memory associated with a regrid
    public ESMF_RegridCreateFromRegrid ! Creates Regrid from existing regrid

    public ESMF_RegridGetName     ! Get name of regrid
    public ESMF_RegridGetField    ! Get fields associated with this regrid
    public ESMF_RegridGetBundle   ! Get bundles associated with this regrid

    public ESMF_RegridValidate
    public ESMF_RegridPrint

!
!EOP

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_RegridTypes.F90,v 1.1 2003/04/08 16:53:54 pwjones Exp $'

!==============================================================================
!
! INTERFACE BLOCKS
!
!==============================================================================

!==============================================================================

      contains

!==============================================================================
!
! This section includes the Regrid Destroy methods and a single instance
! of a create method.  All other create methods are implemented in another
! module to facilitate branching based on type of regridding required.
!
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

      end module ESMF_RegridTypesMod
