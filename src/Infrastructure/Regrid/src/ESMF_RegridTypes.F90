! $Id: ESMF_RegridTypes.F90,v 1.2 2003/05/03 03:58:23 pwjones Exp $
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
!     !  ESMF_RegridType
!
!     ! Description of ESMF_RegridType.

      type ESMF_RegridType
      sequence
      private
        type (ESMF_Base) :: base

        type (ESMF_Bundle), pointer :: & ! if created with bundle pair
           src_bundle,   &! pointer to source field bundle
           dst_bundle     ! pointer to destination field bundle

        type (ESMF_Field), pointer :: & ! if created with field pair
           src_field,    &! pointer to source grid
           dst_field      ! pointer to destination grid

        integer ::       &
           method        &! method used for this regridding
           num_links      ! number of unique links between grids

        type (ESMF_Array), pointer :: &
           src_add,      &! addresses of source field for regrid operation
           dst_add       &! addresses of destination field for regrid op

        type (ESMF_Array), pointer :: &
           weights        ! array of weights for performing the
                          ! regridding transformation

        type (ESMF_Route), pointer :: &
           gather             ! pre-stored communication patterns necessary for
                              ! doing data motion required for regrid
      end type

!------------------------------------------------------------------------------
! !PUBLIC TYPES:
      public ESMF_RegridType

!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:
!
    public ESMF_RegridAddLink      ! Adds address pair and weight to regrid
    public ESMF_RegridTypeGet      ! returns a regrid attribute
    public ESMF_RegridTypeSet      ! sets    a regrid attribute
    public ESMF_RegridConstructEmpty  ! creates an empty regrid structure
    public ESMF_RegridDestruct     ! deallocate memory associated with a regrid

!
!EOP
!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_RegridTypes.F90,v 1.2 2003/05/03 03:58:23 pwjones Exp $'

!==============================================================================
!
! INTERFACE BLOCKS
!
!==============================================================================

!==============================================================================

      contains

!==============================================================================
!
! This section includes utility routines for performing actions on the
! main Regrid type.  All other create methods are implemented in another
! module to facilitate branching based on type of regridding required.
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE:
!     ESMF_RegridAddLink - Adds address pair and regrid weight to regrid

! !INTERFACE:
      subroutine ESMF_RegridAddLink(regrid, src_add, dst_add, weights, rc)
!
! !ARGUMENTS:

      type(ESMF_RegridType), intent(inout) :: regrid

      type(ESMF_Array), intent(in) :: &
         src_add,                     &! address in source      field array
         dst_add                       ! address in destination field array

      type(ESMF_Array), intent(in) :: &
         weights                       ! weight(s) for this link

      integer, intent(out) :: rc

!
! !DESCRIPTION:
!     Adds an address pair and regrid weight to an existing regrid.
!
!     The arguments are:
!     \begin{description}
!     \item[regrid]
!          Regrid type to which the new link is to be added.
!     \item[src\_add]
!          Address in source field array for this link.
!     \item[dst\_add]
!          Address in destination field array for this link.
!     \item[weights]
!          Regrid weight(s) for this link.
!     \item[rc]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:
!  TODO

      integer :: new_links
      type (ESMF_ARRAY) :: &! temps for use when re-sizing arrays
         src_add_tmp, dst_add_tmp, weights_tmp

      rc = ESMF_FAILURE

      !
      !  increment number of links for this regrid
      !
      
      new_links = regrid%num_links + 1
      
      !
      !  if new number of links exceeds array sizes, re-size arrays
      !
      
      ! TODO: resize check
      
      regrid%num_links = new_links
      
      !
      ! TODO: Add addresses and weights to regrid arrays
      !


      rc = ESMF_SUCCESS      

      end subroutine ESMF_RegridAddLink

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:
!     ESMF_RegridTypeGet - Get attribute of a Regrid type

! !INTERFACE:
      subroutine ESMF_RegridTypeGet(regrid, name,            &
                                    src_bundle, dst_bundle,  &
                                    src_field,  dst_field,   &
                                    method, num_links , gather, rc)
!
! !ARGUMENTS:

      type(ESMF_RegridType), intent(in) :: regrid

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
!          Regrid type to be queried.
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
!     \item[[num_links]]
!          Number of unique links between grids for this regrid.
!     \item[[gather]]
!          Route used to gather non-local information to perform regrid.
!     \item[[rc]]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:

      logical :: rcpresent = .FALSE.
      integer :: stat, status = ESMF_SUCCESS

      if (present(rc)) rcpresent = .TRUE.
      
      ! Get name if requested
      if (present(name)) then
         call ESMF_GetName(regrid%base, name, stat)
         if (stat /= ESMF_SUCCESS) status = ESMF_FAILURE
      endif

      ! Get bundles if requested
      if (present(src_bundle)) then
         if (associated(regrid%src_bundle)) then
            src_bundle = regrid%src_bundle
         else
            print *,'ERROR in RegridTypeGet: requested non-existent bundle'
            status = ESMF_FAILURE
         endif
      endif
      if (present(dst_bundle)) then
         if (associated(regrid%dst_bundle)) then
            dst_bundle = regrid%dst_bundle
         else
            print *,'ERROR in RegridTypeGet: requested non-existent bundle'
            status = ESMF_FAILURE
         endif
      endif

      ! Get fields if requested
      if (present(src_field)) then
         if (associated(regrid%src_field)) then
            src_field = regrid%src_field
         else
            print *,'ERROR in RegridTypeGet: requested non-existent field'
            status = ESMF_FAILURE
         endif
      endif
      if (present(dst_field)) then
         if (associated(regrid%dst_field)) then
            dst_field = regrid%dst_field
         else
            print *,'ERROR in RegridTypeGet: requested non-existent field'
            status = ESMF_FAILURE
         endif
      endif

      ! get method or number of links
      if (present(method)) method = regrid%method
      if (present(num_links)) num_links = regrid%num_links
      if (present(gather)) gather = regrid%gather

      if (rcpresent) rc = status

      end subroutine ESMF_RegridTypeGet

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:
!     ESMF_RegridTypeSet - Set attribute of a Regrid

! !INTERFACE:
      subroutine ESMF_RegridTypeSet(regrid, name,            &
                                    src_bundle, dst_bundle,  &
                                    src_field,  dst_field,   &
                                    method, num_links , gather, rc)
!
! !ARGUMENTS:
      type(ESMF_RegridType), intent(in) :: regrid

      character (*),      intent(in), optional :: name
      type (ESMF_Bundle), intent(in), optional :: src_bundle, dst_bundle
      type (ESMF_Field),  intent(in), optional :: src_field,  dst_field
      integer,            intent(in), optional :: method
      integer,            intent(in), optional :: num_links
      type (ESMF_Route),  intent(in), optional :: gather

      integer,            intent(out), optional :: rc

!
! !DESCRIPTION:
!     Sets a Regrid attribute with the given value.
!     The attribute is determined by optional arguments.
!
!     The arguments are:
!     \begin{description}
!     \item[regrid]
!          Class to be modified.
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
!     \item[[num_links]]
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
      
      ! Set name if requested
      if (present(name)) then
         call ESMF_SetName(regrid%base, name, stat)
         if (stat /= ESMF_SUCCESS) status = ESMF_FAILURE
      endif

      ! Set bundles if requested
      if (present(src_bundle)) regrid%src_bundle => src_bundle
      if (present(dst_bundle)) regrid%dst_bundle => dst_bundle
      
      ! Set fields if requested
      if (present(src_field)) regrid%src_field => src_field
      if (present(dst_field)) regrid%dst_field => dst_field
      
      ! get method or number of links
      if (present(method)) regrid%method = method
      if (present(num_links)) regrid%num_links = num_links
      if (present(gather)) regrid%gather = gather

      if (rcpresent) rc = status

      end subroutine ESMF_RegridTypeSet

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:
!     ESMF_RegridConstructEmpty - Create empty regrid structure

! !INTERFACE:
      subroutine ESMF_RegridConstructEmpty(regrid, rc)
!
! !ARGUMENTS:
      type(ESMF_RegridType), intent(inout) :: regrid
      integer, intent(out) :: rc
!
! !DESCRIPTION:
!     ESMF routine which creates and initializes a regrid structure.
!     The structure is later filled with appropriate data using the
!     set function. Intended for internal ESMF use only; end-users 
!     use {\tt ESMF\_Create} functions.
!
!     The arguments are:
!     \begin{description}
!     \item[regrid]
!          The regrid object to be initialized.
!     \item[[rc]]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:

        ! nullify pointers
      
        nullify(regrid%src_bundle)
        nullify(regrid%dst_bundle)
        nullify(regrid%src_field)
        nullify(regrid%dst_field)
        nullify(regrid%src_add)
        nullify(regrid%dst_add)
        nullify(regrid%weights)
        nullify(regrid%gather)

        ! initialize scalars
        
        regrid%method    = ESMF_RegridMethod_none
        regrid%num_links = 0

        rc = ESMF_SUCCESS

      end subroutine ESMF_RegridConstructEmpty

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

!       if created with bundles, nullify bundle pointer
        if (associated(src_bundle)) nullify(src_bundle)
        if (associated(dst_bundle)) nullify(dst_bundle)

!       if created with fields, nullify field pointers
        if (associated(src_field)) nullify(src_field)
        if (associated(dst_field)) nullify(dst_field)

        regrid%method    = ESMF_RegridMethod_none
        regrid%num_links = 0

!       deallocate regrid transform
        !TODO
        !destroy src_add, dst_add, weights ESMF arrays
        nullify(regrid%src_add)
        nullify(regrid%dst_add)
        nullify(regrid%weights)

!       destroy communication structures
        !TODO
        !destroy route regrid%gather
        nullify(regrid%gather)

      end subroutine ESMF_RegridDestruct

!------------------------------------------------------------------------------

      end module ESMF_RegridTypesMod
