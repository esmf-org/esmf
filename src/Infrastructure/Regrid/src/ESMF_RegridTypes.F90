! $Id: ESMF_RegridTypes.F90,v 1.8 2003/07/17 20:02:47 nscollins Exp $
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
!BOPI
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
      use ESMF_ArrayBaseMod  ! ESMF array  class
      use ESMF_ArrayExpandMod  ! ESMF array  class
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

        type (ESMF_BundleType), pointer :: & ! if created with bundle pair
           src_bundlep,   &! pointer to source field bundle
           dst_bundlep     ! pointer to destination field bundle

        type (ESMF_FieldType), pointer :: & ! if created with field pair
           src_fieldp,    &! pointer to source grid
           dst_fieldp      ! pointer to destination grid

        integer ::       &
           method,       &! method used for this regridding
           num_links      ! number of unique links between grids

        type (ESMF_Array), pointer :: &
           src_add,      &! addresses of source field for regrid operation
           dst_add        ! addresses of destination field for regrid op

        type (ESMF_Array), pointer :: &
           weights        ! array of weights for performing the
                          ! regridding transformation

        integer ::       &
           redistrb_option  ! option for redistributing data for regrid

        type (ESMF_Route), pointer :: & ! pre-stored redistribution patterns
           gather,       &! not sure, just trying to get it to compile jw
           src_route,    &! route for redistribution on source side
           dst_route      ! route for redistribution on destination side

      end type


!------------------------------------------------------------------------------
! !PUBLIC DATA MEMBERS:
!
  
      integer, parameter, public ::       &! supported regrid methods
         ESMF_RegridMethod_none     =  0, &! no regridding or undefined regrid
         ESMF_RegridMethod_FieldCopy=  1, &! same Grid so copy field
         ESMF_RegridMethod_Redist   =  2, &! same PhysGrid, just redistribute
         ESMF_RegridMethod_Bilinear =  3, &! bilinear (logically-rect grids)
         ESMF_RegridMethod_Bicubic  =  4, &! bicubic  (logically-rect grids)
         ESMF_RegridMethod_Conserv1 =  5, &! 1st-order conservative
         ESMF_RegridMethod_Conserv2 =  6, &! 2nd-order conservative
         ESMF_RegridMethod_Raster   =  7, &! regrid by rasterizing domain
         ESMF_RegridMethod_NearNbr  =  8, &! nearest-neighbor dist-weighted avg
         ESMF_RegridMethod_Fourier  =  9, &! Fourier transform
         ESMF_RegridMethod_Legendre = 10, &! Legendre transform
         ESMF_RegridMethod_Index    = 11, &! index-space regrid (shift, stencil)
         ESMF_RegridMethod_Linear   = 12, &! linear for 1-d regridding
         ESMF_RegridMethod_Spline   = 13, &! cubic spline for 1-d regridding
         ESMF_RegridMethod_RegridCopy=51, &! copy existing regrid
         ESMF_RegridMethod_Shift    = 52, &! shift addresses of existing regrid
         ESMF_RegridMethod_Adjoint  = 53, &! create adjoint of existing regrid
         ESMF_RegridMethod_File     = 89, &! read a regrid from a file
         ESMF_RegridMethod_User     = 90   ! user-supplied method

      integer, parameter, public ::     &! options for field data motion
         ESMF_RegridDistrb_None   =  0, &! no data motion required or undefined
         ESMF_RegridDistrb_Source =  1, &! redistribute source field
         ESMF_RegridDistrb_Dest   =  2, &! redistribute destination field
         ESMF_RegridDistrb_Both   =  3   ! redistribute both 

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
!EOPI
!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_RegridTypes.F90,v 1.8 2003/07/17 20:02:47 nscollins Exp $'

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
! !IROUTINE: ESMF_RegridAddLink - Adds address pair and regrid weight to regrid

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
! !IROUTINE: ESMF_RegridTypeGet - Get attribute of a Regrid type

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
         if (associated(regrid%src_bundlep)) then
            src_bundle%btypep => regrid%src_bundlep
         else
            print *,'ERROR in RegridTypeGet: requested non-existent bundle'
            status = ESMF_FAILURE
         endif
      endif
      if (present(dst_bundle)) then
         if (associated(regrid%dst_bundlep)) then
            dst_bundle%btypep => regrid%dst_bundlep
         else
            print *,'ERROR in RegridTypeGet: requested non-existent bundle'
            status = ESMF_FAILURE
         endif
      endif

      ! Get fields if requested
      if (present(src_field)) then
         if (associated(regrid%src_fieldp)) then
            src_field%ftypep => regrid%src_fieldp
         else
            print *,'ERROR in RegridTypeGet: requested non-existent field'
            status = ESMF_FAILURE
         endif
      endif
      if (present(dst_field)) then
         if (associated(regrid%dst_fieldp)) then
            dst_field%ftypep => regrid%dst_fieldp
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
! !IROUTINE: ESMF_RegridTypeSet - Set attribute of a Regrid

! !INTERFACE:
      subroutine ESMF_RegridTypeSet(regrid, name,            &
                                    src_bundle, dst_bundle,  &
                                    src_field,  dst_field,   &
                                    method, num_links, gather, rc)
!
! !ARGUMENTS:
      type(ESMF_RegridType), intent(inout) :: regrid

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
      
      ! Set name if requested
      if (present(name)) then
         call ESMF_SetName(regrid%base, name, "Regrid", stat)
         if (stat /= ESMF_SUCCESS) status = ESMF_FAILURE
      endif

      ! Set bundles if requested
      if (present(src_bundle)) regrid%src_bundlep => src_bundle%btypep
      if (present(dst_bundle)) regrid%dst_bundlep => dst_bundle%btypep
      
      ! Set fields if requested
      if (present(src_field)) regrid%src_fieldp => src_field%ftypep
      if (present(dst_field)) regrid%dst_fieldp => dst_field%ftypep
      
      ! get method or number of links
      if (present(method)) regrid%method = method
      if (present(num_links)) regrid%num_links = num_links
      if (present(gather)) regrid%gather = gather

      if (rcpresent) rc = status

      end subroutine ESMF_RegridTypeSet

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_RegridConstructEmpty - Create empty regrid structure

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
      
        nullify(regrid%src_bundlep)
        nullify(regrid%dst_bundlep)
        nullify(regrid%src_fieldp)
        nullify(regrid%dst_fieldp)
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
! !IROUTINE: ESMF_RegridDestruct - Free any Regrid memory allocated internally

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
        if (associated(regrid%src_bundlep)) nullify(regrid%src_bundlep)
        if (associated(regrid%dst_bundlep)) nullify(regrid%dst_bundlep)

!       if created with fields, nullify field pointers
        if (associated(regrid%src_fieldp)) nullify(regrid%src_fieldp)
        if (associated(regrid%dst_fieldp)) nullify(regrid%dst_fieldp)

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

        ! blithly assume the world is a good place
        rc = ESMF_SUCCESS

      end subroutine ESMF_RegridDestruct

!------------------------------------------------------------------------------

      end module ESMF_RegridTypesMod
