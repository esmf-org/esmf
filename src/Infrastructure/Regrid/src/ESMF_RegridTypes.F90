! $Id: ESMF_RegridTypes.F90,v 1.11 2003/08/27 23:38:29 nscollins Exp $
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
      use ESMF_LocalArrayMod
      use ESMF_ArrayBaseMod  ! ESMF array  class
      use ESMF_ArrayExpandMod  ! ESMF array  class
      use ESMF_RouteMod     ! ESMF route  class
      use ESMF_RHandleMod   ! ESMF route handle class
      use ESMF_PhysGridMod  ! ESMF physical grid class
      use ESMF_DistGridMod  ! ESMF distributed grid class
      use ESMF_GridMod      ! ESMF grid   class
      use ESMF_DataMapMod
      use ESMF_FieldMod     ! ESMF field  class
      use ESMF_BundleMod    ! ESMF bundle class

      implicit none

!------------------------------------------------------------------------------
!     !  ESMF_RegridType
!
!     ! Description of ESMF_RegridType.

      type ESMF_RegridType
      sequence
      private
        type (ESMF_Base) :: base

        type (ESMF_Array) :: & 
           srcarray,   &! source array
           dstarray     ! destination array

        type (ESMF_Grid) :: &
           srcgrid,    &! source grid
           dstgrid      ! destination grid

        type (ESMF_DataMap) :: &
           srcdatamap,    &! source datamap
           dstdatamap      ! destination datamap

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
      '$Id: ESMF_RegridTypes.F90,v 1.11 2003/08/27 23:38:29 nscollins Exp $'

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
      subroutine ESMF_RegridAddLink(tv, src_add, dst_add, weight, rc)
!
! !ARGUMENTS:

      type(ESMF_TransformValues), intent(inout) :: tv
      integer, dimension(2), intent(in) :: src_add
      integer, dimension(2), intent(in) :: dst_add
      real(kind=ESMF_IKIND_R8), intent(in) :: weight
      integer, intent(out) :: rc

!
! !DESCRIPTION:
!     Adds an address pair and regrid weight to an existing regrid.
!
!     The arguments are:
!     \begin{description}
!     \item[transformvalues]
!          Stored information related to the actual data transformation
!          needed when moving data from one grid to another.
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

      ! do these need to be 2 separate x and y arrays?
      integer, dimension(:), pointer :: src_ptr, dst_ptr
      real(kind=ESMF_IKIND_R8), dimension(:,:), pointer :: wgt_ptr
      type (ESMF_ARRAY) :: &! temps for use when re-sizing arrays
         src_add_tmp, dst_add_tmp, weights_tmp

      rc = ESMF_FAILURE

      call ESMF_LocalArrayGetData(tv%srcindex, src_ptr, ESMF_DATA_REF, rc)
      call ESMF_LocalArrayGetData(tv%dstindex, dst_ptr, ESMF_DATA_REF, rc)
      call ESMF_LocalArrayGetData(tv%weights, wgt_ptr, ESMF_DATA_REF, rc)

      !
      !  increment number of links for this regrid
      !
      
      tv%numlinks = tv%numlinks + 1
      
      !
      !  if new number of links exceeds array sizes, re-size arrays
      !
      
      ! TODO: resize check
      
      
      !
      ! TODO: Add addresses and weights to regrid arrays
      !
      src_ptr((tv%numlinks * 2) + 0) = src_add(1)
      src_ptr((tv%numlinks * 2) + 1) = src_add(2)
      dst_ptr((tv%numlinks * 2) + 0) = dst_add(1)
      dst_ptr((tv%numlinks * 2) + 1) = dst_add(2)
      wgt_ptr(tv%numlinks, 1) = weight


      rc = ESMF_SUCCESS      

      end subroutine ESMF_RegridAddLink

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_RegridTypeGet - Get attribute of a Regrid type

! !INTERFACE:
      subroutine ESMF_RegridTypeGet(regrid, name,                &
                                    srcarray, dstarray,        &
                                    srcgrid,  dstgrid,         &
                                    srcdatamap,  dstdatamap,   &
                                    method, num_links , gather, rc)
!
! !ARGUMENTS:

      type(ESMF_RegridType), intent(in) :: regrid

      character (*),      intent(out), optional :: name
      type (ESMF_Array),  intent(out), optional :: srcarray, dstarray
      type (ESMF_Grid),   intent(out), optional :: srcgrid,  dstgrid
      type (ESMF_DataMap),  intent(out), optional :: srcdatamap,  dstdatamap
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

      ! Get arrays if requested
      if (present(srcarray)) then
         srcarray = regrid%srcarray
      endif
      if (present(dstarray)) then
         dstarray = regrid%dstarray
      endif

      ! Get grids if requested
      if (present(srcgrid)) then
         srcgrid = regrid%srcgrid
      endif
      if (present(dstgrid)) then
         dstgrid = regrid%dstgrid
      endif

      ! Get datamaps if requested
      if (present(srcdatamap)) then
         srcdatamap = regrid%srcdatamap
      endif
      if (present(dstdatamap)) then
         dstdatamap = regrid%dstdatamap
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
      subroutine ESMF_RegridTypeSet(regrid, name,                &
                                    srcarray, dstarray,        &
                                    srcgrid,  dstgrid,         &
                                    srcdatamap,  dstdatamap,   &
                                    method, num_links, gather, rc)
!
! !ARGUMENTS:
      type(ESMF_RegridType), intent(inout) :: regrid

      character (*),      intent(in), optional :: name
      type (ESMF_Array),  intent(in), optional :: srcarray, dstarray
      type (ESMF_Grid),   intent(in), optional :: srcgrid,  dstgrid
      type (ESMF_DataMap), intent(in), optional :: srcdatamap,  dstdatamap
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

      ! Set arrays if requested
      if (present(srcarray)) regrid%srcarray = srcarray
      if (present(dstarray)) regrid%dstarray = dstarray
      
      ! Set grids if requested
      if (present(srcgrid)) regrid%srcgrid = srcgrid
      if (present(dstgrid)) regrid%dstgrid = dstgrid
      
      ! Set datamaps if requested
      if (present(srcdatamap)) regrid%srcdatamap = srcdatamap
      if (present(dstdatamap)) regrid%dstdatamap = dstdatamap
      
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
