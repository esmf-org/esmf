! $Id: ESMF_Regrid.F90,v 1.31 2003/08/28 18:47:02 nscollins Exp $
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
      use ESMF_BaseMod         ! ESMF base   class
      use ESMF_DELayoutMod     ! ESMF DE layout class
      use ESMF_ArrayBaseMod    ! ESMF array  class
      use ESMF_ArrayExpandMod  ! ESMF array  class
      use ESMF_RouteMod        ! ESMF route  class
      use ESMF_RHandleMod      ! ESMF route handle class
      use ESMF_PhysGridMod     ! ESMF physical grid class
      use ESMF_DistGridMod     ! ESMF distributed grid class
      use ESMF_GridMod         ! ESMF grid   class
      use ESMF_DataMapMod      ! ESMF datamap class
      use ESMF_FieldMod        ! ESMF field  class
      use ESMF_BundleMod       ! ESMF bundle class
      use ESMF_RegridTypesMod  ! ESMF regrid data types and utilities
      use ESMF_RegridBilinearMod  ! ESMF rg methods related to bilinear regrid
      use ESMF_RegridNearNbrMod  ! ESMF rg methods related to nearest-nbr regrid
      use ESMF_RegridConservMod ! ESMF rg methods related to conservative regrid

      implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
      private

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
      public ESMF_Regrid

!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:
!
      public ESMF_RegridCreate     ! create and fill a routehandle object
      public ESMF_RegridRun        ! perform a regrid operation

      public ESMF_RegridGet        ! returns value of a regrid attribute
      public ESMF_RegridDestroy    ! deallocate memory associated with a regrid
      public ESMF_RegridValidate   ! Error checking and validation
      public ESMF_RegridPrint      ! Prints various regrid info


!
!EOPI
!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
         '$Id: ESMF_Regrid.F90,v 1.31 2003/08/28 18:47:02 nscollins Exp $'

!==============================================================================

      contains

!==============================================================================
!
! This section includes some of the Regrid Create methods.
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_RegridCreate - Precomputes Regrid data

! !INTERFACE:
      subroutine ESMF_RegridCreate(srcarray, srcgrid, srcdatamap, &
                                            dstarray, dstgrid, dstdatamap, &
                                            routehandle, regridmethod, &
                                            srcmask, dstmask, &
                                            blocking, rc)
!
! !ARGUMENTS:
      type(ESMF_Array), intent(in) :: srcarray
      type(ESMF_Grid), intent(in) :: srcgrid
      type(ESMF_DataMap), intent(in) :: srcdatamap
      type(ESMF_Array), intent(inout) :: dstarray
      type(ESMF_Grid), intent(in) :: dstgrid
      type(ESMF_DataMap), intent(in) :: dstdatamap
      type(ESMF_RouteHandle), intent(inout) :: routehandle
      integer, intent(in), optional :: regridmethod
      type(ESMF_Mask), intent(in), optional :: srcmask
      type(ESMF_Mask), intent(in), optional :: dstmask
      type(ESMF_Async), intent(inout), optional :: blocking
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Given a source array, grid, and datamap, this routine precomputes
!     both the communication pattern needed to move data to the proper
!     processors, and the sparse matrix of weights needed to compute the data
!     interpolation for moving data from one grid to another.
!     This routine returns a handle to the precomputed information in the
!     {\tt routehandle} argument.  This same value should be supplied
!     at run time, along with the actual data pointers.  The same precomputed
!     handle can be used on any data which matches the data arrays, grid,
!     and datamaps supplied here, so one does not have to generate multiple
!     routehandles for similar data values.
!
!     TODO: Do we need the parent layout here at this level?  I believe not,
!     because for exclusive processor sets the higher level code should
!     have created proxy grid objects which then are both on the same layout.
!
!     The arguments are:
!     \begin{description}
!     \item [srcarray]
!           {\tt ESMF\_Array} containing source data.
!     \item [srcgrid]
!           {\tt ESMF\_Grid} which corresponds to how the data in the
!           source array has been decomposed.
!     \item [srcdatamap]
!           {\tt ESMF\_DataMap} which describes how the array maps to
!           the specified source grid.
!     \item [dstgrid]
!           {\tt ESMF\_Grid} which corresponds to how the data in the
!           destination array should be decomposed.
!     \item [dstdatamap]
!           {\tt ESMF\_DataMap} which describes how the array should map to
!           the specified destination grid.
!     \item [routehandle]
!           Returned value which identifies the precomputed Route and other
!           necessary information.
!     \item [{[regridtype]}]
!           Type of regridding to do.  A set of predefined types are
!           supplied.
!     \item [{[srcmask]}]
!           Optional {\tt ESMF\_Mask} identifying valid source data.
!     \item [{[dstmask]}]
!           Optional {\tt ESMF\_Mask} identifying valid destination data.
!     \item [{[blocking]}]
!           Optional argument which specifies whether the operation should
!           wait until complete before returning or return as soon
!           as the communication between {\tt DE}s has been scheduled.
!           If not present, default is to do synchronous communications.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!
!     \end{description}
!
!
!   The supported regridding methods for this create function are currently:
!   \begin{description}
!   \item[ESMF\_RegridMethod\_FieldCopy] same Grid, just copy the field
!   \item[ESMF\_RegridMethod\_Redist  ] same PhysGrid just redistribute field
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

      ! TODO: the interfaces have changed - this will no longer be called
      !  with fields, but with the grid, datamap, and array which are either
      !  contained in a field or are specified by the user separately in an
      !  array method call.  i believe the code at this level needs to 
      !  take an already declared routehandle and fill it in with the
      !  route and weights that it computes.  we have no "Regrid" object
      !  at this point - it is a subtype of a routehandle object.  so this
      !  could either compute the route and weights and return them, or
      !  it seems maybe better to fill them in here in case we end up with
      !  more than just a single route and weight array.
      ! TODO: so this code needs to be overhauled...   

      integer :: status             ! Error status
      logical :: rcpresent          ! Return code present
      character (len=ESMF_MAXSTR) :: regrid_name

      ! Initialize return code
      rcpresent = .FALSE.
      status = ESMF_FAILURE
      if(present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_FAILURE
      endif

      ! Call the appropriate create routine based on method choice

      select case(regridmethod)

      !-------------
      case(ESMF_RegridMethod_FieldCopy) ! copy field
         !*** no regrid type required
         print *, "ERROR in ESMF_RegridCreate: ", &
                  "Field copy not yet supported"
         status = ESMF_FAILURE

      !-------------
      case(ESMF_RegridMethod_Redist)   ! redistribution of field
         print *, "ERROR in ESMF_RegridCreate: ", &
                  "Redistribution not yet supported"
         status = ESMF_FAILURE

      !-------------
      case(ESMF_RegridMethod_Bilinear) ! bilinear
          routehandle = ESMF_RegridConstructBilinear( &
                                              srcarray, srcgrid, srcdatamap, &
                                              dstarray, dstgrid, dstdatamap, &
                                              srcmask, dstmask, rc=status)

      !-------------
      case(ESMF_RegridMethod_Bicubic)  ! bicubic
         print *, "ERROR in ESMF_RegridCreate: ", &
                  "Bicubic not yet supported"
         status = ESMF_FAILURE

      !-------------
      case(ESMF_RegridMethod_Conserv1)
      !   routehandle = ESMF_RegridConstructConserv(srcarray, dstarray, &
      !                                        regrid_name, order=1, rc=status)
      !-------------
      case(ESMF_RegridMethod_Conserv2) ! 2nd-order conservative
      !   routehandle = ESMF_RegridConstructConserv(srcarray, dstarray, &
      !                                        regrid_name, order=2, rc=status)
      !-------------
      case(ESMF_RegridMethod_Raster) ! regrid by rasterizing domain
         print *, "ERROR in ESMF_RegridCreate: ", &
                  "Raster method not yet supported"
         status = ESMF_FAILURE
      !-------------
      case(ESMF_RegridMethod_NearNbr) ! nearest-neighbor dist-weighted avg
      !   routehandle = ESMF_RegridConstructNearNbr(srcarray, dstarray, &
      !                                        regrid_name, rc=status)
      !-------------
      case(ESMF_RegridMethod_Fourier) ! Fourier transform
         print *, "ERROR in ESMF_RegridCreate: ", &
                  "Fourier transforms not yet supported"
         status = ESMF_FAILURE
      !-------------
      case(ESMF_RegridMethod_Legendre) ! Legendre transform
         print *, "ERROR in ESMF_RegridCreate: ", &
                  "Legendre transforms not yet supported"
         status = ESMF_FAILURE
      !-------------
      case(ESMF_RegridMethod_Index) ! index-space regridding (shift, stencil)
         print *, "ERROR in ESMF_RegridCreate: ", &
                  "Index-space methods not yet supported"
         status = ESMF_FAILURE
      !-------------
      case(ESMF_RegridMethod_Linear) ! linear for 1-d regridding
         print *, "ERROR in ESMF_RegridCreate: ", &
                  "1-d linear methods not yet supported"
         status = ESMF_FAILURE
      !-------------
      case(ESMF_RegridMethod_Spline) ! cubic spline for 1-d regridding
         print *, "ERROR in ESMF_RegridCreate: ", &
                  "1-d cubic splines not yet supported"
         status = ESMF_FAILURE
      !-------------
      case(ESMF_RegridMethod_User) ! cubic spline for 1-d regridding
         print *, "ERROR in ESMF_RegridCreate: ", &
                  "User-defined regridding not yet supported"
         status = ESMF_FAILURE
      !-------------
      case default
         print *, "ERROR in ESMF_RegridCreate: Invalid method"
         status = ESMF_FAILURE
      end select

      if (status /= ESMF_SUCCESS) then
         ! Use error function eventually...
         print *, 'ERROR in ESMF_RegridCreate: error in creation'
      endif

      if (rcpresent) rc = status

      end subroutine ESMF_RegridCreate

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_RegridRun - Performs a regridding between two arrays

! !INTERFACE:
      subroutine ESMF_RegridRun(srcarray, dstarray, routehandle, rc)
!
! !ARGUMENTS:

      type (ESMF_Array), intent(in) :: &
         srcarray            ! array to be regridded
         
      type (ESMF_RouteHandle), intent(in) :: &
         routehandle          ! precomputed regrid structure with
                              !  regridding info

      type (ESMF_Array), intent(out) :: &
         dstarray            ! resulting regridded array

      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Given a source array and precomputed regridding information, this 
!     routine regrids the source array to a new array on the destination
!     grid.  
!
!EOP
    integer :: status
    logical :: rcpresent
    type(ESMF_Route) :: rh
    type(ESMF_Array) :: tempdst 
    type(ESMF_TransformValues) :: tv

   ! get the first route from the table and run it to gather the
   ! data values which overlap this bounding box.
 
   call ESMF_RouteHandleGet(routehandle, route1=rh, rc=status)

   ! from the domain or from someplace, get the counts of how many data points
   ! we are expecting from other DEs.  we might also need to know what
   ! data type is coming since this is user data and not coordinates at 
   ! execution time.  or does the incoming data have to match the type
   ! of the outgoing array?  so we can get the data type and shape from
   ! the dstarray argument to this function.  and what about halo widths?

   !tempdst = ESMF_ArrayCreate(rank, type, kind, counts, halo_widths, rc)
   !call ESMF_RouteRun(rh, srcarray, tempdst, status)

   ! get the indirect indicies and weights from the routehandle

   !call ESMF_RouteHandleGet(routehandle, transformvalues=tv, rc=status)
   !call ESMF_TransformValues(tv, ?=srcindex, ?=dstindex, ?=weights)

   ! get a real f90 pointer from all the arrays
   ! i4ptr and r8ptr TKR can be fixed, but unfortunately the dptr can be
   ! whatever the user wants - so this code might need to move into
   ! another file and be macroized heavily for TKR.
   !call ESMF_ArrayGetData(srcindex, i4ptr, ESMF_DATA_REF, rc)
   !call ESMF_ArrayGetData(dstindex, i4ptr, ESMF_DATA_REF, rc)
   !call ESMF_ArrayGetData(weights, r8ptr, ESMF_DATA_REF, rc)

   !call ESMF_ArrayGetData(tempdst, dptr, ESMF_DATA_REF, rc)
   !call ESMF_ArrayGetData(dstaddr, dptr, ESMF_DATA_REF, rc)

   ! TODO: apply the weights from src to destination
   !  does this need a nested loop and an array of ESMF_Arrays, one
   !  for each DE which sends data?  i think the answer is not for now
   !  because all data has been pushed into a single array.  but eventually
   !  if we want to start supporting vectors or other complicated data 
   !  shapes, we may have to start preserving the array and datamaps
   !  from the original locations.

   !*** initialize dest field to zero
   
   !dstarrayptr = zero

   !*** do the regrid

   ! will look something like   
   !do n=1,num_links
   !  dstarrayptr(dstindexptr(n)) = dstarrayptr(dstindexptr(n)) + &
   !                                tempdstptr(srcindexptr(n)) * weightptr(n))
   !end do

   ! set return codes
   ! nuke temp array

   end subroutine ESMF_RegridRun

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_RegridGet - Get an attribute of a Regrid

! !INTERFACE:
      subroutine ESMF_RegridGet(regrid, name,            &
                                srcarray, dstarray,  &
                                srcgrid,  dstgrid,   &
                                srcdatamap,  dstdatamap,   &
                                method, num_links , gather, rc)
!
! !ARGUMENTS:

      type(ESMF_Regrid),  intent(inout) :: regrid
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
!          Regrid to be queried.
!     \item[TODO:] fix this doc - make it match arg list and fix double
!          brackets to be bracket, curley brace, bracket.
!     \item[[name]]
!          Name for this regrid.
!     \item[[srcbundle]]
!          If created with bundles, the source bundle associated
!          with this regrid. 
!     \item[[dstbundle]]
!          If created with bundles, the destination bundle associated
!          with this regrid. 
!     \item[[srcfield]]
!          If created with fields, the source field associated
!          with this regrid. 
!     \item[[dstfield]]
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

      logical :: rcpresent
      integer :: status

      ! Assume failure until sure of success
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if (present(rc)) then 
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
      endif
      
      ! Get name if requested
      if (present(name)) then
         call ESMF_RegridTypeGet(regrid%ptr, name=name, rc=status)
         if (status .ne. ESMF_SUCCESS) then
             print *, "ESMF_RegridGet: failed getting regrid name"
             return
         endif
      endif

! TODO: make these arrays, grid, datamaps.   Are we really going to 
!  store the arrays & grids this was based on?  why?   nsc.
!      ! Get bundles if requested
!      if (present(srcbundle)) then
!         call ESMF_RegridTypeGet(regrid%ptr, srcbundle=srcbundle, rc=status)
!        if (status .ne. ESMF_SUCCESS) then
!            print *, "ESMF_RegridGet: failed getting regrid bundle"
!            return
!        endif
!      endif
!      if (present(dstbundle)) then
!         call ESMF_RegridTypeGet(regrid%ptr, dstbundle=dstbundle, rc=status)
!        if (status .ne. ESMF_SUCCESS) then
!            print *, "ESMF_RegridGet: failed getting regrid bundle"
!            return
!        endif
!      endif
!
!      ! Get fields if requested
!      if (present(srcfield)) then
!         call ESMF_RegridTypeGet(regrid%ptr, srcfield=srcfield, rc=status)
!        if (status .ne. ESMF_SUCCESS) then
!            print *, "ESMF_RegridGet: failed getting regrid ..."
!            return
!        endif
!      endif
!      if (present(dstfield)) then
!         call ESMF_RegridTypeGet(regrid%ptr, dstfield=dstfield, rc=stat)
!        if (status .ne. ESMF_SUCCESS) then
!            print *, "ESMF_RegridGet: failed getting regrid ..."
!            return
!        endif
!         if (stat /= ESMF_SUCCESS) status = ESMF_FAILURE
!      endif
!
      ! get method or number of links or gather route
      if (present(method)) then
         call ESMF_RegridTypeGet(regrid%ptr, method=method, rc=status)
         if (status .ne. ESMF_SUCCESS) then
             print *, "ESMF_RegridGet: failed getting regrid ..."
             return
         endif
      endif
      if (present(num_links)) then
         call ESMF_RegridTypeGet(regrid%ptr, num_links=num_links, rc=status)
         if (status .ne. ESMF_SUCCESS) then
             print *, "ESMF_RegridGet: failed getting regrid ..."
             return
         endif
      endif
      if (present(gather)) then
         call ESMF_RegridTypeGet(regrid%ptr, gather=gather, rc=status)
         if (status .ne. ESMF_SUCCESS) then
             print *, "ESMF_RegridGet: failed getting regrid ..."
             return
         endif
      endif

      if (rcpresent) rc = status

      end subroutine ESMF_RegridGet

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_RegridDestroy - Free all resources associated with a Regrid

! !INTERFACE:
      subroutine ESMF_RegridDestroy(routehandle, rc)
!
! !ARGUMENTS:
      type(ESMF_RouteHandle), intent(inout) :: routehandle
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Destroys all a {\tt Regrid} object previously allocated
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

      integer :: status             ! Error status
      logical :: rcpresent          ! Return code present

      ! Initialize return code
      rcpresent = .FALSE.
      status = ESMF_FAILURE
      if(present(rc)) then
        rcpresent=.TRUE.
        rc = ESMF_FAILURE
      endif

      ! Does this destroy each of the routes?  it certainly needs to
      !  destroy the arrays in the TransformValues object.  (i think the
      !  answer is yes - this code should call route delete here.)

      !call ESMF_RouteHandleGet(routehandle, rhandle1=rh, transformvalues=tv, &
      !                          rc=status)
      !call ESMF_RouteDestroy(rh, status)
      !call ESMF_TransformValuesDestroy(tv, status)
      if (status /= ESMF_SUCCESS) then
        ! Use error function eventually...
        print *, "ERROR in ESMF_RegridDestroy: Regrid destruct"
        return
      endif

      ! Set return values.
      if (rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_RegridDestroy

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_RegridValidate - Check internal consistency of a Regrid

! !INTERFACE:
      subroutine ESMF_RegridValidate(routehandle, opt, rc)
!
! !ARGUMENTS:
      type(ESMF_RouteHandle), intent(in) :: routehandle
      character (len=*), intent(in), optional :: opt
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Validates that a Regrid is internally consistent.
!
!     The arguments are:
!     \begin{description}
!     \item[routehandle]
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
      ! code goes here
      ! 

      end subroutine ESMF_RegridValidate

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_RegridPrint - Print the contents of a Regrid

! !INTERFACE:
      subroutine ESMF_RegridPrint(routehandle, opt, rc)
!
! !ARGUMENTS:
      type(ESMF_RouteHandle), intent(in) :: routehandle
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

      ! TODO: does this even need to be here?  it seems the print and
      ! validate routines should be in the routehandle code only.

      ! 
      ! code goes here
      ! 

      end subroutine ESMF_RegridPrint

!------------------------------------------------------------------------------

   end module ESMF_RegridMod
