! $Id: ESMF_FieldComm.F90,v 1.69 2005/02/28 21:56:29 nscollins Exp $
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
#define ESMF_FILENAME "ESMF_FieldComm.F90"
!
!     ESMF Field Communications module
      module ESMF_FieldCommMod
!
!==============================================================================
!
! This file contains the Field communication methods.
!
!------------------------------------------------------------------------------
! INCLUDES
#include "ESMF.h"

!------------------------------------------------------------------------------
!
!BOPI
! !MODULE: ESMF_FieldCommMod - Communication routines for Field objects
!
! !DESCRIPTION:
! The code in this file implements the {\tt ESMF\_Field} class
! communication routines, including Regridding, Redistribution, Halo, Gather,
! Scatter, and others.
!
! This type is implemented in Fortran 90 and a corresponding
! C++ interface is provided for access.
!
!------------------------------------------------------------------------------
! !USES:
      use ESMF_BaseTypesMod    ! ESMF base class
      use ESMF_BaseMod
      use ESMF_LogErrMod
      use ESMF_IOSpecMod
      use ESMF_VMMod
      use ESMF_DELayoutMod    ! ESMF layout class
      use ESMF_LocalArrayMod
      use ESMF_ArrayMod
      use ESMF_RHandleMod
      use ESMF_RouteMod
      use ESMF_ArrayCommMod
      use ESMF_ArrayDataMapMod
      use ESMF_GridTypesMod
      use ESMF_GridMod
      use ESMF_FieldDataMapMod
      use ESMF_FieldMod
      use ESMF_RegridMod
      use ESMF_RegridTypesMod
      implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
       private

!  <none>
!------------------------------------------------------------------------------
! !PUBLIC TYPES:
!  <none>

!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:
!

   ! These are the recommended entry points; the code itself is in Array:
                             ! Synchronize boundary data between decompositions
   public ESMF_FieldHaloStore, ESMF_FieldHalo, ESMF_FieldHaloRelease 
                             ! Redistribute existing arrays, matching grids
   public ESMF_FieldRedistStore, ESMF_FieldRedist, ESMF_FieldRedistRelease 
                             ! Regridding and interpolation, different grids
   public ESMF_FieldRegridStore, ESMF_FieldRegrid, ESMF_FieldRegridRelease 

   public ESMF_FieldGather   ! Combine 1 decomposed field into 1 on 1 DE
  !public ESMF_FieldAllGather! Combine 1 decomposed field into N copies on N DEs

   public ESMF_FieldScatter  ! Split 1 field into a decomposed one over N DEs
  !public ESMF_FieldBroadcast! Send 1 field to all DEs, none decomposed
  !public ESMF_FieldAlltoAll ! might make sense with bundles; each DE could
                             ! call with a different non-decomposed field
                             ! and the result would be a packed bundle of
                             ! data with decomposed fields on each DE.

   public ESMF_FieldReduce     ! Global reduction operation, return on 1 DE
  !public ESMF_FieldAllReduce  ! Global reduction operation, return on each DE

!
!EOPI

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_FieldComm.F90,v 1.69 2005/02/28 21:56:29 nscollins Exp $'

!==============================================================================
!
! INTERFACE BLOCKS
!
!==============================================================================
!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_FieldHalo - Temporary interface to ease transition
!
! !INTERFACE:
      interface ESMF_FieldHalo
   
! !PRIVATE MEMBER FUNCTIONS:
        module procedure ESMF_FieldHaloRun
        module procedure ESMF_FieldHaloDeprecated

! !DESCRIPTION:
!     Temporary interface to east transition from old syntax to new.
!     All new code should be using the {\tt ESMF\_FieldHaloRun} syntax.
!    
!EOPI
      end interface
!
!
!
!==============================================================================
!
      contains
!
!==============================================================================
!
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!
! Entry points for fuctionality which will happen mostly below at the Grid
!   level, but needs a Data Pointer as well as grid info to operate.
!   These include Reduction operations, Halo, and Transpose.
!
!------------------------------------------------------------------------------
#if 0
!-------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldAllGather"
!BOPI
! !IROUTINE: ESMF_FieldAllGather - Data allgather operation on a Field

! !INTERFACE:
      subroutine ESMF_FieldAllGather(field, array, blockingflag, commhandle, rc)
!
!
! !ARGUMENTS:
      type(ESMF_Field), intent(in) :: field                 
      type(ESMF_Array), intent(out) :: array
      type(ESMF_BlockingFlag), intent(in), optional :: blockingflag
      type(ESMF_CommHandle), intent(inout), optional :: commhandle
      integer, intent(out), optional :: rc               
!
! !DESCRIPTION:
!     Perform an allgather operation
!     over the data in an {\tt ESMF\_Field}.  If the {\tt ESMF\_Field} is
!     decomposed over N DEs, this routine returns a copy of the
!     entire collected data {\tt ESMF\_Array} on each of the N DEs.
!
!     The arguments are:
!     \begin{description}
!     \item [field] 
!           {\tt ESMF\_Field} containing data to be gathered.
!     \item [array] 
!           Newly created array containing the collected data.
!           It is the size of the entire undecomposed grid.
!     \item [{[blockingflag]}]
!           Optional argument which specifies whether the operation should
!           wait until complete before returning or return as soon
!           as the communication between DEs has been scheduled.
!           If not present, default is to do synchronous communications.
!           Valid values for this flag are {\tt ESMF\_BLOCKING} and 
!           {\tt ESMF\_NONBLOCKING}.  
!      (This feature is not yet supported.  All operations are synchronous.)
!     \item [{[commhandle]}]
!           If the {\tt blockingflag} is set to {\tt ESMF\_NONBLOCKING} this 
!           argument is required.  Information about the pending operation
!           will be stored in the {\tt ESMF\_CommHandle} and can be queried
!           or waited for later.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS: 

      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present
      type(ESMF_FieldType), pointer :: ftypep     ! field type info
   
      ! Initialize return code   
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent = .TRUE. 
        rc = ESMF_FAILURE
      endif     

      ftypep => field%ftypep

      ! Call Array method to perform actual work
      call ESMF_ArrayAllGather(ftypep%localfield%localdata, ftypep%grid, &
                               ftypep%mapping, array, status)
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      ! Set return values.
      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldAllGather
#endif


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldGather"
!BOP
! !IROUTINE: ESMF_FieldGather - Data gather operation on a Field

! !INTERFACE:
      subroutine ESMF_FieldGather(field, dstPET, array, blockingflag, &
                                  commhandle, rc)
!
!
! !ARGUMENTS:
      type(ESMF_Field), intent(in) :: field                 
      integer, intent(in) :: dstPET
      type(ESMF_Array), intent(out) :: array
      type(ESMF_BlockingFlag), intent(in), optional :: blockingflag
      type(ESMF_CommHandle), intent(inout), optional :: commhandle
      integer, intent(out), optional :: rc               
!
! !DESCRIPTION:
!     Collect all local data associated with a distributed {\tt ESMF\_Field}
!     into a new {\tt ESMF\_Array} which is created only on a single PET.
!     This routine must be called collectively, that is, on all PETs in 
!     an {\tt ESMF\_VM}.  The framework will create a new 
!     {\tt ESMF\_Array} to hold the resulting data only on the specified 
!     destination PET.  After this call returns the {\tt array} argument
!     will be valid only on the {\tt dstPET} and invalid on all other PETs.
!     The input {\tt field} will be unchanged; the routine creates a copy of
!     the collected data.
!
!     The arguments are:
!     \begin{description}
!     \item [field] 
!           {\tt ESMF\_Field} containing data to be gathered.
!     \item [dstPET] 
!           Destination PET number where the gathered data is to be returned.
!     \item [array] 
!           Newly created {\tt ESMF\_Array} containing the collected data on 
!           the specified PET.  It is the size of the entire undecomposed grid.
!           On all other PETs this argument returns an invalid object.
!           Note that the user should not create an {\tt ESMF\_Array} before
!           making this call; the {\tt ESMF\_Array} should be an uninitialized
!           variable.  When this routine returns, there will be a valid 
!           {\tt ESMF\_Array} only on
!           the specified PET number, so code which will access the 
!           {\tt ESMF\_Array} should check the current PET number and only
!           try to access it from a single PET.
!     \item [{[blockingflag]}]
!           Optional argument which specifies whether the operation should
!           wait until complete before returning or return as soon
!           as the communication between DEs has been scheduled.
!           If not present, the default is to do synchronous communications.
!           Valid values for this flag are {\tt ESMF\_BLOCKING} and 
!           {\tt ESMF\_NONBLOCKING}.
!      (This feature is not yet supported.  All operations are synchronous.)
!     \item [{[commhandle]}]
!           If the {\tt blockingflag} is set to {\tt ESMF\_NONBLOCKING} this 
!           argument is required.  Information about the pending operation
!           will be stored in the {\tt ESMF\_CommHandle} and can be queried
!           or waited for later.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS: 

      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present
      type(ESMF_FieldType), pointer :: ftypep     ! field type info
   
      ! Initialize return code   
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent = .TRUE. 
        rc = ESMF_FAILURE
      endif     
 
      ftypep => field%ftypep

      call ESMF_ArrayGather(ftypep%localfield%localdata, &
                            ftypep%grid, ftypep%mapping, dstPET, &
                            array, status)
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      ! Set return values.
      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldGather


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldHaloRun"
!BOP
! !IROUTINE: ESMF_FieldHalo - Execute a halo operation on a Field

! !INTERFACE:
      ! Private name; call using ESMF_FieldHalo()
      subroutine ESMF_FieldHaloRun(field, routehandle, blockingflag, &
                                   commhandle, halodirection, routeOptions, rc)
!
!
! !ARGUMENTS:
      type(ESMF_Field), intent(inout) :: field
      type(ESMF_RouteHandle), intent(inout) :: routehandle
      type(ESMF_BlockingFlag), intent(in), optional :: blockingflag
      type(ESMF_CommHandle), intent(inout), optional :: commhandle
      type(ESMF_HaloDirection), intent(in), optional :: halodirection
      type(ESMF_RouteOptions), intent(in), optional :: routeOptions
      integer, intent(out), optional :: rc               
!
! !DESCRIPTION:
!     Perform a halo operation over the data
!     in an {\tt ESMF\_Field}.  This routine updates the data 
!     inside the {\tt ESMF\_Field} in place.
!
!     The arguments are:
!     \begin{description}
!     \item [field] 
!           {\tt ESMF\_Field} containing data to be haloed.
!     \item [routehandle] 
!           {\tt ESMF\_RouteHandle} which was returned by the corresponding
!           {\tt ESMF\_FieldHaloStore()} call. It is associated with 
!           the precomputed data movement and communication needed to 
!           perform the halo operation.
!     \item [{[blockingflag]}]
!           Optional argument which specifies whether the operation should
!           wait until complete before returning or return as soon
!           as the communication between DEs has been scheduled.
!           If not present, default is what was specified at Store time.
!           If {\tt both} was specified at Store time, this defaults to  
!           blocking.
!           Valid values for this flag are {\tt ESMF\_BLOCKING} and 
!           {\tt ESMF\_NONBLOCKING}.
!      (This feature is not yet supported.  All operations are synchronous.)
!     \item [{[commhandle]}]
!           If the {\tt blockingflag} is set to {\tt ESMF\_NONBLOCKING} this 
!           argument is required.  Information about the pending operation
!           will be stored in the {\tt ESMF\_CommHandle} and can be queried
!           or waited for later.
!     \item [{[halodirection]}]
!           Optional argument to restrict halo direction to a subset of the
!           possible halo directions.  If not specified, the halo is executed
!           along all boundaries.  This option is used only in the situation where
!           the halo must be precomputed at this time.
!           (This feature is not yet supported.)
!     \item [{[routeOptions]}]
!           Not normally specified.  Specify which internal strategy to select
!           when executing the communication needed to update the halo.
!           See Section~\ref{opt:routeopt} for possible values.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS: 

      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present
      integer :: htype
      logical :: allInOne
      type(ESMF_FieldType), pointer :: ftypep     ! field type info
   
      ! Initialize return code   
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent = .TRUE. 
        rc = ESMF_FAILURE
      endif     

      ! Initialize other variables
      ftypep => field%ftypep
      allInOne = .false.

      ! if the routehandle has not been precomputed, do so now
      ! first check if the RouteHandle is valid (constructed)
      call ESMF_RouteHandleValidate(routehandle, rc=status)
      
      ! if valid, check the routehandle type
      if (status.eq.ESMF_SUCCESS) then
        call ESMF_RouteHandleGet(routehandle, htype=htype, rc=status)
        if (htype .eq. ESMF_UNINITIALIZEDHANDLE) then
          allInOne = .true.
        elseif (htype .ne. ESMF_HALOHANDLE) then
          call ESMF_LogMsgSetError(ESMF_RC_ARG_VALUE, &
                                   "routehandle not defined for halo", &
                                   ESMF_CONTEXT, rc)
          return
        endif
      ! if not valid, try calling HaloStore to initialize
      else
        allInOne = .true.
      endif

      ! if needed, call FieldHaloStore to set up the routehandle
      if (allInOne) then
        call ESMF_LogWrite("uninitialized routehandle: calling FieldHaloStore", &
                              ESMF_LOG_WARNING, &
                              ESMF_CONTEXT)
        call ESMF_FieldHaloStore(field, routehandle, halodirection, &
                                  routeOptions, status)
      endif

      call ESMF_ArrayHalo(ftypep%localfield%localdata, routehandle, &
                          blockingflag, commhandle, routeOptions, rc=status)
      if (ESMF_LogMsgFoundError(status, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldHaloRun

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldHaloRelease"
!BOP
! !IROUTINE: ESMF_FieldHaloRelease - Release resources associated w/ handle

! !INTERFACE:
      subroutine ESMF_FieldHaloRelease(routehandle, rc)
!
!
! !ARGUMENTS:
      type(ESMF_RouteHandle), intent(inout) :: routehandle
      integer, intent(out), optional :: rc               
!
! !DESCRIPTION:
!     Release all stored information about the halo operation associated
!     with this {\tt ESMF\_RouteHandle}.
!
!     The arguments are:
!     \begin{description}
!     \item [routehandle] 
!           {\tt ESMF\_RouteHandle} associated with this halo operation.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP

      call ESMF_RouteHandleDestroy(routehandle, rc)

      end subroutine ESMF_FieldHaloRelease

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldHaloStore"
!BOP
! !IROUTINE: ESMF_FieldHaloStore - Precompute a halo operation on a Field

! !INTERFACE:
      subroutine ESMF_FieldHaloStore(field, routehandle, halodirection, &
                                     routeOptions, rc)
!
!
! !ARGUMENTS:
      type(ESMF_Field), intent(inout) :: field
      type(ESMF_RouteHandle), intent(inout) :: routehandle
      type(ESMF_HaloDirection), intent(in), optional :: halodirection
      type(ESMF_RouteOptions), intent(in), optional :: routeOptions
      integer, intent(out), optional :: rc               
!
! !DESCRIPTION:
!     Precompute the data movement or communication operations needed
!     to perform a halo operation over the data in an {\tt ESMF\_Field}.
!     The list of operations will be associated internally to the
!     framework with the 
!     {\tt ESMF\_RouteHandle} object.  
!     To perform the actual halo operation
!     the {\tt ESMF\_FieldHalo()} routine must be called with the
!     {\tt ESMF\_Field} containing the data to be updated and the
!     {\tt ESMF\_RouteHandle} computed during this store call.
!     If more than one {\tt ESMF\_Field} has identical
!     {\tt ESMF\_Grid}s and {\tt ESMF\_FieldDataMap}s, then
!     the same {\tt ESMF\_RouteHandle} can be computed once and used
!     in multiple executions of the halo operation.

!     The arguments are:
!     \begin{description}
!     \item [field] 
!           {\tt ESMF\_Field} containing data to be haloed.
!     \item [routehandle] 
!           {\tt ESMF\_RouteHandle} which will be returned after being
!           associated with the precomputed
!           information for a halo operation on this {\tt ESMF\_Field}.
!           This handle must be supplied at run time to execute the halo.
!     \item [{[halodirection]}]
!           Optional argument to restrict halo direction to a subset of the
!           possible halo directions.  If not specified, the halo is executed
!           along all boundaries. (This feature is not yet supported.)
!     \item [{[routeOptions]}]
!           Not normally specified.  Specify which internal strategy to select
!           when executing the communication needed to update the halo.
!           See Section~\ref{opt:routeopt} for possible values.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS: 

      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present
      type(ESMF_FieldType), pointer :: ftypep     ! field type info
   
      ! Initialize return code   
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent = .TRUE. 
        rc = ESMF_FAILURE
      endif     

      ! Sanity checks for good field, and that it has an associated grid
      ! and data before going down to the next level.
      if (.not.associated(field%ftypep)) then
         if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                "Uninitialized or already destroyed Field", &
                                 ESMF_CONTEXT, rc)) return
      endif

      ftypep => field%ftypep

      if (ftypep%fieldstatus .ne. ESMF_STATUS_READY) then
         if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                "Uninitialized or already destroyed Field", &
                                 ESMF_CONTEXT, rc)) return
      endif


      call ESMF_ArrayHaloStore(ftypep%localfield%localdata, ftypep%grid, &
                               ftypep%mapping, routehandle, &
                               halodirection, routeOptions, rc=status)
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldHaloStore

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldRedist"
!BOP
! !IROUTINE: ESMF_FieldRedist - Data redistribution operation on a Field

! !INTERFACE:
      subroutine ESMF_FieldRedist(srcField, dstField, routehandle, &
                                  blockingflag, commhandle, parentVM, &
                                  routeOptions, rc)
!
! !ARGUMENTS:
      type(ESMF_Field), intent(in) :: srcField
      type(ESMF_Field), intent(inout) :: dstField
      type(ESMF_RouteHandle), intent(inout) :: routehandle
      type(ESMF_BlockingFlag), intent(in), optional :: blockingflag
      type(ESMF_CommHandle), intent(inout), optional :: commhandle
      type(ESMF_VM), intent(in), optional :: parentVM
      type(ESMF_RouteOptions), intent(in), optional :: routeOptions
      integer, intent(out), optional :: rc               
!
! !DESCRIPTION:
!     Perform a redistribution operation over the data
!     in an {\tt ESMF\_Field}.  
!     This routine reads the source field and leaves 
!     the data untouched.  It reads the {\t ESMF\_Grid} 
!     and {\tt ESMF\_FieldDataMap}
!     from the destination field and updates the array data in the destination.
!     The {\tt ESMF\_Grid}s may have different decompositions (different
!     {\tt ESMF\_DELayout}s) or different data maps, but the source and
!     destination grids must describe the same set of coordinates.
!     Unlike {\tt ESMF\_FieldRegrid} this routine does not do interpolation,
!     only data movement.
!
!     The arguments are:
!     \begin{description}
!     \item [srcField]
!           {\tt ESMF\_Field} containing source data.
!     \item [dstField]
!           {\tt ESMF\_Field} containing destination grid.
!     \item [routehandle]          
!           {\tt ESMF\_RouteHandle} which was returned by the corresponding
!           {\tt ESMF\_FieldRedistStore()} call. It is associated with
!           the precomputed data movement and communication needed to
!           perform the redistribution operation.
!     \item [{[blockingflag]}]
!           Optional argument which specifies whether the operation should
!           wait until complete before returning or return as soon
!           as the communication between DEs has been scheduled.
!           If not present, default is to do synchronous communication.
!           Valid values for this flag are {\tt ESMF\_BLOCKING} and 
!           {\tt ESMF\_NONBLOCKING}.
!      (This feature is not yet supported.  All operations are synchronous.)
!     \item [{[commhandle]}]
!           If the {\tt blockingflag} is set to {\tt ESMF\_NONBLOCKING} this 
!           argument is required.  Information about the pending operation
!           will be stored in the {\tt ESMF\_CommHandle} and can be queried
!           or waited for later.
!     \item [{[parentVM]}]
!           {\tt ESMF\_VM} which encompasses both {\tt ESMF\_Field}s,
!           most commonly the VM of the Coupler if the redistribution is
!           inter-component, but could also be the individual VM for a
!           component if the redistribution is intra-component.  This argument
!           is only used in the situation where the routehandle has not been
!           precomputed yet.
!     \item [{[routeOptions]}]
!           Not normally specified.  Specify which internal strategy to select
!           when executing the communication needed to redistribute the data.
!           See Section~\ref{opt:routeopt} for possible values.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS: 

      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present
      integer :: htype
      logical :: allInOne
      type(ESMF_FieldType), pointer :: dstFtypep, srcFtypep
   
      ! Initialize return code   
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent = .TRUE. 
        rc = ESMF_FAILURE
      endif     

      ! Initialize other variables
      dstFtypep => dstField%ftypep
      srcFtypep => srcField%ftypep
      allInOne = .false.

      ! if the routehandle has not been precomputed, do so now
      ! first check if the RouteHandle is valid (constructed)
      call ESMF_RouteHandleValidate(routehandle, rc=status)

      ! if valid, check the routehandle type
      if (status.eq.ESMF_SUCCESS) then
        call ESMF_RouteHandleGet(routehandle, htype=htype, rc=status)
        if (htype .eq. ESMF_UNINITIALIZEDHANDLE) then
          allInOne = .true.
        elseif (htype .ne. ESMF_REDISTHANDLE) then
          call ESMF_LogMsgSetError(ESMF_RC_ARG_VALUE, &
                                   "routehandle not defined for redist", &
                                   ESMF_CONTEXT, rc)
          return
        endif
      ! if not valid, try calling RedistStore to initialize
      else
        allInOne = .true.
      endif

      ! if needed, call FieldRedistStore
      if (allInOne) then
        call ESMF_LogWrite("uninitialized routehandle: calling FieldRedistStore", &
                              ESMF_LOG_WARNING, &
                              ESMF_CONTEXT)
        if (.not. present(parentVM)) then
          call ESMF_LogMsgSetError(ESMF_RC_ARG_VALUE, &
          "if parentVM not specified, routehandle must be precomputed", &
                                   ESMF_CONTEXT, rc)
          return
        endif
        call ESMF_FieldRedistStore(srcField, dstField, parentVM, &
                                   routeOptions, routehandle, status)
        if (ESMF_LogMsgFoundError(status, &
              "routehandle invalid, and unable to precompute one on the fly", &
                                  ESMF_CONTEXT, rc)) return
        
      endif

      call ESMF_ArrayRedist(srcFtypep%localfield%localdata, &
                            dstFtypep%localfield%localdata, &
                            routehandle, blockingflag, commhandle, &
                            routeOptions, status)
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      ! if this is a redist call with an uninitialized routehandle, then
      ! destroy it before returning    TODO:  is this right?
      if (allInOne) call ESMF_FieldRedistRelease(routehandle, status)

      ! Set return values.
      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldRedist

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldRedistRelease"
!BOP
! !IROUTINE: ESMF_FieldRedistRelease - Release resources associated w/ handle

! !INTERFACE:
      subroutine ESMF_FieldRedistRelease(routehandle, rc)
!
!
! !ARGUMENTS:
      type(ESMF_RouteHandle), intent(inout) :: routehandle
      integer, intent(out), optional :: rc               
!
! !DESCRIPTION:
!     Release all stored information about the redistribution associated
!     with this {\tt ESMF\_RouteHandle}.
!
!     The arguments are:
!     \begin{description}
!     \item [routehandle] 
!           {\tt ESMF\_RouteHandle} associated with this redistribution.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP

      call ESMF_RouteHandleDestroy(routehandle, rc)

      end subroutine ESMF_FieldRedistRelease


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldRedistStore"
!BOP
! !IROUTINE: ESMF_FieldRedistStore - Data redistribution operation on a Field

! !INTERFACE:
      subroutine ESMF_FieldRedistStore(srcField, dstField, parentVM, &
                                       routeOptions, routehandle, rc)
!
!
! !ARGUMENTS:
      type(ESMF_Field), intent(in) :: srcField
      type(ESMF_Field), intent(inout) :: dstField
      type(ESMF_VM), intent(in) :: parentVM
      type(ESMF_RouteHandle), intent(out) :: routehandle
      type(ESMF_RouteOptions), intent(in), optional :: routeOptions
      integer, intent(out), optional :: rc               
!
! !DESCRIPTION:
!     Precompute the data movement or communications operations needed to
!     accomplish a data redistribution operation over the data
!     in an {\tt ESMF\_Field}.  Data redistribution differs from regridding
!     in that redistribution does no interpolation, only a 1-for-1 movement
!     of data from one location to another.
!     Therefore, while
!     the {\tt ESMF\_Grid}s for the source and destination may have
!     different decompositions (different {\tt ESMF\_DELayout}s)
!     or different data maps, the source and destination grids
!     must describe the same set of coordinates.

!     The arguments are:
!     \begin{description}
!     \item [srcField] 
!           {\tt ESMF\_Field} containing source data.
!     \item [dstField] 
!           {\tt ESMF\_Field} containing destination grid.
!     \item [parentVM]
!           {\tt ESMF\_VM} which encompasses both {\tt ESMF\_Field}s, 
!           most commonly the VM
!           of the Coupler if the redistribution is inter-component, 
!           but could also be the individual VM for a component if the 
!           redistribution is intra-component.  
!     \item [{[routeOptions]}]
!           Not normally specified.  Specify which internal strategy to select
!           when executing the communication needed to execute the
!           See Section~\ref{opt:routeopt} for possible values.
!     \item [routehandle] 
!           {\tt ESMF\_RouteHandle} which will be used to execute the
!           redistribution when {\tt ESMF\_FieldRedist} is called.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS: 

      integer :: localrc                          ! Error status
      integer :: srcHalo, dstHalo
      type(ESMF_FieldType), pointer :: dstFtypep, srcFtypep
   
      ! Initialize return code   
      localrc = ESMF_FAILURE
      if (present(rc)) rc = ESMF_FAILURE

      ! Validate the fields before proceeding.
      call ESMF_FieldValidate(srcField, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
      call ESMF_FieldValidate(dstField, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      ! The current version of the code only works correctly for the
      ! redistribution function if both the src and destination fields
      ! have identical halo widths.  Add a check here for that, which 
      ! can be removed if we augment the code to support halo mismatches.
      ! TODO: fix redist to not impose this restriction
      call ESMF_FieldGet(srcField, haloWidth=srcHalo, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
      call ESMF_FieldGet(dstField, haloWidth=dstHalo, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (srcHalo .ne. dstHalo) then
          call ESMF_LogMsgSetError(ESMF_RC_NOT_IMPL, &
             "Source and Destination Fields must have identical Halo Widths", &
                                   ESMF_CONTEXT, rc)
          return
      endif

      dstFtypep => dstField%ftypep
      srcFtypep => srcField%ftypep


      call ESMF_ArrayRedistStore(srcFtypep%localfield%localdata, &
                                 srcFtypep%grid, &
                                 srcFtypep%mapping, &
                                 dstFtypep%localfield%localdata, &
                                 dstFtypep%grid, &
                                 dstFtypep%mapping, &
                                 parentVM, &
                                 routeOptions, routehandle, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      ! Set return values.
      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldRedistStore


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldRedistStoreNew"
!BOP
! !IROUTINE: ESMF_FieldRedistStore - Data redistribution operation on a Field

! !INTERFACE:
      subroutine ESMF_FieldRedistStoreNew(srcField, decompIds, dstField, &
                                          parentVM, routeOptions, &
                                          routehandle, rc)
!
!
! !ARGUMENTS:
      type(ESMF_Field), intent(in) :: srcField
      integer, dimension(:), intent(in) :: decompIds
      type(ESMF_Field), intent(out) :: dstField
      type(ESMF_VM), intent(in) :: parentVM
      type(ESMF_RouteOptions), intent(in), optional :: routeOptions
      type(ESMF_RouteHandle), intent(inout) :: routehandle
      integer, intent(out), optional :: rc               
!
! !DESCRIPTION:
!     Precompute a redistribution operation over the data
!     in a {\tt ESMF\_Field}.  This routine reads the source field and leaves 
!     the data untouched.  This version of RedistStore creates the
!     destination {\tt ESMF\_Field} and its underlying {\tt ESMF\_Grid} and
!     {\tt ESMF\_FieldDataMap} from the source grid and input decompIds.
!     Unlike {\tt ESMF\_FieldRegrid} this routine does not do interpolation,
!     only data movement.
!
!     The arguments are:
!     \begin{description}
!     \item [srcField] 
!           {\tt ESMF\_Field} containing source data.
!     \item [decompIds] 
!           Array of decomposition identifiers.
!     \item [dstField] 
!           {\tt ESMF\_Field} containing destination grid.
!     \item [parentVM]
!           {\tt ESMF\_VM} which encompasses both {\tt ESMF\_Field}s, 
!           most commonly the VM
!           of the Coupler if the redistribution is inter-component, 
!           but could also be the individual VM for a component if the 
!           redistribution is intra-component.  
!     \item [{[routeOptions]}]
!           Not normally specified.  Specify which internal strategy to select
!           when executing the communication needed to execute the
!           See Section~\ref{opt:routeopt} for possible values.
!     \item [routehandle] 
!           {\tt ESMF\_RouteHandle} which will be used to execute the
!           redistribution when {\tt ESMF\_FieldRedist} is called.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS: 

      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present
      !type(ESMF_Grid) :: dstGrid
      type(ESMF_FieldType), pointer :: dstFtypep, srcFtypep
   
      ! Initialize return code   
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent = .TRUE. 
        rc = ESMF_FAILURE
      endif     

      ! Sanity checks for good source field, and that it has an associated grid
      ! and data before going down to the next level.
      if (.not.associated(srcField%ftypep)) then
         if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                "Uninitialized or already destroyed Field", &
                                 ESMF_CONTEXT, rc)) return
      endif

      srcFtypep => srcField%ftypep

      if (srcFtypep%fieldstatus.ne.ESMF_STATUS_READY) then
         if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                "Uninitialized or already destroyed Field", &
                                 ESMF_CONTEXT, rc)) return
      endif

      ! create the destination grid by copying the source grid and adding
      ! input decompids
      ! dstGrid = ESMF_GridCreateCopy(srcGrid, rc)
      ! TODO: finish grid routines to redistribute a grid or else query the
      !       source grid for all necessary information to create a new grid

      ! dstField = ESMF_FieldCreate ...  TODO: pass in any other needed arguments
      ! dstFtypep => dstField%ftypep

      call ESMF_ArrayRedistStore(srcFtypep%localfield%localdata, &
                                 srcFtypep%grid, &
                                 srcFtypep%mapping, &
                                 dstFtypep%localfield%localdata, &
                                 dstFtypep%grid, &
                                 dstFtypep%mapping, &
                                 parentVM, routeOptions, &
                                 routehandle, status)
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      ! Set return values.
      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldRedistStoreNew

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldReduce"
!BOPI
! !IROUTINE: ESMF_FieldReduce - Reduction operation on a Field

! !INTERFACE:
      subroutine ESMF_FieldReduce(field, rtype, result, blockingflag, &
                                  commhandle, rc)
!
!
! !ARGUMENTS:
      type(ESMF_Field) :: field                 
      integer :: rtype
      integer :: result
      type(ESMF_BlockingFlag), intent(in), optional :: blockingflag
      type(ESMF_CommHandle), intent(inout), optional :: commhandle
      integer, intent(out), optional :: rc               
!
! !DESCRIPTION:
!     Perform a reduction operation over the data in a {\tt ESMF\_Field}.
!
!     The arguments are:
!     \begin{description}
!     \item [field] 
!           {\tt ESMF\_Field} containing data to be reduced.
!     \item [rtype]
!           Type of reduction operation to perform.  Options include: ...
!           (Not yet implemented).
!     \item [result] 
!           Numeric result (may be single number, may be array)
!     \item [{[blockingflag]}]
!           Optional argument which specifies whether the operation should
!           wait until complete before returning or return as soon
!           as the communication between DEs has been scheduled.
!           If not present, default is to do synchronous communications.
!           Valid values for this flag are {\tt ESMF\_BLOCKING} and 
!           {\tt ESMF\_NONBLOCKING}.
!      (This feature is not yet supported.  All operations are synchronous.)
!     \item [{[commhandle]}]
!           If the {\tt blockingflag} is set to {\tt ESMF\_NONBLOCKING} this 
!           argument is required.  Information about the pending operation
!           will be stored in the {\tt ESMF\_CommHandle} and can be queried
!           or waited for later.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS: 

      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present
   
!     Initialize return code   
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent = .TRUE. 
        rc = ESMF_FAILURE
      endif     

!     Call Grid method to perform actual work
      !call ESMF_GridReduce(field%ftypep%grid, &
      !                     field%ftypep%localfield%localdata, &
      !                     rtype, result, status)
      !if (ESMF_LogMsgFoundError(status, &
      !                            ESMF_ERR_PASSTHRU, &
      !                            ESMF_CONTEXT, rc)) return

!     Set return values.
      !if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldReduce


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldRegrid"
!BOP
! !IROUTINE: ESMF_FieldRegrid - Data regrid operation on a Field

! !INTERFACE:
      subroutine ESMF_FieldRegrid(srcField, dstField, routehandle, &
                                  parentVM, regridmethod, regridnorm, &
                                  srcMask, dstMask, blockingflag, &
                                  commhandle, routeOptions, rc)
!
! !ARGUMENTS:
      type(ESMF_Field), intent(in) :: srcField                 
      type(ESMF_Field), intent(inout) :: dstField                 
      type(ESMF_RouteHandle), intent(inout) :: routehandle
      type(ESMF_VM), intent(in), optional :: parentVM
      type(ESMF_RegridMethod), intent(in), optional :: regridmethod
      type(ESMF_RegridNormOpt), intent(in), optional :: regridnorm
      type(ESMF_Mask), intent(in), optional :: srcMask                 
      type(ESMF_Mask), intent(in), optional :: dstMask                 
      type(ESMF_BlockingFlag), intent(in), optional :: blockingflag
      type(ESMF_CommHandle), intent(inout), optional :: commhandle
      type(ESMF_RouteOptions), intent(in), optional :: routeOptions
      integer, intent(out), optional :: rc               
!
! !DESCRIPTION:
!     Perform a regrid operation over the data
!     in an {\tt ESMF\_Field}.  This routine reads the source field and 
!     leaves the data untouched.  It uses the {\tt ESMF\_Grid} and
!     {\tt ESMF\_FieldDataMap} information in the destination field to
!     control the transformation of data.  The array data in the 
!     destination field is overwritten by this call.
!
!     The arguments are:
!     \begin{description}
!     \item [srcField] 
!           {\tt ESMF\_Field} containing source data.
!     \item [dstField] 
!           {\tt ESMF\_Field} containing destination grid and data map.
!     \item [routehandle]
!           {\tt ESMF\_RouteHandle} which will be returned after being
!           associated with the precomputed
!           information for a regrid operation on this {\tt ESMF\_Field}.
!           This handle must be supplied at run time to execute the regrid.
!     \item [{[parentVM]}]
!           {\tt ESMF\_VM} which encompasses both {\tt ESMF\_Field}s,
!           most commonly the VM of the Coupler if the regridding is
!           inter-component, but could also be the individual VM for
!           a component if the regridding is intra-component.  This argument
!           is used only if the routehandle has not been previously computed
!           during a RegridStore call.
!     \item [{[regridmethod]}]
!           Type of regridding to do.  A set of predefined methods are
!           supplied.  This argument is used only if the routehandle has
!           not been previously computed during a RegridStore call.
!     \item [{[regridnorm]}]
!           Normalization option, only for specific regrid types.
!           This argument is used only if the routehandle has not been
!           previously computed during a RegridStore call.
!     \item [{[srcMask]}]
!           Optional {\tt ESMF\_Mask} identifying valid source data.
!           (Not yet implemented.)
!     \item [{[dstMask]}]
!           Optional {\tt ESMF\_Mask} identifying valid destination data.
!           (Not yet implemented.)
!     \item [{[blockingflag]}]
!           Optional argument which specifies whether the operation should
!           wait until complete before returning or return as soon
!           as the communication between DEs has been scheduled.
!           If not present, default is to do synchronous communications.
!           Valid values for this flag are {\tt ESMF\_BLOCKING} and 
!           {\tt ESMF\_NONBLOCKING}.
!      (This feature is not yet supported.  All operations are synchronous.)
!     \item [{[commhandle]}]
!           If the {\tt blockingflag} is set to {\tt ESMF\_NONBLOCKING} this 
!           argument is required.  Information about the pending operation
!           will be stored in the {\tt ESMF\_CommHandle} and can be queried
!           or waited for later.
!     \item [{[routeOptions]}]
!           Not normally specified.  Specify which internal strategy to select
!           when executing the communication needed to execute the regrid.
!           See Section~\ref{opt:routeopt} for possible values.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS: 

      integer :: localrc                          ! Error status
      integer :: htype, srcCount, dstCount
      logical :: allInOne
      logical :: hasSrcData        ! does this DE contain localdata from src?
      logical :: hasDstData        ! does this DE contain localdata from dst?
      type(ESMF_Array) :: srcArray, dstArray
      type(ESMF_Grid) :: srcGrid, dstGrid
      type(ESMF_FieldDataMap) :: srcDatamap, dstDatamap
      type(ESMF_RelLoc) :: srcRelLoc, dstRelLoc
   
      ! Initialize return code   
      if (present(rc)) rc = ESMF_FAILURE

      ! Initialize other variables
      allInOne = .false.

      ! if the routehandle has not been precomputed, do so now
      ! first check if the RouteHandle is valid (constructed)
      call ESMF_RouteHandleValidate(routehandle, rc=localrc)

      ! if valid, check the routehandle type
      if (localrc.eq.ESMF_SUCCESS) then
        call ESMF_RouteHandleGet(routehandle, htype=htype, rc=localrc)
        if (htype .eq. ESMF_UNINITIALIZEDHANDLE) then
          allInOne = .true.
        elseif (htype .ne. ESMF_REGRIDHANDLE) then
          call ESMF_LogMsgSetError(ESMF_RC_ARG_VALUE, &
                                   "routehandle not defined for regrid", &
                                   ESMF_CONTEXT, rc)
          return
        endif
      ! if not valid, try calling RegridStore to initialize
      else
        allInOne = .true.
      endif

      ! if needed, call FieldRegridStore to set up the routehandle
      if (allInOne) then
        call ESMF_LogWrite("uninitialized routehandle: calling FieldRegridStore", &
                           ESMF_LOG_WARNING, &
                           ESMF_CONTEXT)
        call ESMF_FieldRegridStore(srcField, dstField, parentVM, &
                                   routehandle, regridmethod, regridnorm, &
                                   srcMask, dstMask, routeOptions, localrc)
      endif

      ! TODO: we need not only to know if this DE has data in the field,
      !   but also the de id for both src & dest fields

      ! This routine is called on every processor in the parent layout.
      !  It is quite possible that the source and destination fields do
      !  not completely cover every processor on that layout.  Make sure
      !  we do not go lower than this on the processors which are uninvolved
      !  in this communication.

      ! if srclayout ^ parentlayout == NULL, nothing to send from this DE id.
      call ESMF_FieldGet(srcField, datamap=srcDataMap, grid=srcGrid, rc=localrc)
      call ESMF_FieldDataMapGet(srcDataMap, horzRelloc=srcRelLoc, rc=localrc)
      call ESMF_GridGetDELocalInfo(srcGrid, horzRelLoc=srcRelLoc, &
                                   localCellCount=srcCount, rc=localrc)
      call ESMF_FieldGetArray(srcField, srcArray, rc=localrc)
 !jw   if (hasData.eq.ESMF_FALSE .OR. srcCount.le.0) then
      if (srcCount.le.0) then
        hasSrcData = .false.
      else
        hasSrcData = .true.
      endif

      ! if dstlayout ^ parentlayout == NULL, nothing to recv on this DE id.
      call ESMF_FieldGet(dstField, datamap=dstDataMap, grid=dstGrid, rc=localrc)
      call ESMF_FieldDataMapGet(dstDataMap, horzRelloc=dstRelLoc, rc=localrc)
      call ESMF_GridGetDELocalInfo(dstGrid, horzRelLoc=dstRelLoc, &
                                   localCellCount=dstCount, rc=localrc)
      call ESMF_FieldGetArray(dstField, dstArray, rc=localrc)
 !jw   if (hasData.eq.ESMF_FALSE .OR. dstCount.le.0) then
      if (dstCount.le.0) then
        hasDstData = .false.
      else
        hasDstData = .true.
      endif

      ! if neither are true this DE cannot be involved in the communication
      !  and it can just return now.
      if ((.not. hasSrcData) .and. (.not. hasDstData)) then
        if (present(rc)) rc = ESMF_SUCCESS
        return
      endif


      call ESMF_ArrayRegrid(srcArray, srcDatamap, hasSrcData, &
                            dstArray, dstDatamap, hasDstData, &
                            routehandle, srcMask, dstMask, &
                            blockingflag, commhandle, routeOptions, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      ! if this is a regrid call with an uninitialized routehandle, then 
      ! destroy it before returning    TODO:  is this right?
      if (allInOne) call ESMF_FieldRegridRelease(routehandle, localrc)

      ! Set return values.
      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldRegrid

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldRegridRelease"
!BOP
! !IROUTINE: ESMF_FieldRegridRelease - Release information for this handle

! !INTERFACE:
      subroutine ESMF_FieldRegridRelease(routehandle, rc)
!
!
! !ARGUMENTS:
      type(ESMF_RouteHandle), intent(inout) :: routehandle
      integer, intent(out), optional :: rc               
!
! !DESCRIPTION:
!     Release all stored information about the regridding associated
!     with this {\tt ESMF\_RouteHandle}.
!
!     The arguments are:
!     \begin{description}
!     \item [routehandle] 
!           {\tt ESMF\_RouteHandle} associated with this regrid operation.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS: 

      call ESMF_RouteHandleDestroy(routehandle, rc)

      end subroutine ESMF_FieldRegridRelease

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldRegridStore"
!BOP
! !IROUTINE: ESMF_FieldRegridStore - Data regrid operation on a Field

! !INTERFACE:
      subroutine ESMF_FieldRegridStore(srcField, dstField, parentVM, &
                                       routehandle, regridmethod, regridnorm, &
                                       srcMask, dstMask, routeOptions, rc)
!
!
! !ARGUMENTS:
      type(ESMF_Field), intent(in) :: srcField                 
      type(ESMF_Field), intent(inout) :: dstField                 
      type(ESMF_VM), intent(in) :: parentVM
      type(ESMF_RouteHandle), intent(inout) :: routehandle
      type(ESMF_RegridMethod), intent(in) :: regridmethod
      type(ESMF_RegridNormOpt), intent(in), optional :: regridnorm
      type(ESMF_Mask), intent(in), optional :: srcMask                 
      type(ESMF_Mask), intent(in), optional :: dstMask                 
      type(ESMF_RouteOptions), intent(in), optional :: routeOptions
      integer, intent(out), optional :: rc               
!
! !DESCRIPTION:
!     Precompute the data movement or communications operations plus the
!     interpolation information needed to execute
!     a regrid operation which will move and transform data
!     from the source field to the destination field.
!     This information is associated with the {\tt ESMF\_RouteHandle}
!     which must then be supplied during the actual execution of the
!     regrid operation.
!
!     The arguments are:
!     \begin{description}
!     \item [srcField] 
!           {\tt ESMF\_Field} containing source data.
!     \item [dstField] 
!           {\tt ESMF\_Field} containing destination grid and data map.
!     \item [parentVM]
!           {\tt ESMF\_VM} which encompasses both {\tt ESMF\_Field}s, 
!           most commonly the vm of the Coupler if the regridding is
!           inter-component, but could also be the individual vm for
!           a component if the regridding is intra-component.  
!     \item [routehandle]
!           Output from this call, identifies the precomputed work which
!           will be executed when {\tt ESMF\_FieldRegrid} is called.
!     \item [regridmethod]
!           Type of regridding to do.  A set of predefined methods are
!           supplied.
!     \item [{[regridnorm]}]
!           Normalization option, only for specific regrid types.
!     \item [{[srcMask]}]
!           Optional {\tt ESMF\_Mask} identifying valid source data.
!     \item [{[dstMask]}]
!           Optional {\tt ESMF\_Mask} identifying valid destination data.
!     \item [{[routeOptions]}]
!           Not normally specified.  Specify which internal strategy to select
!           when executing the communication needed to execute the regrid.
!           See Section~\ref{opt:routeopt} for possible values.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS: 

      integer :: localrc              ! Error status
      integer :: dstCount, srcCount
      logical :: hasSrcData           ! does this DE contain localdata from src?
      logical :: hasDstData           ! does this DE contain localdata from dst?
      type(ESMF_Array) :: srcArray, dstArray
      type(ESMF_FieldDataMap) :: srcDatamap, dstDatamap
      type(ESMF_Grid) :: srcGrid, dstGrid
      type(ESMF_RelLoc) :: srcRelLoc, dstRelLoc
   
      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_FAILURE

      ! This routine is called on every processor in the parent layout.
      !  It is quite possible that the source and destination fields do
      !  not completely cover every processor on that layout.  Make sure
      !  we do not go lower than this on the processors which are uninvolved
      !  in this communication.

      ! if srclayout ^ parentlayout == NULL or if local count = 0, then
      ! nothing to send from this DE id.
      call ESMF_FieldGet(srcField, datamap=srcDataMap, grid=srcGrid, rc=localrc)
      call ESMF_FieldGetArray(srcField, srcArray, rc=localrc)
      call ESMF_FieldDataMapGet(srcDataMap, horzRelloc=srcRelLoc, rc=localrc)
      call ESMF_GridGetDELocalInfo(srcGrid, horzRelLoc=srcRelLoc, &
                                   localCellCount=srcCount, rc=localrc)
      if (srcCount.le.0) then
        hasSrcData = .false.
      else
        hasSrcData = .true.
      endif

      ! if dstlayout ^ parentlayout == NULL, nothing to recv on this DE id.
      call ESMF_FieldGet(dstField, datamap=dstDataMap, grid=dstGrid, rc=localrc)
      call ESMF_FieldGetArray(dstField, dstArray, rc=localrc)
      call ESMF_FieldDataMapGet(dstDataMap, horzRelloc=dstRelLoc, rc=localrc)
      call ESMF_GridGetDELocalInfo(dstGrid, horzRelLoc=dstRelLoc, &
                                   localCellCount=dstCount, rc=localrc)
      if (dstCount.le.0) then
        hasDstData = .false.
      else
        hasDstData = .true.
      endif

      ! if neither are true this DE cannot be involved in the communication
      !  and it can just return now.
      if ((.not. hasSrcData) .AND. (.not. hasDstData)) then
        if (present(rc)) rc = ESMF_SUCCESS
        return
      endif

      call ESMF_ArrayRegridStore(srcArray, srcGrid, srcDatamap, hasSrcData, & 
                                 dstArray, dstGrid, dstDatamap, hasDstData, &
                                 parentVM, routehandle, &
                                 regridmethod, regridnorm, &    
                                 srcMask, dstMask, routeOptions, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      ! Set return values.
      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldRegridStore

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldScatter"
!BOPI
! !IROUTINE: ESMF_FieldScatter - Data scatter operation on a Field

! !INTERFACE:
      subroutine ESMF_FieldScatter(array, srcDe, field, &
                                   blockingflag, commhandle, rc)
!
!
! !ARGUMENTS:
      type(ESMF_Array), intent(inout) :: array
      integer, intent(in) :: srcDe
      type(ESMF_Field), intent(inout) :: field                 
      type(ESMF_BlockingFlag), intent(in), optional :: blockingflag
      type(ESMF_CommHandle), intent(inout), optional :: commhandle
      integer, intent(out), optional :: rc               
!
! !DESCRIPTION:
!     Perform a scatter operation over the data in an {\tt ESMF\_Array}, 
!     returning it as the data array in an {\tt ESMF\_Field}.  
!     If the Field is decomposed over N DEs, this routine
!     takes a single array on the specified DE and returns 
!     a decomposed copy on each of the N DEs, as the 
!     {\tt ESMF\_Array} associated with the given empty {\tt ESMF\_Field}.
!
!     The arguments are:
!     \begin{description}
!     \item [array] 
!      Input {\tt ESMF\_Array} containing the collected data.
!      It must be the size of the entire undecomposed grid.
!     \item [srcDe]
!      Integer DE number where the data to be Scattered is located.
!      The {\tt ESMF\_Array} input is ignored on all other DEs.
!     \item [field] 
!      Empty Field containing {\tt ESMF\_Grid} which will correspond to the 
!      data in the array which will be scattered.  When this routine returns
!      each {\tt ESMF\_Field} will contain a valid data array containing the 
!      subset of the decomposed data.
!     \item [{[blockingflag]}]
!      Optional argument which specifies whether the operation should
!      wait until complete before returning or return as soon
!      as the communication between DEs has been scheduled.
!      If not present, default is to do synchronous communications.
!      Valid values for this flag are {\tt ESMF\_BLOCKING} and 
!      {\tt ESMF\_NONBLOCKING}.
!      (This feature is not yet supported.  All operations are synchronous.)
!     \item [{[commhandle]}]
!      If the {\tt blockingflag} is set to {\tt ESMF\_NONBLOCKING} this 
!      argument is required.  Information about the pending operation
!      will be stored in the {\tt ESMF\_CommHandle} and can be queried
!      or waited for later.
!     \item [{[rc]}] 
!      Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS: 

      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present
      type(ESMF_FieldType), pointer :: ftypep     ! field type info
      type(ESMF_DELayout) :: delayout             ! layout
      type(ESMF_Array) :: dstarray                ! Destination array
      integer :: i, datarank, numDims
      integer :: dimorder(ESMF_MAXDIM)   
      integer :: dimlengths(ESMF_MAXDIM)   
      integer :: decomps(ESMF_MAXGRIDDIM), decompids(ESMF_MAXDIM)
   
      ! Initialize return code   
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent = .TRUE. 
        rc = ESMF_FAILURE
      endif     

      ftypep => field%ftypep

      ! Query the datamap and set info for grid so it knows how to
      !  match up the array indices and the grid indices.
      call ESMF_FieldDataMapGet(ftypep%mapping, dataIndexList=dimorder, rc=status)
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      call ESMF_GridGet(ftypep%grid, dimCount=numDims, rc=status) 

      !call ESMF_GridGet(ftypep%grid, decomps, rc=status)   !TODO
      !if (ESMF_LogMsgFoundError(status, &
      !                            ESMF_ERR_PASSTHRU, &
      !                            ESMF_CONTEXT, rc)) return
      decomps(1) = 1    ! TODO: remove this once the grid call is created
      decomps(2) = 2

      ! And get the Array sizes
      call ESMF_ArrayGet(ftypep%localfield%localdata, rank=datarank, &
                         counts=dimlengths, rc=status)
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      do i=1, datarank
        decompids(i) = dimorder(i)
        if(dimorder(i).ne.0) decompids(i) = decomps(dimorder(i))
      enddo

      ! Call Array method to perform actual work
      call ESMF_GridGet(ftypep%grid, delayout=delayout, rc=status)
      call ESMF_ArrayScatter(array, delayout, decompids, srcDe, dstarray, &
                             status)
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      ! TODO: do we need to set dimorder here?  should datamap be an input
      !  to this routine, or specified at create time?   or should this be
      !  a field create method?
      ftypep%localfield%localdata = dstarray

      ! Set return values.
      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldScatter


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldHaloDeprecated"
!BOPI
! !IROUTINE: ESMF_FieldHaloDeprecated - Data halo operation on a Field

! !INTERFACE:
      subroutine ESMF_FieldHaloDeprecated(field, blocking, commhandle, rc)
!
!
! !ARGUMENTS:
      type(ESMF_Field), intent(inout) :: field                 
      type(ESMF_BlockingFlag), intent(in), optional :: blocking
      type(ESMF_CommHandle), intent(inout), optional :: commhandle
      integer, intent(out), optional :: rc               
!
! !DESCRIPTION:
!  {\tt DEPRECATED!} - these args are for the previous version of Halo
!   which did internal caching.  The current version of the software 
!    has a Precompute/Store call, and FieldHalo takes this
!    handle and does the execution of a precomputed route.  This routine
!    remains only until the new interfaces are working.
!
!     Perform a {\tt Halo} operation over the data
!     in an {\tt ESMF\_Field}.  This routine updates the data 
!     inside the {\tt ESMF\_Field} in place.
!
!     The arguments are:
!     \begin{description}
!     \item [field] 
!           {\tt ESMF\_Field} containing data to be halo'd.
!     \item [{[blocking]}]
!           Optional argument which specifies whether the operation should
!           wait until complete before returning or return as soon
!           as the communication between DEs has been scheduled.
!           If not present, default is to do synchronous communications.
!           Valid values for this flag are {\tt ESMF\_BLOCKING} and 
!           {\tt ESMF\_NONBLOCKING}.
!      (This feature is not yet supported.  All operations are synchronous.)
!     \item [{[commhandle]}]
!           If the blocking flag is set to {\tt ESMF\_NONBLOCKING} this 
!           argument is required.  Information about the pending operation
!           will be stored in the {\tt ESMF\_CommHandle} and can be queried
!           or waited for later.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS: 

      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present
      type(ESMF_FieldType), pointer :: ftypep     ! field type info
      type(ESMF_DELayout) :: delayout
      type(ESMF_VM) :: vm
      integer :: datarank, numDims
      integer :: dimorder(ESMF_MAXDIM)   
      integer :: dimlengths(ESMF_MAXDIM)   
      type(ESMF_RelLoc) :: horzRelLoc, vertRelLoc
      type(ESMF_Route) :: route
      type(ESMF_LocalArray) :: local_array
      integer :: nDEs
      integer :: my_DE
      integer, dimension(:), allocatable :: global_count
      integer, dimension(:,:), allocatable :: globalStartPerDEPerDim
      type(ESMF_AxisIndex), dimension(:,:), pointer :: src_AI, dst_AI
      type(ESMF_AxisIndex), dimension(:,:), pointer :: gl_src_AI, gl_dst_AI
      type(ESMF_Logical), dimension(:), allocatable :: periodic
      integer :: AI_count

   
      ! Initialize return code   
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent = .TRUE. 
        rc = ESMF_FAILURE
      endif     

      ! Get the Layout from the Field's Grid
      ftypep => field%ftypep
      call ESMF_GridGet(ftypep%grid, delayout=delayout, rc=status)
      
      ! Get the associated VM
      call ESMF_DELayoutGetVM(delayout, vm, rc=status)

      ! Our DE number in the layout
      call ESMF_DELayoutGet(delayout, localDE=my_DE, rc=status)

      ! Query the datamap and set info for grid so it knows how to
      !  match up the array indices and the grid indices.
      call ESMF_FieldDataMapGet(ftypep%mapping, &
                           horzRelLoc=horzRelLoc, vertRelLoc=vertRelLoc, &
                           dataIndexList=dimorder, rc=status)
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      ! And get the Array sizes
      call ESMF_ArrayGet(ftypep%localfield%localdata, rank=datarank, &
                         counts=dimlengths, rc=status)
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      ! Get global starting counts and global counts
      call ESMF_DELayoutGet(delayout, deCount=nDEs, rc=status)
      call ESMF_GridGet(ftypep%grid, dimCount=numDims, rc=status)
      AI_count = nDEs
      allocate(global_count(numDims), stat=status)
      allocate(periodic(numDims), stat=status)
      allocate(globalStartPerDEPerDim(nDEs, numDims), stat=status)
      allocate(src_AI(nDEs, numDims), stat=status)
      allocate(dst_AI(nDEs, numDims), stat=status)
      allocate(gl_src_AI(nDEs, numDims), stat=status)
      allocate(gl_dst_AI(nDEs, numDims), stat=status)

      call ESMF_GridGet(ftypep%grid, &
                        horzRelLoc=horzRelLoc, vertRelLoc=vertRelLoc, &
                        globalCellCountPerDim=global_count, &
                        globalStartPerDEPerDim=globalStartPerDEPerDim, &
                        periodic=periodic, rc=status)
      if (ESMF_LogMsgFoundError(status, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      ! set up things we need to find a cached route or precompute one
      call ESMF_ArrayGetAllAxisIndices(ftypep%localfield%localdata, ftypep%grid, &
                                       ftypep%mapping, totalindex=dst_AI, &
                                       compindex=src_AI, rc=status)       

      ! translate AI's into global numbering
      call ESMF_GridDELocalToGlobalAI(ftypep%grid, horzRelLoc=horzRelLoc, &
                                      vertRelLoc=vertRelLoc, &
                                      localAI2D=dst_AI, &
                                      globalAI2D=gl_dst_AI, rc=status)
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return
      call ESMF_GridDELocalToGlobalAI(ftypep%grid, horzRelLoc=horzRelLoc, &
                                      vertRelLoc=vertRelLoc, &
                                      localAI2D=src_AI, &
                                      globalAI2D=gl_src_AI, rc=status)
      if (ESMF_LogMsgFoundError(status, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
          
      ! Create the route object.
      route = ESMF_RouteCreate(vm, rc) 

      call ESMF_RoutePrecomputeHalo(route, datarank, my_DE, gl_src_AI, &
                                    gl_dst_AI, AI_count, &
                                    globalStartPerDEPerDim, &
                                    global_count, delayout, periodic, status)

      ! Once table is full, execute the communications it represents.

      local_array = ftypep%localfield%localdata
      call ESMF_RouteRun(route, local_array, local_array, rc=status)
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      ! delete the route
      call ESMF_RouteDestroy(route, rc)

      ! get rid of temporary arrays
      if (allocated(globalStartPerDEPerDim)) &
         deallocate(globalStartPerDEPerDim, stat=status)
      if (allocated(global_count)) deallocate(global_count, stat=status)
      if (allocated(periodic))     deallocate(periodic, stat=status)
      if (associated(src_AI))      deallocate(src_AI, stat=status)
      if (associated(dst_AI))      deallocate(dst_AI, stat=status)
      if (associated(gl_src_AI))   deallocate(gl_src_AI, stat=status)
      if (associated(gl_dst_AI))   deallocate(gl_dst_AI, stat=status)

      ! Set return values.
      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldHaloDeprecated



!------------------------------------------------------------------------------
!------------------------------------------------------------------------------

      end module ESMF_FieldCommMod
