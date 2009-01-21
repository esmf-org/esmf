! $Id: ESMF_Route.F90,v 1.97.2.3 2009/01/21 21:25:23 cdeluca Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2009, University Corporation for Atmospheric Research, 
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
! Laboratory, University of Michigan, National Centers for Environmental 
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
#define ESMF_FILENAME "ESMF_Route.F90"
!
!     ESMF Route Module
      module ESMF_RouteMod
!
!==============================================================================
!
! This file contains the F90 wrapper code for the C++ implementation of
!  the Route class.
!
!------------------------------------------------------------------------------
! INCLUDES
#include "ESMF.h"
!==============================================================================
!BOPI
! !MODULE: ESMF_RouteMod - Store and execute a predefined communication pattern
!
! !DESCRIPTION:
!
! The code in this file implements the F90 wrapper code for the C++
!  implementation of the {\tt ESMC\_Route} class.
!
!------------------------------------------------------------------------------
! !USES:
      use ESMF_UtilTypesMod
      use ESMF_InitMacrosMod
      use ESMF_LogErrMod
      use ESMF_BaseMod       
      use ESMF_VMMod
      use ESMF_DELayoutMod  
      use ESMF_LocalArrayMod
      use ESMF_XPacketMod  
      implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
      private

!------------------------------------------------------------------------------
!     !  ESMF_Route
!
!     ! Description of ESMF_Route. 

      type ESMF_Route
      sequence
      private
        type(ESMF_Pointer) :: this    ! opaque pointer to C++ class data
	ESMF_INIT_DECLARE
      end type

      type ESMF_RouteOptions 
      sequence
      private
        integer :: option
      end type

      ! must match C++ code, must increase by powers of 2
      type(ESMF_RouteOptions), parameter :: ESMF_ROUTE_OPTION_ASYNC = &
                                              ESMF_RouteOptions(1)
      type(ESMF_RouteOptions), parameter :: ESMF_ROUTE_OPTION_SYNC = &
                                              ESMF_RouteOptions(2)
      type(ESMF_RouteOptions), parameter :: ESMF_ROUTE_OPTION_PACK_PET = &
                                              ESMF_RouteOptions(4)
      type(ESMF_RouteOptions), parameter :: ESMF_ROUTE_OPTION_PACK_XP = &
                                              ESMF_RouteOptions(8)
      type(ESMF_RouteOptions), parameter :: ESMF_ROUTE_OPTION_PACK_NOPACK = &
                                              ESMF_RouteOptions(16)
      type(ESMF_RouteOptions), parameter :: ESMF_ROUTE_OPTION_PACK_VECTOR = &
                                              ESMF_RouteOptions(32)
      type(ESMF_RouteOptions), parameter :: ESMF_ROUTE_OPTION_PACK_BUFFER = &
                                              ESMF_RouteOptions(64)
      type(ESMF_RouteOptions), parameter :: ESMF_ROUTE_OPTION_DEFAULT = &
                                              ESMF_RouteOptions(64+4+1)
					      
      
!------------------------------------------------------------------------------
! !PUBLIC TYPES:
      public ESMF_Route, ESMF_RouteOptions
      public ESMF_ROUTE_OPTION_ASYNC,       ESMF_ROUTE_OPTION_SYNC, &
             ESMF_ROUTE_OPTION_PACK_PET,    ESMF_ROUTE_OPTION_PACK_XP, &
             ESMF_ROUTE_OPTION_PACK_NOPACK, ESMF_ROUTE_OPTION_PACK_VECTOR, &
             ESMF_ROUTE_OPTION_PACK_BUFFER, ESMF_ROUTE_OPTION_DEFAULT
!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:
!
      public ESMF_RouteGetInit
      public ESMF_RouteSetInitCreated
      public ESMF_RouteCreate                 ! interface only, deep class
      public ESMF_RouteDestroy                ! interface only, deep class

      !public ESMF_RouteGet                    ! get and set values
      public ESMF_RouteSet 
      public ESMF_RouteSetSend
      public ESMF_RouteSetRecv
      public ESMF_RouteSetRecvItems

      public ESMF_RouteGetRecvItems

      public ESMF_RoutePrecomputeHalo
      public ESMF_RoutePrecomputeRedist
      public ESMF_RoutePrecomputeRedistV
      public ESMF_RoutePrecomputeRedistA2A
      public ESMF_RoutePrecomputeRegrid
      public ESMF_RoutePrecomputeDomList
      public ESMF_RouteRun
      public ESMF_RouteRunList
      public ESMF_RouteRunF90PtrI411D
      public ESMF_RouteRunF90PtrI421D
      public ESMF_RouteRunF90PtrR811D
      public ESMF_RouteRunF90PtrR821D
      public ESMF_RouteRunF90PtrR832D
 
      public ESMF_RouteValidate
      public ESMF_RoutePrint
 
! Overloaded = operator function
      public operator(+), assignment(=)


!
!EOPI

!------------------------------------------------------------------------------

interface assignment(=)
  module procedure iras
  module procedure rias
end interface

interface operator(+)
  module procedure radd
end interface

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_Route.F90,v 1.97.2.3 2009/01/21 21:25:23 cdeluca Exp $'

!==============================================================================
!
      contains



!==============================================================================
!
! Route Initialiation and Validation functions
!
!------------------------------------------------------------------------------

! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_RouteSetInitCreated()"
!BOPI
! !IROUTINE: ESMF_RouteSetInitCreated - Set Route init code to "CREATED"

! !INTERFACE:
  subroutine ESMF_RouteSetInitCreated(r, rc)
!
! !ARGUMENTS:
    type(ESMF_Route), intent(inout)           :: r
    integer,          intent(out),  optional  :: rc  
!         
!
! !DESCRIPTION:
!      Set init code in Route object to "CREATED".
!
!     The arguments are:
!     \begin{description}
!     \item[r] 
!          Specified {\tt ESMF\_Route} object.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
!------------------------------------------------------------------------------

    ! Assume failure until success
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    
    ! Set init code
    ESMF_INIT_SET_CREATED(r)

    ! Return success
    if (present(rc)) rc = ESMF_SUCCESS
    
  end subroutine ESMF_RouteSetInitCreated
!------------------------------------------------------------------------------

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_RouteGetInit"
!BOPI
! !IROUTINE: ESMF_RouteGetInit - Get the Init status 

! !INTERFACE:
      function ESMF_RouteGetInit(d)
!
! !RETURN VALUE:
      ESMF_INIT_TYPE :: ESMF_RouteGetInit
!
! !ARGUMENTS:
      type(ESMF_Route), intent(in),optional :: d
!
! !DESCRIPTION:
!     Get the init status
!
!     The arguments are:
!     \begin{description}
!     \item[d] 
!          The class to be queried 
!     \end{description}
!
!EOPI
 
  if (present(d)) then
     ESMF_RouteGetInit=ESMF_INIT_GET(d)
  else
     ESMF_RouteGetInit=ESMF_INIT_CREATED
  endif 
end function ESMF_RouteGetInit

!==============================================================================
!
! Overloaded operators
!
!------------------------------------------------------------------------------
function radd(first, second)
  type(ESMF_RouteOptions) :: radd
  type(ESMF_RouteOptions), intent(in) :: first, second

  radd%option = first%option + second%option

end function radd

subroutine iras(first, second)
  type(ESMF_RouteOptions), intent(out) :: first
  integer, intent(in) :: second

  first%option = second

end subroutine iras

subroutine rias(first, second)
  integer, intent(out) :: first
  type(ESMF_RouteOptions), intent(in) :: second

  first = second%option

end subroutine rias

!==============================================================================
!
! This section includes the Route Create and Destroy methods.
!
!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_RouteCreate"
!BOPI
! !IROUTINE: ESMF_RouteCreate - Create a new Route

! !INTERFACE:
      function ESMF_RouteCreate(vm, rc)
!
! !RETURN VALUE:
      type(ESMF_Route) :: ESMF_RouteCreate
!
! !ARGUMENTS:
      type(ESMF_VM), intent(in) :: vm
      integer, intent(out), optional :: rc               
!
! !DESCRIPTION:
!     Allocates memory for a new {\tt ESMF\_Route} and constructs its
!     internals.
!
!     The arguments are:
!     \begin{description}
!     \item[vm] 
!          An {\tt ESMF\_VM} object which encompasses all virtual address
!          spaces accessed by the route operation.  
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI

        ! local variables
        type (ESMF_Route) :: route         ! new C++ Route
        integer :: status                  ! local error status

        ! Set initial values
        status = ESMF_RC_NOT_IMPL
        route%this = ESMF_NULL_POINTER

        ! Initialize return code; assume failure until success is certain
        if (present(rc)) rc = ESMF_RC_NOT_IMPL

        ! check input variables
        ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit,vm,rc)

        ! Call C++ create code
        call c_ESMC_RouteCreate(route, vm, status)
        if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

        ! Set return values
        ESMF_RouteCreate = route

        if (present(rc)) rc = ESMF_SUCCESS

	ESMF_INIT_SET_CREATED(ESMF_RouteCreate)

        end function ESMF_RouteCreate

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_RouteDestroy"
!BOPI
! !IROUTINE: ESMF_RouteDestroy - Free all resources associated with a Route 

! !INTERFACE:
      subroutine ESMF_RouteDestroy(route, rc)
!
! !ARGUMENTS:
      type(ESMF_Route), intent(inout) :: route   
      integer, intent(out), optional :: rc        
!
! !DESCRIPTION:
!     Destroys an {\tt ESMF\_Route} previously allocated
!     via an {\tt ESMF_RouteCreate()} routine.
!
!     The arguments are:
!     \begin{description}
!     \item[route] 
!          The {\tt ESMF\_Route} to be destroyed.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI

        ! local variables
        integer :: status                  ! local error status

        ! Initialize return code; assume failure until success is certain
        if (present(rc)) rc = ESMF_RC_NOT_IMPL

        ! check input variables
        ESMF_INIT_CHECK_DEEP(ESMF_RouteGetInit,route,rc)

        ! Call C++ destroy code
        call c_ESMC_RouteDestroy(route, status)
        if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

        ! nullify pointer
        route%this = ESMF_NULL_POINTER

	ESMF_INIT_SET_DELETED(route)

        if (present(rc)) rc = ESMF_SUCCESS
        end subroutine ESMF_RouteDestroy


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_RouteGet"
!BOPI
! !IROUTINE: ESMF_RouteGet - Get values from a Route

! !INTERFACE:
      subroutine ESMF_RouteGet(route, value1, value2, rc)
!
! !ARGUMENTS:
      type(ESMF_Route), intent(inout) :: route
      integer, intent(out), optional :: value1
      integer, intent(out), optional :: value2
      integer, intent(out), optional :: rc             

!
! !DESCRIPTION:
!     Queries information from an {\tt ESMF\_Route}.
!
!     The arguments are:
!     \begin{description}
!     \item[route] 
!          {\tt ESMF\_Route} to be queried.
!     \item[{[value1]}]
!          Value to be retrieved.         
!     \item[{[value2]}]
!          Value to be retrieved.         
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI

        ! local variables
        integer :: status                  ! local error status

        ! Initialize return code; assume failure until success is certain
        if (present(rc)) rc = ESMF_RC_NOT_IMPL
        status = ESMF_RC_NOT_IMPL

        if (present(value1)) then
          ! code to be added here
        endif

        if (present(value2)) then
          ! code to be added here
        endif

        ESMF_INIT_CHECK_DEEP(ESMF_RouteGetInit,route,rc)

        ! Call C++  code
	! This function is not implemented yet -- P.Li 11/27/06
!       call c_ESMC_RouteGet(route, value1, value2, status)
        if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

        if (present(rc)) rc = ESMF_SUCCESS
        end subroutine ESMF_RouteGet


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_RouteGetRecvItems"
!BOPI
! !IROUTINE: ESMF_RouteGetRecvItems - Get size of receive buffer

! !INTERFACE:
      subroutine ESMF_RouteGetRecvItems(route, nitems, rc)
!
! !ARGUMENTS:
      type(ESMF_Route), intent(inout) :: route
      integer, intent(out) :: nitems
      integer, intent(out), optional :: rc            

!
! !DESCRIPTION:
!     Get the required size of the receive buffer for an {\tt ESMF\_Route} in
!     number of items (not in bytes).  In general a receive buffer is
!     already allocated, but if not, the caller can first query the route
!     for the size of the receive buffer, allocate it, and then call
!     {\tt RouteRun} to move the data.
!
!     The arguments are:
!     \begin{description}
!     \item[route] 
!          {\tt ESMF\_Route} to be modified.
!     \item[nitems]
!          Size of the receive buffer for this route, in number of items.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI

        ! local variables
        integer :: status                  ! local error status
        logical :: rcpresent               ! did user specify rc?

        ! Set initial values
        status = ESMF_RC_NOT_IMPL
        rcpresent = .FALSE.   

        ! Initialize return code; assume failure until success is certain
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_RC_NOT_IMPL
        endif

        ESMF_INIT_CHECK_DEEP(ESMF_RouteGetInit,route,rc)

        ! Call C++  code
        call c_ESMC_RouteGetRecvItems(route, nitems, status)
        if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_RouteGetRecvItems

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_RoutePrecomputeDomList"
!BOPI
! !IROUTINE: ESMF_RoutePrecomputeDomList - Precompute communication paths

! !INTERFACE:
      subroutine ESMF_RoutePrecomputeDomList(route, rank, &
                                             srcDELayout, dstDELayout, &
                                             srcDomainList, dstDomainList, &
                                             hasSrcData, hasDstData, rc)

! !ARGUMENTS:
      type(ESMF_Route),      intent(inout) :: route
      integer,               intent(in   ) :: rank
      type(ESMF_DELayout),   intent(inout) :: srcDELayout
      type(ESMF_DELayout),   intent(inout) :: dstDELayout
      type(ESMF_DomainList), intent(inout) :: srcDomainList
      type(ESMF_DomainList), intent(inout) :: dstDomainList
      logical,               intent(in   ), optional :: hasSrcData
      logical,               intent(in   ), optional :: hasDstData
      integer,               intent(  out), optional :: rc

!
! !DESCRIPTION:
!     Precompute the data movement needed internally by the regrid operation
!     to move the weight information for data interpolation, and store this
!     information in an {\tt ESMF\_Route}.
!
!     The arguments are:
!     \begin{description}
!     \item[route] 
!          {\tt ESMF\_Route} to associate this information with.
!     \item[rank]
!          Data rank.
!     \item[srcDELayout]
!          The {\tt ESMF\_DELayout} of the source igrid.
!     \item[dstDELayout]
!          The {\tt ESMF\_DELayout} of the destination igrid.
!     \item[srcDomainList]
!          An {\tt ESMF\_DomainList} which contains a list of rectangular
!          regions of a larger rectangular block of memory, to be sent 
!          during the execution of this route.
!     \item[dstDomainList]
!          An {\tt ESMF\_DomainList} which contains a list of rectangular
!          regions of a larger rectangular block of memory, to be received 
!          during the execution of this route.
!     \item[{[hasSrcData]}]
!          Logical which should be false if this DE contains no source data.  
!          The default is to assume the current DE contains source data.
!     \item[{[hasDstData]}]
!          Logical which should be false if this DE contains no destination data.  
!          The default is to assume the current DE contains destination data.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI

      ! local variables
      integer :: localrc                 ! local error status
      integer :: i, j                    ! counters
      logical :: hasDstDataUse, hasSrcDataUse
      type(ESMF_Logical) :: hasDstDataX, hasSrcDataX

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ESMF_INIT_CHECK_DEEP(ESMF_RouteGetInit,route,rc)
      ESMF_INIT_CHECK_DEEP(ESMF_DELayoutGetInit,srcDELayout,rc)
      ESMF_INIT_CHECK_DEEP(ESMF_DELayoutGetInit,dstDELayout,rc)

      hasDstDataUse = .true.
      hasDstDataX   = ESMF_TRUE
      if (present(hasDstData)) then
        hasDstDataUse = hasDstData
        if (.not.(hasDstData)) hasDstDataX = ESMF_FALSE
      endif
      hasSrcDataUse = .true.
      hasSrcDataX   = ESMF_TRUE
      if (present(hasSrcData)) then
        hasSrcDataUse = hasSrcData
        if (.not.(hasSrcData)) hasSrcDataX = ESMF_FALSE
      endif

      ! Translate AxisIndices from F90 to C++
      if (hasDstDataUse) then
        do i   = 1, dstDomainList%num_domains
          do j = 1, dstDomainList%domains(i)%rank
            dstDomainList%domains(i)%ai(j)%min = &
                           dstDomainList%domains(i)%ai(j)%min - 1
            dstDomainList%domains(i)%ai(j)%max = &
                           dstDomainList%domains(i)%ai(j)%max - 1
          enddo
        enddo
      endif

      if (hasSrcDataUse) then
        do i   = 1, srcDomainList%num_domains
          do j = 1, srcDomainList%domains(i)%rank
            srcDomainList%domains(i)%ai(j)%min = &
                           srcDomainList%domains(i)%ai(j)%min - 1
            srcDomainList%domains(i)%ai(j)%max = &
                           srcDomainList%domains(i)%ai(j)%max - 1
          enddo
        enddo
      endif

      ! Call C++  code
      call c_ESMC_RoutePrecomputeDomList(route, rank, &
                                         srcDELayout, dstDELayout, &
                                         srcDomainList, dstDomainList, &
                                         hasSrcDataX, hasDstDataX, localrc)
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
                    ESMF_CONTEXT, rcToReturn=rc)) then
        print *, "Route PrecomputeDomainList error"
        print *, "return after adding 1 back to AIs"
      endif

      ! Translate AxisIndices back to F90 from C++
      if (hasDstDataUse) then
        do i   = 1, dstDomainList%num_domains
          do j = 1, dstDomainList%domains(i)%rank
            dstDomainList%domains(i)%ai(j)%min = &
                           dstDomainList%domains(i)%ai(j)%min + 1
            dstDomainList%domains(i)%ai(j)%max = &
                           dstDomainList%domains(i)%ai(j)%max + 1
          enddo
        enddo
      endif

      if (hasSrcDataUse) then
        do i   = 1, srcDomainList%num_domains
          do j = 1, srcDomainList%domains(i)%rank
            srcDomainList%domains(i)%ai(j)%min = &
                           srcDomainList%domains(i)%ai(j)%min + 1
            srcDomainList%domains(i)%ai(j)%max = &
                           srcDomainList%domains(i)%ai(j)%max + 1
          enddo
        enddo
      endif

      if (present(rc)) rc = localrc

      end subroutine ESMF_RoutePrecomputeDomList

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_RoutePrecomputeHalo"
!BOPI
! !IROUTINE: ESMF_RoutePrecomputeHalo - Precompute communication paths for a halo

! !INTERFACE:
      subroutine ESMF_RoutePrecomputeHalo(route, rank, my_DE, AI_exc, AI_tot, &
                                          AI_count, global_start, global_count, &
                                          delayout, periodic, rc)

! !ARGUMENTS:
      type(ESMF_Route), intent(inout) :: route
      integer, intent(in) :: rank
      integer, intent(in) :: my_DE
      type(ESMF_AxisIndex), dimension(:,:), pointer :: AI_exc
      type(ESMF_AxisIndex), dimension(:,:), pointer :: AI_tot
      integer, intent(in) :: AI_count
      integer, dimension(:,:), intent(in) :: global_start
      integer, dimension(ESMF_MAXIGRIDDIM), intent(in) :: global_count
      type(ESMF_DELayout), intent(inout) :: delayout
      type(ESMF_Logical), intent(in) :: periodic(:)
      integer, intent(out), optional :: rc

!
! !DESCRIPTION:
!     Precompute the data movement needed to execute a halo
!     operation, and store this information in an {\tt ESMF\_Route}.
!
!     The arguments are:
!     \begin{description}
!     \item[route] 
!          {\tt ESMF\_Route} to associate this information with.
!     \item[rank]
!          Data rank.
!     \item[my_DE]
!          The ID of the local DE.
!     \item[AI_exc]
!          A 2D array of {\tt ESMF\_AxisIndex}s, describing the
!          exclusive region of the data for each DE.
!          (Note: is this actually the computational region?)
!     \item[AI_tot]
!          A 2D array of {\tt ESMF\_AxisIndex}s, describing the
!          total region of the data for each DE.
!     \item[AI_count]
!          Integer number of DEs involved.
!     \item[global_start]
!          Array of integer offsets for the corner of each DE relative
!          to the undecomposed object.
!     \item[global_count]
!          Array of integers, one per dimension, for the total count
!          of items for the undecomposed object.
!     \item[DELayout]
!          {\tt ESMF\_DELayout} for this data decomposition.
!     \item[periodic]
!          An array of {\tt ESMF\_Logical} flags indicating whether the 
!          boundaries in each dimension are to be treated as periodic or not.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI

        ! local variables
        integer :: status                  ! local error status
        integer :: i,j                     ! counters
        logical :: rcpresent               ! did user specify rc?

        ! Set initial values
        status = ESMF_RC_NOT_IMPL
        rcpresent = .FALSE.   

        ! Initialize return code; assume failure until success is certain
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_RC_NOT_IMPL
        endif

        ESMF_INIT_CHECK_DEEP(ESMF_RouteGetInit,route,rc)
        ESMF_INIT_CHECK_DEEP(ESMF_DELayoutGetInit,delayout,rc)

        ! Translate AxisIndices from F90 to C++
        do j=1,rank
          do i=1,AI_count
            AI_exc(i,j)%min = AI_exc(i,j)%min - 1
            AI_exc(i,j)%max = AI_exc(i,j)%max - 1
            AI_tot(i,j)%min = AI_tot(i,j)%min - 1
            AI_tot(i,j)%max = AI_tot(i,j)%max - 1
          enddo
        enddo

        ! Call C++  code
        call c_ESMC_RoutePrecomputeHalo(route, rank, my_DE, AI_exc, AI_tot, &
                                        AI_count, global_start, global_count, &
                                        delayout, periodic, status)
        if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
                      ESMF_CONTEXT, rcToReturn=rc))  then
          print *, "Route Precompute Halo error"
          print *, "return after adding 1 back to AIs"
        endif

        ! Translate AxisIndices back to  F90 from C++
        do j=1,rank
          do i=1,AI_count
            AI_exc(i,j)%min = AI_exc(i,j)%min + 1
            AI_exc(i,j)%max = AI_exc(i,j)%max + 1
            AI_tot(i,j)%min = AI_tot(i,j)%min + 1
            AI_tot(i,j)%max = AI_tot(i,j)%max + 1
          enddo
        enddo

        if (rcpresent) rc = status

        end subroutine ESMF_RoutePrecomputeHalo

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_RoutePrecomputeRedist - Precompute communication paths

! !INTERFACE:
      subroutine ESMF_RoutePrecomputeRedist(route, rank, &
                                            hasSrcData, srcDELayout, mySrcDE, &
                                            srcGlobalCompAIperDEperRank, &
                                            mySrcGlobalTotalAIperRank, &
                                            hasDstData, dstDELayout, myDstDE, &
                                            dstGlobalCompAIperDEperRank, &
                                            myDstGlobalTotalAIperRank, rc)

! !ARGUMENTS:
      type(ESMF_Route), intent(inout) :: route
      integer, intent(in) :: rank
      type(ESMF_Logical), intent(in) :: hasSrcData
      type(ESMF_DELayout), intent(inout) :: srcDElayout
      integer, intent(in) :: mySrcDE
      type(ESMF_AxisIndex), dimension(:), pointer :: mySrcGlobalTotalAIperRank
      type(ESMF_AxisIndex), dimension(:,:), pointer :: &
                                                     srcGlobalCompAIperDEperRank
      type(ESMF_Logical), intent(in) :: hasDstData
      type(ESMF_DELayout), intent(inout) :: dstDElayout
      integer, intent(in) :: myDstDE
      type(ESMF_AxisIndex), dimension(:), pointer :: myDstGlobalTotalAIperRank
      type(ESMF_AxisIndex), dimension(:,:), pointer :: &
                                                     dstGlobalCompAIperDEperRank
      integer, intent(out), optional :: rc

! TODO: do we need to pass myDEs in?
!
! !DESCRIPTION:
!     Precompute the data movement needed to execute a 
!     data redistribution operation, and store this 
!     information in an {\tt ESMF\_Route}.
!
!     The arguments are:
!     \begin{description}
!     \item[route] 
!          {\tt ESMF\_Route} to associate this information with.
!     \item[rank]
!          Data rank.
!     \item[hasSrcData]
!          ESMF logical for whether this PET contains valid data.
!     \item[srcDELayout]
!          {\tt ESMF\_DELayout} for the source data decomposition.
!     \item[srcMyDE]
!          The ID of the source DE.
!     \item[mySrcGlobalTotalAIperRank]
!          A 1D array of {\tt ESMF\_AxisIndex}s, describing the
!          total region in global index space of the data on this DE
!          in the source.
!     \item[srcGlobalCompAIperDEperRank]
!          A 2D array of {\tt ESMF\_AxisIndex}s, describing the
!          computational region in global index space of the data for
!          each DE in the source.
!     \item[hasDstData]
!          ESMF logical for whether this PET contains valid data.
!     \item[dstDELayout]
!          {\tt ESMF\_DELayout} for the destination data decomposition.
!     \item[dstMyDE]
!          The ID of the destination DE.
!     \item[myDstGlobalTotalAIperRank]
!          A 1D array of {\tt ESMF\_AxisIndex}s, describing the
!          total region in global index space of the data on this DE
!          in the destination.
!     \item[dstGlobalCompAIperDEperRank]
!          A 2D array of {\tt ESMF\_AxisIndex}s, describing the
!          computational region in global index space of the data for
!          each DE in the destination.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI

        ! local variables
        integer :: status                  ! local error status
        logical :: rcpresent               ! did user specify rc?
        integer :: i,j                     ! counters
        integer :: dstDECount, srcDECount

        ! Set initial values
        status = ESMF_RC_NOT_IMPL
        rcpresent = .FALSE.   

        ! Initialize return code; assume failure until success is certain
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_RC_NOT_IMPL
        endif

        ESMF_INIT_CHECK_DEEP(ESMF_RouteGetInit,route,rc)
        ESMF_INIT_CHECK_DEEP(ESMF_DELayoutGetInit,srcDELayout,rc)
        ESMF_INIT_CHECK_DEEP(ESMF_DELayoutGetInit,dstDELayout,rc)

        ! TODO get DE counts from layouts
        ! TODO make sure DE counts are the same as the first rank 
        !      sizes of the AIs and the rank should match the second
        ! set some sizes to pass to C++ code
        srcDECount = size(srcGlobalCompAIperDEperRank, 1)
        dstDECount = size(dstGlobalCompAIperDEperRank, 1)

        ! Translate AxisIndices from F90 to C++
        do j   = 1,rank
          mySrcGlobalTotalAIperRank(j)%min = &
                                    mySrcGlobalTotalAIperRank(j)%min - 1
          mySrcGlobalTotalAIperRank(j)%max = &
                                    mySrcGlobalTotalAIperRank(j)%max - 1
          myDstGlobalTotalAIperRank(j)%min = &
                                    myDstGlobalTotalAIperRank(j)%min - 1
          myDstGlobalTotalAIperRank(j)%max = &
                                    myDstGlobalTotalAIperRank(j)%max - 1
          do i = 1,dstDECount
            dstGlobalCompAIperDEperRank(i,j)%min = &
                                    dstGlobalCompAIperDEperRank(i,j)%min - 1
            dstGlobalCompAIperDEperRank(i,j)%max = &
                                    dstGlobalCompAIperDEperRank(i,j)%max - 1
          enddo
          do i = 1,srcDECount
            srcGlobalCompAIperDEperRank(i,j)%min = &
                                    srcGlobalCompAIperDEperRank(i,j)%min - 1
            srcGlobalCompAIperDEperRank(i,j)%max = &
                                    srcGlobalCompAIperDEperRank(i,j)%max - 1
          enddo
        enddo

        ! Call C++  code
        call c_ESMC_RoutePrecomputeRedist(route, rank, &
                                          hasSrcData, srcDELayout, &
                                          mySrcDE, srcDECount, &
                                          srcGlobalCompAIperDEperRank, &
                                          mySrcGlobalTotalAIperRank, &
                                          hasDstData, dstDELayout, &
                                          myDstDE, dstDECount, &
                                          dstGlobalCompAIperDEperRank, &
                                          myDstGlobalTotalAIperRank, status)
        if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
                    ESMF_CONTEXT, rcToReturn=rc))  then
          print *, "Route PrecomputeRedist error"
          print *, "return after adding 1 back to AIs"
        endif

        ! Translate AxisIndices back to  F90 from C++
        do j   = 1,rank
	    mySrcGlobalTotalAIperRank(j)%min = &
                                         mySrcGlobalTotalAIperRank(j)%min  + 1
	    mySrcGlobalTotalAIperRank(j)%max = &
                                         mySrcGlobalTotalAIperRank(j)%max  + 1
	    myDstGlobalTotalAIperRank(j)%min = &
                                         myDstGlobalTotalAIperRank(j)%min  + 1
	    myDstGlobalTotalAIperRank(j)%max = &
                                         myDstGlobalTotalAIperRank(j)%max  + 1
          do i = 1,srcDECount
	    srcGlobalCompAIperDEperRank(i,j)%min = &
	                              srcGlobalCompAIperDEperRank(i,j)%min + 1
	    srcGlobalCompAIperDEperRank(i,j)%max = &
	                              srcGlobalCompAIperDEperRank(i,j)%max + 1
          enddo
          do i = 1,dstDECount
	    dstGlobalCompAIperDEperRank(i,j)%min = &
	                              dstGlobalCompAIperDEperRank(i,j)%min + 1
	    dstGlobalCompAIperDEperRank(i,j)%max = &
	                              dstGlobalCompAIperDEperRank(i,j)%max + 1
          enddo
        enddo

        if (rcpresent) rc = status

        end subroutine ESMF_RoutePrecomputeRedist

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_RoutePrecomputeRedistV - Precompute communication paths

! !INTERFACE:
      subroutine ESMF_RoutePrecomputeRedistV(route, rank, hasDstData, &
                                             dstMyDE, dstVector, dstCompAI, &
                                             dstTotalAI, dstAICountPerDE, &
                                             dstGlobalStart, &
                                             dstGlobalCount, dstDElayout, &
                                             hasSrcData, &
                                             srcMyDE, srcVector, srcCompAI, &
                                             srcTotalAI, srcAICountPerDE, &
                                             srcGlobalStart, &
                                             srcGlobalCount, srcDElayout, rc)

! !ARGUMENTS:
      type(ESMF_Route), intent(inout) :: route
      integer, intent(in) :: rank
      type(ESMF_Logical), intent(in) :: hasDstData
      integer, intent(in) :: dstMyDE
      logical, intent(in) :: dstVector
      type(ESMF_AxisIndex), dimension(:,:), pointer :: dstCompAI
      type(ESMF_AxisIndex), dimension(:,:), pointer :: dstTotalAI
      integer, dimension(:), intent(in) :: dstAICountPerDE
      integer, dimension(:,:), intent(in) :: dstGlobalStart
      integer, dimension(ESMF_MAXIGRIDDIM), intent(in) :: dstGlobalCount
      type(ESMF_DELayout), intent(inout) :: dstDElayout
      type(ESMF_Logical), intent(in) :: hasSrcData
      integer, intent(in) :: srcMyDE
      logical, intent(in) :: srcVector
      type(ESMF_AxisIndex), dimension(:,:), pointer :: srcCompAI
      type(ESMF_AxisIndex), dimension(:,:), pointer :: srcTotalAI
      integer, dimension(:), intent(in) :: srcAICountPerDE
      integer, dimension(:,:), intent(in) :: srcGlobalStart
      integer, dimension(ESMF_MAXIGRIDDIM), intent(in) :: srcGlobalCount
      type(ESMF_DELayout), intent(inout) :: srcDElayout
      integer, intent(out), optional :: rc

!
! !DESCRIPTION:
!     Precompute the data movement needed to execute a data redistribution
!     operation where at least one of the fields has been distributed as a
!     vector, and store this information in an {\tt ESMF\_Route}.
!
!     The arguments are:
!     \begin{description}
!     \item[route] 
!          {\tt ESMF\_Route} to associate this information with.
!     \item[rank]
!          Data rank.
!     \item[dstMyDE]
!          The ID of the destination DE.
!     \item[dstVector]
!          A logical flag denoting whether or not the destination Field
!          has been distributed as a vector.
!     \item[dstCompAI]
!          A 2D array of {\tt ESMF\_AxisIndex}s, describing the
!          computational region of the data for each DE in the destination.
!     \item[dstTotalAI]
!          A 2D array of {\tt ESMF\_AxisIndex}s, describing the
!          total region of the data for each DE in the destination.
!     \item[dstGlobalStart]
!          Array of integer offsets for the corner of each DE relative to
!          the undecomposed destination object.
!     \item[dstGlobalCount]
!          Array of integers, one per dimension, for the total count
!          of items for the undecomposed destination object.
!     \item[dstDELayout]
!          {\tt ESMF\_DELayout} for the destination data decomposition.
!     \item[srcMyDE]
!          The ID of the source DE.
!     \item[srcVector]
!          A logical flag denoting whether or not the source Field
!          has been distributed as a vector.
!     \item[srcCompAI]
!          A 2D array of {\tt ESMF\_AxisIndex}s, describing the
!          computational region of the data for each DE in the source.
!     \item[srcTotalAI]
!          A 2D array of {\tt ESMF\_AxisIndex}s, describing the
!          total region of the data for each DE in the source.
!     \item[srcGlobalStart]
!          Array of integer offsets for the corner of each DE relative to
!          the undecomposed source object.
!     \item[srcGlobalCount]
!          Array of integers, one per dimension, for the total count
!          of items for the undecomposed source object.
!     \item[srcDELayout]
!          {\tt ESMF\_DELayout} for the source data decomposition.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI

        ! local variables
        integer :: status                  ! local error status
        logical :: rcpresent               ! did user specify rc?
        integer :: i,j                     ! counters
        integer :: dstAICount, srcAICount
        integer :: dstGSCount, srcGSCount
        type(ESMF_Logical) :: dstESMFVector, srcESMFVector

        ! Set initial values
        status = ESMF_RC_NOT_IMPL
        rcpresent = .FALSE.   

        ! Initialize return code; assume failure until success is certain
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_RC_NOT_IMPL
        endif

        ESMF_INIT_CHECK_DEEP(ESMF_RouteGetInit,route,rc)
        ESMF_INIT_CHECK_DEEP(ESMF_DELayoutGetInit,srcDELayout,rc)
        ESMF_INIT_CHECK_DEEP(ESMF_DELayoutGetInit,dstDELayout,rc)

        ! set some sizes to pass to C++ code
        dstAICount = size(dstCompAI,1)
        srcAICount = size(srcCompAI,1)
        dstGSCount = size(dstGlobalStart,1)
        srcGSCount = size(srcGlobalStart,1)

        ! Translate AxisIndices from F90 to C++
        ! TODO: fix so that for the vector field we do not pass in both
        !       computational and total AIs, since they are the same
        !       to do that will require something like the commented
        !       out lines below
        do j   = 1,rank
          do i = 1,dstAICount
            dstCompAI(i,j)%min  =  dstCompAI(i,j)%min - 1
            dstCompAI(i,j)%max  =  dstCompAI(i,j)%max - 1
!           if (.not.dstVector) then
              dstTotalAI(i,j)%min = dstTotalAI(i,j)%min - 1
              dstTotalAI(i,j)%max = dstTotalAI(i,j)%max - 1
!           endif
          enddo
          do i = 1,srcAICount
            srcCompAI(i,j)%min  =  srcCompAI(i,j)%min - 1
            srcCompAI(i,j)%max  =  srcCompAI(i,j)%max - 1
!           if (.not.srcVector) then
              srcTotalAI(i,j)%min = srcTotalAI(i,j)%min - 1
              srcTotalAI(i,j)%max = srcTotalAI(i,j)%max - 1
!           endif
          enddo
        enddo

        ! set ESMF logicals to pass across the language interface
        dstESMFVector = ESMF_FALSE
        srcESMFVector = ESMF_FALSE
        if (dstVector) dstESMFVector = ESMF_TRUE
        if (srcVector) srcESMFVector = ESMF_TRUE

        ! Call C++  code
        call c_ESMC_RoutePrecomputeRedistV(route, rank, hasDstData, &
                                           dstMyDE, dstESMFVector, &
                                           dstCompAI, dstTotalAI, &
                                           dstAICount, dstAICountPerDE, &
                                           dstGlobalStart, dstGSCount, &
                                           dstGlobalCount, dstDElayout, &
                                           hasSrcData, &
                                           srcMyDE, srcESMFVector, &
                                           srcCompAI, srcTotalAI, &
                                           srcAICount, srcAICountPerDE, &
                                           srcGlobalStart, srcGSCount, &
                                           srcGlobalCount, srcDElayout, status)
        if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
                      ESMF_CONTEXT, rcToReturn=rc))  then
          print *, "Route PrecomputeRedistV error"
          print *, "return after adding 1 back to AIs"
        endif

        ! Translate AxisIndices back to  F90 from C++
        do j   = 1,rank
          do i = 1,dstAICount
            dstCompAI(i,j)%min  =  dstCompAI(i,j)%min + 1
            dstCompAI(i,j)%max  =  dstCompAI(i,j)%max + 1
            dstTotalAI(i,j)%min = dstTotalAI(i,j)%min + 1
            dstTotalAI(i,j)%max = dstTotalAI(i,j)%max + 1
          enddo
          do i = 1,srcAICount
            srcCompAI(i,j)%min  =  srcCompAI(i,j)%min + 1
            srcCompAI(i,j)%max  =  srcCompAI(i,j)%max + 1
            srcTotalAI(i,j)%min = srcTotalAI(i,j)%min + 1
            srcTotalAI(i,j)%max = srcTotalAI(i,j)%max + 1
          enddo
        enddo

        if (rcpresent) rc = status

        end subroutine ESMF_RoutePrecomputeRedistV
!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_RoutePrecomputeRedistA2A - Precompute communication paths

! !INTERFACE:
      subroutine ESMF_RoutePrecomputeRedistA2A(route, rank, hasDstData, &
                                             dstMyDE, dstCompAI, &
                                             dstAICountPerDE, &
                                             dstGlobalStart, &
                                             dstGlobalCount, dstDElayout, &
					     hasSrcData, &
                                             srcMyDE, srcCompAI, &
                                             srcAICountPerDE, &
                                             srcGlobalStart, &
                                             srcGlobalCount, srcDElayout, rc)

! !ARGUMENTS:
      type(ESMF_Route), intent(inout) :: route
      integer, intent(in) :: rank
      type(ESMF_Logical), intent(in) :: hasDstData
      integer, intent(in) :: dstMyDE
      type(ESMF_AxisIndex), dimension(:,:), pointer :: dstCompAI
      integer, dimension(:), intent(in) :: dstAICountPerDE
      integer, dimension(:,:), intent(in) :: dstGlobalStart
      integer, dimension(ESMF_MAXIGRIDDIM), intent(in) :: dstGlobalCount
      type(ESMF_DELayout), intent(inout) :: dstDElayout
      type(ESMF_Logical), intent(in) :: hasSrcData
      integer, intent(in) :: srcMyDE
      type(ESMF_AxisIndex), dimension(:,:), pointer :: srcCompAI
      integer, dimension(:), intent(in) :: srcAICountPerDE
      integer, dimension(:,:), intent(in) :: srcGlobalStart
      integer, dimension(ESMF_MAXIGRIDDIM), intent(in) :: srcGlobalCount
      type(ESMF_DELayout), intent(inout) :: srcDElayout
      integer, intent(out), optional :: rc

!
! !DESCRIPTION:
!     Precompute the data movement needed to execute a data redistribution
!     operation from an arbitrarily distributed igrid to an arbitrary 
!     distributed igrid
!
!     The arguments are:
!     \begin{description}
!     \item[route] 
!          {\tt ESMF\_Route} to associate this information with.
!     \item[rank]
!          Data rank.
!     \item[dstMyDE]
!          The ID of the destination DE.
!     \item[dstCompAI]
!          A 2D array of {\tt ESMF\_AxisIndex}s, describing the
!          computational region of the data for each DE in the destination.
!     \item[dstGlobalStart]
!          Array of integer offsets for the corner of each DE relative to
!          the undecomposed destination object.
!     \item[dstGlobalCount]
!          Array of integers, one per dimension, for the total count
!          of items for the undecomposed destination object.
!     \item[dstDELayout]
!          {\tt ESMF\_DELayout} for the destination data decomposition.
!     \item[srcMyDE]
!          The ID of the source DE.
!     \item[srcCompAI]
!          A 2D array of {\tt ESMF\_AxisIndex}s, describing the
!          computational region of the data for each DE in the source.
!     \item[srcGlobalStart]
!          Array of integer offsets for the corner of each DE relative to
!          the undecomposed source object.
!     \item[srcGlobalCount]
!          Array of integers, one per dimension, for the total count
!          of items for the undecomposed source object.
!     \item[srcDELayout]
!          {\tt ESMF\_DELayout} for the source data decomposition.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI

        ! local variables
        integer :: status                  ! local error status
        logical :: rcpresent               ! did user specify rc?
        integer :: i,j                     ! counters
        integer :: dstAICount, srcAICount
        integer :: dstGSCount, srcGSCount

        ! Set initial values
        status = ESMF_RC_NOT_IMPL
        rcpresent = .FALSE.   

        ! Initialize return code; assume failure until success is certain
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_RC_NOT_IMPL
        endif

        ESMF_INIT_CHECK_DEEP(ESMF_RouteGetInit,route,rc)
        ESMF_INIT_CHECK_DEEP(ESMF_DELayoutGetInit,srcDELayout,rc)
        ESMF_INIT_CHECK_DEEP(ESMF_DELayoutGetInit,dstDELayout,rc)

        ! set some sizes to pass to C++ code
        dstAICount = size(dstCompAI,1)
        srcAICount = size(srcCompAI,1)
        dstGSCount = size(dstGlobalStart,1)
        srcGSCount = size(srcGlobalStart,1)

        ! Translate AxisIndices from F90 to C++
        ! TODO: fix so that for the vector field we do not pass in both
        !       computational and total AIs, since they are the same
        !       to do that will require something like the commented
        !       out lines below
        do j   = 1,rank
          do i = 1,dstAICount
            dstCompAI(i,j)%min  =  dstCompAI(i,j)%min - 1
            dstCompAI(i,j)%max  =  dstCompAI(i,j)%max - 1
          enddo
          do i = 1,srcAICount
            srcCompAI(i,j)%min  =  srcCompAI(i,j)%min - 1
            srcCompAI(i,j)%max  =  srcCompAI(i,j)%max - 1
          enddo
        enddo

        ! Call C++  code
        call c_ESMC_RoutePrecomputeRedistA2A(route, rank, hasDstData, &
                                           dstMyDE, &
                                           dstCompAI, &
                                           dstAICount, dstAICountPerDE, &
                                           dstGlobalStart, dstGSCount, &
                                           dstGlobalCount, dstDElayout, &
                                           hasSrcData, srcMyDE, &
                                           srcCompAI, &
                                           srcAICount, srcAICountPerDE, &
                                           srcGlobalStart, srcGSCount, &
                                           srcGlobalCount, srcDElayout, status)
        if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
                      ESMF_CONTEXT, rcToReturn=rc))  then
          print *, "Route PrecomputeRedistV error"
          print *, "return after adding 1 back to AIs"
        endif

        ! Translate AxisIndices back to  F90 from C++
        do j   = 1,rank
          do i = 1,dstAICount
            dstCompAI(i,j)%min  =  dstCompAI(i,j)%min + 1
            dstCompAI(i,j)%max  =  dstCompAI(i,j)%max + 1
          enddo
          do i = 1,srcAICount
            srcCompAI(i,j)%min  =  srcCompAI(i,j)%min + 1
            srcCompAI(i,j)%max  =  srcCompAI(i,j)%max + 1
          enddo
        enddo

        if (rcpresent) rc = status

        end subroutine ESMF_RoutePrecomputeRedistA2A

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_RoutePrecomputeRegrid - Precompute communication paths

! !INTERFACE:
      subroutine ESMF_RoutePrecomputeRegrid(route, rank, &
               my_DE_dst, AI_dst_exc, AI_dst_tot, AI_dst_count, &
               dst_global_start, dst_global_count, dstDElayout, &
               my_DE_src, AI_src_exc, AI_src_tot, AI_src_count, &
               src_global_start, src_global_count, srcDElayout, rc)


! !ARGUMENTS:
      type(ESMF_Route), intent(inout) :: route
      integer, intent(in) :: rank
      integer, intent(in) :: my_DE_dst
      type(ESMF_AxisIndex), dimension(:,:), pointer :: AI_dst_exc
      type(ESMF_AxisIndex), dimension(:,:), pointer :: AI_dst_tot
      integer, intent(in) :: AI_dst_count
      integer, dimension(:,:), intent(in) :: dst_global_start
      integer, dimension(ESMF_MAXIGRIDDIM), intent(in) :: dst_global_count
      type(ESMF_DELayout), intent(inout) :: dstDElayout
      integer, intent(in) :: my_DE_src
      type(ESMF_AxisIndex), dimension(:,:), pointer :: AI_src_exc
      type(ESMF_AxisIndex), dimension(:,:), pointer :: AI_src_tot
      integer, intent(in) :: AI_src_count
      integer, dimension(:,:), intent(in) :: src_global_start
      integer, dimension(ESMF_MAXIGRIDDIM), intent(in) :: src_global_count
      type(ESMF_DELayout), intent(inout) :: srcDElayout
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Precompute the data movement needed to execute a 
!     data regrid operation, and store this 
!     information in an {\tt ESMF\_Route}.
!
!     The arguments are:
!     \begin{description}
!     \item[route] 
!          {\tt ESMF\_Route} to associate this information with.
!     \item[rank]
!          Data rank.
!     \item[my_DE_dst]
!          The ID of the destination DE.
!     \item[AI_dst_exc]
!          A 2D array of {\tt ESMF\_AxisIndex}s, describing the
!          computational region of the data for each DE in the destination.
!     \item[AI_dst_tot]
!          A 2D array of {\tt ESMF\_AxisIndex}s, describing the
!          total region of the data for each DE in the destination.
!     \item[AI_dst_count]
!          Integer count of number of {\tt ESMF\_AxisIndex}s in the
!          destination.
!     \item[dst_global_start]
!          Array of integer offsets for the corner of each DE relative to
!          the undecomposed destination object.
!     \item[dst_global_count]
!          Array of integers, one per dimension, for the total count
!          of items for the undecomposed destination object.
!     \item[dstDELayout]
!          {\tt ESMF\_DELayout} for the destination data decomposition.
!     \item[my_DE_src]
!          The ID of the source DE.
!     \item[AI_src_exc]
!          A 2D array of {\tt ESMF\_AxisIndex}s, describing the
!          computational region of the data for each DE in the source.
!     \item[AI_src_tot]
!          A 2D array of {\tt ESMF\_AxisIndex}s, describing the
!          total region of the data for each DE in the source.
!     \item[AI_src_count]
!          Integer count of number of {\tt ESMF\_AxisIndex}s in the
!     \item[src_global_start]
!          Array of integer offsets for the corner of each DE relative to
!          the undecomposed source object.
!     \item[src_global_count]
!          Array of integers, one per dimension, for the total count
!          of items for the undecomposed source object.
!     \item[srcDELayout]
!          {\tt ESMF\_DELayout} for the source data decomposition.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI

        ! local variables
        integer :: status                  ! local error status
        integer :: i,j                     ! counters
        logical :: rcpresent               ! did user specify rc?

        ! Set initial values
        status = ESMF_RC_NOT_IMPL
        rcpresent = .FALSE.   

        ! Initialize return code; assume failure until success is certain
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_RC_NOT_IMPL
        endif

        ESMF_INIT_CHECK_DEEP(ESMF_RouteGetInit,route,rc)
        ESMF_INIT_CHECK_DEEP(ESMF_DELayoutGetInit,srcDELayout,rc)
        ESMF_INIT_CHECK_DEEP(ESMF_DELayoutGetInit,dstDELayout,rc)


        ! Translate AxisIndices from F90 to C++
        do j=1,rank
          do i=1,AI_dst_count
            AI_dst_exc(i,j)%min = AI_dst_exc(i,j)%min - 1
            AI_dst_exc(i,j)%max = AI_dst_exc(i,j)%max - 1
            AI_dst_tot(i,j)%min = AI_dst_tot(i,j)%min - 1
            AI_dst_tot(i,j)%max = AI_dst_tot(i,j)%max - 1
          enddo
          do i=1,AI_src_count
            AI_src_exc(i,j)%min = AI_src_exc(i,j)%min - 1
            AI_src_exc(i,j)%max = AI_src_exc(i,j)%max - 1
            AI_src_tot(i,j)%min = AI_src_tot(i,j)%min - 1
            AI_src_tot(i,j)%max = AI_src_tot(i,j)%max - 1
          enddo
        enddo

        ! Call C++  code
        call c_ESMC_RoutePrecomputeRegrid(route, rank, &
           my_DE_dst, AI_dst_exc, AI_dst_tot, AI_dst_count, &
           dst_global_start, dst_global_count, dstDElayout, &
           my_DE_src, AI_src_exc, AI_src_tot, AI_src_count, &
           src_global_start, src_global_count, srcDElayout, status)
        if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
                      ESMF_CONTEXT, rcToReturn=rc))  then
          print *, "Route PrecomputeRegrid error"
          print *, "return after adding 1 back to AIs"
        endif

        ! Translate AxisIndices back to  F90 from C++
        do j=1,rank
          do i=1,AI_dst_count
            AI_dst_exc(i,j)%min = AI_dst_exc(i,j)%min + 1
            AI_dst_exc(i,j)%max = AI_dst_exc(i,j)%max + 1
            AI_dst_tot(i,j)%min = AI_dst_tot(i,j)%min + 1
            AI_dst_tot(i,j)%max = AI_dst_tot(i,j)%max + 1
          enddo
          do i=1,AI_src_count
            AI_src_exc(i,j)%min = AI_src_exc(i,j)%min + 1
            AI_src_exc(i,j)%max = AI_src_exc(i,j)%max + 1
            AI_src_tot(i,j)%min = AI_src_tot(i,j)%min + 1
            AI_src_tot(i,j)%max = AI_src_tot(i,j)%max + 1
          enddo
        enddo

        if (rcpresent) rc = status

        end subroutine ESMF_RoutePrecomputeRegrid

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_RouteSet"
!BOPI
! !IROUTINE: ESMF_RouteSet - Set options on Route

! !INTERFACE:
      subroutine ESMF_RouteSet(route, options, rc)
!
! !ARGUMENTS:
      type(ESMF_Route), intent(inout) :: route
      type(ESMF_RouteOptions), intent(in) :: options
      integer, intent(out), optional :: rc            

!
! !DESCRIPTION:
!     Set the communication options for an {\tt ESMF\_Route}.
!
!     The arguments are:
!     \begin{description}
!     \item[route] 
!          {\tt ESMF\_Route} to be modified.
!     \item[options]
!          One of the valid paramaters of type {\tt ESMF\_RouteOptions}.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI

        ! local variables
        integer :: status                  ! local error status
        logical :: rcpresent               ! did user specify rc?

        ! Set initial values
        status = ESMF_RC_NOT_IMPL
        rcpresent = .FALSE.   

        ! Initialize return code; assume failure until success is certain
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_RC_NOT_IMPL
        endif

        ESMF_INIT_CHECK_DEEP(ESMF_RouteGetInit,route,rc)

        ! Call C++  code
        call c_ESMC_RouteSet(route, options, status)
        if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_RouteSet

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_RouteSetRecvItems"
!BOPI
! !IROUTINE: ESMF_RouteSetRecvItems - Set size of receive buffer

! !INTERFACE:
      subroutine ESMF_RouteSetRecvItems(route, nitems, rc)
!
! !ARGUMENTS:
      type(ESMF_Route), intent(inout) :: route
      integer, intent(in) :: nitems
      integer, intent(out), optional :: rc            

!
! !DESCRIPTION:
!     Set the required size of the receive buffer for an {\tt ESMF\_Route} in
!     number of items (not in bytes).  In general a receive buffer is
!     already allocated, but if not, the caller can first query the route
!     for the size of the receive buffer, allocate it, and then call
!     {\tt ESMF\_RouteRun()} to move the data.
!
!     The arguments are:
!     \begin{description}
!     \item[route] 
!          {\tt ESMF\_Route} to be modified.
!     \item[nitems]
!          Size of the receive buffer for this route, in number of items.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI

        ! local variables
        integer :: status                  ! local error status
        logical :: rcpresent               ! did user specify rc?

        ! Set initial values
        status = ESMF_RC_NOT_IMPL
        rcpresent = .FALSE.   

        ! Initialize return code; assume failure until success is certain
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_RC_NOT_IMPL
        endif

        ESMF_INIT_CHECK_DEEP(ESMF_RouteGetInit,route,rc)

        ! Call C++  code
        call c_ESMC_RouteSetRecvItems(route, nitems, status)
        if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_RouteSetRecvItems

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_RouteSetRecv"
!BOPI
! !IROUTINE: ESMF_RouteSetRecv - Set receive values in a Route

! !INTERFACE:
      subroutine ESMF_RouteSetRecv(route, srcPET, xp, rc)
!
! !ARGUMENTS:
      type(ESMF_Route), intent(inout) :: route
      integer, intent(in) :: srcPET
      type(ESMF_XPacket), intent(inout) :: xp
      integer, intent(out), optional :: rc            

!
! !DESCRIPTION:
!     Set receive information in an {\tt ESMF\_Route}.
!
!     The arguments are:
!     \begin{description}
!     \item[route] 
!          {\tt ESMF\_Route} to be modified.
!     \item[srcPET]
!          Source PET id.
!     \item[xp]
!          Exchange packet describing the data to be received.  Note that
!          an exchange packet only contains offsets and counts; the base
!          address will be specified at route run time.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI

        ! local variables
        integer :: status                  ! local error status
        logical :: rcpresent               ! did user specify rc?

        ! Set initial values
        status = ESMF_RC_NOT_IMPL
        rcpresent = .FALSE.   

        ! Initialize return code; assume failure until success is certain
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_RC_NOT_IMPL
        endif

	! Check initialization
        ESMF_INIT_CHECK_DEEP(ESMF_RouteGetInit,route,rc)
        ESMF_INIT_CHECK_SHALLOW(ESMF_XPacketGetInit,ESMF_XPacketInit,xp)


        ! Call C++  code
        call c_ESMC_RouteSetRecv(route, srcPET, xp, status)
        if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_RouteSetRecv

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_RouteSetSend"
!BOPI
! !IROUTINE: ESMF_RouteSetSend - Set send values in a Route

! !INTERFACE:
      subroutine ESMF_RouteSetSend(route, destPET, xp, rc)
!
! !ARGUMENTS:
      type(ESMF_Route), intent(inout) :: route
      integer, intent(in) :: destPET
      type(ESMF_XPacket), intent(inout) :: xp
      integer, intent(out), optional :: rc            

!
! !DESCRIPTION:
!     Set send information in an {\tt ESMF\_Route}.
!
!     The arguments are:
!     \begin{description}
!     \item[route] 
!          {\tt ESMF\_Route} to be modified.
!     \item[destPET]
!          Destination PET id.
!     \item[xp]
!          Exchange packet describing the data to be sent.  Note that
!          an exchange packet only contains offsets and counts; the base
!          address will be specified at route run time.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI

        ! local variables
        integer :: status                  ! local error status
        logical :: rcpresent               ! did user specify rc?

        ! Set initial values
        status = ESMF_RC_NOT_IMPL
        rcpresent = .FALSE.   

        ! Initialize return code; assume failure until success is certain
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_RC_NOT_IMPL
        endif

	! Check initialization
        ESMF_INIT_CHECK_DEEP(ESMF_RouteGetInit,route,rc)
        ESMF_INIT_CHECK_SHALLOW(ESMF_XPacketGetInit,ESMF_XPacketInit,xp)


        ! Call C++  code
        call c_ESMC_RouteSetSend(route, destPET, xp, status)
        if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_RouteSetSend

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_RoutePrint"
!BOPI
! !IROUTINE: ESMF_RoutePrint - Print the contents of a Route

! !INTERFACE:
      subroutine ESMF_RoutePrint(route, options, rc)
!
! !ARGUMENTS:
      type(ESMF_Route), intent(inout) :: route      
      character (len=*), intent(in), optional :: options      
      integer, intent(out), optional :: rc           
!
! !DESCRIPTION:
!     Print information about an {\tt ESMF\_Route}.  
!
!     Note:  Many {\tt ESMF\_<class>Print} methods are implemented in C++.
!     On some platforms/compilers there is a potential issue with interleaving
!     Fortran and C++ output to {\tt stdout} such that it doesn't appear in
!     the expected order.  If this occurs, it is recommended to use the
!     standard Fortran call {\tt flush(6)} as a workaround until this issue
!     is fixed in a future release. 
!
!     The arguments are:
!     \begin{description}
!     \item[route] 
!          {\tt ESMF\_Route} to be printed.
!     \item[{[options]}]
!          Print options that control the type of information and level of 
!          detail.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI

       character (len=6) :: defaultopts      ! default print options
       integer :: status                     ! local error status
       logical :: rcpresent

       ! Initialize return code; assume failure until success is certain       
       status = ESMF_RC_NOT_IMPL
       rcpresent = .FALSE.
       if (present(rc)) then
         rcpresent = .TRUE.  
         rc = ESMF_RC_NOT_IMPL
       endif

       ! Check initialization
       ESMF_INIT_CHECK_DEEP(ESMF_RouteGetInit,route,rc)

       defaultopts = "brief"

       if(present(options)) then
           call c_ESMC_RoutePrint(route, options, status)   
       else
           call c_ESMC_RoutePrint(route, defaultopts, status)
       endif

       if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

       ! Set return values
       if (rcpresent) rc = ESMF_SUCCESS
 
       end subroutine ESMF_RoutePrint

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_RouteRun"
!BOPI
! !IROUTINE: ESMF_RouteRun - Execute the communications the Route represents

! !INTERFACE:
      subroutine ESMF_RouteRun(route, srcArray, dstArray, rc)
!
! !ARGUMENTS:
      type(ESMF_Route), intent(inout) :: route
      type(ESMF_LocalArray), intent(inout), optional :: srcarray
      type(ESMF_LocalArray), intent(inout), optional :: dstarray
      integer, intent(out), optional :: rc            

!
! !DESCRIPTION:
!     Execute the communications this {\tt ESMF\_Route} represents.
!
!     The arguments are:
!     \begin{description}
!     \item[route] 
!          {\tt ESMF\_Route} to be executed.
!     \item[{[srcArray]}]
!          {\tt ESMF\_LocalArray} containing data to be sent.
!     \item[{[dstArray]}]
!          {\tt ESMF\_LocalArray} containing data to be received.
!          If not specified, {\tt srcArray} is both source and destination.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI

        ! local variables
        integer :: status                  ! local error status
        logical :: rcpresent               ! did user specify rc?
        type(ESMF_Pointer) :: empty
        type(ESMF_Pointer) :: srcLocArrayPtr, &
                              dstLocArrayPtr
  
        ! Set initial values
        status = ESMF_RC_NOT_IMPL
        rcpresent = .FALSE.   
        empty= ESMF_NULL_POINTER

        ! Initialize return code; assume failure until success is certain
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_RC_NOT_IMPL
        endif

	! Check initialization
        ESMF_INIT_CHECK_DEEP(ESMF_RouteGetInit,route,rc)
!        ESMF_INIT_CHECK_DEEP(ESMF_LocalArrayGetInit,srcarray,rc)
!        ESMF_INIT_CHECK_DEEP(ESMF_LocalArrayGetInit,dstarray,rc)
 
       ! translate F90 LocalArrays to C++ version of LocalArray
       if (present(srcarray)) then
          call ESMF_LocalArrayGetThis(srcArray,srcLocArrayPtr,status)

          if (ESMF_LogMsgFoundError(status,ESMF_ERR_PASSTHRU, & 
              ESMF_CONTEXT, rcToReturn=rc)) return
       endif
       if (present(dstarray)) then
          call ESMF_LocalArrayGetThis(dstArray,dstLocArrayPtr,status)

          if (ESMF_LogMsgFoundError(status,ESMF_ERR_PASSTHRU, & 
              ESMF_CONTEXT, rcToReturn=rc)) return
       endif


       ! Run Route depending on inputs
       if ((.not.present(srcarray)) .and. (.not.present(dstarray))) then
            ! nothing to do here
            status = ESMF_SUCCESS
        else if (.not.present(srcarray)) then
            call c_ESMC_RouteRunLA(route, empty, dstLocArrayPtr, status)
        else if (.not.present(dstarray)) then
            call c_ESMC_RouteRunLA(route, srcLocArrayPtr, empty, status)
        else  ! both srcarray and dstarray are specified
            call c_ESMC_RouteRunLA(route, srcLocArrayPtr, dstLocArrayPtr, status)
        endif

        if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_RouteRun

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_RouteRunList"
!BOPI
! !IROUTINE: ESMF_RouteRunList - Execute the communications the Route represents

! !INTERFACE:
      subroutine ESMF_RouteRunList(route, srcArrayList, dstArrayList, rc)
!
! !ARGUMENTS:
      type(ESMF_Route), intent(inout) :: route
      type(ESMF_LocalArray), intent(inout), optional :: srcArrayList(:)
      type(ESMF_LocalArray), intent(inout), optional :: dstArrayList(:)
      integer, intent(out), optional :: rc            

!
! !DESCRIPTION:
!     Execute the communications this {\tt ESMF\_Route} represents.
!
!     The arguments are:
!     \begin{description}
!     \item[route] 
!          {\tt ESMF\_Route} to be executed.
!     \item[{[srcArrayList]}]
!          List of {\tt ESMF\_LocalArray}s containing data to be sent.
!     \item[{[dstArrayList]}]
!          List of {\tt ESMF\_LocalArray}s containing data to be received.
!          If not specified, {\tt srcArrayList} is both source and destination.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI

        ! local variables
        integer :: status                  ! local error status
        logical :: rcpresent               ! did user specify rc?
        integer :: srcCount, dstCount
	integer :: i
        type(ESMF_Pointer) :: empty(1)
        type(ESMF_Pointer),allocatable :: srcArrayPtrList(:), &
                                          dstArrayPtrList(:)



        ! Set initial values
        status = ESMF_RC_NOT_IMPL
        rcpresent = .FALSE.   
        empty(1)= ESMF_NULL_POINTER

        ! Initialize return code; assume failure until success is certain
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_RC_NOT_IMPL
        endif

	! Check initialization
        ESMF_INIT_CHECK_DEEP(ESMF_RouteGetInit,route,rc)

        ! Pass exact counts into c interface.
        srcCount = 0
        dstCount = 0
        if (present(srcArrayList)) then
          srcCount = size(srcArrayList)
        endif
        if (present(dstArrayList)) then
          dstCount = size(srcArrayList)
        endif

	! Check initialization
!	if (srcCount .ne. 0) then
!  	  do i=1,srcCount,1
!	    ESMF_INIT_CHECK_DEEP(ESMF_LocalArrayGetInit,srcarrayList(i),rc)
!	  end do
!	end if
!	if (dstCount .ne. 0) then
!  	  do i=1,dstCount,1
!	    ESMF_INIT_CHECK_DEEP(ESMF_LocalArrayGetInit,dstarrayList(i),rc)
!	  end do
!	end if

        ! Translate F90 LocalArray into C++ Version
       if (present(srcarrayList)) then
          allocate(srcArrayPtrList(srcCount))
          do i=1,srcCount
             call ESMF_LocalArrayGetThis(srcArrayList(i), &
                                         srcArrayPtrList(i),status)

             if (ESMF_LogMsgFoundError(status,ESMF_ERR_PASSTHRU, & 
                 ESMF_CONTEXT, rcToReturn=rc)) return
          enddo
       endif
       if (present(dstarrayList)) then
          allocate(dstArrayPtrList(dstCount))
          do i=1,dstCount
             call ESMF_LocalArrayGetThis(dstArrayList(i), &
                                         dstArrayPtrList(i),status)

             if (ESMF_LogMsgFoundError(status,ESMF_ERR_PASSTHRU, & 
                 ESMF_CONTEXT, rcToReturn=rc)) return
          enddo
       endif


        ! Run Route depending on inputs
        if ((srcCount .eq. 0) .and. (dstCount .eq. 0)) then
            ! nothing to do here
            status = ESMF_SUCCESS
        else if (srcCount .eq. 0) then
            call c_ESMC_RouteRunLAL(route, empty, dstArrayPtrList, &
                                    srcCount, dstCount, status)
        else if (dstCount .eq. 0) then
            call c_ESMC_RouteRunLAL(route, srcArrayPtrList, empty, &
                                    srcCount, dstCount, status)
        else  ! both srcCount and dstCount is > 0
            call c_ESMC_RouteRunLAL(route, srcArrayPtrList, dstArrayPtrList, &
                                    srcCount, dstCount, status)
        endif

        if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

        ! deallocate temporary arrays
        if (present(srcarrayList)) then
           deallocate(srcArrayPtrList)
        endif
        if (present(dstarrayList)) then
          deallocate(dstArrayPtrList)
        endif


        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_RouteRunList

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_RouteRunF90PtrI411D"
!BOPI
! !IROUTINE: ESMF_RouteRunF90PtrI411D - Execute the communications the Route represents

! !INTERFACE:
      subroutine ESMF_RouteRunF90PtrI411D(route, srcarray, dstarray, rc)
!
! !ARGUMENTS:
      type(ESMF_Route), intent(inout) :: route
      integer(ESMF_KIND_I4), pointer, optional :: srcarray(:)
      integer(ESMF_KIND_I4), pointer, optional :: dstarray(:)
      integer, intent(out), optional :: rc            

!
! !DESCRIPTION:
!     Execute the communications this {\tt ESMF\_Route} represents.
!
!     The arguments are:
!     \begin{description}
!     \item[route] 
!          {\tt ESMF\_Route} to be executed.
!     \item[{[srcarray]}]
!          1D, Integer*4 Fortran array containing data to be sent.
!     \item[{[dstarray]}]
!          1D, Integer*4 Fortran array containing data to be received.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI

        ! local variables
        integer :: status                  ! local error status
        logical :: rcpresent               ! did user specify rc?
        type(ESMF_Pointer) :: empty

        ! Set initial values
        status = ESMF_RC_NOT_IMPL
        rcpresent = .FALSE.   
        empty = ESMF_NULL_POINTER

        ! Initialize return code; assume failure until success is certain
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_RC_NOT_IMPL
        endif

	! Check initialization
        ESMF_INIT_CHECK_DEEP(ESMF_RouteGetInit,route,rc)

        ! Call C++  code
        if (associated(srcarray)) then
          if (associated(dstarray)) then
            call c_ESMC_RouteRunNA(route, srcarray, dstarray, ESMF_TYPEKIND_I4, status)
          else
            call c_ESMC_RouteRunNA(route, srcarray, empty, ESMF_TYPEKIND_I4, status)
          endif
        else
          if (associated(dstarray)) then
            call c_ESMC_RouteRunNA(route, empty, dstarray, ESMF_TYPEKIND_I4, status)
          else
            call c_ESMC_RouteRunNA(route, empty, empty, ESMF_TYPEKIND_I4, status)
          endif
        endif
        if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_RouteRunF90PtrI411D

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_RouteRunF90PtrI421D"
!BOPI
! !IROUTINE: ESMF_RouteRunF90PtrI421D - Execute the communications the Route represents

! !INTERFACE:
      subroutine ESMF_RouteRunF90PtrI421D(route, srcarray, dstarray, rc)
!
! !ARGUMENTS:
      type(ESMF_Route), intent(inout) :: route
      integer(ESMF_KIND_I4), pointer, optional :: srcarray(:,:)
      integer(ESMF_KIND_I4), pointer, optional :: dstarray(:)
      integer, intent(out), optional :: rc            

!
! !DESCRIPTION:
!     Execute the communications this {\tt ESMF\_Route} represents.
!
!     The arguments are:
!     \begin{description}
!     \item[route] 
!          {\tt ESMF\_Route} to be executed.
!     \item[{[srcarray]}]
!          2D, Integer*4 Fortran array containing data to be sent.
!     \item[{[dstarray]}]
!          1D, Integer*4 Fortran array containing data to be received.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI

        ! local variables
        integer :: status                  ! local error status
        logical :: rcpresent               ! did user specify rc?
        type(ESMF_Pointer) :: empty

        ! Set initial values
        status = ESMF_RC_NOT_IMPL
        rcpresent = .FALSE.   
        empty = ESMF_NULL_POINTER

        ! Initialize return code; assume failure until success is certain
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_RC_NOT_IMPL
        endif

	! Check initialization
        ESMF_INIT_CHECK_DEEP(ESMF_RouteGetInit,route,rc)

        ! Call C++  code
        if (associated(srcarray)) then
          if (associated(dstarray)) then
            call c_ESMC_RouteRunNA(route, srcarray, dstarray, ESMF_TYPEKIND_I4, status)
          else
            call c_ESMC_RouteRunNA(route, srcarray, empty, ESMF_TYPEKIND_I4, status)
          endif
        else
          if (associated(dstarray)) then
            call c_ESMC_RouteRunNA(route, empty, dstarray, ESMF_TYPEKIND_I4, status)
          else
            call c_ESMC_RouteRunNA(route, empty, empty, ESMF_TYPEKIND_I4, status)
          endif
        endif
        if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_RouteRunF90PtrI421D

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_RouteRunF90PtrR811D"
!BOPI
! !IROUTINE: ESMF_RouteRunF90PtrR811D - Execute the communications the Route represents

! !INTERFACE:
      subroutine ESMF_RouteRunF90PtrR811D(route, srcarray, dstarray, rc)
!
! !ARGUMENTS:
      type(ESMF_Route), intent(inout) :: route
      real(ESMF_KIND_R8), pointer, optional :: srcarray(:)
      real(ESMF_KIND_R8), pointer, optional :: dstarray(:)
      integer, intent(out), optional :: rc            

!
! !DESCRIPTION:
!     Execute the communications this {\tt ESMF\_Route} represents.
!
!     The arguments are:
!     \begin{description}
!     \item[route] 
!          {\tt ESMF\_Route} to be executed.
!     \item[{[srcarray]}]
!          1D, Real*8 Fortran array containing data to be sent.
!     \item[{[dstarray]}]
!          1D, Real*8 Fortran array containing data to be received.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI

        ! local variables
        integer :: status                  ! local error status
        logical :: rcpresent               ! did user specify rc?
        type(ESMF_Pointer) :: empty

        ! Set initial values
        status = ESMF_RC_NOT_IMPL
        rcpresent = .FALSE.   
        empty = ESMF_NULL_POINTER

        ! Initialize return code; assume failure until success is certain
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_RC_NOT_IMPL
        endif

	! Check initialization
        ESMF_INIT_CHECK_DEEP(ESMF_RouteGetInit,route,rc)

        ! Call C++  code
        if (associated(srcarray)) then
          if (associated(dstarray)) then
            call c_ESMC_RouteRunNA(route, srcarray, dstarray, ESMF_TYPEKIND_R8, status)
          else
            call c_ESMC_RouteRunNA(route, srcarray, empty, ESMF_TYPEKIND_R8, status)
          endif
        else
          if (associated(dstarray)) then
            call c_ESMC_RouteRunNA(route, empty, dstarray, ESMF_TYPEKIND_R8, status)
          else
            call c_ESMC_RouteRunNA(route, empty, empty, ESMF_TYPEKIND_R8, status)
          endif
        endif
        if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_RouteRunF90PtrR811D

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_RouteRunF90PtrR821D"
!BOPI
! !IROUTINE: ESMF_RouteRunF90PtrR821D - Execute the communications the Route represents

! !INTERFACE:
      subroutine ESMF_RouteRunF90PtrR821D(route, srcarray, dstarray, rc)
!
! !ARGUMENTS:
      type(ESMF_Route), intent(inout) :: route
      real(ESMF_KIND_R8), pointer, optional :: srcarray(:,:)
      real(ESMF_KIND_R8), pointer, optional :: dstarray(:)
      integer, intent(out), optional :: rc            

!
! !DESCRIPTION:
!     Execute the communications this {\tt ESMF\_Route} represents.
!
!     The arguments are:
!     \begin{description}
!     \item[route] 
!          {\tt ESMF\_Route} to be executed.
!     \item[{[srcarray]}]
!          2D, Real*8 Fortran array containing data to be sent.
!     \item[{[dstarray]}]
!          1D, Real*8 Fortran array containing data to be received.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI

        ! local variables
        integer :: status                  ! local error status
        logical :: rcpresent               ! did user specify rc?
        type(ESMF_Pointer) :: empty

        ! Set initial values
        status = ESMF_RC_NOT_IMPL
        rcpresent = .FALSE.   
        empty= ESMF_NULL_POINTER

        ! Initialize return code; assume failure until success is certain
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_RC_NOT_IMPL
        endif

	! Check initialization
        ESMF_INIT_CHECK_DEEP(ESMF_RouteGetInit,route,rc)


        ! Call C++  code
        if (associated(srcarray)) then
          if (associated(dstarray)) then
            call c_ESMC_RouteRunNA(route, srcarray, dstarray, ESMF_TYPEKIND_R8, status)
          else
            call c_ESMC_RouteRunNA(route, srcarray, empty, ESMF_TYPEKIND_R8, status)
          endif
        else
          if (associated(dstarray)) then
            call c_ESMC_RouteRunNA(route, empty, dstarray, ESMF_TYPEKIND_R8, status)
          else
            call c_ESMC_RouteRunNA(route, empty, empty, ESMF_TYPEKIND_R8, status)
          endif
        endif
        if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_RouteRunF90PtrR821D

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_RouteRunF90PtrR832D"
!BOPI
! !IROUTINE: ESMF_RouteRunF90PtrR832D - Execute the communications the Route represents
!
! !INTERFACE:
      subroutine ESMF_RouteRunF90PtrR832D(route, srcarray, dstarray, rc)
!
! !ARGUMENTS:
      type(ESMF_Route), intent(inout) :: route
      real(ESMF_KIND_R8), pointer, optional :: srcarray(:,:,:)
      real(ESMF_KIND_R8), pointer, optional :: dstarray(:,:)
      integer, intent(out), optional :: rc            

!
! !DESCRIPTION:
!     Execute the communications this {\tt ESMF\_Route} represents.
!
!     The arguments are:
!     \begin{description}
!     \item[route] 
!          {\tt ESMF\_Route} to be executed.
!     \item[{[srcarray]}]
!          3D, Real*8 Fortran array containing data to be sent.
!     \item[{[dstarray]}]
!          2D, Real*8 Fortran array containing data to be received.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI

        ! local variables
        integer :: status                  ! local error status
        logical :: rcpresent               ! did user specify rc?
        type(ESMF_Pointer) :: empty

        ! Set initial values
        status = ESMF_RC_NOT_IMPL
        rcpresent = .FALSE.   
        empty = ESMF_NULL_POINTER

        ! Initialize return code; assume failure until success is certain
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_RC_NOT_IMPL
        endif

	! Check initialization
        ESMF_INIT_CHECK_DEEP(ESMF_RouteGetInit,route,rc)

        ! Call C++  code
        if (associated(srcarray)) then
          if (associated(dstarray)) then
            call c_ESMC_RouteRunNA(route, srcarray, dstarray, ESMF_TYPEKIND_R8, status)
          else
            call c_ESMC_RouteRunNA(route, srcarray, empty, ESMF_TYPEKIND_R8, status)
          endif
        else
          if (associated(dstarray)) then
            call c_ESMC_RouteRunNA(route, empty, dstarray, ESMF_TYPEKIND_R8, status)
          else
            call c_ESMC_RouteRunNA(route, empty, empty, ESMF_TYPEKIND_R8, status)
          endif
        endif
        if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_RouteRunF90PtrR832D

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_RouteValidate"
!BOPI
! !IROUTINE: ESMF_RouteValidate - Check internal consistency of a Route

! !INTERFACE:
      subroutine ESMF_RouteValidate(route, srcbufcount, srcbufsizes, &
                                           dstbufcount, dstbufsizes, &
                                           options, rc)
!
! !ARGUMENTS:
      type(ESMF_Route), intent(inout) :: route       
      integer, intent(in), optional :: srcbufcount            
      integer, intent(in), optional :: srcbufsizes(:)
      integer, intent(in), optional :: dstbufcount            
      integer, intent(in), optional :: dstbufsizes(:)
      character (len=*), intent(in), optional :: options    
      integer, intent(out), optional :: rc            
!
! !DESCRIPTION:
!     Validates that a {\tt ESMF\_Route} is internally consistent.
!
!     The arguments are:
!     \begin{description}
!     \item[route] 
!          {\tt ESMF\_Route} to be verified.
!     \item[{[srcbufcount]}]
!          Integer count of local buffers on this DE to do more advanced
!          error checking.  Defaults to 1.
!     \item[{[srcbufsizes]}]
!          Integer array of buffer item sizes, for more advanced error checking.
!          No default; if not specified, the advanced error checking will
!          be skipped.
!     \item[{[dstbufcount]}]
!          Integer count of local buffers on this DE to do more advanced
!          error checking.  Defaults to 1.
!     \item[{[dstbufsizes]}]
!          Integer array of buffer item sizes, for more advanced error checking.
!          No default; if not specified, the advanced error checking will
!          be skipped.
!     \item[{[options]}]
!          Validation options.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI

       character (len=ESMF_MAXSTR) :: optstring
       integer :: srcnumbufs, dstnumbufs
       ! TODO: this needs to be allocatable.  use fixed value for now.
       !integer, allocatable :: srcitemcounts(:), dstitemcounts(:)
       integer :: srcitemcounts(1), dstitemcounts(1)
       integer :: status

       ! Initialize return code; assume failure until success is certain       
       if (present(rc)) rc = ESMF_RC_NOT_IMPL

       ESMF_INIT_CHECK_DEEP(ESMF_RouteGetInit,route,rc)

       ! Set the source defaults if not specified
       if (present(srcbufcount)) then
           srcnumbufs = srcbufcount
       else
           srcnumbufs = 0
       endif
      
       if (present(srcbufsizes)) then
           srcitemcounts(1) = srcbufsizes(1)
       else
           srcitemcounts(1) = 0
       endif

       ! Set the defaults if not specified
       if (present(dstbufcount)) then
           dstnumbufs = dstbufcount
       else
           dstnumbufs = 0
       endif
      
       if (present(dstbufsizes)) then
           dstitemcounts(1) = dstbufsizes(1)
       else
           dstitemcounts(1) = 0
       endif

       if (present(options)) then
           optstring = options
       else
           optstring = "quick"
       endif

       ! now make the call
       call c_ESMC_RouteValidate(route, srcnumbufs, srcitemcounts, &
                                 dstnumbufs, dstitemcounts, optstring, status)


       if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

       ! Set return values
       if (present(rc)) rc = ESMF_SUCCESS

       end subroutine ESMF_RouteValidate

!------------------------------------------------------------------------------

       end module ESMF_RouteMod
