! $Id: ESMF_Route.F90,v 1.51 2004/04/20 22:53:25 nscollins Exp $
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
!!#include "ESMF_Route.h"
!==============================================================================
!BOPI
! !MODULE: ESMF_RouteMod - One line general statement about this class
!
! !DESCRIPTION:
!
! The code in this file implements the F90 wrapper code for the C++
!  implementation of the {\tt Route} class.
!
!------------------------------------------------------------------------------
! !USES:
      use ESMF_BaseMod    ! ESMF base class
      use ESMF_newDELayoutMod    ! ESMF layout class
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
      end type

!------------------------------------------------------------------------------
! !PUBLIC TYPES:
      public ESMF_Route
!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:
!
! the following routines apply to deep classes only
      public ESMF_RouteCreate                 ! interface only, deep class
      public ESMF_RouteDestroy                ! interface only, deep class

      !public ESMF_RouteGet                    ! get and set values
      public ESMF_RouteSetSend
      public ESMF_RouteSetRecv
      public ESMF_RouteSetRecvItems

      public ESMF_RouteGetCached
      public ESMF_RouteGetRecvItems

      public ESMF_RoutePrecomputeHalo
      public ESMF_RoutePrecomputeRedist
      public ESMF_RoutePrecomputeRegrid
      public ESMF_RoutePrecomputeDomList
      public ESMF_RouteRun
      public ESMF_RouteRunF90PtrI411D
      public ESMF_RouteRunF90PtrI421D
      public ESMF_RouteRunF90PtrR811D
      public ESMF_RouteRunF90PtrR821D
      public ESMF_RouteRunF90PtrR832D
 
      public ESMF_RouteValidate
      public ESMF_RoutePrint
 
!
!
!EOPI

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_Route.F90,v 1.51 2004/04/20 22:53:25 nscollins Exp $'

!==============================================================================
!
! INTERFACE BLOCKS
!
!==============================================================================
!BOPI
! !INTERFACE:
!      interface ESMF_RouteCreate 

! !PRIVATE MEMBER FUNCTIONS:
!      module procedure ESMF_RouteCreateNew

! !DESCRIPTION:
!     This interface provides a single entry point for Route create
!     methods.
!
!EOPI
!      end interface 
!
!------------------------------------------------------------------------------

!     < add other interfaces here>

!==============================================================================

      contains

!==============================================================================
!
! This section includes the Route Create and Destroy methods.
!
!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_RouteCreate - Create a new Route

! !INTERFACE:
      function ESMF_RouteCreate(delayout, rc)
!
! !RETURN VALUE:
      type(ESMF_Route) :: ESMF_RouteCreate
!
! !ARGUMENTS:
      type(ESMF_newDELayout), intent(in) :: delayout
      integer, intent(out), optional :: rc               
!
! !DESCRIPTION:
!     Allocates memory for a new {\tt Route} object and constructs its
!     internals.
!
!     The arguments are:
!     \begin{description}
!     \item[delayout] 
!          A Layout object which encompasses all DEs involved in the
!          route operation.  This is the union of the layouts
!          for the source and destination Fields.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS:  AAAn.n.n

        ! local variables
        type (ESMF_Route) :: route         ! new C++ Route
        integer :: status                  ! local error status
        logical :: rcpresent               ! did user specify rc?

        ! Set initial values
        status = ESMF_FAILURE
        rcpresent = .FALSE.   
        route%this = ESMF_NULL_POINTER

        ! Initialize return code; assume failure until success is certain
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
        endif

        ! Call C++ create code
        call c_ESMC_RouteCreate(route, delayout, status)
        if (status .ne. ESMF_SUCCESS) then  
          print *, "Route create error"
          return  
        endif

        ! Set return values
        ESMF_RouteCreate = route

        if (rcpresent) rc = ESMF_SUCCESS

        end function ESMF_RouteCreate

!------------------------------------------------------------------------------
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
!     Destroys a {\tt Route} object previously allocated
!     via an {\tt ESMF_RouteCreate routine}.
!
!     The arguments are:
!     \begin{description}
!     \item[route] 
!          The class to be destroyed.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS: 

        ! local variables
        integer :: status                  ! local error status
        logical :: rcpresent               ! did user specify rc?

        ! Set initial values
        status = ESMF_FAILURE
        rcpresent = .FALSE.   

        ! Initialize return code; assume failure until success is certain
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
        endif

        ! Call C++ destroy code
        call c_ESMC_RouteDestroy(route, status)
        if (status .ne. ESMF_SUCCESS) then  
          print *, "Route destroy error"
          return  
        endif

        ! nullify pointer
        route%this = ESMF_NULL_POINTER

        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_RouteDestroy


!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_RouteGet - Get values from a Route

! !INTERFACE:
      subroutine ESMF_RouteGet(route, value1, value2, rc)
!
! !ARGUMENTS:
      type(ESMF_Route), intent(in) :: route
      integer, intent(out), optional :: value1
      integer, intent(out), optional :: value2
      integer, intent(out), optional :: rc             

!
! !DESCRIPTION:
!     Returns the value of Route attribute <Value>.
!
!     The arguments are:
!     \begin{description}
!     \item[route] 
!          Class to be queried.
!     \item[{[value1]}]
!          Value to be retrieved.         
!     \item[{[value2]}]
!          Value to be retrieved.         
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS: 

        ! local variables
        integer :: status                  ! local error status
        logical :: rcpresent               ! did user specify rc?

        ! Set initial values
        status = ESMF_FAILURE
        rcpresent = .FALSE.   

        ! Initialize return code; assume failure until success is certain
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
        endif

        if (present(value1)) then
          ! code to be added here
        endif

        if (present(value2)) then
          ! code to be added here
        endif

        ! Call C++  code
!       call c_ESMC_RouteGet(route, value1, value2, status)
        if (status .ne. ESMF_SUCCESS) then  
          print *, "Route Get error"
          return  
        endif

        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_RouteGet


!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_RouteSetRecvItems - Set size of recv buf in #items

! !INTERFACE:
      subroutine ESMF_RouteSetRecvItems(Route, nitems, rc)
!
! !ARGUMENTS:
      type(ESMF_Route), intent(in) :: route
      integer, intent(in) :: nitems
      integer, intent(out), optional :: rc            

!
! !DESCRIPTION:
!     Set the required size of the receive buffer for a Route in
!     number of items (not in bytes).  In general a receive buffer is
!     already allocated, but if not, the caller can first query the route
!     for the size of the receive buffer, allocate it, and then call
!     {\tt RouteRun} to move the data.
!
!     The arguments are:
!     \begin{description}
!     \item[route] 
!          Route to be modified.
!     \item[nitems]
!          Size of the receive buffer for this route, in number of items.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS: 

        ! local variables
        integer :: status                  ! local error status
        logical :: rcpresent               ! did user specify rc?

        ! Set initial values
        status = ESMF_FAILURE
        rcpresent = .FALSE.   

        ! Initialize return code; assume failure until success is certain
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
        endif

        ! Call C++  code
        call c_ESMC_RouteSetRecvItems(route, nitems, status)
        if (status .ne. ESMF_SUCCESS) then  
          print *, "Route Set error"
          return  
        endif

        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_RouteSetRecvItems

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_RouteGetRecvItems - Get size of recv buf in #items

! !INTERFACE:
      subroutine ESMF_RouteGetRecvItems(Route, nitems, rc)
!
! !ARGUMENTS:
      type(ESMF_Route), intent(in) :: route
      integer, intent(out) :: nitems
      integer, intent(out), optional :: rc            

!
! !DESCRIPTION:
!     Get the required size of the receive buffer for a Route in
!     number of items (not in bytes).  In general a receive buffer is
!     already allocated, but if not, the caller can first query the route
!     for the size of the receive buffer, allocate it, and then call
!     {\tt RouteRun} to move the data.
!
!     The arguments are:
!     \begin{description}
!     \item[route] 
!          Route to be modified.
!     \item[nitems]
!          Size of the receive buffer for this route, in number of items.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS: 

        ! local variables
        integer :: status                  ! local error status
        logical :: rcpresent               ! did user specify rc?

        ! Set initial values
        status = ESMF_FAILURE
        rcpresent = .FALSE.   

        ! Initialize return code; assume failure until success is certain
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
        endif

        ! Call C++  code
        call c_ESMC_RouteGetRecvItems(route, nitems, status)
        if (status .ne. ESMF_SUCCESS) then  
          print *, "Route Get error"
          return  
        endif

        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_RouteGetRecvItems

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_RouteSetRecv - Set recv values in a Route

! !INTERFACE:
      subroutine ESMF_RouteSetRecv(Route, srcDE, xp, rc)
!
! !ARGUMENTS:
      type(ESMF_Route), intent(in) :: route
      integer, intent(in) :: srcDE
      type(ESMF_XPacket), intent(in) :: xp
      integer, intent(out), optional :: rc            

!
! !DESCRIPTION:
!     Set a Route attribute with the given value.
!
!     The arguments are:
!     \begin{description}
!     \item[route] 
!          Route to be modified.
!     \item[srcDE]
!          Source DE id.
!     \item[xp]
!          Exchange packet describing the data to be received.  Note that
!          an exchange packet only contains offsets and counts; the base
!          address will be specified at RouteRun time.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS: 

        ! local variables
        integer :: status                  ! local error status
        logical :: rcpresent               ! did user specify rc?

        ! Set initial values
        status = ESMF_FAILURE
        rcpresent = .FALSE.   

        ! Initialize return code; assume failure until success is certain
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
        endif

        ! Call C++  code
        call c_ESMC_RouteSetRecv(route, srcDE, xp, status)
        if (status .ne. ESMF_SUCCESS) then  
          print *, "Route Set error"
          return  
        endif

        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_RouteSetRecv

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_RouteSetSend - Set send values in a Route

! !INTERFACE:
      subroutine ESMF_RouteSetSend(route, destDE, xp, rc)
!
! !ARGUMENTS:
      type(ESMF_Route), intent(inout) :: route
      integer, intent(in) :: destDE
      type(ESMF_XPacket), intent(in) :: xp
      integer, intent(out), optional :: rc            

!
! !DESCRIPTION:
!     Set a Route attribute with the given value.
!     May be multiple routines, one per attribute.
!
!     The arguments are:
!     \begin{description}
!     \item[route] 
!          Route to be modified.
!     \item[destDE]
!          Destination DE id.
!     \item[xp]
!          Exchange packet describing the data to be sent.  Note that
!          an exchange packet only contains offsets and counts; the base
!          address will be specified at RouteRun time.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS: 

        ! local variables
        integer :: status                  ! local error status
        logical :: rcpresent               ! did user specify rc?

        ! Set initial values
        status = ESMF_FAILURE
        rcpresent = .FALSE.   

        ! Initialize return code; assume failure until success is certain
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
        endif

        ! Call C++  code
        call c_ESMC_RouteSetSend(route, destDE, xp, status)
        if (status .ne. ESMF_SUCCESS) then  
          print *, "Route Set error"
          return  
        endif

        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_RouteSetSend

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_RouteGetCached - Search for a precomputed Route

! !INTERFACE:
      subroutine ESMF_RouteGetCached(rank, &
                 my_DE_dst, AI_dst_exc, AI_dst_tot, AI_dst_count, dstDElayout, &
                 my_DE_src, AI_src_exc, AI_src_tot, AI_src_count, srcDElayout, &
                 periodic, hascachedroute, route, rc)
!
! !ARGUMENTS:
      integer, intent(in) :: rank
      integer, intent(in) :: my_DE_dst
      type(ESMF_AxisIndex), dimension(:,:), pointer :: AI_dst_exc
      type(ESMF_AxisIndex), dimension(:,:), pointer :: AI_dst_tot
      integer, intent(in) :: AI_dst_count
      type(ESMF_newDELayout), intent(in) :: dstDElayout
      integer, intent(in) :: my_DE_src
      type(ESMF_AxisIndex), dimension(:,:), pointer :: AI_src_exc
      type(ESMF_AxisIndex), dimension(:,:), pointer :: AI_src_tot
      integer, intent(in) :: AI_src_count
      type(ESMF_newDELayout), intent(in) :: srcDElayout
      type(ESMF_Logical), dimension(:), intent(in) :: periodic
      logical, intent(out) :: hascachedroute
      type(ESMF_Route), intent(out) :: route
      integer, intent(out), optional :: rc            
!
! !DESCRIPTION:
!     Search for an appropriate precomputed Route.
!
!     The arguments are:
!     \begin{description}
!     \item [ needs to be updated ]
!     \item[hascachedroute]
!          Logical return code for whether a {\tt Route} was found.
!     \item[route]
!          If found, the returned {\tt Route} object.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS: 

        ! local variables
        integer :: status                  ! local error status
        integer :: i,j                     ! counters
        logical :: rcpresent               ! did user specify rc?
        type(ESMF_Logical) :: lcache
        type(ESMF_Route) :: lroute

        ! Set initial values
        status = ESMF_FAILURE
        rcpresent = .FALSE.   
        hascachedroute = .FALSE.
        route%this = ESMF_NULL_POINTER

        ! Initialize return code; assume failure until success is certain
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
        endif

        ! Translate AxisIndices from F90 to C++
        ! TODO: fix this hack:
        !  The halo code sends in src=dst, so don't decrement
        !  twice in that case.  The AI arrays should really have
        !  an explicit flag saying which indexing scheme they
        !  are currently in, so we can just say "go to fortran order"
        !  or "go to C order" and if already there don't increment 
        !  or decrement too many times.
        do j=1,rank
          do i=1,AI_dst_count
            AI_dst_exc(i,j)%min = AI_dst_exc(i,j)%min - 1
            AI_dst_exc(i,j)%max = AI_dst_exc(i,j)%max - 1
            AI_dst_tot(i,j)%min = AI_dst_tot(i,j)%min - 1
            AI_dst_tot(i,j)%max = AI_dst_tot(i,j)%max - 1
          enddo
          if (.not. associated(AI_src_exc, AI_dst_exc)) then
            do i=1,AI_src_count
              AI_src_exc(i,j)%min = AI_src_exc(i,j)%min - 1
              AI_src_exc(i,j)%max = AI_src_exc(i,j)%max - 1
            enddo
          endif
          if (.not. associated(AI_src_tot, AI_dst_tot)) then
            do i=1,AI_src_count
              AI_src_tot(i,j)%min = AI_src_tot(i,j)%min - 1
              AI_src_tot(i,j)%max = AI_src_tot(i,j)%max - 1
            enddo
          endif
        enddo

        ! Call C++  code
        call c_ESMC_RouteGetCached(rank, &
               my_DE_dst, AI_dst_exc, AI_dst_tot, AI_dst_count, dstDElayout, &
               my_DE_src, AI_src_exc, AI_src_tot, AI_src_count, srcDElayout, &
               periodic, lcache, lroute, status)

        ! Translate AxisIndices back to  F90 from C++
        do j=1,rank
          do i=1,AI_dst_count
            AI_dst_exc(i,j)%min = AI_dst_exc(i,j)%min + 1
            AI_dst_exc(i,j)%max = AI_dst_exc(i,j)%max + 1
            AI_dst_tot(i,j)%min = AI_dst_tot(i,j)%min + 1
            AI_dst_tot(i,j)%max = AI_dst_tot(i,j)%max + 1
          enddo
          if (.not. associated(AI_src_exc, AI_dst_exc)) then
            do i=1,AI_src_count
              AI_src_exc(i,j)%min = AI_src_exc(i,j)%min + 1
              AI_src_exc(i,j)%max = AI_src_exc(i,j)%max + 1
            enddo
          endif
          if (.not. associated(AI_src_tot, AI_dst_tot)) then
            do i=1,AI_src_count
              AI_src_tot(i,j)%min = AI_src_tot(i,j)%min + 1
              AI_src_tot(i,j)%max = AI_src_tot(i,j)%max + 1
            enddo
          endif
        enddo

        if (status .ne. ESMF_SUCCESS) then  
          !print *, "Route Get Cached error"
            hascachedroute = .false.
            if (rcpresent) rc = ESMF_FAILURE
          return  
        endif


        ! Set return values
        if (lcache .eq. ESMF_TRUE) then
            hascachedroute = .true.
            route = lroute
        endif

        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_RouteGetCached

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_RouteRun - Execute the communications the Route represents

! !INTERFACE:
      subroutine ESMF_RouteRun(route, srcarray, dstarray, rc)
!
! !ARGUMENTS:
      type(ESMF_Route), intent(in) :: route
      type(ESMF_LocalArray), intent(in), optional :: srcarray
      type(ESMF_LocalArray), intent(in), optional :: dstarray
      integer, intent(out), optional :: rc            

!
! !DESCRIPTION:
!     Execute the communications a Route represents.
!
!     The arguments are:
!     \begin{description}
!     \item[route] 
!          Route to be executed.
!     \item[{[srcarray]}]
!          {\tt LocalArray} containing data to be sent.
!     \item[{[dstarray]}]
!          {\tt LocalArray} containing data to be received.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS: 

        ! local variables
        integer :: status                  ! local error status
        logical :: rcpresent               ! did user specify rc?

        ! Set initial values
        status = ESMF_FAILURE
        rcpresent = .FALSE.   

        ! Initialize return code; assume failure until success is certain
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
        endif

        ! Call C++  code
        call c_ESMC_RouteRunLA(route, srcarray, dstarray, status)
        if (status .ne. ESMF_SUCCESS) then  
          print *, "Route Run error"
          return  
        endif

        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_RouteRun

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_RouteRunF90PtrI411D - Execute the communications the Route represents

! !INTERFACE:
      subroutine ESMF_RouteRunF90PtrI411D(route, srcarray, dstarray, rc)
!
! !ARGUMENTS:
      type(ESMF_Route), intent(in) :: route
      integer(ESMF_KIND_I4), pointer, optional :: srcarray(:)
      integer(ESMF_KIND_I4), pointer, optional :: dstarray(:)
      integer, intent(out), optional :: rc            

!
! !DESCRIPTION:
!     Execute the communications a Route represents.
!
!     The arguments are:
!     \begin{description}
!     \item[route] 
!          Route to be executed.
!     \item[{[srcarray]}]
!          F90 Array containing data to be sent.
!     \item[{[dstarray]}]
!          F90 Array containing data to be received.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS: 

        ! local variables
        integer :: status                  ! local error status
        logical :: rcpresent               ! did user specify rc?

        ! Set initial values
        status = ESMF_FAILURE
        rcpresent = .FALSE.   

        ! Initialize return code; assume failure until success is certain
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
        endif

        ! Call C++  code
        call c_ESMC_RouteRunNA(route, srcarray, dstarray, ESMF_I4, status)
        if (status .ne. ESMF_SUCCESS) then  
          print *, "Route Run error"
          return  
        endif

        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_RouteRunF90PtrI411D

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_RouteRunF90PtrI421D - Execute the communications the Route represents

! !INTERFACE:
      subroutine ESMF_RouteRunF90PtrI421D(route, srcarray, dstarray, rc)
!
! !ARGUMENTS:
      type(ESMF_Route), intent(in) :: route
      integer(ESMF_KIND_I4), pointer, optional :: srcarray(:,:)
      integer(ESMF_KIND_I4), pointer, optional :: dstarray(:)
      integer, intent(out), optional :: rc            

!
! !DESCRIPTION:
!     Execute the communications a Route represents.
!
!     The arguments are:
!     \begin{description}
!     \item[route] 
!          Route to be executed.
!     \item[{[srcarray]}]
!          F90 Array containing data to be sent.
!     \item[{[dstarray]}]
!          F90 Array containing data to be received.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS: 

        ! local variables
        integer :: status                  ! local error status
        logical :: rcpresent               ! did user specify rc?

        ! Set initial values
        status = ESMF_FAILURE
        rcpresent = .FALSE.   

        ! Initialize return code; assume failure until success is certain
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
        endif

        ! Call C++  code
        call c_ESMC_RouteRunNA(route, srcarray, dstarray, ESMF_I4, status)
        if (status .ne. ESMF_SUCCESS) then  
          print *, "Route Run error"
          return  
        endif

        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_RouteRunF90PtrI421D

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_RouteRunF90PtrR811D - Execute the communications the Route represents

! !INTERFACE:
      subroutine ESMF_RouteRunF90PtrR811D(route, srcarray, dstarray, rc)
!
! !ARGUMENTS:
      type(ESMF_Route), intent(in) :: route
      real(ESMF_KIND_R8), pointer, optional :: srcarray(:)
      real(ESMF_KIND_R8), pointer, optional :: dstarray(:)
      integer, intent(out), optional :: rc            

!
! !DESCRIPTION:
!     Execute the communications a Route represents.
!
!     The arguments are:
!     \begin{description}
!     \item[route] 
!          Route to be executed.
!     \item[{[srcarray]}]
!          F90 Array containing data to be sent.
!     \item[{[dstarray]}]
!          F90 Array containing data to be received.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS: 

        ! local variables
        integer :: status                  ! local error status
        logical :: rcpresent               ! did user specify rc?

        ! Set initial values
        status = ESMF_FAILURE
        rcpresent = .FALSE.   

        ! Initialize return code; assume failure until success is certain
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
        endif

        ! Call C++  code
        call c_ESMC_RouteRunNA(route, srcarray, dstarray, ESMF_R8, status)
        if (status .ne. ESMF_SUCCESS) then  
          print *, "Route Run error"
          return  
        endif

        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_RouteRunF90PtrR811D

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_RouteRunF90PtrR821D - Execute the communications the Route represents

! !INTERFACE:
      subroutine ESMF_RouteRunF90PtrR821D(route, srcarray, dstarray, rc)
!
! !ARGUMENTS:
      type(ESMF_Route), intent(in) :: route
      real(ESMF_KIND_R8), pointer, optional :: srcarray(:,:)
      real(ESMF_KIND_R8), pointer, optional :: dstarray(:)
      integer, intent(out), optional :: rc            

!
! !DESCRIPTION:
!     Execute the communications a Route represents.
!
!     The arguments are:
!     \begin{description}
!     \item[route] 
!          Route to be executed.
!     \item[{[srcarray]}]
!          F90 Array containing data to be sent.
!     \item[{[dstarray]}]
!          F90 Array containing data to be received.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS: 

        ! local variables
        integer :: status                  ! local error status
        logical :: rcpresent               ! did user specify rc?

        ! Set initial values
        status = ESMF_FAILURE
        rcpresent = .FALSE.   

        ! Initialize return code; assume failure until success is certain
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
        endif

        ! Call C++  code
        call c_ESMC_RouteRunNA(route, srcarray, dstarray, ESMF_R8, status)
        if (status .ne. ESMF_SUCCESS) then  
          print *, "Route Run error"
          return  
        endif

        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_RouteRunF90PtrR821D

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_RouteRunF90PtrR832D - Execute the communications the Route represents
!
! !INTERFACE:
      subroutine ESMF_RouteRunF90PtrR832D(route, srcarray, dstarray, rc)
!
! !ARGUMENTS:
      type(ESMF_Route), intent(in) :: route
      real(ESMF_KIND_R8), pointer, optional :: srcarray(:,:,:)
      real(ESMF_KIND_R8), pointer, optional :: dstarray(:,:)
      integer, intent(out), optional :: rc            

!
! !DESCRIPTION:
!     Execute the communications a Route represents.
!
!     The arguments are:
!     \begin{description}
!     \item[route] 
!          Route to be executed.
!     \item[{[srcarray]}]
!          F90 Array containing data to be sent.
!     \item[{[dstarray]}]
!          F90 Array containing data to be received.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS: 

        ! local variables
        integer :: status                  ! local error status
        logical :: rcpresent               ! did user specify rc?

        ! Set initial values
        status = ESMF_FAILURE
        rcpresent = .FALSE.   

        ! Initialize return code; assume failure until success is certain
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
        endif

        ! Call C++  code
        call c_ESMC_RouteRunNA(route, srcarray, dstarray, ESMF_R8, status)
        if (status .ne. ESMF_SUCCESS) then  
          print *, "Route Run error"
          return  
        endif

        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_RouteRunF90PtrR832D

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_RoutePrecomputeHalo - Precompute communication paths for a halo

! !INTERFACE:
      subroutine ESMF_RoutePrecomputeHalo(route, rank, my_DE, AI_exc, AI_tot, &
                                          AI_count, global_start, global_count, &
                                          delayout, periodic, rc)

! !ARGUMENTS:
      type(ESMF_Route), intent(in) :: route
      integer, intent(in) :: rank
      integer, intent(in) :: my_DE
      type(ESMF_AxisIndex), dimension(:,:), pointer :: AI_exc
      type(ESMF_AxisIndex), dimension(:,:), pointer :: AI_tot
      integer, intent(in) :: AI_count
      integer, dimension(:,:), intent(in) :: global_start
      integer, dimension(ESMF_MAXGRIDDIM), intent(in) :: global_count
      type(ESMF_newDELayout), intent(in) :: delayout
      type(ESMF_Logical), intent(in) :: periodic(:)
      integer, intent(out), optional :: rc

!
! !DESCRIPTION:
!     Execute the communications a Route represents.
!
!     The arguments are:
!     \begin{description}
!     \item[route] 
!          Route to be executed.
!     \item[ TBDocd ]  
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS: 

        ! local variables
        integer :: status                  ! local error status
        integer :: i,j                     ! counters
        logical :: rcpresent               ! did user specify rc?

        ! Set initial values
        status = ESMF_FAILURE
        rcpresent = .FALSE.   

        ! Initialize return code; assume failure until success is certain
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
        endif

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
        if (status .ne. ESMF_SUCCESS) then  
          print *, "Route Precompute Halo error"
          ! don't return before adding 1 back to AIs
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
      subroutine ESMF_RoutePrecomputeRedist(route, rank, dstMyDE, dstCompAI, &
                                            dstTotalAI, dstGlobalStart, &
                                            dstGlobalCount, dstDElayout, &
                                            srcMyDE, srcCompAI, &
                                            srcTotalAI, srcGlobalStart, &
                                            srcGlobalCount, srcDElayout, rc)

! !ARGUMENTS:
      type(ESMF_Route), intent(in) :: route
      integer, intent(in) :: rank
      integer, intent(in) :: dstMyDE
      type(ESMF_AxisIndex), dimension(:,:), pointer :: dstCompAI
      type(ESMF_AxisIndex), dimension(:,:), pointer :: dstTotalAI
      integer, dimension(:,:), intent(in) :: dstGlobalStart
      integer, dimension(ESMF_MAXGRIDDIM), intent(in) :: dstGlobalCount
      type(ESMF_newDELayout), intent(in) :: dstDElayout
      integer, intent(in) :: srcMyDE
      type(ESMF_AxisIndex), dimension(:,:), pointer :: srcCompAI
      type(ESMF_AxisIndex), dimension(:,:), pointer :: srcTotalAI
      integer, dimension(:,:), intent(in) :: srcGlobalStart
      integer, dimension(ESMF_MAXGRIDDIM), intent(in) :: srcGlobalCount
      type(ESMF_newDELayout), intent(in) :: srcDElayout
      integer, intent(out), optional :: rc

!
! !DESCRIPTION:
!     Execute the communications a Route represents.
!
!     The arguments are:
!     \begin{description}
!     \item[route] 
!          Route to be executed.
!     \item[ TBDocd ]  
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS: 

        ! local variables
        integer :: status                  ! local error status
        logical :: rcpresent               ! did user specify rc?
        integer :: i,j                     ! counters
        integer :: dstAICount, srcAICount

        ! Set initial values
        status = ESMF_FAILURE
        rcpresent = .FALSE.   

        ! Initialize return code; assume failure until success is certain
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
        endif

        ! set some sizes to pass to C++ code
        dstAICount = size(dstCompAI,1)
        srcAICount = size(srcCompAI,1)

        ! Translate AxisIndices from F90 to C++
        do j   = 1,rank
          do i = 1,dstAICount
            dstCompAI(i,j)%min  =  dstCompAI(i,j)%min - 1
            dstCompAI(i,j)%max  =  dstCompAI(i,j)%max - 1
            dstTotalAI(i,j)%min = dstTotalAI(i,j)%min - 1
            dstTotalAI(i,j)%max = dstTotalAI(i,j)%max - 1
          enddo
          do i = 1,srcAICount
            srcCompAI(i,j)%min  =  srcCompAI(i,j)%min - 1
            srcCompAI(i,j)%max  =  srcCompAI(i,j)%max - 1
            srcTotalAI(i,j)%min = srcTotalAI(i,j)%min - 1
            srcTotalAI(i,j)%max = srcTotalAI(i,j)%max - 1
          enddo
        enddo

        ! Call C++  code
        call c_ESMC_RoutePrecomputeRedist(route, rank, &
                                          dstMyDE, dstCompAI, dstTotalAI, &
                                          dstAICount, dstGlobalStart, &
                                          dstGlobalCount, dstDElayout, &
                                          srcMyDE, srcCompAI, srcTotalAI, &
                                          srcAICount, srcGlobalStart, &
                                          srcGlobalCount, srcDElayout, status)
        if (status .ne. ESMF_SUCCESS) then  
          print *, "Route PrecomputeRedist error"
          ! don't return before adding 1 back to AIs
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

        end subroutine ESMF_RoutePrecomputeRedist

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
      type(ESMF_Route), intent(in) :: route
      integer, intent(in) :: rank
      integer, intent(in) :: my_DE_dst
      type(ESMF_AxisIndex), dimension(:,:), pointer :: AI_dst_exc
      type(ESMF_AxisIndex), dimension(:,:), pointer :: AI_dst_tot
      integer, intent(in) :: AI_dst_count
      integer, dimension(:,:), intent(in) :: dst_global_start
      integer, dimension(ESMF_MAXGRIDDIM), intent(in) :: dst_global_count
      type(ESMF_newDELayout), intent(in) :: dstDElayout
      integer, intent(in) :: my_DE_src
      type(ESMF_AxisIndex), dimension(:,:), pointer :: AI_src_exc
      type(ESMF_AxisIndex), dimension(:,:), pointer :: AI_src_tot
      integer, intent(in) :: AI_src_count
      integer, dimension(:,:), intent(in) :: src_global_start
      integer, dimension(ESMF_MAXGRIDDIM), intent(in) :: src_global_count
      type(ESMF_newDELayout), intent(in) :: srcDElayout
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Execute the communications a Route represents.
!
!     The arguments are:
!     \begin{description}
!     \item[route] 
!          Route to be executed.
!     \item[ TBDocd ]  
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS: 

        ! local variables
        integer :: status                  ! local error status
        integer :: i,j                     ! counters
        logical :: rcpresent               ! did user specify rc?

        ! Set initial values
        status = ESMF_FAILURE
        rcpresent = .FALSE.   

        ! Initialize return code; assume failure until success is certain
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
        endif

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
        if (status .ne. ESMF_SUCCESS) then  
          print *, "Route PrecomputeRegrid error"
          ! don't return before adding 1 back to AIs
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
!BOPI
! !IROUTINE: ESMF_RoutePrecomputeDomList - Precompute communication paths

! !INTERFACE:
      subroutine ESMF_RoutePrecomputeDomList(route, rank, my_DE, &
                                 sendDomainList, recvDomainList, rc)

! !ARGUMENTS:
      type(ESMF_Route), intent(in) :: route
      integer, intent(in) :: rank
      integer, intent(in) :: my_DE
      type(ESMF_DomainList) :: sendDomainList
      type(ESMF_DomainList) :: recvDomainList
      integer, intent(out), optional :: rc

!
! !DESCRIPTION:
!     Execute the communications a Route represents.
!
!     The arguments are:
!     \begin{description}
!     \item[route] 
!          Route to be executed.
!     \item[ TBDocd ]  
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS: 

        ! local variables
        integer :: status                  ! local error status
        integer :: i,j                     ! counters
        logical :: rcpresent               ! did user specify rc?

        ! Set initial values
        status = ESMF_FAILURE
        rcpresent = .FALSE.   

        ! Initialize return code; assume failure until success is certain
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
        endif

        ! Translate AxisIndices from F90 to C++
        do i = 1, recvDomainList%num_domains
          do j = 1, recvDomainList%domains(i)%rank
            recvDomainList%domains(i)%ai(j)%min = recvDomainList%domains(i)%ai(j)%min - 1
            recvDomainList%domains(i)%ai(j)%max = recvDomainList%domains(i)%ai(j)%max - 1
          enddo
        enddo
        do i = 1, sendDomainList%num_domains
          do j = 1, sendDomainList%domains(i)%rank
            sendDomainList%domains(i)%ai(j)%min = sendDomainList%domains(i)%ai(j)%min - 1
            sendDomainList%domains(i)%ai(j)%max = sendDomainList%domains(i)%ai(j)%max - 1
          enddo
        enddo

        ! Call C++  code
        call c_ESMC_RoutePrecomputeDomList(route, rank, my_DE, &
                               sendDomainList, recvDomainList, status)
        if (status .ne. ESMF_SUCCESS) then  
          print *, "Route PrecomputeDomainList error"
          ! don't return before adding 1 back to AIs
        endif

        ! Translate AxisIndices back to F90 from C++
        do i = 1, recvDomainList%num_domains
          do j = 1, recvDomainList%domains(i)%rank
            recvDomainList%domains(i)%ai(j)%min = recvDomainList%domains(i)%ai(j)%min + 1
            recvDomainList%domains(i)%ai(j)%max = recvDomainList%domains(i)%ai(j)%max + 1
          enddo
        enddo
        do i = 1, sendDomainList%num_domains
          do j = 1, sendDomainList%domains(i)%rank
            sendDomainList%domains(i)%ai(j)%min = sendDomainList%domains(i)%ai(j)%min + 1
            sendDomainList%domains(i)%ai(j)%max = sendDomainList%domains(i)%ai(j)%max + 1
          enddo
        enddo

        if (rcpresent) rc = status

        end subroutine ESMF_RoutePrecomputeDomList

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_RouteValidate - Check internal consistency of a Route

! !INTERFACE:
      subroutine ESMF_RouteValidate(route, options, rc)
!
! !ARGUMENTS:
      type(ESMF_Route), intent(in) :: route       
      character (len=*), intent(in), optional :: options    
      integer, intent(out), optional :: rc            
!
! !DESCRIPTION:
!     Validates that a Route is internally consistent.
!
!     The arguments are:
!     \begin{description}
!     \item[route] 
!          Class to be queried.
!     \item[{[options]}]
!          Validation options.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS:  XXXn.n, YYYn.n
!
       character (len=6) :: defaultopts      ! default validate options
       integer :: status                     ! local error status
       logical :: rcpresent

       ! Initialize return code; assume failure until success is certain       
       status = ESMF_FAILURE
       rcpresent = .FALSE.
       if (present(rc)) then
         rcpresent = .TRUE.  
         rc = ESMF_FAILURE
       endif

       defaultopts = "quick"

       if(present(options)) then
           call c_ESMC_RouteValidate(route, options, status)   
       else
           call c_ESMC_RouteValidate(route, defaultopts, status)
       endif

       if (status .ne. ESMF_SUCCESS) then
         print *, "Route validate error"
         return
       endif

       ! Set return values
       if (rcpresent) rc = ESMF_SUCCESS

       end subroutine ESMF_RouteValidate

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_RoutePrint - Print the contents of a Route

! !INTERFACE:
      subroutine ESMF_RoutePrint(route, options, rc)
!
! !ARGUMENTS:
      type(ESMF_Route), intent(in) :: route      
      character (len=*), intent(in), optional :: options      
      integer, intent(out), optional :: rc           
!
! !DESCRIPTION:
!      Print information about a Route.  
!
!     The arguments are:
!     \begin{description}
!     \item[route] 
!          Class to be queried.
!     \item[{[options]}]
!          Print ptions that control the type of information and level of 
!          detail.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS:  SSSn.n, GGGn.n

       character (len=6) :: defaultopts      ! default print options
       integer :: status                     ! local error status
       logical :: rcpresent

       ! Initialize return code; assume failure until success is certain       
       status = ESMF_FAILURE
       rcpresent = .FALSE.
       if (present(rc)) then
         rcpresent = .TRUE.  
         rc = ESMF_FAILURE
       endif

       defaultopts = "brief"

       if(present(options)) then
           call c_ESMC_RoutePrint(route, options, status)   
       else
           call c_ESMC_RoutePrint(route, defaultopts, status)
       endif

       if (status .ne. ESMF_SUCCESS) then
         print *, "Route print error"
         return
       endif

       ! Set return values
       if (rcpresent) rc = ESMF_SUCCESS
 
       end subroutine ESMF_RoutePrint

!------------------------------------------------------------------------------

       end module ESMF_RouteMod
