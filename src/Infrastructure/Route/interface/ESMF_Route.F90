! $Id: ESMF_Route.F90,v 1.14 2003/04/29 21:37:47 nscollins Exp $
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
#include "ESMF_Route.h"
!==============================================================================
!BOP
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
      use ESMF_DELayoutMod
      use ESMF_ArrayMod
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

      public ESMF_RouteGetCached

      public ESMF_RoutePrecompute
      public ESMF_RoutePrecomputeHalo
      public ESMF_RouteRun
 
      public ESMF_RouteValidate
      public ESMF_RoutePrint
 
!
!
!EOP

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_Route.F90,v 1.14 2003/04/29 21:37:47 nscollins Exp $'

!==============================================================================
!
! INTERFACE BLOCKS
!
!==============================================================================
!BOP
! !INTERFACE:
!      interface ESMF_RouteCreate 

! !PRIVATE MEMBER FUNCTIONS:
!      module procedure ESMF_RouteCreateNew

! !DESCRIPTION:
!     This interface provides a single entry point for Route create
!     methods.
!
!EOP
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
!BOP
! !IROUTINE: ESMF_RouteCreate - Create a new Route

! !INTERFACE:
      function ESMF_RouteCreate(layout, rc)
!
! !RETURN VALUE:
      type(ESMF_Route) :: ESMF_RouteCreate
!
! !ARGUMENTS:
      type(ESMF_DELayout), intent(in) :: layout
      integer, intent(out), optional :: rc               
!
! !DESCRIPTION:
!     Allocates memory for a new {\tt Route} object and constructs its
!     internals.
!
!     The arguments are:
!     \begin{description}
!     \item[layout] 
!          A Layout object which encompasses all DEs involved in the
!          route operation.  This is the union of the layouts
!          for the source and destination Fields.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
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
        call c_ESMC_RouteCreate(route, layout, status)
        if (status .ne. ESMF_SUCCESS) then  
          print *, "Route create error"
          return  
        endif

        ! Set return values
        ESMF_RouteCreate = route

        if (rcpresent) rc = ESMF_SUCCESS

        end function ESMF_RouteCreate

!------------------------------------------------------------------------------
!BOP
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
!EOP
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
          print *, "Route create error"
          return  
        endif

        ! nullify pointer
        route%this = ESMF_NULL_POINTER

        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_RouteDestroy


!------------------------------------------------------------------------------
!BOP
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
!EOP
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
!BOP
! !IROUTINE: ESMF_RouteSetRecv - Set recv values in a Route

! !INTERFACE:
      subroutine ESMF_RouteSetRecv(Route, src_de, xp, rc)
!
! !ARGUMENTS:
      type(ESMF_Route), intent(in) :: route
      integer, intent(in) :: src_de
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
!     \item[src_de]
!          Source DE id.
!     \item[xp]
!          Exchange packet describing the data to be received.  Note that
!          an exchange packet only contains offsets and counts; the base
!          address will be specified at RouteRun time.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
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
        call c_ESMC_RouteSetRecv(route, src_de, xp, status)
        if (status .ne. ESMF_SUCCESS) then  
          print *, "Route Set error"
          return  
        endif

        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_RouteSetRecv

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_RouteSetSend - Set send values in a Route

! !INTERFACE:
      subroutine ESMF_RouteSetSend(route, dest_de, xp, rc)
!
! !ARGUMENTS:
      type(ESMF_Route), intent(inout) :: route
      integer, intent(in) :: dest_de
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
!     \item[dest_de]
!          Destination DE id.
!     \item[xp]
!          Exchange packet describing the data to be sent.  Note that
!          an exchange packet only contains offsets and counts; the base
!          address will be specified at RouteRun time.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
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
        call c_ESMC_RouteSetSend(route, dest_de, xp, status)
        if (status .ne. ESMF_SUCCESS) then  
          print *, "Route Set error"
          return  
        endif

        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_RouteSetSend

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_RouteGetCached - Search for a precomputed Route

! !INTERFACE:
      subroutine ESMF_RouteGetCached(rank, &
                       my_DE_dst, AI_dst, AI_dst_count, layout_dst, &
                       my_DE_src, AI_src, AI_src_count, layout_src, &
                       hascachedroute, route, rc)
!
! !ARGUMENTS:
      integer, intent(in) :: rank
      integer, intent(in) :: my_DE_dst
      type(ESMF_AxisIndex), dimension(:,:), pointer :: AI_dst
      integer, intent(in) :: AI_dst_count
      type(ESMF_DELayout), intent(in) :: layout_dst
      integer, intent(in) :: my_DE_src
      type(ESMF_AxisIndex), dimension(:,:), pointer :: AI_src
      integer, intent(in) :: AI_src_count
      type(ESMF_DELayout), intent(in) :: layout_src
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
!EOP
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
        do j=1,rank
          do i=1,AI_dst_count
            AI_dst(i,j)%l = AI_dst(i,j)%l - 1
            AI_dst(i,j)%r = AI_dst(i,j)%r - 1
          enddo
          do i=1,AI_src_count
            AI_src(i,j)%l = AI_src(i,j)%l - 1
            AI_src(i,j)%r = AI_src(i,j)%r - 1
          enddo
        enddo

        ! Call C++  code
        call c_ESMC_RouteGetCached(rank, &
                       my_DE_dst, AI_dst, AI_dst_count, layout_dst, &
                       my_DE_src, AI_src, AI_src_count, layout_src, &
                       lcache, lroute, rc)

        ! Translate AxisIndices back to  F90 from C++
        do j=1,rank
          do i=1,AI_dst_count
            AI_dst(i,j)%l = AI_dst(i,j)%l + 1
            AI_dst(i,j)%r = AI_dst(i,j)%r + 1
          enddo
          do i=1,AI_src_count
            AI_src(i,j)%l = AI_src(i,j)%l + 1
            AI_src(i,j)%r = AI_src(i,j)%r + 1
          enddo
        enddo

        if (status .ne. ESMF_SUCCESS) then  
          !print *, "Route Get Cached error"
          return  
        endif


        ! Set return values
        if (lcache .eq. ESMF_TF_TRUE) then
            hascachedroute = .true.
            route = lroute
        endif

        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_RouteGetCached

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_RouteRun - Execute the communications the Route represents

! !INTERFACE:
      subroutine ESMF_RouteRun(route, srcarray, dstarray, rc)
!
! !ARGUMENTS:
      type(ESMF_Route), intent(in) :: route
      type(ESMF_Array), intent(in), optional :: srcarray
      type(ESMF_Array), intent(in), optional :: dstarray
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
!          Local {\tt Array} containing data to be sent.
!     \item[{[dstarray]}]
!          Local {\tt Array} containing data to be received.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
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
        call c_ESMC_RouteRun(route, srcarray, dstarray, status)
        if (status .ne. ESMF_SUCCESS) then  
          print *, "Route Run error"
          return  
        endif

        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_RouteRun

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_RoutePrecompute - Precompute communication paths

! !INTERFACE:
      subroutine ESMF_RoutePrecompute(route, rank, &
               my_DE_dst, AI_dst_exc, AI_dst_tot, AI_dst_count, layout_dst, &
               my_DE_src, AI_src_exc, AI_src_tot, AI_src_count, layout_src, rc)

! !ARGUMENTS:
      type(ESMF_Route), intent(in) :: route
      integer, intent(in) :: rank
      integer, intent(in) :: my_DE_dst
      type(ESMF_AxisIndex), dimension(:,:), pointer :: AI_dst_exc
      type(ESMF_AxisIndex), dimension(:,:), pointer :: AI_dst_tot
      integer, intent(in) :: AI_dst_count
      type(ESMF_DELayout), intent(in) :: layout_dst
      integer, intent(in) :: my_DE_src
      type(ESMF_AxisIndex), dimension(:,:), pointer :: AI_src_exc
      type(ESMF_AxisIndex), dimension(:,:), pointer :: AI_src_tot
      integer, intent(in) :: AI_src_count
      type(ESMF_DELayout), intent(in) :: layout_src
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
!EOP
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
            AI_dst_exc(i,j)%l = AI_dst_exc(i,j)%l - 1
            AI_dst_exc(i,j)%r = AI_dst_exc(i,j)%r - 1
            AI_dst_tot(i,j)%l = AI_dst_tot(i,j)%l - 1
            AI_dst_tot(i,j)%r = AI_dst_tot(i,j)%r - 1
          enddo
          do i=1,AI_src_count
            AI_src_exc(i,j)%l = AI_src_exc(i,j)%l - 1
            AI_src_exc(i,j)%r = AI_src_exc(i,j)%r - 1
            AI_src_tot(i,j)%l = AI_src_tot(i,j)%l - 1
            AI_src_tot(i,j)%r = AI_src_tot(i,j)%r - 1
          enddo
        enddo

        ! Call C++  code
        call c_ESMC_RoutePrecompute(route, rank, &
           my_DE_dst, AI_dst_exc, AI_dst_tot, AI_dst_count, layout_dst, &
           my_DE_src, AI_src_exc, AI_src_tot, AI_src_count, layout_src, status)
        if (status .ne. ESMF_SUCCESS) then  
          print *, "Route Precompute error"
          ! don't return before adding 1 back to AIs
        endif

        ! Translate AxisIndices back to  F90 from C++
        do j=1,rank
          do i=1,AI_dst_count
            AI_dst_exc(i,j)%l = AI_dst_exc(i,j)%l + 1
            AI_dst_exc(i,j)%r = AI_dst_exc(i,j)%r + 1
            AI_dst_tot(i,j)%l = AI_dst_tot(i,j)%l + 1
            AI_dst_tot(i,j)%r = AI_dst_tot(i,j)%r + 1
          enddo
          do i=1,AI_src_count
            AI_src_exc(i,j)%l = AI_src_exc(i,j)%l + 1
            AI_src_exc(i,j)%l = AI_src_exc(i,j)%l + 1
            AI_src_tot(i,j)%r = AI_src_tot(i,j)%r + 1
            AI_src_tot(i,j)%r = AI_src_tot(i,j)%r + 1
          enddo
        enddo

        if (rcpresent) rc = status

        end subroutine ESMF_RoutePrecompute

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_RoutePrecomputeHalo - Precompute communication paths for a halo

! !INTERFACE:
      subroutine ESMF_RoutePrecomputeHalo(route, rank, &
                       my_DE, AI_exc, AI_tot, AI_count, layout, rc)

! !ARGUMENTS:
      type(ESMF_Route), intent(in) :: route
      integer, intent(in) :: rank
      integer, intent(in) :: my_DE
      type(ESMF_AxisIndex), dimension(:,:), pointer :: AI_exc
      type(ESMF_AxisIndex), dimension(:,:), pointer :: AI_tot
      integer, intent(in) :: AI_count
      type(ESMF_DELayout), intent(in) :: layout
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
!EOP
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
            AI_exc(i,j)%l = AI_exc(i,j)%l - 1
            AI_exc(i,j)%r = AI_exc(i,j)%r - 1
            AI_tot(i,j)%l = AI_tot(i,j)%l - 1
            AI_tot(i,j)%r = AI_tot(i,j)%r - 1
          enddo
        enddo

        ! Call C++  code
        call c_ESMC_RoutePrecomputeHalo(route, rank, my_DE, AI_exc, AI_tot, &
                                        AI_count, layout, status)
        if (status .ne. ESMF_SUCCESS) then  
          print *, "Route Precompute Halo error"
          ! don't return before adding 1 back to AIs
        endif

        ! Translate AxisIndices back to  F90 from C++
        do j=1,rank
          do i=1,AI_count
            AI_exc(i,j)%l = AI_exc(i,j)%l + 1
            AI_exc(i,j)%r = AI_exc(i,j)%r + 1
            AI_tot(i,j)%l = AI_tot(i,j)%l + 1
            AI_tot(i,j)%r = AI_tot(i,j)%r + 1
          enddo
        enddo

        if (rcpresent) rc = status

        end subroutine ESMF_RoutePrecomputeHalo

!------------------------------------------------------------------------------
!BOP
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
!EOP
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
!BOP
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
!EOP
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
