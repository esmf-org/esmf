! $Id: ESMF_RHandle.F90,v 1.6 2003/08/29 20:33:22 nscollins Exp $
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
!     ESMF RHandle Module
      module ESMF_RHandleMod
!
!==============================================================================
!
! This file contains the F90 wrapper code for the C++ implementation of
!  the RouteHandle class.
!
!------------------------------------------------------------------------------
! INCLUDES
#include "ESMF.h"
#include "ESMF_Route.h"
!==============================================================================
!BOP
! !MODULE: ESMF_RHandleMod - One line general statement about this class
!
! !DESCRIPTION:
!
! The code in this file implements the F90 wrapper code for the C++
!  implementation of the {\tt ESMF\_RouteHandle} class ...
!
! < Insert a paragraph or two explaining the function of this class. >
!
!------------------------------------------------------------------------------
! !USES:
      use ESMF_BaseMod
      use ESMF_LocalArrayMod
      use ESMF_RouteMod    
      implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
      private

!------------------------------------------------------------------------------
!     !  ESMF_TransformValues
!
!     ! Description of ESMF_TransformValues      
!     !  Could be a src x dst sparse matrix, but instead
!     !   this stores explicitly the source index, destination
!     !   index, and weighting factor.  

      type ESMF_TransformValues      
      sequence
        integer :: numlinks
        ! TODO: some domain list or count array needed here...
        !    decide what is really needed - this is just a guess for now.
        integer, dimension(:), pointer :: countsperdomain
        type (ESMF_LocalArray) :: srcindex             
        type (ESMF_LocalArray) :: dstindex        
        type (ESMF_LocalArray) :: weights
      end type

!------------------------------------------------------------------------------
!     !  ESMF_RouteHandleType
!
      integer, parameter :: ESMF_HALOHANDLE=1, ESMF_REDISTHANDLE=2, &
                            ESMF_REGRIDHANDLE=3

!------------------------------------------------------------------------------
!     !  ESMF_RouteHandle
!
!     ! Description of ESMF_RouteHandle. 

      type ESMF_RouteHandle
      sequence
      private
        type(ESMF_Pointer) :: this    ! opaque pointer to C++ class data
      end type

!------------------------------------------------------------------------------
! !PUBLIC TYPES:
      public ESMF_TransformValues
      public ESMF_RouteHandle
      public ESMF_HALOHANDLE, ESMF_REDISTHANDLE, ESMF_REGRIDHANDLE

!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:
!
!  Pick one or the other of the init/create sections depending on
!  whether this is a deep class (the class/derived type has pointers to
!  other memory which must be allocated/deallocated) or a shallow class
!  (the class/derived type is self-contained) and needs no destroy methods
!  other than deleting the memory for the object/derived type itself.

! the following routines apply to deep classes only
      public ESMF_RouteHandleCreate                 ! interface only, deep class
      public ESMF_RouteHandleDestroy                ! interface only, deep class

      public ESMF_RouteHandleGet                    ! get and set values
      public ESMF_RouteHandleSet
 
      public ESMF_RouteHandleValidate
      public ESMF_RouteHandlePrint
 
! < list the rest of the public interfaces here >
!
!
!EOP

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_RHandle.F90,v 1.6 2003/08/29 20:33:22 nscollins Exp $'

!==============================================================================

      contains

!==============================================================================
!
! This section includes the RouteHandle Create and Destroy methods.
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_RouteHandleCreate - Create a new RouteHandle

! !INTERFACE:
      function ESMF_RouteHandleCreate(rc)
!
! !RETURN VALUE:
      type(ESMF_RouteHandle) :: ESMF_RouteHandleCreate
!
! !ARGUMENTS:
      integer, intent(out), optional :: rc               
!
! !DESCRIPTION:
!     Allocates memory for a new {\tt RouteHandle} object and constructs its
!     internals.
!
!     The arguments are:
!     \begin{description}
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:  AAAn.n.n

        ! local variables
        type (ESMF_RouteHandle) :: rhandle     ! new C++ RouteHandle
        integer :: status                  ! local error status
        logical :: rcpresent               ! did user specify rc?

        ! Set initial values
        status = ESMF_FAILURE
        rcpresent = .FALSE.   
        rhandle%this = ESMF_NULL_POINTER

        ! Initialize return code; assume failure until success is certain
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
        endif

        ! Call C++ create code
        call c_ESMC_RouteHandleCreate(status)
        if (status .ne. ESMF_SUCCESS) then  
          print *, "RouteHandle create error"
          return  
        endif

        ! Set return values
        ESMF_RouteHandleCreate = rhandle

        if (rcpresent) rc = ESMF_SUCCESS

        end function ESMF_RouteHandleCreate

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_RouteHandleDestroy - Free all resources associated with a RouteHandle 

! !INTERFACE:
      subroutine ESMF_RouteHandleDestroy(rhandle, rc)
!
! !ARGUMENTS:
      type(ESMF_RouteHandle), intent(inout) :: rhandle   
      integer, intent(out), optional :: rc        
!
! !DESCRIPTION:
!     Destroys a {\tt RouteHandle} object previously allocated
!     via an {\tt ESMF_RouteHandleCreate routine}.
!
!     The arguments are:
!     \begin{description}
!     \item[rhandle] 
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
        call c_ESMC_RouteHandleDestroy(rhandle, status)
        if (status .ne. ESMF_SUCCESS) then  
          print *, "RouteHandle create error"
          return  
        endif

        ! nullify pointer
        rhandle%this = ESMF_NULL_POINTER

        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_RouteHandleDestroy


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_RouteHandleGet - Get values from a RouteHandle

! !INTERFACE:
      subroutine ESMF_RouteHandleGet(rhandle, htype, route1, route2, &
                                     tdata, label, rc)
!
! !ARGUMENTS:
      type(ESMF_RouteHandle), intent(in) :: rhandle
      integer, intent(out), optional :: htype
      type(ESMF_Route), intent(out), optional :: route1
      type(ESMF_Route), intent(out), optional :: route2
      type(ESMF_TransformValues), intent(out), optional :: tdata
      character(len=*), intent(out), optional :: label
      integer, intent(out), optional :: rc             

!
! !DESCRIPTION:
!     Returns the value of RouteHandle attribute <Value>.
!
!     The arguments are:
!     \begin{description}
!     \item[rhandle] 
!          Class to be queried.
!     \item[{[htype]}]
!          Value to be retrieved.         
!     \item[{[route1]}]
!          Value to be retrieved.         
!     \item[{[route2]}]
!          Value to be retrieved.         
!     \item[{[tdata]}]
!          Value to be retrieved.         
!     \item[{[label]}]
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
        integer :: oldhtype
        type(ESMF_Route) :: oldroute1
        type(ESMF_Route) :: oldroute2
        type(ESMF_TransformValues) :: oldtdata
        character(len=ESMF_MAXSTR) :: oldlabel

        ! Set initial values
        status = ESMF_FAILURE
        rcpresent = .FALSE.   

        ! Initialize return code; assume failure until success is certain
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
        endif

        ! Call C++  code
        ! TODO: handle label string going through the interface
        call c_ESMC_RouteHandleGet(rhandle, oldhtype, oldroute1, oldroute2, &
                                   oldtdata, status)
        if (status .ne. ESMF_SUCCESS) then  
          print *, "RouteHandle Get error"
          return  
        endif

        if (present(htype)) then
            htype = oldhtype    
        endif

        if (present(route1)) then
            route1 = oldroute1    
        endif

        if (present(route2)) then
            route2 = oldroute2    
        endif

        if (present(tdata)) then
            tdata = oldtdata    
        endif

        if (present(label)) then
            label = oldlabel    
        endif


        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_RouteHandleGet

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_RouteHandleSet - Set values in a RouteHandle

! !INTERFACE:
      subroutine ESMF_RouteHandleSet(rhandle, htype, route1, route2, &
                                     tdata, label, rc)
!
! !ARGUMENTS:
      type(ESMF_RouteHandle), intent(in) :: rhandle
      integer, intent(in), optional :: htype
      type(ESMF_Route), intent(in), optional :: route1
      type(ESMF_Route), intent(in), optional :: route2
      type(ESMF_TransformValues), intent(in), optional :: tdata
      character(len=*), intent(in), optional :: label
      integer, intent(out), optional :: rc            

!
! !DESCRIPTION:
!     Set a RouteHandle attribute with the given value.
!     May be multiple routines, one per attribute.
!
!     The arguments are:
!     \begin{description}
!     \item[rhandle] 
!          Class to be modified.
!     \item[{[htype]}]
!          Value to be set.         
!     \item[{[route1]}]
!          Value to be set.         
!     \item[{[route2]}]
!          Value to be set.         
!     \item[{[tdata]}]
!          Value to be set.         
!     \item[{[label]}]
!          Value to be set.         
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS: 

        ! local variables
        integer :: status                  ! local error status
        logical :: rcpresent               ! did user specify rc?
        logical :: changed
        integer :: oldhtype
        type(ESMF_Route) :: oldroute1
        type(ESMF_Route) :: oldroute2
        type(ESMF_TransformValues) :: oldtdata
        character(len=ESMF_MAXSTR) :: oldlabel

        ! Set initial values
        status = ESMF_FAILURE
        rcpresent = .FALSE.   

        ! Initialize return code; assume failure until success is certain
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
        endif

        ! Get old values and only replace the ones specified.
        call ESMF_RouteHandleGet(rhandle, oldhtype, oldroute1, oldroute2, &
                                 oldtdata, oldlabel, status)
        changed = .false.

        if (present(htype)) then
          changed = .true.
          oldhtype = htype
        endif

        if (present(route1)) then
          changed = .true.
          oldroute1 = route1
        endif

        if (present(route2)) then
          changed = .true.
          oldroute2 = route2
        endif

        if (present(tdata)) then
          changed = .true.
          oldtdata = tdata
        endif

        if (present(label)) then
          changed = .true.
          oldlabel = label
        endif

        ! Overwrite the changed values
        if (changed) then
            ! Call C++  code
            ! TODO: handle label string going through the interface
            call c_ESMC_RouteHandleSet(rhandle, oldhtype, oldroute1, &
                                       oldroute2, oldtdata, status)
            if (status .ne. ESMF_SUCCESS) then  
              print *, "RouteHandle Set error"
              return  
            endif
        endif

        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_RouteHandleSet

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_RouteHandleValidate - Check internal consistency of a RouteHandle

! !INTERFACE:
      subroutine ESMF_RouteHandleValidate(rhandle, options, rc)
!
! !ARGUMENTS:
      type(ESMF_RouteHandle), intent(in) :: rhandle       
      character (len=*), intent(in), optional :: options    
      integer, intent(out), optional :: rc            
!
! !DESCRIPTION:
!     Validates that a RouteHandle is internally consistent.
!
!     The arguments are:
!     \begin{description}
!     \item[rhandle] 
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
           call c_ESMC_RouteHandleValidate(rhandle, options, status)   
       else
           call c_ESMC_RouteHandleValidate(rhandle, defaultopts, status)
       endif

       if (status .ne. ESMF_SUCCESS) then
         print *, "RouteHandle validate error"
         return
       endif

       ! Set return values
       if (rcpresent) rc = ESMF_SUCCESS

       end subroutine ESMF_RouteHandleValidate

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_RouteHandlePrint - Print the contents of a RouteHandle

! !INTERFACE:
      subroutine ESMF_RouteHandlePrint(rhandle, options, rc)
!
! !ARGUMENTS:
      type(ESMF_RouteHandle), intent(in) :: rhandle      
      character (len=*), intent(in), optional :: options      
      integer, intent(out), optional :: rc           
!
! !DESCRIPTION:
!      Print information about a RouteHandle.  
!
!     The arguments are:
!     \begin{description}
!     \item[rhandle] 
!          Class to be queried.
!     \item[{[options]}]
!          Print options that control the type of information and level of 
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
           call c_ESMC_RouteHandlePrint(rhandle, options, status)   
       else
           call c_ESMC_RouteHandlePrint(rhandle, defaultopts, status)
       endif

       if (status .ne. ESMF_SUCCESS) then
         print *, "RouteHandle print error"
         return
       endif

       ! Set return values
       if (rcpresent) rc = ESMF_SUCCESS
 
       end subroutine ESMF_RouteHandlePrint

!------------------------------------------------------------------------------

       end module ESMF_RHandleMod
