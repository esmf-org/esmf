! $Id: ESMF_RHandle.F90,v 1.12 2003/09/25 16:25:42 jwolfe Exp $
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
!     !  Defined on the C++ side, this holds the indices and weights needed to
!     !  compute the data transformation from source to destination in a regrid.

      type ESMF_TransformValues      
      sequence
        type(ESMF_Pointer) :: this    ! opaque pointer to C++ class data
      end type

!------------------------------------------------------------------------------
!     !  ESMF_RouteHandleType
!
      integer, parameter :: ESMF_HALOHANDLE=1, ESMF_REDISTHANDLE=2, &
                            ESMF_REGRIDHANDLE=3

!------------------------------------------------------------------------------
!     !  ESMF_RouteHandle
!
!     ! Defined on the C++ side, this structure holds one or more routes and
!     ! the required transform values to perform a regrid.

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

! the following routines apply to deep classes only
      public ESMF_TransformValuesCreate             ! interface only, deep class
      public ESMF_TransformValuesDestroy            ! interface only, deep class

      public ESMF_TransformValuesGet                ! get and set values
      public ESMF_TransformValuesSet
 
      public ESMF_TransformValuesValidate
      public ESMF_TransformValuesPrint
 
      public ESMF_RouteHandleCreate                 ! interface only, deep class
      public ESMF_RouteHandleDestroy                ! interface only, deep class

      public ESMF_RouteHandleGet                    ! get and set values
      public ESMF_RouteHandleSet
 
      public ESMF_RouteHandleValidate
      public ESMF_RouteHandlePrint
 
!EOP

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_RHandle.F90,v 1.12 2003/09/25 16:25:42 jwolfe Exp $'

!==============================================================================

      contains

!==============================================================================
!
! This section includes the TransformValues methods.
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_TransformValuesCreate - Create a new TransformValues obj

! !INTERFACE:
      function ESMF_TransformValuesCreate(rc)
!
! !RETURN VALUE:
      type(ESMF_TransformValues) :: ESMF_TransformValuesCreate
!
! !ARGUMENTS:
      integer, intent(out), optional :: rc               
!
! !DESCRIPTION:
!     Allocates memory for a new {\tt TransformValues} object and constructs its
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
        type (ESMF_TransformValues) :: tv     ! new C++ TransformValues
        integer :: status                  ! local error status
        logical :: rcpresent               ! did user specify rc?

        ! Set initial values
        status = ESMF_FAILURE
        rcpresent = .FALSE.   
        tv%this = ESMF_NULL_POINTER

        ! Initialize return code; assume failure until success is certain
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
        endif

        ! Call C++ create code
        call c_ESMC_TransformValuesCreate(tv, status)
        if (status .ne. ESMF_SUCCESS) then  
          print *, "TransformValues create error"
          return  
        endif

        ! Set return values
        ESMF_TransformValuesCreate = tv

        if (rcpresent) rc = ESMF_SUCCESS

        end function ESMF_TransformValuesCreate

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_TransformValuesDestroy - Free resources associated with a TransformValues 

! !INTERFACE:
      subroutine ESMF_TransformValuesDestroy(tv, rc)
!
! !ARGUMENTS:
      type(ESMF_TransformValues), intent(inout) :: tv   
      integer, intent(out), optional :: rc        
!
! !DESCRIPTION:
!     Destroys a {\tt TransformValues} object previously allocated
!     via an {\tt ESMF_TransformValuesCreate} routine.
!
!     The arguments are:
!     \begin{description}
!     \item[tv] 
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
        call c_ESMC_TransformValuesDestroy(tv, status)
        if (status .ne. ESMF_SUCCESS) then  
          print *, "TransformValues create error"
          return  
        endif

        ! nullify pointer
        tv%this = ESMF_NULL_POINTER

        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_TransformValuesDestroy


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_TransformValuesGet - Get values from a TransformValues

! !INTERFACE:
      subroutine ESMF_TransformValuesGet(tv, numlist, srcindex, dstindex, &
                                         weights, rc)
!
! !ARGUMENTS:
      type(ESMF_TransformValues), intent(in) :: tv
      integer, intent(out), optional :: numlist
      type(ESMF_LocalArray), intent(out), optional :: srcindex
      type(ESMF_LocalArray), intent(out), optional :: dstindex
      type(ESMF_LocalArray), intent(out), optional :: weights
      integer, intent(out), optional :: rc             

!
! !DESCRIPTION:
!     Returns the requested parts of a {\tt TransformValues} type.
!
!     The arguments are:
!     \begin{description}
!     \item[tv] 
!          Class to be queried.
!     \item[{[numlist]}]
!          Value to be retrieved.         
!     \item[{[srcindex]}]
!          Value to be retrieved.         
!     \item[{[dstindex]}]
!          Value to be retrieved.         
!     \item[{[weights]}]
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
        integer :: curnumlist
        type(ESMF_LocalArray) :: cursrc
        type(ESMF_LocalArray) :: curdst
        type(ESMF_LocalArray) :: curweights

        ! Set initial values
        status = ESMF_FAILURE
        rcpresent = .FALSE.   

        ! Initialize return code; assume failure until success is certain
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
        endif

        ! Call C++  code to get all current values
        call c_ESMC_TransformValuesGet(tv, curnumlist, cursrc, &
                                       curdst, curweights, status)
        if (status .ne. ESMF_SUCCESS) then  
          print *, "TransformValues Get error"
          return  
        endif

        if (present(numlist)) then
            numlist = curnumlist    
        endif

        if (present(srcindex)) then
            srcindex = cursrc    
        endif

        if (present(dstindex)) then
            dstindex = curdst    
        endif

        if (present(weights)) then
            weights = curweights    
        endif

        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_TransformValuesGet

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_TransformValuesSet - Set values in a TransformValues

! !INTERFACE:
      subroutine ESMF_TransformValuesSet(tv, numlist, srcindex, dstindex, &
                                         weights, rc)
!
! !ARGUMENTS:
      type(ESMF_TransformValues), intent(in) :: tv
      integer, intent(in), optional :: numlist
      type(ESMF_LocalArray), intent(in), optional :: srcindex
      type(ESMF_LocalArray), intent(in), optional :: dstindex
      type(ESMF_LocalArray), intent(in), optional :: weights
      integer, intent(out), optional :: rc             

!
! !DESCRIPTION:
!     Sets the requested parts of a {\tt TransformValues} type.
!
!     The arguments are:
!     \begin{description}
!     \item[tv] 
!          Class to be changed.
!     \item[{[numlist]}]
!          Value to be set.         
!     \item[{[srcindex]}]
!          Value to be set.         
!     \item[{[dstindex]}]
!          Value to be set.         
!     \item[{[weights]}]
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
        integer :: curnumlist
        type(ESMF_LocalArray) :: cursrc
        type(ESMF_LocalArray) :: curdst
        type(ESMF_LocalArray) :: curweights

        ! Set initial values
        status = ESMF_FAILURE
        rcpresent = .FALSE.   

        ! Initialize return code; assume failure until success is certain
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
        endif

        ! Get old values and only replace the ones specified.
        call c_ESMC_TransformValuesGet(tv, curnumlist, cursrc, &
                                       curdst, curweights, status)
        changed = .false.

        if (present(numlist)) then
          changed = .true.
          curnumlist = numlist    
        endif

        if (present(srcindex)) then
          changed = .true.
          cursrc = srcindex    
        endif

        if (present(dstindex)) then
          changed = .true.
          curdst = dstindex    
        endif

        if (present(weights)) then
          changed = .true.
          curweights = weights    
        endif

        ! Overwrite the changed values
        if (changed) then
            ! Call C++  code
            call c_ESMC_TransformValuesSet(tv, curnumlist, cursrc, &
                                           curdst, curweights, status)
            if (status .ne. ESMF_SUCCESS) then  
              print *, "TransformValues Set error"
              return  
            endif
        endif

        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_TransformValuesSet

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_TransformValuesValidate - Check internal consistency of a TransformValues

! !INTERFACE:
      subroutine ESMF_TransformValuesValidate(tv, options, rc)
!
! !ARGUMENTS:
      type(ESMF_TransformValues), intent(in) :: tv       
      character (len=*), intent(in), optional :: options    
      integer, intent(out), optional :: rc            
!
! !DESCRIPTION:
!     Validates that a TransformValues is internally consistent.
!
!     The arguments are:
!     \begin{description}
!     \item[tv] 
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
           call c_ESMC_TransformValuesValidate(tv, options, status)   
       else
           call c_ESMC_TransformValuesValidate(tv, defaultopts, status)
       endif

       if (status .ne. ESMF_SUCCESS) then
         print *, "TransformValues validate error"
         return
       endif

       ! Set return values
       if (rcpresent) rc = ESMF_SUCCESS

       end subroutine ESMF_TransformValuesValidate

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_TransformValuesPrint - Print the contents of a TransformValues

! !INTERFACE:
      subroutine ESMF_TransformValuesPrint(tv, options, rc)
!
! !ARGUMENTS:
      type(ESMF_TransformValues), intent(in) :: tv      
      character (len=*), intent(in), optional :: options      
      integer, intent(out), optional :: rc           
!
! !DESCRIPTION:
!      Print information about a TransformValues.  
!
!     The arguments are:
!     \begin{description}
!     \item[tv] 
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
           call c_ESMC_TransformValuesPrint(tv, options, status)   
       else
           call c_ESMC_TransformValuesPrint(tv, defaultopts, status)
       endif

       if (status .ne. ESMF_SUCCESS) then
         print *, "TransformValues print error"
         return
       endif

       ! Set return values
       if (rcpresent) rc = ESMF_SUCCESS
 
       end subroutine ESMF_TransformValuesPrint

!------------------------------------------------------------------------------

!==============================================================================
!
! This section includes the RouteHandle methods.
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
        call c_ESMC_RouteHandleCreate(rhandle, status)
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
        oldlabel = "fake"  ! not handing strings thru interface yet
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
        call c_ESMC_RouteHandleGet(rhandle, oldhtype, oldroute1, oldroute2, &
                                   oldtdata, status)
        oldlabel = "fake"  ! not handling strings thru the interface yet
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
