! $Id: ESMF_RHandle.F90,v 1.20 2004/06/07 05:21:09 nscollins Exp $
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
#define ESMF_FILENAME "ESMF_RHandle.F90"
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
!==============================================================================
!BOPI
! !MODULE: ESMF_RHandleMod - Manage RouteHandles which describe Routes
!
! !DESCRIPTION:
!
! The code in this file implements the F90 wrapper code for the C++
!  implementation of the {\tt ESMF\_RouteHandle} class.  This class
!  contains the handle information for precomputed information needed to
!  execute an {\tt ESMF\_Route} operation.
!
!------------------------------------------------------------------------------
! !USES:
      use ESMF_BaseMod
      use ESMF_LogErrMod
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
!     !  ESMF_TVWrapper
!
!     !  Simple derived types which allows us to pass the entire F90 dope 
!     !  vector across the language boundary uninterpreted.

      type ESMF_TVWrapperR8
      sequence
        real(ESMF_KIND_R8), dimension(:), pointer :: r8ptr
      end type

      type ESMF_TVWrapperI4
      sequence
        real(ESMF_KIND_I4), dimension(:), pointer :: i4ptr
      end type

!------------------------------------------------------------------------------
!     !  ESMF_RouteHandleType
!
      integer, parameter :: ESMF_HALOHANDLE=1, &
                            ESMF_REDISTHANDLE=2, &
                            ESMF_REGRIDHANDLE=3

!------------------------------------------------------------------------------
!     !  ESMF_RouteHandle
!
!     ! Defined on the C++ side, this structure holds one or more routes and
!     ! the required transform values to perform a regrid.

      type ESMF_RouteHandle
      sequence
      private
#ifndef ESMF_NO_INITIALIZERS
        type(ESMF_Pointer) :: this = ESMF_NULL_POINTER
#else
        type(ESMF_Pointer) :: this    ! opaque pointer to C++ class data
#endif
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

      public ESMF_TransformValuesCreate           ! interface only, deep class
      public ESMF_TransformValuesDestroy          ! interface only, deep class

      public ESMF_TransformValuesGet              ! get and set values
      public ESMF_TransformValuesGetF90Ptrs
      public ESMF_TransformValuesSet
 
      public ESMF_TransformValuesValidate
      public ESMF_TransformValuesPrint
 
      public ESMF_RouteHandleCreate               ! interface only, deep class
      public ESMF_RouteHandleDestroy              ! interface only, deep class

      public ESMF_RouteHandleGet                  ! get and set values
      public ESMF_RouteHandleSet
 
      public ESMF_RouteHandleValidate
      public ESMF_RouteHandlePrint
 
!EOPI

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_RHandle.F90,v 1.20 2004/06/07 05:21:09 nscollins Exp $'

!==============================================================================

      contains

!==============================================================================
!
! This section includes the TransformValues methods.
!
!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_TransformValuesCreate"
!BOPI
! !IROUTINE: ESMF_TransformValuesCreate - Create a new TransformValues obj

! !INTERFACE:
      function ESMF_TransformValuesCreate(count, rc)
!
! !RETURN VALUE:
      type(ESMF_TransformValues) :: ESMF_TransformValuesCreate
!
! !ARGUMENTS:
      integer, intent(in), optional :: count               
      integer, intent(out), optional :: rc               
!
! !DESCRIPTION:
!     Allocates memory for a new {\tt ESMF\_TransformValues} object 
!     and constructs its internals.
!
!     The arguments are:
!     \begin{description}
!     \item[{[count]}] 
!          Optional item count.  If set and greater than 0, the 
!          TransformValues object will return with space already allocated
!          to contain lists of this length.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI

        ! local variables
        type (ESMF_TransformValues) :: tv     ! new C++ TransformValues
        integer :: status                     ! local error status
        logical :: rcpresent                  ! did user specify rc?
        integer :: nitems

        ! Set initial values
        status = ESMF_FAILURE
        rcpresent = .FALSE.   
        tv%this = ESMF_NULL_POINTER

        ! Initialize return code; assume failure until success is certain
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
        endif

        ! Make sure you supply a default value
        if (present(count)) nitems = count
        if (.not.present(count)) nitems = 0

        ! Call C++ create code
        call c_ESMC_TransformValuesCreate(tv, nitems, status)
        if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

        ! Set return values
        ESMF_TransformValuesCreate = tv

        if (rcpresent) rc = ESMF_SUCCESS

        end function ESMF_TransformValuesCreate

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_TransformValuesDestroy"
!BOPI
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
!     via an {\tt ESMF_TransformValuesCreate()} routine.
!
!     The arguments are:
!     \begin{description}
!     \item[tv] 
!          The {\tt ESMF\_TransformValues} to be destroyed.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI

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
        if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

        ! nullify pointer
        tv%this = ESMF_NULL_POINTER

        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_TransformValuesDestroy


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_TransformValuesGet"
!BOPI
! !IROUTINE: ESMF_TransformValuesGet - Get values from a TransformValues

! !INTERFACE:
      subroutine ESMF_TransformValuesGet(tv, numList, srcIndex, dstIndex, &
                                         weights, rc)
!
! !ARGUMENTS:
      type(ESMF_TransformValues), intent(in) :: tv
      integer, intent(out), optional :: numList
      type(ESMF_LocalArray), intent(out), optional :: srcIndex
      type(ESMF_LocalArray), intent(out), optional :: dstIndex
      type(ESMF_LocalArray), intent(out), optional :: weights
      integer, intent(out), optional :: rc             

!
! !DESCRIPTION:
!     Returns the requested parts of an {\tt ESMF\_TransformValues} type.
!
!     The arguments are:
!     \begin{description}
!     \item[tv] 
!          {\tt ESMF\_TransformValues} to be queried.
!     \item[{[numList]}]
!          Value to be retrieved.         
!     \item[{[srcIndex]}]
!          Value to be retrieved.         
!     \item[{[dstIndex]}]
!          Value to be retrieved.         
!     \item[{[weights]}]
!          Value to be retrieved.         
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI

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
        if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

        if (present(numList)) then
            numList = curnumlist    
        endif

        if (present(srcIndex)) then
            srcIndex = cursrc    
        endif

        if (present(dstIndex)) then
            dstIndex = curdst    
        endif

        if (present(weights)) then
            weights = curweights    
        endif

        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_TransformValuesGet

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_TransformValuesGetF90Ptrs"
!BOPI
! !IROUTINE: ESMF_TransformValuesGetF90Ptrs - Get Fortran ptrs from TransformValues

! !INTERFACE:
      subroutine ESMF_TransformValuesGetF90Ptrs(tv, numList, srcIndex, &
                                                dstIndex, weights, rc)
!
! !ARGUMENTS:
      type(ESMF_TransformValues), intent(in) :: tv
      integer, intent(out), optional :: numList
      real(ESMF_KIND_I4), dimension(:), pointer, optional :: srcIndex
      real(ESMF_KIND_I4), dimension(:), pointer, optional :: dstIndex
      real(ESMF_KIND_R8), dimension(:), pointer, optional :: weights
      integer, intent(out), optional :: rc             

!
! !DESCRIPTION:
!     Returns the requested parts of a {\tt TransformValues} type.
!
!     The arguments are:
!     \begin{description}
!     \item[tv] 
!          {\tt ESMF\_TransformValues} to be queried.
!     \item[{[numList]}]
!          Value to be retrieved.         
!     \item[{[srcIndex]}]
!          Value to be retrieved.         
!     \item[{[dstIndex]}]
!          Value to be retrieved.         
!     \item[{[weights]}]
!          Value to be retrieved.         
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI

        ! local variables
        integer :: status                  ! local error status
        logical :: rcpresent               ! did user specify rc?
        integer :: curnumlist
        type(ESMF_TVWrapperI4) :: srcwrap
        type(ESMF_TVWrapperI4) :: dstwrap
        type(ESMF_TVWrapperR8) :: wwrap

        ! Set initial values
        status = ESMF_FAILURE
        rcpresent = .FALSE.   

        ! Initialize return code; assume failure until success is certain
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
        endif

        ! Call C++  code to get all current values
        call c_ESMC_TransformValuesGetF90Ptr(tv, curnumlist, srcwrap, &
                                             dstwrap, wwrap, status)
        if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

        if (present(numList)) then
            numList = curnumlist    
        endif

        if (present(srcIndex)) then
            srcIndex => srcwrap%i4ptr
        endif

        if (present(dstIndex)) then
            dstIndex => dstwrap%i4ptr
        endif

        if (present(weights)) then
            weights => wwrap%r8ptr
        endif

        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_TransformValuesGetF90Ptrs

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_TransformValueSet"
!BOPI
! !IROUTINE: ESMF_TransformValuesSet - Set values in a TransformValues

! !INTERFACE:
      subroutine ESMF_TransformValuesSet(tv, numList, srcIndex, dstIndex, &
                                         weights, rc)
!
! !ARGUMENTS:
      type(ESMF_TransformValues), intent(in) :: tv
      integer, intent(in), optional :: numList
      type(ESMF_LocalArray), intent(in), optional :: srcIndex
      type(ESMF_LocalArray), intent(in), optional :: dstIndex
      type(ESMF_LocalArray), intent(in), optional :: weights
      integer, intent(out), optional :: rc             

!
! !DESCRIPTION:
!     Sets the requested parts of an {\tt ESMF\_TransformValues} type.
!
!     The arguments are:
!     \begin{description}
!     \item[tv] 
!          {\tt ESMF\_TransformValues} to be changed.
!     \item[{[numList]}]
!          Value to be set.         
!     \item[{[srcIndex]}]
!          Value to be set.         
!     \item[{[dstIndex]}]
!          Value to be set.         
!     \item[{[weights]}]
!          Value to be set.         
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI

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

        if (present(numList)) then
          changed = .true.
          curnumlist = numList    
        endif

        if (present(srcIndex)) then
          changed = .true.
          cursrc = srcIndex    
        endif

        if (present(dstIndex)) then
          changed = .true.
          curdst = dstIndex    
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
            if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return
        endif

        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_TransformValuesSet

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_TransformValuesValidate"
!BOPI
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
!     Verifies that an {\tt ESMF\_TransformValues} is internally consistent.
!
!     The arguments are:
!     \begin{description}
!     \item[tv] 
!          {\tt ESMF\_TransformValues} to be validated.
!     \item[{[options]}]
!          Validation options.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI

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

        if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

       ! Set return values
       if (rcpresent) rc = ESMF_SUCCESS

       end subroutine ESMF_TransformValuesValidate

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_TransformValuesPrint"
!BOPI
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
!     Print information about an {\tt ESMF\_TransformValues}.  
!
!     The arguments are:
!     \begin{description}
!     \item[tv] 
!          {\tt ESMF\_TransformValues} to be queried.
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

        if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

       ! Set return values
       if (rcpresent) rc = ESMF_SUCCESS
 
       end subroutine ESMF_TransformValuesPrint

!------------------------------------------------------------------------------

!==============================================================================
!
! This section includes the RouteHandle methods.
!
!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_RouteHandleCreate"
!BOPI
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
!     Allocates memory for a new {\tt ESMF\_RouteHandle} object and 
!     constructs its internals.
!
!     The arguments are:
!     \begin{description}
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI

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
        if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

        ! Set return values
        ESMF_RouteHandleCreate = rhandle

        if (rcpresent) rc = ESMF_SUCCESS

        end function ESMF_RouteHandleCreate

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_RouteHandleDestroy"
!BOPI
! !IROUTINE: ESMF_RouteHandleDestroy - Free all resources associated with a RouteHandle 

! !INTERFACE:
      subroutine ESMF_RouteHandleDestroy(rhandle, rc)
!
! !ARGUMENTS:
      type(ESMF_RouteHandle), intent(inout) :: rhandle   
      integer, intent(out), optional :: rc        
!
! !DESCRIPTION:
!     Destroys an {\tt ESMF\_RouteHandle} object previously allocated
!     via an {\tt ESMF_RouteHandleCreate()} routine.
!
!     The arguments are:
!     \begin{description}
!     \item[rhandle] 
!          The {\tt ESMF\_RouteHandle} to be destroyed.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI

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
        if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

        ! nullify pointer
        rhandle%this = ESMF_NULL_POINTER

        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_RouteHandleDestroy


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_RouteHandleGet"
!BOPI
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
!     Returns information about an {\tt ESMF\_RouteHandle}.
!
!     The arguments are:
!     \begin{description}
!     \item[rhandle] 
!          {\tt ESMF\_RouteHandle} to be queried.
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
!EOPI

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
        if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

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
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_RouteHandleSet"
!BOPI
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
!     Set an {\tt ESMF\_RouteHandle} attribute with the given value.
!
!     The arguments are:
!     \begin{description}
!     \item[rhandle] 
!          {\tt ESMF\_RouteHandle} to be modified.
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
!EOPI

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
            if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return
        endif

        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_RouteHandleSet

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_RouteHandleValidate"
!BOPI
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
!     Validates that an {\tt ESMF\_RouteHandle} is internally consistent.
!
!     The arguments are:
!     \begin{description}
!     \item[rhandle] 
!          {\tt ESMF\_RouteHandle} to be queried.
!     \item[{[options]}]
!          Validation options.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI

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

       ! See if this has been created yet or not.
       if ((rhandle%this).eq.ESMF_NULL_POINTER) then
         if (present(rc)) rc = ESMF_FAILURE
         return
       endif

       if(present(options)) then
           call c_ESMC_RouteHandleValidate(rhandle, options, status)   
       else
           call c_ESMC_RouteHandleValidate(rhandle, defaultopts, status)
       endif

       if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

       ! Set return values
       if (rcpresent) rc = ESMF_SUCCESS

       end subroutine ESMF_RouteHandleValidate

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_RouteHandlePrint"
!BOPI
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
!          {\tt ESMF\_RouteHandle} to print contents of.
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

       if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

       ! Set return values
       if (rcpresent) rc = ESMF_SUCCESS
 
       end subroutine ESMF_RouteHandlePrint

!------------------------------------------------------------------------------

       end module ESMF_RHandleMod
