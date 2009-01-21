! $Id: ESMF_RHandle.F90,v 1.39.2.4 2009/01/21 21:25:23 cdeluca Exp $
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
      use ESMF_UtilTypesMod
      use ESMF_InitMacrosMod
      use ESMF_LogErrMod
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
	ESMF_INIT_DECLARE
      end type

!------------------------------------------------------------------------------
!     !  ESMF_TVWrapper
!
!     !  Simple derived types which allows us to pass the entire F90 dope 
!     !  vector across the language boundary uninterpreted.
!     !  Since these two types were not used as input arguments of a function
!     !  No initialization routines are added -- P.Li 12/3/2006

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
!     !  MUST STAY IN SYNC WITH C++ header file
!
      integer, parameter :: ESMF_HALOHANDLE=1, &
                            ESMF_REDISTHANDLE=2, &
                            ESMF_REGRIDHANDLE=3, &
                            ESMF_UNINITIALIZEDHANDLE=4, &
                            ESMF_ARRAYSPARSEMATMULHANDLE=5

!------------------------------------------------------------------------------
!     !  ESMF_RouteMappingType
!     !  MUST STAY IN SYNC WITH C++ header file
!
      integer, parameter :: ESMF_1TO1HANDLEMAP=1, &
                            ESMF_ALLTO1HANDLEMAP=2, &
                            ESMF_INDIRECTHANDLEMAP=3, &
                            ESMF_NOHANDLEMAP=4, &
                            ESMF_UNKNOWNHANDLEMAP=5

!------------------------------------------------------------------------------
!     !  ESMF_RouteHandle
!
!     ! Defined on the C++ side, this structure holds one or more routes and
!     ! the required transform values to perform a regrid.

      type ESMF_RouteHandle
      sequence
      private
        type(ESMF_Pointer) :: this    ! opaque pointer to C++ class data
	ESMF_INIT_DECLARE
      end type

!------------------------------------------------------------------------------
! !PUBLIC TYPES:
      public ESMF_TransformValues
      public ESMF_RouteHandle
      public ESMF_HALOHANDLE, ESMF_REDISTHANDLE, ESMF_REGRIDHANDLE, &
             ESMF_UNINITIALIZEDHANDLE
      public ESMF_1TO1HANDLEMAP, ESMF_ALLTO1HANDLEMAP, ESMF_INDIRECTHANDLEMAP, &
             ESMF_NOHANDLEMAP, ESMF_UNKNOWNHANDLEMAP

!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:
!

      public ESMF_TransformValuesGetInit

      public ESMF_TransformValuesCreate           ! interface only, deep class
      public ESMF_TransformValuesDestroy          ! interface only, deep class

      public ESMF_TransformValuesGet              ! get and set values
      public ESMF_TransformValuesGetF90Ptrs
      public ESMF_TransformValuesSet
 
      public ESMF_TransformValuesValidate
      public ESMF_TransformValuesPrint
 
      public ESMF_RouteHandleGetInit
      public ESMF_RouteHandleSetInitCreated

      public ESMF_RouteHandleCreate               ! interface only, deep class
      public ESMF_RouteHandleDestroy              ! interface only, deep class
      
      public ESMF_RouteHandleRelease

      public ESMF_RouteHandleGet                  ! get and set values
      public ESMF_RouteHandleSet
 
      public ESMF_RouteHandleValidate
      public ESMF_RouteHandlePrint
 
!EOPI

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_RHandle.F90,v 1.39.2.4 2009/01/21 21:25:23 cdeluca Exp $'

!==============================================================================

      contains

!==============================================================================
!
! TransformValues GetInit function
!
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_TransformValuesGetInit"
!BOPI
! !IROUTINE: ESMF_TransformValuesGetInit - Get the Init status 

! !INTERFACE:
      function ESMF_TransformValuesGetInit(d)
!
! !RETURN VALUE:
      ESMF_INIT_TYPE :: ESMF_TransformValuesGetInit
!
! !ARGUMENTS:
      type(ESMF_TransformValues), intent(in),optional :: d
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
     ESMF_TransformValuesGetInit=ESMF_INIT_GET(d)
  else
     ESMF_TransformValuesGetInit=ESMF_INIT_CREATED
  endif 
end function ESMF_TransformValuesGetInit


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
        integer :: nitems

        ! Set initial values
        status = ESMF_RC_NOT_IMPL
        tv%this = ESMF_NULL_POINTER

        ! Initialize return code; assume failure until success is certain
        if (present(rc)) rc = ESMF_RC_NOT_IMPL

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

        if (present(rc)) rc = ESMF_SUCCESS

	ESMF_INIT_SET_CREATED(ESMF_TransformValuesCreate)

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

        ! Set initial values
        status = ESMF_RC_NOT_IMPL

        ! Initialize return code; assume failure until success is certain
        if (present(rc)) rc = ESMF_RC_NOT_IMPL

        ! Call C++ destroy code
        call c_ESMC_TransformValuesDestroy(tv, status)
        if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

        ! nullify pointer
        tv%this = ESMF_NULL_POINTER

	ESMF_INIT_SET_DELETED(tv)

        if (present(rc)) rc = ESMF_SUCCESS

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
        integer :: curnumlist
        type(ESMF_Pointer) :: cursrcPtr
        type(ESMF_Pointer) :: curdstPtr
        type(ESMF_Pointer) :: curweightsPtr

        ! Set initial values
        status = ESMF_RC_NOT_IMPL

        ! Initialize return code; assume failure until success is certain
        if (present(rc)) rc = ESMF_RC_NOT_IMPL

        ESMF_INIT_CHECK_DEEP(ESMF_TransformValuesGetInit,tv,rc)

        ! Call C++  code to get all current values
        call c_ESMC_TransformValuesGet(tv, curnumlist, cursrcPtr, &
                                       curdstPtr, curweightsPtr, status)
        if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

        if (present(numList)) then
            numList = curnumlist    
        endif

        if (present(srcIndex)) then
            call ESMF_LocalArraySetThis(srcIndex,cursrcPtr)
            call ESMF_LocalArraySetInitCreated(srcIndex)
        endif

        if (present(dstIndex)) then
            call ESMF_LocalArraySetThis(dstIndex,curdstPtr)
            call ESMF_LocalArraySetInitCreated(dstIndex)
        endif

        if (present(weights)) then
            call ESMF_LocalArraySetThis(weights,curweightsPtr)
            call ESMF_LocalArraySetInitCreated(weights)
        endif

        if (present(rc)) rc = ESMF_SUCCESS

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
        integer :: curnumlist
        type(ESMF_TVWrapperI4) :: srcwrap
        type(ESMF_TVWrapperI4) :: dstwrap
        type(ESMF_TVWrapperR8) :: wwrap

        ! Set initial values
        status = ESMF_RC_NOT_IMPL

        ! Initialize return code; assume failure until success is certain
        if (present(rc)) rc = ESMF_RC_NOT_IMPL

        ESMF_INIT_CHECK_DEEP(ESMF_TransformValuesGetInit,tv,rc)

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

        if (present(rc)) rc = ESMF_SUCCESS

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
        logical :: changed
        integer :: curnumlist
        type(ESMF_Pointer) :: cursrcPtr
        type(ESMF_Pointer) :: curdstPtr
        type(ESMF_Pointer) :: curweightsPtr

        ! Set initial values
        status = ESMF_RC_NOT_IMPL

        ! Initialize return code; assume failure until success is certain
        if (present(rc)) rc = ESMF_RC_NOT_IMPL

        ESMF_INIT_CHECK_DEEP(ESMF_TransformValuesGetInit,tv,rc)
        ESMF_INIT_CHECK_DEEP(ESMF_LocalArrayGetInit,srcIndex,rc)
        ESMF_INIT_CHECK_DEEP(ESMF_LocalArrayGetInit,dstIndex,rc)
        ESMF_INIT_CHECK_DEEP(ESMF_LocalArrayGetInit,weights,rc)



        ! Get old values and only replace the ones specified.
        call c_ESMC_TransformValuesGet(tv, curnumlist, cursrcPtr, &
                                       curdstPtr, curweightsPtr, status)
        changed = .false.

        if (present(numList)) then
          changed = .true.
          curnumlist = numList    
        endif

        if (present(srcIndex)) then
          changed = .true.
          call ESMF_LocalArrayGetThis(srcIndex,cursrcPtr)
        endif

        if (present(dstIndex)) then
          changed = .true.
          call ESMF_LocalArrayGetThis(dstIndex,curdstPtr)
        endif

        if (present(weights)) then
          changed = .true.
          call ESMF_LocalArrayGetThis(weights,curweightsPtr)
        endif

        ! Overwrite the changed values
        if (changed) then
            ! Call C++  code
            call c_ESMC_TransformValuesSet(tv, curnumlist, cursrcPtr, &
                                           curdstPtr, curweightsPtr, status)
            if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return
        endif

        if (present(rc)) rc = ESMF_SUCCESS

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

       ! Initialize return code; assume failure until success is certain       
       status = ESMF_RC_NOT_IMPL
       if (present(rc)) rc = ESMF_RC_NOT_IMPL

       ESMF_INIT_CHECK_DEEP(ESMF_TransformValuesGetInit,tv,rc)

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
       if (present(rc)) rc = ESMF_SUCCESS

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
!     Note:  Many {\tt ESMF\_<class>Print} methods are implemented in C++.
!     On some platforms/compilers there is a potential issue with interleaving
!     Fortran and C++ output to {\tt stdout} such that it doesn't appear in
!     the expected order.  If this occurs, it is recommended to use the
!     standard Fortran call {\tt flush(6)} as a workaround until this issue
!     is fixed in a future release. 
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

       ! Initialize return code; assume failure until success is certain       
       status = ESMF_RC_NOT_IMPL
       if (present(rc)) rc = ESMF_RC_NOT_IMPL

       ESMF_INIT_CHECK_DEEP(ESMF_TransformValuesGetInit,tv,rc)

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
       if (present(rc)) rc = ESMF_SUCCESS
 
       end subroutine ESMF_TransformValuesPrint

!------------------------------------------------------------------------------
!==============================================================================
!
! RouteHandle GetInit function
!
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_RouteHandleGetInit"
!BOPI
! !IROUTINE: ESMF_RouteHandleGetInit - Get the Init status 

! !INTERFACE:
      function ESMF_RouteHandleGetInit(d)
!
! !RETURN VALUE:
      ESMF_INIT_TYPE :: ESMF_RouteHandleGetInit
!
! !ARGUMENTS:
      type(ESMF_RouteHandle), intent(in),optional :: d
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
     ESMF_RouteHandleGetInit=ESMF_INIT_GET(d)
  else
     ESMF_RouteHandleGetInit=ESMF_INIT_CREATED
  endif 
end function ESMF_RouteHandleGetInit


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_RouteHandleSetInitCreated()"
!BOPI
! !IROUTINE: ESMF_RouteHandleSetInitCreated - Set RouteHandle init code to "CREATED"

! !INTERFACE:
  subroutine ESMF_RouteHandleSetInitCreated(rh, rc)
!
! !ARGUMENTS:
    type(ESMF_RouteHandle), intent(inout)           :: rh
    integer,                intent(out),  optional  :: rc  
!         
!
! !DESCRIPTION:
!      Set init code in RouteHandle object to "CREATED".
!
!     The arguments are:
!     \begin{description}
!     \item[rh] 
!          Specified {\tt ESMF\_RouteHandle} object.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
!------------------------------------------------------------------------------

    ! Assume failure until success
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    
    ! Set init code
    ESMF_INIT_SET_CREATED(rh)

    ! Return success
    if (present(rc)) rc = ESMF_SUCCESS
    
  end subroutine ESMF_RouteHandleSetInitCreated
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

        ! Set initial values
        status = ESMF_RC_NOT_IMPL
        rhandle%this = ESMF_NULL_POINTER

        ! Initialize return code; assume failure until success is certain
        if (present(rc)) rc = ESMF_RC_NOT_IMPL

        ! Call C++ create code
        call c_ESMC_RouteHandleCreate(rhandle, status)
        if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

        ! Set return values
        ESMF_RouteHandleCreate = rhandle

        if (present(rc)) rc = ESMF_SUCCESS

	ESMF_INIT_SET_CREATED(ESMF_RouteHandleCreate)

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

        ! Set initial values
        status = ESMF_RC_NOT_IMPL

        ! Initialize return code; assume failure until success is certain
        if (present(rc)) rc = ESMF_RC_NOT_IMPL

        ! check input variable
        ESMF_INIT_CHECK_DEEP(ESMF_RouteHandleGetInit,rhandle,rc)

        ! was handle already destroyed?
        if (rhandle%this .eq. ESMF_NULL_POINTER) then
            if (present(rc)) rc = ESMF_SUCCESS
            return
        endif 

        ! Call C++ destroy code
        call c_ESMC_RouteHandleDestroy(rhandle, status)
        if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

        ! nullify pointer
        rhandle%this = ESMF_NULL_POINTER

        if (present(rc)) rc = ESMF_SUCCESS

	ESMF_INIT_SET_DELETED(rhandle)

        end subroutine ESMF_RouteHandleDestroy


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_RouteHandleRelease"
!BOPI
! !IROUTINE: ESMF_RouteHandleRelease - Release all RouteHandle resources

! !INTERFACE:
      subroutine ESMF_RouteHandleRelease(routehandle, rc)
!
! !ARGUMENTS:
      type(ESMF_RouteHandle), intent(inout) :: routehandle   
      integer, intent(out), optional :: rc        
!
! !DESCRIPTION:
!     Same as {\tt ESMF\_RouteHandleDestroy}.
!
!     The arguments are:
!     \begin{description}
!     \item[routehandle] 
!          The {\tt ESMF\_RouteHandle} to be released.
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


        ! check input variable
        ESMF_INIT_CHECK_DEEP(ESMF_RouteHandleGetInit,routehandle,rc)

        call ESMF_RouteHandleDestroy(routehandle, rc=status)
        if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
                    ESMF_CONTEXT, rcToReturn=rc))  return

        if (present(rc)) rc = ESMF_SUCCESS
        end subroutine ESMF_RouteHandleRelease


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_RouteHandleGet"
!BOPI
! !IROUTINE: ESMF_RouteHandleGet - Get values from a RouteHandle

! !INTERFACE:
      subroutine ESMF_RouteHandleGet(rhandle, htype, route_count, rmaptype, &
                                     which_route, route, tv_count, tvmaptype, &
                                     which_tv, tdata, label, name, rc)
!
! !ARGUMENTS:
      type(ESMF_RouteHandle), intent(in) :: rhandle
      integer, intent(out), optional :: htype
      integer, intent(out), optional :: route_count
      integer, intent(out), optional :: rmaptype
      integer, intent(in), optional :: which_route
      type(ESMF_Route), intent(out), optional :: route
      integer, intent(out), optional :: tv_count
      integer, intent(out), optional :: tvmaptype
      integer, intent(in), optional :: which_tv
      type(ESMF_TransformValues), intent(out), optional :: tdata
      character(len=*), intent(out), optional :: label
      character(len=*), intent(out), optional :: name
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
!          Route type, e.g. halo, data redistribution, regrid.  Valid return
!          values are {\tt ESMF\_HALOHANDLE}, {\tt ESMF\_REDISTHANDLE},
!          {\tt ESMF\_REGRIDHANDLE}, or {\tt ESMF\_UNINITIALISEDHANDLE}.
!     \item[{[route\_count]}]
!          Number of internal route tables stored in this route handle.
!     \item[{[rmaptype]}]
!          Describes the mapping between lists of input addresses and number
!          of stored routes.   Valid values are one of
!          {\tt ESMF\_1TO1HANDLEMAP}, {\tt ESMF\_ALLTO1HANDLEMAP},
!          {\tt ESMF\_INDIRECTHANDLEMAP}, {\tt ESMF\_NOHANDLEMAP},
!          or {\tt ESMF\_UNKNOWNHANDLEMAP}.
!     \item[{[which\_route]}]
!          If {\tt route} is specified, this input value must be set to
!          indicate which of the possible list of routes is to be returned.
!          This must be equal to or less than the {\tt route\_count}.
!          The index numbers start at 1.
!     \item[{[route]}]
!          {\tt ESMF\_Route} is returned here.
!     \item[{[tv\_count]}]
!          Number of internal transform values stored in this route handle.
!     \item[{[tvmaptype]}]
!          Describes the mapping between lists of input addresses and number
!          of stored transform values.   Valid values are one of
!          {\tt ESMF\_1TO1HANDLEMAP}, {\tt ESMF\_ALLTO1HANDLEMAP},
!          {\tt ESMF\_INDIRECTHANDLEMAP}, {\tt ESMF\_NOHANDLEMAP},
!          or {\tt ESMF\_UNKNOWNHANDLEMAP}.
!     \item[{[which\_tv]}]
!          If {\tt tdata} is specified, this input value must be set to
!          indicate which of the possible list of transforms is to be returned.
!          This must be equal to or less than the {\tt tv\_count}.
!          The index numbers start at 1.
!     \item[{[tdata]}]
!          Transform values are returned here.
!     \item[{[label]}]
!          If specified at store time, the optional character label is
!          returned here.  If not set, {\tt "NONE"} is returned.
!     \item [{[name]}]
!          Name of the RouteHandle object.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI

        ! local variables
        integer :: status                  ! local error status
        integer :: myhtype, myrcount, mytvcount, myrmaptype, mytvmaptype
        integer :: zerobase_which
        character(len=ESMF_MAXSTR) :: mylabel

        ! Set initial values
        status = ESMF_RC_NOT_IMPL

        ! Initialize return code; assume failure until success is certain
        if (present(rc)) rc = ESMF_RC_NOT_IMPL

        ESMF_INIT_CHECK_DEEP(ESMF_RouteHandleGetInit,rhandle,rc)

        ! Call C++  code
        ! TODO: handle label string going through the interface
        call c_ESMC_RouteHandleGetInfo(rhandle, myhtype, &
                                       myrcount, myrmaptype, &
                                       mytvcount, mytvmaptype, status)
        if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

        ! TODO: need to handle all 3 of these across the interface
        mylabel = "NONE"

        ! query for route type
        if (present(htype)) then
            htype = myhtype    
        endif

        ! query for route mapping type
        if (present(rmaptype)) then
            rmaptype = myrmaptype    
        endif

        ! query for route count
        if (present(route_count)) then
            route_count = myrcount    
        endif

        ! query for tv mapping type
        if (present(tvmaptype)) then
            tvmaptype = mytvmaptype    
        endif

        ! query for tv count
        if (present(tv_count)) then
            tv_count = mytvcount    
        endif

        ! query for a specific route in the list
        if ((.not.present(which_route)) .and. (.not.present(route))) then
            ! fine - it is ok not to specify both
            continue  
        else if (present(which_route) .and. present(route)) then
            ! also fine - both specified, do the query
            if (which_route .gt. myrcount) then
                ! set error and return - cannot query for a route
                ! number larger than the number stored.
                call ESMF_LogMsgSetError(ESMF_RC_ARG_BAD, &
                          "which_route larger than number of existing routes", &
                          ESMF_CONTEXT, rc)
                return
            endif
            zerobase_which = which_route - 1
            call c_ESMC_RouteHandleGetRoute(rhandle, zerobase_which, route, &
                                            status)
            if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return
            ! Make datatype valid
            call ESMF_RouteSetInitCreated(route)
        else  
            ! cannot specify one arg but not the other
            call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, &
                          "Must specify both which_route and route together", &
                          ESMF_CONTEXT, rc) 
            return
            ! TODO: we could make it so if which_route is not specified the
            ! first one is returned, but this could be more error prone. 
            ! this interface is generally only used inside the framework, 
            ! so asking the calling code to be specific seems safer.
        endif

        ! query for a specific tv in the list
        if ((.not.present(which_tv)) .and. (.not.present(tdata))) then
            ! fine - it is ok not to specify both
            continue  
        else if (present(which_tv) .and. present(tdata)) then
            ! also fine - both specified, do the query
            if (which_tv .gt. mytvcount) then
                ! set error and return - cannot query for a route
                ! number larger than the number stored.
                call ESMF_LogMsgSetError(ESMF_RC_ARG_BAD, &
                    "which_tv larger than number of existing transform vals", &
                     ESMF_CONTEXT, rc) 
                return
            endif
            zerobase_which = which_tv - 1
            call c_ESMC_RouteHandleGetTValues(rhandle, zerobase_which, tdata, &
                                              status)
            if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return
            ! Make datatype valid
            ESMF_INIT_SET_CREATED(tdata)
        else  
            ! cannot specify one arg but not the other
            call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, &
                          "Must specify both which_tv and tdata together", &
                          ESMF_CONTEXT, rc) 
            return
            ! TODO: same commment as above, same conclusion even more because
            ! only the internal regrid code uses these values.
        endif

        if (present(label)) then
            label = mylabel    
        endif


        ! Special call to get name out of Base class
        if (present(name)) then
          call c_ESMC_GetName(rhandle, name, status)
          if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        endif
    
        if (present(rc)) rc = ESMF_SUCCESS

        end subroutine ESMF_RouteHandleGet

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_RouteHandleSet"
!BOPI
! !IROUTINE: ESMF_RouteHandleSet - Set values in a RouteHandle

! !INTERFACE:
      subroutine ESMF_RouteHandleSet(rhandle, htype, route_count, rmaptype, &
                                     which_route, route, tv_count, tvmaptype, &
                                     which_tv, tdata, label, name, rc)
!
! !ARGUMENTS:
      type(ESMF_RouteHandle), intent(in) :: rhandle
      integer, intent(in), optional :: htype
      integer, intent(in), optional :: route_count
      integer, intent(in), optional :: rmaptype
      integer, intent(in), optional :: which_route
      type(ESMF_Route), intent(in), optional :: route
      integer, intent(in), optional :: tv_count
      integer, intent(in), optional :: tvmaptype
      integer, intent(in), optional :: which_tv
      type(ESMF_TransformValues), intent(in), optional :: tdata
      character(len=*), intent(in), optional :: label
      character(len = *), intent(in),   optional  :: name    
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
!          Route type, e.g. halo, data redistribution, regrid.  Valid 
!          values are {\tt ESMF\_HALOHANDLE}, {\tt ESMF\_REDISTHANDLE},
!          {\tt ESMF\_REGRIDHANDLE}, or {\tt ESMF\_UNINITIALISEDHANDLE}.
!     \item[{[route\_count]}]
!          Set number of internal route tables stored in this route handle.
!     \item[{[rmaptype]}]
!          Set the mapping between lists of input addresses and number
!          of stored routes.   Valid values are 
!          {\tt ESMF\_1TO1HANDLEMAP}, {\tt ESMF\_ALLTO1HANDLEMAP},
!          {\tt ESMF\_INDIRECTHANDLEMAP}, {\tt ESMF\_NOHANDLEMAP},
!          or {\tt ESMF\_UNKNOWNHANDLEMAP}.
!     \item[{[which\_route]}]
!          If {\tt route} is specified, this input value must be set to
!          indicate which of the possible list of routes is to be set.
!          This must be equal to or less than the {\tt route\_count}.
!          The index numbers start at 1.
!     \item[{[route]}]
!          {\tt ESMF\_Route} to be set in the route handle.
!     \item[{[tv\_count]}]
!          Set number of internal transform values stored in this route handle.
!     \item[{[tvmaptype]}]
!          Set the mapping between lists of input addresses and number
!          of stored transform values.   Valid values are
!          {\tt ESMF\_1TO1HANDLEMAP}, {\tt ESMF\_ALLTO1HANDLEMAP},
!          {\tt ESMF\_INDIRECTHANDLEMAP}, {\tt ESMF\_NOHANDLEMAP},
!          or {\tt ESMF\_UNKNOWNHANDLEMAP}.
!     \item[{[which\_tv]}]
!          If {\tt tdata} is specified, this input value must be set to
!          indicate which of the possible list of transforms is to be set.
!          This must be equal to or less than the {\tt tv\_count}.
!          The index numbers start at 1.
!     \item[{[tdata]}]
!          Transform values to be set in the route handle.
!     \item[{[label]}]
!          Optional character label.  Not interpreted by the framework;
!          any value useful to the caller can be used.
!     \item [{[name]}]
!          The RouteHandle name.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI

        ! local variables
        integer :: status                  ! local error status
        integer :: zero = 0
        integer :: zerobase_which

        ! Set initial values
        status = ESMF_RC_NOT_IMPL

        ! Initialize return code; assume failure until success is certain
        if (present(rc)) rc = ESMF_RC_NOT_IMPL

        ESMF_INIT_CHECK_DEEP(ESMF_RouteHandleGetInit,rhandle,rc)
        ESMF_INIT_CHECK_DEEP(ESMF_RouteGetInit,route,rc)
        ESMF_INIT_CHECK_DEEP(ESMF_TransformValuesGetInit,tdata,rc)
        if (present(htype)) then
            call c_ESMC_RouteHandleSetType(rhandle, htype, status)
            if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return
        endif

        ! Set the name in Base object
        if (present(name)) then
          call c_ESMC_SetName(rhandle, "RouteHandle", name, status)
          if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        endif

        if (present(route_count)) then
            call c_ESMC_RouteHandleSetRouteCount(rhandle, route_count, status)
            if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return
        endif

        if (present(rmaptype)) then
            call c_ESMC_RouteHandleSetRMapType(rhandle, rmaptype, status)
            if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return
        endif

        if (present(route)) then
            if (present(which_route)) then
                zerobase_which = which_route - 1
                call c_ESMC_RouteHandleSetRoute(rhandle, zerobase_which, route, &
                                                status)
            else
                call c_ESMC_RouteHandleSetRoute(rhandle, zero, route, status)
            endif
            if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return
        endif

        if (present(tv_count)) then
            call c_ESMC_RouteHandleSetTvCount(rhandle, tv_count, status)
            if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return
        endif

        if (present(tvmaptype)) then
            call c_ESMC_RouteHandleSetTVMapType(rhandle, tvmaptype, status)
            if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return
        endif

        if (present(tdata)) then
            if (present(which_tv)) then
                zerobase_which = which_tv - 1
                call c_ESMC_RouteHandleSetTValues(rhandle, zerobase_which, tdata, &
                                                status)
            else
                call c_ESMC_RouteHandleSetTValues(rhandle, zero, tdata, status)
            endif
            if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return
        endif

        if (present(label)) then
            call c_ESMC_RouteHandleSetLabel(rhandle, label, status)
            if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

        endif

        if (present(rc)) rc = ESMF_SUCCESS

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

       ! Initialize return code; assume failure until success is certain       
       status = ESMF_RC_NOT_IMPL
       if (present(rc)) rc = ESMF_RC_NOT_IMPL

       ESMF_INIT_CHECK_DEEP(ESMF_RouteHandleGetInit,rhandle,rc)

       defaultopts = "quick"

       ! See if this has been created yet or not.
       if ((rhandle%this).eq.ESMF_NULL_POINTER) then
         if (present(rc)) rc = ESMF_RC_NOT_IMPL
         return
       endif

       ! TODO: the following code is commented out because the C-side
       !       validate routine is empty
       !if(present(options)) then
       !    call c_ESMC_RouteHandleValidate(rhandle, options, status)   
       !else
       !    call c_ESMC_RouteHandleValidate(rhandle, defaultopts, status)
       !endif

       !if (ESMF_LogMsgFoundError(status, &
       !                           ESMF_ERR_PASSTHRU, &
       !                           ESMF_CONTEXT, rc)) return

       ! Set return values
       if (present(rc)) rc = ESMF_SUCCESS

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
!     Print information about a RouteHandle.  
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

       ! Initialize return code; assume failure until success is certain       
       status = ESMF_RC_NOT_IMPL
       if (present(rc)) rc = ESMF_RC_NOT_IMPL

       ESMF_INIT_CHECK_DEEP(ESMF_RouteHandleGetInit,rhandle,rc)

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
       if (present(rc)) rc = ESMF_SUCCESS
 
       end subroutine ESMF_RouteHandlePrint

!------------------------------------------------------------------------------

       end module ESMF_RHandleMod
