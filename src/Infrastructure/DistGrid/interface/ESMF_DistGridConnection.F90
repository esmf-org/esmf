! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2018, University Corporation for Atmospheric Research, 
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
! Laboratory, University of Michigan, National Centers for Environmental 
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
#define ESMF_FILENAME "ESMF_DistGridConnection.F90"
!==============================================================================
!
! ESMF DistGrid Module
module ESMF_DistGridConnectionMod
!
!==============================================================================
!
! This file contains the DistGridConnection shallow class implementation.
!
!------------------------------------------------------------------------------
! INCLUDES
#include "ESMF.h"

!==============================================================================
!BOPI
! !MODULE: ESMF_DistGridConnectionMod
!
!------------------------------------------------------------------------------

! !USES:
  use ESMF_UtilTypesMod           ! ESMF utility types
  use ESMF_InitMacrosMod          ! ESMF initializer macros
  use ESMF_LogErrMod              ! ESMF error handling
  use ESMF_F90InterfaceMod        ! ESMF F90-C++ interface helper
  
  implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
  private

!------------------------------------------------------------------------------
! ! ESMF_DistGridConnection

  type ESMF_DistGridConnection
#ifndef ESMF_NO_SEQUENCE
  sequence
#endif
    private
    integer :: connection(2*7+2)  ! reserve for maximum dimCount
    integer :: elementCount       ! number of actual elements in connection
    ESMF_INIT_DECLARE
  end type

!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:

! - ESMF-public methods:

  public ESMF_DistGridConnection
  public ESMF_DistGridConnectionGet
  public ESMF_DistGridConnectionSet
  public ESMF_DistGridConnectionSetIntl
  public ESMF_DistGridConnectionPrint
  public ESMF_InterArrayCreateDGConn
  

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DistGridConnectionInt()"
!BOPI
! !IROUTINE: ESMF_DistGridConnectionInt - Construct a DistGrid connection element
! !INTERFACE:
  subroutine ESMF_DistGridConnectionInt(connection, tileIndexA, tileIndexB, &
    positionVector, keywordEnforcer, orientationVector, rc)
!
! !ARGUMENTS:
    integer,        target, intent(out)           :: connection(:)
    integer,                intent(in)            :: tileIndexA
    integer,                intent(in)            :: tileIndexB
    integer,                intent(in)            :: positionVector(:)
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,                intent(in),  optional :: orientationVector(:)
    integer,                intent(out), optional :: rc
!
! !DESCRIPTION:
!   This call helps to construct a DistGrid connection,
!   which is a simple vector of integers, out of its components.
!
!   The arguments are:
!   \begin{description}
!   \item[connection] 
!     Element to be constructed. The provided {\tt connection} array must 
!     be dimensioned to hold exactly the number of integers that result from
!     the input information.
!   \item[tileIndexA] 
!     Index of one of the two tiles that are to be connected.
!   \item[tileIndexB] 
!     Index of one of the two tiles that are to be connected.
!   \item[positionVector] 
!     Position of tile B's minIndex with respect to tile A's minIndex.
!   \item[{[orientationVector]}]
!     Associates each dimension of tile A with a dimension in tile B's 
!     index space. Negative index values may be used to indicate a 
!     reversal in index orientation. It is erroneous to associate multiple
!     dimensions of tile A with the same index in tile B. By default
!     {\tt orientationVector = (/1,2,3,.../)}, i.e. same orientation as tile A.
!   \item[{[rc]}] 
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    integer               :: localrc      ! local return code
    type(ESMF_InterArray) :: connectionArg        ! helper variable
    type(ESMF_InterArray) :: positionVectorArg    ! helper variable
    type(ESMF_InterArray) :: orientationVectorArg ! helper variable

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    
    ! Deal with (optional) array arguments
    connectionArg = ESMF_InterArrayCreate(connection, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    positionVectorArg = ESMF_InterArrayCreate(positionVector, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    orientationVectorArg = ESMF_InterArrayCreate(orientationVector, &
      rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! call into the C++ interface, which will sort out optional arguments
    call c_ESMC_DistGridConnection(connectionArg, &
      tileIndexA, tileIndexB, positionVectorArg, orientationVectorArg, &
      localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
      
    ! garbage collection
    call ESMF_InterArrayDestroy(connectionArg, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterArrayDestroy(positionVectorArg, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterArrayDestroy(orientationVectorArg, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    
    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS
 
  end subroutine ESMF_DistGridConnectionInt
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DistGridConnectionGet()"
!BOP
! !IROUTINE: ESMF_DistGridConnectionGet - Get DistGridConnection
! !INTERFACE:
  subroutine ESMF_DistGridConnectionGet(connection, keywordEnforcer, &
    tileIndexA, tileIndexB, dimCount, positionVector, orientationVector, rc)
!
! !ARGUMENTS:
    type(ESMF_DistGridConnection), intent(in)            :: connection
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,                       intent(out), optional :: tileIndexA
    integer,                       intent(out), optional :: tileIndexB
    integer,                       intent(out), optional :: dimCount
    integer,                       intent(out), optional :: positionVector(:)
    integer,                       intent(out), optional :: orientationVector(:)
    integer,                       intent(out), optional :: rc
!
! !DESCRIPTION:
!   \label{api:DistGridConnectionGet}
!   Get connection parameters from an {\tt ESMF\_DistGridConnection} object.
!   This interface provides access to all variables required to create a new
!   connection using the {\tt ESMF\_DistGridConnectionSet()} method.
!
!   The arguments are:
!   \begin{description}
!   \item[connection]
!     DistGridConnection object.
!   \item[{[tileIndexA]}]
!     Index of one of the two connected tiles.
!   \item[{[tileIndexB]}]
!     Index of the other connected tile.
!   \item[{[dimCount]}]
!     Number of dimensions of {\tt positionVector}.
!   \item[{[positionVector]}]
!     Position of tile B's minIndex with respect to tile A's minIndex.
!     This array's size should be at least equal to {\tt dimCount}.
!   \item[{[orientationVector]}]
!     Lists which dimension of tile A is associated to which dimension of
!     tile B. Negative index values may be used to indicate a reversal
!     in index orientation. Should be at least of size {\tt dimCount}.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer :: localrc      ! local return code
    integer :: localdimCount

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! ensure connection is valid
ESMF_INIT_CHECK_SHALLOW_SHORT(ESMF_DistGridConnectionGetInit, connection, rc)

    ! check if connection contains any elements
    localdimCount = (connection % elementCount - 2)/2

    if (localdimCount <= 0) then
      call ESMF_LogSetError(rcToCheck=ESMF_RC_OBJ_BAD, &
        msg="Insufficient number of elements found.", &
        ESMF_CONTEXT, rcToReturn=rc)
      return
    end if

    ! get conected tiles
    if (present(tileIndexA)) tileIndexA = connection % connection(1)
    if (present(tileIndexB)) tileIndexB = connection % connection(2)

    if (present(dimCount)) dimCount = localdimCount

    if (present(positionVector)) then
      if (size(positionVector) < localdimCount) then
        call ESMF_LogSetError(rcToCheck=ESMC_RC_ARG_SIZE, &
          msg="Size of positionVector array smaller than number of dimensions.", &
          ESMF_CONTEXT, rcToReturn=rc)
        return
      end if
      positionVector = 0
      positionVector(1:localdimCount) = &
        connection % connection(3:2+localdimCount)
    end if

    if (present(orientationVector)) then
      if (size(orientationVector) < localdimCount) then
        call ESMF_LogSetError(rcToCheck=ESMC_RC_ARG_SIZE, &
          msg="Size of orientationVector array smaller than number of dimensions.", &
          ESMF_CONTEXT, rcToReturn=rc)
        return
      end if
      orientationVector = 0
      orientationVector(1:localdimCount) = &
        connection % connection(3+localdimCount:2+2*localdimCount)
    end if

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_DistGridConnectionGet
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DistGridConnectionSet()"
!BOP
! !IROUTINE: ESMF_DistGridConnectionSet - Set DistGridConnection
! !INTERFACE:
  subroutine ESMF_DistGridConnectionSet(connection, tileIndexA, tileIndexB, &
    positionVector, keywordEnforcer, orientationVector, rc)
!
! !ARGUMENTS:
    type(ESMF_DistGridConnection),intent(out)         :: connection
    integer,                     intent(in)           :: tileIndexA
    integer,                     intent(in)           :: tileIndexB
    integer,                     intent(in)           :: positionVector(:)
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,                     intent(in), optional :: orientationVector(:)
    integer,                     intent(out), optional:: rc
!         
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
!   \label{api:DistGridConnectionSet}
!   Set an {\tt ESMF\_DistGridConnection} object to represent a connection 
!   according to the provided index space information.
!
!   The arguments are:
!   \begin{description}
!   \item[connection] 
!     DistGridConnection object.
!   \item[tileIndexA] 
!     Index of one of the two tiles that are to be connected.
!   \item[tileIndexB] 
!     Index of one of the two tiles that are to be connected.
!   \item[positionVector] 
!     Position of tile B's minIndex with respect to tile A's minIndex.
!   \item[{[orientationVector]}]
!     Associates each dimension of tile A with a dimension in tile B's 
!     index space. Negative index values may be used to indicate a 
!     reversal in index orientation. It is erroneous to associate multiple
!     dimensions of tile A with the same index in tile B. By default
!     {\tt orientationVector = (/1,2,3,.../)}, i.e. same orientation as tile A.
!   \item[{[rc]}] 
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code
    integer                 :: dimCount

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! mark output as uninitialized    
    ESMF_INIT_SET_DELETED(connection)

    ! set the actual elementCount in connection member
    dimCount = size(positionVector)
    connection%elementCount = 2*dimCount+2
    
    call ESMF_DistGridConnectionInt(connection%connection(1:2*dimCount+2), &
      tileIndexA=tileIndexA, tileIndexB=tileIndexB, &
      positionVector=positionVector, orientationVector=orientationVector, &
      rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! mark output as successfully initialized
    ESMF_INIT_SET_DEFINED(connection)

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS
 
  end subroutine ESMF_DistGridConnectionSet
!------------------------------------------------------------------------------


! -------------------------- ESMF-private method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DistGridConnectionSetIntl()"
!BOPI
! !IROUTINE: ESMF_DistGridConnectionSetIntl - Set DistGridConnection directly
! !INTERFACE:
  subroutine ESMF_DistGridConnectionSetIntl(connection, farray, rc)
!
! !ARGUMENTS:
    type(ESMF_DistGridConnection),intent(out)         :: connection
    integer,                     intent(in)           :: farray(:)
    integer,                     intent(out), optional:: rc
!         
!
! !DESCRIPTION:
!
!   The arguments are:
!   \begin{description}
!   \item[connection] 
!     DistGridConnection object.
!   \item[{[rc]}] 
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    
    connection%elementCount = size(farray)
    connection%connection(1:size(farray)) = farray(1:size(farray))

    ! mark output as successfully initialized
    ESMF_INIT_SET_DEFINED(connection)

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS
 
  end subroutine ESMF_DistGridConnectionSetIntl
!------------------------------------------------------------------------------


! -------------------------- ESMF-private method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DistGridConnectionPrint()"
!BOPI
! !IROUTINE: ESMF_DistGridConnectionPrint - Print DistGridConnection
! !INTERFACE:
  subroutine ESMF_DistGridConnectionPrint(connection, rc)
!
! !ARGUMENTS:
    type(ESMF_DistGridConnection),intent(in )         :: connection
    integer,                     intent(out), optional:: rc
!         
!
! !DESCRIPTION:
!
!   The arguments are:
!   \begin{description}
!   \item[connection] 
!     DistGridConnection object.
!   \item[{[rc]}] 
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    
    print *, "DistGridConnectionPrint - elementCount=", connection%elementCount
    print *, "DistGridConnectionPrint - connection=", &
      connection%connection(1:connection%elementCount)
    
    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS
 
  end subroutine ESMF_DistGridConnectionPrint
!------------------------------------------------------------------------------


! -------------------------- ESMF-private method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_InterArrayCreateDGConn()"
!BOPI
! !IROUTINE: ESMF_InterArrayCreateDGConn - Create InterArray from DistGrid Connection List

! !INTERFACE:
  function ESMF_InterArrayCreateDGConn(connectionList, initFlag, rc)
!
! !ARGUMENTS:
    type(ESMF_DistGridConnection), intent(in),  optional :: connectionList(:)
    logical,                       intent(in),  optional :: initFlag
    integer,                       intent(out), optional :: rc
!         
! !RETURN VALUE:
    type(ESMF_InterArray) :: ESMF_InterArrayCreateDGConn
!
! !DESCRIPTION:
!   Create a compacted 2D {\tt ESMF\_InterArray} from a list of 
!   DistGridConnection objects. All of the DistGridConnection objects in
!   {\tt connectionLis} must have the same elementCount.
!
!   The arguments are:
!   \begin{description}
!   \item[{[connectionList}]]
!     List of DistGridConnection objects.
!   \item[{[initFlag}]]
!     Flag indicating initialization status of the {\tt connectionList}. A value
!     of {\tt .true.} indicates that the {\tt connectionList} has been
!     initialized, and the entries are valid for use. A value of {\tt .false.}
!     inidicates that the {\tt connectionList} has not been initialized, and
!     therefore an InterArray with maximum size elementCount must be created.
!     This option is for passing connection lists from the C++ layer back to the
!     Fortran layer. The default is {\tt .true.}.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    integer               :: localrc      ! local return code
    integer               :: i, elementCount, stat, connectionListSize
    integer, pointer      :: farray(:,:)
    type(ESMF_InterArray) :: array
    logical               :: initAux
    
    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    
    ! set initAux
    initAux = .true.  ! default
    if (present(initFlag)) initAux = initFlag
    
    ! mark this InterArray as invalid
    call c_ESMC_InterArraySetInvalid(array, localrc)
    ESMF_InterArrayCreateDGConn = array
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    
    ! construction
    connectionListSize = 0
    if (present(connectionList)) connectionListSize = size(connectionList)
    if (connectionListSize > 0) then
      ! determine elementCount
      if (initAux) then
        ! incoming connections are valid, and assume all connections have same
        ! elementCount
        elementCount = connectionList(1)%elementCount
      else
        ! incoming connections are not valid -> must assume larges possible case
        elementCount = 2*7 + 2
      endif
      ! allocate 2D Fortran array to hold connectionList in the internal format
      allocate(farray(elementCount,size(connectionList)), stat=stat)
      if (ESMF_LogFoundAllocError(stat, msg="allocating farray", &
        ESMF_CONTEXT)) &
        return  ! bail out
      if (initAux) then
        ! incoming connections are valid
        do i=1, size(connectionList)
ESMF_INIT_CHECK_SHALLOW_SHORT(ESMF_DistGridConnectionGetInit, connectionList(i), rc)
          if (connectionList(i)%elementCount /= elementCount) then
            call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_BAD, &
              msg="elementCount mismatch between DistGridConnection elements.", &
              ESMF_CONTEXT, rcToReturn=rc)
            return
          endif
          ! copy the connection information
          farray(:,i) = connectionList(i)%connection(1:elementCount)
        enddo
      endif
      ! create InterArray for farray and transfer ownership
      array = ESMF_InterArrayCreate(farray2D=farray, &
        transferOwnership=.true., rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    else
      ! dummy InterArray
      array = ESMF_InterArrayCreate(rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif
 
    ! set return value
    ESMF_InterArrayCreateDGConn = array
    
    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS
 
  end function ESMF_InterArrayCreateDGConn
!------------------------------------------------------------------------------


! -------------------------- ESMF-internal method -----------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DistGridConnectionGetInit"
!BOPI
! !IROUTINE: ESMF_DistGridConnectionGetInit - Internal access routine for init code
!
! !INTERFACE:
  function ESMF_DistGridConnectionGetInit(connection) 
!
! !RETURN VALUE:
    ESMF_INIT_TYPE :: ESMF_DistGridConnectionGetInit   
!
! !ARGUMENTS:
    type(ESMF_DistGridConnection), intent(in), optional :: connection
!
! !DESCRIPTION:
!   Access init code.
!
!   The arguments are:
!   \begin{description}
!   \item [connection]
!     DistGridConnection object.
!   \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    if (present(connection)) then
      ESMF_DistGridConnectionGetInit = ESMF_INIT_GET(connection)
    else
      ESMF_DistGridConnectionGetInit = ESMF_INIT_DEFINED
    endif

  end function ESMF_DistGridConnectionGetInit
!------------------------------------------------------------------------------


end module ESMF_DistGridConnectionMod
