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
#define ESMF_FILENAME "ESMF_DistGrid.F90"
!==============================================================================
!
! ESMF DistGrid Module
module ESMF_DistGridMod
!
!==============================================================================
!
! This file contains the F90 wrapper code for the C++ implementation of
!  the DistGrid class.
!
!------------------------------------------------------------------------------
! INCLUDES
#include "ESMF.h"

!==============================================================================
!BOPI
! !MODULE: ESMF_DistGridMod
!

!   F90 API wrapper of C++ implementation of DistGrid
!
!------------------------------------------------------------------------------

! !USES:
  use ESMF_UtilTypesMod           ! ESMF utility types
  use ESMF_InitMacrosMod          ! ESMF initializer macros
  use ESMF_BaseMod                ! ESMF base class
  use ESMF_LogErrMod              ! ESMF error handling
  use ESMF_VMMod                  ! ESMF VM
  use ESMF_DELayoutMod            ! ESMF DELayout
  use ESMF_F90InterfaceMod        ! ESMF F90-C++ interface helper
  use ESMF_IOUtilMod              ! ESMF I/O utility layer

  use ESMF_DistGridConnectionMod  ! ESMF DistGrid connections
  
  implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
  private
      
!------------------------------------------------------------------------------
!     ! ESMF_DistGrid
!
!------------------------------------------------------------------------------

  ! Fortran type to hold pointer to C++ object
  type ESMF_DistGrid
#ifndef ESMF_NO_SEQUENCE
  sequence
#endif
  private
    type(ESMF_Pointer) :: this
    ESMF_INIT_DECLARE
  end type

!------------------------------------------------------------------------------

  ! Decomp_Flag
  type ESMF_Decomp_Flag
  private
#ifdef ESMF_NO_INITIALIZERS
    integer :: value
#else
    integer :: value = 0
#endif
  end type

  type(ESMF_Decomp_Flag), parameter:: &
    ESMF_DECOMP_BALANCED    = ESMF_Decomp_Flag(1), &
    ESMF_DECOMP_RESTFIRST   = ESMF_Decomp_Flag(2), &
    ESMF_DECOMP_RESTLAST    = ESMF_Decomp_Flag(3), &
    ESMF_DECOMP_CYCLIC      = ESMF_Decomp_Flag(4), &
    ESMF_DECOMP_SYMMEDGEMAX = ESMF_Decomp_Flag(5)
    
!------------------------------------------------------------------------------

  ! DistGridMatch_Flag
  type ESMF_DistGridMatch_Flag
  private
#ifdef ESMF_NO_INITIALIZERS
    integer :: value
#else
    integer :: value = 0
#endif
  end type

  type(ESMF_DistGridMatch_Flag), parameter:: &
    ESMF_DISTGRIDMATCH_INVALID  = ESMF_DistGridMatch_Flag(0), &
    ESMF_DISTGRIDMATCH_NONE     = ESMF_DistGridMatch_Flag(1), &
    ESMF_DISTGRIDMATCH_EXACT    = ESMF_DistGridMatch_Flag(2), &
    ESMF_DISTGRIDMATCH_ALIAS    = ESMF_DistGridMatch_Flag(3)
    
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
! !PUBLIC TYPES:
  public ESMF_DistGrid
  public ESMF_Decomp_Flag, ESMF_DECOMP_BALANCED, ESMF_DECOMP_RESTFIRST, &
    ESMF_DECOMP_RESTLAST, ESMF_DECOMP_CYCLIC, ESMF_DECOMP_SYMMEDGEMAX
  public ESMF_DistGridMatch_Flag, ESMF_DISTGRIDMATCH_INVALID, &
    ESMF_DISTGRIDMATCH_NONE, ESMF_DISTGRIDMATCH_EXACT, ESMF_DISTGRIDMATCH_ALIAS
  public ESMF_DistGridConnection  ! implemented in ESMF_DistGridConnectionMod
  
!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:

! - ESMF-public methods:
  public operator(==)
  public operator(/=)
  public operator(<)
  public operator(>)
  public operator(<=)
  public operator(>=)

  public ESMF_DistGridCreate
  public ESMF_DistGridDestroy
  
  public ESMF_DistGridGet
  public ESMF_DistGridIsCreated
  public ESMF_DistGridMatch
  public ESMF_DistGridPrint
  public ESMF_DistGridSet
  public ESMF_DistGridValidate
  
  public ESMF_DistGridConnectionGet ! implemented in ESMF_DistGridConnectionMod
  public ESMF_DistGridConnectionSet ! implemented in ESMF_DistGridConnectionMod
  public ESMF_DistGridConnectionPrint ! impl. in ESMF_DistGridConnectionMod

! - ESMF-internal methods:
  public ESMF_DistGridGetInit
  public ESMF_DistGridSetInitCreated
  public ESMF_DistGridSeqIndex
  
!EOPI
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
  character(*), parameter, private :: version = &
    '$Id$'

!==============================================================================
! 
! INTERFACE BLOCKS
!
!==============================================================================

! -------------------------- ESMF-public method -------------------------------
!BOPI
! !IROUTINE: ESMF_DistGridCreate -- Generic interface

! !INTERFACE:
  interface ESMF_DistGridCreate

! !PRIVATE MEMBER FUNCTIONS:
!
    module procedure ESMF_DistGridCreateDG
    module procedure ESMF_DistGridCreateDGT
    module procedure ESMF_DistGridCreateRD
    module procedure ESMF_DistGridCreateRDT
    module procedure ESMF_DistGridCreateRDF
    module procedure ESMF_DistGridCreateRDTF
    module procedure ESMF_DistGridCreateDB
    module procedure ESMF_DistGridCreateDBT
    module procedure ESMF_DistGridCreateDBF
    module procedure ESMF_DistGridCreateDBTF
    module procedure ESMF_DistGridCreateDBAI1D1DE
    module procedure ESMF_DistGridCreateDBAI1D1DEI8
    module procedure ESMF_DistGridCreateDBAI1D
    module procedure ESMF_DistGridCreateDBAI
      
! !DESCRIPTION: 
! This interface provides a single entry point for the various 
!  types of {\tt ESMF\_DistGridCreate} functions.   
!EOPI 
  end interface
!==============================================================================
      

! -------------------------- ESMF-public method -------------------------------
!BOPI
! !IROUTINE: ESMF_DistGridGet -- Generic interface

! !INTERFACE:
  interface ESMF_DistGridGet

! !PRIVATE MEMBER FUNCTIONS:
!
    module procedure ESMF_DistGridGetDefault
!    module procedure ESMF_DistGridGetPDe
    module procedure ESMF_DistGridGetPLocalDe
    module procedure ESMF_DistGridGetPLocalDePDim
      
! !DESCRIPTION: 
! This interface provides a single entry point for the various 
!  types of {\tt ESMF\_DistGridGet} functions.   
!EOPI 
  end interface
!==============================================================================

!===============================================================================
! DistGridOperator() interfaces
!===============================================================================

! -------------------------- ESMF-public interface ----------------------------
!BOP
! !IROUTINE: ESMF_DistGridAssignment(=) - DistGrid assignment
!
! !INTERFACE:
!   interface assignment(=)
!   distgrid1 = distgrid2
!
! !ARGUMENTS:
!   type(ESMF_DistGrid) :: distgrid1
!   type(ESMF_DistGrid) :: distgrid2
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
!   Assign distgrid1 as an alias to the same ESMF DistGrid object in memory
!   as distgrid2. If distgrid2 is invalid, then distgrid1 will be equally
!   invalid after the assignment.
!
!   The arguments are:
!   \begin{description}
!   \item[distgrid1]
!     The {\tt ESMF\_DistGrid} object on the left hand side of the assignment.
!   \item[distgrid2]
!     The {\tt ESMF\_DistGrid} object on the right hand side of the assignment.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------

! -------------------------- ESMF-public interface ----------------------------
!BOP
! !IROUTINE: ESMF_DistGridOperator(==) - DistGrid equality operator
!
! !INTERFACE:
  interface operator(==)
!   if (distgrid1 == distgrid2) then ... endif
!             OR
!   result = (distgrid1 == distgrid2)
! !RETURN VALUE:
!   logical :: result
!
! !ARGUMENTS:
!   type(ESMF_DistGrid), intent(in) :: distgrid1
!   type(ESMF_DistGrid), intent(in) :: distgrid2
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
!   Test whether distgrid1 and distgrid2 are valid aliases to the same ESMF
!   DistGrid object in memory. For a more general comparison of two 
!   ESMF DistGrids, going beyond the simple alias test, the 
!   {\tt ESMF\_DistGridMatch()} function (not yet fully implemented) must 
!   be used.
!
!   The arguments are:
!   \begin{description}
!   \item[distgrid1]
!     The {\tt ESMF\_DistGrid} object on the left hand side of the equality
!     operation.
!   \item[distgrid2]
!     The {\tt ESMF\_DistGrid} object on the right hand side of the equality
!     operation.
!   \end{description}
!
!EOP
    module procedure ESMF_DistGridEQ

  end interface
!------------------------------------------------------------------------------

! -------------------------- ESMF-public interface ----------------------------
!BOP
! !IROUTINE: ESMF_DistGridOperator(/=) - DistGrid not equal operator
!
! !INTERFACE:
  interface operator(/=)
!   if (distgrid1 /= distgrid2) then ... endif
!             OR
!   result = (distgrid1 /= distgrid2)
! !RETURN VALUE:
!   logical :: result
!
! !ARGUMENTS:
!   type(ESMF_DistGrid), intent(in) :: distgrid1
!   type(ESMF_DistGrid), intent(in) :: distgrid2
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
!   Test whether distgrid1 and distgrid2 are {\it not} valid aliases to the
!   same ESMF DistGrid object in memory. For a more general comparison of two
!   ESMF DistGrids, going beyond the simple alias test, the
!   {\tt ESMF\_DistGridMatch()} function (not yet fully implemented) must 
!   be used.
!
!   The arguments are:
!   \begin{description}
!   \item[distgrid1]
!     The {\tt ESMF\_DistGrid} object on the left hand side of the non-equality
!     operation.
!   \item[distgrid2]
!     The {\tt ESMF\_DistGrid} object on the right hand side of the non-equality
!     operation.
!   \end{description}
!
!EOP
    module procedure ESMF_DistGridNE

  end interface
!------------------------------------------------------------------------------


! -------------------------- ESMF-internal interface --------------------------
  interface operator(==)
    module procedure ESMF_DistGridMatch_FlagEQ
  end interface
!------------------------------------------------------------------------------


! -------------------------- ESMF-internal interface --------------------------
  interface operator(/=)
    module procedure ESMF_DistGridMatch_FlagNE
  end interface
!------------------------------------------------------------------------------


! -------------------------- ESMF-internal interface --------------------------
  interface operator(<)
    module procedure ESMF_DistGridMatch_FlagLT
  end interface
!------------------------------------------------------------------------------


! -------------------------- ESMF-internal interface --------------------------
  interface operator(>)
    module procedure ESMF_DistGridMatch_FlagGT
  end interface
!------------------------------------------------------------------------------


! -------------------------- ESMF-internal interface --------------------------
  interface operator(<=)
    module procedure ESMF_DistGridMatch_FlagLE
  end interface
!------------------------------------------------------------------------------


! -------------------------- ESMF-internal interface --------------------------
  interface operator(>=)
    module procedure ESMF_DistGridMatch_FlagGE
  end interface
!------------------------------------------------------------------------------


! -------------------------- ESMF-internal interface --------------------------
  interface operator(==)
    module procedure ESMF_Decomp_FlagEQ
  end interface
!------------------------------------------------------------------------------


! -------------------------- ESMF-internal interface --------------------------
  interface operator(/=)
    module procedure ESMF_Decomp_FlagNE
  end interface
!------------------------------------------------------------------------------

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


! -------------------------- ESMF-internal method -----------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DistGridEQ()"
!BOPI
! !IROUTINE:  ESMF_DistGridEQ - Compare two DistGrids for equality
!
! !INTERFACE:
  function ESMF_DistGridEQ(distgrid1, distgrid2)
! 
! !RETURN VALUE:
    logical :: ESMF_DistGridEQ

! !ARGUMENTS:
    type(ESMF_DistGrid), intent(in) :: distgrid1
    type(ESMF_DistGrid), intent(in) :: distgrid2

! !DESCRIPTION:
!   Test if both {\tt distgrid1} and {\tt distgrid2} alias the same ESMF DistGrid 
!   object.
!
!EOPI
!-------------------------------------------------------------------------------

    ESMF_INIT_TYPE init1, init2
    integer :: localrc1, localrc2
    logical :: lval1, lval2

    ! Use the following logic, rather than "ESMF-INIT-CHECK-DEEP", to gain 
    ! init checks on both args, and in the case where both are uninitialized,
    ! to distinguish equality based on uninitialized type (uncreated,
    ! deleted).

    ! TODO: Consider moving this logic to C++: use Base class? status?
    !       Or replicate logic for C interface also.

    ! check inputs
    init1 = ESMF_DistGridGetInit(distgrid1)
    init2 = ESMF_DistGridGetInit(distgrid2)

    ! TODO: this line must remain split in two for SunOS f90 8.3 127000-03
    if (init1 .eq. ESMF_INIT_CREATED .and. &
      init2 .eq. ESMF_INIT_CREATED) then
      ESMF_DistGridEQ = distgrid1%this .eq. distgrid2%this
    else
      ESMF_DistGridEQ = .false.
    endif

  end function ESMF_DistGridEQ
!-------------------------------------------------------------------------------


! -------------------------- ESMF-internal method -----------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DistGridNE()"
!BOPI
! !IROUTINE:  ESMF_DistGridNE - Compare two DistGrids for non-equality
!
! !INTERFACE:
  function ESMF_DistGridNE(distgrid1, distgrid2)
! 
! !RETURN VALUE:
    logical :: ESMF_DistGridNE

! !ARGUMENTS:
    type(ESMF_DistGrid), intent(in) :: distgrid1
    type(ESMF_DistGrid), intent(in) :: distgrid2

! !DESCRIPTION:
!   Test if both {\tt distgrid1} and {\tt distgrid2} alias the same
!   ESMF DistGrid object.
!
!EOPI
!-------------------------------------------------------------------------------

    ESMF_DistGridNE = .not.ESMF_DistGridEQ(distgrid1, distgrid2)

  end function ESMF_DistGridNE
!-------------------------------------------------------------------------------


! -------------------------- ESMF-internal method -----------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DistGridMatch_FlagEQ()"
!BOPI
! !IROUTINE:  ESMF_DistGridMatch_FlagEQ - Compare two DistGridMatch_Flags
!
! !INTERFACE:
  function ESMF_DistGridMatch_FlagEQ(dgmt1, dgmt2)
! 
! !RETURN VALUE:
    logical :: ESMF_DistGridMatch_FlagEQ

! !ARGUMENTS:
    type(ESMF_DistGridMatch_Flag), intent(in) :: dgmt1
    type(ESMF_DistGridMatch_Flag), intent(in) :: dgmt2

! !DESCRIPTION:
!
!EOPI
!-------------------------------------------------------------------------------

    ESMF_DistGridMatch_FlagEQ = dgmt1%value .eq. dgmt2%value

  end function ESMF_DistGridMatch_FlagEQ
!-------------------------------------------------------------------------------


! -------------------------- ESMF-internal method -----------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DistGridMatch_FlagNE()"
!BOPI
! !IROUTINE:  ESMF_DistGridMatch_FlagNE - Compare two DistGridMatch_Flags
!
! !INTERFACE:
  function ESMF_DistGridMatch_FlagNE(dgmt1, dgmt2)
! 
! !RETURN VALUE:
    logical :: ESMF_DistGridMatch_FlagNE

! !ARGUMENTS:
    type(ESMF_DistGridMatch_Flag), intent(in) :: dgmt1
    type(ESMF_DistGridMatch_Flag), intent(in) :: dgmt2

! !DESCRIPTION:
!
!EOPI
!-------------------------------------------------------------------------------

    ESMF_DistGridMatch_FlagNE = .not.ESMF_DistGridMatch_FlagEQ(dgmt1, dgmt2)

  end function ESMF_DistGridMatch_FlagNE
!-------------------------------------------------------------------------------


! -------------------------- ESMF-internal method -----------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DistGridMatch_FlagLT()"
!BOPI
! !IROUTINE:  ESMF_DistGridMatch_FlagLT - Compare two DistGridMatch_Flags
!
! !INTERFACE:
  function ESMF_DistGridMatch_FlagLT(dgmt1, dgmt2)
! 
! !RETURN VALUE:
    logical :: ESMF_DistGridMatch_FlagLT

! !ARGUMENTS:
    type(ESMF_DistGridMatch_Flag), intent(in) :: dgmt1
    type(ESMF_DistGridMatch_Flag), intent(in) :: dgmt2

! !DESCRIPTION:
!
!EOPI
!-------------------------------------------------------------------------------

    ESMF_DistGridMatch_FlagLT = dgmt1%value .lt. dgmt2%value

  end function ESMF_DistGridMatch_FlagLT
!-------------------------------------------------------------------------------


! -------------------------- ESMF-internal method -----------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DistGridMatch_FlagGT()"
!BOPI
! !IROUTINE:  ESMF_DistGridMatch_FlagGT - Compare two DistGridMatch_Flags
!
! !INTERFACE:
  function ESMF_DistGridMatch_FlagGT(dgmt1, dgmt2)
! 
! !RETURN VALUE:
    logical :: ESMF_DistGridMatch_FlagGT

! !ARGUMENTS:
    type(ESMF_DistGridMatch_Flag), intent(in) :: dgmt1
    type(ESMF_DistGridMatch_Flag), intent(in) :: dgmt2

! !DESCRIPTION:
!
!EOPI
!-------------------------------------------------------------------------------

    ESMF_DistGridMatch_FlagGT = dgmt1%value .gt. dgmt2%value

  end function ESMF_DistGridMatch_FlagGT
!-------------------------------------------------------------------------------


! -------------------------- ESMF-internal method -----------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DistGridMatch_FlagLE()"
!BOPI
! !IROUTINE:  ESMF_DistGridMatch_FlagLE - Compare two DistGridMatch_Flags
!
! !INTERFACE:
  function ESMF_DistGridMatch_FlagLE(dgmt1, dgmt2)
! 
! !RETURN VALUE:
    logical :: ESMF_DistGridMatch_FlagLE

! !ARGUMENTS:
    type(ESMF_DistGridMatch_Flag), intent(in) :: dgmt1
    type(ESMF_DistGridMatch_Flag), intent(in) :: dgmt2

! !DESCRIPTION:
!
!EOPI
!-------------------------------------------------------------------------------

    ESMF_DistGridMatch_FlagLE = dgmt1%value .le. dgmt2%value

  end function ESMF_DistGridMatch_FlagLE
!-------------------------------------------------------------------------------


! -------------------------- ESMF-internal method -----------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DistGridMatch_FlagGE()"
!BOPI
! !IROUTINE:  ESMF_DistGridMatch_FlagGE - Compare two DistGridMatch_Flags
!
! !INTERFACE:
  function ESMF_DistGridMatch_FlagGE(dgmt1, dgmt2)
! 
! !RETURN VALUE:
    logical :: ESMF_DistGridMatch_FlagGE

! !ARGUMENTS:
    type(ESMF_DistGridMatch_Flag), intent(in) :: dgmt1
    type(ESMF_DistGridMatch_Flag), intent(in) :: dgmt2

! !DESCRIPTION:
!
!EOPI
!-------------------------------------------------------------------------------

    ESMF_DistGridMatch_FlagGE = dgmt1%value .ge. dgmt2%value

  end function ESMF_DistGridMatch_FlagGE
!-------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DistGridCreateDG()"
!BOP
! !IROUTINE: ESMF_DistGridCreate - Create DistGrid object from DistGrid

! !INTERFACE:
  ! Private name; call using ESMF_DistGridCreate()
  function ESMF_DistGridCreateDG(distgrid, keywordEnforcer, &
    firstExtra, lastExtra, indexflag, connectionList, vm, rc)
!         
! !RETURN VALUE:
    type(ESMF_DistGrid) :: ESMF_DistGridCreateDG
!
! !ARGUMENTS:
    type(ESMF_DistGrid),           intent(in)            :: distgrid
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer, target,               intent(in),  optional :: firstExtra(:)
    integer, target,               intent(in),  optional :: lastExtra(:)
    type(ESMF_Index_Flag),         intent(in),  optional :: indexflag
    type(ESMF_DistGridConnection), intent(in),  optional :: connectionList(:)
    type(ESMF_VM),                 intent(in),  optional :: vm
    integer,                       intent(out), optional :: rc
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \item\apiStatusModifiedSinceVersion{5.2.0r}
! \begin{description}
! \item[6.3.0r] Added argument {\tt vm} to support object creation on a
!               different VM than that of the current context.
! \end{description}
! \end{itemize}
!
! !DESCRIPTION:
!     Create a new DistGrid from an existing DistGrid, keeping the decomposition
!     unchanged. The {\tt firstExtra} and {\tt lastExtra} arguments allow extra
!     elements to be added at the first/last edge DE in each dimension. The 
!     method also allows the {\tt indexflag} to be set. Further, if the 
!     {\tt connectionList} argument is provided it will be used to set 
!     connections in the newly created DistGrid, otherwise the connections of
!     the incoming DistGrid will be used.
!     If neither {\tt firstExtra}, {\tt lastExtra}, {\tt indexflag}, nor 
!     {\tt connectionList} arguments are specified, the method reduces to a 
!     deep copy of the incoming DistGrid object.
!
!     The arguments are:
!     \begin{description}
!     \item[distgrid]
!          Incoming DistGrid object.
!     \item[{[firstExtra]}]
!          Extra elements added to the first DE along each 
!          dimension. This increases the size of the index space compared to 
!          that of the incoming {\tt distgrid}. The decomposition of the
!          enlarged index space is constructed to align with the original index
!          space provided by {\tt distgrid}.
!          The default is a zero vector.
!     \item[{[lastExtra]}]
!          Extra elements added to the last DE along each 
!          dimension. This increases the size of the index space compared to 
!          that of the incoming {\tt distgrid}. The decomposition of the
!          enlarged index space is constructed to align with the original index
!          space provided by {\tt distgrid}.
!          The default is a zero vector.
!     \item[{[indexflag]}]
!          If present, override the indexflag setting of the incoming
!          {\tt distgrid}. See section \ref{const:indexflag} for a 
!          complete list of options. By default use the indexflag setting of 
!          {\tt distgrid}. 
!     \item[{[connectionList]}]
!          If present, override the connections of the incoming {\tt distgrid}.
!          See section \ref{api:DistGridConnectionSet} for the associated Set()
!          method. By default use the connections definded in {\tt distgrid}.
!     \item[{[vm]}]
!          If present, the DistGrid object and the DELayout object
!          are created on the specified {\tt ESMF\_VM} object. The 
!          default is to use the VM of the current context. 
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc            ! local return code
    type(ESMF_DistGrid)   :: dg                 ! opaque pointer to new C++ DistGrid
    type(ESMF_InterArray) :: firstExtraAux      ! helper variable
    type(ESMF_InterArray) :: lastExtraAux       ! helper variable
    type(ESMF_InterArray) :: connectionListAux  ! helper variable

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    
    ! invalidate return value    
    dg%this = ESMF_NULL_POINTER
    ESMF_DistGridCreateDG = dg 

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_DistGridGetInit, distgrid, rc)
    
    ! Deal with (optional) array arguments
    firstExtraAux = ESMF_InterArrayCreate(firstExtra, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    lastExtraAux = ESMF_InterArrayCreate(lastExtra, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    connectionListAux = ESMF_InterArrayCreateDGConn(connectionList, &
      rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Mark this DistGrid as invalid
    dg%this = ESMF_NULL_POINTER

    ! call into the C++ interface, which will sort out optional arguments
    call c_ESMC_DistGridCreateDG(dg, distgrid, firstExtraAux, &
      lastExtraAux, indexflag, connectionListAux, vm, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
      
    ! garbage collection
    call ESMF_InterArrayDestroy(firstExtraAux, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterArrayDestroy(lastExtraAux, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterArrayDestroy(connectionListAux, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    
    ! Set return value
    ESMF_DistGridCreateDG = dg 
 
    ! Set init code
    ESMF_INIT_SET_CREATED(ESMF_DistGridCreateDG)
 
    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS
 
  end function ESMF_DistGridCreateDG
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DistGridCreateDGT()"
!BOP
! !IROUTINE: ESMF_DistGridCreate - Create DistGrid object from DistGrid (multi-tile version)

! !INTERFACE:
  ! Private name; call using ESMF_DistGridCreate()
  function ESMF_DistGridCreateDGT(distgrid, firstExtraPTile, &
    lastExtraPTile, keywordEnforcer, indexflag, connectionList, vm, rc)
!         
! !RETURN VALUE:
    type(ESMF_DistGrid) :: ESMF_DistGridCreateDGT
!
! !ARGUMENTS:
    type(ESMF_DistGrid),           intent(in)            :: distgrid
    integer, target,               intent(in)            :: firstExtraPTile(:,:)
    integer, target,               intent(in)            :: lastExtraPTile(:,:)
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_Index_Flag),         intent(in),  optional :: indexflag
    type(ESMF_DistGridConnection), intent(in),  optional :: connectionList(:)
    type(ESMF_VM),                 intent(in),  optional :: vm
    integer,                       intent(out), optional :: rc
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \item\apiStatusModifiedSinceVersion{5.2.0r}
! \begin{description}
! \item[6.3.0r] Added argument {\tt vm} to support object creation on a
!               different VM than that of the current context.
! \end{description}
! \end{itemize}
!
! !DESCRIPTION:
!     Create a new DistGrid from an existing DistGrid, keeping the decomposition
!     unchanged. The {\tt firstExtraPTile} and {\tt lastExtraPTile} arguments allow extra
!     elements to be added at the first/last edge DE in each dimension. The 
!     method also allows the {\tt indexflag} to be set. Further, if the 
!     {\tt connectionList} argument provided in it will be used to set 
!     connections in the newly created DistGrid, otherwise the connections of
!     the incoming DistGrid will be used.
!     If neither {\tt firstExtraPTile}, {\tt lastExtraPTile}, {\tt indexflag}, nor 
!     {\tt connectionList} arguments are specified, the method reduces to a 
!     deep copy of the incoming DistGrid object.
!
!     The arguments are:
!     \begin{description}
!     \item[distgrid]
!          Incoming DistGrid object.
!     \item[firstExtraPTile]
!          Extra elements added to the first DE along each dimension for each 
!          tile. This increases the size of the index space compared to 
!          that of the incoming {\tt distgrid}. The decomposition of the
!          enlarged index space is constructed to align with the original index
!          space provided by {\tt distgrid}.
!          The default is a zero vector.
!     \item[lastExtraPTile]
!          Extra elements added to the last DE along each dimension for each
!          tile. This increases the size of the index space compared to 
!          that of the incoming {\tt distgrid}. The decomposition of the
!          enlarged index space is constructed to align with the original index
!          space provided by {\tt distgrid}.
!          The default is a zero vector.
!     \item[{[indexflag]}]
!          If present, override the indexflag setting of the incoming
!          {\tt distgrid}. See section \ref{const:indexflag} for a 
!          complete list of options. By default use the indexflag setting of 
!          {\tt distgrid}. 
!     \item[{[connectionList]}]
!          If present, override the connections of the incoming {\tt distgrid}.
!          See section \ref{api:DistGridConnectionSet} for the associated Set()
!          method. By default use the connections definded in {\tt distgrid}.
!     \item[{[vm]}]
!          If present, the DistGrid object and the DELayout object
!          are created on the specified {\tt ESMF\_VM} object. The 
!          default is to use the VM of the current context. 
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc      ! local return code
    type(ESMF_DistGrid)   :: dg           ! opaque pointer to new C++ DistGrid
    type(ESMF_InterArray) :: firstExtraAux ! helper variable
    type(ESMF_InterArray) :: lastExtraAux ! helper variable
    type(ESMF_InterArray) :: connectionListAux ! helper variable

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    
    ! invalidate return value    
    dg%this = ESMF_NULL_POINTER
    ESMF_DistGridCreateDGT = dg 

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_DistGridGetInit, distgrid, rc)
    
    ! Deal with (optional) array arguments
    firstExtraAux = ESMF_InterArrayCreate(farray2D=firstExtraPTile, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    lastExtraAux = ESMF_InterArrayCreate(farray2D=lastExtraPTile, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    connectionListAux = ESMF_InterArrayCreateDGConn(connectionList, &
      rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Mark this DistGrid as invalid
    dg%this = ESMF_NULL_POINTER

    ! call into the C++ interface, which will sort out optional arguments
    call c_ESMC_DistGridCreateDG(dg, distgrid, firstExtraAux, &
      lastExtraAux, indexflag, connectionListAux, vm, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
      
    ! garbage collection
    call ESMF_InterArrayDestroy(firstExtraAux, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterArrayDestroy(lastExtraAux, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterArrayDestroy(connectionListAux, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    
    ! Set return value
    ESMF_DistGridCreateDGT = dg 
 
    ! Set init code
    ESMF_INIT_SET_CREATED(ESMF_DistGridCreateDGT)
 
    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS
 
  end function ESMF_DistGridCreateDGT
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DistGridCreateRD()"
!BOP
! !IROUTINE: ESMF_DistGridCreate - Create DistGrid object with regular decomposition

! !INTERFACE:
  ! Private name; call using ESMF_DistGridCreate()
  function ESMF_DistGridCreateRD(minIndex, maxIndex, keywordEnforcer, regDecomp, &
    decompflag, regDecompFirstExtra, regDecompLastExtra, deLabelList, &
    indexflag, connectionList, delayout, vm, rc)
!         
! !RETURN VALUE:
    type(ESMF_DistGrid) :: ESMF_DistGridCreateRD
!
! !ARGUMENTS:
    integer,                        intent(in)            :: minIndex(:)
    integer,                        intent(in)            :: maxIndex(:)
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,                target, intent(in),  optional :: regDecomp(:)
    type(ESMF_Decomp_Flag), target, intent(in),  optional :: decompflag(:)
    integer,                target, intent(in),  optional :: regDecompFirstExtra(:)
    integer,                target, intent(in),  optional :: regDecompLastExtra(:)
    integer,                target, intent(in),  optional :: deLabelList(:)
    type(ESMF_Index_Flag),          intent(in),  optional :: indexflag
    type(ESMF_DistGridConnection),  intent(in),  optional :: connectionList(:)
    type(ESMF_DELayout),            intent(in),  optional :: delayout
    type(ESMF_VM),                  intent(in),  optional :: vm
    integer,                        intent(out), optional :: rc
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
!     Create an {\tt ESMF\_DistGrid} from a single logically rectangular tile.
!     The tile has a regular decomposition, where the tile is decomposed
!     into a fixed number of DEs along each dimension. A regular decomposition
!     of a single tile is expressed by a single {\tt regDecomp} list of DE 
!     counts in each dimension.
!
!     The arguments are:
!     \begin{description}
!     \item[minIndex]
!          Index space tuple of the lower corner of the single tile.
!     \item[maxIndex]
!          Index space tuple of the upper corner of the single tile.
!     \item[{[regDecomp]}]
!          List of DE counts for each dimension. The total {\tt deCount} is
!          determined as the product of {\tt regDecomp} elements.
!          By default {\tt regDecomp} = (/{\tt deCount},1,...,1/), 
!          where {\tt deCount}
!          is the number of DEs in the {\tt delayout}. If the default
!          {\tt delayout} is used, the {\tt deCount} is equal to {\tt petCount}.
!          This leads to a simple 1 DE per PET distribution, where the
!          decompsition is only along the first dimension.
!     \item[{[decompflag]}]
!          List of decomposition flags indicating how each dimension of the
!          tile is to be divided between the DEs. The default setting
!          is {\tt ESMF\_DECOMP\_BALANCED} in all dimensions. See section
!          \ref{const:decompflag} for a list of valid decomposition options.
!     \item[{[regDecompFirstExtra]}]
!          Specify how many extra elements on the first DEs along each 
!          dimension to consider when applying the regular decomposition 
!          algorithm. This does {\em not} add extra elements to the 
!          index space defined by {\tt minIndex} and {\tt maxIndex}. Instead
!          {\tt regDecompFirstExtra} is used to correctly interpret the 
!          specified index space: The {\tt regDecomp} is first applied to the
!          index space {\em without} the extra elements. The extra elements are
!          then added back in to arrive at the final decomposition. This is 
!          useful when aligning the decomposition of index spaces that only
!          differ in extra elements along the edges, e.g. when dealing with
!          different stagger locations.
!          The default is a zero vector, assuming no extra elements.
!     \item[{[regDecompLastExtra]}]
!          Specify how many extra elements on the last DEs along each 
!          dimension to consider when applying the regular decomposition 
!          algorithm. This does {\em not} add extra elements to the 
!          index space defined by {\tt minIndex} and {\tt maxIndex}. Instead
!          {\tt regDecompLastExtra} is used to correctly interpret the 
!          specified index space: The {\tt regDecomp} is first applied to the
!          index space {\em without} the extra elements. The extra elements are
!          then added back in to arrive at the final decomposition. This is 
!          useful when aligning the decomposition of index spaces that only
!          differ in extra elements along the edges, e.g. when dealing with
!          different stagger locations.
!          The default is a zero vector, assuming no extra elements.
!     \item[{[deLabelList]}]
!          List assigning DE labels to the default sequence of DEs. The default
!          sequence is given by the column major order of the {\tt regDecomp}
!          argument.
!     \item[{[indexflag]}]
!          Indicates whether the indices provided by the {\tt minIndex} and
!          {\tt maxIndex} arguments are forming a global
!          index space or not. This does {\em not} affect the indices held
!          by the DistGrid object, which are always identical to what was
!          specified by {\tt minIndex} and {\tt maxIndex}, regardless of the
!          {\tt indexflag} setting. However, it does affect whether an
!          {\tt ESMF\_Array} object created on the DistGrid can choose global
!          indexing or not. The default is {\tt ESMF\_INDEX\_DELOCAL}.
!          See section \ref{const:indexflag} for a complete list of options.
!     \item[{[connectionList]}]
!          List of {\tt ESMF\_DistGridConnection} objects, defining connections
!          between DistGrid tiles in index space.
!          See section \ref{api:DistGridConnectionSet} for the associated Set()
!          method.
!     \item[{[delayout]}]
!          {\tt ESMF\_DELayout} object to be used. If a DELayout object is
!          specified its {\tt deCount} must match the number indicated by 
!          {\tt regDecomp}. By default a new DELayout object will be created 
!          with the correct number of DEs.
!     \item[{[vm]}]
!          If present, the DistGrid object (and the DELayout object if not 
!          provided) are created on the specified {\tt ESMF\_VM} object. The 
!          default is to use the VM of the current context. 
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc      ! local return code
    type(ESMF_DistGrid)   :: distgrid     ! opaque pointer to new C++ DistGrid
    type(ESMF_InterArray) :: minIndexAux  ! helper variable
    type(ESMF_InterArray) :: maxIndexAux  ! helper variable
    type(ESMF_InterArray) :: regDecompAux ! helper variable
    type(ESMF_Decomp_Flag), target   :: dummyDf(0)  ! satisfy C interface
    type(ESMF_Decomp_Flag), pointer  ::  opt_decompflag(:) ! optional arg helper
    integer                 :: len_decompflag ! helper variable
    type(ESMF_InterArray) :: regDecompFirstExtraAux ! helper variable
    type(ESMF_InterArray) :: regDecompLastExtraAux ! helper variable
    type(ESMF_InterArray) :: deLabelListAux ! helper variable
    type(ESMF_InterArray) :: connectionListAux ! helper variable

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    
    ! invalidate return value    
    distgrid%this = ESMF_NULL_POINTER
    ESMF_DistGridCreateRD = distgrid 
    
    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_DELayoutGetInit, delayout, rc)
    ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc)
    
    ! Deal with (optional) array arguments
    minIndexAux = ESMF_InterArrayCreate(minIndex, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    maxIndexAux = ESMF_InterArrayCreate(maxIndex, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    regDecompAux = ESMF_InterArrayCreate(regDecomp, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    if (present(decompflag)) then
      len_decompflag = size(decompflag)
      opt_decompflag => decompflag
    else
      len_decompflag = 0
      opt_decompflag => dummyDf
    endif
    regDecompFirstExtraAux = ESMF_InterArrayCreate(regDecompFirstExtra, &
      rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    regDecompLastExtraAux = ESMF_InterArrayCreate(regDecompLastExtra, &
      rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    deLabelListAux = ESMF_InterArrayCreate(deLabelList, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    connectionListAux = ESMF_InterArrayCreateDGConn(connectionList, &
      rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Mark this DistGrid as invalid
    distgrid%this = ESMF_NULL_POINTER

    ! call into the C++ interface, which will sort out optional arguments
    call c_ESMC_DistGridCreateRD(distgrid, minIndexAux, maxIndexAux, &
      regDecompAux, opt_decompflag, len_decompflag, regDecompFirstExtraAux, &
      regDecompLastExtraAux, deLabelListAux, indexflag, &
      connectionListAux, delayout, vm, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
      
    ! garbage collection
    call ESMF_InterArrayDestroy(minIndexAux, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterArrayDestroy(maxIndexAux, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterArrayDestroy(regDecompAux, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterArrayDestroy(regDecompFirstExtraAux, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterArrayDestroy(regDecompLastExtraAux, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterArrayDestroy(deLabelListAux, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterArrayDestroy(connectionListAux, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    
    ! Set return value
    ESMF_DistGridCreateRD = distgrid 
 
    ! Set init code
    ESMF_INIT_SET_CREATED(ESMF_DistGridCreateRD)
 
    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS
 
  end function ESMF_DistGridCreateRD
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DistGridCreateRDT()"
!BOP
! !IROUTINE: ESMF_DistGridCreate - Create DistGrid object with regular decomposition (multi-tile version)

! !INTERFACE:
  ! Private name; call using ESMF_DistGridCreate()
  function ESMF_DistGridCreateRDT(minIndexPTile, maxIndexPTile, &
    keywordEnforcer, regDecompPTile, decompflagPTile, regDecompFirstExtraPTile,&
    regDecompLastExtraPTile, deLabelList, indexflag, connectionList, &
    delayout, vm, rc)
!         
! !RETURN VALUE:
    type(ESMF_DistGrid) :: ESMF_DistGridCreateRDT
!
! !ARGUMENTS:
    integer,                        intent(in)            :: minIndexPTile(:,:)
    integer,                        intent(in)            :: maxIndexPTile(:,:)
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,                        intent(in),  optional :: regDecompPTile(:,:)
    type(ESMF_Decomp_Flag), target, intent(in),  optional :: decompflagPTile(:,:)
    integer,                target, intent(in),  optional :: regDecompFirstExtraPTile(:,:)
    integer,                target, intent(in),  optional :: regDecompLastExtraPTile(:,:)
    integer,                        intent(in),  optional :: deLabelList(:)
    type(ESMF_Index_Flag),          intent(in),  optional :: indexflag
    type(ESMF_DistGridConnection),  intent(in),  optional :: connectionList(:)
    type(ESMF_DELayout),            intent(in),  optional :: delayout
    type(ESMF_VM),                  intent(in),  optional :: vm
    integer,                        intent(out), optional :: rc
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
!     Create an {\tt ESMF\_DistGrid} from multiple logically rectangular tiles. 
!     Each tile has a regular decomposition, where the tile is decomposed
!     into a fixed number of DEs along each dimension. A regular decomposition
!     of a multi-tile DistGrid is expressed by a list of DE count vectors, one
!     vector for each tile. If a DELayout is specified, it must contain at least
!     as many DEs as there are tiles.
!
!     The arguments are:
!     \begin{description}
!     \item[minIndexPTile]
!          The first index provides the index space tuple of the lower 
!          corner of a tile. The second index indicates the tile number.
!     \item[maxIndexPTile]
!          The first index provides the index space tuple of the upper
!          corner of a tile. The second index indicates the tile number.
!     \item[{[regDecompPTile]}]
!          List of DE counts for each dimension. The second index steps through
!          the tiles. The total {\tt deCount} is determined as ths sum over
!          the products of {\tt regDecomp} elements for each tile.
!          By default each tile is decomposed only along the first dimension.
!          The default number of DEs per tile is at least 1, but may be greater
!          for the leading tiles if the {\tt deCount} is greater than the 
!          {\tt tileCount}. If no DELayout is specified, the {\tt deCount} is 
!          by default set equal to the number of PETs ({\tt petCount}), or the 
!          number of tiles ({\tt tileCount}), which ever is greater. This means
!          that as long as {\tt petCount} > {\tt tileCount}, the resulting
!          default distribution will be 1 DE per PET. Notice that some tiles
!          may be decomposed into more DEs than other tiles.
!     \item[{[decompflagPTile]}]
!          List of decomposition flags indicating how each dimension of each
!          tile is to be divided between the DEs. The default setting
!          is {\tt ESMF\_DECOMP\_BALANCED} in all dimensions for all tiles. 
!          See section \ref{const:decompflag} for a list of valid decomposition
!          flag options. The second index indicates the tile number.
!     \item[{[regDecompFirstExtraPTile]}]
!          Specify how many extra elements on the first DEs along each 
!          dimension to consider when applying the regular decomposition 
!          algorithm. This does {\em not} add extra elements to the 
!          index space defined by {\tt minIndex} and {\tt maxIndex}. Instead
!          {\tt regDecompFirstExtraPTile} is used to correctly interpret the 
!          specified index space: The {\tt regDecomp} is first applied to the
!          index space {\em without} the extra elements. The extra elements are
!          then added back in to arrive at the final decomposition. This is 
!          useful when aligning the decomposition of index spaces that only
!          differ in extra elements along the edges, e.g. when dealing with
!          different stagger locations.
!          The default is a zero vector, assuming no extra elements.
!     \item[{[regDecompLastExtraPTile]}]
!          Specify how many extra elements on the last DEs along each 
!          dimension to consider when applying the regular decomposition 
!          algorithm. This does {\em not} add extra elements to the 
!          index space defined by {\tt minIndex} and {\tt maxIndex}. Instead
!          {\tt regDecompLastExtraPTile} is used to correctly interpret the 
!          specified index space: The {\tt regDecomp} is first applied to the
!          index space {\em without} the extra elements. The extra elements are
!          then added back in to arrive at the final decomposition. This is 
!          useful when aligning the decomposition of index spaces that only
!          differ in extra elements along the edges, e.g. when dealing with
!          different stagger locations.
!          The default is a zero vector, assuming no extra elements.
!     \item[{[deLabelList]}]
!          List assigning DE labels to the default sequence of DEs. The default
!          sequence is given by the column major order of the {\tt regDecompPTile}
!          elements in the sequence as they appear following the tile index.
!     \item[{[indexflag]}]
!          Indicates whether the indices provided by the {\tt minIndexPTile} and
!          {\tt maxIndexPTile} arguments are forming a global index space or 
!          not. This does {\em not} affect the indices held by the DistGrid 
!          object, which are always identical to what was specified by 
!          {\tt minIndexPTile} and {\tt maxIndexPTile}, regardless of the
!          {\tt indexflag} setting. However, it does affect whether an
!          {\tt ESMF\_Array} object created on the DistGrid can choose global
!          indexing or not. The default is {\tt ESMF\_INDEX\_DELOCAL}.
!          See section \ref{const:indexflag} for a complete list of options.
!     \item[{[connectionList]}]
!          List of {\tt ESMF\_DistGridConnection} objects, defining connections
!          between DistGrid tiles in index space.
!          See section \ref{api:DistGridConnectionSet} for the associated Set()
!          method.
!     \item[{[delayout]}]
!          Optional {\tt ESMF\_DELayout} object to be used. By default a new
!          DELayout object will be created with as many DEs as there are PETs,
!          or tiles, which ever is greater. If a DELayout object is specified,
!          the number of DEs must match {\tt regDecompPTile}, if present. In the
!          case that {\tt regDecompPTile} was not specified, the {\tt deCount}
!          must be at least that of the default DELayout. The 
!          {\tt regDecompPTile} will be constructed accordingly.
!     \item[{[vm]}]
!          Optional {\tt ESMF\_VM} object of the current context. Providing the
!          VM of the current context will lower the method's overhead.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc      ! local return code
    type(ESMF_DistGrid)   :: distgrid     ! opaque pointer to new C++ DistGrid
    type(ESMF_InterArray) :: minIndexAux  ! helper variable
    type(ESMF_InterArray) :: maxIndexAux  ! helper variable
    type(ESMF_InterArray) :: regDecompAux ! helper variable
    type(ESMF_Decomp_Flag), target :: dummyDf(0,0)  ! satisfy C interface
    type(ESMF_Decomp_Flag), pointer::  opt_decompflag(:,:) ! optional arg helper
    integer               :: len1_decompflag ! helper variable
    integer               :: len2_decompflag ! helper variable
    type(ESMF_InterArray) :: regDecompFirstExtraAux ! helper variable
    type(ESMF_InterArray) :: regDecompLastExtraAux ! helper variable
    type(ESMF_InterArray) :: deLabelListAux ! helper variable
    type(ESMF_InterArray) :: connectionListAux ! helper variable

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    
    ! invalidate return value    
    distgrid%this = ESMF_NULL_POINTER
    ESMF_DistGridCreateRDT = distgrid 
    
    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_DELayoutGetInit, delayout, rc)
    ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc)
    
    ! Deal with (optional) array arguments
    minIndexAux = ESMF_InterArrayCreate(farray2D=minIndexPTile, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    maxIndexAux = ESMF_InterArrayCreate(farray2D=maxIndexPTile, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    regDecompAux = ESMF_InterArrayCreate(farray2D=regDecompPTile, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    if (present(decompflagPTile)) then
      len1_decompflag = size(decompflagPTile, 1)
      len2_decompflag = size(decompflagPTile, 2)
      opt_decompflag => decompflagPTile
    else
      len1_decompflag = 0
      len2_decompflag = 0
      opt_decompflag => dummyDf
    endif
    regDecompFirstExtraAux = &
      ESMF_InterArrayCreate(farray2D=regDecompFirstExtraPTile, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    regDecompLastExtraAux = &
      ESMF_InterArrayCreate(farray2D=regDecompLastExtraPTile, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    deLabelListAux = ESMF_InterArrayCreate(deLabelList, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    connectionListAux = ESMF_InterArrayCreateDGConn(connectionList, &
      rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Mark this DistGrid as invalid
    distgrid%this = ESMF_NULL_POINTER

    ! call into the C++ interface, which will sort out optional arguments
    call c_ESMC_DistGridCreateRDT(distgrid, minIndexAux, maxIndexAux, &
      regDecompAux, opt_decompflag, len1_decompflag, len2_decompflag, &
      regDecompFirstExtraAux, regDecompLastExtraAux, deLabelListAux, &
      indexflag, connectionListAux, delayout, vm, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
      
    ! garbage collection
    call ESMF_InterArrayDestroy(minIndexAux, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterArrayDestroy(maxIndexAux, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterArrayDestroy(regDecompAux, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterArrayDestroy(regDecompFirstExtraAux, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterArrayDestroy(regDecompLastExtraAux, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterArrayDestroy(deLabelListAux, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterArrayDestroy(connectionListAux, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    
    ! Set return value
    ESMF_DistGridCreateRDT = distgrid 
 
    ! Set init code
    ESMF_INIT_SET_CREATED(ESMF_DistGridCreateRDT)
 
    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS
 
  end function ESMF_DistGridCreateRDT
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DistGridCreateRDF()"
!BOPI
! !IROUTINE: ESMF_DistGridCreate - Create DistGrid object with regular decomposition and fast axis

! !INTERFACE:
  ! Private name; call using ESMF_DistGridCreate()
  function ESMF_DistGridCreateRDF(minIndex, maxIndex, regDecomp, &
    decompflag, regDecompFirstExtra, regDecompLastExtra, deLabelList, &
    indexflag, connectionList, fastAxis, vm, rc)
!         
! !RETURN VALUE:
    type(ESMF_DistGrid) :: ESMF_DistGridCreateRDF
!
! !ARGUMENTS:
    integer,                        intent(in)            :: minIndex(:)
    integer,                        intent(in)            :: maxIndex(:)
    integer,                        intent(in),  optional :: regDecomp(:)
    type(ESMF_Decomp_Flag), target, intent(in),  optional :: decompflag(:)
    integer,                target, intent(in),  optional :: regDecompFirstExtra(:)
    integer,                target, intent(in),  optional :: regDecompLastExtra(:)
    integer,                        intent(in),  optional :: deLabelList(:)
    type(ESMF_Index_Flag),          intent(in),  optional :: indexflag
    type(ESMF_DistGridConnection),  intent(in),  optional :: connectionList(:)
    integer,                        intent(in)            :: fastAxis
    type(ESMF_VM),                  intent(in),  optional :: vm
    integer,                        intent(out), optional :: rc
!
! !DESCRIPTION:
!     Create an {\tt ESMF\_DistGrid} from a single logically rectangular 
!     tile with regular decomposition. A regular
!     decomposition is of the same rank as the tile and decomposes
!     each dimension into a fixed number of DEs. A regular decomposition of a
!     single tile is expressed by a single {\tt regDecomp} list of DE counts
!     in each dimension.
!
!     The arguments are:
!     \begin{description}
!     \item[minIndex]
!          Index space tuple of the lower corner of the single tile.
!     \item[maxIndex]
!          Index space tuple of the upper corner of the single tile.
!     \item[{[regDecomp]}]
!          List of DE counts for each dimension. The default decomposition will
!          be {\tt deCount}$ \times 1 \times ... \times 1$. The value of
!          {\tt deCount} for a default DELayout equals {\tt petCount}, i.e. the
!          default decomposition will be into as many DEs as there are 
!          PETs and the distribution will be 1 DE per PET.
!     \item[{[decompflag]}]
!          List of decomposition flags indicating how each dimension of the
!          tile is to be divided between the DEs. The default setting
!          is {\tt ESMF\_DECOMP\_BALANCED} in all dimensions. See section
!          \ref{const:decompflag} for a list of valid decomposition options.
!     \item[{[regDecompFirstExtra]}]
!          Specify how many extra elements on the first DEs along each 
!          dimension to consider when applying the regular decomposition 
!          algorithm. This does {\em not} add extra elements to the 
!          index space defined by {\tt minIndex} and {\tt maxIndex}. Instead
!          {\tt regDecompFirstExtra} is used to correctly interpret the 
!          specified index space: The {\tt regDecomp} is first applied to the
!          index space {\em without} the extra elements. The extra elements are
!          then added back in to arrive at the final decomposition. This is 
!          useful when aligning the decomposition of index spaces that only
!          differ in extra elements along the edges, e.g. when dealing with
!          different stagger locations.
!          The default is a zero vector, assuming no extra elements.
!     \item[{[regDecompLastExtra]}]
!          Specify how many extra elements on the last DEs along each 
!          dimension to consider when applying the regular decomposition 
!          algorithm. This does {\em not} add extra elements to the 
!          index space defined by {\tt minIndex} and {\tt maxIndex}. Instead
!          {\tt regDecompLastExtra} is used to correctly interpret the 
!          specified index space: The {\tt regDecomp} is first applied to the
!          index space {\em without} the extra elements. The extra elements are
!          then added back in to arrive at the final decomposition. This is 
!          useful when aligning the decomposition of index spaces that only
!          differ in extra elements along the edges, e.g. when dealing with
!          different stagger locations.
!          The default is a zero vector, assuming no extra elements.
!     \item[{[deLabelList]}]
!          List assigning DE labels to the default sequence of DEs. The default
!          sequence is given by the column major order of the {\tt regDecomp}
!          argument.
!     \item[{[indexflag]}]
!          Indicates whether the indices provided by the {\tt minIndex} and
!          {\tt maxIndex} arguments are forming a global
!          index space or not. This does {\em not} affect the indices held
!          by the DistGrid object, which are always identical to what was
!          specified by {\tt minIndex} and {\tt maxIndex}, regardless of the
!          {\tt indexflag} setting. However, it does affect whether an
!          {\tt ESMF\_Array} object created on the DistGrid can choose global
!          indexing or not. The default is {\tt ESMF\_INDEX\_DELOCAL}.
!          See section \ref{const:indexflag} for a complete list of options.
!     \item[{[connectionList]}]
!          List of {\tt ESMF\_DistGridConnection} objects, defining connections
!          between DistGrid tiles in index space.
!          See section \ref{api:DistGridConnectionSet} for the associated Set()
!          method.
!     \item[fastAxis]
!          Integer value indicating along which axis fast communication is
!          requested. This hint will be used during DELayout creation.
!     \item[{[vm]}]
!          Optional {\tt ESMF\_VM} object of the current context. Providing the
!          VM of the current context will lower the method's overhead.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    integer               :: localrc      ! local return code
    type(ESMF_DistGrid)   :: distgrid     ! opaque pointer to new C++ DistGrid
    type(ESMF_InterArray) :: minIndexAux  ! helper variable
    type(ESMF_InterArray) :: maxIndexAux  ! helper variable
    type(ESMF_InterArray) :: regDecompAux ! helper variable
    type(ESMF_Decomp_Flag), target   :: dummyDf(0)  ! satisfy C interface
    type(ESMF_Decomp_Flag), pointer  ::  opt_decompflag(:) ! optional arg helper
    integer               :: len_decompflag ! helper variable
    type(ESMF_InterArray) :: regDecompFirstExtraAux ! helper variable
    type(ESMF_InterArray) :: regDecompLastExtraAux ! helper variable
    type(ESMF_InterArray) :: deLabelListAux ! helper variable
    type(ESMF_InterArray) :: connectionListAux ! helper variable

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    
    ! invalidate return value    
    distgrid%this = ESMF_NULL_POINTER
    ESMF_DistGridCreateRDF = distgrid 
    
    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc)
    
    ! Deal with (optional) array arguments
    minIndexAux = ESMF_InterArrayCreate(minIndex, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    maxIndexAux = ESMF_InterArrayCreate(maxIndex, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    regDecompAux = ESMF_InterArrayCreate(regDecomp, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    if (present(decompflag)) then
      len_decompflag = size(decompflag)
      opt_decompflag => decompflag
    else
      len_decompflag = 0
      opt_decompflag => dummyDf
    endif
    regDecompFirstExtraAux = ESMF_InterArrayCreate(regDecompFirstExtra, &
      rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    regDecompLastExtraAux = ESMF_InterArrayCreate(regDecompLastExtra, &
      rc=localrc)
    deLabelListAux = ESMF_InterArrayCreate(deLabelList, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    connectionListAux = ESMF_InterArrayCreateDGConn(connectionList, &
      rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Mark this DistGrid as invalid
    distgrid%this = ESMF_NULL_POINTER

    ! call into the C++ interface, which will sort out optional arguments
    call c_ESMC_DistGridCreateRDF(distgrid, minIndexAux, maxIndexAux, &
      regDecompAux, opt_decompflag, len_decompflag, regDecompFirstExtraAux, &
      regDecompLastExtraAux, deLabelListAux, indexflag, &
      connectionListAux, fastAxis, vm, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
      
    ! garbage collection
    call ESMF_InterArrayDestroy(minIndexAux, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterArrayDestroy(maxIndexAux, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterArrayDestroy(regDecompAux, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterArrayDestroy(regDecompFirstExtraAux, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterArrayDestroy(regDecompLastExtraAux, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterArrayDestroy(deLabelListAux, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterArrayDestroy(connectionListAux, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    
    ! Set return value
    ESMF_DistGridCreateRDF = distgrid 
 
    ! Set init code
    ESMF_INIT_SET_CREATED(ESMF_DistGridCreateRDF)
 
    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS
 
  end function ESMF_DistGridCreateRDF
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DistGridCreateRDTF()"
!BOPI
! !IROUTINE: ESMF_DistGridCreate - Create DistGrid object on multiple tiles with regular decomposition and fast axis

! !INTERFACE:
  ! Private name; call using ESMF_DistGridCreate()
  function ESMF_DistGridCreateRDTF(minIndexPTile, maxIndexPTile, regDecomp, &
    decompflag, deLabelList, indexflag, connectionList, fastAxis, vm, rc)
!         
! !RETURN VALUE:
    type(ESMF_DistGrid) :: ESMF_DistGridCreateRDTF
!
! !ARGUMENTS:
    integer,                       intent(in)            :: minIndexPTile(:,:)
    integer,                       intent(in)            :: maxIndexPTile(:,:)
    integer,                       intent(in),  optional :: regDecomp(:,:)
    type(ESMF_Decomp_Flag),target, intent(in),  optional :: decompflag(:,:)
    integer,                       intent(in),  optional :: deLabelList(:)
    type(ESMF_Index_Flag),         intent(in),  optional :: indexflag
    type(ESMF_DistGridConnection), intent(in),  optional :: connectionList(:)
    integer,                       intent(in)            :: fastAxis
    type(ESMF_VM),                 intent(in),  optional :: vm
    integer,                       intent(out), optional :: rc
!
! !DESCRIPTION:
!     Create an {\tt ESMF\_DistGrid} on multiple logically 
!     rectangular tiles with regular decomposition. A regular
!     decomposition is of the same rank as the tile and decomposes
!     each dimension into a fixed number of DEs. A regular decomposition of a
!     multi-tile DistGrid is expressed by a list of DE count vectors, one
!     vector for each tile. Each vector contained in the 
!     {\tt regDecomp} argument ascribes DE counts for each dimension. It is 
!     erroneous to provide more tiles than there are DEs.
!
!     The arguments are:
!     \begin{description}
!     \item[minIndexPTile]
!          The first index provides the index space tuple of the lower 
!          corner of a tile. The second index indicates the tile number.
!     \item[maxIndexPTile]
!          The first index provides the index space tuple of the upper
!          corner of a tile. The second index indicates the tile number.
!     \item[{[regDecomp]}]
!          List of DE counts for each dimension. The second 
!          index indicates the tile number. The default decomposition will
!          be {\tt deCount}$ \times 1 \times ... \times 1$. The value of
!          {\tt deCount} for a default DELayout equals {\tt petCount}, i.e. the
!          default decomposition will be into as many DEs as there are 
!          PETs and the distribution will be 1 DE per PET.
!     \item[{[decompflag]}]
!          List of decomposition flags indicating how each dimension of each
!          tile is to be divided between the DEs. The default setting
!          is {\tt ESMF\_DECOMP\_BALANCED} in all dimensions for all tiles. 
!          See section \ref{const:decompflag} for a list of valid decomposition
!          options. The second index indicates the tile number.
!     \item[{[deLabelList]}]
!          List assigning DE labels to the default sequence of DEs. The default
!          sequence is given by the column major order of the {\tt regDecomp}
!          elements in the sequence as they appear following the tile index.
!     \item[{[indexflag]}]
!          Indicates whether the indices provided by the {\tt minIndex} and
!          {\tt maxIndex} arguments are forming a global
!          index space or not. This does {\em not} affect the indices held
!          by the DistGrid object, which are always identical to what was
!          specified by {\tt minIndex} and {\tt maxIndex}, regardless of the
!          {\tt indexflag} setting. However, it does affect whether an
!          {\tt ESMF\_Array} object created on the DistGrid can choose global
!          indexing or not. The default is {\tt ESMF\_INDEX\_DELOCAL}.
!          See section \ref{const:indexflag} for a complete list of options.
!     \item[{[connectionList]}]
!          List of {\tt ESMF\_DistGridConnection} objects, defining connections
!          between DistGrid tiles in index space.
!          See section \ref{api:DistGridConnectionSet} for the associated Set()
!          method.
!     \item[fastAxis]
!          Integer value indicating along which axis fast communication is
!          requested. This hint will be used during DELayout creation.
!     \item[{[vm]}]
!          Optional {\tt ESMF\_VM} object of the current context. Providing the
!          VM of the current context will lower the method's overhead.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code
    type(ESMF_DistGrid)     :: distgrid     ! opaque pointer to new C++ DistGrid

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    
    ! Initialize the pointer to NULL
    distgrid%this = ESMF_NULL_POINTER
    ESMF_DistGridCreateRDTF = distgrid

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc)
    
    !DUMMY TEST TO QUIET DOWN COMPILER WARNINGS
    !TODO: Remove the following dummy test when dummy argument actually used
    if (size(minIndexPTile) == size(minIndexPTile)) continue

    !DUMMY TEST TO QUIET DOWN COMPILER WARNINGS
    !TODO: Remove the following dummy test when dummy argument actually used
    if (size(maxIndexPTile) == size(maxIndexPTile)) continue

    !DUMMY TEST TO QUIET DOWN COMPILER WARNINGS
    !TODO: Remove the following dummy test when dummy argument actually used
    if (present(regDecomp)) continue

    !DUMMY TEST TO QUIET DOWN COMPILER WARNINGS
    !TODO: Remove the following dummy test when dummy argument actually used
    if (present(decompflag)) continue

    !DUMMY TEST TO QUIET DOWN COMPILER WARNINGS
    !TODO: Remove the following dummy test when dummy argument actually used
    if (present(deLabelList)) continue

    !DUMMY TEST TO QUIET DOWN COMPILER WARNINGS
    !TODO: Remove the following dummy test when dummy argument actually used
    if (present(indexflag)) continue

    !DUMMY TEST TO QUIET DOWN COMPILER WARNINGS
    !TODO: Remove the following dummy test when dummy argument actually used
    if (present(connectionList)) continue

    !DUMMY TEST TO QUIET DOWN COMPILER WARNINGS
    !TODO: Remove the following dummy test when dummy argument actually used
    if (fastaxis == fastaxis) continue
    
    ! Call into the C++ interface, which will sort out optional arguments.
!    call c_ESMC_DistGridCreateRDF(distgrid, minIndexAux, maxIndexAux, &
!      regDecompAux, opt_decompflag, len_decompflag, deLabelListAux, indexflag, &
!      connectionListAux, fastAxis, vm, localrc)
!    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
!      ESMF_CONTEXT, rcToReturn=rc)) return
    
    ! Set return value
    ESMF_DistGridCreateRDTF = distgrid
 
    ! Set init code
    ESMF_INIT_SET_CREATED(ESMF_DistGridCreateRDTF)
 
    ! return successfully
    !if (present(rc)) rc = ESMF_SUCCESS   TODO: enable once implemented
 
  end function ESMF_DistGridCreateRDTF
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DistGridCreateDB()"
!BOP
! !IROUTINE: ESMF_DistGridCreate - Create DistGrid object with DE blocks

! !INTERFACE:
  ! Private name; call using ESMF_DistGridCreate()
  function ESMF_DistGridCreateDB(minIndex, maxIndex, deBlockList, &
    keywordEnforcer, deLabelList, indexflag, connectionList, delayout, vm, &
    indexTK, rc)
!         
! !RETURN VALUE:
    type(ESMF_DistGrid) :: ESMF_DistGridCreateDB
!
! !ARGUMENTS:
    integer,                       intent(in)            :: minIndex(:)
    integer,                       intent(in)            :: maxIndex(:)
    integer,                       intent(in)            :: deBlockList(:,:,:)
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,                       intent(in),  optional :: deLabelList(:)
    type(ESMF_Index_Flag),         intent(in),  optional :: indexflag
    type(ESMF_DistGridConnection), intent(in),  optional :: connectionList(:)
    type(ESMF_DELayout),           intent(in),  optional :: delayout
    type(ESMF_VM),                 intent(in),  optional :: vm
    type(ESMF_TypeKind_Flag),      intent(in),  optional :: indexTK
    integer,                       intent(out), optional :: rc
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \item\apiStatusModifiedSinceVersion{5.2.0r}
! \begin{description}
! \item[7.1.0r] Added argument {\tt indexTK} to support selecting between
!               32-bit and 64-bit sequence indices.
! \end{description}
! \end{itemize}
!
! !DESCRIPTION:
!     \begin{sloppypar}
!     Create an {\tt ESMF\_DistGrid} from a single logically rectangular 
!     tile with decomposition specified by {\tt deBlockList}.
!     \end{sloppypar}
!
!     The arguments are:
!     \begin{description}
!     \item[minIndex]
!          Index space tuple of the lower corner of the single tile.
!     \item[maxIndex]
!          Index space tuple of the upper corner of the single tile.
!     \item[deBlockList]
!          List of DE-local blocks. The third index of {\tt deBlockList}
!          steps through the deBlock elements (i.e. deCount), which are defined
!          by the first two indices. 
!          The first index must be of size {\tt dimCount} and the 
!          second index must be of size 2. Each element of {\tt deBlockList}
!          defined by the first two indices hold the following information.
!          \begin{verbatim}
!                   +---------------------------------------> 2nd index
!                   |    1               2           
!                   | 1  minIndex(1)    maxIndex(1)
!                   | 2  minIndex(2)    maxIndex(2)
!                   | .  minIndex(.)    maxIndex(.)
!                   | .
!                   v
!                  1st index
!          \end{verbatim}
!          It is required that there be no overlap between the DE blocks.
!     \item[{[deLabelList]}]
!          List assigning DE labels to the default sequence of DEs. The default
!          sequence is given by the order of DEs in the {\tt deBlockList} 
!          argument.
!     \item[{[indexflag]}]
!          Indicates whether the indices provided by the {\tt minIndex} and
!          {\tt maxIndex} arguments are forming a global
!          index space or not. This does {\em not} affect the indices held
!          by the DistGrid object, which are always identical to what was
!          specified by {\tt minIndex} and {\tt maxIndex}, regardless of the
!          {\tt indexflag} setting. However, it does affect whether an
!          {\tt ESMF\_Array} object created on the DistGrid can choose global
!          indexing or not. The default is {\tt ESMF\_INDEX\_DELOCAL}.
!          See section \ref{const:indexflag} for a complete list of options.
!     \item[{[connectionList]}]
!          List of {\tt ESMF\_DistGridConnection} objects, defining connections
!          between DistGrid tiles in index space.
!          See section \ref{api:DistGridConnectionSet} for the associated Set()
!          method.
!     \item[{[delayout]}]
!          Optional {\tt ESMF\_DELayout} object to be used. By default a new
!          DELayout object will be created with the correct number of DEs. If
!          a DELayout object is specified its number of DEs must match the 
!          number indicated by {\tt regDecomp}.
!     \item[{[vm]}]
!          Optional {\tt ESMF\_VM} object of the current context. Providing the
!          VM of the current context will lower the method's overhead.
!     \item[{[indexTK]}]
!          Typekind used for global sequence indexing. See section 
!          \ref{const:typekind} for a list of typekind options. Only integer
!          types are supported. The default is to have ESMF automatically choose
!          between {\tt ESMF\_TYPEKIND\_I4} and {\tt ESMF\_TYPEKIND\_I8},
!          depending on whether the global number of elements held by the
!          DistGrid is below or above the 32-bit limit, respectively.
!          Because of the use of signed integers for sequence indices, 
!          element counts of $ > 2^{31}-1 = 2,147,483,647$ will switch to 64-bit 
!          indexing.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc      ! local return code
    type(ESMF_DistGrid)   :: distgrid     ! opaque pointer to new C++ DistGrid
    type(ESMF_InterArray) :: minIndexAux  ! helper variable
    type(ESMF_InterArray) :: maxIndexAux  ! helper variable
    type(ESMF_InterArray) :: deBlockListAux ! helper variable
    type(ESMF_InterArray) :: deLabelListAux ! helper variable
    type(ESMF_InterArray) :: connectionListAux ! helper variable

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    
    ! invalidate return value    
    distgrid%this = ESMF_NULL_POINTER
    ESMF_DistGridCreateDB = distgrid 
    
    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_DELayoutGetInit, delayout, rc)
    ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc)
    
    ! Deal with (optional) array arguments
    minIndexAux = ESMF_InterArrayCreate(minIndex, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    maxIndexAux = ESMF_InterArrayCreate(maxIndex, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    deBlockListAux = ESMF_InterArrayCreate(farray3D=deBlockList, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    deLabelListAux = ESMF_InterArrayCreate(deLabelList, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    connectionListAux = ESMF_InterArrayCreateDGConn(connectionList, &
      rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Mark this DistGrid as invalid
    distgrid%this = ESMF_NULL_POINTER

    ! call into the C++ interface, which will sort out optional arguments
    call c_ESMC_DistGridCreateDB(distgrid, minIndexAux, maxIndexAux, &
      deBlockListAux, deLabelListAux, indexflag, &
      connectionListAux, delayout, vm, indexTK, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
      
    ! garbage collection
    call ESMF_InterArrayDestroy(minIndexAux, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterArrayDestroy(maxIndexAux, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterArrayDestroy(deBlockListAux, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterArrayDestroy(deLabelListAux, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterArrayDestroy(connectionListAux, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    
    ! Set return value
    ESMF_DistGridCreateDB = distgrid 
 
    ! Set init code
    ESMF_INIT_SET_CREATED(ESMF_DistGridCreateDB)
 
    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS
 
  end function ESMF_DistGridCreateDB
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DistGridCreateDBT()"
!BOP
! !IROUTINE: ESMF_DistGridCreate - Create DistGrid object with DE blocks (multi-tile version)

! !INTERFACE:
  ! Private name; call using ESMF_DistGridCreate()
  function ESMF_DistGridCreateDBT(minIndexPTile, maxIndexPTile, deBlockList, &
    deToTileMap, keywordEnforcer, deLabelList, indexflag, connectionList, &
    delayout, vm, indexTK, rc)
!         
! !RETURN VALUE:
    type(ESMF_DistGrid) :: ESMF_DistGridCreateDBT
!
! !ARGUMENTS:
    integer,                       intent(in)            :: minIndexPTile(:,:)
    integer,                       intent(in)            :: maxIndexPTile(:,:)
    integer,                       intent(in)            :: deBlockList(:,:,:)
    integer,                       intent(in)            :: deToTileMap(:)
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,                       intent(in),  optional :: deLabelList(:)
    type(ESMF_Index_Flag),         intent(in),  optional :: indexflag
    type(ESMF_DistGridConnection), intent(in),  optional :: connectionList(:)
    type(ESMF_DELayout),           intent(in),  optional :: delayout
    type(ESMF_VM),                 intent(in),  optional :: vm
    type(ESMF_TypeKind_Flag),      intent(in),  optional :: indexTK
    integer,                       intent(out), optional :: rc
!
! !DESCRIPTION:
!     Create an {\tt ESMF\_DistGrid} on multiple logically 
!     rectangular tiles with decomposition specified by {\tt deBlockList}.
!
!     The arguments are:
!     \begin{description}
!     \item[minIndexPTile]
!          The first index provides the index space tuple of the lower 
!          corner of a tile. The second index indicates the tile number.
!     \item[maxIndexPTile]
!          The first index provides the index space tuple of the upper
!          corner of a tile. The second index indicates the tile number.
!     \item[deBlockList]
!          List of DE-local blocks. The third index of {\tt deBlockList}
!          steps through the deBlock elements (i.e. deCount), which are defined
!          by the first two indices. 
!          The first index must be of size {\tt dimCount} and the 
!          second index must be of size 2. Each element of {\tt deBlockList}
!          defined by the first two indices hold the following information.
!          \begin{verbatim}
!                   +---------------------------------------> 2nd index
!                   |    1               2           
!                   | 1  minIndex(1)    maxIndex(1)
!                   | 2  minIndex(2)    maxIndex(2)
!                   | .  minIndex(.)    maxIndex(.)
!                   | .
!                   v
!                  1st index
!          \end{verbatim}
!          It is required that there be no overlap between the DE blocks.
!     \item[deToTileMap]
!          List assigning each DE to a specific tile. The size of 
!          {\tt deToTileMap} must be equal to {\tt deCount}.
!          The order of DEs is the same as in {\tt deBlockList}.
!     \item[{[deLabelList]}]
!          List assigning DE labels to the default sequence of DEs. The default
!          sequence is given by the order of DEs in the {\tt deBlockList} 
!          argument.
!     \item[{[indexflag]}]
!          Indicates whether the indices provided by the {\tt minIndexPTile} and
!          {\tt maxIndexPTile} arguments are forming a global index space or 
!          not. This does {\em not} affect the indices held by the DistGrid 
!          object, which are always identical to what was specified by 
!          {\tt minIndexPTile} and {\tt maxIndexPTile}, regardless of the
!          {\tt indexflag} setting. However, it does affect whether an
!          {\tt ESMF\_Array} object created on the DistGrid can choose global
!          indexing or not. The default is {\tt ESMF\_INDEX\_DELOCAL}.
!          See section \ref{const:indexflag} for a complete list of options.
!     \item[{[connectionList]}]
!          List of {\tt ESMF\_DistGridConnection} objects, defining connections
!          between DistGrid tiles in index space.
!          See section \ref{api:DistGridConnectionSet} for the associated Set()
!          method.
!     \item[{[delayout]}]
!          Optional {\tt ESMF\_DELayout} object to be used. By default a new
!          DELayout object will be created with the correct number of DEs. If
!          a DELayout object is specified its number of DEs must match the 
!          number indicated by {\tt regDecomp}.
!     \item[{[vm]}]
!          Optional {\tt ESMF\_VM} object of the current context. Providing the
!          VM of the current context will lower the method's overhead.
!     \item[{[indexTK]}]
!          Typekind used for global sequence indexing. See section 
!          \ref{const:typekind} for a list of typekind options. Only integer
!          types are supported. The default is to have ESMF automatically choose
!          between {\tt ESMF\_TYPEKIND\_I4} and {\tt ESMF\_TYPEKIND\_I8},
!          depending on whether the global number of elements held by the
!          DistGrid is below or above the 32-bit limit, respectively.
!          Because of the use of signed integers for sequence indices, 
!          element counts of $ > 2^{31}-1 = 2,147,483,647$ will switch to 64-bit 
!          indexing.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc      ! local return code
    type(ESMF_DistGrid)   :: distgrid     ! opaque pointer to new C++ DistGrid
    type(ESMF_InterArray) :: minIndexAux        ! helper variable
    type(ESMF_InterArray) :: maxIndexAux        ! helper variable
    type(ESMF_InterArray) :: deBlockListAux     ! helper variable
    type(ESMF_InterArray) :: deToTileMapAux     ! helper variable
    type(ESMF_InterArray) :: deLabelListAux     ! helper variable
    type(ESMF_InterArray) :: connectionListAux  ! helper variable

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    
    ! Initialize the pointer to NULL
    distgrid%this = ESMF_NULL_POINTER
    ESMF_DistGridCreateDBT = distgrid
    
    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_DELayoutGetInit, delayout, rc)
    ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc)
    
    ! Deal with (optional) array arguments
    minIndexAux = ESMF_InterArrayCreate(farray2D=minIndexPTile, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    maxIndexAux = ESMF_InterArrayCreate(farray2D=maxIndexPTile, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    deBlockListAux = ESMF_InterArrayCreate(farray3D=deBlockList, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    deToTileMapAux = ESMF_InterArrayCreate(deToTileMap, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    deLabelListAux = ESMF_InterArrayCreate(deLabelList, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    connectionListAux = ESMF_InterArrayCreateDGConn(connectionList, &
      rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
      
    ! call into the C++ interface, which will sort out optional arguments
    call c_ESMC_DistGridCreateDBT(distgrid, minIndexAux, maxIndexAux, &
      deBlockListAux, deToTileMapAux, deLabelListAux, indexflag, &
      connectionListAux, delayout, vm, indexTK, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! garbage collection
    call ESMF_InterArrayDestroy(minIndexAux, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterArrayDestroy(maxIndexAux, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterArrayDestroy(deBlockListAux, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterArrayDestroy(deToTileMapAux, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterArrayDestroy(deLabelListAux, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterArrayDestroy(connectionListAux, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Set return value
    ESMF_DistGridCreateDBT = distgrid 
 
    ! Set init code
    ESMF_INIT_SET_CREATED(ESMF_DistGridCreateDBT)
 
    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS
 
  end function ESMF_DistGridCreateDBT
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DistGridCreateDBF()"
!BOPI
! !IROUTINE: ESMF_DistGridCreate - Create DistGrid object with DE blocks and fast axis

! !INTERFACE:
  ! Private name; call using ESMF_DistGridCreate()
  function ESMF_DistGridCreateDBF(minIndex, maxIndex, &
    deBlockList, deLabelList, indexflag, connectionList, &
    fastAxis, vm, rc)
!         
! !RETURN VALUE:
    type(ESMF_DistGrid) :: ESMF_DistGridCreateDBF
!
! !ARGUMENTS:
    integer,                       intent(in)            :: minIndex(:)
    integer,                       intent(in)            :: maxIndex(:)
    integer,                       intent(in)            :: deBlockList(:,:,:)
    integer,                       intent(in),  optional :: deLabelList(:)
    type(ESMF_Index_Flag),         intent(in),  optional :: indexflag
    type(ESMF_DistGridConnection), intent(in),  optional :: connectionList(:)
    integer,                       intent(in)            :: fastAxis
    type(ESMF_VM),                 intent(in),  optional :: vm
    integer,                       intent(out), optional :: rc
!
! !DESCRIPTION:
!     Create an {\tt ESMF\_DistGrid} from a single logically rectangular
!     tile with decomposition specified by {\tt deBlockList}.
!
!     The arguments are:
!     \begin{description}
!     \item[minIndex]
!          Index space tuple of the lower corner of the single tile.
!     \item[maxIndex]
!          Index space tuple of the upper corner of the single tile.
!     \item[deBlockList]
!          List of DE-local blocks. The third index of {\tt deBlockList}
!          steps through the deBlock elements (i.e. deCount), which are defined
!          by the first two indices. 
!          The first index must be of size {\tt dimCount} and the 
!          second index must be of size 2. Each element of {\tt deBlockList}
!          defined by the first two indices hold the following information.
!          \begin{verbatim}
!                   +---------------------------------------> 2nd index
!                   |    1               2           
!                   | 1  minIndex(1)    maxIndex(1)
!                   | 2  minIndex(2)    maxIndex(2)
!                   | .  minIndex(.)    maxIndex(.)
!                   | .
!                   v
!                  1st index
!          \end{verbatim}
!          It is required that there be no overlap between the DE blocks.
!     \item[{[deLabelList]}]
!          List assigning DE labels to the default sequence of DEs. The default
!          sequence is given by the order of DEs in the {\tt deBlockList} 
!          argument.
!     \item[{[indexflag]}]
!          Indicates whether the indices provided by the {\tt minIndex} and
!          {\tt maxIndex} arguments are forming a global
!          index space or not. This does {\em not} affect the indices held
!          by the DistGrid object, which are always identical to what was
!          specified by {\tt minIndex} and {\tt maxIndex}, regardless of the
!          {\tt indexflag} setting. However, it does affect whether an
!          {\tt ESMF\_Array} object created on the DistGrid can choose global
!          indexing or not. The default is {\tt ESMF\_INDEX\_DELOCAL}.
!          See section \ref{const:indexflag} for a complete list of options.
!     \item[{[connectionList]}]
!          List of {\tt ESMF\_DistGridConnection} objects, defining connections
!          between DistGrid tiles in index space.
!          See section \ref{api:DistGridConnectionSet} for the associated Set()
!          method.
!     \item[fastAxis]
!          Integer value indicating along which axis fast communication is
!          requested. This hint will be used during DELayout creation.
!     \item[{[vm]}]
!          Optional {\tt ESMF\_VM} object of the current context. Providing the
!          VM of the current context will lower the method's overhead.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code
    type(ESMF_DistGrid)     :: distgrid     ! opaque pointer to new C++ DistGrid
    
    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    
    ! Initialize the pointer to NULL
    distgrid%this = ESMF_NULL_POINTER
    ESMF_DistGridCreateDBF = distgrid

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc)
    
    !DUMMY TEST TO QUIET DOWN COMPILER WARNINGS
    !TODO: Remove the following dummy test when dummy argument actually used
    if (size(minIndex) == size(minIndex)) continue

    !DUMMY TEST TO QUIET DOWN COMPILER WARNINGS
    !TODO: Remove the following dummy test when dummy argument actually used
    if (size(maxIndex) == size(maxIndex)) continue

    !DUMMY TEST TO QUIET DOWN COMPILER WARNINGS
    !TODO: Remove the following dummy test when dummy argument actually used
    if (size(deBlockList) == size(deBlockList)) continue

    !DUMMY TEST TO QUIET DOWN COMPILER WARNINGS
    !TODO: Remove the following dummy test when dummy argument actually used
    if (present(deLabelList)) continue

    !DUMMY TEST TO QUIET DOWN COMPILER WARNINGS
    !TODO: Remove the following dummy test when dummy argument actually used
    if (present(indexflag)) continue

    !DUMMY TEST TO QUIET DOWN COMPILER WARNINGS
    !TODO: Remove the following dummy test when dummy argument actually used
    if (present(connectionList)) continue

    !DUMMY TEST TO QUIET DOWN COMPILER WARNINGS
    !TODO: Remove the following dummy test when dummy argument actually used
    if (fastaxis == fastaxis) continue
    
    ! Call into the C++ interface, which will sort out optional arguments.
!    call c_ESMC_DistGridCreateRDF(distgrid, minIndexAux, maxIndexAux, &
!      regDecompAux, opt_decompflag, len_decompflag, deLabelListAux, indexflag, &
!      connectionListAux, fastAxis, vm, localrc)
!    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
!      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Set return value
    ESMF_DistGridCreateDBF = distgrid 
 
    ! Set init code
    ESMF_INIT_SET_CREATED(ESMF_DistGridCreateDBF)
 
    ! return successfully
    !if (present(rc)) rc = ESMF_SUCCESS   TODO: enable once implemented
 
  end function ESMF_DistGridCreateDBF
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DistGridCreateDBTF()"
!BOPI
! !IROUTINE: ESMF_DistGridCreate - Create DistGrid object on multiple tiles with DE blocks and fast axis

! !INTERFACE:
  ! Private name; call using ESMF_DistGridCreate()
  function ESMF_DistGridCreateDBTF(minIndexPTile, maxIndexPTile, deBlockList, &
    deLabelList, indexflag, connectionList, fastAxis, vm, rc)
!         
! !RETURN VALUE:
    type(ESMF_DistGrid) :: ESMF_DistGridCreateDBTF
!
! !ARGUMENTS:
    integer,                       intent(in)            :: minIndexPTile(:,:)
    integer,                       intent(in)            :: maxIndexPTile(:,:)
    integer,                       intent(in)            :: deBlockList(:,:,:)
    integer,                       intent(in),  optional :: deLabelList(:)
    type(ESMF_Index_Flag),         intent(in),  optional :: indexflag
    type(ESMF_DistGridConnection), intent(in),  optional :: connectionList(:)
    integer,                       intent(in)            :: fastAxis
    type(ESMF_VM),                 intent(in),  optional :: vm
    integer,                       intent(out), optional :: rc
!
! !DESCRIPTION:
!     Create an {\tt ESMF\_DistGrid} on multiple logically 
!     rectangular tiles with decomposition specified by {\tt deBlockList}.
!
!     The arguments are:
!     \begin{description}
!     \item[minIndexPTile]
!          The first index provides the index space tuple of the lower 
!          corner of a tile. The second index indicates the tile number.
!     \item[maxIndexPTile]
!          The first index provides the index space tuple of the upper
!          corner of a tile. The second index indicates the tile number.
!     \item[deBlockList]
!          List of DE-local blocks. The third index of {\tt deBlockList}
!          steps through the deBlock elements (i.e. deCount), which are defined
!          by the first two indices. 
!          The first index must be of size {\tt dimCount} and the 
!          second index must be of size 2. Each element of {\tt deBlockList}
!          defined by the first two indices hold the following information.
!          \begin{verbatim}
!                   +---------------------------------------> 2nd index
!                   |    1               2           
!                   | 1  minIndex(1)    maxIndex(1)
!                   | 2  minIndex(2)    maxIndex(2)
!                   | .  minIndex(.)    maxIndex(.)
!                   | .
!                   v
!                  1st index
!          \end{verbatim}
!          It is required that there be no overlap between the DE blocks.
!     \item[{[deLabelList]}]
!          List assigning DE labels to the default sequence of DEs. The default
!          sequence is given by the order of DEs in the {\tt deBlockList} 
!          argument.
!     \item[{[indexflag]}]
!          Indicates whether the indices provided by the {\tt minIndexPTile} and
!          {\tt maxIndexPTile} arguments are forming a global index space or 
!          not. This does {\em not} affect the indices held by the DistGrid 
!          object, which are always identical to what was specified by 
!          {\tt minIndexPTile} and {\tt maxIndexPTile}, regardless of the
!          {\tt indexflag} setting. However, it does affect whether an
!          {\tt ESMF\_Array} object created on the DistGrid can choose global
!          indexing or not. The default is {\tt ESMF\_INDEX\_DELOCAL}.
!          See section \ref{const:indexflag} for a complete list of options.
!     \item[{[connectionList]}]
!          List of {\tt ESMF\_DistGridConnection} objects, defining connections
!          between DistGrid tiles in index space.
!          See section \ref{api:DistGridConnectionSet} for the associated Set()
!          method.
!     \item[fastAxis]
!          Integer value indicating along which axis fast communication is
!          requested. This hint will be used during DELayout creation.
!     \item[{[vm]}]
!          Optional {\tt ESMF\_VM} object of the current context. Providing the
!          VM of the current context will lower the method's overhead.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code
    type(ESMF_DistGrid)     :: distgrid     ! opaque pointer to new C++ DistGrid

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    
    ! Initialize the pointer to NULL
    distgrid%this = ESMF_NULL_POINTER
    ESMF_DistGridCreateDBTF = distgrid

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc)
    
    !DUMMY TEST TO QUIET DOWN COMPILER WARNINGS
    !TODO: Remove the following dummy test when dummy argument actually used
    if (size(minIndexPTile) == size(minIndexPTile)) continue

    !DUMMY TEST TO QUIET DOWN COMPILER WARNINGS
    !TODO: Remove the following dummy test when dummy argument actually used
    if (size(maxIndexPTile) == size(maxIndexPTile)) continue

    !DUMMY TEST TO QUIET DOWN COMPILER WARNINGS
    !TODO: Remove the following dummy test when dummy argument actually used
    if (size(deBlockList) == size(deBlockList)) continue

    !DUMMY TEST TO QUIET DOWN COMPILER WARNINGS
    !TODO: Remove the following dummy test when dummy argument actually used
    if (present(deLabelList)) continue

    !DUMMY TEST TO QUIET DOWN COMPILER WARNINGS
    !TODO: Remove the following dummy test when dummy argument actually used
    if (present(indexflag)) continue

    !DUMMY TEST TO QUIET DOWN COMPILER WARNINGS
    !TODO: Remove the following dummy test when dummy argument actually used
    if (present(connectionList)) continue

    !DUMMY TEST TO QUIET DOWN COMPILER WARNINGS
    !TODO: Remove the following dummy test when dummy argument actually used
    if (fastaxis == fastaxis) continue
    
    ! Call into the C++ interface, which will sort out optional arguments.
!    call c_ESMC_DistGridCreateRDF(distgrid, minIndexAux, maxIndexAux, &
!      regDecompAux, opt_decompflag, len_decompflag, deLabelListAux, indexflag, &
!      connectionListAux, fastAxis, vm, localrc)
!    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
!      ESMF_CONTEXT, rcToReturn=rc)) return
    
    ! Set return value
    ESMF_DistGridCreateDBTF = distgrid
 
    ! Set init code
    ESMF_INIT_SET_CREATED(ESMF_DistGridCreateDBTF)
 
    ! return successfully
    !if (present(rc)) rc = ESMF_SUCCESS   TODO: enable once implemented
 
  end function ESMF_DistGridCreateDBTF
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DistGridCreateDBAI1D1DE()"
!BOP
! !IROUTINE: ESMF_DistGridCreate - Create 1D DistGrid object from user's arbitrary sequence index list 1 DE per PET

! !INTERFACE:
  ! Private name; call using ESMF_DistGridCreate()
  function ESMF_DistGridCreateDBAI1D1DE(arbSeqIndexList, keywordEnforcer, rc)
!         
! !RETURN VALUE:
    type(ESMF_DistGrid) :: ESMF_DistGridCreateDBAI1D1DE
!
! !ARGUMENTS:
    integer, intent(in)            :: arbSeqIndexList(:)
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer, intent(out), optional :: rc
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
!     Create an {\tt ESMF\_DistGrid} of {\tt dimCount} 1 from a PET-local list
!     of sequence indices. The PET-local size of the {\tt arbSeqIndexList}
!     argument determines the number of local elements in the created DistGrid.
!     The sequence indices must be unique across all PETs. A default
!     DELayout with 1 DE per PET across all PETs of the current VM is 
!     automatically created.
!
!     The arguments are:
!     \begin{description}
!     \item[arbSeqIndexList]
!          List of arbitrary sequence indices that reside on the local PET.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code
    type(ESMF_DistGrid)     :: distgrid     ! opaque pointer to new C++ DistGrid
    type(ESMF_VM)           :: vm           ! opaque pointer to VM object
    type(ESMF_InterArray)   :: indicesAux   ! index helper
    integer                 :: localSize(1) ! number of local indices
    integer, allocatable    :: globalSizes(:)  ! array of all sizes
    integer                 :: petCount        ! num pets
    integer, allocatable    :: deblock(:,:,:)  ! Array of sizes
    integer                 :: i, csum         ! loop variable
    integer                 :: minC(1), maxC(1)! min/max corner

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    
    ! invalidate return value    
    distgrid%this = ESMF_NULL_POINTER
    ESMF_DistGridCreateDBAI1D1DE = distgrid 
    
    call ESMF_VMGetCurrent(vm, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
      
    call ESMF_VMGet(vm, petCount=petCount, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! number of local indices
    localSize(1) = size(arbSeqIndexList)

    ! gather all sizes locally
    allocate(globalSizes(petCount))
    call ESMF_VMAllGather(vm, localSize, globalSizes, 1, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! set up the deblocks
    allocate(deblock(1,2,petCount))
    csum = 0
    do i=1,petCount
      deblock(1,1,i) = csum + 1 ! min
      csum = csum + globalSizes(i)
      deblock(1,2,i) = csum     ! max
    enddo

    ! create fitting DistGrid
    minC(1) = deblock(1,1,1)
    maxC(1) = deblock(1,2,petCount)
    distgrid = ESMF_DistGridCreate(minC, maxC, deBlockList=deblock, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! garbage collection
    deallocate(deblock)
    deallocate(globalSizes)
    
    ! set return value
    ESMF_DistGridCreateDBAI1D1DE = distgrid 

    ! prepare to set local arbitrary sequence indices
    indicesAux = ESMF_InterArrayCreate(farray1D=arbSeqIndexList, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! set local arbitrary sequence indices in DistGrid object
    ! localDe=0, collocation=1
    call c_ESMC_DistGridSetArbSeqIndex(distgrid, indicesAux, 0, 1, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! garbage collection
    call ESMF_InterArrayDestroy(indicesAux, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
 
    ! Set init code
    ESMF_INIT_SET_CREATED(ESMF_DistGridCreateDBAI1D1DE)
 
    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS
 
  end function ESMF_DistGridCreateDBAI1D1DE
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DistGridCreateDBAI1D1DEI8()"
!BOP
! !IROUTINE: ESMF_DistGridCreate - Create 1D DistGrid object from user's arbitrary 64-bit sequence index list 1 DE per PET

! !INTERFACE:
  ! Private name; call using ESMF_DistGridCreate()
  function ESMF_DistGridCreateDBAI1D1DEI8(arbSeqIndexList, keywordEnforcer, rc)
!         
! !RETURN VALUE:
    type(ESMF_DistGrid) :: ESMF_DistGridCreateDBAI1D1DEI8
!
! !ARGUMENTS:
    integer(ESMF_KIND_I8),  intent(in)            :: arbSeqIndexList(:)
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,                intent(out), optional :: rc
!
! !DESCRIPTION:
!     Create an {\tt ESMF\_DistGrid} of {\tt dimCount} 1 from a PET-local list
!     of sequence indices. The PET-local size of the {\tt arbSeqIndexList}
!     argument determines the number of local elements in the created DistGrid.
!     The sequence indices must be unique across all PETs. A default
!     DELayout with 1 DE per PET across all PETs of the current VM is 
!     automatically created.
!
!     The arguments are:
!     \begin{description}
!     \item[arbSeqIndexList]
!          List of arbitrary sequence indices that reside on the local PET.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
!------------------------------------------------------------------------------

!TODO: deBlockList setup must also use I8!!!! 
!TODO: All of DistGrid indexing must support I8 fully support I8 seqIndex option

    integer                 :: localrc      ! local return code
    type(ESMF_DistGrid)     :: distgrid     ! opaque pointer to new C++ DistGrid
    type(ESMF_VM)           :: vm           ! opaque pointer to VM object
    type(ESMF_InterArray)   :: indicesAux   ! index helper
    integer                 :: localSize(1) ! number of local indices
    integer, allocatable    :: globalSizes(:)  ! array of all sizes
    integer                 :: petCount        ! num pets
    integer, allocatable    :: deblock(:,:,:)  ! Array of sizes
    integer                 :: i, csum         ! loop variable
    integer                 :: minC(1), maxC(1)! min/max corner

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    
    ! invalidate return value    
    distgrid%this = ESMF_NULL_POINTER
    ESMF_DistGridCreateDBAI1D1DEI8 = distgrid 
    
    call ESMF_VMGetCurrent(vm, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
      
    call ESMF_VMGet(vm, petCount=petCount, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! number of local indices
    localSize(1) = size(arbSeqIndexList)

    ! gather all sizes locally
    allocate(globalSizes(petCount))
    call ESMF_VMAllGather(vm, localSize, globalSizes, 1, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! set up the deblocks
    allocate(deblock(1,2,petCount))
    csum = 0
    do i=1,petCount
      deblock(1,1,i) = csum + 1 ! min
      csum = csum + globalSizes(i)
      deblock(1,2,i) = csum     ! max
    enddo

    ! create fitting DistGrid
    minC(1) = deblock(1,1,1)
    maxC(1) = deblock(1,2,petCount)
    distgrid = ESMF_DistGridCreate(minC, maxC, deBlockList=deblock, &
      indexTK=ESMF_TYPEKIND_I8, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! garbage collection
    deallocate(deblock)
    deallocate(globalSizes)
    
    ! set return value
    ESMF_DistGridCreateDBAI1D1DEI8 = distgrid 

    ! prepare to set local arbitrary sequence indices
    indicesAux = ESMF_InterArrayCreate(farray1DI8=arbSeqIndexList, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! set local arbitrary sequence indices in DistGrid object
    ! localDe=0, collocation=1
    call c_ESMC_DistGridSetArbSeqIndexI8(distgrid, indicesAux, 0, 1, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! garbage collection
    call ESMF_InterArrayDestroy(indicesAux, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
 
    ! Set init code
    ESMF_INIT_SET_CREATED(ESMF_DistGridCreateDBAI1D1DEI8)
 
    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS
 
  end function ESMF_DistGridCreateDBAI1D1DEI8
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DistGridCreateDBAI1D()"
!BOP
! !IROUTINE: ESMF_DistGridCreate - Create 1D DistGrid object from user's arbitrary sequence index list multiple DE/PET

! !INTERFACE:
  ! Private name; call using ESMF_DistGridCreate()
  function ESMF_DistGridCreateDBAI1D(arbSeqIndexList, keywordEnforcer, rc)
!         
! !RETURN VALUE:
    type(ESMF_DistGrid) :: ESMF_DistGridCreateDBAI1D
!
! !ARGUMENTS:
    type(ESMF_PtrInt1D), intent(in) :: arbSeqIndexList(:)
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer, intent(out), optional  :: rc
!
! !DESCRIPTION:
!     Create an {\tt ESMF\_DistGrid} of {\tt dimCount} 1 from a PET-local list
!     of sequence index lists. The PET-local size of the {\tt arbSeqIndexList}
!     argument determines the number of local DEs in the created DistGrid.
!     Each of the local DEs is associated with as many index space elements as
!     there are arbitrary sequence indices in the associated list.
!     The sequence indices must be unique across all DEs. A default
!     DELayout with the correct number of DEs per PET is automatically created.
!
!     The arguments are:
!     \begin{description}
!     \item[arbSeqIndexList]
!          List of arbitrary sequence index lists that reside on the local PET.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code
    type(ESMF_DistGrid)     :: distgrid     ! opaque pointer to new C++ DistGrid
    type(ESMF_DELayout)     :: delayout     ! opaque pointer to DELayout object
    type(ESMF_VM)           :: vm           ! opaque pointer to VM object
    type(ESMF_InterArray)   :: indicesAux   ! index helper
    integer                 :: localDeCount(1)  ! number of local DEs
    integer, allocatable    :: localDeCounts(:) ! array of all local DE counts
    integer, allocatable    :: localSizes(:)    ! number of local indices
    integer, allocatable    :: globalSizes(:)   ! array of all sizes
    integer, allocatable    :: globalSizesOff(:)! globalSizes offsets
    integer                 :: globalSizesCount ! number of entries in globalSizes
    integer                 :: petCount         ! num pets
    integer, allocatable    :: deblock(:,:,:)   ! Array of sizes
    integer, allocatable    :: petMap(:)        ! mapping of DE to PETs
    integer                 :: i, j, k, csum    ! loop variable
    integer                 :: minC(1), maxC(1) ! min/max corner

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    
    ! invalidate return value    
    distgrid%this = ESMF_NULL_POINTER
    ESMF_DistGridCreateDBAI1D = distgrid 
    
    call ESMF_VMGetCurrent(vm, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
      
    call ESMF_VMGet(vm, petCount=petCount, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! number of localDEs
    localDeCount(1) = size(arbSeqIndexList)

    ! gather all localDe counts
    allocate(localDeCounts(petCount))
    call ESMF_VMAllGather(vm, localDeCount, localDeCounts, 1, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
      
    ! setup local sizes
    allocate(localSizes(localDeCount(1)))
    do i=1, localDeCount(1)
      localSizes(i)=size(arbSeqIndexList(i)%ptr)
    enddo
    
    ! prepare globalSizes
    allocate(globalSizesOff(petCount))
    globalSizesCount=0
    do i=1, petCount
      globalSizesOff(i) = globalSizesCount
      globalSizesCount = globalSizesCount + localDeCounts(i)
    enddo
    
    ! gather all of the local sizes on all PETs
    allocate(globalSizes(globalSizesCount))
    call ESMF_VMAllGatherV(vm, localSizes, localDeCount(1), &
      globalSizes, localDeCounts, globalSizesOff, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! set up the petMap
    allocate(petMap(globalSizesCount))
    k=1
    do i=1, petCount
      do j=1, localDeCounts(i)
        petMap(k) = i-1
        k = k+1
      enddo
    enddo    
    
    ! set up the deblocks
    allocate(deblock(1,2,globalSizesCount))
    csum = 0
    do i=1,globalSizesCount
      deblock(1,1,i) = csum + 1 ! min
      csum = csum + globalSizes(i)
      deblock(1,2,i) = csum     ! max
    enddo
    
    ! create fitting DELayout
    delayout = ESMF_DELayoutCreate(petMap, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! create fitting DistGrid
    minC(1) = deblock(1,1,1)
    maxC(1) = deblock(1,2,globalSizesCount)
    distgrid = ESMF_DistGridCreate(minC, maxC, deBlockList=deblock, &
      delayout=delayout, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! garbage collection
    deallocate(deblock)
    deallocate(petMap)
    deallocate(globalSizes)
    deallocate(globalSizesOff)
    deallocate(localSizes)
    deallocate(localDeCounts)
    
    ! set return value
    ESMF_DistGridCreateDBAI1D = distgrid 

    ! set local arbitrary sequence indices
    do i=1, localDeCount(1)
      indicesAux = ESMF_InterArrayCreate(farray1D=arbSeqIndexList(i)%ptr, &
        rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! set local arbitrary sequence indices in DistGrid object
      ! localDe=(i-1), collocation=1
      call c_ESMC_DistGridSetArbSeqIndex(distgrid, indicesAux, i-1, 1, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! destroy temporary InterArray
      call ESMF_InterArrayDestroy(indicesAux, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    enddo
    
    ! Set init code
    ESMF_INIT_SET_CREATED(ESMF_DistGridCreateDBAI1D)
 
    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS
 
  end function ESMF_DistGridCreateDBAI1D
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DistGridCreateDBAI()"
!BOP
! !IROUTINE: ESMF_DistGridCreate - Create (1+n)D DistGrid object from user's arbitrary sequence index list and minIndexPTile/maxIndexPTile

! !INTERFACE:
  ! Private name; call using ESMF_DistGridCreate()
  function ESMF_DistGridCreateDBAI(arbSeqIndexList, arbDim, &
    minIndexPTile, maxIndexPTile, keywordEnforcer, rc)
!         
! !RETURN VALUE:
    type(ESMF_DistGrid) :: ESMF_DistGridCreateDBAI
!
! !ARGUMENTS:
    integer, intent(in)            :: arbSeqIndexList(:)
    integer, intent(in)            :: arbDim
    integer, intent(in)            :: minIndexPTile(:)
    integer, intent(in)            :: maxIndexPTile(:)
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer, intent(out), optional :: rc
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
!     Create an {\tt ESMF\_DistGrid} of {\tt dimCount} $1+n$, where 
!     $n=$ {\tt size(minIndexPTile)} = {\tt size(maxIndexPTile)}.
!
!     The resulting DistGrid will have a 1D distribution determined by the
!     PET-local {\tt arbSeqIndexList}. The PET-local size of the
!     {\tt arbSeqIndexList} argument determines the number of local elements 
!     along the arbitrarily distributed dimension in the created DistGrid. The
!     sequence indices must be unique across all PETs. The associated,
!     automatically created DELayout will have 1 DE per PET across all PETs of
!     the current VM.
!
!     In addition to the arbitrarily distributed dimension, regular DistGrid
!     dimensions can be specified in {\tt minIndexPTile} and {\tt maxIndexPTile}. The
!     $n$ dimensional subspace spanned by the regular dimensions is "multiplied"
!     with the arbitrary dimension on each DE, to form a $1+n$ dimensional
!     total index space described by the DistGrid object. The {\tt arbDim}
!     argument allows to specify which dimension in the resulting DistGrid
!     corresponds to the arbitrarily distributed one.
!
!     The arguments are:
!     \begin{description}
!     \item[arbSeqIndexList]
!          List of arbitrary sequence indices that reside on the local PET.
!     \item[arbDim]
!          Dimension of the arbitrary distribution.
!     \item[minIndexPTile]
!          Index space tuple of the lower corner of the tile. The 
!          arbitrary dimension is {\em not} included in this tile
!     \item[maxIndexPTile]
!          Index space tuple of the upper corner of the tile. The
!          arbitrary dimension is {\em not} included in this tile
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code
    type(ESMF_DistGrid)     :: distgrid     ! opaque pointer to new C++ DistGrid
    type(ESMF_VM)           :: vm           ! opaque pointer to VM object
    type(ESMF_InterArray)   :: indicesAux   ! index helper
    integer                 :: localSize(1) ! number of local indices
    integer, allocatable    :: globalSizes(:)  ! array of all sizes
    integer                 :: petCount        ! num pets
    integer, allocatable    :: deblock(:,:,:)  ! Array of sizes
    integer                 :: i, j, jj, csum  ! loop variable
    integer, allocatable    :: minC(:), maxC(:)! min/max corner
    integer, allocatable    :: collocationPDim(:)
    integer                 :: dimCount     ! number of dimension

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! invalidate return value    
    distgrid%this = ESMF_NULL_POINTER
    ESMF_DistGridCreateDBAI = distgrid 
    
    ! check input
    dimCount = size(minIndexPTile)
    if (dimCount /= size(maxIndexPTile)) then
      call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_SIZE, &
          msg="- size(minIndexPTile) must match size(maxIndexPTile)", &
          ESMF_CONTEXT, rcToReturn=rc)
      return
    endif
    if (arbDim < 1 .or. arbDim > dimCount+1) then
      call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_VALUE, &
        msg="- arbDim out of range", &
        ESMF_CONTEXT, rcToReturn=rc)
      return
    endif
    
    ! get VM and related information
    call ESMF_VMGetCurrent(vm, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
      
    call ESMF_VMGet(vm, petCount=petCount, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! number of local indices
    localSize(1) = size(arbSeqIndexList)

    ! gather all sizes locally
    allocate(globalSizes(petCount))
    call ESMF_VMAllGather(vm, localSize, globalSizes, 1, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! set up the deblocks
    allocate(deblock(dimCount+1,2,petCount))
    csum = 0
    do i=1,petCount
      deblock(arbDim,1,i) = csum + 1 ! min
      csum = csum + globalSizes(i)
      deblock(arbDim,2,i) = csum     ! max
      do j=1,dimCount+1
        if (j==arbDim) cycle
        jj=j
        if (j>arbDim) jj=jj-1
        deblock(j,1,i) = minIndexPTile(jj) ! min
        deblock(j,2,i) = maxIndexPTile(jj) ! max
      enddo
    enddo

    ! create fitting DistGrid
    allocate(minC(dimCount+1))
    allocate(maxC(dimCount+1))
    do i=1, dimCount+1
      minC(i) = deblock(i,1,1)
      maxC(i) = deblock(i,2,petCount)
    enddo
    distgrid = ESMF_DistGridCreate(minC, maxC, deBlockList=deblock, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! garbage collection
    deallocate(deblock)
    deallocate(globalSizes)
    deallocate(minC)
    deallocate(maxC)
    
    ! set return value
    ESMF_DistGridCreateDBAI = distgrid 

    ! set collocations to separate arbDim from the reset
    allocate(collocationPDim(dimCount+1))
    collocationPDim = 2 ! initialize
    collocationPDim(arbDim) = 1 ! arbDim singled out as collocation "1"
    call ESMF_DistGridSet(distgrid, collocationPDim=collocationPDim, &
      rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    deallocate(collocationPDim)

    ! prepare to set local arbitrary sequence indices
    indicesAux = ESMF_InterArrayCreate(farray1D=arbSeqIndexList, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! set local arbitrary sequence indices in DistGrid object
    ! localDe=0, collocation=1, i.e. arbDim's collocation
    call c_ESMC_DistGridSetArbSeqIndex(distgrid, indicesAux, 0, 1, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! garbage collection
    call ESMF_InterArrayDestroy(indicesAux, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
 
    ! Set init code
    ESMF_INIT_SET_CREATED(ESMF_DistGridCreateDBAI)
 
    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS
 
  end function ESMF_DistGridCreateDBAI
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DistGridDestroy()"
!BOP
! !IROUTINE: ESMF_DistGridDestroy - Release resources associated with a DistGrid 

! !INTERFACE:
  subroutine ESMF_DistGridDestroy(distgrid, keywordEnforcer, noGarbage, rc)
!
! !ARGUMENTS:
    type(ESMF_DistGrid), intent(inout)          :: distgrid
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    logical,             intent(in),   optional :: noGarbage
    integer,             intent(out),  optional :: rc  
!         
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \item\apiStatusModifiedSinceVersion{5.2.0r}
! \begin{description}
! \item[7.0.0] Added argument {\tt noGarbage}.
!   The argument provides a mechanism to override the default garbage collection
!   mechanism when destroying an ESMF object.
! \end{description}
! \end{itemize}
!
! !DESCRIPTION:
!   Destroys an {\tt ESMF\_DistGrid}, releasing the resources associated
!   with the object.
!
!   By default a small remnant of the object is kept in memory in order to 
!   prevent problems with dangling aliases. The default garbage collection
!   mechanism can be overridden with the {\tt noGarbage} argument.
!
! The arguments are:
! \begin{description}
! \item[distgrid] 
!      {\tt ESMF\_DistGrid} object to be destroyed.
! \item[{[noGarbage]}]
!      If set to {\tt .TRUE.} the object will be fully destroyed and removed
!      from the ESMF garbage collection system. Note however that under this 
!      condition ESMF cannot protect against accessing the destroyed object 
!      through dangling aliases -- a situation which may lead to hard to debug 
!      application crashes.
! 
!      It is generally recommended to leave the {\tt noGarbage} argument
!      set to {\tt .FALSE.} (the default), and to take advantage of the ESMF 
!      garbage collection system which will prevent problems with dangling
!      aliases or incorrect sequences of destroy calls. However this level of
!      support requires that a small remnant of the object is kept in memory
!      past the destroy call. This can lead to an unexpected increase in memory
!      consumption over the course of execution in applications that use 
!      temporary ESMF objects. For situations where the repeated creation and 
!      destruction of temporary objects leads to memory issues, it is 
!      recommended to call with {\tt noGarbage} set to {\tt .TRUE.}, fully 
!      removing the entire temporary object from memory.
! \item[{[rc]}] 
!      Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer                 :: localrc        ! local return code
    type(ESMF_Logical)      :: opt_noGarbage  ! helper variable

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_DistGridGetInit, distgrid, rc)
    
    ! Set default flags
    opt_noGarbage = ESMF_FALSE
    if (present(noGarbage)) opt_noGarbage = noGarbage

    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_DistGridDestroy(distgrid, opt_noGarbage, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
 
    ! Mark this DistGrid as invalid
    distgrid%this = ESMF_NULL_POINTER

    ! Set init code
    ESMF_INIT_SET_DELETED(distgrid)
 
    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS
 
  end subroutine ESMF_DistGridDestroy
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DistGridGetDefault()"
!BOP
! !IROUTINE: ESMF_DistGridGet - Get object-wide DistGrid information

! !INTERFACE:
  ! Private name; call using ESMF_DistGridGet()
  subroutine ESMF_DistGridGetDefault(distgrid, keywordEnforcer, delayout, &
    dimCount, tileCount, deCount, minIndexPTile, maxIndexPTile, &
    elementCountPTile, minIndexPDe, maxIndexPDe, elementCountPDe, &
    deToTileMap, indexCountPDe, collocation, regDecompFlag, &
    connectionCount, connectionList, rc)
!
! !ARGUMENTS:
    type(ESMF_DistGrid),    intent(in)            :: distgrid
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_DELayout),    intent(out), optional :: delayout
    integer,                intent(out), optional :: dimCount
    integer,                intent(out), optional :: tileCount
    integer,                intent(out), optional :: deCount
    integer,        target, intent(out), optional :: minIndexPTile(:,:)
    integer,        target, intent(out), optional :: maxIndexPTile(:,:)
    integer,        target, intent(out), optional :: elementCountPTile(:)
    integer,        target, intent(out), optional :: minIndexPDe(:,:)
    integer,        target, intent(out), optional :: maxIndexPDe(:,:)
    integer,        target, intent(out), optional :: elementCountPDe(:)
    integer,        target, intent(out), optional :: deToTileMap(:)
    integer,        target, intent(out), optional :: indexCountPDe(:,:)
    integer,        target, intent(out), optional :: collocation(:)
    logical,                intent(out), optional :: regDecompFlag
    integer,                intent(out), optional :: connectionCount
    type(ESMF_DistGridConnection), &
                    target, intent(out), optional :: connectionList(:)
    integer,                intent(out), optional :: rc
!         
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \item\apiStatusModifiedSinceVersion{5.2.0r}
! \begin{description}
! \item[7.0.0] Added argument {\tt deCount} to simplify access to this variable.
! \item[7.0.0] Added arguments {\tt connectionCount} and {\tt connectionList}
!    to provide user access to the explicitly defined connections in a DistGrid.
! \end{description}
! \end{itemize}
!         
! !DESCRIPTION:
!   Access internal DistGrid information.
!
!   The arguments are:
!   \begin{description}
!   \item[distgrid] 
!     Queried {\tt ESMF\_DistGrid} object.
!   \item[{[delayout]}]
!     {\tt ESMF\_DELayout} object associated with {\tt distgrid}.
!   \item[{[dimCount]}]
!     Number of dimensions (rank) of {\tt distgrid}.
!   \item[{[tileCount]}]
!     Number of tiles in {\tt distgrid}.
!   \item[{[deCount]}]
!     Number of DEs in the DELayout in {\tt distgrid}.
!   \item[{[minIndexPTile]}]
!     \begin{sloppypar}
!     Lower index space corner per tile. Must enter
!     allocated with {\tt shape(minIndexPTile) == (/dimCount, tileCount/)}.
!   \item[{[maxIndexPTile]}]
!     Upper index space corner per tile. Must enter
!     allocated with {\tt shape(maxIndexPTile) == (/dimCount, tileCount/)}.
!   \item[{[elementCountPTile]}]
!     Number of elements in the exclusive region per tile. Must enter
!     allocated with {\tt shape(elementCountPTile) == (/tileCount/)}.
!   \item[{[minIndexPDe]}]
!     Lower index space corner per DE. Must enter
!     allocated with {\tt shape(minIndexPDe) == (/dimCount, deCount/)}.
!   \item[{[maxIndexPDe]}]
!     Upper index space corner per DE. Must enter
!     allocated with {\tt shape(maxIndexPDe) == (/dimCount, deCount/)}.
!   \item[{[elementCountPDe]}]
!     Number of elements in the exclusive region per DE. Must enter
!     allocated with {\tt shape(elementCountPDe) == (/deCount/)}.
!   \item[{[deToTileMap]}]
!     Map each DE uniquely to a tile. Must enter allocated with
!     {\tt shape(deToTileMap) == (/deCount/)}.
!   \item[{[indexCountPDe]}]
!     Number of indices for each dimension per DE. Must enter
!     allocated with {\tt shape(indexCountPDe) == (/dimCount, deCount/)}.
!   \item[{[collocation]}]
!     Collocation identifier for each dimension. Must enter
!     allocated with {\tt shape(collocation) == (/dimCount/)}.
!   \item[{[regDecompFlag]}]
!     Decomposition scheme. A return value of {\tt .true.} indicates
!     a regular decomposition, i.e. the decomposition is based on a 
!     logically rectangular scheme with specific number of DEs along
!     each dimension. A return value of {\tt .false.} indicates that the
!     decomposition was {\em not} generated from a regular decomposition 
!     description, e.g. a {\tt deBlockList} was used instead.
!   \item[{[connectionCount]}]
!     Number of explicitly defined connections in {\tt distgrid}.
!   \item[{[connectionList]}]
!     List of explicitly defined connections in {\tt distgrid}. Must enter
!     allocated with {\tt shape(connectionList) == (/connectionCount/)}.
!   \item[{[rc]}] 
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{sloppypar}
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc                ! local return code
    integer               :: i                      ! helper variable
    integer               :: dimCountAux            ! helper variable
    type(ESMF_InterArray) :: minIndexPTileAux       ! helper variable
    type(ESMF_InterArray) :: maxIndexPTileAux       ! helper variable
    type(ESMF_InterArray) :: elementCountPTileAux   ! helper variable
    type(ESMF_InterArray) :: minIndexPDeAux         ! helper variable
    type(ESMF_InterArray) :: maxIndexPDeAux         ! helper variable
    type(ESMF_InterArray) :: elementCountPDeAux     ! helper variable
    type(ESMF_InterArray) :: deToTileMapAux         ! helper variable
    type(ESMF_InterArray) :: indexCountPDeAux       ! helper variable
    type(ESMF_InterArray) :: collocationAux         ! helper variable
    type(ESMF_Logical)      :: regDecompFlagAux       ! helper variable
    type(ESMF_InterArray) :: connectionListAux      ! helper variable
    integer, pointer        :: farray2D(:,:)          ! helper variable

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    
    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_DistGridGetInit, distgrid, rc)
    
    ! Deal with (optional) array arguments
    minIndexPTileAux = &
      ESMF_InterArrayCreate(farray2D=minIndexPTile, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    maxIndexPTileAux = &
      ESMF_InterArrayCreate(farray2D=maxIndexPTile, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    elementCountPTileAux = ESMF_InterArrayCreate(elementCountPTile, &
      rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    minIndexPDeAux = &
      ESMF_InterArrayCreate(farray2D=minIndexPDe, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    maxIndexPDeAux = &
      ESMF_InterArrayCreate(farray2D=maxIndexPDe, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    elementCountPDeAux = ESMF_InterArrayCreate(elementCountPDe, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    deToTileMapAux = ESMF_InterArrayCreate(deToTileMap, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    indexCountPDeAux = ESMF_InterArrayCreate(farray2D=indexCountPDe, &
      rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    collocationAux = ESMF_InterArrayCreate(collocation, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    connectionListAux = ESMF_InterArrayCreateDGConn(connectionList, &
      initFlag=.false., rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    
    ! call into the C++ interface, which will sort out optional arguments
    call c_ESMC_DistGridGet(distgrid, dimCountAux, tileCount, deCount, &
      minIndexPTileAux, maxIndexPTileAux, elementCountPTileAux, &
      minIndexPDeAux, maxIndexPDeAux, elementCountPDeAux, &
      deToTileMapAux, indexCountPDeAux, collocationAux, &
      regDecompFlagAux, connectionCount, connectionListAux, &
      delayout, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
      
    if (present (regDecompFlag)) &
      regDecompFlag = regDecompFlagAux
    
    ! Set init code for deep C++ objects
    if (present(delayout)) then
      call ESMF_DELayoutSetInitCreated(delayout, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif
    
    ! copy connectionList data
    if (present(connectionList)) then
      ! access the array inside of connectionListAux
      call ESMF_InterArrayGet(connectionListAux, farray2D=farray2D, &
        rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      ! construct the DistGridConnections
      do i=1, size(connectionList)
        call ESMF_DistGridConnectionSetIntl(connectionList(i), &
          farray=farray2D(1:2*dimCountAux+2, i), rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      enddo
    endif
    
    ! copy dimCount
    if (present(dimCount)) then
      dimCount = dimCountAux
    endif

    ! garbage collection
    call ESMF_InterArrayDestroy(minIndexPTileAux, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterArrayDestroy(maxIndexPTileAux, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterArrayDestroy(elementCountPTileAux, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterArrayDestroy(minIndexPDeAux, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterArrayDestroy(maxIndexPDeAux, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterArrayDestroy(elementCountPDeAux, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterArrayDestroy(deToTileMapAux, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterArrayDestroy(indexCountPDeAux, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterArrayDestroy(collocationAux, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterArrayDestroy(connectionListAux, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_DistGridGetDefault
!------------------------------------------------------------------------------

#ifdef PROTOCODE
! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DistGridGetPDe()"
!BOPI
! !IROUTINE: ESMF_DistGridGet - Get DE local information about DistGrid

! !INTERFACE:
  ! Private name; call using ESMF_DistGridGet()
  subroutine ESMF_DistGridGetPDe(distgrid, de, regDecompDeCoord, tile, rc)
!
! !ARGUMENTS:
    type(ESMF_DistGrid),    intent(in)            :: distgrid
    integer,                intent(in)            :: de
    integer, target,        intent(out), optional :: regDecompDeCoord(:)
    integer,                intent(out), optional :: tile
    integer,                intent(out), optional :: rc
!         
!
! !DESCRIPTION:
!   Access internal DistGrid information.
!
!   The arguments are:
!   \begin{description}
!   \item[distgrid]
!     Queried {\tt ESMF\_DistGrid} object.
!   \item[de]
!     DE for which information is requested. {\tt \[0,..,deCount-1\]}
!   \item[{[regDecompDeCoord]}]
!     For regular decompositions upon return this array holds the index space
!     tuple of the specified DE with respect to the local tile. For other
!     decompositions a run-time warning will be issued if
!     {\tt regDecompDeCoord} is requested.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_DistGridGetInit, distgrid, rc)
    
    ! call into the C++ interface, which will sort out optional arguments
!    call c_ESMC_DistGridGet(distgrid, delayout, tileCount, tileListAux, &
!      dimCount, dimExtentAux, regDecompFlag, localrc)
!    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
!      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    !if (present(rc)) rc = ESMF_SUCCESS   TODO: enable once implemented

  end subroutine ESMF_DistGridGetPDe
!------------------------------------------------------------------------------
#endif

! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DistGridGetPLocalDe()"
!BOP
! !IROUTINE: ESMF_DistGridGet - Get DE-local DistGrid information

! !INTERFACE:
  ! Private name; call using ESMF_DistGridGet()
  subroutine ESMF_DistGridGetPLocalDe(distgrid, localDe, keywordEnforcer, &
    collocation, arbSeqIndexFlag, seqIndexList, elementCount, rc)
!
! !ARGUMENTS:
    type(ESMF_DistGrid),    intent(in)            :: distgrid
    integer,                intent(in)            :: localDe
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,                intent(in),  optional :: collocation
    logical,                intent(out), optional :: arbSeqIndexFlag
    integer,        target, intent(out), optional :: seqIndexList(:)
    integer,                intent(out), optional :: elementCount
    integer,                intent(out), optional :: rc
!         
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
!   Access internal DistGrid information.
!
!   The arguments are:
!   \begin{description}
!   \item[distgrid]
!     Queried {\tt ESMF\_DistGrid} object.
!   \item[localDe]
!     Local DE for which information is requested. {\tt [0,..,localDeCount-1]}
!   \item[{[collocation]}]
!     Collocation for which information is requested. Default to first
!     collocation in {\tt collocation} list.
!   \item[{[arbSeqIndexFlag]}]
!     Indicates whether collocation is associated with arbitrary sequence
!     indices.
!   \item[{[seqIndexList]}]
!     \begin{sloppypar}
!     List of sequence indices for the elements on {\tt localDe}, with
!     {\tt shape(seqIndexList) == (/elementCountPDe(localDeToDeMap(localDe))/)}.
!     \end{sloppypar}
!   \item[{[elementCount]}]
!     Number of elements in the localDe, i.e. identical to
!     elementCountPDe(localDeToDeMap(localDe)).
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc            ! local return code
    type(ESMF_Logical)    :: arbSeqIndexFlagAux ! helper variable
    type(ESMF_InterArray) :: seqIndexListAux    ! helper variable

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    
    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_DistGridGetInit, distgrid, rc)
    
    ! Deal with (optional) array arguments
    seqIndexListAux = ESMF_InterArrayCreate(seqIndexList, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! call into the C++ interface, which will sort out optional arguments
    call c_ESMC_DistGridGetPLocalDe(distgrid, localDe, collocation, &
      arbSeqIndexFlagAux, seqIndexListAux, elementCount, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
      
    ! logicals
    if (present (arbSeqIndexFlag)) &
      arbSeqIndexFlag = arbSeqIndexFlagAux
    
    ! garbage collection
    call ESMF_InterArrayDestroy(seqIndexListAux, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_DistGridGetPLocalDe
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DistGridGetPLocalDePDim()"
!BOP
! !IROUTINE: ESMF_DistGridGet - Get DE-local DistGrid information for a specific dimension

! !INTERFACE:
  ! Private name; call using ESMF_DistGridGet()
  subroutine ESMF_DistGridGetPLocalDePDim(distgrid, localDe, dim, &
           indexList, keywordEnforcer, rc)
!
! !ARGUMENTS:
    type(ESMF_DistGrid),    intent(in)            :: distgrid
    integer,                intent(in)            :: localDe
    integer,                intent(in)            :: dim
    integer,        target, intent(out)           :: indexList(:)
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,                intent(out), optional :: rc
!         
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
!   Access internal DistGrid information.
!
!   The arguments are:
!   \begin{description}
!   \item[distgrid] 
!     Queried {\tt ESMF\_DistGrid} object.
!   \item[localDe] 
!     Local DE for which information is requested. {\tt [0,..,localDeCount-1]}
!   \item[dim] 
!     Dimension for which information is requested. {\tt [1,..,dimCount]}
!   \item[indexList]
!     Upon return this holds the list of DistGrid tile-local indices
!     for {\tt localDe} along dimension {\tt dim}. The supplied variable 
!     must be at least of size {\tt indexCountPDe(dim, localDeToDeMap(localDe))}.
!   \item[{[rc]}] 
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code
    type(ESMF_InterArray) :: indexListAux    ! helper variable

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    
    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_DistGridGetInit, distgrid, rc)
    
    ! Deal with (optional) array arguments
    indexListAux = ESMF_InterArrayCreate(indexList, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! call into the C++ interface, which will sort out optional arguments
    call c_ESMC_DistGridGetPLocalDePDim(distgrid, localDe, dim, indexListAux, &
      localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    
    ! garbage collection
    call ESMF_InterArrayDestroy(indexListAux, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_DistGridGetPLocalDePDim
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DistGridIsCreated()"
!BOP
! !IROUTINE: ESMF_DistGridIsCreated - Check whether a DistGrid object has been created

! !INTERFACE:
  function ESMF_DistGridIsCreated(distgrid, keywordEnforcer, rc)
! !RETURN VALUE:
    logical :: ESMF_DistGridIsCreated
!
! !ARGUMENTS:
    type(ESMF_DistGrid), intent(in)            :: distgrid
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,             intent(out), optional :: rc

! !DESCRIPTION:
!   Return {\tt .true.} if the {\tt distgrid} has been created. Otherwise return
!   {\tt .false.}. If an error occurs, i.e. {\tt rc /= ESMF\_SUCCESS} is 
!   returned, the return value of the function will also be {\tt .false.}.
!
! The arguments are:
!   \begin{description}
!   \item[distgrid]
!     {\tt ESMF\_DistGrid} queried.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
  !-----------------------------------------------------------------------------    
    ESMF_DistGridIsCreated = .false.   ! initialize
    if (present(rc)) rc = ESMF_SUCCESS
    if (ESMF_DistGridGetInit(distgrid)==ESMF_INIT_CREATED) &
      ESMF_DistGridIsCreated = .true.
  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DistGridMatch()"
!BOP
! !IROUTINE: ESMF_DistGridMatch - Check if two DistGrid objects match

! !INTERFACE:
  function ESMF_DistGridMatch(distgrid1, distgrid2, keywordEnforcer, rc)
!
! !RETURN VALUE:
    type(ESMF_DistGridMatch_Flag) :: ESMF_DistGridMatch
      
! !ARGUMENTS:
    type(ESMF_DistGrid),  intent(in)            :: distgrid1
    type(ESMF_DistGrid),  intent(in)            :: distgrid2
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,              intent(out), optional :: rc  
!
! !DESCRIPTION:
!   Determine to which level {\tt distgrid1} and {\tt distgrid2} match. 
!
!   Returns a range of values of type {\tt ESMF\_DistGridMatch\_Flag},
!   indicating how closely the DistGrids match. For a description of the
!   possible return values, see~\ref{const:distgridmatch}. 
!   Note that this call only performs PET local matching. Different return values
!   may be returned on different PETs for the same DistGrid pair.
!
!   The arguments are:
!   \begin{description}
!   \item[distgrid1] 
!     {\tt ESMF\_DistGrid} object.
!   \item[distgrid2] 
!     {\tt ESMF\_DistGrid} object.
!   \item[{[rc]}] 
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer                       :: localrc      ! local return code
    type(ESMF_DistGridMatch_Flag) :: matchResult

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    
    ! Initialize return value to invalid, in case of bail-out
    ESMF_DistGridMatch = ESMF_DISTGRIDMATCH_INVALID
    
    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_DistGridGetInit, distgrid1, rc)
    ESMF_INIT_CHECK_DEEP(ESMF_DistGridGetInit, distgrid2, rc)
    
    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_DistGridMatch(distgrid1, distgrid2, matchResult, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Set the actual return value
    ESMF_DistGridMatch = matchResult

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS
    
  end function ESMF_DistGridMatch
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DistGridPrint()"
!BOP
! !IROUTINE: ESMF_DistGridPrint - Print DistGrid information

! !INTERFACE:
  subroutine ESMF_DistGridPrint(distgrid, keywordEnforcer, rc)
!
! !ARGUMENTS:
    type(ESMF_DistGrid),  intent(in)            :: distgrid
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,              intent(out), optional :: rc  
!         
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
!     Prints internal information about the specified {\tt ESMF\_DistGrid} 
!     object to {\tt stdout}. \\
!
!     The arguments are:
!     \begin{description}
!     \item[distgrid] 
!          Specified {\tt ESMF\_DistGrid} object.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_DistGridGetInit, distgrid, rc)
    
    ! Flush before crossing language interface to ensure correct output order
    call ESMF_UtilIOUnitFlush(ESMF_UtilIOStdout, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Call into the C++ interface
    call c_ESMC_DistGridPrint(distgrid, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS
 
  end subroutine ESMF_DistGridPrint
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DistGridSet()"
!BOPI
! !IROUTINE: ESMF_DistGridSet - Set DistGrid sequence index collocation labels

! !INTERFACE:
  subroutine ESMF_DistGridSet(distgrid, collocationPDim, rc)
!
! !ARGUMENTS:
    type(ESMF_DistGrid),  intent(inout)           :: distgrid
    integer,              intent(in)              :: collocationPDim(:)
    integer,              intent(out),  optional  :: rc  
!
! !DESCRIPTION:
!      Set the sequence index collocation labels in {\tt distgrid}.
!      The method returns an error code if problems are found.  
!
!     The arguments are:
!     \begin{description}
!     \item[distgrid] 
!          Specified {\tt ESMF\_DistGrid} object.
!     \item[collocationPDim] 
!          List of size {\tt dimCount} specifying which dimensions are
!          covered by which sequence index. Each entry is associated with the
!          corresponding dimension. Dimensions with identical entries in the
!          {\tt collocationPDim} argument are collocated within the same
!          sequence index space. Dimensions with different entries are located
!          in orthogonal sequence index spaces.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    integer               :: localrc      ! local return code
    type(ESMF_InterArray) :: collocationPDimAux  ! helper variable

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    
    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_DistGridGetInit, distgrid, rc)
    
    collocationPDimAux = &
      ESMF_InterArrayCreate(collocationPDim, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_DistGridSet(distgrid, collocationPDimAux, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! garbage collection
    call ESMF_InterArrayDestroy(collocationPDimAux, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS
    
  end subroutine ESMF_DistGridSet
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DistGridValidate()"
!BOP
! !IROUTINE: ESMF_DistGridValidate - Validate DistGrid internals

! !INTERFACE:
  subroutine ESMF_DistGridValidate(distgrid, keywordEnforcer, rc)
!
! !ARGUMENTS:
    type(ESMF_DistGrid),  intent(in)            :: distgrid
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,              intent(out), optional :: rc  
!         
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
!      Validates that the {\tt distgrid} is internally consistent.
!      The method returns an error code if problems are found.  
!
!     The arguments are:
!     \begin{description}
!     \item[distgrid] 
!          Specified {\tt ESMF\_DistGrid} object.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    
    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_DistGridGetInit, distgrid, rc)
    
    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_DistGridValidate(distgrid, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS
    
  end subroutine ESMF_DistGridValidate
!------------------------------------------------------------------------------


! -------------------------- ESMF-internal method -----------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DistGridGetInit"
!BOPI
! !IROUTINE: ESMF_DistGridGetInit - Internal access routine for init code
!
! !INTERFACE:
      function ESMF_DistGridGetInit(distgrid) 
!
! !RETURN VALUE:
      ESMF_INIT_TYPE :: ESMF_DistGridGetInit   
!
! !ARGUMENTS:
      type(ESMF_DistGrid), intent(in), optional :: distgrid
!
! !DESCRIPTION:
!      Access deep object init code.
!
!     The arguments are:
!     \begin{description}
!     \item [distgrid]
!           DistGrid object.
!     \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    if (present(distgrid)) then
      ESMF_DistGridGetInit = ESMF_INIT_GET(distgrid)
    else
      ESMF_DistGridGetInit = ESMF_INIT_CREATED
    endif

    end function ESMF_DistGridGetInit
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DistGridSetInitCreated()"
!BOPI
! !IROUTINE: ESMF_DistGridSetInitCreated - Set DistGrid init code to "CREATED"

! !INTERFACE:
  subroutine ESMF_DistGridSetInitCreated(distgrid, rc)
!
! !ARGUMENTS:
    type(ESMF_DistGrid),  intent(inout)           :: distgrid
    integer,              intent(out),  optional  :: rc  
!
! !DESCRIPTION:
!      Set init code in DistGrid object to "CREATED".
!
!     The arguments are:
!     \begin{description}
!     \item[distgrid] 
!          Specified {\tt ESMF\_DistGrid} object.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
!------------------------------------------------------------------------------

    ! initialize return code; assume routine not implemented
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    
    ! Set init code
    ESMF_INIT_SET_CREATED(distgrid)

    ! Return success
    if (present(rc)) rc = ESMF_SUCCESS
    
  end subroutine ESMF_DistGridSetInitCreated
!------------------------------------------------------------------------------


! -------------------------- ESMF-internal method -----------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DistGridSeqIndex"
!BOPI
! !IROUTINE: ESMF_DistGridSeqIndex - Compute the canonical sequence index
!
! !INTERFACE:
  function ESMF_DistGridSeqIndex(minIndex, maxIndex, index, rc) 
!
! !RETURN VALUE:
    integer :: ESMF_DistGridSeqIndex   
!
! !ARGUMENTS:
    integer,                        intent(in)            :: minIndex(:)
    integer,                        intent(in)            :: maxIndex(:)
    integer,                        intent(in)            :: index(:)
    integer,                        intent(out), optional :: rc
!
! !DESCRIPTION:
!   Compute the canonical sequence index (single integer number) associated
!   with the index tuple passed in as {\tt index}. Canonical indices start
!   at 1 for the starting corner, specified via the {\tt minIndex} argument,
!   and increase from there in column major fashion up to the end corner. The
!   end corner is defined by the {\tt maxIndex} argument.
!
!   If the {\tt index} tuple is not within the index space defined by
!   {\tt minIndex} and {\tt maxIndex}, an invalid sequence index value 
!   of -1 is returned by this function.
!
!   The size of {\tt minIndex}, {\tt maxIndex}, and {\tt index} must all be
!   the same, or an error is returned in the return code.
!
!   The arguments are:
!   \begin{description}
!   \item[minIndex]
!        Index space tuple of the lower corner of the single tile.
!   \item[maxIndex]
!        Index space tuple of the upper corner of the single tile.
!   \item[index]
!        index space tuple of the index point to be converted into the 
!        sequence index.
!   \item[{[rc]}]
!        Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc            ! local return code
    integer                 :: seqIndex
    integer                 :: i, count

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    
    ESMF_DistGridSeqIndex = -1 ! initialize to an invalid value
    
    ! error checking of the input
    count = size(minIndex)
    if (count /= size(maxIndex)) then
      call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_SIZE, &
        msg="size(maxIndex) must match size(minIndex)", &
        ESMF_CONTEXT, rcToReturn=rc)
      return
    endif
    if (count /= size(index)) then
      call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_SIZE, &
        msg="size(index) must match size(minIndex)", &
        ESMF_CONTEXT, rcToReturn=rc)
      return
    endif
    
    seqIndex = 0 ! initialize to base 0
    do i = count, 1, -1
      ! error check:
      if (index(i)<minIndex(i) .or. index(i)>maxIndex(i)) then
        seqIndex = -2 ! will come out as -1 after the shift below
        exit
      endif
      ! first time multiply with zero intentionally:
      seqIndex = seqIndex * (maxIndex(i)-minIndex(i)+1)
      seqIndex = seqIndex + index(i)-minIndex(i)
    enddo
    seqIndex = seqIndex+1 ! shift sequence index to base 1 
 
    ! return successfully
    ESMF_DistGridSeqIndex = seqIndex
    if (present(rc)) rc = ESMF_SUCCESS

  end function ESMF_DistGridSeqIndex
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_Decomp_FlagEQ"
!BOPI
! !IROUTINE: ESMF_DecompFlagEqual - Equality of DecompFlag statuses
!
! !INTERFACE:
      function ESMF_Decomp_FlagEQ(decompFlag1, decompFlag2)

! !RETURN VALUE:
      logical :: ESMF_Decomp_FlagEQ

! !ARGUMENTS:

      type (ESMF_Decomp_Flag), intent(in) :: &
         decompFlag1,      &! Two igrid statuses to compare for
         decompFlag2        ! equality

! !DESCRIPTION:
!     This routine compares two ESMF DecompFlag statuses to see if
!     they are equivalent.
!
!     The arguments are:
!     \begin{description}
!     \item[decompFlag1, decompFlag2]
!          Two decomposition flags to compare for equality
!     \end{description}
!
!EOPI

      ESMF_Decomp_FlagEQ = (decompFlag1%value == &
                            decompFlag2%value)

      end function ESMF_Decomp_FlagEQ
!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_Decomp_FlagNE"
!BOPI
! !IROUTINE: ESMF_DecompFlagNotEqual - Non-equality of DecompFlag statuses
!
! !INTERFACE:
      function ESMF_Decomp_FlagNE(decompFlag1, decompFlag2)

! !RETURN VALUE:
      logical :: ESMF_Decomp_FlagNE

! !ARGUMENTS:

      type (ESMF_Decomp_Flag), intent(in) :: &
         decompFlag1,      &! Two DecompFlag Statuses to compare for
         decompFlag2        ! inequality

! !DESCRIPTION:
!     This routine compares two ESMF DecompFlag statuses to see if
!     they are unequal.
!
!     The arguments are:
!     \begin{description}
!     \item[decompFlag1, decompFlag2]
!          Two decomposition flags to compare for inequality
!     \end{description}
!
!EOPI

      ESMF_Decomp_FlagNE = (decompFlag1%value /= &
                            decompFlag2%value)

      end function ESMF_Decomp_FlagNE
#undef  ESMF_METHOD



end module ESMF_DistGridMod
