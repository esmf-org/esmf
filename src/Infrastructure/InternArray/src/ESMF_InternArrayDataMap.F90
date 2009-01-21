! $Id: ESMF_InternArrayDataMap.F90,v 1.12.2.3 2009/01/21 21:25:22 cdeluca Exp $
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
#define ESMF_FILENAME "ESMF_ArrayDataMap.F90"
!==============================================================================
!
! ESMF InternArrayDataMap module
module ESMF_InternArrayDataMapMod
!
!==============================================================================
!
! !PURPOSE:
!
! The code in this file implements the ArrayDataMap derived type
!  and functions which operate on ArrayDataMaps.  
!  This type is implemented in Fortran 90 and a corresponding
!  C++ interface is provided for access.
!
! ArrayDataMaps are used to store the mapping of the array index orders
!   compared to the igrid specifications; to indicate C++ vs Fortran
!   index ordering, and to store complex data type interleave information.
!
!------------------------------------------------------------------------------
! INCLUDES
#include "ESMF.h"

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!
! This section contains the basic derived type for encapsulating the
!  various mappings between igrids, arrays, and interleaved arrays, including
!  the internal subroutines and functions which operate on them.
!
!

!------------------------------------------------------------------------------
!BOPI
! !MODULE: ESMF_ArrayDataMapMod
      
! !USES:
  use ESMF_UtilTypesMod     ! ESMF utility types
  use ESMF_InitMacrosMod    ! ESMF initializer macros
  use ESMF_BaseMod          ! ESMF base class
  use ESMF_LogErrMod        ! ESMF error handling
  use ESMF_IOSpecMod

  implicit none

!------------------------------------------------------------------------------
! !PUBLIC TYPES:
  private

!------------------------------------------------------------------------------
!  ! Interleaved types are used when there are multiple variables or
!  ! if individual data items are > scalar.  This is the public flag.

  type ESMF_InterleaveFlag
  sequence
  private
    integer :: il
  end type

  type(ESMF_InterleaveFlag), parameter ::  &
    ESMF_INTERLEAVE_BY_BLOCK = ESMF_InterleaveFlag(1), &
    ESMF_INTERLEAVE_BY_ITEM  = ESMF_InterleaveFlag(2)

!------------------------------------------------------------------------------
!  ! Expected to be used most often for packed bundles, to allow access to
!  ! data for an individual field.  Can also be used to describe vector data
!  ! that is packed into a single array.  This is the private object.

  type ESMF_InterleaveType
  sequence
  private
    type(ESMF_InterleaveFlag) :: il_type
    integer :: il_start
    integer :: il_end
    integer :: il_strides
  end type

!------------------------------------------------------------------------------
!  ! This describes how the data items are located relative to an individual
!  ! cell or element.  Putting this here means that different Fields which
!  ! have different relative locations can still share the same IGrid.
!  ! See the IGrid object for a description of 'staggering' which is a 
!  ! per-IGrid concept. 

  type ESMF_RelLoc
  sequence
  !private
    integer :: relloc
  end type

  type(ESMF_RelLoc), parameter :: &
    ESMF_CELL_UNDEFINED = ESMF_RelLoc( 0), &
    ESMF_CELL_CENTER    = ESMF_RelLoc( 1), &
    ESMF_CELL_NFACE     = ESMF_RelLoc( 2), &
    ESMF_CELL_SFACE     = ESMF_RelLoc( 3), &
    ESMF_CELL_EFACE     = ESMF_RelLoc( 4), &
    ESMF_CELL_WFACE     = ESMF_RelLoc( 5), &
    ESMF_CELL_NECORNER  = ESMF_RelLoc( 6), &
    ESMF_CELL_NWCORNER  = ESMF_RelLoc( 7), &
    ESMF_CELL_SECORNER  = ESMF_RelLoc( 8), &
    ESMF_CELL_SWCORNER  = ESMF_RelLoc( 9), &
    ESMF_CELL_TOPFACE   = ESMF_RelLoc(10), &
    ESMF_CELL_BOTFACE   = ESMF_RelLoc(11), &
    ESMF_CELL_CELL      = ESMF_RelLoc(12), &
    ESMF_CELL_VERTEX    = ESMF_RelLoc(13)
 
!------------------------------------------------------------------------------
!  ! A set of predefined index orders, which shortcut setting
!  ! the dimension order array explicitly.   Note that this is merely
!  ! an alternative to specifying a 1D integer array of index numbers.

  type ESMF_IndexOrder
  sequence
  private
    integer :: iorder
  end type
 
  type(ESMF_IndexOrder), parameter :: &
    ESMF_INDEX_I   = ESMF_IndexOrder(0), &
    ESMF_INDEX_IJ  = ESMF_IndexOrder(1), &
    ESMF_INDEX_JI  = ESMF_IndexOrder(2), &
    ESMF_INDEX_IJK = ESMF_IndexOrder(3), &
    ESMF_INDEX_JIK = ESMF_IndexOrder(4), &
    ESMF_INDEX_KJI = ESMF_IndexOrder(5), &
    ESMF_INDEX_IKJ = ESMF_IndexOrder(6), &
    ESMF_INDEX_JKI = ESMF_IndexOrder(7), &
    ESMF_INDEX_KIJ = ESMF_IndexOrder(8)

!------------------------------------------------------------------------------
!  ! ESMF_ArrayDataMap
!  ! The data map type, which should fully describe the mapping
!  ! between index orders in the igrid and the memory layout of
!  ! the data array, plus other metadata info such as where the
!  ! data is relative to the igrid, and any interleaving info needed.

  type ESMF_InternArrayDataMap
  sequence
  !private
#ifndef ESMF_NO_INITIALIZERS
    type(ESMF_Status) :: status = ESMF_STATUS_UNINIT
#else
    type(ESMF_Status) :: status 
#endif
    ! index orders - how the igrid dims (X,Y,Z) or (u,v) or (i,j,k)
    !  map onto the array memory layout as declared.  
    !  set dim numbers to 1-N, use 0 for dims which are part of data
    !  items and do not map to the igrid dims.
    integer :: dataRank                             ! scalar, vector, etc.
    integer, dimension(ESMF_MAXDIM) :: dataDimOrder ! 0 = not a data dim
    integer, dimension(ESMF_MAXDIM) :: dataNonIGridCounts ! for non-igrid dims
    ! TODO: plus C++ vs F90 native index flag
    ! TODO: plus complex number interleave flag? (r,i) vs (rrr...) (iii...)
    ESMF_INIT_DECLARE
  end type


!------------------------------------------------------------------------------
! !PUBLIC MEMBER TYPES:
!
  public ESMF_InternArrayDataMap

  public ESMF_InterleaveFlag
  public ESMF_INTERLEAVE_BY_BLOCK, ESMF_INTERLEAVE_BY_ITEM

  public ESMF_InterleaveType ! public only for field datamap use

  public ESMF_IndexOrder
  public ESMF_INDEX_I,   ESMF_INDEX_IJ,  ESMF_INDEX_JI
  public ESMF_INDEX_IJK, ESMF_INDEX_JIK, ESMF_INDEX_KJI
  public ESMF_INDEX_IKJ, ESMF_INDEX_JKI, ESMF_INDEX_KIJ 

  public ESMF_RelLoc
  public ESMF_CELL_UNDEFINED, ESMF_CELL_CENTER
  public ESMF_CELL_NFACE,     ESMF_CELL_SFACE
  public ESMF_CELL_EFACE,     ESMF_CELL_WFACE
  public ESMF_CELL_NECORNER,  ESMF_CELL_NWCORNER
  public ESMF_CELL_SECORNER,  ESMF_CELL_SWCORNER
  public ESMF_CELL_TOPFACE,   ESMF_CELL_BOTFACE
  public ESMF_CELL_CELL,      ESMF_CELL_VERTEX

! !PUBLIC MEMBER FUNCTIONS:
!
  public ESMF_ArrayDataMapSetDefault
  public ESMF_ArrayDataMapSetInvalid

  public ESMF_ArrayDataMapGet, ESMF_ArrayDataMapSet

  public ESMF_ArrayDataMapValidate
  public ESMF_ArrayDataMapPrint

  public ESMF_RelLocString, ESMF_InterleaveFlagString, ESMF_IndexOrderString
  public ESMF_InterleaveTypeString

  public operator(.eq.), operator(.ne.)

!EOPI

  public ESMF_ArrayDataMapInit
  public ESMF_ArrayDataMapGetInit
 
!
!------------------------------------------------------------------------------
! leave the following line as-is; it will insert the cvs ident string
! into the object file for tracking purposes.
  character(*), parameter, private :: version =  &
    '$Id: ESMF_InternArrayDataMap.F90,v 1.12.2.3 2009/01/21 21:25:22 cdeluca Exp $'
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
!
! interface blocks here
!
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_ArrayDataMapSetDefault - Set all defaults in a ArrayDataMap

! !INTERFACE:
  interface ESMF_ArrayDataMapSetDefault

! !PRIVATE MEMBER FUNCTIONS:
    module procedure ESMF_ArrayDataMapSetDefIndex
    module procedure ESMF_ArrayDataMapSetDefExplicit

! !DESCRIPTION:
! This interface provides a single entry point for {\tt ESMF\_ArrayDataMap}
!  initialization methods.
!EOPI

  end interface 
                                      

!------------------------------------------------------------------------------
! overload .eq. & .ne. with additional derived types so you can compare
!  them as if they were simple integers.

  interface operator (.eq.)
    module procedure ESMF_rleq
    module procedure ESMF_ileq
    module procedure ESMF_ioeq
  end interface

  interface operator (.ne.)
    module procedure ESMF_rlne
    module procedure ESMF_ilne
    module procedure ESMF_ione
  end interface

!------------------------------------------------------------------------------
! end of declarations & definitions
!------------------------------------------------------------------------------

      contains

!------------------------------------------------------------------------------
! function to compare two ESMF_RelLoc flags to see if they're the same or not

function ESMF_rleq(rl1, rl2)
 logical ESMF_rleq
 type(ESMF_RelLoc), intent(in) :: rl1, rl2

 ESMF_rleq = (rl1%relloc .eq. rl2%relloc)
end function

function ESMF_rlne(rl1, rl2)
 logical ESMF_rlne
 type(ESMF_RelLoc), intent(in) :: rl1, rl2

 ESMF_rlne = (rl1%relloc .ne. rl2%relloc)
end function

!------------------------------------------------------------------------------
! function to compare two ESMF_InterleaveFlag flags 

function ESMF_ileq(il1, il2)
 logical ESMF_ileq
 type(ESMF_InterleaveFlag), intent(in) :: il1, il2

 ESMF_ileq = (il1%il .eq. il2%il)
end function

function ESMF_ilne(il1, il2)
 logical ESMF_ilne
 type(ESMF_InterleaveFlag), intent(in) :: il1, il2

 ESMF_ilne = (il1%il .ne. il2%il)
end function


!------------------------------------------------------------------------------
! function to compare two ESMF_IndexOrder flags 

function ESMF_ioeq(io1, io2)
 logical ESMF_ioeq
 type(ESMF_IndexOrder), intent(in) :: io1, io2

 ESMF_ioeq = (io1%iorder .eq. io2%iorder)
end function

function ESMF_ione(io1, io2)
 logical ESMF_ione
 type(ESMF_IndexOrder), intent(in) :: io1, io2

 ESMF_ione = (io1%iorder .ne. io2%iorder)
end function


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayDataMapGet"
!BOP
! !IROUTINE: ESMF_ArrayDataMapGet - Get values from an ArrayDataMap
!
! !INTERFACE:
      subroutine ESMF_ArrayDataMapGet(arraydatamap, dataRank, dataIndexList, &
                                      counts, rc)
!
! !ARGUMENTS:
      type(ESMF_InternArrayDataMap), intent(inout) :: arraydatamap  
      integer, intent(out), optional :: dataRank    
      integer, dimension(:), intent(out), optional :: dataIndexList
      integer, dimension(:), intent(out), optional :: counts 
      integer, intent(out), optional :: rc       
!
! !DESCRIPTION:
!   Return information from an {\tt ESMF\_ArrayDataMap}.
!
!   The arguments are:
!     \begin{description}
!     \item [arraydatamap]
!           An {\tt ESMF\_ArrayDataMap}.
!     \item [{[datarank]}]
!	    The number of dimensions in the data {\tt ESMF\_Array}.
!     \item [{[dataIndexList]}] 
!           An integer array, {\tt datarank} long, which specifies
!           the mapping between rank numbers in the {\tt ESMF\_IGrid}
!           and the {\tt ESMF\_Array}.  If there is no correspondance
!           (because the {\tt ESMF\_Array} has a higher rank than the
!           {\tt ESMF\_IGrid}) the index value will be 0.
!     \item [{[counts]}]
!           An integer array, with length ({\tt datarank} minus the igrid rank).
!           Each entry is the default item count which would be used
!           for those ranks which do not correspond to igrid ranks when
!           creating an {\tt ESMF\_Field} using only an 
!           an {\tt ESMF\_ArraySpec} and an {\tt ESMF\_ArrayDataMap}.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
 
        ! local vars
        integer :: status                     ! local error status
        logical :: rcpresent                  ! did user specify rc?
        integer :: i, dimlength

        ! initialize return code
        status = ESMF_RC_NOT_IMPL
        rcpresent = .FALSE.
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_RC_NOT_IMPL
        endif

    ! Check init status of arguments
    ESMF_INIT_CHECK_SHALLOW(ESMF_ArrayDataMapGetInit, ESMF_ArrayDataMapInit, arraydatamap)

        if (present(dataRank)) dataRank = arraydatamap%dataRank

        if (present(dataIndexList)) then
           dimlength = size(dataIndexList,1)
           if (dimlength .lt. arraydatamap%dataRank) then
              if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                "dataIndexList array too short for dataRank", &
                                 ESMF_CONTEXT, rc)) return
           endif

           do i=1, dimlength
             dataIndexList(i) = arraydatamap%dataDimOrder(i)
           enddo
        endif

        if (present(counts)) then
           dimlength = size(counts)
           do i=1, dimlength
             counts(i) = arraydatamap%dataNonIGridCounts(i)
           enddo
        endif

        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_ArrayDataMapGet


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayDataMapPrint"
!BOP
! !IROUTINE: ESMF_ArrayDataMapPrint - Print an ArrayDataMap
!
! !INTERFACE:
      subroutine ESMF_ArrayDataMapPrint(arraydatamap, options, rc)
!
! !ARGUMENTS:
      type(ESMF_InternArrayDataMap), intent(inout) :: arraydatamap
      character (len = *), intent(in), optional :: options
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!     Prints information about the {\tt arraydatamap} to {\tt stdout}.
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
!     \item [arraydatamap]
!           {\tt ESMF\_ArrayDataMap} to print.
!     \item [{[options]}]
!           Print options are not yet supported.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP

        integer :: i, j
        !character(len=ESMF_MAXSTR) :: msgbuf

    ! Check init status of arguments
    ESMF_INIT_CHECK_SHALLOW(ESMF_ArrayDataMapGetInit, ESMF_ArrayDataMapInit, arraydatamap)

      !jw  write(msgbuf,*)  "ArrayDataMap print:"
      !jw  call ESMF_LogWrite(msgbuf, ESMF_LOG_INFO)
        write(*,*)  "ArrayDataMap print:"
        if (arraydatamap%status .ne. ESMF_STATUS_READY) then
      !jw    write(msgbuf,*)  "Uninitialized or Invalid object"
      !jw    call ESMF_LogWrite(msgbuf, ESMF_LOG_INFO)
          write(*,*)  "Uninitialized or Invalid object"
          if (present(rc)) rc = ESMF_SUCCESS
          return
        endif

        ! individual data item information
      !jw  write(msgbuf,*)  " Data rank = ", arraydatamap%dataRank
      !jw  call ESMF_LogWrite(msgbuf, ESMF_LOG_INFO)
        write(*,*)  " Data rank = ", arraydatamap%dataRank
      !jw  write(msgbuf,*)  " Data Index Order and Lengths for non-IGrid Indices:"
      !jw  call ESMF_LogWrite(msgbuf, ESMF_LOG_INFO)
        write(*,*)  " Data Index Order and Lengths for non-IGrid Indices:"
        j = 1
        do i=1, ESMF_MAXDIM
            if (arraydatamap%dataDimOrder(i) .eq. 0) then
      !jw         write(msgbuf,*)  i, "Non-IGrid index, length = ", arraydatamap%dataNonIGridCounts(j)
      !jw         call ESMF_LogWrite(msgbuf, ESMF_LOG_INFO)
               write(*,*)  i, "Non-IGrid index, length = ", arraydatamap%dataNonIGridCounts(j)
               j = j + 1
            else
      !jw         write(msgbuf,*)  i, "IGrid index ", arraydatamap%dataDimOrder(i)
      !jw         call ESMF_LogWrite(msgbuf, ESMF_LOG_INFO)
               write(*,*)  i, "IGrid index ", arraydatamap%dataDimOrder(i)
            endif
        enddo

        end subroutine ESMF_ArrayDataMapPrint


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayDataMapSet"
!BOP
! !IROUTINE: ESMF_ArrayDataMapSet - Set values in an ArrayDataMap
!
! !INTERFACE:
      subroutine ESMF_ArrayDataMapSet(arraydatamap, dataRank, &
                                      dataIndexList, counts, rc)
!
! !ARGUMENTS:
      type(ESMF_InternArrayDataMap), intent(inout) :: arraydatamap  
      integer, intent(in), optional :: dataRank    
      integer, dimension(:), intent(in), optional :: dataIndexList
      integer, dimension(:), intent(in), optional :: counts 
      integer, intent(out), optional :: rc       
!
! !DESCRIPTION:
!   Set values in an {\tt ESMF\_ArrayDataMap}.
!
!   The arguments are:
!     \begin{description}
!     \item [arraydatamap]
!           An {\tt ESMF\_ArrayDataMap}.
!     \item [{[datarank]}]
!	    The number of dimensions in the data {\tt ESMF\_Array}.
!     \item [{[dataIndexList]}] 
!           An integer array, {\tt datarank} long, which specifies
!           the mapping between rank numbers in the {\tt ESMF\_IGrid}
!           and the {\tt ESMF\_Array}.  If there is no correspondance
!           (because the {\tt ESMF\_Array} has a higher rank than the
!           {\tt ESMF\_IGrid}) the index value must be 0. 
!
!           For example, if the data array is 3D and the igrid is 2D,
!           and the first and second data indices correspond to the igrid
!           and the last data index is to store duplicate values for the
!           same igrid location, then the value for this input would be
!           {\tt 1, 2, 0}.  If the middle index was the one which was
!           for the duplicate values, it would be {\tt 1, 0, 2}.
!     \item [{[counts]}]
!           An integer array, with length ({\tt datarank} minus the igrid rank).
!           If the {\tt ESMF\_Array} is a higher rank than the
!           {\tt ESMF\_IGrid}, the additional dimensions may 
!           optionally each have an item count defined here.  
!           This allows {\tt ESMF\_FieldCreate()} to take 
!           an {\tt ESMF\_ArraySpec} and an {\tt ESMF\_ArrayDataMap}
!           and create the appropriately sized {\tt ESMF\_Array} for each DE.
!           These values are unneeded if the ranks of the data and igrid
!           are the same, and ignored if {\tt ESMF\_FieldCreate()} is called
!           called with an already-created {\tt ESMF\_Array}.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
 
        ! local vars
        integer :: status                     ! local error status
        logical :: rcpresent                  ! did user specify rc?
        integer :: i, dimlength

        ! initialize return code
        status = ESMF_RC_NOT_IMPL
        rcpresent = .FALSE.
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_RC_NOT_IMPL
        endif

    ! Check init status of arguments
    ESMF_INIT_CHECK_SHALLOW(ESMF_ArrayDataMapGetInit, ESMF_ArrayDataMapInit, arraydatamap)


        if (present(dataRank)) arraydatamap%dataRank = dataRank

        if (present(dataIndexList)) then
           dimlength = size(dataIndexList,1)
           if (dimlength .lt. arraydatamap%dataRank) then
              if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                "dataIndexList array too short for dataRank", &
                                 ESMF_CONTEXT, rc)) return
           endif

           do i=1, dimlength
             arraydatamap%dataDimOrder(i) = dataIndexList(i)
           enddo
        endif

        if (present(counts)) then
           arraydatamap%dataNonIGridCounts(:) = 1
           dimlength = size(counts)
           do i=1, dimlength
             arraydatamap%dataNonIGridCounts(i) = counts(i)
           enddo
        endif

        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_ArrayDataMapSet



!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayDataMapSetDefExplicit"
!BOP
! !IROUTINE:  ESMF_ArrayDataMapSetDefault - Set ArrayDataMap default values

! !INTERFACE:
      ! Private name; call using ESMF_ArrayDataMapSetDefault()
      subroutine ESMF_ArrayDataMapSetDefExplicit(arraydatamap, dataRank, &
                                                 dataIndexList, counts, rc)
!
! !ARGUMENTS:
      type(ESMF_InternArrayDataMap) :: arraydatamap
      integer, intent(in) :: dataRank
      integer, dimension(:), intent(in), optional :: dataIndexList
      integer, dimension(:), intent(in), optional :: counts
      integer, intent(out), optional :: rc  
!
! !DESCRIPTION:
!     Set default values of an {\tt ESMF\_ArrayDataMap}.  This differs
!     from {\tt ESMF\_ArrayDataMapSet()} in that all values which are
!     not specified here will be overwritten with default values.
!
!     \begin{description}
!     \item [arraydatamap]
!           An {\tt ESMF\_ArrayDataMap}.
!     \item [datarank]
!	    The number of dimensions in the data {\tt ESMF\_Array}.
!     \item [{[dataIndexList]}] 
!           An integer array, {\tt datarank} long, which specifies
!           the mapping between rank numbers in the {\tt ESMF\_IGrid}
!           and the {\tt ESMF\_Array}.  If there is no correspondance
!           (because the {\tt ESMF\_Array} has a higher rank than the
!           {\tt ESMF\_IGrid}) the index value must be 0.  The default is
!           a 1-to-1 mapping with the {\tt ESMF\_IGrid}.
!
!           For example, if the data array is 3D and the igrid is 2D,
!           and the first and second data indices correspond to the igrid
!           and the last data index is to store duplicate values for the
!           same igrid location, then the value for this input would be
!           {\tt 1, 2, 0}.  If the middle index was the one which was
!           for the duplicate values, it would be {\tt 1, 0, 2}.
!           The default values for this are {\tt 1, 0, ...} for a 1D igrid
!           and {\tt 1, 2, 0, ... } for a 2D igrid.
!     \item [{[counts]}]
!           An integer array, with length ({\tt datarank} minus the igrid rank).
!           If the {\tt ESMF\_Array} is a higher rank than the
!           {\tt ESMF\_IGrid}, the additional dimensions may 
!           optionally each have an item count defined here.  
!           This allows {\tt ESMF\_FieldCreate()} to take 
!           an {\tt ESMF\_ArraySpec} and an {\tt ESMF\_ArrayDataMap}
!           and create the appropriately sized {\tt ESMF\_Array} for each DE.
!           These values are unneeded if the ranks of the data and igrid
!           are the same, and ignored if {\tt ESMF\_FieldCreate()} is called
!           called with an already-created {\tt ESMF\_Array}.  If unspecified,
!           the default lengths are 1.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI

        ! local vars
        integer :: status                     ! local error status
        logical :: rcpresent                  ! did user specify rc?
        integer :: i, dimlength

        ! init return code
        status = ESMF_RC_NOT_IMPL
        if (present(rc)) then
            rcpresent=.TRUE.
            rc = ESMF_RC_NOT_IMPL
        else
          rcpresent = .FALSE.
        endif

    ! Check init status of arguments
    ESMF_INIT_CHECK_SHALLOW(ESMF_ArrayDataMapGetInit, ESMF_ArrayDataMapInit, arraydatamap)

        ! initialize the contents of the arraydatamap
        arraydatamap%dataRank = dataRank

        ! set the defaults
        arraydatamap%dataDimOrder(:) = 0
        arraydatamap%dataDimOrder(1:dataRank) = (/ (i,i=1,dataRank) /)

        ! now overwrite with what the user passed in
        if (present(dataIndexList)) then
           dimlength = size(dataIndexList,1)
           if (dimlength .lt. arraydatamap%dataRank) then
              if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                "dataIndexList array too short for dataRank", &
                                 ESMF_CONTEXT, rc)) return
           endif

           do i=1, dimlength
             arraydatamap%dataDimOrder(i) = dataIndexList(i)
           enddo
        endif


        ! counts for dimensions not aligned with the igrid
        arraydatamap%dataNonIGridCounts(:) = 1
        if (present(counts)) then
          arraydatamap%dataNonIGridCounts(1:size(counts)) = counts(:)
        endif

        ! mark object as initialized and ready to be used
        arraydatamap%status = ESMF_STATUS_READY

        ! if user asked for it, return error code
        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_ArrayDataMapSetDefExplicit


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayDataMapSetDefIndex"
!BOP
! !IROUTINE:  ESMF_ArrayDataMapSetDefault - Set ArrayDataMap default values

! !INTERFACE:
      ! Private name; call using ESMF_ArrayDataMapSetDefault()
      subroutine ESMF_ArrayDataMapSetDefIndex(arraydatamap, indexorder, counts, rc)
!
! !ARGUMENTS:
      type(ESMF_InternArrayDataMap) :: arraydatamap
      type(ESMF_IndexOrder), intent(in) :: indexorder
      integer, dimension(:), intent(in), optional :: counts 
      integer, intent(out), optional :: rc  
!
! !DESCRIPTION:
!     Set default values of an {\tt ESMF\_ArrayDataMap}.  This differs
!     from {\tt ESMF\_ArrayDataMapSet()} in that all values which are
!     not specified here will be overwritten with default values.
!
!     \begin{description}
!     \item [arraydatamap]
!           An {\tt ESMF\_ArrayDataMap}.
!     \item [indexorder]
!           An {\tt ESMF\_DataIndexOrder} which specifies one of several
!           common predefined mappings between the igrid and data ranks.
!           This is simply a convenience for the common cases; there is
!           a more general form of this call which allows the mapping to
!           be specified as an integer array of index numbers directly.
!     \item [{[counts]}]
!           An integer array, with length ({\tt datarank} minus the igrid rank).
!           If the {\tt ESMF\_Array} is a higher rank than the
!           {\tt ESMF\_IGrid}, the additional dimensions may 
!           optionally each have an item count defined here.  
!           This allows {\tt ESMF\_FieldCreate()} to take 
!           an {\tt ESMF\_ArraySpec} and an {\tt ESMF\_ArrayDataMap}
!           and create the appropriately sized {\tt ESMF\_Array} for each DE.
!           These values are unneeded if the ranks of the data and igrid
!           are the same, and ignored if {\tt ESMF\_FieldCreate()} is called
!           called with an already-created {\tt ESMF\_Array}.  If unspecified,
!           the default lengths are 1.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP

        ! local vars
        integer :: status                     ! local error status
        logical :: rcpresent                  ! did user specify rc?

        ! init return code
        status = ESMF_RC_NOT_IMPL
        if (present(rc)) then
            rcpresent=.TRUE.
            rc = ESMF_RC_NOT_IMPL
        else
          rcpresent = .FALSE.
        endif

    ! Check init status of arguments
    ESMF_INIT_CHECK_SHALLOW(ESMF_ArrayDataMapGetInit, ESMF_ArrayDataMapInit, arraydatamap)

        ! initialize the contents of the arraydatamap

        ! set up the mapping of igrid indicies to array indicies
        arraydatamap%dataDimOrder(:) = 0

        select case (indexorder%iorder)
          case(ESMF_INDEX_I%iorder) 
            arraydatamap%dataRank = 1
            arraydatamap%dataDimOrder(1) = 1

          case(ESMF_INDEX_IJ%iorder)
            arraydatamap%dataRank=2
            arraydatamap%dataDimOrder(1) = 1
            arraydatamap%dataDimOrder(2) = 2

          case(ESMF_INDEX_JI%iorder) 
            arraydatamap%dataRank=2
            arraydatamap%dataDimOrder(1) = 2
            arraydatamap%dataDimOrder(2) = 1

          case(ESMF_INDEX_IJK%iorder)
            arraydatamap%dataRank=3
            arraydatamap%dataDimOrder(1) = 1
            arraydatamap%dataDimOrder(2) = 2
            arraydatamap%dataDimOrder(3) = 3

          case(ESMF_INDEX_JIK%iorder)
            arraydatamap%dataRank=3
            arraydatamap%dataDimOrder(1) = 2
            arraydatamap%dataDimOrder(2) = 1
            arraydatamap%dataDimOrder(3) = 3

          case(ESMF_INDEX_KJI%iorder)
            arraydatamap%dataRank=3
            arraydatamap%dataDimOrder(1) = 3
            arraydatamap%dataDimOrder(2) = 2
            arraydatamap%dataDimOrder(3) = 1

          case(ESMF_INDEX_IKJ%iorder)
            arraydatamap%dataRank=3
            arraydatamap%dataDimOrder(1) = 1
            arraydatamap%dataDimOrder(2) = 3
            arraydatamap%dataDimOrder(3) = 2

          case(ESMF_INDEX_JKI%iorder)
            arraydatamap%dataRank=3
            arraydatamap%dataDimOrder(1) = 2
            arraydatamap%dataDimOrder(2) = 3
            arraydatamap%dataDimOrder(3) = 1

          case(ESMF_INDEX_KIJ%iorder)
            arraydatamap%dataRank=3
            arraydatamap%dataDimOrder(1) = 3
            arraydatamap%dataDimOrder(2) = 1
            arraydatamap%dataDimOrder(3) = 2

          case default 
            if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                 "unrecognized igrid index order", &
                                 ESMF_CONTEXT, rc)) return
        end select

        arraydatamap%dataNonIGridCounts(:) = 1
        if (present(counts)) then
          arraydatamap%dataNonIGridCounts(1:size(counts)) = counts(:)
        endif

        ! mark object as initialized and ready to be used
        arraydatamap%status = ESMF_STATUS_READY

        ! if user asked for it, return error code
        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_ArrayDataMapSetDefIndex


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayDataMapSetInvalid"
!BOP
! !IROUTINE:  ESMF_ArrayDataMapSetInvalid - Set ArrayDataMap to invalid status

! !INTERFACE:
      subroutine ESMF_ArrayDataMapSetInvalid(arraydatamap, rc)
!
! !ARGUMENTS:
      type(ESMF_InternArrayDataMap), intent(inout) :: arraydatamap
      integer, intent(out), optional :: rc  
!
! !DESCRIPTION:
!     Set the contents of an {\tt ESMF\_ArrayDataMap}
!     to an uninitialized value.
!
!     The arguments are:
!     \begin{description}
!     \item [arraydatamap]
!           An {\tt ESMF\_ArrayDataMap}.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP

        ! Initialize return code; assume routine not implemented
        if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_SHALLOW(ESMF_ArrayDataMapGetInit, ESMF_ArrayDataMapInit, arraydatamap)

        arraydatamap%status = ESMF_STATUS_INVALID

        ! If user asked for it, return error code
        if (present(rc)) rc = ESMF_SUCCESS

        end subroutine ESMF_ArrayDataMapSetInvalid


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayDataMapValidate"
!BOP
! !IROUTINE: ESMF_ArrayDataMapValidate - Check validity of an ArrayDataMap
!
! !INTERFACE:
      subroutine ESMF_ArrayDataMapValidate(arraydatamap, options, rc)
!
! !ARGUMENTS:
      type(ESMF_InternArrayDataMap), intent(inout) :: arraydatamap
      character (len = *), intent(in), optional :: options
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!      Validates that the {\tt arraydatamap} is internally consistent.
!      Currently this method determines if the {\tt arraydatamap}  
!      is set up for use.  The method returns an error code if problems 
!      are found.  

!
!     The arguments are:
!     \begin{description}
!     \item [arraydatamap]
!           {\tt ESMF\_ArrayDataMap} to validate.
!     \item [{[options]}]
!           Validation options are not yet supported.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!       \end{description}
!
!EOP

        ! initialize return code
        if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_SHALLOW(ESMF_ArrayDataMapGetInit, ESMF_ArrayDataMapInit, arraydatamap)

        if (arraydatamap%status .ne. ESMF_STATUS_READY) return
            
        ! TODO: add more validation here - for index numbers, etc
 
        if (present(rc)) rc = ESMF_SUCCESS

        end subroutine ESMF_ArrayDataMapValidate

! -------------------------- ESMF-internal method -----------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayDataMapInit()"
!BOPI
! !IROUTINE: ESMF_ArrayDataMapInit - Init ArrayDataMap internals

! !INTERFACE:
  subroutine ESMF_ArrayDataMapInit(datamap)
!
! !ARGUMENTS:
    type(ESMF_InternArrayDataMap), intent(inout)  :: datamap
!         
!
! !DESCRIPTION:
!      Initialize ArrayDataMap internals.
!
!     The arguments are:
!     \begin{description}
!     \item[datamap] 
!          Specified {\tt ESMF\_InternArrayDataMap} object.
!     \end{description}
!
!EOPI
! !REQUIREMENTS:  SSSn.n, GGGn.n
!------------------------------------------------------------------------------
    datamap%status = ESMF_STATUS_UNINIT
    ESMF_INIT_SET_DEFINED(datamap)
  end subroutine ESMF_ArrayDataMapInit
!------------------------------------------------------------------------------


! -------------------------- ESMF-internal method -----------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayDataMapGetInit"
!BOPI
! !IROUTINE: ESMF_ArrayDataMapGetInit - Internal access routine for init code
!
! !INTERFACE:
  function ESMF_ArrayDataMapGetInit(datamap) 
!
! !RETURN VALUE:
      ESMF_INIT_TYPE :: ESMF_ArrayDataMapGetInit   
!
! !ARGUMENTS:
      type(ESMF_InternArrayDataMap), intent(in), optional :: datamap
!
! !DESCRIPTION:
!      Access deep object init code.
!
!     The arguments are:
!     \begin{description}
!     \item [arrayspec]
!           InternArrayDataMap object.
!     \end{description}
!
!EOPI

    if (present(datamap)) then
      ESMF_ArrayDataMapGetInit = ESMF_INIT_GET(datamap)
    else
      ESMF_ArrayDataMapGetInit = ESMF_INIT_DEFINED
    endif

  end function ESMF_ArrayDataMapGetInit
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_RelLocString"
!BOPI
! !IROUTINE:  ESMF_RelLocString - Return a relloc as a string
!
! !INTERFACE:
      subroutine ESMF_RelLocString(relloc, string, rc)
!
!
! !ARGUMENTS:
      type(ESMF_RelLoc), intent(in) :: relloc
      character (len = *), intent(out) :: string
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Return an {\tt ESMF\_RelLoc} as a printable string.
!
!     The arguments are:
!     \begin{description}
!     \item [relloc]
!           The {\tt ESMF\_RelLoc} to be turned into a string.
!     \item [string]
!          Return string.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

        ! Initialize return code; assume routine not implemented
        if (present(rc)) rc = ESMF_RC_NOT_IMPL

        if (relloc .eq. ESMF_CELL_UNDEFINED) string = "Undefined"
        if (relloc .eq. ESMF_CELL_CENTER)    string = "Cell Center"
        if (relloc .eq. ESMF_CELL_NFACE)     string = "Cell North Face"
        if (relloc .eq. ESMF_CELL_SFACE)     string = "Cell South Face"
        if (relloc .eq. ESMF_CELL_EFACE)     string = "Cell East Face"
        if (relloc .eq. ESMF_CELL_WFACE)     string = "Cell West Face"
        if (relloc .eq. ESMF_CELL_NECORNER)  string = "Cell NorthEast Corner"
        if (relloc .eq. ESMF_CELL_NWCORNER)  string = "Cell NorthWest Corner"
        if (relloc .eq. ESMF_CELL_SECORNER)  string = "Cell SouthEast Corner"
        if (relloc .eq. ESMF_CELL_SWCORNER)  string = "Cell SouthWest Corner"
        if (relloc .eq. ESMF_CELL_CELL)      string = "Full Cell"
        if (relloc .eq. ESMF_CELL_VERTEX)    string = "Cell Vertex"

        if (present(rc)) rc = ESMF_SUCCESS

        end subroutine ESMF_RelLocString

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_InterleaveFlagString"
!BOPI
! !IROUTINE:  ESMF_InterleaveFlagString - Return a interleave as a string
!
! !INTERFACE:
      subroutine ESMF_InterleaveFlagString(interleave, string, rc)
!
! !ARGUMENTS:
      type(ESMF_InterleaveFlag), intent(in) :: interleave
      character (len = *), intent(out) :: string
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Return an {\tt ESMF\_Interleave} as a printable string.
!
!     The arguments are:
!     \begin{description}
!     \item [interleave]
!           The {\tt ESMF\_Interleave} to be turned into a string.
!     \item [string]
!          Return string.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!       \end{description}
!
!
!EOPI

        ! Initialize return code; assume routine not implemented
        if (present(rc)) rc = ESMF_RC_NOT_IMPL

        if (interleave .eq. ESMF_INTERLEAVE_BY_BLOCK) string = "Block Interleave"
        if (interleave .eq. ESMF_INTERLEAVE_BY_ITEM) string = "Item Interleave"

        if (present(rc)) rc = ESMF_SUCCESS

        end subroutine ESMF_InterleaveFlagString

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_InterleaveTypeString"
!BOPI
! !IROUTINE:  ESMF_InterleaveTypeString - Return an interleave type as a string
!
! !INTERFACE:
      subroutine ESMF_InterleaveTypeString(interleaveType, string, rc)
!
! !ARGUMENTS:
      type(ESMF_InterleaveType), intent(in) :: interleaveType
      character (len = *), intent(out) :: string
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Return an {\tt ESMF\_InterleaveType} as a printable string.
!
!     The arguments are:
!     \begin{description}
!     \item [interleaveType]
!           The {\tt ESMF\_InterleaveType} to be turned into a string.
!     \item [string]
!          Return string.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!       \end{description}
!
!
!EOPI
        ! Initialize return code; assume routine not implemented
        if (present(rc)) rc = ESMF_RC_NOT_IMPL

        if (interleaveType%il_type .eq. ESMF_INTERLEAVE_BY_BLOCK) string = "Block Interleave"
        if (interleaveType%il_type .eq. ESMF_INTERLEAVE_BY_ITEM) string = "Item Interleave"

        if (present(rc)) rc = ESMF_SUCCESS

        end subroutine ESMF_InterleaveTypeString

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_IndexOrderString"
!BOPI
! !IROUTINE:  ESMF_IndexOrderString - Return a indexorder as a string
!
! !INTERFACE:
      subroutine ESMF_IndexOrderString(indexorder, string, rc)
!
!
! !ARGUMENTS:
      type(ESMF_IndexOrder), intent(in) :: indexorder   ! turn into string
      character (len = *), intent(out) :: string        ! where to return it
      integer, intent(out), optional :: rc              ! return code
!
! !DESCRIPTION:
!     Return an {\tt ESMF\_IndexOrder} as a printable string.
!
!     The arguments are:
!     \begin{description}
!     \item [indexorder]
!           The {\tt ESMF\_IndexOrder} to be turned into a string.
!     \item [string]
!           The corresponding string value.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI

        ! Initialize return code; assume routine not implemented
        if (present(rc)) rc = ESMF_RC_NOT_IMPL

        if (indexorder.eq.ESMF_INDEX_I  ) string = "I"
        if (indexorder.eq.ESMF_INDEX_IJ ) string = "IJ"
        if (indexorder.eq.ESMF_INDEX_JI ) string = "JI"
        if (indexorder.eq.ESMF_INDEX_IJK) string = "IJK"
        if (indexorder.eq.ESMF_INDEX_JIK) string = "JIK"
        if (indexorder.eq.ESMF_INDEX_KJI) string = "KJI"
        if (indexorder.eq.ESMF_INDEX_IKJ) string = "IKJ"
        if (indexorder.eq.ESMF_INDEX_JKI) string = "JKI"
        if (indexorder.eq.ESMF_INDEX_KIJ) string = "KIJ"

        if (present(rc)) rc = ESMF_SUCCESS

        end subroutine ESMF_IndexOrderString


!------------------------------------------------------------------------------


end module ESMF_InternArrayDataMapMod

