! $Id: ESMF_FieldDataMap.F90,v 1.27.4.3 2007/10/18 02:42:47 cdeluca Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2007, University Corporation for Atmospheric Research, 
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
! Laboratory, University of Michigan, National Centers for Environmental 
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!------------------------------------------------------------------------------
#define ESMF_FILENAME "ESMF_FieldDataMap.F90"
!
! ESMF FieldDataMap module
!
!------------------------------------------------------------------------------
!
! !PURPOSE:
!
! The code in this file implements the FieldDataMap derived type
!  and functions which operate on FieldDataMaps.  
!  This type is implemented in Fortran 90 and a corresponding
!  C++ interface is provided for access.
!
! FieldDataMaps are used to indicate where data 
!   values are located relative to an individual cell/element, and
!   store interleave information for vector and higher order data items.
!   They also contain an ArrayDataMap which contains the data to grid
!   rank mappings.
!
!------------------------------------------------------------------------------
!
#include "ESMF.h"

! module definition

      module ESMF_FieldDataMapMod

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!
! This section contains the basic derived type for encapsulating the
!  various mappings between grids, arrays, and interleaved arrays, including
!  the internal subroutines and functions which operate on them.
!
!

!------------------------------------------------------------------------------
!BOPI
! !MODULE: ESMF_FieldDataMapMod
      
! !USES:
      use ESMF_UtilTypesMod    ! ESMF base class
      use ESMF_BaseMod
      use ESMF_LogErrMod
      use ESMF_IOSpecMod
      use ESMF_ArrayDataMapMod

! !PUBLIC TYPES:
      implicit none
      private

!------------------------------------------------------------------------------
!  ! ESMF_FieldDataMap
!  ! The data map type, which should fully describe the mapping
!  ! between index orders in the grid and the memory layout of
!  ! the data array, plus other metadata info such as where the
!  ! data is relative to the grid, and any interleaving info needed.

      type ESMF_FieldDataMap
      sequence
      private
#ifndef ESMF_NO_INITIALIZERS
        type(ESMF_Status) :: status = ESMF_STATUS_UNINIT
#else
        type(ESMF_Status) :: status 
#endif
        type(ESMF_ArrayDataMap) :: adm

        ! plus the additional information we need about vector ordering
        ! on individual data items and the relative locations of the data
        ! in each grid cell.
        type(ESMF_Logical) :: isScalar                  ! scalar values
        integer, dimension(ESMF_MAXDIM) :: rankLength   ! len if > scalar
        type(ESMF_InterleaveType) :: interleave         ! if > scalar
        type(ESMF_RelLoc) :: horzRelloc                 ! data item loc/cell
        type(ESMF_RelLoc) :: vertRelloc                 ! data item loc/cell

      end type


!------------------------------------------------------------------------------
! !PUBLIC MEMBER TYPES:
!
      public ESMF_FieldDataMap


! !PUBLIC MEMBER FUNCTIONS:
!
      public ESMF_FieldDataMapSetDefault
      public ESMF_FieldDataMapSetInvalid

      public ESMF_FieldDataMapGet, ESMF_FieldDataMapSet

      public ESMF_FieldDataMapWriteRestart, ESMF_FieldDataMapReadRestart
      public ESMF_FieldDataMapWrite, ESMF_FieldDataMapRead 
      public ESMF_FieldDataMapValidate, ESMF_FieldDataMapPrint
      public ESMF_FieldDataMapSerialize, ESMF_FieldDataMapDeserialize

!EOPI
 
!
!------------------------------------------------------------------------------
! leave the following line as-is; it will insert the cvs ident string
! into the object file for tracking purposes.
     character(*), parameter, private :: version =  &
         '$Id: ESMF_FieldDataMap.F90,v 1.27.4.3 2007/10/18 02:42:47 cdeluca Exp $'
!------------------------------------------------------------------------------



!------------------------------------------------------------------------------
!
! interface blocks here
!
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_FieldDataMapSetDefault - Initialize a FieldDataMap type

! !INTERFACE:
      interface ESMF_FieldDataMapSetDefault

! !PRIVATE MEMBER FUNCTIONS:
       module procedure ESMF_FieldDataMapSetDefIndex
       module procedure ESMF_FieldDataMapSetDefExplicit

! !DESCRIPTION:
! This interface provides a single entry point for {\tt ESMF\_FieldDataMap}
!  initialization methods.
!EOPI

      end interface 
                                      

!------------------------------------------------------------------------------
! overload .eq. & .ne. with additional derived types so you can compare
!  them as if they were simple integers.

!------------------------------------------------------------------------------
! end of declarations & definitions
!------------------------------------------------------------------------------

      contains

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldDataMapGet"
!BOP
! !IROUTINE: ESMF_FieldDataMapGet - Get values from a FieldDataMap 
!
! !INTERFACE:
      subroutine ESMF_FieldDataMapGet(fielddatamap, dataRank, dataIndexList, counts, &
                                      horzRelloc, vertRelloc, rc)
!
! !ARGUMENTS:
      type(ESMF_FieldDataMap), intent(in) :: fielddatamap  
      integer, intent(out), optional :: dataRank    
      integer, dimension(:), intent(out), optional :: dataIndexList
      integer, dimension(:), intent(out), optional :: counts 
      type(ESMF_RelLoc), intent(out), optional :: horzRelloc 
      type(ESMF_RelLoc), intent(out), optional :: vertRelloc 
      integer, intent(out), optional :: rc       
!
! !DESCRIPTION:
!   Return information about this {\tt ESMF\_FieldDataMap}.
!
!  The arguments are:
!  \begin{description}
!  \item [fielddatamap]
!     An {\tt ESMF\_FieldDataMap}.
!  \item [{[datarank]}]
!     The number of dimensions in the data {\tt ESMF\_Array}.
!  \item [{[dataIndexList]}]
!      An integer array, {\tt datarank} long, which specifies
!      the mapping between rank numbers in the {\tt ESMF\_Grid}
!      and the {\tt ESMF\_Array}.  If there is no correspondance
!      (because the {\tt ESMF\_Array} has a higher rank than the
!      {\tt ESMF\_Grid}) the index value will be 0.
!  \item [{[counts]}]
!      An integer array, with length ({\tt datarank} minus the grid rank).
!      Each entry is the default item count which would be used
!      for those ranks which do not correspond to grid ranks when
!      creating an {\tt ESMF\_Field} using only an
!      an {\tt ESMF\_ArraySpec} and an {\tt ESMF\_ArrayDataMap}.
!  \item [{[horzRelloc]}]
!        Relative location of data per grid cell/vertex in the horzontal
!        grid.
!  \item [{[vertRelloc]}]
!        Relative location of data per grid cell/vertex in the vertical grid.
!  \item [{[rc]}]
!        Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!  \end{description}
!
!
!EOP
 
        ! local vars
        integer :: status                     ! local error status
        logical :: rcpresent                  ! did user specify rc?

        ! initialize return code
        status = ESMF_FAILURE
        rcpresent = .FALSE.
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
        endif


        ! get any values from the internal array fielddatamap
        call ESMF_ArrayDataMapGet(fielddatamap%adm, dataRank, dataIndexList, &
                                  counts, status)
        if (status .ne. ESMF_SUCCESS) return

        ! TODO: need to add rankLength and vector information here

        if (present(horzRelloc)) horzRelloc = fielddatamap%horzRelloc
        if (present(vertRelloc)) vertRelloc = fielddatamap%vertRelloc

        if (present(rc)) rc = ESMF_SUCCESS

        end subroutine ESMF_FieldDataMapGet

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldDataMapPrint"
!BOP
! !IROUTINE: ESMF_FieldDataMapPrint - Print a FieldDataMap 
!
!
! !INTERFACE:
      subroutine ESMF_FieldDataMapPrint(fielddatamap, options, rc)
!
!
! !ARGUMENTS:
      type(ESMF_FieldDataMap), intent(in) :: fielddatamap
      character (len = *), intent(in), optional :: options
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!     Prints information about the {\tt fielddatamap} to {\tt stdout}.
!
!     The arguments are:
!     \begin{description}
!     \item [fielddatamap]
!           {\tt ESMF\_FieldDataMap} to print.
!     \item [{[options]}]
!           Print options are not yet supported.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP

        character (len = ESMF_MAXSTR) :: str
        !character (len = ESMF_MAXSTR) :: msgbuf

      !jw  write (msgbuf, *)  "FieldDataMap print:"
      !jw  call ESMF_LogWrite(msgbuf, ESMF_LOG_INFO)
        write (*, *)  "FieldDataMap print:"
        if (fielddatamap%status .ne. ESMF_STATUS_READY) then
      !jw    write (msgbuf, *)  "Uninitialized or Invalid object"
      !jw    call ESMF_LogWrite(msgbuf, ESMF_LOG_INFO)
          write (*, *)  "Uninitialized or Invalid object"
          return
        endif

        ! individual data item information
        call ESMF_ArrayDataMapPrint(fielddatamap%adm, options, rc)

        call ESMF_RelLocString(fielddatamap%horzRelloc, str, rc)
      !jw  write (msgbuf, *)  "Horizontal Relative location = ", trim(str)
      !jw  call ESMF_LogWrite(msgbuf, ESMF_LOG_INFO)
        write (*, *)  "Horizontal Relative location = ", trim(str)
        call ESMF_RelLocString(fielddatamap%vertRelloc, str, rc)
      !jw  write (msgbuf, *)  "Vertical Relative location = ", trim(str)
      !jw  call ESMF_LogWrite(msgbuf, ESMF_LOG_INFO)
        write (*, *)  "Vertical Relative location = ", trim(str)
        call ESMF_InterleaveTypeString(fielddatamap%interleave, str, rc)
        write (*, *)  "Interleave type = ", trim(str)
        ! TODO: need methods
      !nsc write (msgbuf, *)  "Interleave type = ", trim(str), &
        !         ".  Interleave Start,end,stride = ",  &
        !         fielddatamap%interleave%il_start, & 
        !         fielddatamap%interleave%il_end, & 
        !         fielddatamap%interleave%il_strides
      
        end subroutine ESMF_FieldDataMapPrint

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldDataMapSet"
!BOP
! !IROUTINE: ESMF_FieldDataMapSet - Set values in a FieldDataMap
!
! !INTERFACE:
      subroutine ESMF_FieldDataMapSet(fielddatamap, dataRank, dataIndexList, counts, &
                                      horzRelloc, vertRelloc, rc)
!
! !ARGUMENTS:
      type(ESMF_FieldDataMap), intent(inout) :: fielddatamap  
      integer, intent(in), optional :: dataRank    
      integer, dimension(:), intent(in), optional :: dataIndexList
      integer, dimension(:), intent(in), optional :: counts 
      type(ESMF_RelLoc), intent(in), optional :: horzRelloc 
      type(ESMF_RelLoc), intent(in), optional :: vertRelloc 
      integer, intent(out), optional :: rc       
!
! !DESCRIPTION:
! Set values in an {\tt ESMF\_FieldDataMap}.
!
! The arguments are:
!  \begin{description}
!  \item [fielddatamap]
!        An {\tt ESMF\_FieldDataMap}.
!     \item [{[datarank]}]
!           The number of dimensions in the data {\tt ESMF\_Array}.
!     \item [{[dataIndexList]}] 
!           An integer array, {\tt datarank} long, which specifies
!           the mapping between rank numbers in the {\tt ESMF\_Grid}
!           and the {\tt ESMF\_Array}.  If there is no correspondance
!           (because the {\tt ESMF\_Array} has a higher rank than the
!           {\tt ESMF\_Grid}) the index value must be 0.
!     \item [{[counts]}]
!           An integer array, with length ({\tt datarank} minus the grid rank).
!           If the {\tt ESMF\_Array} is a higher rank than the
!           {\tt ESMF\_Grid}, the additional dimensions may
!           optionally each have an item count defined here.
!           This allows {\tt ESMF\_FieldCreate()} to take 
!           an {\tt ESMF\_ArraySpec} and an {\tt ESMF\_ArrayDataMap}
!           and create the appropriately sized {\tt ESMF\_Array} for each DE.
!           These values are unneeded if the ranks of the data and grid
!           are the same, and ignored if {\tt ESMF\_FieldCreate()} is called
!           called with an already-created {\tt ESMF\_Array}.
!  \item [{[horzRelloc]}]
!        Relative location of data per grid cell/vertex in the horzontal grid.
!  \item [{[vertRelloc]}]
!        Relative location of data per grid cell/vertex in the vertical grid.
!  \item [{[rc]}]
!        Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!  \end{description}
!
!
!EOP
! !REQUIREMENTS: 
 
        ! local vars
        integer :: status                     ! local error status
        logical :: rcpresent                  ! did user specify rc?

        ! initialize return code
        status = ESMF_FAILURE
        rcpresent = .FALSE.
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
        endif


        ! set the internal array map
        call ESMF_ArrayDataMapSet(fielddatamap%adm, dataRank, dataIndexList, &
                                   counts, status)
        if (status .ne. ESMF_SUCCESS) return

        ! TODO: add the vector information here

        if (present(horzRelloc)) fielddatamap%horzRelloc = horzRelloc
        if (present(vertRelloc)) fielddatamap%vertRelloc = vertRelloc

        if (present(rc)) rc = ESMF_SUCCESS

        end subroutine ESMF_FieldDataMapSet


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldDataMapSetDefExplicit"
!BOP
! !IROUTINE: ESMF_FieldDataMapSetDefault - Set FieldDataMap default values
!
! !INTERFACE:
      ! Private name; call using ESMF_FieldDataMapSetDefault()
      subroutine ESMF_FieldDataMapSetDefExplicit(fielddatamap, dataRank, &
                                                 dataIndexList, counts, &
                                                 horzRelloc, vertRelloc, rc)
!
! !ARGUMENTS:
      type(ESMF_FieldDataMap) :: fielddatamap
      integer, intent(in) :: dataRank
      integer, dimension(:), intent(in), optional :: dataIndexList
      integer, dimension(:), intent(in), optional :: counts
      type(ESMF_RelLoc), intent(in), optional :: horzRelloc 
      type(ESMF_RelLoc), intent(in), optional :: vertRelloc 
      integer, intent(out), optional :: rc  
!
! !DESCRIPTION:
!     Set default values of an {\tt ESMF\_FieldDataMap}.  This differs
!     from {\tt ESMF\_FieldDataMapSet()} in that all values which are
!     not specified here will be overwritten with default values.
!            
!     \begin{description}
!     \item [fielddatamap]
!       An {\tt ESMF\_FieldDataMap}.
!     \item [datarank]
!           The number of dimensions in the data {\tt ESMF\_Array}.
!     \item [{[dataIndexList]}] 
!           An integer array, {\tt datarank} long, which specifies
!           the mapping between rank numbers in the {\tt ESMF\_Grid}
!           and the {\tt ESMF\_Array}.  If there is no correspondance
!           (because the {\tt ESMF\_Array} has a higher rank than the
!           {\tt ESMF\_Grid}) the index value must be 0.  The default is
!           a 1-to-1 mapping with the {\tt ESMF\_Grid}.
!     \item [{[counts]}]
!           An integer array, with length ({\tt datarank} minus the grid rank).
!           If the {\tt ESMF\_Array} is a higher rank than the
!           {\tt ESMF\_Grid}, the additional dimensions may
!           optionally each have an item count defined here.
!           This allows {\tt ESMF\_FieldCreate()} to take
!           an {\tt ESMF\_ArraySpec} and an {\tt ESMF\_ArrayDataMap}
!           and create the appropriately sized {\tt ESMF\_Array} for each DE.
!           These values are unneeded if the ranks of the data and grid
!           are the same, and ignored if {\tt ESMF\_FieldCreate()} is called
!           called with an already-created {\tt ESMF\_Array}.  If unspecified,
!           the default lengths are 1.
! \item [{[horzRelloc]}]
!       Relative location of data per grid cell/vertex in the horizontal grid.
!       The default is {\tt ESMF\_CELL\_CENTER}.
! \item [{[vertRelloc]}]
!       Relative location of data per grid cell/vertex in the vertical grid.
!       The default is {\tt ESMF\_CELL\_UNDEFINED}.
! \item [{[rc]}]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOP

        ! local vars
        integer :: status                     ! local error status
        logical :: rcpresent                  ! did user specify rc?

        ! init return code
        status = ESMF_FAILURE
        if (present(rc)) then
            rcpresent=.TRUE.
            rc = ESMF_FAILURE    
        else
          rcpresent = .FALSE.
        endif

        ! initialize the contents of the internal array fielddatamap
        call ESMF_ArrayDataMapSetDefault(fielddatamap%adm, dataRank, dataIndexList, &
                                         counts, status)
        if (status .ne. ESMF_SUCCESS) return

        ! assume scalar data and use the relloc the caller gave
        fielddatamap%rankLength = 0

        fielddatamap%horzRelloc = ESMF_CELL_CENTER
        if (present(horzRelloc)) fielddatamap%horzRelloc = horzRelloc

        fielddatamap%vertRelloc = ESMF_CELL_UNDEFINED
        if (present(vertRelloc)) fielddatamap%vertRelloc = vertRelloc

        ! mark object as initialized and ready to be used
        fielddatamap%status = ESMF_STATUS_READY

        ! if user asked for it, return error code
        if (present(rc)) rc = ESMF_SUCCESS

        end subroutine ESMF_FieldDataMapSetDefExplicit


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldDataMapSetDefIndex"
!BOP
! !IROUTINE:  ESMF_FieldDataMapSetDefault - Set FieldDataMap default values

! !INTERFACE:
      ! Private name; call using ESMF_FieldDataMapSetDefault()
      subroutine ESMF_FieldDataMapSetDefIndex(fielddatamap, indexorder, counts, &
                                              horzRelloc, vertRelloc, rc)
!
! !ARGUMENTS:
      type(ESMF_FieldDataMap) :: fielddatamap
      type(ESMF_IndexOrder), intent(in) :: indexorder
      integer, dimension(:), intent(in), optional :: counts 
      type(ESMF_RelLoc), intent(in), optional :: horzRelloc 
      type(ESMF_RelLoc), intent(in), optional :: vertRelloc 
      integer, intent(out), optional :: rc  
!
! !DESCRIPTION:
!     Set default values of an {\tt ESMF\_FieldDataMap}.  This differs
!     from {\tt ESMF\_FieldDataMapSet()} in that all values which are
!     not specified here will be overwritten with default values.
!       
!     \begin{description}
!     \item [fielddatamap]
!           An {\tt ESMF\_FieldDataMap}.
!     \item [indexorder]
!           An {\tt ESMF\_DataIndexOrder} which specifies one of several
!           common predefined mappings between the grid and data ranks.
!           This is simply a convenience for the common cases; there is
!           a more general form of this call which allows the mapping to
!           be specified as an integer array of index numbers directly.
!     \item [{[counts]}]
!           An integer array, with length ({\tt datarank} minus the grid rank).
!           If the {\tt ESMF\_Array} is a higher rank than the
!           {\tt ESMF\_Grid}, the additional dimensions may
!           optionally each have an item count defined here.
!           This allows {\tt ESMF\_FieldCreate()} to take
!           an {\tt ESMF\_ArraySpec} and an {\tt ESMF\_ArrayDataMap}
!           and create the appropriately sized {\tt ESMF\_Array} for each DE.
!           These values are unneeded if the ranks of the data and grid
!           are the same, and ignored if {\tt ESMF\_FieldCreate()} is called
!           called with an already-created {\tt ESMF\_Array}.  If unspecified,
!           the default lengths are 1.
!  \item [{[horzRelloc]}]
!        Relative location of data per grid cell/vertex in the horzontal
!        grid.  The default is {\tt ESMF\_CELL\_CENTER}.
!  \item [{[vertRelloc]}]
!        Relative location of data per grid cell/vertex in the vertical grid.
!        The default is {\tt ESMF\_CELL\_UNDEFINED}.
!  \end{description}
!
!EOP

        ! local vars
        integer :: status                     ! local error status
        logical :: rcpresent                  ! did user specify rc?

        ! init return code
        status = ESMF_FAILURE
        if (present(rc)) then
            rcpresent=.TRUE.
            rc = ESMF_FAILURE    
        else
            rcpresent = .FALSE.
        endif

        ! initialize the contents of the internal array fielddatamap
        call ESMF_ArrayDataMapSetDefault(fielddatamap%adm, indexorder, &
                                         counts, status)
        if (status .ne. ESMF_SUCCESS) return

        ! assume scalar data and use the relloc the caller gave
        fielddatamap%rankLength = 0

        if (present(horzRelloc)) then
          fielddatamap%horzRelloc = horzRelloc
        else
          fielddatamap%horzRelloc = ESMF_CELL_CENTER
        endif

        if (present(vertRelloc)) then
          fielddatamap%vertRelloc = vertRelloc
        else
          fielddatamap%vertRelloc = ESMF_CELL_UNDEFINED
        endif

        ! mark object as initialized and ready to be used
        fielddatamap%status = ESMF_STATUS_READY

        ! if user asked for it, return error code
        if (present(rc)) rc = ESMF_SUCCESS

        end subroutine ESMF_FieldDataMapSetDefIndex


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldDataMapSetInvalid"
!BOP
! !IROUTINE:  ESMF_FieldDataMapSetInvalid - Set FieldDataMap to an invalid status

! !INTERFACE:
      subroutine ESMF_FieldDataMapSetInvalid(fielddatamap, rc)
!
! !ARGUMENTS:
      type(ESMF_FieldDataMap), intent(inout) :: fielddatamap
      integer, intent(out), optional :: rc  
!
! !DESCRIPTION:
!     Set the contents of an {\tt ESMF\_FieldDataMap}
!     to an invalid status.
!
!     The arguments are:
!     \begin{description}
!     \item [fielddatamap]
!           An {\tt ESMF\_FieldDataMap}.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!       \end{description}
!
!
!EOP

        call ESMF_ArrayDataMapSetInvalid(fielddatamap%adm, rc)

        fielddatamap%status = ESMF_STATUS_INVALID

        ! If user asked for it, return error code
        if (present(rc)) rc = ESMF_SUCCESS

        end subroutine ESMF_FieldDataMapSetInvalid

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldDataMapValidate"
!BOP
! !IROUTINE: ESMF_FieldDataMapValidate - Check validity of a FieldDataMap
!
! !INTERFACE:
      subroutine ESMF_FieldDataMapValidate(fielddatamap, options, rc)
!
! !ARGUMENTS:
      type(ESMF_FieldDataMap), intent(in) :: fielddatamap
      character (len = *), intent(in), optional :: options
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!      Validates that the {\tt fielddatamap} is internally consistent.
!      Currently this method determines if the {\tt fielddatamap} is uninitialized 
!      or already destroyed.  The method returns an error code if problems 
!      are found.  
!
!     The arguments are:
!     \begin{description}
!     \item [fielddatamap]
!           {\tt ESMF\_FieldDataMap} to validate.
!     \item [{[options]}]
!           Validation options are not yet supported.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
        
        ! local vars
        integer :: status                     ! local error status
        logical :: rcpresent                  ! did user specify rc?

        ! initialize return code
        status = ESMF_FAILURE
        rcpresent = .FALSE.
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
        endif


        if (fielddatamap%status .ne. ESMF_STATUS_READY) then
          if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                "Uninitialized or already destroyed FieldDataMap", &
                                 ESMF_CONTEXT, rc)) return
        endif
 
        if (present(rc)) rc = ESMF_SUCCESS

        end subroutine ESMF_FieldDataMapValidate

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldDataMapWriteRestart"
!BOPI
! !INTERFACE:
      subroutine ESMF_FieldDataMapWriteRestart(fielddatamap, iospec, rc)
!
! !ARGUMENTS:
      type(ESMF_FieldDataMap), intent(in) :: fielddatamap
      type(ESMF_IOSpec), intent(in), optional :: iospec
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!      Used to save all data to disk as quickly as possible.  
!      (see Read/Write for other options).  Internally this routine uses the
!      same I/O interface as Read/Write, but the default options are to
!      select the fastest way to save data to disk.
!
!     The arguments are:
!     \begin{description}
!     \item [fielddatamap]
!           {\tt ESMF\_FieldDataMap} to save.
!     \item [{[iospec]}]
!           File specification.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!       \end{description}
!
!
!EOPI
! !REQUIREMENTS: FLD1.6.8

!	BOP/EOP changed to BOPI/EOPI until code is added.
!
! TODO: code goes here
!
        if (present(rc)) rc = ESMF_FAILURE

        end subroutine ESMF_FieldDataMapWriteRestart


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldDataMapReadRestart"
!BOPI
! !IROUTINE: ESMF_FieldDataMapReadRestart - Reinitialize a FieldDataMap type
!
! !INTERFACE:
      function ESMF_FieldDataMapReadRestart(name, iospec, rc)
!
! !RETURN VALUE:
      type(ESMF_FieldDataMap) :: ESMF_FieldDataMapReadRestart
!
!
! !ARGUMENTS:
      character (len = *), intent(in) :: name
      type(ESMF_IOSpec), intent(in), optional :: iospec
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!      Used to reinitialize
!      all data associated with a {\tt ESMF\_FieldDataMap} 
!      from the last call to WriteRestart.
!
!      The arguments are:
!     \begin{description}
!     \item [name]
!           Name of data to reinitialize.
!     \item [{[iospec]}]
!           File specification.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!       \end{description}
!
!EOPI
! !REQUIREMENTS: FLD1.6.8

!	Changed BOP/EOP to BOPI/EOPI until code is added.
!
! TODO: code goes here
!
 
        ESMF_FieldDataMapReadRestart%status = ESMF_STATUS_UNINIT
        if (present(rc)) rc = ESMF_FAILURE

        end function ESMF_FieldDataMapReadRestart


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldDataMapWrite"
!BOPI
! !IROUTINE: ESMF_FieldDataMapWrite - Store a FieldDataMap type
!
! !INTERFACE:
      subroutine ESMF_FieldDataMapWrite(fielddatamap, iospec, rc)
!
! !ARGUMENTS:
      type(ESMF_FieldDataMap), intent(in) :: fielddatamap
      type(ESMF_IOSpec), intent(in), optional :: iospec
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!      Used to write data to persistent storage in a variety of formats.  
!      (see WriteRestart/ReadRestart for quick data dumps.)  Details of I/O 
!      options specified in the IOSpec derived type. 
!
!      The arguments are:
!     \begin{description}
!     \item [fielddatamap]
!           {\tt ESMF\_FieldDataMap} to save.
!     \item [{[iospec]}]
!           File specification.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!       \end{description}
!
!
!
!EOPI
! !REQUIREMENTS: FLD3.1, FLD3.2, FLD3.3, FLD3.4, FLD3.5

!
!	Changed BOP/EOP to BOPI/EOPI until code is added.
! TODO: code goes here
!
        if (present(rc)) rc = ESMF_FAILURE

        end subroutine ESMF_FieldDataMapWrite


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldDataMapRead"
!BOPI
! !IROUTINE: ESMF_FieldDataMapRead - Read a stored FieldDataMap type
!
! !INTERFACE:
      function ESMF_FieldDataMapRead(name, iospec, rc)
!
! !RETURN VALUE:
      type(ESMF_FieldDataMap) :: ESMF_FieldDataMapRead
!
! !ARGUMENTS:
      character (len = *), intent(in) :: name
      type(ESMF_IOSpec), intent(in), optional :: iospec
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!      Used to read data from persistent storage in a variety of formats.
!
!      The arguments are:
!     \begin{description}
!     \item [name]
!           Name of data to read.
!     \item [{[iospec]}]
!           File specification.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!       \end{description}
!
!
!EOPI
! !REQUIREMENTS: (which req number is this?)

!
!	Changed BOP/EOP to BOPI/EOPI until code is added.
! TODO: code goes here
!
        ESMF_FieldDataMapRead%status = ESMF_STATUS_UNINIT
        if (present(rc)) rc = ESMF_SUCCESS

        end function ESMF_FieldDataMapRead


!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldDataMapSerialize"

!BOPI
! !IROUTINE: ESMF_FieldDataMapSerialize - Serialize fielddatamap info into a byte stream
!
! !INTERFACE:
      subroutine ESMF_FieldDataMapSerialize(fielddatamap, buffer, length, offset, rc) 
!
! !ARGUMENTS:
      type(ESMF_FieldDataMap), intent(in) :: fielddatamap 
      integer(ESMF_KIND_I4), pointer, dimension(:) :: buffer
      integer, intent(inout) :: length
      integer, intent(inout) :: offset
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!      Takes an {\tt ESMF\_FieldDataMap} object and adds all the information needed
!      to save the information to a file or recreate the object based on this
!      information.   Expected to be used by {\tt ESMF\_StateReconcile()} and
!      by {\tt ESMF\_FieldDataMapWrite()} and {\tt ESMF\_FieldDataMapRead()}.
!
!     The arguments are:
!     \begin{description}
!     \item [fielddatamap]
!           {\tt ESMF\_FieldDataMap} object to be serialized.
!     \item [buffer]
!           Data buffer which will hold the serialized information.
!     \item [length]
!           Current length of buffer, in bytes.  If the serialization
!           process needs more space it will allocate it and update
!           this length.
!     \item [offset]
!           Current write offset in the current buffer.  This will be
!           updated by this routine and return pointing to the next
!           available byte in the buffer.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI

      integer :: localrc                     ! Error status


      call c_ESMC_FieldDataMapSerialize(fielddatamap%status, &
                                 fielddatamap%isScalar, &
                                 fielddatamap%rankLength(1), &
                                 fielddatamap%interleave, &
                                 fielddatamap%horzRelloc, &
                                 fielddatamap%vertRelloc, &
                                 buffer(1), length, offset, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                 ESMF_ERR_PASSTHRU, &
                                 ESMF_CONTEXT, rc)) return

      call c_ESMC_ArrayDataMapSerialize(fielddatamap%adm%status, &
                                 fielddatamap%adm%dataRank, &
                                 fielddatamap%adm%dataDimOrder, &
                                 fielddatamap%adm%dataNonGridCounts, &
                                 buffer(1), length, offset, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                 ESMF_ERR_PASSTHRU, &
                                 ESMF_CONTEXT, rc)) return

      if  (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldDataMapSerialize

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldDataMapDeserialize"

!BOPI
! !IROUTINE: ESMF_FieldDataMapDeserialize - Deserialize a byte stream into a FieldDataMap
!
! !INTERFACE:
      subroutine ESMF_FieldDataMapDeserialize(fielddatamap, buffer, offset, rc) 
!
! !ARGUMENTS:
      type(ESMF_FieldDataMap), intent(in) :: fielddatamap 
      integer(ESMF_KIND_I4), pointer, dimension(:) :: buffer
      integer, intent(inout) :: offset
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!      Takes a byte-stream buffer and reads the information needed to
!      recreate a FieldDataMap object.  Recursively calls the deserialize routines
!      needed to recreate the subobjects.
!      Expected to be used by {\tt ESMF\_StateReconcile()} and
!      by {\tt ESMF\_FieldDataMapWrite()} and {\tt ESMF\_FieldDataMapRead()}.
!
!     The arguments are:
!     \begin{description}
!     \item [fielddatamap]
!           {\tt ESMF\_FieldDataMap} object to be deserialized.
!     \item [buffer]
!           Data buffer which holds the serialized information.
!     \item [offset]
!           Current read offset in the current buffer.  This will be
!           updated by this routine and return pointing to the next
!           unread byte in the buffer.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI

      integer :: localrc                ! Error status

      call c_ESMC_FieldDataMapDeserialize(fielddatamap%status, &
                                 fielddatamap%isScalar, &
                                 fielddatamap%rankLength(1), &
                                 fielddatamap%interleave, &
                                 fielddatamap%horzRelloc, &
                                 fielddatamap%vertRelloc, &
                                 buffer(1), offset, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                 ESMF_ERR_PASSTHRU, &
                                 ESMF_CONTEXT, rc)) return

      call c_ESMC_ArrayDataMapDeserialize(fielddatamap%adm%status, &
                                 fielddatamap%adm%dataRank, &
                                 fielddatamap%adm%dataDimOrder, &
                                 fielddatamap%adm%dataNonGridCounts, &
                                 buffer(1), offset, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                 ESMF_ERR_PASSTHRU, &
                                 ESMF_CONTEXT, rc)) return

      if  (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldDataMapDeserialize
!------------------------------------------------------------------------------


        end module ESMF_FieldDataMapMod


