! $Id: ESMF_ArrayDataMap.F90,v 1.1 2004/05/03 16:12:58 nscollins Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2003, University Corporation for Atmospheric Research, 
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
! Laboratory, University of Michigan, National Centers for Environmental 
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
! NASA Goddard Space Flight Center.
! Licensed under the GPL.
!
!------------------------------------------------------------------------------
!
! ESMF ArrayDataMap module
!
!------------------------------------------------------------------------------
!
! !PURPOSE:
!
! The code in this file implements the ArrayDataMap derived type
!  and functions which operate on ArrayDataMaps.  
!  This type is implemented in Fortran 90 and a corresponding
!  C++ interface is provided for access.
!
! ArrayDataMaps are used to store the mapping of the array index orders
!   compared to the grid specifications; to indicate where data 
!   values are located relative to an individual cell/element,
!   store interleave information for larger than scalar data,
!   field interleave information for packed bundles, and any
!   other information needed to relate the data array to the grid.  
!
!------------------------------------------------------------------------------
!
#include "ESMF.h"

! module definition

      module ESMF_ArrayDataMapMod

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
! !MODULE: ESMF_ArrayDataMapMod
      
! !USES:
      use ESMF_BaseMod
      use ESMF_IOSpecMod

! !PUBLIC TYPES:
      implicit none
      private

!------------------------------------------------------------------------------
!  ! Interleaved types are used when there are multiple variables or
!  ! if individual data items are > scalar

      type ESMF_InterleaveType
      sequence
      private
          integer :: il_type
      end type

      type(ESMF_InterleaveType), parameter ::  &
                    ESMF_IL_BLOCK = ESMF_InterleaveType(1), &
                    ESMF_IL_ITEM = ESMF_InterleaveType(2)

!------------------------------------------------------------------------------
!  ! Expected to be used most often for packed bundles, to allow access to
!  ! data for an individual field.  Can also be used to describe vector data
!  ! that is packed into a single array.

      type ESMF_Interleave
      sequence
      private
         type(ESMF_InterleaveType) :: il_type
         integer :: il_start
         integer :: il_end
         integer :: il_strides 
      end type

!------------------------------------------------------------------------------
!  ! This describes how the data items are located relative to an individual
!  ! cell or element.  Putting this here means that different Fields which
!  ! have different relative locations can still share the same Grid.
!  ! See the Grid object for a description of 'staggering' which is a 
!  ! per-Grid concept. 

      type ESMF_RelLoc
      sequence
      private
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
!  ! between index orders in the grid and the memory layout of
!  ! the data array, plus other metadata info such as where the
!  ! data is relative to the grid, and any interleaving info needed.

      type ESMF_ArrayDataMap
      sequence
      private
#ifndef ESMF_NO_INITIALIZERS
        type(ESMF_Status) :: status = ESMF_STATE_UNINIT
#else
        type(ESMF_Status) :: status 
#endif
        ! index orders - how the grid dims (X,Y,Z) or (u,v) or (i,j,k)
        !  map onto the array memory layout as declared.  
        !  set dim numbers to 1-N, use 0 for dims which are part of data
        !  items and do not map to the grid dims.
        !!integer :: gridRank                             ! grid rank
        !!integer, dimension(ESMF_MAXDIM) :: gridDimOrder ! 0 = not a grid dim
        !!integer, dimension(ESMF_MAXDIM) :: gridSense    ! +/- iteration order
        !!integer, dimension(ESMF_MAXDIM) :: gridDecomp   ! 0 = not decomposed
        ! individual data item information
        integer :: dataRank                             ! scalar, vector, etc.
        type(ESMF_Logical) :: isScalar                  ! scalar values
        integer, dimension(ESMF_MAXDIM) :: dataDimOrder ! 0 = not a data dim
        integer, dimension(ESMF_MAXDIM) :: dataNonGridCounts ! for non-grid dims
        integer, dimension(ESMF_MAXDIM) :: rankLength   ! len if > scalar
        type(ESMF_Interleave) :: interleave             ! if > scalar
        ! data location relative to an individual grid cell
        type(ESMF_RelLoc) :: horzRelloc                ! data item loc/cell
        type(ESMF_RelLoc) :: vertRelloc                 ! data item loc/cell
      end type


!------------------------------------------------------------------------------
! !PUBLIC MEMBER TYPES:
!
      public ESMF_ArrayDataMap

      public ESMF_InterleaveType   ! why?
      public ESMF_Interleave
      public ESMF_IL_BLOCK, ESMF_IL_ITEM

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
      public ESMF_ArrayDataMapInit
      public ESMF_ArrayDataMapSetInvalid

      public ESMF_ArrayDataMapGet, ESMF_ArrayDataMapSet

      public ESMF_ArrayDataMapWriteRestart, ESMF_ArrayDataMapReadRestart
      public ESMF_ArrayDataMapWrite, ESMF_ArrayDataMapRead 
      public ESMF_ArrayDataMapValidate, ESMF_ArrayDataMapPrint

      public ESMF_RelLocString, ESMF_InterleaveString, ESMF_IndexOrderString

      public operator(.eq.), operator(.ne.)

!EOPI
 
!
!------------------------------------------------------------------------------
! leave the following line as-is; it will insert the cvs ident string
! into the object file for tracking purposes.
      character(*), parameter, private :: version =  &
             '$Id: ESMF_ArrayDataMap.F90,v 1.1 2004/05/03 16:12:58 nscollins Exp $'
!------------------------------------------------------------------------------



!------------------------------------------------------------------------------
!
! interface blocks here
!
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_ArrayDataMapInit - Initialize a ArrayDataMap type

! !INTERFACE:
      interface ESMF_ArrayDataMapInit

! !PRIVATE MEMBER FUNCTIONS:
       module procedure ESMF_ArrayDataMapInitIndex
       module procedure ESMF_ArrayDataMapInitExplicit

! !DESCRIPTION:
! This interface provides a single entry point for {\tt ESMF\_ArrayDataMap}
!  initialization methods.
!EOP

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
! function to compare two ESMF_InterleaveType flags 

function ESMF_ileq(il1, il2)
 logical ESMF_ileq
 type(ESMF_InterleaveType), intent(in) :: il1, il2

 ESMF_ileq = (il1%il_type .eq. il2%il_type)
end function

function ESMF_ilne(il1, il2)
 logical ESMF_ilne
 type(ESMF_InterleaveType), intent(in) :: il1, il2

 ESMF_ilne = (il1%il_type .ne. il2%il_type)
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
!------------------------------------------------------------------------------
!
! This section includes the ArrayDataMap Init and Invalidate routines
!
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_ArrayDataMapInit - initialize the contents of a ArrayDataMap

! !INTERFACE:
      ! Private name: scCall using ESMF_ArrayDataMapInit()
      subroutine ESMF_ArrayDataMapInitIndex(datamap, dataIorder, counts, &
                                       horzRelloc, vertRelloc, rc)
!
! !ARGUMENTS:
      type(ESMF_ArrayDataMap) :: datamap
      type(ESMF_IndexOrder), intent(in) :: dataIorder
      integer, dimension(:), intent(in), optional :: counts 
      type(ESMF_RelLoc), intent(in), optional :: horzRelloc 
      type(ESMF_RelLoc), intent(in), optional :: vertRelloc 
      integer, intent(out), optional :: rc  
!
! !DESCRIPTION:
!     Initialize the contents of a {\tt ESMF\_ArrayDataMap} type.
!
!     The arguments are:
!     \begin{description} 
!     \item [datamap]
!           An {\tt ESMF\_ArrayDataMap} object.
!     \item [dataIorder] 
!           An {\tt ESMF\_IndexOrder} object which describes one of several
!           predefined Index Orders.  There is another version of the Init
!           call which allows a more general form of the indexing; this is
!           a convenience routine for the most common cases.
!     \item [{[counts]}]
!           If the {\tt ESMF\_Array} object is a higher rank than the
!           {\tt ESMF\_Grid}, the additional dimensions may each have an 
!           item count defined here.  This allows an {\tt ESMF\_FieldCreate()}
!           call to take an {\tt ESMF\_ArraySpec} and an {\tt ESMF\_ArrayDataMap}
!           and create the appropriately sized {\tt ESMF\_Array} for each
!           {\tt DE}.  If the {\tt ESMF\_Array} is created first, the counts
!           can be obtained from the {\tt ESMF\_Array} and this argument
!           is unneeded.  If the ranks of the grid and array are the same, 
!           this is also unneeded.
!     \item [{[horzRelloc]}]
!           Relative location of data per grid cell/vertex in the horzontal
!           grid.
!     \item [{[vertRelloc]}]
!           Relative location of data per grid cell/vertex in the vertical grid.
!	\end{description}
!
!EOP
! !REQUIREMENTS: internal

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

        ! initialize the contents of the datamap

        ! set up the mapping of grid indicies to array indicies
        datamap%dataDimOrder(:) = 0

        select case (dataIorder%iorder)
          case(ESMF_INDEX_I%iorder) 
            datamap%dataRank = 1
            datamap%dataDimOrder(1) = 1

          case(ESMF_INDEX_IJ%iorder)
            datamap%dataRank=2
            datamap%dataDimOrder(1) = 1
            datamap%dataDimOrder(2) = 2

          case(ESMF_INDEX_JI%iorder) 
            datamap%dataRank=2
            datamap%dataDimOrder(1) = 2
            datamap%dataDimOrder(2) = 1

          case(ESMF_INDEX_IJK%iorder)
            datamap%dataRank=3
            datamap%dataDimOrder(1) = 1
            datamap%dataDimOrder(2) = 2
            datamap%dataDimOrder(3) = 3

          case(ESMF_INDEX_JIK%iorder)
            datamap%dataRank=3
            datamap%dataDimOrder(1) = 2
            datamap%dataDimOrder(2) = 1
            datamap%dataDimOrder(3) = 3

          case(ESMF_INDEX_KJI%iorder)
            datamap%dataRank=3
            datamap%dataDimOrder(1) = 3
            datamap%dataDimOrder(2) = 2
            datamap%dataDimOrder(3) = 1

          case(ESMF_INDEX_IKJ%iorder)
            datamap%dataRank=3
            datamap%dataDimOrder(1) = 1
            datamap%dataDimOrder(2) = 3
            datamap%dataDimOrder(3) = 2

          case(ESMF_INDEX_JKI%iorder)
            datamap%dataRank=3
            datamap%dataDimOrder(1) = 2
            datamap%dataDimOrder(2) = 3
            datamap%dataDimOrder(3) = 1

          case(ESMF_INDEX_KIJ%iorder)
            datamap%dataRank=3
            datamap%dataDimOrder(1) = 3
            datamap%dataDimOrder(2) = 1
            datamap%dataDimOrder(3) = 2

          case default 
            print *, "ERROR: ESMF_ArrayDataMapInit - unrecognized grid index order"
            return
        end select

        ! assume scalar data and use the relloc the caller gave
        datamap%rankLength = 0

        datamap%dataNonGridCounts(:) = 1
        if (present(counts)) then
          datamap%dataNonGridCounts(1:size(counts)) = counts(:)
        endif

        if (present(horzRelloc)) then
          datamap%horzRelloc = horzRelloc
        else
          datamap%horzRelloc = ESMF_CELL_CENTER
        endif

        if (present(vertRelloc)) then
          datamap%vertRelloc = vertRelloc
        else
          datamap%vertRelloc = ESMF_CELL_UNDEFINED
        endif

        ! mark object as initialized and ready to be used
        datamap%status = ESMF_STATE_READY

        ! if user asked for it, return error code
        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_ArrayDataMapInitIndex


!------------------------------------------------------------------------------
!BOPI
! !INTERFACE:
      subroutine ESMF_ArrayDataMapInitExplicit(datamap, dataRank, dataIndices, &
                                          counts, horzRelloc, vertRelloc, rc)
!
! !ARGUMENTS:
      type(ESMF_ArrayDataMap) :: datamap
      integer, intent(in) :: dataRank
      integer, dimension(:), intent(in) :: dataIndices
      integer, dimension(:), intent(in), optional :: counts
      type(ESMF_RelLoc), intent(in), optional :: horzRelloc 
      type(ESMF_RelLoc), intent(in), optional :: vertRelloc 
      integer, intent(out), optional :: rc  
!
! !DESCRIPTION:
!      ESMF routine to initialize the contents of a {\tt ESMF\_ArrayDataMap} type.
!
!     The arguments are:
!     \begin{description}
!     \item [datamap]
!           An {\tt ESMF\_ArrayDataMap} object.
!     \item [dataRank] 
!	    The number of the array dimensions.
!     \item [dataIndices] 
!	    TODO: Add description           
!     \item [{[counts]}]
!           If the {\tt ESMF\_Array} object is a higher rank than the
!           {\tt ESMF\_Grid}, the additional dimensions may each have an
!           item count defined here.  This allows an {\tt ESMF\_FieldCreate()}
!           call to take an {\tt ESMF\_ArraySpec} and an {\tt ESMF\_ArrayDataMap}
!           and create the appropriately sized {\tt ESMF\_Array} for each
!           {\tt DE}.  If the {\tt ESMF\_Array} is created first, the counts
!           can be obtained from the {\tt ESMF\_Array} and this argument
!           is unneeded.  If the ranks of the grid and array are the same,
!           this is also unneeded.
!     \item [{[horzRelloc]}]
!           Relative location of data per grid cell/vertex in the horzontal
!           grid.
!     \item [{[vertRelloc]}]
!           Relative location of data per grid cell/vertex in the vertical grid.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!       \end{description}
!
!EOPI
! !REQUIREMENTS: internal

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

        ! initialize the contents of the datamap

        ! set the defaults
        datamap%dataDimOrder(:) = 0

        ! now overwrite with what the user passed in
        datamap%dataRank = dataRank
        datamap%dataDimOrder(1:size(dataIndices)) = dataIndices

        ! assume scalar data and use the relloc the caller gave
        datamap%rankLength = 0

        ! counts for dimensions not aligned with the grid
        datamap%dataNonGridCounts(:) = 1
        if (present(counts)) then
          datamap%dataNonGridCounts(1:size(counts)) = counts(:)
        endif

        datamap%horzRelloc = ESMF_CELL_CENTER
        if (present(horzRelloc)) datamap%horzRelloc = horzRelloc

        datamap%vertRelloc = ESMF_CELL_UNDEFINED
        if (present(vertRelloc)) datamap%vertRelloc = vertRelloc

        ! mark object as initialized and ready to be used
        datamap%status = ESMF_STATE_READY

        ! if user asked for it, return error code
        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_ArrayDataMapInitExplicit


!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_ArrayDataMapSetInvalid - set contents of a ArrayDataMap to uninitialized value.

! !INTERFACE:
      subroutine ESMF_ArrayDataMapSetInvalid(datamap, rc)
!
! !ARGUMENTS:
      type(ESMF_ArrayDataMap), intent(inout) :: datamap
      integer, intent(out), optional :: rc  
!
! !DESCRIPTION:
!      ESMF routine to set the contents of a {\tt ESMF\_ArrayDataMap} type
!      to an uninitialized value.
!
!     The arguments are:
!     \begin{description}
!     \item [datamap]
!           An {\tt ESMF\_ArrayDataMap} object.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!       \end{description}
!
!
!EOP
! !REQUIREMENTS: internal

        datamap%status = ESMF_STATE_INVALID

        ! If user asked for it, return error code
        if (present(rc)) rc = ESMF_SUCCESS

        end subroutine ESMF_ArrayDataMapSetInvalid


!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!
! This section contains the Get and Set routines
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_ArrayDataMapGet - Get object from a ArrayDataMap type.
!
! !INTERFACE:
      subroutine ESMF_ArrayDataMapGet(datamap, dataRank, dataIorder, counts, &
                                 horzRelloc, vertRelloc, rc)
!
! !ARGUMENTS:
      type(ESMF_ArrayDataMap), intent(in) :: datamap  
      integer, intent(out), optional :: dataRank    
      integer, dimension(:), intent(out), optional :: dataIorder
      integer, dimension(:), intent(out), optional :: counts 
      type(ESMF_RelLoc), intent(out), optional :: horzRelloc 
      type(ESMF_RelLoc), intent(out), optional :: vertRelloc 
      integer, intent(out), optional :: rc       
!
! !DESCRIPTION:
!   Return info about the current {\tt ESMF\_ArrayDataMap} described by this object.
!
!   The arguments are:
!     \begin{description}
!     \item [datamap]
!           An {\tt ESMF\_ArrayDataMap} object.
!     \item [{[datarank]}]
!	    The number of array dimensions.
!     \item [{[dataIorder]}] 
!           An {\tt ESMF\_IndexOrder} object which describes one of several
!           predefined Index Orders.  There is another version of the Init
!           call which allows a more general form of the indexing; this is
!           a convenience routine for the most common cases.
!     \item [{[counts]}]
!           If the {\tt ESMF\_Array} object is a higher rank than the
!           {\tt ESMF\_Grid}, the additional dimensions may each have an
!           item count defined here.  This allows an {\tt ESMF\_FieldCreate()}
!           call to take an {\tt ESMF\_ArraySpec} and an {\tt ESMF\_ArrayDataMap}
!           and create the appropriately sized {\tt ESMF\_Array} for each
!           {\tt DE}.  If the {\tt ESMF\_Array} is created first, the counts
!           can be obtained from the {\tt ESMF\_Array} and this argument
!           is unneeded.  If the ranks of the grid and array are the same,
!           this is also unneeded.
!     \item [{[horzRelloc]}]
!           Relative location of data per grid cell/vertex in the horzontal
!           grid.
!     \item [{[vertRelloc]}]
!           Relative location of data per grid cell/vertex in the vertical grid.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!       \end{description}
!
!
!EOP
! !REQUIREMENTS: 
 
        ! local vars
        integer :: status                     ! local error status
        logical :: rcpresent                  ! did user specify rc?
        integer :: i, dimlength

        ! initialize return code
        status = ESMF_FAILURE
        rcpresent = .FALSE.
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
        endif


        if (present(dataRank)) dataRank = datamap%dataRank

        if (present(dataIorder)) then
           dimlength = size(dataIorder,1)
           if (dimlength .lt. datamap%dataRank) then
             print *, "ESMF_ArrayDataMapGet: dataIorder array too short for dataRank"
             return
           endif

           do i=1, dimlength
             dataIorder(i) = datamap%dataDimOrder(i)
           enddo
        endif

        if (present(counts)) then
           dimlength = size(counts)
           do i=1, dimlength
             counts(i) = datamap%dataNonGridCounts(i)
           enddo
        endif

        if (present(horzRelloc)) horzRelloc = datamap%horzRelloc
        if (present(vertRelloc)) vertRelloc = datamap%vertRelloc

        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_ArrayDataMapGet


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_ArrayDataMapSet - Set a ArrayDataMap type object.
!
! !INTERFACE:
      subroutine ESMF_ArrayDataMapSet(datamap, dataRank, dataIorder, counts,  &
                                 horzRelloc, vertRelloc, rc)
!
! !ARGUMENTS:
      type(ESMF_ArrayDataMap), intent(inout) :: datamap  
      integer, intent(in), optional :: dataRank    
      integer, dimension(:), intent(in), optional :: dataIorder
      integer, dimension(:), intent(in), optional :: counts 
      type(ESMF_RelLoc), intent(in), optional :: horzRelloc 
      type(ESMF_RelLoc), intent(in), optional :: vertRelloc 
      integer, intent(out), optional :: rc       
!
! !DESCRIPTION:
!   Set info about the given {\tt ESMF\_ArrayDataMap}.
!
!   The arguments are:
!     \begin{description}
!     \item [datamap]
!           An {\tt ESMF\_ArrayDataMap} object.
!     \item [{[datarank]}]
!           The number of array dimensions.
!     \item [{[dataIorder]}]
!           An {\tt ESMF\_IndexOrder} object which describes one of several
!           predefined Index Orders.  There is another version of the Init
!           call which allows a more general form of the indexing; this is
!           a convenience routine for the most common cases.
!     \item [{[counts]}]
!           If the {\tt ESMF\_Array} object is a higher rank than the
!           {\tt ESMF\_Grid}, the additional dimensions may each have an
!           item count defined here.  This allows an {\tt ESMF\_FieldCreate()}
!           call to take an {\tt ESMF\_ArraySpec} and an {\tt ESMF\_ArrayDataMap}
!           and create the appropriately sized {\tt ESMF\_Array} for each
!           {\tt DE}.  If the {\tt ESMF\_Array} is created first, the counts
!           can be obtained from the {\tt ESMF\_Array} and this argument
!           is unneeded.  If the ranks of the grid and array are the same,
!           this is also unneeded.
!     \item [{[horzRelloc]}]
!           Relative location of data per grid cell/vertex in the horzontal
!           grid.
!     \item [{[vertRelloc]}]
!           Relative location of data per grid cell/vertex in the vertical grid.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!       \end{description}
!
!
!EOP
! !REQUIREMENTS: 
 
        ! local vars
        integer :: status                     ! local error status
        logical :: rcpresent                  ! did user specify rc?
        integer :: i, dimlength

        ! initialize return code
        status = ESMF_FAILURE
        rcpresent = .FALSE.
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
        endif


        if (present(dataRank)) datamap%dataRank = dataRank

        if (present(dataIorder)) then
           dimlength = size(dataIorder,1)
           if (dimlength .lt. datamap%dataRank) then
             print *, "ESMF_ArrayDataMapSet: dataIorder array too short for dataRank"
             return
           endif

           do i=1, dimlength
             datamap%dataDimOrder(i) = dataIorder(i)
           enddo
        endif

        if (present(counts)) then
           datamap%dataNonGridCounts(:) = 1
           dimlength = size(counts)
           do i=1, dimlength
             datamap%dataNonGridCounts(i) = counts(i)
           enddo
        endif

        if (present(horzRelloc)) datamap%horzRelloc = horzRelloc
        if (present(vertRelloc)) datamap%vertRelloc = vertRelloc

        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_ArrayDataMapSet



!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!
! This section is I/O for ArrayDataMaps
!
!

!------------------------------------------------------------------------------
!BOPI
! !INTERFACE:
      subroutine ESMF_ArrayDataMapWriteRestart(datamap, iospec, rc)
!
! !ARGUMENTS:
      type(ESMF_ArrayDataMap), intent(in) :: datamap
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
!     \item [datamap]
!           {\tt ESMF\_ArrayDataMap} object to save.
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

        end subroutine ESMF_ArrayDataMapWriteRestart


!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_ArrayDataMapReadRestart - Reinitialize a ArrayDataMap type
!
! !INTERFACE:
      function ESMF_ArrayDataMapReadRestart(name, iospec, rc)
!
! !RETURN VALUE:
      type(ESMF_ArrayDataMap) :: ESMF_ArrayDataMapReadRestart
!
!
! !ARGUMENTS:
      character (len = *), intent(in) :: name
      type(ESMF_IOSpec), intent(in), optional :: iospec
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!      Used to reinitialize
!      all data associated with a {\tt ESMF\_ArrayDataMap} 
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
 
        ESMF_ArrayDataMapReadRestart%status = ESMF_STATE_UNINIT
        if (present(rc)) rc = ESMF_FAILURE

        end function ESMF_ArrayDataMapReadRestart


!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_ArrayDataMapWrite - Store a ArrayDataMap type
!
! !INTERFACE:
      subroutine ESMF_ArrayDataMapWrite(datamap, iospec, rc)
!
! !ARGUMENTS:
      type(ESMF_ArrayDataMap), intent(in) :: datamap
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
!     \item [datamap]
!           {\tt ESMF\_ArrayDataMap} object to save.
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

        end subroutine ESMF_ArrayDataMapWrite


!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_ArrayDataMapRead - Read a stored ArrayDataMap type
!
! !INTERFACE:
      function ESMF_ArrayDataMapRead(name, iospec, rc)
!
! !RETURN VALUE:
      type(ESMF_ArrayDataMap) :: ESMF_ArrayDataMapRead
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
        ESMF_ArrayDataMapRead%status = ESMF_STATE_UNINIT
        if (present(rc)) rc = ESMF_SUCCESS

        end function ESMF_ArrayDataMapRead


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_ArrayDataMapValidate - Validate internal state of a ArrayDataMap type
!
! !INTERFACE:
      subroutine ESMF_ArrayDataMapValidate(datamap, options, rc)
!
! !ARGUMENTS:
      type(ESMF_ArrayDataMap), intent(in) :: datamap
      character (len = *), intent(in) :: options
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!      Routine to validate the internal state of a {\tt ESMF\_ArrayDataMap}.
!
!      The arguments are:
!     \begin{description}
!     \item [datamap]
!           {\tt ESMF\_ArrayDataMap} object to validate.
!     \item [{[options]}]
!           Validation options.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!       \end{description}
!
!EOP
! !REQUIREMENTS:  FLD4.1
        
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


        if (datamap%status .ne. ESMF_STATE_READY) return
            
        ! TODO: add more validation here - for index numbers, etc
 
        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_ArrayDataMapValidate

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_ArrayDataMapPrint - Print a ArrayDataMap type
!
!
! !INTERFACE:
      subroutine ESMF_ArrayDataMapPrint(datamap, options, rc)
!
!
! !ARGUMENTS:
      type(ESMF_ArrayDataMap), intent(in) :: datamap
      character (len = *), intent(in) :: options
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!      Routine to print information about a {\tt ESMF\_ArrayDataMap}.
!
!     The arguments are:
!     \begin{description}
!     \item [datamap]
!           {\tt ESMF\_ArrayDataMap} object to print.
!     \item [{[options]}]
!           Print options.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!       \end{description}
!
!EOP
! !REQUIREMENTS:

        integer :: i, j
        character (len = ESMF_MAXSTR) :: str

        print *, "ArrayDataMap print:"
        if (datamap%status .ne. ESMF_STATE_READY) then
          print *, "Uninitialized or Invalid object"
          if (present(rc)) rc = ESMF_FAILURE
          return
        endif

        ! individual data item information
        print *, " Data rank = ", datamap%dataRank
        print *, " Data Index Order and Lengths for non-Grid Indices:"
        j = 1
        do i=1, ESMF_MAXDIM
            if (datamap%dataDimOrder(i) .eq. 0) then
               print *, i, "Non-Grid index, length = ", datamap%dataNonGridCounts(j)
               j = j + 1
            else
               print *, i, "Grid index ", datamap%dataDimOrder(i)
            endif
        enddo
        if (datamap%dataRank .gt. 1) then
          print *, "  length of each dimension"
          do i=1, datamap%dataRank
              print *, i, datamap%rankLength(i)
          enddo
        endif

        call ESMF_RelLocString(datamap%horzRelloc, str, rc)
        print *, "  Horizontal Relative location = ", trim(str)
        call ESMF_RelLocString(datamap%vertRelloc, str, rc)
        print *, "  Vertical Relative location = ", trim(str)
        call ESMF_InterleaveString(datamap%interleave%il_type, str, rc)
        print *, "  Interleave type = ", trim(str), ".  Start,end,stride = ",  &
                                         datamap%interleave%il_start, & 
                                         datamap%interleave%il_end, & 
                                         datamap%interleave%il_strides
      
        end subroutine ESMF_ArrayDataMapPrint


!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!BOP
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
!      Routine to turn a relloc into a string.
!
!     The arguments are:
!     \begin{description}
!     \item [relloc]
!           The {\tt ESMF\_RelLoc} object to be turned into a string.
!     \item [string]
!          Return string.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!       \end{description}
!
!
!EOP
! !REQUIREMENTS:

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
!BOP
! !IROUTINE:  ESMF_InterleaveString - Return a interleave as a string
!
! !INTERFACE:
      subroutine ESMF_InterleaveString(interleave, string, rc)
!
!
! !ARGUMENTS:
      type(ESMF_InterleaveType), intent(in) :: interleave
      character (len = *), intent(out) :: string
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!      Routine to turn an interleave into a string.
!
!     The arguments are:
!     \begin{description}
!     \item [interleave]
!           The {\tt ESMF\_InterleaveType} object to be turned into a string.
!     \item [string]
!          Return string.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!       \end{description}
!
!
!EOP
! !REQUIREMENTS:

        if (interleave .eq. ESMF_IL_BLOCK) string = "Block Interleave"
        if (interleave .eq. ESMF_IL_ITEM) string = "Item Interleave"

        if (present(rc)) rc = ESMF_SUCCESS

        end subroutine ESMF_InterleaveString

!------------------------------------------------------------------------------
!BOP
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
!      Routine to turn an indexorder into a string.
!
!      The arguments are:
!     \begin{description}
!     \item [indexorder]
!           The {\tt ESMF\_IndexOrder} object to be turned into a string.
!     \item [string]
!           The corresponding string value.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!       \end{description}
!
!EOP
! !REQUIREMENTS:

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


        end module ESMF_ArrayDataMapMod


