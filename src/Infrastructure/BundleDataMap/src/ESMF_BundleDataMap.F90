! $Id: ESMF_BundleDataMap.F90,v 1.2 2004/05/05 15:42:38 nscollins Exp $
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
! ESMF BundleDataMap module
!
!------------------------------------------------------------------------------
!
! !PURPOSE:
!
! The code in this file implements the BundleDataMap derived type
!  and functions which operate on BundleDataMaps.  
!  This type is implemented in Fortran 90 and a corresponding
!  C++ interface is provided for access.
!
! BundleDataMaps are used to store the mapping of the array index orders
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

      module ESMF_BundleDataMapMod

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
! !MODULE: ESMF_BundleDataMapMod
      
! !USES:
      use ESMF_BaseMod
      use ESMF_IOSpecMod
      use ESMF_ArrayDataMapMod
      use ESMF_FieldDataMapMod

! !PUBLIC TYPES:
      implicit none
      private

!------------------------------------------------------------------------------
!  ! Interleaved types are used when there are multiple variables or
!  ! if individual data items are > scalar

      type ESMF_BundleInterleaveType
      sequence
      private
          integer :: bil_type
      end type

      type(ESMF_BundleInterleaveType), parameter ::  &
                    ESMF_BIL_BLOCK = ESMF_BundleInterleaveType(1), &
                    ESMF_BIL_ITEM  = ESMF_BundleInterleaveType(2)

!------------------------------------------------------------------------------
!  ! Expected to be used most often for packed bundles, to allow access to
!  ! data for an individual field.  Can also be used to describe vector data
!  ! that is packed into a single array.

      type ESMF_BundleInterleave
      sequence
      private
         type(ESMF_BundleInterleaveType) :: bil_type
         integer :: bil_start
         integer :: bil_end
         integer :: bil_strides 
      end type

 
!------------------------------------------------------------------------------
!  ! ESMF_BundleDataMap
!  ! The data map type, which should fully describe the mapping
!  ! between index orders in the grid and the memory layout of
!  ! the data array, plus other metadata info such as where the
!  ! data is relative to the grid, and any interleaving info needed.

      type ESMF_BundleDataMap
      sequence
      private
#ifndef ESMF_NO_INITIALIZERS
        type(ESMF_Status) :: status = ESMF_STATE_UNINIT
#else
        type(ESMF_Status) :: status 
#endif
        ! TODO: add interleaves here
        type(ESMF_FieldDataMap) :: fdm
        type(ESMF_BundleInterleave) :: bil
      end type


!------------------------------------------------------------------------------
! !PUBLIC MEMBER TYPES:
!
      public ESMF_BundleDataMap

      public ESMF_BundleInterleave
      public ESMF_BIL_BLOCK, ESMF_BIL_ITEM


! !PUBLIC MEMBER FUNCTIONS:
!
      public ESMF_BundleDataMapInit
      public ESMF_BundleDataMapSetInvalid

      public ESMF_BundleDataMapGet, ESMF_BundleDataMapSet

      public ESMF_BundleDataMapWriteRestart, ESMF_BundleDataMapReadRestart
      public ESMF_BundleDataMapWrite, ESMF_BundleDataMapRead 
      public ESMF_BundleDataMapValidate, ESMF_BundleDataMapPrint

      public ESMF_BundleInterleaveString

      public operator(.eq.), operator(.ne.)

!EOPI
 
!
!------------------------------------------------------------------------------
! leave the following line as-is; it will insert the cvs ident string
! into the object file for tracking purposes.
      character(*), parameter, private :: version =  &
             '$Id: ESMF_BundleDataMap.F90,v 1.2 2004/05/05 15:42:38 nscollins Exp $'
!------------------------------------------------------------------------------



!------------------------------------------------------------------------------
!
! interface blocks here
!
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_BundleDataMapInit - Initialize a BundleDataMap type

! !INTERFACE:
      interface ESMF_BundleDataMapInit

! !PRIVATE MEMBER FUNCTIONS:
       module procedure ESMF_BundleDataMapInitIndex
       module procedure ESMF_BundleDataMapInitExplicit

! !DESCRIPTION:
! This interface provides a single entry point for {\tt ESMF\_BundleDataMap}
!  initialization methods.
!EOP

      end interface 
                                      

!------------------------------------------------------------------------------
! overload .eq. & .ne. with additional derived types so you can compare
!  them as if they were simple integers.

interface operator (.eq.)
 module procedure ESMF_bileq
end interface

interface operator (.ne.)
 module procedure ESMF_bilne
end interface

!------------------------------------------------------------------------------
! end of declarations & definitions
!------------------------------------------------------------------------------


      contains


!------------------------------------------------------------------------------
! function to compare two ESMF_BundleInterleaveType flags 

function ESMF_bileq(il1, il2)
 logical ESMF_bileq
 type(ESMF_BundleInterleaveType), intent(in) :: il1, il2

 ESMF_bileq = (il1%bil_type .eq. il2%bil_type)
end function

function ESMF_bilne(il1, il2)
 logical ESMF_bilne
 type(ESMF_BundleInterleaveType), intent(in) :: il1, il2

 ESMF_bilne = (il1%bil_type .ne. il2%bil_type)
end function



!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!
! This section includes the BundleDataMap Init and Invalidate routines
!
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_BundleDataMapInit - initialize the contents of a BundleDataMap

! !INTERFACE:
      ! Private name: scCall using ESMF_BundleDataMapInit()
      subroutine ESMF_BundleDataMapInitIndex(datamap, dataIorder, counts, &
                                       horzRelloc, vertRelloc, rc)
!
! !ARGUMENTS:
      type(ESMF_BundleDataMap) :: datamap
      type(ESMF_IndexOrder), intent(in) :: dataIorder
      integer, dimension(:), intent(in), optional :: counts 
      type(ESMF_RelLoc), intent(in), optional :: horzRelloc 
      type(ESMF_RelLoc), intent(in), optional :: vertRelloc 
      integer, intent(out), optional :: rc  
!
! !DESCRIPTION:
!     Initialize the contents of a {\tt ESMF\_BundleDataMap} type.
!
!     The arguments are:
!     \begin{description} 
!     \item [datamap]
!           An {\tt ESMF\_BundleDataMap} object.
!     \item [dataIorder] 
!           An {\tt ESMF\_IndexOrder} object which describes one of several
!           predefined Index Orders.  There is another version of the Init
!           call which allows a more general form of the indexing; this is
!           a convenience routine for the most common cases.
!     \item [{[counts]}]
!           If the {\tt ESMF\_Array} object is a higher rank than the
!           {\tt ESMF\_Grid}, the additional dimensions may each have an 
!           item count defined here.  This allows an {\tt ESMF\_FieldCreate()}
!           call to take an {\tt ESMF\_ArraySpec} and an {\tt ESMF\_BundleDataMap}
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
            print *, "ERROR: ESMF_BundleDataMapInit - unrecognized grid index order"
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

        end subroutine ESMF_BundleDataMapInitIndex


!------------------------------------------------------------------------------
!BOPI
! !INTERFACE:
      subroutine ESMF_BundleDataMapInitExplicit(datamap, dataRank, dataIndices, &
                                          counts, horzRelloc, vertRelloc, rc)
!
! !ARGUMENTS:
      type(ESMF_BundleDataMap) :: datamap
      integer, intent(in) :: dataRank
      integer, dimension(:), intent(in) :: dataIndices
      integer, dimension(:), intent(in), optional :: counts
      type(ESMF_RelLoc), intent(in), optional :: horzRelloc 
      type(ESMF_RelLoc), intent(in), optional :: vertRelloc 
      integer, intent(out), optional :: rc  
!
! !DESCRIPTION:
!      ESMF routine to initialize the contents of a {\tt ESMF\_BundleDataMap} type.
!
!     The arguments are:
!     \begin{description}
!     \item [datamap]
!           An {\tt ESMF\_BundleDataMap} object.
!     \item [dataRank] 
!	    The number of the array dimensions.
!     \item [dataIndices] 
!	    TODO: Add description           
!     \item [{[counts]}]
!           If the {\tt ESMF\_Array} object is a higher rank than the
!           {\tt ESMF\_Grid}, the additional dimensions may each have an
!           item count defined here.  This allows an {\tt ESMF\_FieldCreate()}
!           call to take an {\tt ESMF\_ArraySpec} and an {\tt ESMF\_BundleDataMap}
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

        end subroutine ESMF_BundleDataMapInitExplicit


!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_BundleDataMapSetInvalid - set contents of a BundleDataMap to uninitialized value.

! !INTERFACE:
      subroutine ESMF_BundleDataMapSetInvalid(datamap, rc)
!
! !ARGUMENTS:
      type(ESMF_BundleDataMap), intent(inout) :: datamap
      integer, intent(out), optional :: rc  
!
! !DESCRIPTION:
!      ESMF routine to set the contents of a {\tt ESMF\_BundleDataMap} type
!      to an uninitialized value.
!
!     The arguments are:
!     \begin{description}
!     \item [datamap]
!           An {\tt ESMF\_BundleDataMap} object.
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

        end subroutine ESMF_BundleDataMapSetInvalid


!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!
! This section contains the Get and Set routines
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_BundleDataMapGet - Get object from a BundleDataMap type.
!
! !INTERFACE:
      subroutine ESMF_BundleDataMapGet(datamap, dataRank, dataIorder, counts, &
                                 horzRelloc, vertRelloc, rc)
!
! !ARGUMENTS:
      type(ESMF_BundleDataMap), intent(in) :: datamap  
      integer, intent(out), optional :: dataRank    
      integer, dimension(:), intent(out), optional :: dataIorder
      integer, dimension(:), intent(out), optional :: counts 
      type(ESMF_RelLoc), intent(out), optional :: horzRelloc 
      type(ESMF_RelLoc), intent(out), optional :: vertRelloc 
      integer, intent(out), optional :: rc       
!
! !DESCRIPTION:
!   Return info about the current {\tt ESMF\_BundleDataMap} described by this object.
!
!   The arguments are:
!     \begin{description}
!     \item [datamap]
!           An {\tt ESMF\_BundleDataMap} object.
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
!           call to take an {\tt ESMF\_ArraySpec} and an {\tt ESMF\_BundleDataMap}
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
             print *, "ESMF_BundleDataMapGet: dataIorder array too short for dataRank"
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

        end subroutine ESMF_BundleDataMapGet


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_BundleDataMapSet - Set a BundleDataMap type object.
!
! !INTERFACE:
      subroutine ESMF_BundleDataMapSet(datamap, dataRank, dataIorder, counts,  &
                                 horzRelloc, vertRelloc, rc)
!
! !ARGUMENTS:
      type(ESMF_BundleDataMap), intent(inout) :: datamap  
      integer, intent(in), optional :: dataRank    
      integer, dimension(:), intent(in), optional :: dataIorder
      integer, dimension(:), intent(in), optional :: counts 
      type(ESMF_RelLoc), intent(in), optional :: horzRelloc 
      type(ESMF_RelLoc), intent(in), optional :: vertRelloc 
      integer, intent(out), optional :: rc       
!
! !DESCRIPTION:
!   Set info about the given {\tt ESMF\_BundleDataMap}.
!
!   The arguments are:
!     \begin{description}
!     \item [datamap]
!           An {\tt ESMF\_BundleDataMap} object.
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
!           call to take an {\tt ESMF\_ArraySpec} and an {\tt ESMF\_BundleDataMap}
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
             print *, "ESMF_BundleDataMapSet: dataIorder array too short for dataRank"
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

        end subroutine ESMF_BundleDataMapSet



!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!
! This section is I/O for BundleDataMaps
!
!

!------------------------------------------------------------------------------
!BOPI
! !INTERFACE:
      subroutine ESMF_BundleDataMapWriteRestart(datamap, iospec, rc)
!
! !ARGUMENTS:
      type(ESMF_BundleDataMap), intent(in) :: datamap
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
!           {\tt ESMF\_BundleDataMap} object to save.
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

        end subroutine ESMF_BundleDataMapWriteRestart


!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_BundleDataMapReadRestart - Reinitialize a BundleDataMap type
!
! !INTERFACE:
      function ESMF_BundleDataMapReadRestart(name, iospec, rc)
!
! !RETURN VALUE:
      type(ESMF_BundleDataMap) :: ESMF_BundleDataMapReadRestart
!
!
! !ARGUMENTS:
      character (len = *), intent(in) :: name
      type(ESMF_IOSpec), intent(in), optional :: iospec
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!      Used to reinitialize
!      all data associated with a {\tt ESMF\_BundleDataMap} 
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
 
        ESMF_BundleDataMapReadRestart%status = ESMF_STATE_UNINIT
        if (present(rc)) rc = ESMF_FAILURE

        end function ESMF_BundleDataMapReadRestart


!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_BundleDataMapWrite - Store a BundleDataMap type
!
! !INTERFACE:
      subroutine ESMF_BundleDataMapWrite(datamap, iospec, rc)
!
! !ARGUMENTS:
      type(ESMF_BundleDataMap), intent(in) :: datamap
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
!           {\tt ESMF\_BundleDataMap} object to save.
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

        end subroutine ESMF_BundleDataMapWrite


!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_BundleDataMapRead - Read a stored BundleDataMap type
!
! !INTERFACE:
      function ESMF_BundleDataMapRead(name, iospec, rc)
!
! !RETURN VALUE:
      type(ESMF_BundleDataMap) :: ESMF_BundleDataMapRead
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
        ESMF_BundleDataMapRead%status = ESMF_STATE_UNINIT
        if (present(rc)) rc = ESMF_SUCCESS

        end function ESMF_BundleDataMapRead


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_BundleDataMapValidate - Validate internal state of a BundleDataMap type
!
! !INTERFACE:
      subroutine ESMF_BundleDataMapValidate(datamap, options, rc)
!
! !ARGUMENTS:
      type(ESMF_BundleDataMap), intent(in) :: datamap
      character (len = *), intent(in) :: options
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!      Routine to validate the internal state of a {\tt ESMF\_BundleDataMap}.
!
!      The arguments are:
!     \begin{description}
!     \item [datamap]
!           {\tt ESMF\_BundleDataMap} object to validate.
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

        end subroutine ESMF_BundleDataMapValidate

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_BundleDataMapPrint - Print a BundleDataMap type
!
!
! !INTERFACE:
      subroutine ESMF_BundleDataMapPrint(datamap, options, rc)
!
!
! !ARGUMENTS:
      type(ESMF_BundleDataMap), intent(in) :: datamap
      character (len = *), intent(in) :: options
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!      Routine to print information about a {\tt ESMF\_BundleDataMap}.
!
!     The arguments are:
!     \begin{description}
!     \item [datamap]
!           {\tt ESMF\_BundleDataMap} object to print.
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

        print *, "BundleDataMap print:"
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
      
        end subroutine ESMF_BundleDataMapPrint


!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_BundleInterleaveString - Return a interleave as a string
!
! !INTERFACE:
      subroutine ESMF_BundleInterleaveString(interleave, string, rc)
!
!
! !ARGUMENTS:
      type(ESMF_BundleInterleaveType), intent(in) :: interleave
      character (len = *), intent(out) :: string
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!      Routine to turn an interleave into a string.
!
!     The arguments are:
!     \begin{description}
!     \item [interleave]
!           The {\tt ESMF\_BundleInterleaveType} object to be turned into a string.
!     \item [string]
!          Return string.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!       \end{description}
!
!
!EOP
! !REQUIREMENTS:

        if (interleave .eq. ESMF_BIL_BLOCK) string = "Block Interleave"
        if (interleave .eq. ESMF_BIL_ITEM) string = "Item Interleave"

        if (present(rc)) rc = ESMF_SUCCESS

        end subroutine ESMF_BundleInterleaveString

!------------------------------------------------------------------------------


        end module ESMF_BundleDataMapMod


