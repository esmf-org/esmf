! $Id: ESMF_FieldDataMap.F90,v 1.4 2004/05/26 18:30:39 nscollins Exp $
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
! FieldDataMaps are used to store the mapping of the array index orders
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
      use ESMF_BaseMod
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
        type(ESMF_Status) :: status = ESMF_STATE_UNINIT
#else
        type(ESMF_Status) :: status 
#endif
        type(ESMF_ArrayDataMap) :: adm

        ! plus the additional information we need about vector ordering
        ! on individual data items and the relative locations of the data
        ! in each grid cell.
        type(ESMF_Logical) :: isScalar                  ! scalar values
        integer, dimension(ESMF_MAXDIM) :: rankLength   ! len if > scalar
        type(ESMF_Interleave) :: interleave             ! if > scalar
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

!EOPI
 
!
!------------------------------------------------------------------------------
! leave the following line as-is; it will insert the cvs ident string
! into the object file for tracking purposes.
     character(*), parameter, private :: version =  &
         '$Id: ESMF_FieldDataMap.F90,v 1.4 2004/05/26 18:30:39 nscollins Exp $'
!------------------------------------------------------------------------------



!------------------------------------------------------------------------------
!
! interface blocks here
!
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_FieldDataMapSetDefault - Initialize a FieldDataMap type

! !INTERFACE:
      interface ESMF_FieldDataMapSetDefault

! !PRIVATE MEMBER FUNCTIONS:
       module procedure ESMF_FieldDataMapSetDefaultIndex
       module procedure ESMF_FieldDataMapSetDefaultExplicit

! !DESCRIPTION:
! This interface provides a single entry point for {\tt ESMF\_FieldDataMap}
!  initialization methods.
!EOP

      end interface 
                                      

!------------------------------------------------------------------------------
! overload .eq. & .ne. with additional derived types so you can compare
!  them as if they were simple integers.

!------------------------------------------------------------------------------
! end of declarations & definitions
!------------------------------------------------------------------------------

      contains

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!
! This section includes the FieldDataMap Init and Invalidate routines
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_FieldDataMapSetDefault - initialize the contents of a FieldDataMap

! !INTERFACE:
      ! Private name: scCall using ESMF_FieldDataMapSetDefault()
      subroutine ESMF_FieldDataMapSetDefaultIndex(datamap, dataIorder, counts, &
                                       horzRelloc, vertRelloc, rc)
!
! !ARGUMENTS:
      type(ESMF_FieldDataMap) :: datamap
      type(ESMF_IndexOrder), intent(in) :: dataIorder
      integer, dimension(:), intent(in), optional :: counts 
      type(ESMF_RelLoc), intent(in), optional :: horzRelloc 
      type(ESMF_RelLoc), intent(in), optional :: vertRelloc 
      integer, intent(out), optional :: rc  
!
! !DESCRIPTION:
!  Initialize the contents of a {\tt ESMF\_FieldDataMap} type.
!
!  The arguments are:
!  \begin{description} 
!  \item [datamap]
!        An {\tt ESMF\_FieldDataMap} object.
!  \item [dataIorder] 
!        An {\tt ESMF\_IndexOrder} object which describes one of several
!        predefined Index Orders.  There is another version of the Init
!        call which allows a more general form of the indexing; this is
!        a convenience routine for the most common cases.
!  \item [{[counts]}]
!        If the {\tt ESMF\_Array} object is a higher rank than the
!        {\tt ESMF\_Grid}, the additional dimensions may each have an 
!        item count defined here.  This allows an {\tt ESMF\_FieldCreate()}
!        call to take an {\tt ESMF\_ArraySpec} and an {\tt ESMF\_FieldDataMap}
!        and create the appropriately sized {\tt ESMF\_Array} for each
!        {\tt DE}.  If the {\tt ESMF\_Array} is created first, the counts
!        can be obtained from the {\tt ESMF\_Array} and this argument
!        is unneeded.  If the ranks of the grid and array are the same, 
!        this is also unneeded.
!  \item [{[horzRelloc]}]
!        Relative location of data per grid cell/vertex in the horzontal
!        grid.
!  \item [{[vertRelloc]}]
!        Relative location of data per grid cell/vertex in the vertical grid.
!  \end{description}
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

        ! initialize the contents of the internal array datamap
        call ESMF_ArrayDataMapSetDefault(datamap%adm, dataIorder, counts, status)
        if (status .ne. ESMF_SUCCESS) return

        ! assume scalar data and use the relloc the caller gave
        datamap%rankLength = 0

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

        end subroutine ESMF_FieldDataMapSetDefaultIndex


!------------------------------------------------------------------------------
!BOPI
! !INTERFACE:
      subroutine ESMF_FieldDataMapSetDefaultExplicit(datamap, dataRank, dataIndices, &
                                          counts, horzRelloc, vertRelloc, rc)
!
! !ARGUMENTS:
      type(ESMF_FieldDataMap) :: datamap
      integer, intent(in) :: dataRank
      integer, dimension(:), intent(in) :: dataIndices
      integer, dimension(:), intent(in), optional :: counts
      type(ESMF_RelLoc), intent(in), optional :: horzRelloc 
      type(ESMF_RelLoc), intent(in), optional :: vertRelloc 
      integer, intent(out), optional :: rc  
!
! !DESCRIPTION:
!   ESMF routine to initialize the contents of a {\tt ESMF\_FieldDataMap} type.
!
! The arguments are:
! \begin{description}
! \item [datamap]
!       An {\tt ESMF\_FieldDataMap} object.
! \item [dataRank] 
!       The number of the array dimensions.
! \item [dataIndices] 
!	The Grid rank which corresponds to this Array rank.  If
!       there is no correspondance (because the Array has a higher rank
!       than the Grid), the value must be 0.  The default is a 1-to-1
!       mapping of Grid to Array ranks.
! \item [{[counts]}]
!       If the {\tt ESMF\_Array} object is a higher rank than the
!       {\tt ESMF\_Grid}, the additional dimensions may each have an
!       item count defined here.  This allows an {\tt ESMF\_FieldCreate()}
!       call to take an {\tt ESMF\_ArraySpec} and an {\tt ESMF\_FieldDataMap}
!       and create the appropriately sized {\tt ESMF\_Array} for each
!       {\tt DE}.  If the {\tt ESMF\_Array} is created first, the counts
!       can be obtained from the {\tt ESMF\_Array} and this argument
!       is unneeded.  If the ranks of the grid and array are the same,
!       this is also unneeded.
! \item [{[horzRelloc]}]
!       Relative location of data per grid cell/vertex in the horzontal grid.
! \item [{[vertRelloc]}]
!       Relative location of data per grid cell/vertex in the vertical grid.
! \item [{[rc]}]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
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

        ! initialize the contents of the internal array datamap
        call ESMF_ArrayDataMapSetDefault(datamap%adm, dataRank, dataIndices, &
                                   counts, status)
        if (status .ne. ESMF_SUCCESS) return

        ! assume scalar data and use the relloc the caller gave
        datamap%rankLength = 0

        datamap%horzRelloc = ESMF_CELL_CENTER
        if (present(horzRelloc)) datamap%horzRelloc = horzRelloc

        datamap%vertRelloc = ESMF_CELL_UNDEFINED
        if (present(vertRelloc)) datamap%vertRelloc = vertRelloc

        ! mark object as initialized and ready to be used
        datamap%status = ESMF_STATE_READY

        ! if user asked for it, return error code
        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_FieldDataMapSetDefaultExplicit


!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_FieldDataMapSetInvalid - set contents of a FieldDataMap to uninitialized value.

! !INTERFACE:
      subroutine ESMF_FieldDataMapSetInvalid(datamap, rc)
!
! !ARGUMENTS:
      type(ESMF_FieldDataMap), intent(inout) :: datamap
      integer, intent(out), optional :: rc  
!
! !DESCRIPTION:
!      ESMF routine to set the contents of a {\tt ESMF\_FieldDataMap} type
!      to an uninitialized value.
!
!     The arguments are:
!     \begin{description}
!     \item [datamap]
!           An {\tt ESMF\_FieldDataMap} object.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!       \end{description}
!
!
!EOP
! !REQUIREMENTS: internal

        call ESMF_ArrayDataMapSetInvalid(datamap%adm, rc)

        datamap%status = ESMF_STATE_INVALID

        ! If user asked for it, return error code
        if (present(rc)) rc = ESMF_SUCCESS

        end subroutine ESMF_FieldDataMapSetInvalid


!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!
! This section contains the Get and Set routines
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_FieldDataMapGet - Get object from a FieldDataMap type.
!
! !INTERFACE:
      subroutine ESMF_FieldDataMapGet(datamap, dataRank, dataIndices, counts, &
                                 horzRelloc, vertRelloc, rc)
!
! !ARGUMENTS:
      type(ESMF_FieldDataMap), intent(in) :: datamap  
      integer, intent(out), optional :: dataRank    
      integer, dimension(:), intent(out), optional :: dataIndices
      integer, dimension(:), intent(out), optional :: counts 
      type(ESMF_RelLoc), intent(out), optional :: horzRelloc 
      type(ESMF_RelLoc), intent(out), optional :: vertRelloc 
      integer, intent(out), optional :: rc       
!
! !DESCRIPTION:
!   Return info about the current {\tt ESMF\_FieldDataMap} described 
!   by this object.
!
!  The arguments are:
!  \begin{description}
!  \item [datamap]
!        An {\tt ESMF\_FieldDataMap} object.
!  \item [{[datarank]}]
!	 The number of array dimensions.
!  \item [{[dataIndices]}]
!        The Grid rank which corresponds to this Array rank.  If
!        there is no correspondance (because the Array has a higher rank
!        than the Grid), the value will be 0.
!  \item [{[counts]}]
!        If the {\tt ESMF\_Array} object is a higher rank than the
!        {\tt ESMF\_Grid}, the additional dimensions may each have an
!        item count defined here.  This allows an {\tt ESMF\_FieldCreate()}
!        call to take an {\tt ESMF\_ArraySpec} and an {\tt ESMF\_FieldDataMap}
!        and create the appropriately sized {\tt ESMF\_Array} for each
!        {\tt DE}.  If the {\tt ESMF\_Array} is created first, the counts
!        can be obtained from the {\tt ESMF\_Array} and this argument
!        is unneeded.  If the ranks of the grid and array are the same,
!        this is also unneeded.
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


        ! get any values from the internal array datamap
        call ESMF_ArrayDataMapGet(datamap%adm, dataRank, dataIndices, &
                                  counts, status)
        if (status .ne. ESMF_SUCCESS) return

        ! TODO: need to add rankLength and vector information here

        if (present(horzRelloc)) horzRelloc = datamap%horzRelloc
        if (present(vertRelloc)) vertRelloc = datamap%vertRelloc

        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_FieldDataMapGet


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_FieldDataMapSet - Set a FieldDataMap type object.
!
! !INTERFACE:
      subroutine ESMF_FieldDataMapSet(datamap, dataRank, dataIndices, counts, &
                                 horzRelloc, vertRelloc, rc)
!
! !ARGUMENTS:
      type(ESMF_FieldDataMap), intent(inout) :: datamap  
      integer, intent(in), optional :: dataRank    
      integer, dimension(:), intent(in), optional :: dataIndices
      integer, dimension(:), intent(in), optional :: counts 
      type(ESMF_RelLoc), intent(in), optional :: horzRelloc 
      type(ESMF_RelLoc), intent(in), optional :: vertRelloc 
      integer, intent(out), optional :: rc       
!
! !DESCRIPTION:
! Set info about the given {\tt ESMF\_FieldDataMap}.
!
! The arguments are:
!  \begin{description}
!  \item [datamap]
!        An {\tt ESMF\_FieldDataMap} object.
!  \item [{[datarank]}]
!        The number of array dimensions.
!  \item [{[dataIndices]}]
!	 The Grid rank which corresponds to this Array rank.  
!        If there is no correspondance (because the Array has a higher rank
!        than the Grid), the value must be 0.  The default is a 1-to-1
!        mapping of Grid to Array ranks.
!  \item [{[dataIorder]}]
!        An {\tt ESMF\_IndexOrder} object which describes one of several
!        predefined Index Orders.  There is another version of the Init
!        call which allows a more general form of the indexing; this is
!        a convenience routine for the most common cases.
!  \item [{[counts]}]
!        If the {\tt ESMF\_Array} object is a higher rank than the
!        {\tt ESMF\_Grid}, the additional dimensions may each have an
!        item count defined here.  This allows an {\tt ESMF\_FieldCreate()}
!        call to take an {\tt ESMF\_ArraySpec} and an {\tt ESMF\_FieldDataMap}
!        and create the appropriately sized {\tt ESMF\_Array} for each
!        {\tt DE}.  If the {\tt ESMF\_Array} is created first, the counts
!        can be obtained from the {\tt ESMF\_Array} and this argument
!        is unneeded.  If the ranks of the grid and array are the same,
!        this is also unneeded.
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
        call ESMF_ArrayDataMapSet(datamap%adm, dataRank, dataIndices, &
                                   counts, status)
        if (status .ne. ESMF_SUCCESS) return

        ! TODO: add the vector information here

        if (present(horzRelloc)) datamap%horzRelloc = horzRelloc
        if (present(vertRelloc)) datamap%vertRelloc = vertRelloc

        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_FieldDataMapSet



!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!
! This section is I/O for FieldDataMaps
!
!

!------------------------------------------------------------------------------
!BOPI
! !INTERFACE:
      subroutine ESMF_FieldDataMapWriteRestart(datamap, iospec, rc)
!
! !ARGUMENTS:
      type(ESMF_FieldDataMap), intent(in) :: datamap
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
!           {\tt ESMF\_FieldDataMap} object to save.
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
 
        ESMF_FieldDataMapReadRestart%status = ESMF_STATE_UNINIT
        if (present(rc)) rc = ESMF_FAILURE

        end function ESMF_FieldDataMapReadRestart


!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_FieldDataMapWrite - Store a FieldDataMap type
!
! !INTERFACE:
      subroutine ESMF_FieldDataMapWrite(datamap, iospec, rc)
!
! !ARGUMENTS:
      type(ESMF_FieldDataMap), intent(in) :: datamap
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
!           {\tt ESMF\_FieldDataMap} object to save.
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
        ESMF_FieldDataMapRead%status = ESMF_STATE_UNINIT
        if (present(rc)) rc = ESMF_SUCCESS

        end function ESMF_FieldDataMapRead


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_FieldDataMapValidate - Validate internal state of a FieldDataMap type
!
! !INTERFACE:
      subroutine ESMF_FieldDataMapValidate(datamap, options, rc)
!
! !ARGUMENTS:
      type(ESMF_FieldDataMap), intent(in) :: datamap
      character (len = *), intent(in) :: options
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!      Routine to validate the internal state of a {\tt ESMF\_FieldDataMap}.
!
!      The arguments are:
!     \begin{description}
!     \item [datamap]
!           {\tt ESMF\_FieldDataMap} object to validate.
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

        end subroutine ESMF_FieldDataMapValidate

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_FieldDataMapPrint - Print a FieldDataMap type
!
!
! !INTERFACE:
      subroutine ESMF_FieldDataMapPrint(datamap, options, rc)
!
!
! !ARGUMENTS:
      type(ESMF_FieldDataMap), intent(in) :: datamap
      character (len = *), intent(in) :: options
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!      Routine to print information about a {\tt ESMF\_FieldDataMap}.
!
!     The arguments are:
!     \begin{description}
!     \item [datamap]
!           {\tt ESMF\_FieldDataMap} object to print.
!     \item [{[options]}]
!           Print options.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!       \end{description}
!
!EOP
! !REQUIREMENTS:

        character (len = ESMF_MAXSTR) :: str

        print *, "FieldDataMap print:"
        if (datamap%status .ne. ESMF_STATE_READY) then
          print *, "Uninitialized or Invalid object"
          if (present(rc)) rc = ESMF_FAILURE
          return
        endif

        ! individual data item information
        call ESMF_ArrayDataMapPrint(datamap%adm, options, rc)

        call ESMF_RelLocString(datamap%horzRelloc, str, rc)
        print *, "  Horizontal Relative location = ", trim(str)
        call ESMF_RelLocString(datamap%vertRelloc, str, rc)
        print *, "  Vertical Relative location = ", trim(str)
       ! TODO: this needs to become an InterleavePrint() call
       !call ESMF_InterleaveString(datamap%interleave%il_type, str, rc)
       !print *, "  Interleave type = ", trim(str), ".  Start,end,stride = ",  &
       !                                 datamap%interleave%il_start, & 
       !                                 datamap%interleave%il_end, & 
       !                                 datamap%interleave%il_strides
      
        end subroutine ESMF_FieldDataMapPrint


        end module ESMF_FieldDataMapMod


