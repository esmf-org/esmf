! $Id: ESMF_DataMap.F90,v 1.5 2003/09/09 20:11:07 nscollins Exp $
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
! ESMF DataMap module
!
!------------------------------------------------------------------------------
!
! !PURPOSE:
!
! The code in this file implements the DataMap derived type
!  and functions which operate on DataMaps.  
!  This type is implemented in Fortran 90 and a corresponding
!  C++ interface is provided for access.
!
! DataMaps are used to store the mapping of the array index orders
!   compared to the grid specifications; to indicate where data 
!   values are located relative to an individual cell/element,
!   store interleave information for larger than scalar data,
!   field interleave information for packed bundles, and any
!   other information needed to relate the data array to the grid.  
!
! TODO:  There is a single type of Create call which is currently fully
!   implemented.  The rest of the variants of Create, and the 
!   Get and Set routines still need to be implemented.
!
!------------------------------------------------------------------------------
!
#include "ESMF.h"

! module definition

      module ESMF_DataMapMod

!
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!
! This section contains the basic derived type for mappings, and the
!  internal subroutines and functions which operate on them.
!
!

!------------------------------------------------------------------------------
!BOPI
! !MODULE: ESMF_DataMapMod
      
! !USES:
      use ESMF_BaseMod
      use ESMF_IOMod

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
                           ESMF_CELL_CELL      = ESMF_RelLoc(10), &
                           ESMF_CELL_VERTEX    = ESMF_RelLoc(11)

 
!------------------------------------------------------------------------------
!  ! A set of predefined index orders, which shortcut setting
!  ! the dimension order array explicitly.
      type ESMF_IndexOrder
      sequence
      private
          integer :: iorder
      end type
 
      type(ESMF_IndexOrder), parameter :: &
                ESMF_IO_I   = ESMF_IndexOrder(0), &
                ESMF_IO_IJ  = ESMF_IndexOrder(1), &
                ESMF_IO_JI  = ESMF_IndexOrder(2), &
                ESMF_IO_IJK = ESMF_IndexOrder(3), &
                ESMF_IO_JIK = ESMF_IndexOrder(4), &
                ESMF_IO_KJI = ESMF_IndexOrder(5), &
                ESMF_IO_IKJ = ESMF_IndexOrder(6), &
                ESMF_IO_JKI = ESMF_IndexOrder(7), &
                ESMF_IO_KIJ = ESMF_IndexOrder(8)

!------------------------------------------------------------------------------
!  ! ESMF_DataMap
!  ! The data map type, which should fully describe the mapping
!  ! between index orders in the grid and the memory layout of
!  ! the data array, plus other metadata info such as where the
!  ! data is relative to the grid, and any interleaving info needed.
!  ! Note that the actual memory layout isn't here - it's encapsulated
!  ! in the Array class.  

      ! TODO: this should be a shallow type and not a deep class.
      !  it can be initialized with no allocation or state needed. 
      type ESMF_DataMapType
      sequence
      private
        ! index orders - how the grid dims (X,Y,Z) or (u,v) or (i,j,k)
        !  map onto the array memory layout as declared.  
        !  set dim numbers to 1-N, use 0 for dims which are part of data
        !  items and do not map to the grid dims.
        integer :: gridrank                             ! grid rank
        integer, dimension (ESMF_MAXDIM) :: dim_order   ! 0 = not a grid dim
        integer, dimension (ESMF_MAXDIM) :: sense       ! +/- iteration order
        ! individual data item information
        integer :: datarank                             ! scalar, vector, etc.
        integer, dimension (ESMF_MAXDIM) :: ranklength  ! len if > scalar
        type(ESMF_RelLoc) :: relloc                     ! data item loc/cell
        type(ESMF_Interleave) :: interleave             ! if > scalar
      end type


!------------------------------------------------------------------------------
!  ! ESMF_DataMap

      type ESMF_DataMap
      sequence
      private
#ifndef ESMF_NO_INITIALIZERS
        type(ESMF_DataMapType), pointer :: dmp => NULL()
#else
        type(ESMF_DataMapType), pointer :: dmp
#endif
      end type


!------------------------------------------------------------------------------
! !PUBLIC MEMBER TYPES:
!
      public ESMF_DataMap

      public ESMF_Interleave, ESMF_InterleaveType
      public ESMF_IL_BLOCK, ESMF_IL_ITEM

      public ESMF_IndexOrder, ESMF_IO_I, ESMF_IO_IJ, ESMF_IO_JI, &
             ESMF_IO_IJK, ESMF_IO_JIK, ESMF_IO_KJI, ESMF_IO_IKJ, &
             ESMF_IO_JKI, ESMF_IO_KIJ 

      public ESMF_RelLoc
      public ESMF_CELL_UNDEFINED, ESMF_CELL_CENTER
      public ESMF_CELL_NFACE,     ESMF_CELL_SFACE
      public ESMF_CELL_EFACE,     ESMF_CELL_WFACE
      public ESMF_CELL_NECORNER,  ESMF_CELL_NWCORNER
      public ESMF_CELL_SECORNER,  ESMF_CELL_SWCORNER
      public ESMF_CELL_CELL,      ESMF_CELL_VERTEX


! !PUBLIC MEMBER FUNCTIONS:
!
      public ESMF_DataMapCreate, ESMF_DataMapDestroy
      !public ESMF_DataMapConstruct, ESMF_DataMapDestruct
      public ESMF_DataMapSetInvalid

      public ESMF_DataMapGet, ESMF_DataMapSet

      public ESMF_DataMapWriteRestart, ESMF_DataMapReadRestart
      public ESMF_DataMapWrite, ESMF_DataMapRead 
      public ESMF_DataMapValidate, ESMF_DataMapPrint

      public ESMF_RelLocString, ESMF_InterleaveString

      public operator(.eq.), operator(.ne.)

!EOPI
 
!
!------------------------------------------------------------------------------
! leave the following line as-is; it will insert the cvs ident string
! into the object file for tracking purposes.
      character(*), parameter, private :: version =  &
             '$Id: ESMF_DataMap.F90,v 1.5 2003/09/09 20:11:07 nscollins Exp $'
!------------------------------------------------------------------------------



!------------------------------------------------------------------------------
!
! interface blocks here
!
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_DataMapCreate - Create a DataMap type

! !INTERFACE:
      interface ESMF_DataMapCreate

! !PRIVATE MEMBER FUNCTIONS:
       module procedure ESMF_DataMapCreateNew
       module procedure ESMF_DataMapCreateFastest

! !DESCRIPTION:
! This interface provides a single entry point for {\tt ESMF\_DataMap}
!  creation methods.
!EOP

      end interface 
                                      

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_DataMapConstruct - Initialize contents of a DataMap type

! !INTERFACE:
       interface ESMF_DataMapConstruct

! !PRIVATE MEMBER FUNCTIONS:
        module procedure ESMF_DataMapConstructNew
!       module procedure ESMF_DataMapConstructFastest

! !DESCRIPTION:
! This interface provides a single entry point for {\tt ESMF\_DataMap}
!  construction methods.
!EOPI

       end interface 

!------------------------------------------------------------------------------
! overload .eq. & .ne. with additional derived types so you can compare
!  them as if they were simple integers.

interface operator (.eq.)
 module procedure ESMF_rleq
 module procedure ESMF_ileq
end interface

interface operator (.ne.)
 module procedure ESMF_rlne
 module procedure ESMF_ilne
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
!------------------------------------------------------------------------------
!
! This section includes all the DataMap Create and Destroy routines
!
!

!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      function ESMF_DataMapCreateNew(iorder, relloc, rc)
!
! !RETURN VALUE:
      type(ESMF_DataMap) :: ESMF_DataMapCreateNew
!
!
! !ARGUMENTS:
      type(ESMF_IndexOrder), intent(in) :: iorder
      type(ESMF_RelLoc), intent(in), optional :: relloc
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!      Allocates space for and initializes an {\tt ESMF\_DataMap} object.
!
!      This version assumes the data is scalar and matches the {\tt ESMF\_Grid}
!      in order of {\tt ESMF\_Grid} index vs. {\tt ESMF\_Array} rank.  This creates a map
!      suitable for a {\tt ESMF\_Field} and not for a Packed Array associated with
!      a Bundle.
!
!      \begin{description}
!      \item[iorder]
!          One of 8 predefined index orderings.
!      \item[{[relloc]}]
!          Relative location of data per cell/vertex.
!      \item[{[rc]}] 
!          Return code equals {\tt ESMF\_SUCCESS} if the method
!          executes without errors.
!      \end{description}
!
!EOP
! !REQUIREMENTS:

        ! Local vars
        type (ESMF_DataMapType), pointer :: dmp    ! pointer to new DataMap
        integer :: status                     ! local error status
        logical :: rcpresent                  ! did user specify rc?

        ! Initialize pointers
        nullify(dmp) 
        nullify(ESMF_DataMapCreateNew%dmp)

        ! Initialize return code; assume failure until success is certain
        status = ESMF_FAILURE
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
        else
          rcpresent = .FALSE.
        endif

        ! Allocate space for DM object and call Construct method to initialize
        allocate(dmp, stat=status)
        if (status .ne. 0) then         ! this is a fortran rc, NOT an ESMF rc
          print *, "DataMap allocation error"
          return
        endif
    
        call ESMF_DataMapConstructNew(dmp, iorder, relloc, status)
        if (status .ne. ESMF_SUCCESS) then
           print *, "DataMap construction error"
           return
        endif

        ! Set return values
        ESMF_DataMapCreateNew%dmp => dmp
        if (rcpresent) rc = ESMF_SUCCESS

        end function ESMF_DataMapCreateNew


!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      function ESMF_DataMapCreateFastest(rc);

! !RETURN VALUE:
      type(ESMF_DataMap) :: ESMF_DataMapCreateFastest  
!
! !ARGUMENTS:
      integer, intent(out), optional :: rc           ! return code
!
! !DESCRIPTION:
!      Returns rc=OK if the {\tt ESMF\_DataMap} was created without error.
!      Returns an {\tt ESMF\_DataMap} which should allow the data array to be
!      iterated in the fastest access method possible on this hardware
!      and in this language.
!
!EOP
! !REQUIREMENTS:

!
! TODO: code goes here
!
        type (ESMF_DataMapType), pointer :: dmp
 
        allocate(dmp)

        dmp%datarank = 0

        ESMF_DataMapCreateFastest%dmp => dmp

        end function ESMF_DataMapCreateFastest



!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      subroutine ESMF_DataMapDestroy(datamap, rc)
!
! !ARGUMENTS:
      type(ESMF_DataMap) :: datamap 
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!     Returns rc=OK if the {\tt ESMF\_DataMap} was destroyed without error.
!     Releases all resources associated with this {\tt ESMF\_DataMap}.
!
!EOP
! !REQUIREMENTS: 

        ! Local vars
        integer :: status                     ! local error status
        logical :: rcpresent                  ! did user specify rc?

        ! Initialize return code
        status = ESMF_FAILURE
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
        else
          rcpresent = .FALSE.
        endif

        ! Call destroy to release other resources, then release this block
        call ESMF_DataMapDestruct(datamap%dmp, status)
        if (status .ne. ESMF_SUCCESS) then
            print *, "DataMap Destruct error"
            return
        endif

        deallocate(datamap%dmp, stat=status)
        if (status .ne. 0) then  ! fortran rc, not ESMF
            print *, "DataMap deallocate error"
            return
        endif
        nullify(datamap%dmp)

        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_DataMapDestroy


!------------------------------------------------------------------------------
!BOPI
! !INTERFACE:
      subroutine ESMF_DataMapConstructNew(datamap, iorder, relloc, rc)
!
! !ARGUMENTS:
      type(ESMF_DataMapType), pointer :: datamap
      type(ESMF_IndexOrder), intent(in) :: iorder
      type(ESMF_RelLoc), intent(in), optional :: relloc
      integer, intent(out), optional :: rc  
!
! !DESCRIPTION:
!      ESMF routine to initialize the contents of a {\tt ESMF\_DataMap} type.
!      The corresponding internal routine is {\tt ESMF\_Destruct}.
!
!EOPI
! !REQUIREMENTS: internal

!       local vars
        integer :: rank, i
        integer :: status                     ! local error status
        logical :: rcpresent                  ! did user specify rc?

!       init return code
        status = ESMF_FAILURE
        if (present(rc)) then
            rcpresent=.TRUE.
            rc = ESMF_FAILURE    
        else
          rcpresent = .FALSE.
        endif

!       initialize the contents of the datamap

!       set up the mapping of grid indicies to array indicies
        datamap%dim_order = 0

        select case (iorder%iorder)
          case(ESMF_IO_I%iorder) 
            datamap%gridrank = 1
            datamap%dim_order(1) = 1

          case(ESMF_IO_IJ%iorder)
            datamap%gridrank=2
            datamap%dim_order(1) = 1
            datamap%dim_order(2) = 2

          case(ESMF_IO_JI%iorder) 
            datamap%gridrank=2
            datamap%dim_order(1) = 2
            datamap%dim_order(2) = 1

          case(ESMF_IO_IJK%iorder)
            datamap%gridrank=3
            datamap%dim_order(1) = 1
            datamap%dim_order(2) = 2
            datamap%dim_order(3) = 3

          case(ESMF_IO_JIK%iorder)
            datamap%gridrank=3
            datamap%dim_order(1) = 2
            datamap%dim_order(2) = 1
            datamap%dim_order(3) = 3

          case(ESMF_IO_KJI%iorder)
            datamap%gridrank=3
            datamap%dim_order(1) = 3
            datamap%dim_order(2) = 2
            datamap%dim_order(3) = 1

          case(ESMF_IO_IKJ%iorder)
            datamap%gridrank=3
            datamap%dim_order(1) = 1
            datamap%dim_order(2) = 3
            datamap%dim_order(3) = 2

          case(ESMF_IO_JKI%iorder)
            datamap%gridrank=3
            datamap%dim_order(1) = 2
            datamap%dim_order(2) = 3
            datamap%dim_order(3) = 1

          case(ESMF_IO_KIJ%iorder)
            datamap%gridrank=3
            datamap%dim_order(1) = 3
            datamap%dim_order(2) = 1
            datamap%dim_order(3) = 2

          case default 
            print *, "unrecognized index order in ConstructDataMap"
            return
        end select

!       set the sense to undefined
        datamap%sense = 0
   
!       in this interface assume scalar data and use the relloc the caller gave
        datamap%datarank = 0
        datamap%ranklength = 0
        if (present(relloc)) then
          datamap%relloc = relloc
        else
          datamap%relloc = ESMF_CELL_CENTER
        endif

!       if user asked for it, return error code
        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_DataMapConstructNew


!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      subroutine ESMF_DataMapSetInvalid(datamap, rc)
!
! !ARGUMENTS:
      type(ESMF_DataMap), intent(inout) :: datamap
      integer, intent(out), optional :: rc  
!
! !DESCRIPTION:
!      ESMF routine to set the contents of a {\tt ESMF\_DataMap} type
!      to an uninitialized value.
!
!EOP
! !REQUIREMENTS: internal


!       local vars
        integer :: rank, i
        integer :: status                     ! local error status
        logical :: rcpresent                  ! did user specify rc?

        ! Init return code
        status = ESMF_FAILURE
        if (present(rc)) then
            rcpresent=.TRUE.
            rc = ESMF_FAILURE    
        else
          rcpresent = .FALSE.
        endif

        ! If this contained a previous datamap, deallocate it
        if (associated(datamap%dmp)) then
           deallocate(datamap%dmp, stat=status)
        endif

        ! Make sure it's invalid
        nullify(datamap%dmp)

        ! If user asked for it, return error code
        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_DataMapSetInvalid


!------------------------------------------------------------------------------
!BOPI
! !INTERFACE:
      subroutine ESMF_DataMapDestruct(datamap, rc)
!
! !ARGUMENTS:
      type(ESMF_DataMapType), pointer :: datamap  
      integer, intent(out), optional :: rc   
!
! !DESCRIPTION:
!      Release all resources except the {\tt ESMF\_DataMap} datatype itself.
!
!
!EOPI
! !REQUIREMENTS: internal

!       local vars
        integer :: status                     ! local error status
        logical :: rcpresent                  ! did user specify rc?

!       initialize return code
        status = ESMF_FAILURE
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
        else
          rcpresent = .FALSE.
        endif

!
! TODO: code goes here
!

        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_DataMapDestruct


!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!
! This section contains the routines which get and set the datamaps.
!

!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      subroutine ESMF_DataMapGet(datamap, gridrank, dimlist, &
                       relloc, rc)
!
! !ARGUMENTS:
      type(ESMF_DataMap), intent(in) :: datamap  
      integer, intent(out), optional :: gridrank    
      integer, dimension (:), intent(out), optional :: dimlist 
      type(ESMF_RelLoc), optional :: relloc
      integer, intent(out), optional :: rc       
!
! !DESCRIPTION:
!      Return info about the current {\tt ESMF\_DataMap} described by this object.
!
!EOP
! !REQUIREMENTS: 
 
        ! local vars
        integer :: status                     ! local error status
        logical :: rcpresent                  ! did user specify rc?
        integer :: i, dimlength
        type(ESMF_DataMapType), pointer :: dmp

        ! initialize return code
        status = ESMF_FAILURE
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
        else
          rcpresent = .FALSE.
        endif

        dmp => datamap%dmp

        if (present(gridrank)) then
           gridrank = dmp%gridrank
        endif

        if (present(relloc)) then
           relloc = dmp%relloc
        endif

        if (present(dimlist)) then
           dimlength = size(dimlist,1)
           if (dimlength .lt. gridrank) then
             print *, "ESMF_DataMapGet: dimlist too short for grid rank"
             return
           endif

           do i=1, gridrank
             dimlist(i) = dmp%dim_order(i)
           enddo
        endif

        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_DataMapGet


!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      subroutine ESMF_DataMapSet(datamap, rank, dimlist, rc)
!
! !ARGUMENTS:
      type(ESMF_DataMap), intent(in) :: datamap 
      integer, intent(in), optional :: rank    
      integer, dimension (1:ESMF_MAXDIM), intent(in), optional :: dimlist 
      integer, intent(out), optional :: rc    
!
! !DESCRIPTION:
!      Return info about the current {\tt ESMF\_DataMap} described by this object.
!
!EOP
! !REQUIREMENTS: 

!
! TODO: code goes here
!
        end subroutine ESMF_DataMapSet



!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!
! This section is I/O for DataMaps
!
!

!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      subroutine ESMF_DataMapWriteRestart(datamap, iospec, rc)
!
! !ARGUMENTS:
      type(ESMF_DataMap), intent(in) :: datamap                ! datamap to save
      type(ESMF_IOSpec), intent(in), optional :: iospec    ! file specs
      integer, intent(out), optional :: rc                 ! return code
!
! !DESCRIPTION:
!      Used to save all data to disk as quickly as possible.  
!      (see Read/Write for other options).  Internally this routine uses the
!      same I/O interface as Read/Write, but the default options are to
!      select the fastest way to save data to disk.
!
!EOP
! !REQUIREMENTS: FLD1.6.8

!
! TODO: code goes here
!
        end subroutine ESMF_DataMapWriteRestart


!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      function ESMF_DataMapReadRestart(name, iospec, rc)
!
! !RETURN VALUE:
      type(ESMF_DataMap) :: ESMF_DataMapReadRestart
!
!
! !ARGUMENTS:
      character (len = *), intent(in) :: name
      type(ESMF_IOSpec), intent(in), optional :: iospec
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!      Used to reinitialize
!      all data associated with a {\tt ESMF\_DataMap} 
!      from the last call to WriteRestart.
!
!EOP
! !REQUIREMENTS: FLD1.6.8

!
! TODO: code goes here
!
        type (ESMF_DataMapType), pointer :: dmp
 
        allocate(dmp)
        dmp%datarank = 0

        ESMF_DataMapReadRestart%dmp => dmp

        end function ESMF_DataMapReadRestart


!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      subroutine ESMF_DataMapWrite(datamap, iospec, rc)
!
! !ARGUMENTS:
      type(ESMF_DataMap), intent(in) :: datamap          ! datamap to save
      type(ESMF_IOSpec), intent(in), optional :: iospec    ! file specs
      integer, intent(out), optional :: rc                 ! return code
!
! !DESCRIPTION:
!      Used to write data to persistent storage in a variety of formats.  
!      (see WriteRestart/ReadRestart for quick data dumps.)  Details of I/O 
!      options specified in the IOSpec derived type. 
!
!
!EOP
! !REQUIREMENTS: FLD3.1, FLD3.2, FLD3.3, FLD3.4, FLD3.5

!
! TODO: code goes here
!
        end subroutine ESMF_DataMapWrite


!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      function ESMF_DataMapRead(name, iospec, rc)
!
! !RETURN VALUE:
      type(ESMF_DataMap) :: ESMF_DataMapRead
!
! !ARGUMENTS:
      character (len = *), intent(in) :: name
      type(ESMF_IOSpec), intent(in), optional :: iospec
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!      Used to read data from persistent storage in a variety of formats.
!
!
!EOP
! !REQUIREMENTS: (which req number is this?)

!
! TODO: code goes here
!
        type (ESMF_DataMapType), pointer :: dmp
 
        allocate(dmp)
        dmp%datarank = 0

        ESMF_DataMapRead%dmp => dmp

        end function ESMF_DataMapRead


!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      subroutine ESMF_DataMapValidate(datamap, options, rc)
!
! !ARGUMENTS:
      type(ESMF_DataMap), intent(in) :: datamap
      character (len = *), intent(in) :: options
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!      Routine to validate the internal state of a {\tt ESMF\_DataMap}.
!
!EOP
! !REQUIREMENTS:  FLD4.1

!
! TODO: code goes here
!
        end subroutine ESMF_DataMapValidate

!------------------------------------------------------------------------------
!BOP
!
!
! !INTERFACE:
      subroutine ESMF_DataMapPrint(datamap, options, rc)
!
!
! !ARGUMENTS:
      type(ESMF_DataMap), intent(in) :: datamap
      character (len = *), intent(in) :: options
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!      Routine to print information about a {\tt ESMF\_DataMap}.
!
!EOP
! !REQUIREMENTS:

        integer :: i 
        character (len = ESMF_MAXSTR) :: str
        type(ESMF_DataMapType), pointer :: dmp

        print *, "DataMap print:"
        if (.not.associated(datamap%dmp)) then
          print *, "Uninitialized or Destroyed object"
          if (present(rc)) rc = ESMF_FAILURE
          return
        endif

        dmp => datamap%dmp
        print *, " Grid rank = ", dmp%gridrank
        print *, " Dim order and sense:"
        do i=1, ESMF_MAXDIM
            print *, i, dmp%dim_order(i), dmp%sense(i)
        enddo

        ! individual data item information
        print *, " Data rank = ", dmp%datarank
        if (dmp%datarank .gt. 1) then
          print *, "  length of each dimension"
          do i=1, dmp%datarank
              print *, i, dmp%ranklength(i)
          enddo
        endif

        call ESMF_RelLocString(dmp%relloc, str, rc)
        print *, "  Data relative location = ", trim(str)
        call ESMF_InterleaveString(dmp%interleave%il_type, str, rc)
        print *, "  Interleave type = ", trim(str), ".  Start,end,stride = ",  &
                                         dmp%interleave%il_start, & 
                                         dmp%interleave%il_end, & 
                                         dmp%interleave%il_strides
      
        end subroutine ESMF_DataMapPrint


!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_RelLocString - Return a relloc as a string
!
! !INTERFACE:
      subroutine ESMF_RelLocString(relloc, string, rc)
!
!
! !ARGUMENTS:
      type(ESMF_RelLoc), intent(in) :: relloc           ! turn into string
      character (len = *), intent(out) :: string        ! where to return it
      integer, intent(out), optional :: rc              ! return code
!
! !DESCRIPTION:
!      Routine to turn a relloc into a string.
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
! !IROUTINE:  ESMF_InterleaveString - Return a indexorder as a string
!
! !INTERFACE:
      subroutine ESMF_InterleaveString(interleave, string, rc)
!
!
! !ARGUMENTS:
      type(ESMF_InterleaveType), intent(in) :: interleave   ! turn into string
      character (len = *), intent(out) :: string        ! where to return it
      integer, intent(out), optional :: rc              ! return code
!
! !DESCRIPTION:
!      Routine to turn an interleave into a string.
!
!EOP
! !REQUIREMENTS:

        if (interleave .eq. ESMF_IL_BLOCK) string = "Block Interleave"
        if (interleave .eq. ESMF_IL_ITEM) string = "Item Interleave"

        if (present(rc)) rc = ESMF_SUCCESS

        end subroutine ESMF_InterleaveString

!------------------------------------------------------------------------------

        end module ESMF_DataMapMod


