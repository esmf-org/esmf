! $Id: ESMF_ArrayMap.F90,v 1.1 2002/11/04 22:16:58 nscollins Exp $
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
! ESMF ArrayMap module
!
!------------------------------------------------------------------------------
!
! !PURPOSE:
!
! The code in this file implements the ArrayMap derived type
!  and functions which operate on ArrayMaps.  
!  This type is implemented in Fortran 90 and a corresponding
!  C++ interface is provided for access.
!
! ArrayMaps are used to store the mapping of the array index orders
!   compared to the grid specifications; to indicate where data 
!   values are located relative to an individual cell/element,
!   store interleave information for larger than scalar data,
!   field interleave information for packed bundles, and any
!   other information needed to relate the data array to the grid.  
!
!
!------------------------------------------------------------------------------
!
#include "ESMF_Macros.inc"
! if needed, add this here
!#include "ESMF_Data.h"

! module definition

      module ESMF_ArrayMapMod

!
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!
! This section contains the basic derived type for mappings, and the
!  internal subroutines and functions which operate on them.
!
!

!------------------------------------------------------------------------------
!BOP
! !MODULE: ESMF_ArrayMapMod
      
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
      end type ESMF_InterleaveType

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
      end type ESMF_Interleave


!------------------------------------------------------------------------------
!  ! ESMF_ArrayMap
!  ! Describes the individual data items in an array.

      type ESMF_ArrayMap
      sequence
      private
        ! individual data item information
        integer :: datarank                             ! scalar, vector, etc.
        integer, dimension (ESMF_MAXDIM) :: ranklength  ! len if > scalar
        type(ESMF_Interleave) :: interleave             ! if > scalar
      end type ESMF_ArrayMap



! !PUBLIC MEMBER TYPES:
!
      public ESMF_ArrayMap

      public ESMF_Interleave, ESMF_InterleaveType
      public ESMF_IL_BLOCK, ESMF_IL_ITEM


! !PUBLIC MEMBER FUNCTIONS:
!
      public ESMF_ArrayMapInit

      public ESMF_ArrayMapGet, ESMF_ArrayMapSet

      public ESMF_ArrayMapCheckpoint, ESMF_ArrayMapRestore
      public ESMF_ArrayMapWrite, ESMF_ArrayMapRead 
      public ESMF_ArrayMapValidate, ESMF_ArrayMapPrint


!EOP
 
!
!------------------------------------------------------------------------------
! leave the following line as-is; it will insert the cvs ident string
! into the object file for tracking purposes.
      character(*), parameter, private :: version = '$Id: ESMF_ArrayMap.F90,v 1.1 2002/11/04 22:16:58 nscollins Exp $'
!------------------------------------------------------------------------------




!------------------------------------------------------------------------------
! end of declarations & definitions
!------------------------------------------------------------------------------


      contains


!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!
! This section includes the ArrayMapInit routine.
!
!

!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      subroutine ESMF_ArrayMapInit(datarank, ranklengths, interleave, rc)
!
!
! !ARGUMENTS:
      integer, intent(in), optional :: datarank
      integer, dimension(:), intent(in), optional :: ranklengths
      type(ESMF_Interleave), intent(in), optional :: interleave
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!      Allocates space for and initializes an {\tt ESMF\_ArrayMap} object.
!
!      \begin{description}
!      \item[[datarank]]
!          0 = scalar, 1 = vector, 2 = tensor, etc.  The default is scalar.
!      \item[[ranklengths]]
!          If data is larger than scalar, the length of the vector or
!          shape of the tensor.
!      \item[[interleave]]
!          If data is larger than scalar, how the parts of the data are
!          interleaved.
!      \item[[rc]] 
!          Return code equals {\tt ESMF\_SUCCESS} if the method
!          executes without errors.
!      \end{description}
!
!EOP
! !REQUIREMENTS:

!       local vars
        integer :: status=ESMF_FAILURE        ! local error status
        logical :: rcpresent=.FALSE.          ! did user specify rc?

!       initialize return code; assume failure until success is certain
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
        endif

!       TODO: add code here to initialize the type

!       set return values
        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_ArrayMapInit




!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!
! This section contains the routines which get and set the arraymaps.
!

!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      subroutine ESMF_ArrayMapGet(arraymap, rank, dimlist, rc)
!
! !ARGUMENTS:
      type(ESMF_ArrayMap), intent(in) :: arraymap  
      integer, intent(out), optional :: rank    
      integer, dimension (1:ESMF_MAXDIM), intent(out), optional :: dimlist 
      integer, intent(out), optional :: rc       
!
! !DESCRIPTION:
!      Return info about the current arraymap described by this object.
!
!EOP
! !REQUIREMENTS: 

!
! code goes here
!
        end subroutine ESMF_ArrayMapGet


!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      subroutine ESMF_ArrayMapSet(arraymap, rank, dimlist, rc)
!
! !ARGUMENTS:
      type(ESMF_ArrayMap), intent(in) :: arraymap 
      integer, intent(in), optional :: rank    
      integer, dimension (1:ESMF_MAXDIM), intent(in), optional :: dimlist 
      integer, intent(out), optional :: rc    
!
! !DESCRIPTION:
!      Return info about the current arraymap described by this object.
!
!EOP
! !REQUIREMENTS: 

!
! code goes here
!
        end subroutine ESMF_ArrayMapSet



!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!
! This section is I/O for ArrayMaps
!
!

!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      subroutine ESMF_ArrayMapCheckpoint(arraymap, iospec, rc)
!
! !ARGUMENTS:
      type(ESMF_ArrayMap), intent(in) :: arraymap                ! arraymap to save
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
! code goes here
!
        end subroutine ESMF_ArrayMapCheckpoint


!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      function ESMF_ArrayMapRestore(name, iospec, rc)
!
! !RETURN VALUE:
      type(ESMF_ArrayMap) :: ESMF_ArrayMapRestore
!
!
! !ARGUMENTS:
      character (len = *), intent(in) :: name              ! arraymap name to restore
      type(ESMF_IOSpec), intent(in), optional :: iospec    ! file specs
      integer, intent(out), optional :: rc                 ! return code
!
! !DESCRIPTION:
!      Used to reinitialize
!      all data associated with a ArrayMap from the last call to Checkpoint.
!
!EOP
! !REQUIREMENTS: FLD1.6.8

!
! code goes here
!
        type (ESMF_ArrayMap) :: dm
 
        dm%datarank = 0

        ESMF_ArrayMapRestore = dm

        end function ESMF_ArrayMapRestore


!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      subroutine ESMF_ArrayMapWrite(arraymap, iospec, rc)
!
! !ARGUMENTS:
      type(ESMF_ArrayMap), intent(in) :: arraymap          ! arraymap to save
      type(ESMF_IOSpec), intent(in), optional :: iospec    ! file specs
      integer, intent(out), optional :: rc                 ! return code
!
! !DESCRIPTION:
!      Used to write data to persistent storage in a variety of formats.  
!      (see Checkpoint/Restore for quick data dumps.)  Details of I/O 
!      options specified in the IOSpec derived type. 
!
!
!EOP
! !REQUIREMENTS: FLD3.1, FLD3.2, FLD3.3, FLD3.4, FLD3.5

!
! code goes here
!
        end subroutine ESMF_ArrayMapWrite


!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      function ESMF_ArrayMapRead(name, iospec, rc)
!
! !RETURN VALUE:
      type(ESMF_ArrayMap) :: ESMF_ArrayMapRead
!
! !ARGUMENTS:
      character (len = *), intent(in) :: name              ! arraymap name to read
      type(ESMF_IOSpec), intent(in), optional :: iospec    ! file specs
      integer, intent(out), optional :: rc                 ! return code
!
! !DESCRIPTION:
!      Used to read data from persistent storage in a variety of formats.
!
!
!EOP
! !REQUIREMENTS: (which req number is this?)

!
! code goes here
!
        type (ESMF_ArrayMap) :: dm
 
        dm%datarank = 0

        ESMF_ArrayMapRead = dm

        end function ESMF_ArrayMapRead


!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      subroutine ESMF_ArrayMapValidate(arraymap, options, rc)
!
! !ARGUMENTS:
      type(ESMF_ArrayMap), intent(in) :: arraymap        ! what to check
      character (len = *), intent(in) :: options       ! select validate options
      integer, intent(out), optional :: rc             ! return code
!
! !DESCRIPTION:
!      Routine to validate the internal state of a arraymap.
!
!EOP
! !REQUIREMENTS:  FLD4.1

!
! code goes here
!
        end subroutine ESMF_ArrayMapValidate

!------------------------------------------------------------------------------
!BOP
!
!
! !INTERFACE:
      subroutine ESMF_ArrayMapPrint(arraymap, options, rc)
!
!
! !ARGUMENTS:
      type(ESMF_ArrayMap), intent(in) :: arraymap           ! what to print
      character (len = *), intent(in) :: options         ! select print options
      integer, intent(out), optional :: rc               ! return code
!
! !DESCRIPTION:
!      Routine to print information about a arraymap.
!
!EOP
! !REQUIREMENTS:

!
! code goes here
!
        end subroutine ESMF_ArrayMapPrint


        end module ESMF_ArrayMapMod


