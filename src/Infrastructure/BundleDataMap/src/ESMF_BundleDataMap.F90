! $Id: ESMF_BundleDataMap.F90,v 1.5 2004/05/12 12:15:09 nscollins Exp $
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

! !PUBLIC TYPES:
      implicit none
      private

!------------------------------------------------------------------------------
!  ! Interleave for packed arrays - are data for different fields 
!  ! concatenated or interleaved by item,

      type ESMF_BundleInterleave
      sequence
      private
          integer :: bil
      end type

      type(ESMF_BundleInterleave), parameter ::  &
                    ESMF_BIL_BYFIELD = ESMF_BundleInterleave(1), &
                    ESMF_BIL_BYITEM  = ESMF_BundleInterleave(2)

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
        ! only the bundle interleave needed here because each field contains
        ! its own private data map.   
#ifndef ESMF_NO_INITIALIZERS
        type(ESMF_BundleInterleave) :: bil = ESMF_BIL_BYFIELD
#else
        type(ESMF_BundleInterleave) :: bil
#endif
      end type


!------------------------------------------------------------------------------
! !PUBLIC MEMBER TYPES:
!
      public ESMF_BundleDataMap

      public ESMF_BundleInterleave
      public ESMF_BIL_BYFIELD, ESMF_BIL_BYITEM


! !PUBLIC MEMBER FUNCTIONS:
!
      ! TODO: this may need to become Create/Destroy (because it may need
      ! to be a deep object instead of a shallow one.)
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
       '$Id: ESMF_BundleDataMap.F90,v 1.5 2004/05/12 12:15:09 nscollins Exp $'
!------------------------------------------------------------------------------



!------------------------------------------------------------------------------
!
! interface blocks here
!
!------------------------------------------------------------------------------


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
! function to compare two ESMF_BundleInterleave flags 

function ESMF_bileq(il1, il2)
 logical ESMF_bileq
 type(ESMF_BundleInterleave), intent(in) :: il1, il2

 ESMF_bileq = (il1%bil .eq. il2%bil)
end function

function ESMF_bilne(il1, il2)
 logical ESMF_bilne
 type(ESMF_BundleInterleave), intent(in) :: il1, il2

 ESMF_bilne = (il1%bil .ne. il2%bil)
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
      subroutine ESMF_BundleDataMapInit(bundledatamap, bundleInterleave, rc)
!
! !ARGUMENTS:
      type(ESMF_BundleDataMap) :: bundledatamap
      type(ESMF_BundleInterleave), intent(in), optional :: bundleInterleave
      integer, intent(out), optional :: rc  
!
! !DESCRIPTION:
!     Initialize the contents of a {\tt ESMF\_BundleDataMap} type.
!
!     The arguments are:
!     \begin{description} 
!     \item [bundledatamap]
!           An {\tt ESMF\_BundleDataMap} object.
!     \item [{[bundleInterleave]}]
!           Type of interleave for {\tt ESMF\_Bundle} data if packed into
!           a single array.  Options are {\tt ESMF\_BIL\_BYITEM} and
!           {\tt ESMF\_BIL\_BYFIELD}.  If not specified, the default
!           is interleave by field.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
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

        ! set the default
        bundledatamap%bil = ESMF_BIL_BYFIELD

        ! initialize the contents of the bundle datamap
        if (present(bundleInterleave)) bundledatamap%bil = bundleInterleave
  
        ! mark object as initialized and ready to be used
        bundledatamap%status = ESMF_STATE_READY

        ! if user asked for it, return error code
        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_BundleDataMapInit


!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_BundleDataMapSetInvalid - set contents of a BundleDataMap to uninitialized value.

! !INTERFACE:
      subroutine ESMF_BundleDataMapSetInvalid(bundledatamap, rc)
!
! !ARGUMENTS:
      type(ESMF_BundleDataMap), intent(inout) :: bundledatamap
      integer, intent(out), optional :: rc  
!
! !DESCRIPTION:
!      ESMF routine to set the contents of a {\tt ESMF\_BundleDataMap} type
!      to an uninitialized value.
!
!     The arguments are:
!     \begin{description}
!     \item [bundledatamap]
!           An {\tt ESMF\_BundleDataMap} object.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP
! !REQUIREMENTS: internal

        bundledatamap%status = ESMF_STATE_INVALID

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
      subroutine ESMF_BundleDataMapGet(bundledatamap, bundleInterleave, rc)
!
! !ARGUMENTS:
      type(ESMF_BundleDataMap), intent(in) :: bundledatamap  
      type(ESMF_BundleInterleave), intent(out), optional :: bundleInterleave
      integer, intent(out), optional :: rc  
!
! !DESCRIPTION:
!   Return info about the {\tt ESMF\_BundleDataMap} described by this object.
!
!   The arguments are:
!     \begin{description}
!     \item [bundledatamap]
!           An {\tt ESMF\_BundleDataMap} object.
!     \item [{[bundleInterleave]}]
!           Type of interleave for {\tt ESMF\_Bundle} data if packed into
!           a single array.  Possible values are {\tt ESMF\_BIL\_BYITEM} and
!           {\tt ESMF\_BIL\_BYFIELD}. 
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
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

        ! if specified, return value
        if (present(bundleInterleave)) bundleInterleave = bundledatamap%bil

        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_BundleDataMapGet


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_BundleDataMapSet - Set a BundleDataMap type object.
!
! !INTERFACE:
      subroutine ESMF_BundleDataMapSet(bundledatamap, bundleInterleave, rc)
!
! !ARGUMENTS:
      type(ESMF_BundleDataMap), intent(inout) :: bundledatamap  
      type(ESMF_BundleInterleave), intent(in), optional :: bundleInterleave
      integer, intent(out), optional :: rc  
!
! !DESCRIPTION:
!   Set info about the given {\tt ESMF\_BundleDataMap}.
!
!   The arguments are:
!     \begin{description}
!     \item [bundledatamap]
!           An {\tt ESMF\_BundleDataMap} object.
!     \item [{[bundleInterleave]}]
!           Type of interleave for {\tt ESMF\_Bundle} data if packed into
!           a single array.  Options are {\tt ESMF\_BIL\_BYITEM} and
!           {\tt ESMF\_BIL\_BYFIELD}.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
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


        ! if specified, set value
        if (present(bundleInterleave)) bundledatamap%bil = bundleInterleave

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
      subroutine ESMF_BundleDataMapWriteRestart(bundledatamap, iospec, rc)
!
! !ARGUMENTS:
      type(ESMF_BundleDataMap), intent(in) :: bundledatamap
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
!     \item [bundledatamap]
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
      subroutine ESMF_BundleDataMapWrite(bundledatamap, iospec, rc)
!
! !ARGUMENTS:
      type(ESMF_BundleDataMap), intent(in) :: bundledatamap
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
!     \item [bundledatamap]
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
      subroutine ESMF_BundleDataMapValidate(bundledatamap, options, rc)
!
! !ARGUMENTS:
      type(ESMF_BundleDataMap), intent(in) :: bundledatamap
      character (len = *), intent(in) :: options
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!      Routine to validate the internal state of a {\tt ESMF\_BundleDataMap}.
!
!      The arguments are:
!     \begin{description}
!     \item [bundledatamap]
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


        if (bundledatamap%status .ne. ESMF_STATE_READY) return
            
        ! TODO: add more validation here - for index numbers, etc
 
        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_BundleDataMapValidate

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_BundleDataMapPrint - Print a BundleDataMap type
!
!
! !INTERFACE:
      subroutine ESMF_BundleDataMapPrint(bundledatamap, options, rc)
!
!
! !ARGUMENTS:
      type(ESMF_BundleDataMap), intent(in) :: bundledatamap
      character (len = *), intent(in) :: options
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!      Routine to print information about a {\tt ESMF\_BundleDataMap}.
!
!     The arguments are:
!     \begin{description}
!     \item [bundledatamap]
!           {\tt ESMF\_BundleDataMap} object to print.
!     \item [{[options]}]
!           Print options.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!       \end{description}
!
!EOP
! !REQUIREMENTS:

        character (len = ESMF_MAXSTR) :: str

        print *, "BundleDataMap print:"
        if (bundledatamap%status .ne. ESMF_STATE_READY) then
          print *, "Uninitialized or Invalid object"
          if (present(rc)) rc = ESMF_FAILURE
          return
        endif

        ! TODO: add print code here
        call ESMF_BundleInterleaveString(bundledatamap%bil, str, rc)
        print *, " Data: ", str
  
        if (present(rc)) rc = ESMF_SUCCESS
      
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
      type(ESMF_BundleInterleave), intent(in) :: interleave
      character (len = *), intent(out) :: string
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!      Routine to turn an interleave into a string.
!
!     The arguments are:
!     \begin{description}
!     \item [interleave]
!           The {\tt ESMF\_BundleInterleave} object to be turned into a string.
!     \item [string]
!          Return string.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!       \end{description}
!
!
!EOP
! !REQUIREMENTS:

        if (interleave .eq. ESMF_BIL_BYFIELD) string = "Interleave by Field"
        if (interleave .eq. ESMF_BIL_BYITEM) string = "Interleave by Item"

        if (present(rc)) rc = ESMF_SUCCESS

        end subroutine ESMF_BundleInterleaveString

!------------------------------------------------------------------------------


        end module ESMF_BundleDataMapMod


