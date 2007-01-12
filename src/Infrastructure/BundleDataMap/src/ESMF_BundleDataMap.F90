! $Id: ESMF_BundleDataMap.F90,v 1.31 2007/01/12 00:12:23 oehmke Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2008, University Corporation for Atmospheric Research, 
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
! Laboratory, University of Michigan, National Centers for Environmental 
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!------------------------------------------------------------------------------
#define ESMF_FILENAME "ESMF_BundleDataMap.F90"
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
! BundleDataMaps are used to store the mapping of multiple field data arrays
!   into a single packed array.  Other DataMaps exist at the Field and
!   Array level to indicate mappings of array to grid indices, data location
!   within a cell, vector interleave information, and any other items needed
!   to define how data is mapped between ESMF objects.
!
!------------------------------------------------------------------------------
!
#include "ESMF.h"

! module definition

      module ESMF_BundleDataMapMod

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!
! This section contains the basic derived types for encapsulating the
!  mapping information needed to identify which data items in a packed
!  array corresponds to individual field data.
!
!------------------------------------------------------------------------------
!BOPI
! !MODULE: ESMF_BundleDataMapMod
      
! !USES:
      use ESMF_UtilTypesMod    ! ESMF base class
      use ESMF_InitMacrosMod
      use ESMF_LogErrMod
      use ESMF_BaseMod
      use ESMF_IOSpecMod
      use ESMF_InternArrayDataMapMod

! !PUBLIC TYPES:
      implicit none
      private


!------------------------------------------------------------------------------
!  ! ESMF_BundleDataMap
!  ! The data map type, which should fully describe the mapping
!  ! between index orders in the grid and the memory layout of
!  ! the data array, plus other metadata info such as where the
!  ! data is relative to the grid, and any interleaving info needed.

      type ESMF_BundleDataMap
      sequence
      private
        ! only the bundle interleave needed here because each field contains
        ! its own private data map.   
#ifndef ESMF_NO_INITIALIZERS
        type(ESMF_Status) :: status = ESMF_STATUS_UNINIT
        type(ESMF_InterleaveFlag) :: bil = ESMF_INTERLEAVE_BY_ITEM
#else
        type(ESMF_Status) :: status 
        type(ESMF_InterleaveFlag) :: bil
#endif
        ESMF_INIT_DECLARE
      end type


!------------------------------------------------------------------------------
! !PUBLIC MEMBER TYPES:
!
      public ESMF_BundleDataMap


! !PUBLIC MEMBER FUNCTIONS:
!
      public ESMF_BundleDataMapInit     ! For Standardized Initialization
      public ESMF_BundleDataMapGetInit  ! For Standardized Initialization

      ! TODO: this may need to become Create/Destroy (because it may need
      ! to be a deep object instead of a shallow one.)
      public ESMF_BundleDataMapSetDefault
      public ESMF_BundleDataMapSetInvalid

      public ESMF_BundleDataMapGet, ESMF_BundleDataMapSet

      public ESMF_BundleDataMapWriteRestart, ESMF_BundleDataMapReadRestart
      public ESMF_BundleDataMapWrite, ESMF_BundleDataMapRead 
      public ESMF_BundleDataMapValidate, ESMF_BundleDataMapPrint


      public operator(.eq.), operator(.ne.)

!EOPI
 
!
!------------------------------------------------------------------------------
! leave the following line as-is; it will insert the cvs ident string
! into the object file for tracking purposes.
     character(*), parameter, private :: version =  &
       '$Id: ESMF_BundleDataMap.F90,v 1.31 2007/01/12 00:12:23 oehmke Exp $'
!------------------------------------------------------------------------------



!------------------------------------------------------------------------------
!
! interface blocks here
!
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
! end of declarations & definitions
!------------------------------------------------------------------------------


      contains

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_BundleDataMapGetInit"
!BOPI
! !IROUTINE:  ESMF_BundleDataMapGetInit - Get initialization status.

! !INTERFACE:
    function ESMF_BundleDataMapGetInit(s)
!
! !ARGUMENTS:
       type(ESMF_BundleDataMap), intent(in), optional :: s
       ESMF_INIT_TYPE :: ESMF_BundleDataMapGetInit
!
! !DESCRIPTION:
!     Get the initialization status of the shallow class {\tt bundledatamap}.
!
!     The arguments are:
!     \begin{description}
!     \item [s]
!           {\tt ESMF\_BundleDataMap} from which to retreive status.
!     \end{description}
!
!EOPI

       if (present(s)) then
         ESMF_BundleDataMapGetInit = ESMF_INIT_GET(s)
       else
         ESMF_BundleDataMapGetInit = ESMF_INIT_DEFINED
       endif

    end function ESMF_BundleDataMapGetInit

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_BundleDataMapInit"
!BOPI
! !IROUTINE:  ESMF_BundleDataMapInit - Initialize BundleDataMap

! !INTERFACE:
    subroutine ESMF_BundleDataMapInit(s)
!
! !ARGUMENTS:
       type(ESMF_BundleDataMap) :: s
!
! !DESCRIPTION:
!      Initialize the shallow class {\tt bundledatamap}.
!
!     The arguments are:
!     \begin{description}
!     \item [s]
!           {\tt ESMF\_BundleDataMap} of which being initialized.
!     \end{description}
!
!EOPI

        s%status = ESMF_STATUS_UNINIT
        s%bil = ESMF_INTERLEAVE_BY_ITEM

        ESMF_INIT_SET_DEFINED(s)
    end subroutine ESMF_BundleDataMapInit



!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_BundleDataMapGet"
!BOP
! !IROUTINE: ESMF_BundleDataMapGet - Get values from a BundleDataMap
!
! !INTERFACE:
      subroutine ESMF_BundleDataMapGet(bundledatamap, bundleinterleave, rc)
!
! !ARGUMENTS:
      type(ESMF_BundleDataMap), intent(in) :: bundledatamap  
      type(ESMF_InterleaveFlag), intent(out), optional :: bundleinterleave
      integer, intent(out), optional :: rc  
!
! !DESCRIPTION:
!   Return values from an {\tt ESMF\_BundleDataMap}.
!
!   The arguments are:
!     \begin{description}
!     \item [bundledatamap]
!           An {\tt ESMF\_BundleDataMap}.
!     \item [{[bundleinterleave]}]
!           Type of interleave for {\tt ESMF\_Bundle} data if packed into
!           a single array.  Possible values are 
!           {\tt ESMF\_INTERLEAVE\_BY\_ITEM} and
!           {\tt ESMF\_INTERLEAVE\_BY\_FIELD}. 
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
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

        ! check variables
        ESMF_INIT_CHECK_SHALLOW(ESMF_BundleDataMapGetInit, ESMF_BundleDataMapInit,bundledatamap)

        ! if specified, return value
        if (present(bundleinterleave)) bundleinterleave = bundledatamap%bil

        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_BundleDataMapGet


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_BundleDataMapPrint"
!BOP
! !IROUTINE: ESMF_BundleDataMapPrint - Print information about a BundleDataMap
!
!
! !INTERFACE:
      subroutine ESMF_BundleDataMapPrint(bundledatamap, options, rc)
!
!
! !ARGUMENTS:
      type(ESMF_BundleDataMap), intent(in) :: bundledatamap
      character (len = *), intent(in), optional :: options
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!           Prints diagnostic information about the {\tt bundledatamap}
!           to {\tt stdout}.  
!
!     The arguments are:
!     \begin{description}
!     \item [bundledatamap]
!           {\tt ESMF\_BundleDataMap} to print.
!     \item [{[options]}]
!           Print options are not yet supported.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!       \end{description}
!
!EOP

        !character(len=ESMF_MAXSTR) :: msgbuf
        character (len = ESMF_MAXSTR) :: str

      !jw  write (msgbuf, *)  "BundleDataMap print:"
      !jw  call ESMF_LogWrite(msgbuf, ESMF_LOG_INFO)
        write (*, *)  "BundleDataMap print:"

        if (bundledatamap%status .ne. ESMF_STATUS_READY) then
      !jw    write (msgbuf, *)  "Uninitialized or Invalid object"
      !jw    call ESMF_LogWrite(msgbuf, ESMF_LOG_INFO)
          write (*, *)  "Uninitialized or Invalid object"
          return
        endif

        ! TODO: add print code here
        call ESMF_InterleaveFlagString(bundledatamap%bil, str, rc)
      !jw  write (msgbuf, *)  " Data: ", str
      !jw  call ESMF_LogWrite(msgbuf, ESMF_LOG_INFO)
        write (*, *)  " Data: ", str
  
        if (present(rc)) rc = ESMF_SUCCESS
      
        end subroutine ESMF_BundleDataMapPrint


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_BundleDataMapSet"
!BOP
! !IROUTINE: ESMF_BundleDataMapSet - Set values in a BundleDataMap 
!
! !INTERFACE:
      subroutine ESMF_BundleDataMapSet(bundledatamap, bundleinterleave, rc)
!
! !ARGUMENTS:
      type(ESMF_BundleDataMap), intent(inout) :: bundledatamap  
      type(ESMF_InterleaveFlag), intent(in), optional :: bundleinterleave
      integer, intent(out), optional :: rc  
!
! !DESCRIPTION:
!   Set values in an {\tt ESMF\_BundleDataMap}.
!
!   The arguments are:
!     \begin{description}
!     \item [bundledatamap]
!           An {\tt ESMF\_BundleDataMap}.
!     \item [{[bundleinterleave]}]
!           Type of interleave for {\tt ESMF\_Bundle} data if packed into
!           a single array.  Options are {\tt ESMF\_INTERLEAVE\_BY\_ITEM} and
!           {\tt ESMF\_INTERLEAVE\_BY\_FIELD}.
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

        ! check variables
        ESMF_INIT_CHECK_SHALLOW(ESMF_BundleDataMapGetInit, ESMF_BundleDataMapInit,bundledatamap)

        ! if specified, set value
        if (present(bundleinterleave)) bundledatamap%bil = bundleinterleave

        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_BundleDataMapSet



!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_BundleDataMapSetDefault"
!BOP
! !IROUTINE:  ESMF_BundleDataMapSetDefault - Set BundleDataMap default values

! !INTERFACE:
      subroutine ESMF_BundleDataMapSetDefault(bundledatamap, bundleinterleave, rc)
!
! !ARGUMENTS:
      type(ESMF_BundleDataMap) :: bundledatamap
      type(ESMF_InterleaveFlag), intent(in), optional :: bundleinterleave
      integer, intent(out), optional :: rc  
!
! !DESCRIPTION:
!     Set default values of a {\tt ESMF\_BundleDataMap} type.
!     The differences between this routine and {\tt ESMF\_BundleDataMapSet()}
!     is that unspecified arguments are reset to their default values.
!
!     The arguments are:
!     \begin{description} 
!     \item [bundledatamap]
!           An {\tt ESMF\_BundleDataMap}.
!     \item [{[bundleinterleave]}]
!           Type of interleave for {\tt ESMF\_Bundle} data if packed into
!           a single array.  Options are {\tt ESMF\_INTERLEAVE\_BY\_ITEM} and
!           {\tt ESMF\_INTERLEAVE\_BY\_FIELD}.  If not specified, the default
!           is interleave by field.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
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

        ! check variables
        ESMF_INIT_CHECK_SHALLOW(ESMF_BundleDataMapGetInit, ESMF_BundleDataMapInit,bundledatamap)

        ! set the default
        bundledatamap%bil = ESMF_INTERLEAVE_BY_BLOCK

        ! initialize the contents of the bundle datamap
        if (present(bundleinterleave)) bundledatamap%bil = bundleinterleave
  
        ! mark object as initialized and ready to be used
        bundledatamap%status = ESMF_STATUS_READY

        ! if user asked for it, return error code
        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_BundleDataMapSetDefault


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_BundleDataMapSetInvalid"
!BOP
! !IROUTINE:  ESMF_BundleDataMapSetInvalid - Set BundleDataMap to invalid status

! !INTERFACE:
      subroutine ESMF_BundleDataMapSetInvalid(bundledatamap, rc)
!
! !ARGUMENTS:
      type(ESMF_BundleDataMap), intent(inout) :: bundledatamap
      integer, intent(out), optional :: rc  
!
! !DESCRIPTION:
!      ESMF routine to set the contents of an {\tt ESMF\_BundleDataMap}
!      to an invalid status.
!
!     The arguments are:
!     \begin{description}
!     \item [bundledatamap]
!           An {\tt ESMF\_BundleDataMap}.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP

        bundledatamap%status = ESMF_STATUS_INVALID

        ! If user asked for it, return error code
        if (present(rc)) rc = ESMF_SUCCESS

        end subroutine ESMF_BundleDataMapSetInvalid


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_BundleDataMapValidate"
!BOP
! !IROUTINE: ESMF_BundleDataMapValidate - Check validity of a BundleDataMap
!
! !INTERFACE:
      subroutine ESMF_BundleDataMapValidate(bundledatamap, options, rc)
!
! !ARGUMENTS:
      type(ESMF_BundleDataMap), intent(in) :: bundledatamap
      character (len = *), intent(in), optional :: options
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!      Validates that the {\tt bundledatamap} is internally consistent.
!      Currently this method determines if the {\tt bundledatamap} is uninitialized 
!      or already destroyed.  The method returns an error code if problems 
!      are found.  
!
!      The arguments are:
!     \begin{description}
!     \item [bundledatamap]
!           {\tt ESMF\_BundleDataMap} to validate.
!     \item [{[options]}]
!           Validation options are not yet supported.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if the {\tt bundledatamap}
!           is valid.
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

        ! check variables
        ESMF_INIT_CHECK_SHALLOW(ESMF_BundleDataMapGetInit, ESMF_BundleDataMapInit,bundledatamap)

        if (bundledatamap%status .ne. ESMF_STATUS_READY) then
         if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                "Uninitialized or already destroyed BundleDataMap", &
                                 ESMF_CONTEXT, rc)) return
        endif
            
        ! TODO: add more validation here - for index numbers, etc
 
        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_BundleDataMapValidate

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_BundleDataMapWriteRestart"
!BOPI
! !IROUTINE: ESMF_BundleDataMapWriteRestart - Save a BundleDataMap
!
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
!           {\tt ESMF\_BundleDataMap} to save.
!     \item [{[iospec]}]
!           File specification.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!       \end{description}
!
!
!EOPI

!	BOP/EOP changed to BOPI/EOPI until code is added.
!
! TODO: code goes here
!
        if (present(rc)) rc = ESMF_FAILURE

        end subroutine ESMF_BundleDataMapWriteRestart


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_BundleDataMapReadRestart"
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

!	Changed BOP/EOP to BOPI/EOPI until code is added.
!
! TODO: code goes here
!
 
        ESMF_BundleDataMapReadRestart%status = ESMF_STATUS_UNINIT
        if (present(rc)) rc = ESMF_FAILURE

        end function ESMF_BundleDataMapReadRestart


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_BundleDataMapWrite"
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
!           {\tt ESMF\_BundleDataMap} to save.
!     \item [{[iospec]}]
!           File specification.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!       \end{description}
!
!
!
!EOPI

!
!	Changed BOP/EOP to BOPI/EOPI until code is added.
! TODO: code goes here
!
        if (present(rc)) rc = ESMF_FAILURE

        end subroutine ESMF_BundleDataMapWrite


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_BundleDataMapRead"
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

!
!	Changed BOP/EOP to BOPI/EOPI until code is added.
! TODO: code goes here
!
        ESMF_BundleDataMapRead%status = ESMF_STATUS_UNINIT
        if (present(rc)) rc = ESMF_SUCCESS

        end function ESMF_BundleDataMapRead




        end module ESMF_BundleDataMapMod


