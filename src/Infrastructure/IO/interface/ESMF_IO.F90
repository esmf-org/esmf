! $Id: ESMF_IO.F90,v 1.8 2003/04/15 18:05:53 nscollins Exp $
!-------------------------------------------------------------------------
!
! ESMF IO module
!
! This code covered by the GNU public license.  See licence file for details.
! NCAR, 2002.
!

!-------------------------------------------------------------------------
!
! !PURPOSE:
!
! The code in this file implements the IO defined type
!  and functions which operate on it.  This is an
!  interface to the actual IO class object in the ../src dir.
!
! See the ESMF Developers Guide document for more details.
!
! (all lines between the !BOP and !EOP markers will be included in the
! automated document processing.)
!
!
!-------------------------------------------------------------------------

! module definition

      module ESMF_IOMod

!
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!BOP
! !MODULE: ESMF_IOmod
!
! !USES:
      use ESMF_BaseMod
      implicit none

! !PUBLIC TYPES:
      private
    
      integer, parameter :: UNKNOWN=0, NETCDF=1

      type ESMF_IOType
      sequence
      private
         integer :: iotype
      end type

      type ESMF_IOSpec
      sequence
      private
          type (ESMF_Status) :: iostatus
          type (ESMF_IOType) :: iotype
          integer :: current_status
          ! should be a derived type or enum
          logical :: async_io
      end type

   
! !DESCRIPTION:
!     The following routines apply to general I/O characteristics.

! !PUBLIC MEMBER TYPES:
      public ESMF_IOSpec

! !PUBLIC MEMBER FUNCTIONS:

!     Temporary for putting system dependent I/O code in a single place.
      public ESMF_IOFlush
!
!     function ESMF_IOSpecCreate() (interface only)
!     function ESMF_IOSpecCreateNew()
!     function ESMF_IOSpecCreateCopy()
! or
!     function ESMF_IOSpecInit()
!
!     subroutine ESMF_IOSpecConstruct()
!     subroutine ESMF_IOSpecDestruct()
!
!     subroutine ESMF_IOSpecSet()
!     subroutine ESMF_IOSpecGet()
! 
!     subroutine ESMF_IOSpecSetName()  
!     subroutine ESMF_IOSpecGetName()
!     subroutine ESMF_IOSpecSetDestName()
!     subroutine ESMF_IOSpecGetDestName()
!     subroutine ESMF_IOSpecSetSrcName()
!     subroutine ESMF_IOSpecGetSrcName()
!
!     subroutine ESMF_IOSpecSetType()  
!     subroutine ESMF_IOSpecGetType()
!     subroutine ESMF_IOSpecSetDestType()
!     subroutine ESMF_IOSpecGetDestType()
!     subroutine ESMF_IOSpecSetSrcType()
!     subroutine ESMF_IOSpecGetSrcType()
!
!     subroutine ESMF_IOSpecSetOptions()  
!     subroutine ESMF_IOSpecGetOptions()
!     subroutine ESMF_IOSpecSetDestOptions()
!     subroutine ESMF_IOSpecGetDestOptions()
!     subroutine ESMF_IOSpecSetSrcOptions()
!     subroutine ESMF_IOSpecGetSrcOptions()
!
!     subroutine ESMF_IOSpecPrint()
!     subroutine ESMF_IOSpecValidate()
!
!     subroutine ESMF_IOSpecxxx
!

!EOP

!BOP
!  !INTERFACE:
      interface ESMF_IOSpecCreate
!
      end interface

! add interface block for Create here

!-------------------------------------------------------------------------

      contains

!-------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_IOFlush - flush output on a unit number
!
! !INTERFACE:
      subroutine ESMF_IOFlush(unitNumber, rc)
!
! !PARAMETERS:
      integer, intent(in) :: unitNumber
      integer, intent(out), optional :: rc               ! return code

!
! !DESCRIPTION:
!   Call the system-dependent routine to force output.

!
! !REQUIREMENTS: 

!EOP
      integer :: status

      status = ESMF_FAILURE

#if   defined(PARCH_linux)
      print *, "need to call flush() here"
#elif defined(PARCH_IRIX64)
      call flush(unitNumber, status)
#elif defined(PARCH_rs6000)
      call flush_(unitNumber, status)
#elif defined(PARCH_mac_osx)
      print *, "need to call flush() here"
#elif defined(PARCH_solaris)
      print *, "need to call flush() here"
#elif defined(PARCH_alpha)
      print *, "need to call flush() here"
#else
      print *, "unknown architecture in ESMF_IOFlush()"
#endif

      end subroutine ESMF_IOFlush


!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!BOP
!
!
! !INTERFACE:
      function ESMF_IOSpecCreateNew(rc)
!
! !RETURN VALUE:
      type (ESMF_IOSpec), pointer :: ESMF_IOSpecCreateNew
!
! !PARAMETERS:
      integer, intent(out), optional :: rc               ! return code

!
! !DESCRIPTION:

!
! !REQUIREMENTS: 

!EOP
      type (ESMF_IOSpec), pointer :: ios

      ALLOCATE(ios)

      ESMF_IOSpecCreateNew => ios

      end function ESMF_IOSpecCreateNew


!-------------------------------------------------------------------------
!BOP
!
!
! !INTERFACE:
      function ESMF_IOSpecCreateCopy(iospec, rc)
!
! !RETURN VALUE:
      type (ESMF_IOSpec) :: ESMF_IOSpecCreateCopy
!
! !PARAMETERS:
      type (ESMF_IOSpec), intent(in) :: iospec           ! thing to copy
      integer, intent(out), optional :: rc               ! return code

!
! !DESCRIPTION:

!
! !REQUIREMENTS: 

!EOP
      ESMF_IOSpecCreateCopy = iospec

      end function ESMF_IOSpecCreateCopy




      end module ESMF_IOMod

