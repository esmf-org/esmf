! $Id: ESMF_IOSpec.F90,v 1.16 2007/05/05 03:05:47 rosalind Exp $
!-------------------------------------------------------------------------
!
! ESMF IOSpec module
!
! This code covered by the GNU public license.  See licence file for details.
! NCAR, 2002.
!

!-------------------------------------------------------------------------
#define ESMF_FILENAME "ESMF_IOSpec.F90"
!
! !PURPOSE:
!
! The code in this file implements the IOSpec defined type
!  and functions which operate on it.  
!
! (all lines between the !BOP and !EOP markers will be included in the
! automated document processing.)
!
!
!-------------------------------------------------------------------------

! module definition

      module ESMF_IOSpecMod

!
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!BOPI
! !MODULE: ESMF_IOSpecMod
!
! !INCLUDES
#include "ESMF.h"

! !USES:
      use ESMF_UtilTypesMod    ! ESMF base class
      use ESMF_BaseMod
      use ESMF_LogErrMod
      implicit none

! !PUBLIC TYPES:
      private
    
      ! File format
      type ESMF_IOFileFormat
      sequence
      private
         integer :: iofileformat
      end type

      ! Predefined file formats
      type(ESMF_IOFileFormat), parameter :: &
                          ESMF_IO_FILEFORMAT_UNSPECIFIED=ESMF_IOFileFormat(0), &
                          ESMF_IO_FILEFORMAT_NETCDF=ESMF_IOFileFormat(1), &
                          ESMF_IO_FILEFORMAT_HDF=ESMF_IOFileFormat(2)

      ! What type of I/O - Read only, write only, R/W, append with truncation
      type ESMF_IORWType
      sequence
      private
         integer :: iorwtype
      end type

      type(ESMF_IORWType), parameter :: &
                             ESMF_IO_RWTYPE_UNSPECIFIED = ESMF_IORWType(0), &
                             ESMF_IO_RWTYPE_READONLY = ESMF_IORWType(1), &
                             ESMF_IO_RWTYPE_WRITEONLY = ESMF_IORWType(2), &
                             ESMF_IO_RWTYPE_READWRITE = ESMF_IORWType(3), &
                             ESMF_IO_RWTYPE_APPEND = ESMF_IORWType(4), &
                             ESMF_IO_RWTYPE_TRUNCATE = ESMF_IORWType(5)

      ! The combined values a user can specify.
      type ESMF_IOSpec
      sequence
      private
          type (ESMF_Status) :: iostatus
          type (ESMF_IOFileFormat) :: iofileformat
          type (ESMF_IORWType) :: iorwtype
          character(len=ESMF_MAXSTR) :: filename
          logical :: asyncIO       ! TODO: should be a derived type or enum
      end type

      ! This type captures information about what's currently being
      ! written out and is computed and updated internally - nothing in
      ! here is specified by the user.
      type ESMF_IOState
      sequence
      private
          integer :: nestlevel
          integer :: filestate   ! should be enum or status
          integer :: funit
          logical :: isopen
          logical :: define_mode
          logical :: singlefile
          logical :: parallel
          logical :: using_mpiio
      end type
          
   
! !DESCRIPTION:
!     The following routines apply to general I/O characteristics.

! !PUBLIC MEMBER TYPES:
      public ESMF_IOSpec, ESMF_IOState
      public ESMF_IOFileFormat, ESMF_IO_FILEFORMAT_UNSPECIFIED
      public ESMF_IO_FILEFORMAT_NETCDF, ESMF_IO_FILEFORMAT_HDF
      public ESMF_IORWType, ESMF_IO_RWTYPE_UNSPECIFIED
      public ESMF_IO_RWTYPE_READONLY, ESMF_IO_RWTYPE_WRITEONLY
      public ESMF_IO_RWTYPE_READWRITE, ESMF_IO_RWTYPE_APPEND
      public ESMF_IO_RWTYPE_TRUNCATE


! !PUBLIC MEMBER FUNCTIONS:

!     Temporary for putting system dependent I/O code in a single place.
      public ESMF_IOFlush
!
!     ! shallow class, only needs Get and Set
      public ESMF_IOSpecSet
      public ESMF_IOSpecGet
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
      public operator(.eq.), operator(.ne.)
      
!EOPI

!-------------------------------------------------------------------------

interface operator (.eq.)
 module procedure ESMF_iospeq
end interface

interface operator (.ne.)
 module procedure ESMF_iospne
end interface

interface operator (.eq.)
 module procedure ESMF_iorweq
end interface

interface operator (.ne.)
 module procedure ESMF_iorwne
end interface

!------------------------------------------------------------------------------

      contains

!------------------------------------------------------------------------------
! function to compare two ESMF_IOFileFormats to see if they're the same or not

function ESMF_iospeq(iosp1, iosp2)
 logical ESMF_iospeq
 type(ESMF_IOFileFormat), intent(in) :: iosp1, iosp2

 ESMF_iospeq = (iosp1%iofileformat .eq. iosp2%iofileformat)
end function

function ESMF_iospne(iosp1, iosp2)
 logical ESMF_iospne
 type(ESMF_IOFileFormat), intent(in) :: iosp1, iosp2

 ESMF_iospne = (iosp1%iofileformat .ne. iosp2%iofileformat)
end function

!------------------------------------------------------------------------------
! function to compare two ESMF_IORWTypes to see if they're the same or not

function ESMF_iorweq(iorw1, iorw2)
 logical ESMF_iorweq
 type(ESMF_IORWType), intent(in) :: iorw1, iorw2

 ESMF_iorweq = (iorw1%iorwtype .eq. iorw2%iorwtype)
end function

function ESMF_iorwne(iorw1, iorw2)
 logical ESMF_iorwne
 type(ESMF_IORWType), intent(in) :: iorw1, iorw2

 ESMF_iorwne = (iorw1%iorwtype .ne. iorw2%iorwtype)
end function

!-------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_IOFlush"
!BOPI
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

!EOPI
      integer :: status

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      status = ESMF_RC_NOT_IMPL


#if   defined(PARCH_linux)
      print *, "need to call flush() here"
#elif defined(PARCH_IRIX64)
      call flush(unitNumber, status)
#elif defined(PARCH_aix)
      call flush_(unitNumber, status)
#elif defined(PARCH_darwin)
      print *, "need to call flush() here"
#elif defined(PARCH_sunos)
      print *, "need to call flush() here"
#elif defined(PARCH_osf1)
      call flush(unitNumber, status)
#else
      print *, "unknown architecture in ESMF_IOFlush()"
#endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_IOFlush

!-------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_IOSpecGet"
!BOP
! !IROUTINE: ESMF_IOSpecGet - Get values in an IOSpec
!
! !INTERFACE:
      subroutine ESMF_IOSpecGet(iospec, filename, iofileformat, &
                                iorwtype, asyncIO, rc)
!
!
! !PARAMETERS:
      type (ESMF_IOSpec), intent(in) :: iospec
      character(len=*), intent(out), optional :: filename
      type (ESMF_IOFileFormat), intent(out), optional :: iofileformat
      type (ESMF_IORWType), intent(out), optional :: iorwtype
      logical, intent(out), optional :: asyncIO
      integer, intent(out), optional :: rc

!
! !DESCRIPTION:
!   (insert documentation here.)

!
! !REQUIREMENTS: 

!EOP

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      if (present(filename)) then
          filename = iospec%filename
      endif

      if (present(iorwtype)) then
          iorwtype = iospec%iorwtype
      endif

      if (present(iofileformat)) then
          iofileformat = iospec%iofileformat
      endif

      if (present(asyncIO)) then
          asyncIO = iospec%asyncIO
      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_IOSpecGet


!-------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_IOSpecSet - Set values in an IOSpec
!
! !INTERFACE:
      subroutine ESMF_IOSpecSet(iospec, filename, iofileformat, &
                                iorwtype, asyncIO, rc)
!
!
! !PARAMETERS:
      type (ESMF_IOSpec), intent(inout) :: iospec
      character(len=*), intent(in), optional :: filename
      type (ESMF_IOFileFormat), intent(in), optional :: iofileformat
      type (ESMF_IORWType), intent(in), optional :: iorwtype
      logical, intent(in), optional :: asyncIO
      integer, intent(out), optional :: rc

!
! !DESCRIPTION:
!   (insert documentation here.)

!
! !REQUIREMENTS: 

!EOP

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      if (present(filename)) then
          iospec%filename = filename
      else
          iospec%filename = trim("Default")   ! FIXME
      endif

      if (present(iorwtype)) then
          iospec%iorwtype = iorwtype
      else
          iospec%iorwtype = ESMF_IO_RWTYPE_UNSPECIFIED
      endif

      if (present(iofileformat)) then
          iospec%iofileformat = iofileformat
      else
          iospec%iofileformat = ESMF_IO_FILEFORMAT_UNSPECIFIED
      endif

      if (present(asyncIO)) then
          iospec%asyncIO = asyncIO
      else
          iospec%asyncIO = .false.
      endif

      iospec%iostatus = ESMF_STATUS_READY
      
      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_IOSpecSet


!-------------------------------------------------------------------------
!BOPI
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

!EOPI
      ESMF_IOSpecCreateCopy = iospec
      if (present(rc)) rc = ESMF_SUCCESS

      end function ESMF_IOSpecCreateCopy




      end module ESMF_IOSpecMod









