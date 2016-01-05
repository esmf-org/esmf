! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2016, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
#define ESMF_FILENAME "ESMF_IO.F90"
!==============================================================================
!
!     ESMF IO Module
      module ESMF_IOMod
!     
!==============================================================================
!     
! This file contains the IO class definition and all IO class methods.
!     
!------------------------------------------------------------------------------
! INCLUDES
#include "ESMF.h"

!==============================================================================
!BOPI
! !MODULE: ESMF_IOMod
!     
! !DESCRIPTION:
! Part of IO Fortran API wrapper of C++ implementation.
!
! Defines Fortran wrapper entry points for corresponding
! C++ class {\tt ESMCI\_IO} implementation.
!     
! See {\tt ../include/ESMCI\_IO.h} for complete description.  TODO ??
!
!------------------------------------------------------------------------------
! !USES:
      ! inherit from ESMF base class
      use ESMF_UtilTypesMod
      use ESMF_BaseMod
      use ESMF_InitMacrosMod
      use ESMF_LogErrMod
      use ESMF_CompMod

      ! for ReadRestart()/WriteRestart()
      ! use ESMF_IOSpecMod

      ! associated derived types
      ! use ESMF_???Mod TODO

      ! type definition for this module
      ! use ESMF_IOTypeMod  TODO ??

      implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
      private
!------------------------------------------------------------------------------

      ! File format
!      type ESMF_IOFileFormat
!      sequence
!      private
!         integer :: iofileformat
!      end type

      ! Predefined file formats
!      type(ESMF_IOFileFormat), parameter :: &
!                       ESMF_IO_FILEFORMAT_UNSPECIFIED = ESMF_IOFileFormat(0), &
!                       ESMF_IO_FILEFORMAT_NETCDF      = ESMF_IOFileFormat(1), &
!                       ESMF_IO_FILEFORMAT_HDF         = ESMF_IOFileFormat(2), &
!                       ESMF_IO_FILEFORMAT_XML         = ESMF_IOFileFormat(3)

      ! What type of I/O - Read only, write only, R/W, append with truncation
!      type ESMF_IORWType
!      sequence
!      private
!         integer :: iorwtype
!      end type

!      type(ESMF_IORWType), parameter :: &
!                             ESMF_IO_RWTYPE_UNSPECIFIED = ESMF_IORWType(0), &
!                             ESMF_IO_RWTYPE_READONLY    = ESMF_IORWType(1), &
!                             ESMF_IO_RWTYPE_WRITEONLY   = ESMF_IORWType(2), &
!                             ESMF_IO_RWTYPE_READWRITE   = ESMF_IORWType(3), &
!                             ESMF_IO_RWTYPE_APPEND      = ESMF_IORWType(4), &
!                             ESMF_IO_RWTYPE_TRUNCATE    = ESMF_IORWType(5)

      type ESMF_IO
#ifndef ESMF_NO_SEQUENCE
      sequence
#endif
      private
        type(ESMF_Pointer) :: this
        ESMF_INIT_DECLARE
      end type

!     type ESMF_IO  TODO:  define properties in C++ class
!     sequence
!     private
!         character(len=ESMF_MAXSTR) :: name
!         <object>
!         type (ESMF_IOFileFormat)   :: fileFormat
!         character(len=ESMF_MAXSTR) :: fileName
!         type (ESMF_IORWType)       :: readWriteType
!         character(len=ESMF_MAXSTR) :: convention
!         character(len=ESMF_MAXSTR) :: purpose
!     end type

!------------------------------------------------------------------------------
! !PUBLIC TYPES:
      public ESMF_IO
      !public ESMF_IO, ESMF_IOFileFormat, ESMF_IORWType
!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:
      !public ESMF_IOClose  TODO ??
      public ESMF_IOCreate
      public ESMF_IODestroy 
      !public ESMF_IOFlush 
      !public ESMF_IOGet     
      !public ESMF_IOOpen  TODO ??     
      !public ESMF_IOPrint
      public ESMF_IORead
      !public ESMF_IOReadRestart
      !public ESMF_IOSet
      !public ESMF_IOValidate
      !public ESMF_IOWrite
      !public ESMF_IOWriteRestart
!EOPI

! !PRIVATE MEMBER FUNCTIONS:
      !private ESMF_IOCreateNew  TODO ??
      !private ESMF_IOCreateCopy  TODO ??

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id$'

!==============================================================================
!
! INTERFACE BLOCKS
!
!==============================================================================
! TODO ??
!==============================================================================

       contains

!==============================================================================
!BOPI
! !IROUTINE: ESMF_IOCreate - Create a new ESMF IO object
!
! !INTERFACE:
!     function ESMF_IOCreate(name, <object>, fileFormat, fileName, &
!                            readWritetype, convention, purpose, rc)
!
! !RETURN VALUE:
!     type(ESMF_IO) :: ESMF_IOCreate
!
! !ARGUMENTS:
!     character (len=*),       intent(in),  optional :: name
!     <object>, see below for supported values
!     type(ESMF_IOFileFormat), intent(in)            :: fileFormat
!     character (len=*),       intent(in),  optional :: fileName
!     type (ESMF_IORWType),    intent(in),  optional :: readWriteType
!     character (len=*),       intent(in),  optional :: convention
!     character (len=*),       intent(in),  optional :: purpose
!     integer,                 intent(out), optional :: rc
!   
! !DESCRIPTION:
!     Creates and sets the initial values in a new {\tt ESMF\_IO} object.    
!
!     Supported values for <object> are:
!     \begin{description}
!     \item type(ESMF\_Array),       intent(inout) :: array
!     \item type(ESMF\_CplComp),     intent(inout) :: comp
!     \item type(ESMF\_GridComp),    intent(inout) :: comp
!     \item type(ESMF\_Field),       intent(inout) :: field
!     \item type(ESMF\_FieldBundle), intent(inout) :: fbundle
!     \item type(ESMF\_Grid),        intent(inout) :: grid
!     \item type(ESMF\_State),       intent(inout) :: state
!     \end{description}
!
!     The arguments are:
!     \begin{description}
!     \item[{[name]}]     
!          The name for the newly created io object.  If not specified, a
!          default unique name will be generated: "IONNN" where NNN
!          is a unique sequence number from 001 to 999.
!     \item[<object>]
!           An {\tt ESMF} object.  See above for supported values.
!     \item[fileFormat]
!          The file format to be used by the {\tt ESMF\_IO} object.
!     \item[{[fileName]}]
!          The file name to be used by the {\tt ESMF\_IO} object.  If a path
!          is not specified, it will default to
!          ESMF\_DIR/<object\_class>/etc/<fileName>.
!          If fileName is not specified at all, and fileFormat is
!          {\tt ESMF\_IO\_FILEFORMAT\_XML}, and convention is CF, it will
!          default to
!          ESMF\_DIR/<object\_class>/etc/ESMF\_<object\_name><convention><purpose>.xml
!          If fileName is not specified at all, and fileFormat is
!          {\tt ESMF\_IO\_FILEFORMAT\_XML}, and convention is ESG, it will
!          default to
!          ESMF\_DIR/<object\_class>/etc/<object\_name><convention><purpose>.xml
!     \item[{[readWriteType]}]
!          The {\tt ESMF\_IO} object's read/write type.
!     \item[{[convention]}]
!          The convention of an Attribute package for the given ESMF <object>.
!     \item[{[purpose]}]
!          The purpose of an Attribute package for the given ESMF <object>.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!     
!EOPI
! !REQUIREMENTS:
!     IOx.y, IOx.y.z  TODO
!
!     ! initialize name length to zero for non-existent name
!     integer :: nameLen, localrc
!
!     ! Assume failure until success
!     if (present(rc)) rc = ESMF_RC_NOT_IMPL
!     localrc = ESMF_RC_NOT_IMPL
!
!     TODO:  init checks
!
!     nameLen = 0
!
!     ! get length of given name for C++ validation
!     if (present(name)) then
!       nameLen = len_trim(name)
!     end if
!
!     invoke C to C++ entry point to allocate and initialize new io
!     call c_ESMC_IOCreate(ESMF_IOCreate, nameLen, name, &
!                          <object>, fileFormat, fileName, readWriteType, &
!                          convention, purpose, localrc)
!     if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
!         ESMF_CONTEXT, rcToReturn=rc)) return
!
!     call ESMF_IOSetInitCreated(ESMF_IOCreate)
!
!     ! Return success
!     if (present(rc)) rc = ESMF_SUCCESS
!     end function ESMF_IOCreate

!==============================================================================
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_IOCreate()"
!BOPI
! !IROUTINE: ESMF_IOCreate - Create a new ESMF IO object for an ESMF GridComp
!
! !INTERFACE:
      function ESMF_IOCreate(name, gridComp, rc)
!
! !RETURN VALUE:
      type(ESMF_IO) :: ESMF_IOCreate
!
! !ARGUMENTS:
      character (len=*),       intent(in),  optional :: name
      type(ESMF_GridComp),     intent(inout)         :: gridComp
      integer,                 intent(out), optional :: rc
!   
! !DESCRIPTION:
!     Creates and sets the initial values in a new {\tt ESMF\_IO} object.    
!
!     The arguments are:
!     \begin{description}
!     \item[{[name]}]     
!          The name for the newly created io object.  If not specified, a
!          default unique name will be generated: "IONNN" where NNN
!          is a unique sequence number from 001 to 999.
!     \item[gridComp]
!           An {\tt ESMF\GridComp} object.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!     
!EOPI
! !REQUIREMENTS:
!     IOx.y, IOx.y.z  TODO
!
!     ! initialize name length to zero for non-existent name
      integer :: nameLen, localrc
!
!     ! Assume failure until success
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      localrc = ESMF_RC_NOT_IMPL
!
!     TODO:  init checks
!
      nameLen = 0
!
!     ! get length of given name for C++ validation
      if (present(name)) then
        nameLen = len_trim(name)
      end if
!
!     invoke C to C++ entry point to allocate and initialize new io
      call c_ESMC_IO_XMLCreate(ESMF_IOCreate, nameLen, name, 0, "", &
                               gridComp%compp%base, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
!
!     call ESMF_IOSetInitCreated(ESMF_IOCreate)  TODO
!
!     ! Return success
      if (present(rc)) rc = ESMF_SUCCESS
      end function ESMF_IOCreate

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_IODestroy()"
!BOPI
! !IROUTINE: ESMF_IODestroy - Release resources associated with an ESMF IO object
!
! !INTERFACE:
      subroutine ESMF_IODestroy(io, rc)
!
! !ARGUMENTS:
      type(ESMF_IO)                  :: io
      integer, intent(out), optional :: rc
!     
! !DESCRIPTION:
!     Releases resources associated with this {\tt ESMF\_IO} object.
!
!     The arguments are:
!     \begin{description}
!     \item[io]
!       Destroy contents of this {\tt ESMF\_IO} object.
!     \item[[rc]]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS:
!     IOx.y, IOx.y.z  TODO
!
      integer :: localrc                        ! local return code
!
!     ! Assume failure until success
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      localrc = ESMF_RC_NOT_IMPL
!
!     ! check inputs
!     ESMF_INIT_CHECK_DEEP(ESMF_IOGetInit,io,rc)  TODO
!
!     invoke C to C++ entry point
      call c_ESMC_IO_XMLDestroy(io, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) then 
!       ! Don't bail out until Delete
        write(*,*)" c_ESMC_IODestroy fails"
      endif
!
!     call ESMF_IOSetInitDeleted(io)
!
!     ! Return success
      if (present(rc)) rc = localrc
      end subroutine ESMF_IODestroy

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_IOGet - Get an ESMF IO object's properties
!
! !INTERFACE:
!     subroutine ESMF_IOGet(io, name, <object>, fileFormat, fileName, &
!                           readWriteType, convention, purpose, rc)
!
! !ARGUMENTS:
!     type(ESMF_IO),           intent(in)            :: io
!     character (len=*),       intent(out), optional :: name
!     <object>, see below for supported values
!     type(ESMF_IOFileFormat), intent(out), optional :: fileFormat
!     character (len=*),       intent(out), optional :: fileName
!     type (ESMF_IORWType),    intent(out), optional :: readWriteType
!     character (len=*),       intent(out), optional :: convention
!     character (len=*),       intent(out), optional :: purpose
!     integer,                 intent(out), optional :: rc
!   
! !DESCRIPTION:
!     Gets one or more of the properties of an {\tt ESMF\_IO} object.
!
!     Supported values for <object> are:
!     \begin{description}
!     \item type(ESMF\_Array),       intent(out) :: array
!     \item type(ESMF\_CplComp),     intent(out) :: comp
!     \item type(ESMF\_GridComp),    intent(out) :: comp
!     \item type(ESMF\_Field),       intent(out) :: field
!     \item type(ESMF\_FieldBundle), intent(out) :: fbundle
!     \item type(ESMF\_Grid),        intent(out) :: grid
!     \item type(ESMF\_State),       intent(out) :: state
!     \end{description}
!
!     The arguments are:
!     \begin{description}
!     \item[io]
!          The {\tt ESMF\_IO} object instance to query.
!     \item[{[name]}]
!          The name of this io object.
!     \item[fileFormat]
!          The file format of this io object.
!     \item[{[fileName]}]
!          The file name of this io object.
!     \item[{[readWriteType]}]
!          The read/write type of this io object.
!     \item[{[convention]}]
!          The convention of the Attribute package for <object>.
!     \item[{[purpose]}]
!          The purpose of the Attribute package for <object>.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS:
!     IOx.y, IOx.y.z  TODO
!
!     ! temp name for C++ to fill
!     character (len=ESMF_MAXSTR) :: tempName
!
!     ! initialize name lengths to zero for non-existent name
!     integer :: nameLen, tempNameLen
!     integer :: localrc                        ! local return code
!
!     ! Assume failure until success
!     if (present(rc)) rc = ESMF_RC_NOT_IMPL
!     localrc = ESMF_RC_NOT_IMPL
!
!     nameLen = 0
!     tempNameLen = 0
!
!     ! check variables  TODO
!     ESMF_INIT_CHECK_DEEP(ESMF_IOGetInit,io,rc)
!
!     ! get length of given name for C++ validation
!     if (present(name)) then
!       nameLen = len(name)
!     end if
!
!     invoke C to C++ entry point
!     call c_ESMC_IOGet(io, nameLen, tempNameLen, tempName, &
!                       <object>, fileFormat, fileName, &
!                       readWriteType, convention, purpose, localrc)
!     if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
!       ESMF_CONTEXT, rcToReturn=rc)) return
!
!     ! copy temp name back to given name to restore native Fortran
!     !   storage style
!     if (present(name)) then
!       name = tempName(1:tempNameLen)
!     endif
!    
!     ! Return success
!     if (present(rc)) rc = ESMF_SUCCESS
!     end subroutine ESMF_IOGet
!
!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_IOPrint()"
!BOPI
! !IROUTINE:  ESMF_IOPrint - Print the contents of a ESMF IO object
!
! !INTERFACE:
!     subroutine ESMF_IOPrint(io, options, rc)
!
! !ARGUMENTS:
!     type(ESMF_IO),     intent(in)            :: io
!     character (len=*), intent(in),  optional :: options
!     integer,           intent(out), optional :: rc
!
! !DESCRIPTION:
!     Prints out an {\tt ESMF\_IO}'s properties to {\tt stdout}, in
!     support of testing and debugging.  The options control the type of
!     information and level of detail. \\
! 
!     Note:  Many {\tt ESMF\_<class>Print} methods are implemented in C++.
!     On some platforms/compilers there is a potential issue with interleaving
!     Fortran and C++ output to {\tt stdout} such that it doesn't appear in
!     the expected order.  If this occurs, it is recommended to use the
!     standard Fortran call {\tt flush(6)} as a workaround until this issue
!     is fixed in a future release. \\
!
!     The arguments are:
!     \begin{description}
!     \item[io]
!          {\tt ESMF\_IO} object to be printed out.
!     \item[{[options]}]
!          Print options. If none specified, prints all {\tt io} property
!          values.\\
!          "convention"    - print the io's object's attribute convention. \\
!          "fileFormat"    - print the io's file format. \\
!          "fileName"      - print the io's file name. \\
!          "name"          - print the io's name. \\
!          "object"        - print the io's associated ESMF object type and
!                            name. \\
!          "purpose"       - print the io's object's attribute purpose. \\
!          "readWriteType" - print the io's read/write type. \\
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS:
!     IOx.y, IOx.y.z  TODO
!
!     integer :: localrc                        ! local return code
!
!     ! Assume failure until success
!     if (present(rc)) rc = ESMF_RC_NOT_IMPL
!     localrc = ESMF_RC_NOT_IMPL
!      
!     ! check variables TODO
!     ESMF_INIT_CHECK_DEEP(ESMF_IOGetInit,io,rc)
!
!     invoke C to C++ entry point
!     call c_ESMC_IOPrint(io, options, localrc)  TODO
!     if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
!       ESMF_CONTEXT, rcToReturn=rc)) return
!
!     ! Return success
!     if (present(rc)) rc = ESMF_SUCCESS
!     end subroutine ESMF_IOPrint
!
!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_IORead - Perform a read on an ESMF IO object
!
! !INTERFACE:
!     subroutine ESMF_IORead(io, <object>, fileFormat, fileName, &
!                            schemaFileName, readWriteType, &
!                            convention, purpose, rc)
!
! TODO derived types, other than <object>, need to be inout?
! !ARGUMENTS:
!     type(ESMF_IO),           intent(in)            :: io
!     <object>, see below for supported values
!     type(ESMF_IOFileFormat), intent(in)            :: fileFormat
!     character (len=*),       intent(in),  optional :: fileName
!     character (len=*),       intent(in),  optional :: schemaFileName
!     type (ESMF_IORWType),    intent(in),  optional :: readWriteType
!     character (len=*),       intent(in),  optional :: convention
!     character (len=*),       intent(in),  optional :: purpose
!     integer,                 intent(out), optional :: rc
!   
! !DESCRIPTION:
!     Perform a read on an {\tt ESMF\_IO} object.  Any properties specified
!     will override, but not reset, those previously set on the io object.
!
!     Supported values for <object> are:
!     \begin{description}
!     \item type(ESMF\_Array),       intent(inout), optional :: array
!     \item type(ESMF\_CplComp),     intent(inout), optional :: comp
!     \item type(ESMF\_GridComp),    intent(inout), optional :: comp
!     \item type(ESMF\_Field),       intent(inout), optional :: field
!     \item type(ESMF\_FieldBundle), intent(inout), optional :: fbundle
!     \item type(ESMF\_Grid),        intent(inout), optional :: grid
!     \item type(ESMF\_State),       intent(inout), optional :: state
!     \end{description}
!
!     The arguments are:
!     \begin{description}
!     \item[io]
!          The object instance to read.
!     \item[{[<object>]}]
!           An {\tt ESMF} object for which read data is destined.
!           See above for supported values.
!     \item[{[fileFormat]}]
!          The file format to be used during the read.
!     \item[{[fileName]}]
!          The file name to be read from.
!     \item[{[schemaFileName]}]
!          The file name of the schema used to validate fileName.
!     \item[{[readWriteType]}]
!          The read type to be used during the read.  Write types, other than
!          {\tt ESMF\_IO\_RWTYPE\_READWRITE}, are invalid.           
!     \item[{[convention]}]
!          Selects, for reading, the Attribute package of the given ESMF
!          <object> which matches this convention.
!     \item[{[purpose]}]
!          Selects, for reading, the Attribute package of the given ESMF
!          <object> which matches this purpose.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!EOPI
! !REQUIREMENTS:
!     IOx.y, IOx.y.z  TODO
!
!     integer :: localrc
!
!     ! Assume failure until success
!     if (present(rc)) rc = ESMF_RC_NOT_IMPL
!     localrc = ESMF_RC_NOT_IMPL
!
!     ! check variables  TODO
!     ESMF_INIT_CHECK_DEEP(ESMF_IOGetInit,io,rc)
!
!     invoke C to C++ entry point  TODO
!     call c_ESMC_IORead(io, <object>, fileFormat, fileName, schemaFileName, &
!                       readWriteType, convention, purpose, localrc)
!     if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
!       ESMF_CONTEXT, rcToReturn=rc)) return
!
!     ! Return success
!     if (present(rc)) rc = ESMF_SUCCESS
!     end subroutine ESMF_IORead
!
!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_IORead()"
!BOPI
! !IROUTINE: ESMF_IORead - Perform a read on an ESMF IO object
!
! !INTERFACE:
      subroutine ESMF_IORead(io, fileName, schemaFileName, rc)
!
! TODO derived types, other than <object>, need to be inout?
! !ARGUMENTS:
      type(ESMF_IO),           intent(in)            :: io
      character (len=*),       intent(in),  optional :: fileName
      character (len=*),       intent(in),  optional :: schemaFileName
      integer,                 intent(out), optional :: rc
!   
! !DESCRIPTION:
!     Perform a read on an {\tt ESMF\_IO} object.  Any properties specified
!     will override, but not reset, those previously set on the io object.
!
!     The arguments are:
!     \begin{description}
!     \item[io]
!          The object instance to read.
!     \item[{[fileName]}]
!          The file name to be read from.
!     \item[{[schemaFileName]}]
!          The file name of the schema used to validate fileName.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!EOPI
! !REQUIREMENTS:
!     IOx.y, IOx.y.z  TODO
!
!     ! initialize fileName length to zero for non-existent name
      integer :: fileNameLen, schemaFileNameLen, localrc
!
!     ! Assume failure until success
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      localrc = ESMF_RC_NOT_IMPL
!
!     ! check variables  TODO
!     ESMF_INIT_CHECK_DEEP(ESMF_IOGetInit,io,rc)

      fileNameLen = 0
      schemaFileNameLen = 0
!
!     ! get length of given fileName for C++ validation
      if (present(fileName)) then
        fileNameLen = len_trim(fileName)
      end if
      if (present(schemaFileName)) then
        schemaFileNameLen = len_trim(schemaFileName)
      end if

!     invoke C to C++ entry point  TODO
      call c_ESMC_IO_XMLRead(io, fileNameLen, fileName, &
                             schemaFileNameLen, schemaFileName, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
!
!     ! Return success
      if (present(rc)) rc = ESMF_SUCCESS
      end subroutine ESMF_IORead
!
!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_IOReadRestart()"
!BOPI
! !IROUTINE: ESMF_IOReadRestart - Restore the contents of a IO (not implemented)
! TODO
! !INTERFACE:
!     function ESMF_IOReadRestart(name, iospec, rc)
!
! !RETURN VALUE:
!     type(ESMF_IO) :: ESMF_IOReadRestart
!
! !ARGUMENTS:
!     character (len=*), intent(in)            :: name
!     type(ESMF_IOSpec), intent(in),  optional :: iospec
!     integer,           intent(out), optional :: rc
!
! !DESCRIPTION:
!     Restores an {\tt ESMF\_IO} object from the last call to
!     {\tt ESMF\_IOWriteRestart()}.  (Not implemented yet).
!
!     The arguments are:
!     \begin{description}
!     \item[name]
!          The name of the object instance to restore.
!     \item[{[iospec]}]      
!          The IO specification of the restart file.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}  
!
!EOPI
! !REQUIREMENTS:
!     IOx.y, IOx.y.z  TODO
!
!     ! get length of given name for C++ validation
!     integer :: nameLen, localrc
!
!     ! Assume failure until success
!     if (present(rc)) rc = ESMF_RC_NOT_IMPL
!     localrc = ESMF_RC_NOT_IMPL
!
!     nameLen = len_trim(name)
!
!     invoke C to C++ entry point to allocate and restore io   TODO
!     call c_ESMC_IOReadRestart(ESMF_IOReadRestart, nameLen, name, &
!                               iospec, localrc)
!     if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
!       ESMF_CONTEXT, rcToReturn=rc)) return
!
!     call ESMF_IOSetInitCreated(ESMF_IOReadRestart)
!
!     ! Return success
!     if (present(rc)) rc = ESMF_SUCCESS
!     end function ESMF_IOReadRestart
!
!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_IOSet - Set one or more properties of an ESMF IO object
!
! !INTERFACE:
!     subroutine ESMF_IOSet(io, name, <object>, fileFormat, fileName, &
!                           readWriteType, convention, purpose, rc)
!
! TODO derived types, other than <object>, need to be inout?
! !ARGUMENTS:
!     type(ESMF_IO),           intent(in)            :: io
!     character (len=*),       intent(in),  optional :: name
!     <object>, see below for supported values
!     type(ESMF_IOFileFormat), intent(in)            :: fileFormat
!     character (len=*),       intent(in),  optional :: fileName
!     type (ESMF_IORWType),    intent(in),  optional :: readWriteType
!     character (len=*),       intent(in),  optional :: convention
!     character (len=*),       intent(in),  optional :: purpose
!     integer,                 intent(out), optional :: rc
!   
! !DESCRIPTION:
!     Sets/resets one or more of the properties of an {\tt ESMF\_IO} object that
!     was previously initialized via {\tt ESMF\_IOCreate()}.
!
!     Supported values for <object> are:
!     \begin{description}
!     \item type(ESMF\_Array),       intent(inout), optional :: array
!     \item type(ESMF\_CplComp),     intent(inout), optional :: comp
!     \item type(ESMF\_GridComp),    intent(inout), optional :: comp
!     \item type(ESMF\_Field),       intent(inout), optional :: field
!     \item type(ESMF\_FieldBundle), intent(inout), optional :: fbundle
!     \item type(ESMF\_Grid),        intent(inout), optional :: grid
!     \item type(ESMF\_State),       intent(inout), optional :: state
!     \end{description}
!
!     The arguments are:
!     \begin{description}
!     \item[io]
!          The object instance to set.
!     \item[{[name]}]     
!          The new name for this io object.
!     \item[{[<object>]}]
!           An {\tt ESMF} object.  See above for supported values.
!     \item[{[fileFormat]}]
!          The file format to be used by the {\tt ESMF\_IO} object.
!     \item[{[fileName]}]
!          The file name to be used by the {\tt ESMF\_IO} object.  If a path
!          is not specified, it will default to
!          ESMF\_DIR/<object\_class>/etc/<fileName>.
!     \item[{[readWriteType]}]
!          The {\tt ESMF\_IO} object's read/write type.
!     \item[{[convention]}]
!          The convention of an Attribute package for the given ESMF <object>.
!     \item[{[purpose]}]
!          The purpose of an Attribute package for the given ESMF <object>.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!EOPI
! !REQUIREMENTS:
!     IOx.y, IOx.y.z  TODO
!
!     ! initialize name length to zero for non-existent name
!     integer :: nameLen, localrc
!
!     ! Assume failure until success
!     if (present(rc)) rc = ESMF_RC_NOT_IMPL
!     localrc = ESMF_RC_NOT_IMPL
!
!     nameLen = 0
!
!     ! check variables  TODO
!     ESMF_INIT_CHECK_DEEP(ESMF_IOGetInit,io,rc)
!
!     ! get length of given name for C++ validation
!     if (present(name)) then
!       nameLen = len_trim(name)
!     end if
!
!     invoke C to C++ entry point  TODO
!     call c_ESMC_IOSet(io, nameLen, name, <object>, fileFormat, fileName, &
!                       readWriteType, convention, purpose, localrc)
!     if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
!       ESMF_CONTEXT, rcToReturn=rc)) return
!
!     ! Return success
!     if (present(rc)) rc = ESMF_SUCCESS
!     end subroutine ESMF_IOSet
!
!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_IOValidate()"
!BOPI
! !IROUTINE:  ESMF_IOValidate - Validate an ESMF IO object's properties
!
! !INTERFACE:
!     subroutine ESMF_IOValidate(io, options, rc)
!
! !ARGUMENTS:
!     type(ESMF_IO),     intent(inout)         :: io
!     character (len=*), intent(in),  optional :: options
!     integer,           intent(out), optional :: rc
!
! !DESCRIPTION:
!     Checks whether an {\tt io} object is valid.   TODO: determine validity rules
!
!     The arguments are:  
!     \begin{description}
!     \item[io]
!          {\tt ESMF\_IO} object to be validated.
!     \item[{[options]}]
!          Validation options are not yet supported.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description} 
!
!EOPI
! !REQUIREMENTS:
!     IOx.y, IOx.y.z  TODO
!
!     integer :: localrc                        ! local return code
!
!     ! Assume failure until success
!     if (present(rc)) rc = ESMF_RC_NOT_IMPL
!     localrc = ESMF_RC_NOT_IMPL
! 
!     ! check variables  TODO
!     ESMF_INIT_CHECK_DEEP(ESMF_IOGetInit,io,rc)
!
!     invoke C to C++ entry point
!     call c_ESMC_IOValidate(io, options, localrc)   TODO
!     if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
!       ESMF_CONTEXT, rcToReturn=rc)) return
!
!     ! Return success
!     if (present(rc)) rc = ESMF_SUCCESS 
!     end subroutine ESMF_IOValidate
!
!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_IOWrite - Perform a write on an ESMF IO object
!
! !INTERFACE:
!     subroutine ESMF_IOWrite(io, <object>, fileFormat, fileName, &
!                             readWriteType, convention, purpose, rc)
!
! TODO derived types, other than <object>, need to be inout?
! !ARGUMENTS:
!     type(ESMF_IO),           intent(in)            :: io
!     <object>, see below for supported values
!     type(ESMF_IOFileFormat), intent(in)            :: fileFormat
!     character (len=*),       intent(in),  optional :: fileName
!     type (ESMF_IORWType),    intent(in),  optional :: readWriteType
!     character (len=*),       intent(in),  optional :: convention
!     character (len=*),       intent(in),  optional :: purpose
!     integer,                 intent(out), optional :: rc
!   
! !DESCRIPTION:
!     Perform a write on an {\tt ESMF\_IO} object.  Any properties specified
!     will override, but not reset, those previously set on the io object.
!
!     Supported values for <object> are:
!     \begin{description}
!     \item type(ESMF\_Array),       intent(in), optional :: array
!     \item type(ESMF\_CplComp),     intent(in), optional :: comp
!     \item type(ESMF\_GridComp),    intent(in), optional :: comp
!     \item type(ESMF\_Field),       intent(in), optional :: field
!     \item type(ESMF\_FieldBundle), intent(in), optional :: fbundle
!     \item type(ESMF\_Grid),        intent(in), optional :: grid
!     \item type(ESMF\_State),       intent(in), optional :: state
!     \end{description}
!
!     The arguments are:
!     \begin{description}
!     \item[io]
!          The object instance to write.
!     \item[{[<object>]}]
!           An {\tt ESMF} object data source from which data is written.
!           See above for supported values.
!     \item[{[fileFormat]}]
!          The file format to be used during the write.
!     \item[{[fileName]}]
!          The file name to be writtten to.
!     \item[{[readWriteType]}]
!          The read type to be used during the write.  Read types, other than
!          {\tt ESMF\_IO\_RWTYPE\_READWRITE}, are invalid.           
!     \item[{[convention]}]
!          Selects, for writing, the Attribute package of the given ESMF
!          <object> which matches this convention.
!     \item[{[purpose]}]
!          Selects, for writing, the Attribute package of the given ESMF
!          <object> which matches this purpose.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!EOPI
! !REQUIREMENTS:
!     IOx.y, IOx.y.z  TODO
!
!     integer :: localrc
!
!     ! Assume failure until success
!     if (present(rc)) rc = ESMF_RC_NOT_IMPL
!     localrc = ESMF_RC_NOT_IMPL
!
!     ! check variables  TODO
!     ESMF_INIT_CHECK_DEEP(ESMF_IOGetInit,io,rc)
!
!     invoke C to C++ entry point  TODO
!     call c_ESMC_IOWrite(io, <object>, fileFormat, fileName, &
!                         readWriteType, convention, purpose, localrc)
!     if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
!       ESMF_CONTEXT, rcToReturn=rc)) return
!
!     ! Return success
!     if (present(rc)) rc = ESMF_SUCCESS
!     end subroutine ESMF_IOWrite
!
!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_IOWriteRestart()"
!BOPI
! !IROUTINE: ESMF_IOWriteRestart - Save the contents of an ESMF IO object (not implemented)
!
! !INTERFACE:
!     subroutine ESMF_IOWriteRestart(io, iospec, rc)
!
! !ARGUMENTS:
!     type(ESMF_IO),     intent(inout)         :: io
!     type(ESMF_IOSpec), intent(in),  optional :: iospec
!     integer,           intent(out), optional :: rc
!
! !DESCRIPTION:
!     Saves an {\tt ESMF\_IO} object.  Default options are to select the
!     fastest way to save to disk.  (Not implemented yet).
!
!     The arguments are:
!     \begin{description}
!     \item[io]
!          The object instance to save.
!     \item[{[iospec]}]
!          The IO specification of the restart file.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS:
!     integer :: localrc                        ! local return code
!
!     ! Assume failure until success
!     if (present(rc)) rc = ESMF_RC_NOT_IMPL
!     localrc = ESMF_RC_NOT_IMPL
!
!     ! check variables  TODO
!     ESMF_INIT_CHECK_DEEP(ESMF_IOGetInit,io,rc)
!
!     invoke C to C++ entry point
!     call c_ESMC_IOWriteRestart(io, iospec, localrc)  TODO
!     if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
!       ESMF_CONTEXT, rcToReturn=rc)) return
!
!     ! Return success
!     if (present(rc)) rc = ESMF_SUCCESS
!     end subroutine ESMF_IOWriteRestart
!
!------------------------------------------------------------------------------

      end module ESMF_IOMod
