! $Id: ESMF_Config.F90,v 1.16 2004/08/19 16:52:19 nscollins Exp $
!==============================================================================
! Earth System Modeling Framework
!
! Copyright 2002-2003, University Corporation for Atmospheric Research, 
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
! Laboratory, University of Michigan, National Centers for Environmental 
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
! NASA Goddard Space Flight Center.
! Licensed under the GPL.
!==============================================================================
#define ESMF_FILENAME "ESMF_Config.F90"
!
!     ESMF Configuration module
      module ESMF_ConfigMod
!
#include "ESMF.h"
!==============================================================================
!
! This file contains the Config class definition and all Config
! class methods.
!
!------------------------------------------------------------------------------
!
!BOPI
! !MODULE: ESMF_ConfigMod - Implements ESMF configuration management
!
!
! !DESCRIPTION:
!
! The code in this file implements the {\tt ESMF\_Config} class that implements 
! ESMF configuration management system.
!
! \subsubsection{Package Overview}
!
!      ESMF Configuration Management is based on NASA DAO's 
!      Inpak 90 package, a Fortran 90 collection of routines/functions
!      for accessing {\em Resource Files} in ASCII format.The package 
!      is optimized for minimizing formatted I/O, performing all of its 
!      string operations in memory using Fortran intrinsic functions.\\
!
!      Module ${\tt ESMF\_ConfigMod}$ is implemented in Fortran 90.
!
! \subsubsection{Resource Files}
!
!      A {\em Resource File} is a text file consisting of variable
!     length lines (records), each possibly starting with a {\em label}
!     (or {\em key}), followed by some data. A simple resource file 
!     looks like this:
!
! \begin{verbatim}
! # Lines starting with # are comments which are
! # ignored during processing.
! my_file_names:         jan87.dat jan88.dat jan89.dat
! radius_of_the_earth:   6.37E6  # these are comments too
! constants:             3.1415   25
! my_favourite_colors:   green blue 022 # text & number are OK
! \end{verbatim}
!
!    In this example, {\tt my\_file\_names:} and {\tt constants:}
!    are labels, while {\tt jan87.dat, jan88.dat} and {\tt jan89.dat} are
!    data associated with label {\tt my\_file\_names:}.
!    Resource files can also contain simple tables of the form,
!
! \begin{verbatim}
! my_table_name::
!  1000     3000     263.0   
!   925     3000     263.0
!   850     3000     263.0
!   700     3000     269.0
!   500     3000     287.0
!   400     3000     295.8
!   300     3000     295.8    
! ::
! \end{verbatim}
!
! Resource files are intended for random access (except between ::'s in a 
! table definition). Normally, the order of records should not be important. 
! However, the order of records may be important if the same label appears 
! multiple times.
!
!    \subsubsection{A Quick Stroll}
!
!    The first step is to create the ESMF Configuration and load the 
!    ASCII resource (rc) file into memory\footnote{See next section 
!    for a complete description of parameters for each routine/function}:
!
! \begin{verbatim}
!       config = ESMF_ConfigCreate ( rc)
!       call ESMF_ConfigLoadFile (config, filename, delayout, rc = rc)
! \end{verbatim}
!
!    Parameter {\tt layout} is optional. If it is passed, multiprocessor
!    performance is optimized. Otherwise, resource file {\tt filename} is
!    read by each processor.
!
!    The next step is to select the label (record) of interest, say
!
! \begin{verbatim}
!       call ESMF_ConfigFindLabel  ( config, 'constants:', rc = rc )
! \end{verbatim}
!
!  The 2 constants above can be retrieved with the following code
!  fragment:
! \begin{verbatim}
!       real    r
!       integer i
!       call {\tt ESMF\_ConfigFindLabel( config, 'constants:', rc = rc)}
!       call {\tt ESMF\_ConfigGetFloat( config, r, rc = rc )}       ! results in r = 3.1415
!       call {\tt ESMF\_ConfigGetInt( config, i, rc = rc )}         ! results in i = 25
! \end{verbatim}
!
!  The file names above can be retrieved with the following
!  code fragment:
! \begin{verbatim}
!       character*20 fn1, fn2, fn3
!       integer      rc
!       call {\tt ESMF\_ConfigFindLabel ( config, 'my_file_names:', rc = rc )}
!       call {\tt ESMF\_ConfigGetString ( config, fn1, rc = rc )}  ! ==> fn1 = 'jan87.dat'
!       call {\tt ESMF\_ConfigGetString ( config, fn2, rc = rc )}  ! ==> fn1 = 'jan88.dat'
!       call {\tt ESMF\_ConfigGetString ( config, fn3, rc = rc )}  ! ==> fn1 = 'jan89.dat'
! \end{verbatim}
!
! To access the table above, the user first must use 
! ${\tt ESMF\_ConfigFindLabel()}$ to locate the beginning of the table, e.g.,
!
! \begin{verbatim}
!       call {\tt ESMF\_ConfigFindLabel(config, 'my_table_name::', rc = rc)}
! \end{verbatim}
!
! Subsequently, ${\tt call ESMF\_ConfigNextLine()}$ can be used to gain 
! access to each row of the table. Here is a code fragment to read the above
! table (7 rows, 3 columns):
!
! \begin{verbatim}
!       real          table(7,3)
!       character*20  word
!       integer       rc
!       call  {\tt ESMF\_ConfigFindLabel(config, 'my_table_name::', rc = rc)}
!       do i = 1, 7
!          call {\tt ESMF\_ConfigNextLine( config, rc = rc )
!          do j = 1, 3
!             call ESMF_ConfigGetFloat( config, table(i, j), rc = rc )
!          end do                   
!       end do
! \end{verbatim}
!
! The work with the configuration {\tt config} is finalized by call to
! ${\tt ESMF\_ConfigDestroy()}$:
! \begin{verbatim}
!       integer rc
!       call {\tt ESMF\_ConfigDestroy( config, rc )
! \end{verbatim}
!
! {\em Common Arguments:}
!
! \begin{verbatim}
! character*(*) ::    filename    file name
! integer       ::    rc          error return code (0 is OK)
! character*(*) ::    label       label (key) to locate record
! character*(*) ::    word        blank delimited string
! character*(*) ::    string      a sequence of characters
! \end{verbatim}
!
! See the Prologues in the next section for additional details.
!
!
!    \subsubsection{Package History}
!       The ESMF Configuration Management Package was evolved by 
!       Leonid Zaslavsky and Arlindo da Silva from Ipack90 package
!       created by Arlindo da Silva at NASA DAO.
!
!       Back in the 70's Eli Isaacson wrote IOPACK in Fortran
!       66.  In June of 1987 Arlindo da Silva wrote Inpak77 using
!       Fortran 77 string functions; Inpak 77 is a vastly
!       simplified IOPACK, but has its own goodies not found in
!       IOPACK.  Inpak 90 removes some obsolete functionality in
!       Inpak77, and parses the whole resource file in memory for
!       performance. 
!
! !REVISION HISTORY:
!
!       2apr2003 Leonid Zaslavsky Created from m_inpak90.F90
!       1may2003 Leonid Zaslavsky Corrected version 
! !USES:

      use ESMF_BaseTypesMod    ! ESMF base class
      use ESMF_BaseMod
      use ESMF_DELayoutMod
      use ESMF_LogErrMod 

      implicit none
      private
!------------------------------------------------------------------------------
! !PUBLIC MEMBER FUNCTIONS:
!------------------------------------------------------------------------------
       public :: ESMF_ConfigCreate     ! creates configuration
       public :: ESMF_ConfigDestroy    ! destroys configuration
       public :: ESMF_ConfigLoadFile   ! loads resource file into memory
       public :: ESMF_ConfigFindLabel  ! selects a label (key)
       public :: ESMF_ConfigNextLine   ! selects next line (for tables)
       public :: ESMF_ConfigGetAttribute ! returns next value
       public :: ESMF_ConfigGetChar    ! returns only a single character
       public :: ESMF_ConfigGetLen ! gets number of words in the line(funcion)
       public :: ESMF_ConfigGetDim ! gets number of lines in the table
                                   ! and max number of columns by word 
!                                  ! counting disregarding type (function)
!------------------------------------------------------------------------------
! !PUBLIC TYPES:
!------------------------------------------------------------------------------
       public :: ESMF_Config
!EOPI

!==============================================================================
!
! INTERFACE BLOCKS
!
!==============================================================================
!BOPI
! !IROUTINE: ESMF_ConfigGetAttribute - Get an attribute from a Config
!
! !INTERFACE:
      interface ESMF_ConfigGetAttribute
   
! !PRIVATE MEMBER FUNCTIONS:
        module procedure ESMF_ConfigGetString
        module procedure ESMF_ConfigGetFloatR4
        module procedure ESMF_ConfigGetFloatR8
        module procedure ESMF_ConfigGetFloatsR4
        module procedure ESMF_ConfigGetFloatsR8
        module procedure ESMF_ConfigGetIntI4
        module procedure ESMF_ConfigGetIntI8
        module procedure ESMF_ConfigGetIntsI4
        module procedure ESMF_ConfigGetIntsI8

! !DESCRIPTION:
!     This interface provides an entry point for getting
!     items from an {\tt ESMF\_Config} object.
!    
 
!EOPI
      end interface
!
!------------------------------------------------------------------------------


! PRIVATE PARAMETER  SETTINGS:
!------------------------------------------------------------------------------
! Revised parameter table to fit Fortran 90 standard.

       integer,   parameter :: LSZ = 256
       integer,   parameter :: MSZ = 200
       integer,   parameter :: NBUF_MAX = MSZ*LSZ ! max size of buffer
       
       character, parameter :: BLK = achar(32)   ! blank (space)
       character, parameter :: TAB = achar(09)   ! TAB
       character, parameter :: EOL = achar(10)   ! end of line mark (newline)
       character, parameter :: EOB = achar(00)   ! end of buffer mark (null)
       character, parameter :: NULL= achar(00)   ! what it says
       
       character(len=*), parameter :: myname='ESMF_ConfigMod'


!    Defines standard i/o units.

        integer, parameter :: stdin  = 5
        integer, parameter :: stdout = 6

#ifdef  sysHP-UX
        ! Special setting for HP-UX

        integer, parameter :: stderr = 7
#else
        ! Generic setting for UNIX other than HP-UX

        integer, parameter :: stderr = 0
#endif

	integer,parameter :: MX_LU=255

!------------------------------------------------------------------------------
! !OPAQUE TYPES:
!------------------------------------------------------------------------------
       type ESMF_Config
          sequence
          private              
          character(len=NBUF_MAX),pointer :: buffer    ! hold the whole file
          character(len=LSZ),     pointer :: this_line ! the current line
          integer :: nbuf                              ! actual size of buffer 
          integer :: next_line                         ! index_ for next line 
                                                       ! on buffer
       end type ESMF_Config

     contains
!
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ConfigCreate"
!-----------------------------------------------------------------------
! Earth System Modeling Framework
!BOP -------------------------------------------------------------------
!
! !IROUTINE: ESMF_ConfigCreate - Create a Config object
!
! !INTERFACE:
      type(ESMF_Config) function ESMF_ConfigCreate( rc )

! !ARGUMENTS:
      integer,intent(out), optional              :: rc 
!
! !DESCRIPTION: 
!   Creates an {\tt ESMF\_Config} for use in subsequent calls.
!
!   The arguments are:
!   \begin{description}
!   \item [{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP -------------------------------------------------------------------
      character(len=*),parameter :: myname_=myname//'::ESMF_ConfigCreate'
      integer iret
      type(ESMF_Config) :: config_local

      iret = 0
 
! Initialization

      allocate(config_local%buffer, config_local%this_line, stat = iret)
      if (ESMF_LogMsgFoundAllocError(iret, "Allocating local buffer", &
                                       ESMF_CONTEXT, rc)) return
      !if(iret /= 0) then
      !   ! TODO:   call perr(myname_,'allocate(...%..)', iret)
      !   print *, myname_,'allocate(...%..)', iret
      !endif

      ESMF_ConfigCreate = config_local
      if (present( rc )) rc = iret

      return
    end function ESMF_ConfigCreate
    

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ConfigDestroy"
!-----------------------------------------------------------------------
! Earth System Modeling Framework
!BOP -------------------------------------------------------------------
!
! !IROUTINE: ESMF_ConfigDestroy - Destroy a Config object
!
! !INTERFACE:
    subroutine ESMF_ConfigDestroy( config, rc )

! !ARGUMENTS:
      type(ESMF_Config), intent(inout) :: config
      integer,intent(out), optional    :: rc
!
! !DESCRIPTION: 
!    Destroys the {\tt config} object.
!
!   The arguments are:
!   \begin{description}
!   \item [config]
!     Already created {\tt ESMF\_Config} object.
!   \item [{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP -------------------------------------------------------------------
      character(len=*),parameter :: myname_=myname//'::ESMF_ConfigDestroy'
      integer :: iret

      iret = 0

      deallocate(config%buffer, config%this_line, stat = iret)
      if (ESMF_LogMsgFoundAllocError(iret, "Deallocating local buffer", &
                                       ESMF_CONTEXT, rc)) return
      !if(iret /= 0) then
      ! SUBSTITUTE:  call perr(myname_,'deallocate(...%..)', iret)
      !   print *, myname_,'deallocate(...%..)', iret
      !endif


      if (present( rc )) rc = iret
      return

     end subroutine ESMF_ConfigDestroy


#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ConfigFindLabel"
!-----------------------------------------------------------------------
! Earth System Modeling Framework
!BOP -------------------------------------------------------------------
!
! !IROUTINE: ESMF_ConfigFindLabel - Find a label
!
! !INTERFACE:
    subroutine ESMF_ConfigFindLabel( config, label, rc )

! !ARGUMENTS:
      type(ESMF_Config), intent(inout)  :: config 
      character(len=*), intent(in)   :: label
      integer, intent(out), optional  :: rc 

! !DESCRIPTION: Finds the {\tt label} (key) in the {\tt config} file. 
!
!               Since the search is done by looking for a word in the 
!               whole resource file, it is important to use special 
!               conventions to distinguish labels from other words 
!               in the resource files. The DAO convention is to finish 
!               line labels by : and table labels by ::.
!
!
!   The arguments are:
!   \begin{description}
!   \item [config]
!     Already created {\tt ESMF\_Config} object.
!   \item [label]
!     Identifying label. 
!   \item [{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     Equals -1 if buffer could not be loaded, -2 if label not found,
!     and -3 if invalid operation with index.
!   \end{description}
!
!EOP -------------------------------------------------------------------
	character(len=*),parameter :: myname_=myname//'ESMF_ConfigFindLabel'

      integer i, j, iret

      iret = 0

!     Determine whether label exists
!     ------------------------------    

      i = index_ ( config%buffer(1:config%nbuf), EOL//label ) + 1
      if ( i .eq. 1 ) then
         config%this_line = BLK // EOL
         if (ESMF_LogMsgFoundError(ESMF_RC_NOT_FOUND, &
                                "label not found", &
                                 ESMF_CONTEXT, rc)) return
         !iret = -2
         !if ( present (rc )) rc = iret
         !return
      elseif(i.le.0) then
         if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                "invalid operation with index", &
                                 ESMF_CONTEXT, rc)) return
         ! SUBSTITUTE:	   call die(myname_,'invalid index_() return',i)
         !print *, myname_,'invalid index_() return',i
         !iret = -3
         !if ( present (rc )) rc = iret
         !return
      end if

!     Extract the line associated with this label
!     -------------------------------------------
      i = i + len ( label )
      j = i + index_(config%buffer(i:config%nbuf),EOL) - 2
      config%this_line = config%buffer(i:j) // BLK // EOL
      
      config%next_line = j + 2
      
      iret = 0
      if ( present (rc )) rc = iret
      
      return
    end subroutine ESMF_ConfigFindLabel


#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ConfigGetString"
!-----------------------------------------------------------------------
! Earth System Modeling Framework
!BOP -------------------------------------------------------------------
!
! !IROUTINE: ESMF_ConfigGetAttribute - Get a character string
!
!
! !INTERFACE:
      ! Private name; call using ESMF_ConfigGetAttribute()
      subroutine ESMF_ConfigGetString( config, value, label, default, rc )

! !ARGUMENTS:
      type(ESMF_Config), intent(inout)       :: config     
      character(len=*), intent(out)          :: value
      character(len=*), intent(in), optional :: label 
      character(len=*), intent(in), optional :: default 
      integer, intent(out), optional         :: rc     
!
! !DESCRIPTION: Gets a sequence of characters. It will be 
!               terminated by the first white space.
!
!   The arguments are:
!   \begin{description}
!   \item [config]
!     Already created {\tt ESMF\_Config} object.
!   \item [value]
!     Returned value. 
!   \item [{[label]}]
!     Identifing label. 
!   \item [{[default]}]
!     Default value if {\tt label} is not found in {\tt config} object. 
!   \item [{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}

!EOP -------------------------------------------------------------------
      character*1   ch
      integer       ib, ie, iret
      
      iret = 0

! Default setting
      if( present( default ) ) then 
         value = default
      else
         value = BLK
      endif

! Processing
      if(present( label )) then
         call ESMF_ConfigFindLabel( config, label, iret )
         if ( iret /= 0 ) then
            if (present(default)) then
               iret = ESMF_SUCCESS
            end if
            if ( present (rc )) rc = iret
            return
         endif
      endif

      call ESMF_Config_trim ( config%this_line )
      
      ch = config%this_line(1:1)
      if ( ch .eq. '"' .or. ch .eq. "'" ) then
         ib = 2
         ie = index_ ( config%this_line(ib:), ch ) 
      else
         ib = 1
         ie = min(index_(config%this_line,BLK),	&
              index_(config%this_line,EOL)) - 1
      end if
      
      if ( ie .lt. ib ) then
         value = BLK
         if ( present ( default )) value = default
         iret = -1
         if ( present (rc )) rc = iret
         return
      else
         ! Get the string, and shift the rest of %this_line to
         ! the left
         
         value = config%this_line(ib:ie) 
         config%this_line = config%this_line(ie+2:)
         iret = 0
      end if

      if ( present (rc )) rc = iret
      return
      
      
    end subroutine ESMF_ConfigGetString
    
    

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ConfigGetFloatR4"
!-----------------------------------------------------------------------
! Earth System Modeling Framework
!BOP -------------------------------------------------------------------
!
! !IROUTINE: ESMF_ConfigGetAttribute - Get a 4-byte real number

!
! !INTERFACE:
      ! Private name; call using ESMF_ConfigGetAttribute()
      subroutine ESMF_ConfigGetFloatR4( config, value, label, default, rc )

! !ARGUMENTS:
      type(ESMF_Config), intent(inout)       :: config    
      real(ESMF_KIND_R4), intent(out)        :: value    
      character(len=*), intent(in), optional :: label
      real, intent(in), optional             :: default 
      integer, intent(out), optional         :: rc     
!
! !DESCRIPTION: 
!   Gets a 4-byte real {\tt value} from the {\tt config} object.
!
!   The arguments are:
!   \begin{description}
!   \item [config]
!     Already created {\tt ESMF\_Config} object.
!   \item [value]
!     Returned value. 
!   \item [{[label]}]
!     Identifying label. 
!   \item [{[default]}]
!     Default value if {\tt label} is not found in {\tt config} object. 
!   \item [{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!

!EOP -------------------------------------------------------------------
!
      integer:: iret
      character*256 :: string
      real(ESMF_KIND_R4) ::     x
      
      iret = 0

! Default setting
      if( present( default ) ) then 
         value = default
      else
         value = 0.0
      endif

! Processing
      if (present (label ) ) then
         call ESMF_ConfigGetString( config, string, label, rc = iret )
      else
         call ESMF_ConfigGetString( config, string, rc = iret )
      endif

      if ( iret .eq. 0 ) then
           read(string,*,iostat=iret) x
           if ( iret .ne. 0 ) iret = -2
      else
         if( present( default )) then
            x = default
            iret = ESMF_SUCCESS
         endif
      end if

      if ( iret .eq. 0 ) then
         value = x
      endif

      if( present( rc )) rc = iret 
      return

    end subroutine ESMF_ConfigGetFloatR4



#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ConfigGetFloatR8"
!-----------------------------------------------------------------------
! Earth System Modeling Framework
!BOP -------------------------------------------------------------------
!
! !IROUTINE: ESMF_ConfigGetAttribute - Get an 8-byte real number

!
! !INTERFACE:
      ! Private name; call using ESMF_ConfigGetAttribute()
      subroutine ESMF_ConfigGetFloatR8( config, value, label, default, rc )

! !ARGUMENTS:
      type(ESMF_Config), intent(inout)       :: config    
      real(ESMF_KIND_R8), intent(out)        :: value 
      character(len=*), intent(in), optional :: label
      real, intent(in), optional             :: default 
      integer, intent(out), optional         :: rc     
!
! !DESCRIPTION: 
!   Gets an 8-byte real {\tt value} from the {\tt config} object.
!
!   The arguments are:
!   \begin{description}
!   \item [config]
!     Already created {\tt ESMF\_Config} object.
!   \item [value]
!     Returned real value. 
!   \item [{[label]}]
!     Identifying label. 
!   \item [{[default]}]
!     Default value if {\tt label} is not found in {\tt config} object. 
!   \item [{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!

!EOP -------------------------------------------------------------------
!
      integer:: iret
      character*256 :: string
      real(ESMF_KIND_R8) ::     x
      
      iret = 0

! Default setting
      if( present( default ) ) then 
         value = default
      else
         value = 0.0
      endif

! Processing
      if (present (label ) ) then
         call ESMF_ConfigGetString( config, string, label, rc = iret )
      else
         call ESMF_ConfigGetString( config, string, rc = iret )
      endif

      if ( iret .eq. 0 ) then
           read(string,*,iostat=iret) x
           if ( iret .ne. 0 ) iret = -2
      else
         if( present( default )) then
            x = default
            iret = ESMF_SUCCESS
         endif
      end if

      if ( iret .eq. 0 ) then
         value = x
      endif

      if( present( rc )) rc = iret 
      return

    end subroutine ESMF_ConfigGetFloatR8



#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ConfigGetFloatsR4"
!-----------------------------------------------------------------------
! Earth System Modeling Framework
!BOP -------------------------------------------------------------------
!
! !IROUTINE: ESMF_ConfigGetAttribute - Get a list of 4-byte real numbers

!
! !INTERFACE:
      ! Private name; call using ESMF_ConfigGetAttribute()
      subroutine ESMF_ConfigGetFloatsR4( config, valueList, count, label,  &
                                         default, rc )

! !ARGUMENTS:
      type(ESMF_Config), intent(inout)       :: config    
      real(ESMF_KIND_R4), intent(inout)      :: valueList(:) 
      integer, intent(in)                    :: count 
      character(len=*), intent(in), optional :: label 
      real, intent(in), optional             :: default
      integer, intent(out), optional         :: rc    
!
! !DESCRIPTION: 
!  Gets a 4-byte real {\tt valueList} of a given {\tt count} from
!  the {\tt config} object.
!
!   The arguments are:
!   \begin{description}
!   \item [config]
!     Already created {\tt ESMF\_Config} object.
!   \item [valueList]
!     Returned real values. 
!   \item [count]
!     Number of returned values expected. 
!   \item [{[label]}]
!     Identifing label. 
!   \item [{[default]}]
!     Default value if label is not found in configuration object. 
!   \item [{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP -------------------------------------------------------------------
      character(len=*),parameter :: myname_=myname//'ESMF_ConfigGetFloat_array'
      integer iret, i 
      
      iret = 0


      if (count.le.0) then
         if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                "invalid SIZE", &
                                 ESMF_CONTEXT, rc)) return
         !print *,myname_,' invalid SIZE =', count
         !iret = -1
         !if(present( rc )) rc = iret
         !return
      endif
       
! Default setting
      if( present( default ) ) then 
         valueList(1:count) = default
      else
         valueList(1:count) = 0.0
      endif

! Processing
      if (present( label )) then
         call ESMF_ConfigFindLabel( config, label, rc = iret )
      end if

      do i = 1, count
         
         if(present( default )) then
            call ESMF_ConfigGetFloatR4( config, valueList(i), default=default, rc=iret )
         else
            call ESMF_ConfigGetFloatR4( config, valueList(i), rc = iret)
         endif
      enddo

      if(present( rc )) rc = iret
      return
    end subroutine ESMF_ConfigGetFloatsR4

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ConfigGetFloatsR8"
!-----------------------------------------------------------------------
! Earth System Modeling Framework
!BOP -------------------------------------------------------------------
!
! !IROUTINE: ESMF_ConfigGetAttribute - Get a list of 8-byte real numbers

!
! !INTERFACE:
      ! Private name; call using ESMF_ConfigGetAttribute()
      subroutine ESMF_ConfigGetFloatsR8( config, valueList, count, label,  &
                                         default, rc )

! !ARGUMENTS:
      type(ESMF_Config), intent(inout)       :: config    
      real(ESMF_KIND_R8), intent(inout)      :: valueList(:) 
      integer, intent(in)                    :: count 
      character(len=*), intent(in), optional :: label 
      real, intent(in), optional             :: default
      integer, intent(out), optional         :: rc    
!
! !DESCRIPTION: 
!   Gets an 8-byte real {\tt valueList} of a given {\tt count} from the
!   {\tt config} object.
!
!   The arguments are:
!   \begin{description}
!   \item [config]
!     Already created {\tt ESMF\_Config} object.
!   \item [valueList]
!     Returned values. 
!   \item [count]
!     Number of returned values expected. 
!   \item [{[label]}]
!     Identifying label. 
!   \item [{[default]}]
!     Default value if label is not found in configuration object. 
!   \item [{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP -------------------------------------------------------------------
      character(len=*),parameter :: myname_=myname//'ESMF_ConfigGetFloat_array'
      integer iret, i 
      
      iret = 0


      if (count.le.0) then
         if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                "invalid SIZE", &
                                 ESMF_CONTEXT, rc)) return
         !print *,myname_,' invalid SIZE =', count
         !iret = -1
         !if(present( rc )) rc = iret
         !return
      endif
       
! Default setting
      if( present( default ) ) then 
         valueList(1:count) = default
      else
         valueList(1:count) = 0.0
      endif

! Processing
      if (present( label )) then
         call ESMF_ConfigFindLabel( config, label, rc = iret )
      end if

      do i = 1, count
         
         if(present( default )) then
            call ESMF_ConfigGetFloatR8( config, valueList(i), default=default, rc=iret )
         else
            call ESMF_ConfigGetFloatR8( config, valueList(i), rc = iret)
         endif
      enddo

      if(present( rc )) rc = iret
      return
    end subroutine ESMF_ConfigGetFloatsR8

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ConfigGetIntI4"
!-----------------------------------------------------------------------
! Earth System Modeling Framework
!BOP -------------------------------------------------------------------
!
! !IROUTINE: ESMF_ConfigGetAttribute - Get a 4-byte integer number

!
! !INTERFACE:
      ! Private name; call using ESMF_ConfigGetAttribute()
      subroutine ESMF_ConfigGetIntI4( config, value, label, default, rc )

! !ARGUMENTS:
      type(ESMF_Config), intent(inout)       :: config     
      integer(ESMF_KIND_I4), intent(out)     :: value
      character(len=*), intent(in), optional :: label 
      integer, intent(in), optional          :: default
      integer, intent(out), optional         :: rc   

!
! !DESCRIPTION: 
!  Gets an integer {\tt value} from the {\tt config} object.
!
!   The arguments are:
!   \begin{description}
!   \item [config]
!     Already created {\tt ESMF\_Config} object.
!   \item [value]
!     Returned integer value. 
!   \item [{[label]}]
!     Identifying label. 
!   \item [{[default]}]
!     Default value if label is not found in configuration object. 
!   \item [{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP -------------------------------------------------------------------
      character*256 string
      real*8        x
      integer(ESMF_KIND_I4)  n
      integer       iret

      iret = 0

! Default setting
      if( present( default ) ) then 
         value = default
      else
         value = 0
      endif

! Processing
      if (present (label ) ) then
         call ESMF_ConfigGetString( config, string, label, rc = iret )
      else
         call ESMF_ConfigGetString( config, string, rc = iret )
      endif

      if ( iret .eq. 0 ) then
           read(string,*,iostat=iret) x
           if ( iret .ne. 0 ) iret = -2
      end if
      if ( iret .eq. 0 ) then
         n = nint(x)
      else
         if( present( default )) then
            n = default
            iret = ESMF_SUCCESS
         else
            n = 0
         endif
      endif

      if ( iret .eq. 0 ) then
         value = n
      endif

      if( present( rc )) rc = iret
      
      return
    end subroutine ESMF_ConfigGetIntI4

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ConfigGetIntI8"
!-----------------------------------------------------------------------
! Earth System Modeling Framework
!BOP -------------------------------------------------------------------
!
! !IROUTINE: ESMF_ConfigGetAttribute - Get an 8-byte integer number

!
! !INTERFACE:
      ! Private name; call using ESMF_ConfigGetAttribute()
      subroutine ESMF_ConfigGetIntI8( config, value, label, default, rc )

! !ARGUMENTS:
      type(ESMF_Config), intent(inout)       :: config     
      integer(ESMF_KIND_I8), intent(out)     :: value
      character(len=*), intent(in), optional :: label 
      integer, intent(in), optional          :: default
      integer, intent(out), optional         :: rc   

!
! !DESCRIPTION: 
!  Gets an 8-byte integer {\tt value} from the {\tt config} object.
!
!   The arguments are:
!   \begin{description}
!   \item [config]
!     Already created {\tt ESMF\_Config} object.
!   \item [value]
!     Returned integer value. 
!   \item [{[label]}]
!     Identifying label. 
!   \item [{[default]}]
!     Default value if label is not found in configuration object. 
!   \item [{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP -------------------------------------------------------------------
      character*256 string
      real*8        x
      integer(ESMF_KIND_I8)  n
      integer       iret

      iret = 0

! Default setting
      if( present( default ) ) then 
         value = default
      else
         value = 0
      endif

! Processing
      if (present (label ) ) then
         call ESMF_ConfigGetString( config, string, label, rc = iret )
      else
         call ESMF_ConfigGetString( config, string, rc = iret )
      endif

      if ( iret .eq. 0 ) then
           read(string,*,iostat=iret) x
           if ( iret .ne. 0 ) iret = -2
      end if
      if ( iret .eq. 0 ) then
         n = nint(x)
      else
         if( present( default )) then
            n = default
            iret = ESMF_SUCCESS
         else
            n = 0
         endif
      endif

      if ( iret .eq. 0 ) then
         value = n
      endif

      if( present( rc )) rc = iret
      
      return
    end subroutine ESMF_ConfigGetIntI8


#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ConfigGetIntsI4"
!-----------------------------------------------------------------------
! Earth System Modeling Framework
!BOP -------------------------------------------------------------------
!
! !IROUTINE: ESMF_ConfigGetAttribute - Get a list of 4-byte integers
!
! !INTERFACE:
      ! Private name; call using ESMF_ConfigGetAttribute()
      subroutine ESMF_ConfigGetIntsI4( config, valueList, count, label,  &
                                       default, rc )

! !ARGUMENTS:
      type(ESMF_Config), intent(inout)       :: config      
      integer(ESMF_KIND_I4), intent(inout)   :: valueList(:)  
      integer, intent(in)                    :: count  
      character(len=*), intent(in), optional :: label 
      integer, intent(in), optional          :: default
      integer, intent(out), optional         :: rc    
!
! !DESCRIPTION: 
!  Gets a 4-byte integer {\tt valueList} of given {\tt count} from the 
!  {\tt config} object.
!
!   The arguments are:
!   \begin{description}
!   \item [config]
!     Already created {\tt ESMF\_Config} object.
!   \item [valueList]
!     Returned values. 
!   \item [count]
!     Number of returned values expected. 
!   \item [{[label]}]
!     Identifying label. 
!   \item [{[default]}]
!     Default value if label is not found in configuration object. 
!   \item [{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP -------------------------------------------------------------------
      character(len=*),parameter :: myname_=myname//'ESMF_ConfigGetInt_array'
      integer iret, i 
      
      iret = 0

      if (count.le.0) then
         if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                "invalid SIZE", &
                                 ESMF_CONTEXT, rc)) return
         !print *,myname_,' invalid SIZE =', count
         !iret = -1
         !if(present( rc )) rc = iret
         !return
      endif
       
 ! Default setting
      if( present( default ) ) then 
         valueList(1:count) = default
      else
         valueList(1:count) = 0
      endif

! Processing 
      if (present( label )) then
         call ESMF_ConfigFindLabel( config, label, rc = iret )
      end if

      do i = 1, count
         
         if(present( default )) then
            call ESMF_ConfigGetIntI4( config, valueList(i), default = default, rc = iret)
         else
            call ESMF_ConfigGetIntI4( config, valueList(i), rc = iret)
         endif
      enddo

      if(present( rc )) rc = iret
      return
    end subroutine ESMF_ConfigGetIntsI4



#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ConfigGetIntsI8"
!-----------------------------------------------------------------------
! Earth System Modeling Framework
!BOP -------------------------------------------------------------------
!
! !IROUTINE: ESMF_ConfigGetAttribute - Get a list of 8-byte integers
!
! !INTERFACE:
      ! Private name; call using ESMF_ConfigGetAttribute()
      subroutine ESMF_ConfigGetIntsI8( config, valueList, count, label,  &
                                       default, rc )

! !ARGUMENTS:
      type(ESMF_Config), intent(inout)       :: config      
      integer(ESMF_KIND_I8), intent(inout)   :: valueList(:)  
      integer, intent(in)                    :: count  
      character(len=*), intent(in), optional :: label 
      integer, intent(in), optional          :: default
      integer, intent(out), optional         :: rc    
!
! !DESCRIPTION: 
!  Gets an 8-byte integer {\tt valueList} of given {\tt count} from
!  the {\tt config} object.
!
!   The arguments are:
!   \begin{description}
!   \item [config]
!     Already created {\tt ESMF\_Config} object.
!   \item [valueList]
!     Returned values. 
!   \item [count]
!     Number of returned values expected. 
!   \item [{[label]}]
!     Identifying label. 
!   \item [{[default]}]
!     Default value if label is not found in configuration object. 
!   \item [{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP -------------------------------------------------------------------
      character(len=*),parameter :: myname_=myname//'ESMF_ConfigGetInt_array'
      integer iret, i 
      
      iret = 0

      if (count.le.0) then
         if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                "invalid SIZE", &
                                 ESMF_CONTEXT, rc)) return
         !print *,myname_,' invalid SIZE =', count
         !iret = -1
         !if(present( rc )) rc = iret
         !return
      endif
       
 ! Default setting
      if( present( default ) ) then 
         valueList(1:count) = default
      else
         valueList(1:count) = 0
      endif

! Processing 
      if (present( label )) then
         call ESMF_ConfigFindLabel( config, label, rc = iret )
      end if

      do i = 1, count
         
         if(present( default )) then
            call ESMF_ConfigGetIntI8( config, valueList(i), default = default, rc = iret)
         else
            call ESMF_ConfigGetIntI8( config, valueList(i), rc = iret)
         endif
      enddo

      if(present( rc )) rc = iret
      return
    end subroutine ESMF_ConfigGetIntsI8




#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ConfigGetChar"
!-----------------------------------------------------------------------
! Earth System Modeling Framework
!BOP -------------------------------------------------------------------
!
! !IROUTINE: ESMF_ConfigGetChar - Get a character
!
! !INTERFACE:
      subroutine ESMF_ConfigGetChar( config, value, label, default, rc )

! !ARGUMENTS:
      type(ESMF_Config), intent(inout)       :: config 
      character, intent(out)                 :: value
      character(len=*), intent(in), optional :: label   
      character, intent(in), optional        :: default
      integer, intent(out), optional         :: rc    
!
! !DESCRIPTION: 
!  Gets a character {\tt value} from the {\tt config} object.
!
!   The arguments are:
!   \begin{description}
!   \item [config]
!     Already created {\tt ESMF\_Config} object.
!   \item [value]
!     Returned value. 
!   \item [{[label]}]
!     Identifying label. 
!   \item [{[default]}]
!     Default value if label is not found in configuration object. 
!   \item [{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!
!EOP -------------------------------------------------------------------
      character*256 string
      integer       iret

      iret = 0

! Default setting
      if( present( default ) ) then 
         value = default
      else
         value = BLK
      endif

! Processing
      if (present (label ) ) then
         call ESMF_ConfigGetString( config, string, label, rc = iret )
      else
         call ESMF_ConfigGetString( config, string, rc = iret )
      endif

      if ( iret .eq. 0 ) then
         value = string(1:1)
      else
         if( present( default )) then
            iret = ESMF_SUCCESS
         endif
      end if

      if (present( rc )) rc = iret

      return

    end subroutine ESMF_ConfigGetChar


#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ConfigGetDim"
!-----------------------------------------------------------------------
! Earth System Modeling Framework
!BOP -------------------------------------------------------------------
!
! !IROUTINE: ESMF_ConfigGetDim - Get table sizes
!
! !INTERFACE:

    subroutine ESMF_ConfigGetDim( config, label, lineCount, columnCount, rc )

      implicit none

      type(ESMF_Config), intent(inout)       :: config    ! ESMF Configuration

     integer, intent(out)                    :: lineCount
     integer, intent(out)                    :: columnCount

      character(len=*), intent(in), optional :: label ! label (if present)
                                                      ! otherwise, current
                                                      ! line

      integer, intent(out), optional        :: rc     ! Error code
!
! !DESCRIPTION: 
!  Returns the number of lines in the table in {\tt lineCount} and 
!  the maximum number of words in a table line in {\tt columnCount}.
!
!   The arguments are:
!   \begin{description}
!   \item [config]
!     Already created {\tt ESMF\_Config} object.
!   \item [lineCount]
!     Returned number of lines in the table. 
!   \item [columnCount]
!     Returned maximum number of words in a table line. 
!   \item [{[label]}]
!     Identifying label.
!   \item [{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP -------------------------------------------------------------------
      integer n, iret
      logical tend

      lineCount = 0
      columnCount = 0
      

      call ESMF_ConfigFindLabel(config, label = label, rc = iret )
      if ( iret /= 0 ) then
         if ( present( rc )) rc = iret
         return
      endif

      do 
         call ESMF_ConfigNextLine( config, tend, rc = iret)
         if (iret /=0 ) then
            lineCount = 0
            columnCount = 0
            exit
         endif
         if ( tend ) then
            exit
         else
            lineCount = lineCount + 1
            n = ESMF_ConfigGetLen( config, rc = iret)
            if ( iret /= 0 ) then
               lineCount = 0
               columnCount = 0
               if ( present( rc )) rc = iret
               return
            else
               columnCount = max(columnCount, n)
            endif
         endif 
      enddo
      
      if ( present( rc )) rc = iret
      return

    end subroutine ESMF_ConfigGetDim
    
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ConfigGetLen"
!-----------------------------------------------------------------------
! Earth System Modeling Framework
!BOP -------------------------------------------------------------------
! !IROUTINE: ESMF_ConfigGetLen - Get the length of the line in words
!
! !INTERFACE:
    integer function ESMF_ConfigGetLen( config, label, rc )

! !ARGUMENTS:
      type(ESMF_Config), intent(inout)       :: config 
      character(len=*), intent(in), optional :: label
      integer, intent(out), optional :: rc         
!
! !DESCRIPTION: 
! Gets the length of the line in words by counting words
! disregarding types.  Returns the word count as an integer.
!
!   The arguments are:
!   \begin{description}
!   \item [config]
!     Already created {\tt ESMF\_Config} object.
!   \item [{[label]}]
!     Identifying label.   If not specified, use the current line.
!   \item [{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP -------------------------------------------------------------------
      character*256 string
      integer iret
      integer count 

      iret = 0
      count = 0
      ESMF_ConfigGetLen = -1    ! assume error
      
      if( present( label )) then
         call ESMF_ConfigFindLabel(config, label = label, rc = iret )
         if( iret /= 0) then
            if (present( rc )) rc = iret
            return
         endif
      endif

      do
         call ESMF_ConfigGetString( config, string, rc = iret )
         if ( iret .eq. 0 ) then
            count = count + 1
         else
            if (iret .eq. -1) iret  = 0  ! end of the line
            exit
         endif
      enddo
 

      ESMF_ConfigGetLen = count

      if( present ( rc )) rc = iret
      return
    end function ESMF_ConfigGetLen


#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ConfigLoadFile"
!-----------------------------------------------------------------------
! Earth System Modeling Framework
!BOP -------------------------------------------------------------------
!
! !IROUTINE: ESMF_ConfigLoadFile - Load resource file into memory
!
! !INTERFACE:
    subroutine ESMF_ConfigLoadFile( config, filename, delayout, unique, rc )

! !ARGUMENTS:
      type(ESMF_Config), intent(inout) :: config     
      character(len=*), intent(in)     :: filename 
      type(ESMF_DELayout), intent(in), optional  :: delayout 
      logical, intent(in), optional    :: unique 
      integer, intent(out), optional :: rc         
!
! !DESCRIPTION: 
!  Resource file with {\tt filename} is loaded into memory.
!
!   The arguments are:
!   \begin{description}
!   \item [config]
!     Already created {\tt ESMF\_Config} object.
!   \item [filename]
!     Configuration file name.
!   \item [{[delayout]}]
!     {\tt ESMF\_DELayout} associated with this {\tt config} object.
!   \item [{[unique]}]
!     If specified as true, uniqueness of labels are checked and 
!     error code set if duplicates found.
!   \item [{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP -------------------------------------------------------------------

      character(len=*),parameter :: myname_='ESMF_ConfigLoadFile'
      integer :: iret

      iret = 0

      if( present ( unique )) then
         call ESMF_ConfigLoadFile_1proc_( config, filename, unique, iret )
      else
         call ESMF_ConfigLoadFile_1proc_( config, filename, rc = iret )
      endif
      if(iret /= 0) then
           if (ESMF_LogMsgFoundError(ESMF_RC_FILE_OPEN, &
                                "unable to load file", &
                                 ESMF_CONTEXT, rc)) return
      !if(iret /= 0) then
! SUBSITUTE call perr(myname_,'ESMF_ConfigLoadFile("'//trim(filename)//'")', iret)
      !   print *, myname_,'ESMF_ConfigLoadFile("'//trim(filename)//'")', iret
      !
      !   if (present( rc )) rc = iret
      !   return
      endif

      if ( present (delayout) ) then
         call ESMF_LogWrite("DELayout not used yet", ESMF_LOG_WARNING, &
                           ESMF_CONTEXT)
         !print *, myname_, ' DE layout is not used yet '
      endif

      if (present( rc )) rc = iret
      return

    end subroutine ESMF_ConfigLoadFile



#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ConfigLoadFile_1proc_"
!-----------------------------------------------------------------------
! Earth System Modeling Framework
!BOPI -------------------------------------------------------------------
!
! !IROUTINE: ESMF_ConfigLoadFile_1proc - Load resource file into memory
!

! !INTERFACE:

    subroutine ESMF_ConfigLoadFile_1proc_( config, filename, unique, rc )


      implicit none

      type(ESMF_Config), intent(inout) :: config     ! ESMF Configuration
      character(len=*), intent(in)  :: filename     ! file name
      logical, intent(in), optional :: unique    ! if unique is present, 
                                                 ! uniqueness of labels
                                                 ! is checked and error
                                                 ! code is set
      integer, intent(out), optional :: rc       ! Error code
                                                 !   0 no error
                                                 ! -98 coult not get unit 
                                                 !     number (strange!)
                                                 ! -98 talk to a wizzard
                                                 ! -99 out of memory: increase
                                                 !     NBUF_MAX 
                                                 !     other iostat from open 
                                                 !     statement.
!
! !DESCRIPTION: Resource file filename is loaded is loaded into memory
!
!EOPI -------------------------------------------------------------------
      integer         lu, ios, loop, ls, ptr, iret
      character*256   line
      character(len=*), parameter :: myname_= 'ESMF_ConfigLoadFile_1proc'

      iret = 0

      if ( present( unique ) ) then
         call ESMF_LogWrite("uniqueness of labels not checked yet", &
                           ESMF_LOG_WARNING, &
                           ESMF_CONTEXT)
         !print *, myname_, ' Uniqueness of labels is not checked yet '
      endif

!     Open file
!     ---------     
      lu = luavail()	! a more portable version
      if ( lu .lt. 0 ) then
         iret = -97
        if ( present (rc )) rc = iret
         return
      end if

	! A open through an interface to avoid portability problems.
	! (J.G.)

      call opntext(lu,filename,'old',ios)
      if ( ios .ne. 0 ) then
	 write(*,'(2a,i5)') myname_,': opntext() error, ios =',ios
         iret = ios
         if ( present (rc )) rc = iret
         return
      end if

!     Read to end of file
!     -------------------
      config%buffer(1:1) = EOL
      ptr = 2                         ! next buffer position
      do loop = 1, NBUF_MAX

!        Read next line
!        --------------
         read(lu,'(a)', end=11) line  ! read next line
         call ESMF_Config_trim ( line )      ! remove trailing blanks
         call ESMF_Config_pad ( line )        ! Pad with # from end of line

!        A non-empty line
!        ----------------
         ls = index_(line,'#' ) - 1    ! line length
         if ( ls .gt. 0 ) then
            if ( (ptr+ls) .gt. NBUF_MAX ) then
               iret = -99
               if ( present (rc )) rc = iret
               return
            end if
            config%buffer(ptr:ptr+ls) = line(1:ls) // EOL
            ptr = ptr + ls + 1
         end if

      end do
      
      iret = -98 ! good chance config%buffer is not big enough 
      if ( present (rc )) rc = iret
      return
      
11    continue

!     All done
!     --------
! Close lu
      call clstext(lu,ios)
      if(ios /= 0) then
         iret = -99
         if ( present (rc )) rc = iret
         return
      endif
      config%buffer(ptr:ptr) = EOB
      config%nbuf = ptr
      config%this_line=' '
      config%next_line=0

      iret = 0
      if ( present (rc )) rc = iret

      return
    end subroutine ESMF_ConfigLoadFile_1proc_

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ConfigNextLine"
!-----------------------------------------------------------------------
! Earth System Modeling Framework
!BOP -------------------------------------------------------------------
!
! !IROUTINE: ESMF_ConfigNextLine - Find next line
!
! !INTERFACE:
    subroutine ESMF_ConfigNextLine( config, tableEnd, rc)

! !ARGUMENTS:
      type(ESMF_Config), intent(inout) :: config 
      logical, intent(out), optional :: tableEnd
      integer, intent(out), optional:: rc 
!
! !DESCRIPTION: 
!   Selects the next line (for tables).
!
!   The arguments are:
!   \begin{description}
!   \item [config]
!     Already created {\tt ESMF\_Config} object.
!   \item [{[tableEnd]}]
!     If specifed as {\tt TRUE}, end of table mark (::) is checked.
!   \item [{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}

!EOP -------------------------------------------------------------------
      integer :: i, j, iret
      logical :: local_tend

      iret = 0
      local_tend = .false.

      if ( config%next_line .ge. config%nbuf ) then
         iret = -1
           if ( present (rc )) rc = iret
         return
      end if

      i = config%next_line
      j = i + index_(config%buffer(i:config%nbuf),EOL) - 2
      config%this_line = config%buffer(i:j) // BLK // EOL
      
      if ( config%this_line(1:2) .eq. '::' ) then
         iret = 0                    ! end of table. We set iret = 0
         local_tend = .true.         ! and end = .true. Used to be
      ! iret = 1  
         config%next_line = config%nbuf + 1
         if ( present (tableEnd )) tableEnd = local_tend
         if ( present (rc )) rc = iret
         return
      end if

      config%next_line = j + 2
      iret = 0
      if ( present (tableEnd )) tableEnd = local_tend
      if ( present (rc )) rc = iret
      return

    end subroutine ESMF_ConfigNextLine


!-----------------------------------------------------------------------



      integer function index_ (string,tok)

      implicit NONE

!-------------------------------------------------------------------------
! !ROUTINE: index_ Extension of the Fortran 77 intrinsic "index" for
!  "string" (input) with length that can exceed 2**15-1 (=MAXLEN).  
!
! !DESCRIPTION: Finds the starting location = "index_", of the first character in "tok" 
!  within "string", where string is of "arbitrary" length.  If tok occurs more than
!  once in "string", then the value of index_ is based on the first occurrence of "tok". 
!
! !CALLING SEQUENCE:
!
!      index_( string,tok )
!
! !INPUT PARAMETERS:
!
      character(len=*), intent(in) :: string, tok
!
!-------------------------------------------------------------------------
      integer idx, i, n, nlen, lt, ibot, itop
      integer, parameter :: MAXLEN = 32767   ! max size of signed 2-byte integer
      n = len(string)         ! length of string
      lt = len(tok)           ! length of token tok
      i = 1                   ! initialize loop index
      nlen = MAXLEN-lt        ! index requires len(sting)+len(tok)<=MAXLEN 
      itop = min(nlen,n)      ! top of string to index
      ibot = 1                ! bottom of string
      idx  = index(string(ibot:itop),tok)  ! set for good, if itop=n (<=MAXLEN)
      do while(idx .eq. 0 .and. itop < n)
       i = i+1
       itop = min(i*MAXLEN-lt,n)      ! subtract lt to find tok at bdry
       ibot = max(1,itop+1-nlen)    ! bottom of string to index
       idx  = index(string(ibot:itop),tok)   ! idx>=0, since itop-ibot<=MAXLEN
      end do
      index_ = idx                    ! case where idx = 0, or (i=1 & idx > 0)
      if(idx > 0) index_ = idx - 1 + ibot
      return
      end function index_

      subroutine ESMF_Config_Trim ( string )

      implicit NONE


!-------------------------------------------------------------------------
!
! !ROUTINE:  ESMF_Config_Trim() - Removes leading blanks from strings.
!
! !DESCRIPTION: 
!
!    Removes blanks and TABS from begenning of string. 
!    This is a low level i90 routine.
! 
! !CALLING SEQUENCE: 
!
!     call ESMF_Config_Trim ( string )
!
! !INPUT PARAMETERS: 
!
      character*256 string    ! the input string
!
! !OUTPUT PARAMETERS:
!
!     character*256 string    ! the modified string
!
!
!-------------------------------------------------------------------------

      integer     ib, i

!     Get rid of leading blanks
!     -------------------------
      ib = 1
      do i = 1, 255
         if ( string(i:i) .ne. ' ' .and.	&
	        string(i:i) .ne. TAB ) go to 21
         ib = ib + 1
      end do
 21   continue

!     String without trailling blanks
!     -------------------------------
      string = string(ib:)

      return
      end subroutine ESMF_Config_trim


      subroutine ESMF_Config_pad ( string )

      implicit NONE


!-------------------------------------------------------------------------!
! !ROUTINE:  ESMF_CONFIG_Pad() --- Pad strings.
! 
! !DESCRIPTION: 
!
!     Pads from the right with the comment character (\#). It also
!  replaces TAB's with blanks for convenience. This is a low level
!  i90 routine.
!
! !CALLING SEQUENCE: 
!
!      call ESMF_Config_pad ( string )
!
! !INPUT PARAMETERS: 
!
       character*256 string       ! input string

! !OUTPUT PARAMETERS:            ! modified string
!
!      character*256 string
!
! !BUGS:  
!
!      It alters TAB's even inside strings.
!
!
! !REVISION HISTORY: 
!
!  19Jun96   da Silva   Original code.
!-------------------------------------------------------------------------

      integer i

!     Pad end of string with #
!     ------------------------
      do i = 256, 1, -1 
         if ( string(i:i) .ne. ' ' .and.	&
	        string(i:i) .ne. '$' ) go to 11
         string(i:i) = '#'
      end do
 11   continue

!     Replace TAB's with blanks
!     -------------------------
      do i = 1, 256
         if ( string(i:i) .eq. TAB ) string(i:i) = BLK
         if ( string(i:i) .eq. '#' ) go to 21
      end do
 21   continue

      return
      end subroutine ESMF_Config_pad

    




!-----------------------------------------------------------------------
!
! !IROUTINE: luavail - locate the next available unit
!
! !DESCRIPTION:
!
!    luavail() Look for an available (not opened and not statically
!    assigned to any I/O attributes to) logical unit.
!
! !INTERFACE:

	function luavail()
	  !!!use m_stdio_Config
	  implicit none
	  integer :: luavail	! result

!-----------------------------------------------------------------------

  character(len=*),parameter :: myname_=myname//'::luavail'

	integer lu,ios
	logical inuse
#ifdef _UNICOS
	character*8 attr
#endif

	lu=-1
	ios=0
	inuse=.true.

	do while(ios.eq.0.and.inuse)
	  lu=lu+1

		! Test #1, reserved

	  inuse = lu.eq.stdout .or. lu.eq.stdin .or. lu.eq.stderr

#ifdef sysSunOS
		! Reserved units under SunOS
	  inuse = lu.eq.100 .or. lu.eq.101 .or. lu.eq.102
#endif

		! Test #2, in-use

	  if(.not.inuse) inquire(unit=lu,opened=inuse,iostat=ios)

#ifdef _UNICOS
		! Test #3, if the user has reserved the unit through
		! UNICOS' assign().

	  if(ios.eq.0 .and. .not.inuse) then
	    call asnqunit(lu,attr,ios)

		! see asnqunig(3f):
		!
		! ios ==  0, has been assigned to some attributes
		!        -1, not been assigned any attributes
		!     >   0, an error condition, but who cares why.

	    inuse=ios.ne.-1		! the unit is in-use
	    if(ios .ge. -1) ios=0		! still a valid test
	  endif
#endif

	  if(lu .ge. MX_LU) ios=-1
	end do

	if(ios.ne.0) lu=-1
	luavail=lu
end function luavail



!-----------------------------------------------------------------------
! !IROUTINE: opntext - portablly open a text file
!
! !DESCRIPTION:
!
!	Open a text (ASCII) file.  Under FORTRAN, it is defined as
!	"formatted" with "sequential" access.
!
! !INTERFACE:

    subroutine opntext(lu,filename,status,ier)
      implicit none

      integer,         intent(in) :: lu     ! logical unit number
      character(len=*),intent(in) :: filename  ! filename to be opended
      character(len=*),intent(in) :: status ! the value for STATUS=<>
      integer,         intent(out):: ier    ! the status

!-----------------------------------------------------------------------
!

		! local parameter
	character(len=*),parameter :: myname_=myname//'::opntext'

	integer,parameter :: iA=ichar('a')
	integer,parameter :: mA=ichar('A')
	integer,parameter :: iZ=ichar('z')

	character(len=len(status)) :: Ustat
	integer :: i,ic

#ifdef _UNICOS
	call asnunit(lu,'-R',ier)	! remove any set attributes
	if(ier.ne.0) return		! let the parent handle it
#endif

	do i=1,len(status)
	  ic=ichar(status(i:i))
	  if(ic .ge. iA .and. ic .le. iZ) ic=ic+(mA-iA)
	  Ustat(i:i)=char(ic)
	end do

	select case(Ustat)

	case ('APPEND')

	  open(				&
	    unit	=lu,		&
	    file	=filename,	&
	    form	='formatted',	&
	    access	='sequential',	&
	    status	='unknown',	&
	    position	='append',	&
	    iostat	=ier		)

	case default

	  open(				&
	    unit	=lu,		&
	    file	=filename,	&
	    form	='formatted',	&
	    access	='sequential',	&
	    status	=status,	&
	    position	='asis',	&
	    iostat	=ier		)

	end select

	end subroutine opntext


!-----------------------------------------------------------------------
!
! !IROUTINE: clstext - close a text file opend with an opntext() call
!
! !DESCRIPTION:
!
! !INTERFACE:

    subroutine clstext(lu,ier,status)
      implicit none

      integer,                    intent(in)  :: lu     ! a logical unit to close
      integer,                    intent(out) :: ier    ! the status
      Character(len=*), optional, intent(In)  :: status ! keep/delete

!-----------------------------------------------------------------------
          character(len=*), parameter :: myname_ = myname//'::clsitext'
          Character(Len=6) :: status_

          status_ = 'KEEP'
          If (Present(status)) Then
             Select Case (Trim(status))
             Case ('DELETE','delete')
                status_ = 'DELETE'
             Case  ('KEEP','keep')
                status_ = 'KEEP'
             Case Default
                ier = -997
                return
             End Select
          End If

	close(lu,iostat=ier,status=status_)
#ifdef _UNICOS
	if(ier .eq. 0) call asnunit(lu,'-R',ier)	! remove any attributes
#endif

	end subroutine clstext


    end module ESMF_ConfigMod
