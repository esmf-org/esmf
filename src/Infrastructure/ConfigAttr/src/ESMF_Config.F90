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
!
!     ESMF Configuration module
      module ESMF_ConfigMod
!
!==============================================================================
!
! This file contains the Config class definition and all Config
! class methods.
!
!------------------------------------------------------------------------------
! INCLUDES
#include "ESMF_Macros.inc"
!------------------------------------------------------------------------------
!
!BOP
! !MODULE: ESMF_ConfigMod - Implements ESMF configuration management
!
!
! !DESCRIPTION:
!
! The code in this file implements the {\tt Config} class that implements 
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
!       cf = ESMF_ConfigCreate (layout, rc)
!       call ESMF_ConfigLoadFile (cf, fname, rc = rc)
! \end{verbatim}
!
!    The next step is to select the label (record) of interest, say
!
! \begin{verbatim}
!       call ESMF_ConfigFindLabel  ( cf, 'constants:', rc = rc )
! \end{verbatim}
!
!  The 2 constants above can be retrieved with the following code
!  fragment:
! \begin{verbatim}
!       real    r
!       integer i
!       call ESMF_ConfigFindLabel( cf, 'constants:', rc = rc)
!       r = ESMF_ConfigGetFloat( cf, rc = rc )       ! results in r = 3.1415
!       i = ESMF_ConfigGetInt( cf, rc = rc )         ! results in i = 25
! \end{verbatim}
!
!  The file names above can be retrieved with the following
!  code fragment:
! \begin{verbatim}
!       character*20 fn1, fn2, fn3
!       integer      rc
!       call ESMF_ConfigFindLabel ( cf, 'my_file_names:', rc = rc )
!       call ESMF_ConfigGetString ( cf, fn1, rc = rc )  ! ==> fn1 = 'jan87.dat'
!       call ESMF_ConfigGetString ( cf, fn2, rc = rc )  ! ==> fn1 = 'jan88.dat'
!       call ESMF_ConfigGetString ( cf, fn3, rc = rc )  ! ==> fn1 = 'jan89.dat'
! \end{verbatim}
!
! To access the table above, the user first must use 
! ${\tt ESMF\_ConfigFindLabel()}$ to locate the beginning of the table, e.g.,
!
! \begin{verbatim}
!       call ESMF_ConfigFindLabel(cf, 'my_table_name::', rc = rc)
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
!       call  ESMF_ConfigFindLabel(cf, 'my_table_name::', rc = rc)
!       do i = 1, 7
!          call  call ESMF_ConfigNextLine( cf, rc = rc )
!          do j = 1, 3
!             table(i,j) = ESMF_ConfigGetFloat( cf, rc = rc )
!          end do                   
!       end do
! \end{verbatim}
!
! The work with the configuration {\tt cf} is finalized by call to
! ${\tt ESMF\_ConfigDestroy()}$:
! \begin{verbatim}
!       integer rc
!       call ESMF_ConfigDestroy( cf, rc )
! \end{verbatim}

!
! \subsubsection{Main Routine/Functions}
!
! \begin{verbatim}
!  ------------------------------------------------------------------
!         Routine/Function                      Description
!  ------------------------------------------------------------------
!  ESMF_ConfigCreate ( fname, layout, rc )  creates configuration
!  ESMF_ConfigDestroy ( cf, rc )            destroys configuration
!
!  ESMF_ConfigLoadFile ( cf, fname, unique, loads resource file 
!                        rc )               into memory
!
!  ESMF_ConfigFindLabel( cf, label, unique, selects a label (key)
!                        rc )  
!
!  ESMF_ConfigNextLine ( cf, end, rc )      selects next line (for 
!                                           tables)
!
!  ESMF_ConfigGetFloat ( cf, label, size,   returns next float number 
!                        default, rc )      (function)
!
!  ESMF_ConfigGetInt ( cf, label, size,     returns next integer number 
!                      default, rc )        (function)
! 
!  ESMF_ConfigGetChar ( cf, label, rc )     returns next charecter array
!                                           /word (function)
!
!  ESMF_ConfigGetString ( cf, label,        retutns next string 
!                         string, rc )
!
!  ESMF_ConfigStringtoFloat ( string, rc )  transfrorms ASCII string to
!                                           float (function)
!
!  ESMF_ConfigStringtoInt ( string, rc )    transfrorms ASCII string to
!                                           integer (function)
!
!  ESMF_ConfigGetLen ( cf, label, unique,   gets number of words in the line
!                      rc)                  by counting disregarding type 
!                                           (function)
!
!  ESMF_ConfigGetDim ( cf, label, lines,    gets number of lines in the tables
!                      columns, unique, rc) and max number of columns by word 
!                                           counting disregarding type
!                                           (function)
!  ------------------------------------------------------------------
! \end{verbatim}
!
! {\em Common Arguments:}
!
! \begin{verbatim}
! character*(*)      fname       file name
! integer            rc        error return code (0 is OK)
! character*(*)      label       label (key) to locate record
! character*(*)      word        blank delimited string
! character*(*)      string      a sequence of characters
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
!       2apr2003 Leonid Zaslavsky created from m_inpak90.F90

! !USES:

      use ESMF_DELayoutMod

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
       public :: ESMF_ConfigGetFloat ! returns next float number (function) 
       public :: ESMF_ConfigGetInt   ! returns next integer number (function)
!       public :: ESMF_ConfigGetChar  ! returns next char array/word (function)
!       public :: ESMF_ConfigGetString     ! retutns next string 
       public :: ESMF_ConfigStringtoFloat ! ASCII string to float (function)
       public :: ESMF_ConfigStringtoInt   ! ASCII string to integer (function)
       public :: ESMF_ConfigGetLen ! gets number of words in the line(funcion)
       public :: ESMF_ConfigGetDim ! gets number of lines in the table
                                   ! and max number of columns by word 
!                                  !counting disregarding type (function)
!------------------------------------------------------------------------------
! !PUBLIC TYPES:
!------------------------------------------------------------------------------
       public :: ESMF_Config     ! WHY DO WE NEED IT PUBLIC?
!EOP

! PRIVATE PARAMETER  SETTINGS:
!------------------------------------------------------------------------------
! Revised parameter table to fit Fortran 90 standard.

     integer,   parameter :: LSZ = 256
#ifndef sysLinux
     integer,   parameter :: NBUF_MAX = 400*(LSZ) ! max size of buffer
#else
!ams
! On Linux with the Fujitsu compiler, I needed to reduce NBUF_MAX
!ams 
     integer,   parameter :: NBUF_MAX = 200*(LSZ) ! max size of buffer
#endif

     character, parameter :: BLK = achar(32)   ! blank (space)
     character, parameter :: TAB = achar(09)   ! TAB
     character, parameter :: EOL = achar(10)   ! end of line mark (newline)
     character, parameter :: EOB = achar(00)   ! end of buffer mark (null)
     character, parameter :: NULL= achar(00)   ! what it says

     integer,parameter :: MALLSIZE_=10	       ! just an estimation

     integer,parameter :: ESMF_Config_MXDEP = 4
     integer,save      :: ESMF_Config_depth = 0

     character(len=*), parameter :: myname='ESMF_ConfigMod'
!-----------------------------------------------------------------------
!------------------------------------------------------------------------------
! !PUBLIC TYPES:
!------------------------------------------------------------------------------
       type ESMF_Config
          integer :: nbuf                              ! actual size of buffer
          character(len=NBUF_MAX),pointer :: buffer    ! hold the whole file?
          character(len=LSZ),     pointer :: this_line ! the current line
          integer :: next_line                         ! index_ for next line 
                                                       ! on buffer
          type(ESMF_Config), pointer :: last
       end type ESMF_Config
!------------------------------------------------------------------------------
! !PRIVATE TYPES:
!------------------------------------------------------------------------------
       type(ESMF_Config),save,pointer :: ESMF_Config_now



      contains
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
! Earth System Modeling Framework
!BOP -------------------------------------------------------------------
!
! !IROUTINE: ESMF_ConfigCreate - Create Configuration
!
! !DESCRIPTION: Create ESMF configuration for a given layout.
!
! !INTERFACE:

    type(ESMF_Config) function ESMF_ConfigCreate( layout, rc )

      implicit none

      type(ESMF_DELayout), intent(in), optional  :: layout   ! ESMF layout
      integer,intent(out), optional              :: rc       ! error code
      !
! !REVISION HISTORY:
! 	7anp2003  Zaslavsky  initial interface/prolog
!EOP -------------------------------------------------------------------

      type(ESMF_Config) :: cf_local
      
      cf_local%next_line = 0
      rc = 0
      ESMF_ConfigCreate = cf_local

  end function ESMF_ConfigCreate


!-----------------------------------------------------------------------
! Earth System Modeling Framework
!BOP -------------------------------------------------------------------
!
! !IROUTINE: ESMF_ConfigDestroy - destroys configuration
!
! !DESCRIPTION: Destroys an ESMF configuration.
!
! !INTERFACE:

    subroutine ESMF_ConfigDestroy( cf, rc )

      implicit none
      type(ESMF_Config), intent(in) :: cf       ! ESMF Configuration
      integer,intent(out), optional :: rc       ! error code
!
! !REVISION HISTORY:
! 	7anp2003  Zaslavsky  initial interface/prolog
!
!EOP -------------------------------------------------------------------

      rc = 0
    end subroutine ESMF_ConfigDestroy


!-----------------------------------------------------------------------
! Earth System Modeling Framework
!BOP -------------------------------------------------------------------
!
! !IROUTINE: ESMF_ConfigLoadFile - load resource file into memory
!
! !DESCRIPTION: Resource file fname is loaded is loaded into memory
!
! !INTERFACE:

    subroutine ESMF_ConfigLoadFile( cf, fname, unique, rc )


      implicit none

      type(ESMF_Config), intent(in) :: cf        ! ESMF Configuration
      character(len=*), intent(in)  :: fname     ! file name
      logical, intent(out), optional :: unique   ! if present, uniqueness
                                                 ! of labels is checked
                                                 ! and true/false returned
      integer, intent(out), optional :: rc       ! Error code
!
! !REVISION HISTORY:
! 	7anp2003  Zaslavsky  initial interface/prolog
!EOP -------------------------------------------------------------------
      end subroutine


!-----------------------------------------------------------------------
! Earth System Modeling Framework
!BOP -------------------------------------------------------------------
!
! !IROUTINE: ESMF_ConfigFindLabel - finds the label (key)
!
! !DESCRIPTION: Finds the label (key) in the resource file. If "unique"
!               is present, uniqueness of the label is verified.
!
!               Since the search is done by looking for a word in the 
!               whole resource file, it is important to use special 
!               conventions to distinguish labels from other words 
!               in the resource files. The DAO convention is to finish 
!               line labels by : and tabel labels by ::..
!
! !INTERFACE:

    subroutine ESMF_ConfigFindLabel( cf, label, unique, rc )

      implicit none

      type(ESMF_Config), intent(in)  :: cf       ! ESMF Configuration
      character(len=*), intent(in)   :: label    ! label
      logical, intent(out), optional :: unique   ! if present, uniqueness
                                                 ! of the label is checked
                                                 ! and true/false returned
      integer, intent(out), optional  :: rc      ! Error code
!
! !REVISION HISTORY:
! 	7anp2003  Zaslavsky  initial interface/prolog
!EOP -------------------------------------------------------------------

    end subroutine ESMF_ConfigFindLabel


!-----------------------------------------------------------------------
! Earth System Modeling Framework
!BOP -------------------------------------------------------------------
!
! !IROUTINE: ESMF_ConfigNextLine - finds next line
!
! !DESCRIPTION: selects the next line (for tables)

!
! !INTERFACE:

    subroutine ESMF_ConfigNextLine( cf, end, rc)

      implicit none

      type(ESMF_Config), intent(in)  :: cf       ! ESMF Configuration
      logical, intent(out), optional :: end      ! if present, end of the
                                                 ! table mark (::) is checked

      integer, intent(out), optional:: rc        ! Error code
!
! !REVISION HISTORY:
! 	7anp2003  Zaslavsky  initial interface/prolog
!
!EOP -------------------------------------------------------------------
!
    end subroutine ESMF_ConfigNextLine


!-----------------------------------------------------------------------
! Earth System Modeling Framework
!BOP -------------------------------------------------------------------
!
! !IROUTINE: ESMF_ConfigGetFloat - gets a floating point number/numbers
!
! !DESCRIPTION: Gets a floating point number/numbers

!
! !INTERFACE:

    real function ESMF_ConfigGetFloat( cf, label, size, default, rc )

      implicit none

      type(ESMF_Config), intent(in)          :: cf       ! ESMF Configuration
      character(len=*), intent(in), optional :: label    ! label
      integer, intent(in), optional          :: size     ! number of floating 
                                                         ! point numbers
      real, intent(in), optional             :: default  ! default value

      integer, intent(out), optional         :: rc       ! Error code
!
! !REVISION HISTORY:
! 	7anp2003  Zaslavsky  initial interface/prolog
!
!EOP -------------------------------------------------------------------
!
      rc = 0
      ESMF_ConfigGetFloat = 0.0
    end function ESMF_ConfigGetFloat



!-----------------------------------------------------------------------
! Earth System Modeling Framework
!BOP -------------------------------------------------------------------
!
! !IROUTINE: ESMF_ConfigGetInt - gets an integer number/numbers
!
! !DESCRIPTION: Gets a floating point number/numbers

!
! !INTERFACE:

    integer function ESMF_ConfigGetInt( cf, label, size, default, rc )

      implicit none

      type(ESMF_Config), intent(in)          :: cf       ! ESMF Configuration
      character(len=*), intent(in), optional :: label    ! label
      integer, intent(in), optional          :: size     ! number of floating 
                                                         ! point numbers
      integer, intent(in), optional          :: default  ! default value

      integer, intent(out), optional         :: rc       ! Error code
!
! !REVISION HISTORY:
! 	7anp2003  Zaslavsky  initial interface/prolog
!
!EOP -------------------------------------------------------------------
      rc = 0
      ESMF_ConfigGetInt = 0
    end function ESMF_ConfigGetInt




!!!! For char and string ----- difference ????


!-----------------------------------------------------------------------
! Earth System Modeling Framework
!BOP -------------------------------------------------------------------
! !IROUTINE: ESMF_ConfigGetLen - gets the length of the line in words
!
! !DESCRIPTION: Gets the length of the line in words by counting words
!               disregarding types
!
! !INTERFACE:

    integer function ESMF_ConfigGetLen( cf, label, unique, rc )

      implicit none

      type(ESMF_Config), intent(in)          :: cf    ! ESMF Configuration

      character(len=*), intent(in), optional :: label ! label (if presented)
                                                      ! otherwise, current
                                                      ! line

      logical, intent(out), optional :: unique        ! if present, uniqueness
                                                      ! of labels is checked
                                                      ! and true/false returned

      integer, intent(out), optional :: rc            ! Error code
!
! !REVISION HISTORY:
! 	7anp2003  Zaslavsky  initial interface/prolog
!
!EOP -------------------------------------------------------------------
      ESMF_ConfigGetLen = 0
      rc = 0
    end function ESMF_ConfigGetLen


!-----------------------------------------------------------------------
! Earth System Modeling Framework
!BOP -------------------------------------------------------------------
!
! !IROUTINE: ESMF_ConfigGetDim - gets table sizes
!
! !DESCRIPTION: Gets number of lines in the table and max number of 
!               words in a table line
!
! !INTERFACE:

    subroutine ESMF_ConfigGetDim( cf, label, lines, columns, unique, rc )

      implicit none

      type(ESMF_Config), intent(in)          :: cf    ! ESMF Configuration

     integer, intent(out)                    :: lines
     integer, intent(out)                    :: columns  

      character(len=*), intent(in), optional :: label ! label (if presented)
                                                      ! otherwise, current
                                                      ! line



      logical, intent(out), optional        :: unique ! if present, uniqueness
                                                      ! of labels is checked
                                                      ! and true/false returned

      integer, intent(out), optional        :: rc     ! Error code
!
! !REVISION HISTORY:
! 	7anp2003  Zaslavsky  initial interface/prolog
!
!EOP -------------------------------------------------------------------

    end subroutine ESMF_ConfigGetDim
    

!-----------------------------------------------------------------------
! Earth System Modeling Framework
!BOP -------------------------------------------------------------------
!
! !IROUTINE: ESMF_ConfigStringtoFloat - transform string to float
!
! !DESCRIPTION: Transform a character string, presumably containing 
!               a flaoating point number, to a floating point format.
!
! !INTERFACE:

    real function ESMF_ConfigStringtoFloat( string, rc )

      implicit none

      character(len=*), intent(in)           :: string   ! ASCII string

      integer, intent(out), optional         :: rc       ! Error code
!
! !REVISION HISTORY:
! 	7anp2003  Zaslavsky  initial interface/prolog
!
!EOP -------------------------------------------------------------------
      rc = 0
      ESMF_ConfigStringtoFloat = 0.0
    end function ESMF_ConfigStringtoFloat

!-----------------------------------------------------------------------
! Earth System Modeling Framework
!BOP -------------------------------------------------------------------
!
! !IROUTINE: ESMF_ConfigStringtoInt - transform string to integer
!
! !DESCRIPTION: Transform a character string, presumably containing 
!               an integer number, to an integer point format.
!
! !INTERFACE:

    integer function ESMF_ConfigStringtoInt( string, rc )

      implicit none

      character(len=*), intent(in)           :: string   ! ASCII string

      integer, intent(out), optional         :: rc       ! Error code
!
! !REVISION HISTORY:
! 	7anp2003  Zaslavsky  initial interface/prolog
!
!EOP -------------------------------------------------------------------

      rc = 0
      ESMF_ConfigStringtoInt = 0
    end function ESMF_ConfigStringtoInt

    
  end module ESMF_ConfigMod
