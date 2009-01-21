! $Id: ESMF_StaggerLoc.F90,v 1.10.2.4 2009/01/21 21:25:21 cdeluca Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2009, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
!
#define ESMF_FILENAME "ESMF_StaggerLoc.F90"
!
!     ESMF Stagger Location
      module ESMF_StaggerLocMod
!
!==============================================================================
!
! This file contains the Stagger Location subroutines
!
!------------------------------------------------------------------------------
! INCLUDES
#include "ESMF.h"
!==============================================================================
!BOPI
! !MODULE: ESMF_StaggerMod - Stagger class
!
! !DESCRIPTION:
!
! The code in this file implements some routines for interacting with the general stagger.
!
!------------------------------------------------------------------------------
! !USES:
      use ESMF_UtilTypesMod    ! ESMF base class
      use ESMF_LogErrMod

      implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
      private

!------------------------------------------------------------------------------
!     ! ESMF_StaggerLoc
!

      type ESMF_StaggerLoc
      sequence
        integer :: staggerloc
      end type



!------------------------------------------------------------------------------
!
! !PUBLIC TYPES:
  public ESMF_StaggerLoc

!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:
  public ESMF_StaggerLocString
  public ESMF_StaggerLocSet
  public ESMF_StaggerLocPrint
  public operator(.eq.), operator(.ne.) 
  public operator(.gt.), operator(.ge.) 
  public operator(.lt.), operator(.le.) 

!------------------------------------------------------------------------------
!
! !PUBLIC DATA MEMBERS:
! 
! For definition of stagger locations see the file Grid\_options.tex.
!
!!!!!

   !! Invalid stagger locations
   type (ESMF_StaggerLoc), parameter, public ::            &
      ESMF_STAGGERLOC_INVALID       = ESMF_StaggerLoc(-2), &  
      ESMF_STAGGERLOC_UNINIT        = ESMF_StaggerLoc(-1)   

   !! 2D predefined stagger locations
   type (ESMF_StaggerLoc), parameter, public ::            &
      ESMF_STAGGERLOC_CENTER        = ESMF_StaggerLoc( 0), &     
      ESMF_STAGGERLOC_EDGE1         = ESMF_StaggerLoc( 1), &  
      ESMF_STAGGERLOC_EDGE2         = ESMF_StaggerLoc( 2), &
      ESMF_STAGGERLOC_CORNER        = ESMF_StaggerLoc( 3)

   !! 3D predefined stagger locations
   type (ESMF_StaggerLoc), parameter, public ::            &
      ESMF_STAGGERLOC_CENTER_VCENTER  = ESMF_StaggerLoc( 0), &
      ESMF_STAGGERLOC_EDGE1_VCENTER   = ESMF_StaggerLoc( 1), &
      ESMF_STAGGERLOC_EDGE2_VCENTER   = ESMF_StaggerLoc( 2), &
      ESMF_STAGGERLOC_CORNER_VCENTER  = ESMF_StaggerLoc( 3), &
      ESMF_STAGGERLOC_CENTER_VFACE    = ESMF_StaggerLoc( 4), &
      ESMF_STAGGERLOC_EDGE1_VFACE     = ESMF_StaggerLoc( 5), &
      ESMF_STAGGERLOC_EDGE2_VFACE     = ESMF_StaggerLoc( 6), &
      ESMF_STAGGERLOC_CORNER_VFACE    = ESMF_StaggerLoc( 7)


!EOPI
!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_StaggerLoc.F90,v 1.10.2.4 2009/01/21 21:25:21 cdeluca Exp $'


!==============================================================================


!==============================================================================
!
! INTERFACE BLOCKS
!
!==============================================================================
!BOPI
! !INTERFACE:
      interface operator (.eq.)

! !PRIVATE MEMBER FUNCTIONS:
         module procedure ESMF_StaggerLocEqual

! !DESCRIPTION:
!     This interface overloads the equality operator for the specific
!     ESMF StaggerLoc.  It is provided for easy comparisons of 
!     these types with defined values.
!
!EOPI
      end interface
!
!------------------------------------------------------------------------------
!BOPI
! !INTERFACE:
      interface operator (.ne.)

! !PRIVATE MEMBER FUNCTIONS:
         module procedure ESMF_StaggerLocNotEqual

! !DESCRIPTION:
!     This interface overloads the inequality operator for the specific
!     ESMF StaggerLoc.  It is provided for easy comparisons of 
!     these types with defined values.
!
!EOPI
      end interface
!------------------------------------------------------------------------------
!BOPI
! !INTERFACE:
      interface operator (.gt.)

! !PRIVATE MEMBER FUNCTIONS:
         module procedure ESMF_StaggerLocGreater

! !DESCRIPTION:
!     This interface overloads the inequality operator for the specific
!     ESMF StaggerLoc.  It is provided for easy comparisons of 
!     these types with defined values.
!
!EOPI
      end interface
!------------------------------------------------------------------------------
!BOPI
! !INTERFACE:
      interface operator (.lt.)

! !PRIVATE MEMBER FUNCTIONS:
         module procedure ESMF_StaggerLocLess

! !DESCRIPTION:
!     This interface overloads the inequality operator for the specific
!     ESMF StaggerLoc.  It is provided for easy comparisons of 
!     these types with defined values.
!
!EOPI
      end interface
!------------------------------------------------------------------------------
!BOPI
! !INTERFACE:
      interface operator (.ge.)

! !PRIVATE MEMBER FUNCTIONS:
         module procedure ESMF_StaggerLocGreaterEqual

! !DESCRIPTION:
!     This interface overloads the inequality operator for the specific
!     ESMF StaggerLoc.  It is provided for easy comparisons of 
!     these types with defined values.
!
!EOPI
      end interface
!------------------------------------------------------------------------------
!BOPI
! !INTERFACE:
      interface operator (.le.)

! !PRIVATE MEMBER FUNCTIONS:
         module procedure ESMF_StaggerLocLessEqual

! !DESCRIPTION:
!     This interface overloads the inequality operator for the specific
!     ESMF StaggerLoc.  It is provided for easy comparisons of 
!     these types with defined values.
!
!EOPI
      end interface



! -------------------------- ESMF-public method -------------------------------
!BOPI
! !IROUTINE: ESMF_StaggerLocSet -- Generic interface

! !INTERFACE:
interface ESMF_StaggerLocSet

! !PRIVATE MEMBER FUNCTIONS:
!
      module procedure ESMF_StaggerLocSetAllDim
      module procedure ESMF_StaggerLocSetDim
      
! !DESCRIPTION: 
! This interface provides a single entry point for the various 
!  types of {\tt ESMF\_StaggerLocSet} functions.   
!EOPI 
end interface

!
!==============================================================================

      contains



!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StaggerLocSetAllDim"
!BOP
! !IROUTINE: ESMF_StaggerLocSet - Set a StaggerLoc to a particular position in the cell

! !INTERFACE:
  ! Private name; call using ESMF_StaggerLocSet() 
     subroutine ESMF_StaggerLocSetAllDim(staggerloc,loc,rc)
!
! !ARGUMENTS:
      type (ESMF_StaggerLoc), intent(inout) :: staggerloc
      integer, intent(in) :: loc(:)
      integer, optional :: rc 

! !DESCRIPTION:
!    Sets a custom {\tt staggerloc} to a position in a cell by using the array
!    {\tt loc}. The values in the array should only be 0,1. If loc(i) is 0 it 
!    means the position should be in the center in that dimension. If loc(i) is 1 then
!    for dimension i, the position should be on the side of the cell. 
!    Please see Section~\ref{sec:usage:staggerloc:adv}
!    for diagrams and further discussion of custom stagger locations. 
!
!     The arguments are:
!     \begin{description}
!     \item[staggerloc]
!          Grid location to be initialized
!     \item[loc]
!          Array holding position data. Each entry in {\tt loc} should only
!          be  0 or 1. note that dimensions beyond those specified are set to 0. 
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP

      integer :: i,sl,loc_size

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      !TODO: error checking here

      ! set stagger location value
      sl=0
      loc_size=size(loc)
      do i=1,loc_size
         sl=sl+loc(i)*2**(i-1)
      enddo

     ! Set stagger location value
      staggerloc%staggerloc=sl 

      ! Set return values.
      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_StaggerLocSetAllDim


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StaggerLocSetDim"
!BOP
! !IROUTINE: ESMF_StaggerLocSet - Set one dimension of a StaggerLoc to a particular position

! !INTERFACE:
  ! Private name; call using ESMF_StaggerLocSet() 
      subroutine ESMF_StaggerLocSetDim(staggerloc,dim,loc,rc)
!
! !ARGUMENTS:
      type (ESMF_StaggerLoc), intent(inout) :: staggerloc
      integer, intent(in) :: dim,loc
      integer, optional :: rc 

! !DESCRIPTION:
!   Sets a particular dimension of a custom {\tt staggerloc} to a position in a cell 
!    by using the variable {\tt loc}. The variable {\tt loc} should only be 0,1. 
!    If {\tt loc} is 0 it means the position 
!    should be in the center in that dimension. If {\tt loc} is +1 then
!    for the dimension, the position should be on the positive side of the cell. 
!    Please see Section~\ref{sec:usage:staggerloc:adv}
!    for diagrams and further discussion of custom stagger locations. 
!
!     The arguments are:
!     \begin{description}
!     \item[staggerloc]
!          Stagger location to be initialized
!     \item[dim]
!          Dimension to be changed (1-7).
!     \item[loc]
!          Position data should be either 0,1.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP

     ! Initialize return code; assume routine not implemented
     if (present(rc)) rc = ESMF_RC_NOT_IMPL

     !TODO: error checking here


     ! Set stagger location value
     staggerloc%staggerloc=loc*2**(dim-1)


     ! Set return values.
     if (present(rc)) rc = ESMF_SUCCESS


     end subroutine ESMF_StaggerLocSetDim



!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StaggerLocString"
!BOP
! !IROUTINE:  ESMF_StaggerLocString - Return a StaggerLoc as a string
!
! !INTERFACE:
      subroutine ESMF_StaggerLocString(staggerloc, string, rc)
!
!
! !ARGUMENTS:
      type(ESMF_StaggerLoc), intent(in) :: staggerloc
      character (len = *), intent(out) :: string
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Return an {\tt ESMF\_StaggerLoc} as a printable string.
!
!     The arguments are:
!     \begin{description}
!     \item [staggerloc]
!           The {\tt ESMF\_StaggerLoc} to be turned into a string.
!     \item [string]
!          Return string.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP

        ! Initialize return code; assume routine not implemented
        if (present(rc)) rc = ESMF_RC_NOT_IMPL

        ! translate staggerloc to string
        ! (Strings should be appropriate for 2D and 3D)
        if (staggerloc .lt. ESMF_STAGGERLOC_CENTER) string="No String For This StaggerLoc" 
        if (staggerloc .eq. ESMF_STAGGERLOC_CENTER) string="Center" 
        if (staggerloc .eq. ESMF_STAGGERLOC_CORNER) string="Corner of Dim. 1 and Dim. 2" 
        if (staggerloc .eq. ESMF_STAGGERLOC_EDGE1)  string="Middle of Face Offset in Dim. 1" 
        if (staggerloc .eq. ESMF_STAGGERLOC_EDGE2)  string="Middle of Face Offset in Dim. 2" 
        if (staggerloc .eq. ESMF_STAGGERLOC_CENTER_VFACE) string="Middle of Face Offset in Dim. 3"
        if (staggerloc .eq. ESMF_STAGGERLOC_EDGE1_VFACE) string="Middle of Edge Offset in Dim. 1 and Dim. 3"
        if (staggerloc .eq. ESMF_STAGGERLOC_EDGE2_VFACE) string="Middle of Edge Offset in Dim. 2 and Dim. 3"
        if (staggerloc .eq. ESMF_STAGGERLOC_CORNER_VFACE) string="Corner of Dim. 1, Dim. 2, and Dim. 3"
        if (staggerloc .gt. ESMF_STAGGERLOC_CORNER_VFACE) string="No String For This StaggerLoc" 

        if (present(rc)) rc = ESMF_SUCCESS

        end subroutine ESMF_StaggerLocString
!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StaggerLocCustom"
!BOPI
! !IROUTINE: ESMF_StaggerLocCustom - Create a custom stagger location which doesn't necessarily correspond to one of the standard points in a cell

! !INTERFACE:
      subroutine ESMF_StaggerLocCustom(staggerloc,label,offsets,rc)
!
! !ARGUMENTS:
      type (ESMF_StaggerLoc), intent(inout) :: staggerloc
      integer , intent(in) :: label
      integer, intent(in),optional :: offsets(:)
      integer, optional :: rc 

! !DESCRIPTION:
!     Creates a custom stagger location which doesn't necessarily correspond to    
!     one of the standard locations in the cell. This is useful if you want to 
!     put data at a non-standard location. Note that because of their ambiguous location
!     these staggers won't be able to be used in some ESMF functionality. 
!
!     The arguments are:
!     \begin{description}
!     \item[staggerloc]
!          Grid location to be initialized
!     \item[label]
!          An integer which distinguishes this custom stagger from others. 
!     \item[offsets]
!          An array containing +1,0 which describes how this stagger should
!          be positioned in index space.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL


      end subroutine ESMF_StaggerLocCustom



!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StaggerLocEqual"
!BOPI
! !IROUTINE: ESMF_StaggerLocEqual - Equality of StaggerLoc statuses
!
! !INTERFACE:
      function ESMF_StaggerLocEqual(StaggerLoc1, StaggerLoc2)

! !RETURN VALUE:
      logical :: ESMF_StaggerLocEqual

! !ARGUMENTS:

      type (ESMF_StaggerLoc), intent(in) :: &
         StaggerLoc1,      &! Two igrid statuses to compare for
         StaggerLoc2        ! equality

! !DESCRIPTION:
!     This routine compares two ESMF StaggerLoc statuses to see if
!     they are equivalent.
!
!     The arguments are:
!     \begin{description}
!     \item[StaggerLoc1, StaggerLoc2]
!          Two igrid statuses to compare for equality
!     \end{description}
!
!EOPI

      ESMF_StaggerLocEqual = (StaggerLoc1%staggerloc == &
                              StaggerLoc2%staggerloc)

      end function ESMF_StaggerLocEqual
!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StaggerLocNotEqual"
!BOPI
! !IROUTINE: ESMF_StaggerLocNotEqual - Non-equality of StaggerLoc statuses
!
! !INTERFACE:
      function ESMF_StaggerLocNotEqual(StaggerLoc1, StaggerLoc2)

! !RETURN VALUE:
      logical :: ESMF_StaggerLocNotEqual

! !ARGUMENTS:

      type (ESMF_StaggerLoc), intent(in) :: &
         StaggerLoc1,      &! Two StaggerLoc Statuses to compare for
         StaggerLoc2        ! inequality

! !DESCRIPTION:
!     This routine compares two ESMF StaggerLoc statuses to see if
!     they are unequal.
!
!     The arguments are:
!     \begin{description}
!     \item[StaggerLoc1, StaggerLoc2]
!          Two statuses of StaggerLocs to compare for inequality
!     \end{description}
!
!EOPI

      ESMF_StaggerLocNotEqual = (StaggerLoc1%staggerloc /= &
                                 StaggerLoc2%staggerloc)

      end function ESMF_StaggerLocNotEqual


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StaggerLocGreater"
!BOPI
! !IROUTINE: ESMF_StaggerLocGreater - Equality of StaggerLoc statuses
!
! !INTERFACE:
      function ESMF_StaggerLocGreater(StaggerLoc1, StaggerLoc2)

! !RETURN VALUE:
      logical :: ESMF_StaggerLocGreater

! !ARGUMENTS:

      type (ESMF_StaggerLoc), intent(in) :: &
         StaggerLoc1,      &! Two igrid statuses to compare for
         StaggerLoc2        ! equality

! !DESCRIPTION:
!     This routine compares two ESMF StaggerLoc statuses to see if
!     they are equivalent.
!
!     The arguments are:
!     \begin{description}
!     \item[StaggerLoc1, StaggerLoc2]
!          Two igrid statuses to compare for equality
!     \end{description}
!
!EOPI

      ESMF_StaggerLocGreater = (StaggerLoc1%staggerloc .gt. &
                              StaggerLoc2%staggerloc)

      end function ESMF_StaggerLocGreater
!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StaggerLocLess"
!BOPI
! !IROUTINE: ESMF_StaggerLocLess - Non-equality of StaggerLoc statuses
!
! !INTERFACE:
      function ESMF_StaggerLocLess(StaggerLoc1, StaggerLoc2)

! !RETURN VALUE:
      logical :: ESMF_StaggerLocLess

! !ARGUMENTS:

      type (ESMF_StaggerLoc), intent(in) :: &
         StaggerLoc1,      &! Two StaggerLoc Statuses to compare for
         StaggerLoc2        ! inequality

! !DESCRIPTION:
!     This routine compares two ESMF StaggerLoc statuses to see if
!     they are unequal.
!
!     The arguments are:
!     \begin{description}
!     \item[StaggerLoc1, StaggerLoc2]
!          Two statuses of StaggerLocs to compare for inequality
!     \end{description}
!
!EOPI

      ESMF_StaggerLocLess = (StaggerLoc1%staggerloc .lt. &
                                 StaggerLoc2%staggerloc)

      end function ESMF_StaggerLocLess

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StaggerLocGreaterEqual"
!BOPI
! !IROUTINE: ESMF_StaggerLocGreaterEqual - Greater than or equal of StaggerLoc statuses
!
! !INTERFACE:
      function ESMF_StaggerLocGreaterEqual(StaggerLoc1, StaggerLoc2)

! !RETURN VALUE:
      logical :: ESMF_StaggerLocGreaterEqual

! !ARGUMENTS:

      type (ESMF_StaggerLoc), intent(in) :: &
         StaggerLoc1,      &! Two igrid statuses to compare for
         StaggerLoc2        ! equality

! !DESCRIPTION:
!     This routine compares two ESMF StaggerLoc statuses to see if
!     they are equivalent.
!
!     The arguments are:
!     \begin{description}
!     \item[StaggerLoc1, StaggerLoc2]
!          Two igrid statuses to compare
!     \end{description}
!
!EOPI

      ESMF_StaggerLocGreaterEqual = (StaggerLoc1%staggerloc .ge. &
                              StaggerLoc2%staggerloc)

      end function ESMF_StaggerLocGreaterEqual
!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StaggerLocLessEqual"
!BOPI
! !IROUTINE: ESMF_StaggerLocLessEqual - Less than or equal of StaggerLoc statuses
!
! !INTERFACE:
      function ESMF_StaggerLocLessEqual(StaggerLoc1, StaggerLoc2)

! !RETURN VALUE:
      logical :: ESMF_StaggerLocLessEqual

! !ARGUMENTS:

      type (ESMF_StaggerLoc), intent(in) :: &
         StaggerLoc1,      &! Two StaggerLoc Statuses to compare for
         StaggerLoc2        ! inequality

! !DESCRIPTION:
!     This routine compares two ESMF StaggerLoc statuses to see if
!     they are unequal.
!
!     The arguments are:
!     \begin{description}
!     \item[StaggerLoc1, StaggerLoc2]
!          Two statuses of StaggerLocs to compare
!     \end{description}
!
!EOPI

      ESMF_StaggerLocLessEqual = (StaggerLoc1%staggerloc .le. &
                                 StaggerLoc2%staggerloc)

      end function ESMF_StaggerLocLessEqual

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StaggerLocPrint"
!BOP
! !IROUTINE: ESMF_StaggerLocPrint - Print information of a ESMF_StaggerLoc object

! !INTERFACE:
      subroutine ESMF_StaggerLocPrint(staggerloc, rc)
!
! !ARGUMENTS:
      type (ESMF_StaggerLoc), intent(in) :: staggerloc
      integer, intent(out), optional     :: rc 

! !DESCRIPTION:
!     Print the internal data members of a ESMF\_StaggerLoc object
!
!     Note:  Many {\tt ESMF\_<class>Print} methods are implemented in C++.
!     On some platforms/compilers there is a potential issue with interleaving
!     Fortran and C++ output to {\tt stdout} such that it doesn't appear in
!     the expected order.  If this occurs, it is recommended to use the
!     standard Fortran call {\tt flush(6)} as a workaround until this issue
!     is fixed in a future release. 
!
!     The arguments are:
!     \begin{description}
!     \item[staggerloc]
!          ESMF\_StaggerLoc object as the method input
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      write(*, *) "StaggerLoc Print Begins =====>"
      write(*, *) "   staggerloc = ", staggerloc%staggerloc
      write(*, *) "StaggerLoc Print Ends   =====>"

      rc = ESMF_SUCCESS

      end subroutine ESMF_StaggerLocPrint

      end module ESMF_StaggerLocMod

