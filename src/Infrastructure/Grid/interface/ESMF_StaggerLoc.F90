! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2018, University Corporation for Atmospheric Research,
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
      use ESMF_StaggerLocTypeMod
      use ESMF_LogErrMod
      use ESMF_IOUtilMod

      implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
      private

!------------------------------------------------------------------------------
!
! !PUBLIC TYPES:
  public ESMF_StaggerLoc

!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:
  public ESMF_StaggerLocGet
  public ESMF_StaggerLocSet
  public ESMF_StaggerLocString
  public ESMF_StaggerLocPrint
  public assignment(=)
  public operator(==), operator(/=) 
  public operator(>), operator(>=) 
  public operator(<), operator(<=) 

!------------------------------------------------------------------------------
!
! !PUBLIC DATA MEMBERS:
! 
! For definition of stagger locations see the file Grid\_options.tex.
!
!!!!!

   !! Invalid stagger locations
   public ESMF_STAGGERLOC_INVALID, ESMF_STAGGERLOC_UNINIT

   !! 2D predefined stagger locations
   public ESMF_STAGGERLOC_CENTER, ESMF_STAGGERLOC_EDGE1, &
     ESMF_STAGGERLOC_EDGE2, ESMF_STAGGERLOC_CORNER

   !! 3D predefined stagger locations
   public ESMF_STAGGERLOC_CENTER_VCENTER, &
     ESMF_STAGGERLOC_EDGE1_VCENTER,       &
     ESMF_STAGGERLOC_EDGE2_VCENTER,       &
     ESMF_STAGGERLOC_CORNER_VCENTER,      &
     ESMF_STAGGERLOC_CENTER_VFACE,        &
     ESMF_STAGGERLOC_EDGE1_VFACE,         &
     ESMF_STAGGERLOC_EDGE2_VFACE,         &
     ESMF_STAGGERLOC_CORNER_VFACE


!EOPI
!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id$'

!==============================================================================


!==============================================================================
!
! INTERFACE BLOCKS
!
!==============================================================================

! -------------------------- ESMF-public method -------------------------------
!BOPI
! !IROUTINE: ESMF_StaggerLocGet -- Generic interface

! !INTERFACE:
interface ESMF_StaggerLocGet

! !PRIVATE MEMBER FUNCTIONS:
!
      module procedure ESMF_StaggerLocGetDim
      
! !DESCRIPTION: 
! This interface provides a single entry point for the various 
!  types of {\tt ESMF\_StaggerLocGet} functions.   
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

!==============================================================================

      contains

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StaggerLocGetDim"
!BOP
! !IROUTINE: ESMF_StaggerLocGet - Get the value of one dimension of a StaggerLoc

! !INTERFACE:
  ! Private name; call using ESMF_StaggerLocGet() 
      subroutine ESMF_StaggerLocGetDim(staggerloc, dim, loc, &
           keywordEnforcer, rc)
!
! !ARGUMENTS:
      type (ESMF_StaggerLoc), intent(in)  :: staggerloc
      integer,                intent(in)  :: dim
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
      integer, optional,      intent(out) :: loc
      integer, optional                   :: rc 

! !DESCRIPTION:
!   Gets the position of a particular dimension of a cell {\tt staggerloc}
!   The argument {\tt loc} will be only be 0,1. 
!    If {\tt loc} is 0 it means the position 
!    should be in the center in that dimension. If {\tt loc} is +1 then
!    for the dimension, the position should be on the positive side of the cell. 
!    Please see Section~\ref{sec:usage:staggerloc:adv} for diagrams.
!
!     The arguments are:
!     \begin{description}
!     \item[staggerloc]
!          Stagger location for which to get information. 
!     \item[dim]
!          Dimension for which to get information (1-7).
!     \item[{[loc]}]
!          Output position data (should be either 0,1).
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
     integer :: tmp

     ! Initialize return code; assume routine not implemented
     if (present(rc)) rc = ESMF_RC_NOT_IMPL

     ! Get stagger location value
     if (present(loc)) then
        tmp=(staggerloc%staggerloc)/(2**(dim-1))
        loc=mod(tmp,2)
     endif

     ! Set return values.
     if (present(rc)) rc = ESMF_SUCCESS


     end subroutine ESMF_StaggerLocGetDim



!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StaggerLocSetAllDim"
!BOP
! !IROUTINE: ESMF_StaggerLocSet - Set a StaggerLoc to a particular position in the cell

! !INTERFACE:
  ! Private name; call using ESMF_StaggerLocSet() 
     subroutine ESMF_StaggerLocSetAllDim(staggerloc, loc, keywordEnforcer, rc)
!
! !ARGUMENTS:
      type (ESMF_StaggerLoc), intent(inout) :: staggerloc
      integer,                intent(in)    :: loc(:)
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
      integer, optional                     :: rc 

! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
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
       subroutine ESMF_StaggerLocSetDim(staggerloc, dim, loc, &
            keywordEnforcer, rc)
!
! !ARGUMENTS:
      type (ESMF_StaggerLoc), intent(inout) :: staggerloc
      integer,                intent(in)    :: dim
      integer,                intent(in)    :: loc
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
      integer, optional                     :: rc 

! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
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
      subroutine ESMF_StaggerLocString(staggerloc, string, &
           keywordEnforcer, rc)
!
!
! !ARGUMENTS:
      type(ESMF_StaggerLoc), intent(in)  :: staggerloc
      character (len = *),   intent(out) :: string
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
      integer, optional,     intent(out) :: rc
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
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
        if (staggerloc < ESMF_STAGGERLOC_CENTER) string="No String For This StaggerLoc" 
        if (staggerloc == ESMF_STAGGERLOC_CENTER) string="Center" 
         if (staggerloc == ESMF_STAGGERLOC_CORNER) string="Corner of Dim. 1 and Dim. 2" 
        if (staggerloc == ESMF_STAGGERLOC_EDGE1)  string="Middle of Face Offset in Dim. 1" 
        if (staggerloc == ESMF_STAGGERLOC_EDGE2)  string="Middle of Face Offset in Dim. 2" 
        if (staggerloc == ESMF_STAGGERLOC_CENTER_VFACE) string="Middle of Face Offset in Dim. 3"
        if (staggerloc == ESMF_STAGGERLOC_EDGE1_VFACE) string="Middle of Edge Offset in Dim. 1 and Dim. 3"
        if (staggerloc == ESMF_STAGGERLOC_EDGE2_VFACE) string="Middle of Edge Offset in Dim. 2 and Dim. 3"
        if (staggerloc == ESMF_STAGGERLOC_CORNER_VFACE) string="Corner of Dim. 1, Dim. 2, and Dim. 3"
        if (staggerloc > ESMF_STAGGERLOC_CORNER_VFACE) string="No String For This StaggerLoc" 

        if (present(rc)) rc = ESMF_SUCCESS

        end subroutine ESMF_StaggerLocString

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StaggerLocPrint"
!BOP
! !IROUTINE: ESMF_StaggerLocPrint - Print StaggerLoc information

! !INTERFACE:
      subroutine ESMF_StaggerLocPrint(staggerloc, keywordEnforcer, rc)
!
! !ARGUMENTS:
      type (ESMF_StaggerLoc), intent(in)  :: staggerloc
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
      integer, optional,      intent(out) :: rc 

! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
!     Print the internal data members of an {\tt ESMF\_StaggerLoc} object. \\
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

      write(ESMF_UtilIOStdout, *) "StaggerLoc Print Begins =====>"
      write(ESMF_UtilIOStdout, *) "   staggerloc = ", staggerloc%staggerloc
      write(ESMF_UtilIOStdout, *) "StaggerLoc Print Ends   =====>"

      rc = ESMF_SUCCESS

      end subroutine ESMF_StaggerLocPrint

      end module ESMF_StaggerLocMod

