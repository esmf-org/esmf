! $Id: ESMF_String.F90,v 1.1 2004/04/23 21:05:43 nscollins Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2003, University Corporation for Atmospheric Research, 
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
! Laboratory, University of Michigan, National Centers for Environmental 
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
! NASA Goddard Space Flight Center.
! Licensed under the GPL.
!
!==============================================================================
!
!     ESMF String module
      module ESMF_StringMod
!
!==============================================================================
! A blank line to keep protex happy.
!BOP

!EOP
!
! This file contains miscellaneous string print routines for use in
! Print and Validate and debugging code
!
!------------------------------------------------------------------------------
! INCLUDES

#include "ESMF.h"

!------------------------------------------------------------------------------
!BOPI
! !MODULE: ESMF_StringMod - Miscellaneous string print routines
!
! !DESCRIPTION:
!
! The code in this file implements the Fortran interfaces to routines
! which turn ESMF derived types into printable strings.
!
!
! !USES:
      use ESMF_BaseMod
      use ESMF_DataMapMod

      implicit none


      public ESMF_StringStatus, ESMF_StringDataType
      public ESMF_StringDataKind, ESMF_StringLogical

      public ESMF_StringRelLoc, ESMF_StringInterleave, ESMF_StringIndexOrder

!------------------------------------------------------------------------- 
contains
!------------------------------------------------------------------------- 

!------------------------------------------------------------------------- 
! misc print routines
!------------------------------------------------------------------------- 
!BOPI 
!  !IROUTINE:  ESMF_StringStatus - Return status as a string
!  
! !INTERFACE: 
      subroutine ESMF_StringStatus(status, string, rc)
!
! !ARGUMENTS:
      type(ESMF_Status), intent(in) :: status
      character(len=*), intent(out) :: string
      integer, intent(out), optional :: rc  

!
! !DESCRIPTION:
!   Return a status variable as a string.
!
!
!     The arguments are:
!     \begin{description}
!     \item[status]
!       The ESMF\_Status of a string.
!     \item[string]
!       The status string.
!     \item[{[rc]}]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI
! !REQUIREMENTS:

      if (status .eq. ESMF_STATE_UNINIT) string = "Uninitialized"
      if (status .eq. ESMF_STATE_READY) string = "Ready"
      if (status .eq. ESMF_STATE_UNALLOCATED) string = "Unallocated"
      if (status .eq. ESMF_STATE_ALLOCATED) string = "Allocated"
      if (status .eq. ESMF_STATE_BUSY) string = "Busy"
      if (status .eq. ESMF_STATE_INVALID) string = "Invalid"
 
      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_StringStatus

!------------------------------------------------------------------------- 
!BOPI 
!  !IROUTINE:  ESMF_StringDataType - Return DataType as a string
!  
! !INTERFACE: 
      subroutine ESMF_StringDataType(datatype, string, rc)
!
! !ARGUMENTS:
      type(ESMF_DataType), intent(in) :: datatype
      character(len=*), intent(out) :: string
      integer, intent(out), optional :: rc  

!
! !DESCRIPTION:
!   Return a datatype variable as a string.
!
!
!     The arguments are:
!     \begin{description}
!     \item[datatype]
!       The ESMF\_DataType of a string.
!     \item[string]
!       The status string.
!     \item[{[rc]}]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS:

      if (datatype .eq. ESMF_DATA_INTEGER) string = "Integer"
      if (datatype .eq. ESMF_DATA_REAL) string = "Real"
      if (datatype .eq. ESMF_DATA_LOGICAL) string = "Logical"
      if (datatype .eq. ESMF_DATA_CHARACTER) string = "Character"
 
      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_StringDataType

!------------------------------------------------------------------------- 
!BOPI 
!  !IROUTINE:  ESMF_StringDataKind - Return DataKind as a string
!  
! !INTERFACE: 
      subroutine ESMF_StringDataKind(datakind, string, rc)
!
! !ARGUMENTS:
      type(ESMF_DataKind), intent(in) :: datakind
      character(len=*), intent(out) :: string
      integer, intent(out), optional :: rc  

!
! !DESCRIPTION:
!   Return a datakind variable as a string.
!
!     The arguments are:
!     \begin{description}
!     \item[datakind]
!       The ESMF\_DataKind of a string.
!     \item[string]
!       The status string.
!     \item[{[rc]}]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI
! !REQUIREMENTS:

      if (datakind .eq. ESMF_I1)  string = "Integer*1"
      if (datakind .eq. ESMF_I2)  string = "Integer*2"
      if (datakind .eq. ESMF_I4)  string = "Integer*4"
      if (datakind .eq. ESMF_I8)  string = "Integer*8"
      if (datakind .eq. ESMF_R4)  string = "Real*4"
      if (datakind .eq. ESMF_R8)  string = "Real*8"
      if (datakind .eq. ESMF_C8)  string = "Complex*8"
      if (datakind .eq. ESMF_C16) string = "Complex*16"
 
      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_StringDataKind

!------------------------------------------------------------------------- 
!BOPI 
!  !IROUTINE:  ESMF_StringLogical - Return Logical as a string
!  
! !INTERFACE: 
      subroutine ESMF_StringLogical(tf, string, rc)
!
! !ARGUMENTS:
      type(ESMF_Logical), intent(in) :: tf
      character(len=*), intent(out) :: string
      integer, intent(out), optional :: rc  

!
! !DESCRIPTION:
!   Return a tf variable as a string.
!
!     The arguments are:
!     \begin{description}
!     \item[tf]
!       The ESMF\_Logical of a string.
!     \item[string]
!       The status string.
!     \item[{[rc]}]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI
! !REQUIREMENTS:

      if (tf .eq. ESMF_TRUE)  string = "True"
      if (tf .eq. ESMF_FALSE) string = "False"
 
      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_StringLogical

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_StringRelLoc - Return a relloc as a string
!
! !INTERFACE:
      subroutine ESMF_StringRelLoc(relloc, string, rc)
!
!
! !ARGUMENTS:
      type(ESMF_RelLoc), intent(in) :: relloc
      character (len = *), intent(out) :: string
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!      Routine to turn a relloc into a string.
!
!     The arguments are:
!     \begin{description}
!     \item [relloc]
!           The {\tt ESMF\_RelLoc} object to be turned into a string.
!     \item [string]
!          Return string.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!       \end{description}
!
!
!EOP
! !REQUIREMENTS:

        if (relloc .eq. ESMF_CELL_UNDEFINED) string = "Undefined"
        if (relloc .eq. ESMF_CELL_CENTER)    string = "Cell Center"
        if (relloc .eq. ESMF_CELL_NFACE)     string = "Cell North Face"
        if (relloc .eq. ESMF_CELL_SFACE)     string = "Cell South Face"
        if (relloc .eq. ESMF_CELL_EFACE)     string = "Cell East Face"
        if (relloc .eq. ESMF_CELL_WFACE)     string = "Cell West Face"
        if (relloc .eq. ESMF_CELL_NECORNER)  string = "Cell NorthEast Corner"
        if (relloc .eq. ESMF_CELL_NWCORNER)  string = "Cell NorthWest Corner"
        if (relloc .eq. ESMF_CELL_SECORNER)  string = "Cell SouthEast Corner"
        if (relloc .eq. ESMF_CELL_SWCORNER)  string = "Cell SouthWest Corner"
        if (relloc .eq. ESMF_CELL_CELL)      string = "Full Cell"
        if (relloc .eq. ESMF_CELL_VERTEX)    string = "Cell Vertex"

        if (present(rc)) rc = ESMF_SUCCESS

        end subroutine ESMF_StringRelLoc

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_StringInterleave - Return a interleave as a string
!
! !INTERFACE:
      subroutine ESMF_StringInterleave(interleave, string, rc)
!
!
! !ARGUMENTS:
      type(ESMF_InterleaveType), intent(in) :: interleave
      character (len = *), intent(out) :: string
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!      Routine to turn an interleave into a string.
!
!     The arguments are:
!     \begin{description}
!     \item [interleave]
!           The {\tt ESMF\_InterleaveType} object to be turned into a string.
!     \item [string]
!          Return string.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!       \end{description}
!
!
!EOP
! !REQUIREMENTS:

        if (interleave .eq. ESMF_IL_BLOCK) string = "Block Interleave"
        if (interleave .eq. ESMF_IL_ITEM) string = "Item Interleave"

        if (present(rc)) rc = ESMF_SUCCESS

        end subroutine ESMF_StringInterleave

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_StringIndexOrder - Return a indexorder as a string
!
! !INTERFACE:
      subroutine ESMF_StringIndexOrder(indexorder, string, rc)
!
!
! !ARGUMENTS:
      type(ESMF_IndexOrder), intent(in) :: indexorder   ! turn into string
      character (len = *), intent(out) :: string        ! where to return it
      integer, intent(out), optional :: rc              ! return code
!
! !DESCRIPTION:
!      Routine to turn an indexorder into a string.
!
!      The arguments are:
!     \begin{description}
!     \item [indexorder]
!           The {\tt ESMF\_IndexOrder} object to be turned into a string.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!       \end{description}
!
!EOP
! !REQUIREMENTS:

        if (indexorder.eq.ESMF_INDEX_I  ) string = "I"
        if (indexorder.eq.ESMF_INDEX_IJ ) string = "IJ"
        if (indexorder.eq.ESMF_INDEX_JI ) string = "JI"
        if (indexorder.eq.ESMF_INDEX_IJK) string = "IJK"
        if (indexorder.eq.ESMF_INDEX_JIK) string = "JIK"
        if (indexorder.eq.ESMF_INDEX_KJI) string = "KJI"
        if (indexorder.eq.ESMF_INDEX_IKJ) string = "IKJ"
        if (indexorder.eq.ESMF_INDEX_JKI) string = "JKI"
        if (indexorder.eq.ESMF_INDEX_KIJ) string = "KIJ"

        if (present(rc)) rc = ESMF_SUCCESS

        end subroutine ESMF_StringIndexOrder

!------------------------------------------------------------------------------

        end module ESMF_StringMod
