! $Id: ESMF_XPacket.F90,v 1.11 2004/06/08 09:27:20 nscollins Exp $
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
#define ESMF_FILENAME "ESMF_XPacket.F90"
!
!     ESMF XPacket Module
      module ESMF_XPacketMod
!
!==============================================================================
!
! This file contains the F90 wrapper code for the C++ implementation of
!  the XPacket class.
!
!------------------------------------------------------------------------------
! INCLUDES
#include "ESMF.h"
!#include "ESMF_Route.h"
!==============================================================================
!BOP
! !MODULE: ESMF_XPacketMod - Multi-dimensional description of a rectangular block of memory
!
! !DESCRIPTION:
!
! The code in this file implements the F90 wrapper code for the C++
!  implementation of the {\tt XPacket} class.  An XPacket is a compact
!  description of the information needed to move a logically rectangular
!  subblock of memory which is embedded in a larger block.
!
!------------------------------------------------------------------------------
! !USES:
      use ESMF_BaseTypesMod
      use ESMF_BaseMod    ! ESMF base class
      use ESMF_LogErrMod
      implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
      private
!
!------------------------------------------------------------------------------
!     !  ESMF_XPacket
!
!     ! Description of ESMF_XPacket. 

      type ESMF_XPacket
      sequence
      private
        integer :: rank
        integer :: offset
        integer :: contig_length
        integer :: stride(ESMF_MAXDIM)
        integer :: rep_count(ESMF_MAXDIM)
      end type

!------------------------------------------------------------------------------
! !PUBLIC TYPES:
      public ESMF_XPacket
!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:
!
      public ESMF_XPacketGet                    ! get and set values
      public ESMF_XPacketSet
      public ESMF_XPacketSetDefault
      public ESMF_XPacketPrint
 
!
!EOP

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_XPacket.F90,v 1.11 2004/06/08 09:27:20 nscollins Exp $'

!==============================================================================

      contains

!==============================================================================
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_XPacketGet"
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_XPacketGet - Get values from a XPacket

! !INTERFACE:
      subroutine ESMF_XPacketGet(xpacket, value1, value2, rc)
!
! !ARGUMENTS:
      type(ESMF_XPacket), intent(in) :: xpacket
      integer, intent(out), optional :: value1
      integer, intent(out), optional :: value2
      integer, intent(out), optional :: rc             

!
! !DESCRIPTION:
!     Query an {\tt ESMF\_XPacket}.
!
!     The arguments are:
!     \begin{description}
!     \item[xpacket] 
!          {\tt ESMF\_XPacket} to be queried.
!     \item[{[value1]}]
!          Value to be retrieved.         
!     \item[{[value2]}]
!          Value to be retrieved.         
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP

        ! local variables
        integer :: status                  ! local error status
        logical :: rcpresent               ! did user specify rc?

        ! Set initial values
        status = ESMF_FAILURE
        rcpresent = .FALSE.   

        ! Initialize return code; assume failure until success is certain
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
        endif

        if (present(value1)) then
          ! code to be added here
        endif

        if (present(value2)) then
          ! code to be added here
        endif

        ! Call C++  code
        !call c_ESMC_XPacketGet(xpacket, value1, value2, status)
        status = ESMF_FAILURE
        if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_XPacketGet

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_XPacketPrint"
!BOP
! !IROUTINE:  ESMF_XPacketPrint - Print the contents of a XPacket

! !INTERFACE:
      subroutine ESMF_XPacketPrint(xpacket, options, rc)
!
!
! !ARGUMENTS:
      type(ESMF_XPacket), intent(in) :: xpacket
      character (len = *), intent(in), optional :: options
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Print information about an {\tt ESMF\_XPacket}.
!
!     The arguments are:
!     \begin{description}
!     \item [xpacket]
!           {\tt ESMF\_XPacket} to print.
!     \item [{[options]}]
!           Standard print options.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP

        if (present(rc)) rc = ESMF_FAILURE

        print *, "XPacket Print:"
        print *, "  Rank=", xpacket%rank, " Left=", xpacket%offset, &
                 " Right=", xpacket%contig_length
        print *, "  Strides=", xpacket%stride, " Nums=", xpacket%rep_count

        if (present(rc)) rc = ESMF_SUCCESS

        end subroutine ESMF_XPacketPrint
 
!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_XPacketSet"
!BOP
! !IROUTINE: ESMF_XPacketSet - Set values in a XPacket

! !INTERFACE:
      subroutine ESMF_XPacketSet(XPacket, value1, value2, rc)
!
! !ARGUMENTS:
      type(ESMF_XPacket), intent(in) :: xpacket
      integer, intent(in), optional :: value1
      integer, intent(in), optional :: value2
      integer, intent(out), optional :: rc            

!
! !DESCRIPTION:
!     Set a XPacket attribute with the given value.
!     May be multiple routines, one per attribute.
!
!     The arguments are:
!     \begin{description}
!     \item[xpacket] 
!          Class to be modified.
!     \item[{[value1]}]
!          Value to be set.         
!     \item[{[value2]}]
!          Value to be set.         
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS: 

        ! local variables
        integer :: status                  ! local error status
        logical :: rcpresent               ! did user specify rc?

        ! Set initial values
        status = ESMF_FAILURE
        rcpresent = .FALSE.   

        ! Initialize return code; assume failure until success is certain
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
        endif

        if (present(value1)) then
          ! code to be added here
        endif

        if (present(value2)) then
          ! code to be added here
        endif

        ! Call C++  code
        !call c_ESMC_XPacketSet(xpacket, value1, value2, status)
        status = ESMF_FAILURE
        if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_XPacketSet

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_XPacketSetDefault"
!BOP
! !IROUTINE: ESMF_XPacketSetDefault - Initialize an XPacket 

! !INTERFACE:
      subroutine ESMF_XPacketSetDefault(xpacket, rank, offset, contig_length, &
                                        stride, rep_count, rc)
!
! !ARGUMENTS:
      type(ESMF_XPacket), intent(inout) :: xpacket   
      integer, intent(in) :: rank
      integer, intent(in) :: offset
      integer, intent(in) :: contig_length
      integer, intent(in) :: stride(:)
      integer, intent(in) :: rep_count(:)
      integer, intent(out), optional :: rc              
!
! !DESCRIPTION:
!     Set the initial values in an {\tt ESMF\_XPacket}.
!
!  The arguments are:
!     \begin{description}
!     \item[xpacket]
!          {\tt ESMF\_XPacket} to be set.
!     \item[rank] 
!          Data rank.
!     \item[offset]
!          Item offset into buffer.
!     \item[contig\_length] 
!          Number of contiguous items.
!     \item[stride] 
!          Number of items to skip in each dimension to return to the start
!          of the next group of contiguous items.
!     \item[rep\_count]
!          Number of times to repeat the copy operation per dimension.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP

        ! local variables
        integer :: status                  ! local error status
        logical :: rcpresent               ! did user specify rc?

        ! Set initial values
        status = ESMF_FAILURE
        rcpresent = .FALSE.   

        ! Initialize return code; assume failure until success is certain
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
        endif
 
        xpacket%rank = rank
        xpacket%offset = offset
        xpacket%contig_length = contig_length
        xpacket%stride = stride
        xpacket%rep_count = rep_count

        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_XPacketSetDefault



!------------------------------------------------------------------------------

       end module ESMF_XPacketMod
