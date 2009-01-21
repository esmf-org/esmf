! $Id: ESMF_XPacket.F90,v 1.21.2.3 2009/01/21 21:25:23 cdeluca Exp $
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
!==============================================================================
!BOPI
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
      use ESMF_UtilTypesMod
      use ESMF_InitMacrosMod
      use ESMF_LogErrMod
      use ESMF_BaseMod    ! ESMF base class
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
	ESMF_INIT_DECLARE
      end type

!------------------------------------------------------------------------------
! !PUBLIC TYPES:
      public ESMF_XPacket
!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:
!

      public ESMF_XPacketInit
      public ESMF_XPacketValidate
      public ESMF_XPacketGetInit

      public ESMF_XPacketGet                    ! get and set values
      public ESMF_XPacketSet
      public ESMF_XPacketSetDefault
      public ESMF_XPacketPrint
 
!
!EOPI

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_XPacket.F90,v 1.21.2.3 2009/01/21 21:25:23 cdeluca Exp $'

!==============================================================================

      contains

!==============================================================================
!
! XPacket Initialiation and Validation functions
!
!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_XPacketGetInit"
!BOPI
! !IROUTINE: ESMF_XPacketGetInit - Get the Init status 

! !INTERFACE:
      function ESMF_XPacketGetInit(s)
!
! !RETURN VALUE:
      ESMF_INIT_TYPE :: ESMF_XPacketGetInit
!
! !ARGUMENTS:
      type(ESMF_XPacket), intent(in),optional :: s
!
! !DESCRIPTION:
!     Get the init status
!
!     The arguments are:
!     \begin{description}
!     \item[s] 
!          The class to be queried 
!     \end{description}
!
!EOPI
  if (present(s)) then
     ESMF_XPacketGetInit=ESMF_INIT_GET(s)
  else
     ESMF_XPacketGetInit=ESMF_INIT_DEFINED
  endif 
end function ESMF_XPacketGetInit

!---------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_XPacketInit"
!BOPI
! !IROUTINE: ESMF_XPacketInit - Initialize XPacket variables

! !INTERFACE:
      subroutine ESMF_XPacketInit(s)
!
! !ARGUMENTS:
      type(ESMF_XPacket), intent(inout) :: s
!
! !DESCRIPTION:
!     Initialize XPacket
!
!     The arguments are:
!     \begin{description}
!     \item[s] 
!          The class to be queried 
!     \end{description}
!
!EOPI

  ESMF_INIT_SET_DEFINED(s)

end subroutine ESMF_XPacketInit

!-------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_XPacketValidate"
!BOP
! !IROUTINE: ESMF_XPacketValidate - Validateialize XPacket variables

! !INTERFACE:
      subroutine ESMF_XPacketValidate(s,rc)
!
! !ARGUMENTS:
      type(ESMF_XPacket), intent(inout) :: s
      integer, intent(out), optional::rc
!
! !DESCRIPTION:
!     Check if s is initialized.  If not, initialize it.
!
!     The arguments are:
!     \begin{description}
!     \item[s] 
!          The class to be validated
!     \item[rc] 
!          return value, always return ESMF_SUCCESS.
!     \end{description}
!
!EOP

  ESMF_INIT_CHECK_SHALLOW(ESMF_XPacketGetInit,ESMF_XPacketInit,s)

  if (present(rc)) rc=ESMF_SUCCESS

end subroutine ESMF_XPacketValidate

!==============================================================================
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_XPacketGet"
!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_XPacketGet - Get values from a XPacket

! !INTERFACE:
      subroutine ESMF_XPacketGet(xpacket, value1, value2, rc)
!
! !ARGUMENTS:
      type(ESMF_XPacket), intent(inout) :: xpacket
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
!EOPI

        ! local variables
        integer :: status                  ! local error status
        logical :: rcpresent               ! did user specify rc?

        ! Set initial values
        status = ESMF_RC_NOT_IMPL
        rcpresent = .FALSE.   

        ! Initialize return code; assume failure until success is certain
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_RC_NOT_IMPL
        endif

        ESMF_INIT_CHECK_SHALLOW(ESMF_XPacketGetInit,ESMF_XPacketInit,xpacket)

        if (present(value1)) then
          ! code to be added here
        endif

        if (present(value2)) then
          ! code to be added here
        endif

        ! Call C++  code
	! This function is not implemented yet -- P.Li 11/27/06
        !call c_ESMC_XPacketGet(xpacket, value1, value2, status)
        status = ESMF_RC_NOT_IMPL
        if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_XPacketGet

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_XPacketPrint"
!BOPI
! !IROUTINE:  ESMF_XPacketPrint - Print the contents of a XPacket

! !INTERFACE:
      subroutine ESMF_XPacketPrint(xpacket, options, rc)
!
!
! !ARGUMENTS:
      type(ESMF_XPacket), intent(inout) :: xpacket
      character (len = *), intent(in), optional :: options
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Print information about an {\tt ESMF\_XPacket}.
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
!     \item [xpacket]
!           {\tt ESMF\_XPacket} to print.
!     \item [{[options]}]
!           Standard print options.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI

        ! Initialize rc
        if (present(rc)) rc = ESMF_RC_NOT_IMPL
	
        ESMF_INIT_CHECK_SHALLOW(ESMF_XPacketGetInit,ESMF_XPacketInit,xpacket)

        print *, "XPacket Print:"
        print *, "  Rank=", xpacket%rank, " Left=", xpacket%offset, &
                 " Right=", xpacket%contig_length
        print *, "  Strides=", xpacket%stride, " Nums=", xpacket%rep_count

        if (present(rc)) rc = ESMF_SUCCESS

        end subroutine ESMF_XPacketPrint
 
!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_XPacketSet"
!BOPI
! !IROUTINE: ESMF_XPacketSet - Set values in a XPacket

! !INTERFACE:
      subroutine ESMF_XPacketSet(XPacket, value1, value2, rc)
!
! !ARGUMENTS:
      type(ESMF_XPacket), intent(inout) :: xpacket
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
!EOPI
! !REQUIREMENTS: 

        ! local variables
        integer :: status                  ! local error status
        logical :: rcpresent               ! did user specify rc?

        ! Set initial values
        status = ESMF_RC_NOT_IMPL
        rcpresent = .FALSE.   

        ! Initialize return code; assume failure until success is certain
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_RC_NOT_IMPL
        endif

        ESMF_INIT_CHECK_SHALLOW(ESMF_XPacketGetInit,ESMF_XPacketInit,xpacket)

        if (present(value1)) then
          ! code to be added here
        endif

        if (present(value2)) then
          ! code to be added here
        endif

        ! Call C++  code
	! This function is not implemented yet - 11/27/2006
        !call c_ESMC_XPacketSet(xpacket, value1, value2, status)
        status = ESMF_RC_NOT_IMPL
        if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_XPacketSet

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_XPacketSetDefault"
!BOPI
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
!EOPI

        ! local variables
        integer :: status, i               ! local error status
        logical :: rcpresent               ! did user specify rc?

        ! Set initial values
        status = ESMF_RC_NOT_IMPL
        rcpresent = .FALSE.   

        ! Initialize return code; assume failure until success is certain
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_RC_NOT_IMPL
        endif

        ESMF_INIT_CHECK_SHALLOW(ESMF_XPacketGetInit,ESMF_XPacketInit,xpacket)
 
        xpacket%rank          = rank
        xpacket%offset        = offset
        xpacket%contig_length = contig_length
        xpacket%stride        = 0
        xpacket%rep_count     = 0
        do i = 1,rank
          xpacket%stride(i)    = stride(i)
          xpacket%rep_count(i) = rep_count(i)
        enddo

        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_XPacketSetDefault



!------------------------------------------------------------------------------

       end module ESMF_XPacketMod
