! $Id: ESMF_XPacket.F90,v 1.3 2003/03/11 23:15:57 nscollins Exp $
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
#include "ESMF_Route.h"
!==============================================================================
!BOP
! !MODULE: ESMF_XPacketMod - One line general statement about this class
!
! !DESCRIPTION:
!
! The code in this file implements the F90 wrapper code for the C++
!  implementation of the {\tt XPacket} class ...
!
! < Insert a paragraph or two explaining the function of this class. >
!
!------------------------------------------------------------------------------
! !USES:
      use ESMF_BaseMod    ! ESMF base class
!     use ESMF_<XXX>Mod   ! any other dependencies
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
        integer :: left
        integer :: right
        integer :: stride(ESMF_MAXDIM)
        integer :: num(ESMF_MAXDIM)
      end type

!------------------------------------------------------------------------------
! !PUBLIC TYPES:
      public ESMF_XPacket
!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:
!
      public ESMF_XPacketInit                   ! shallow class

      public ESMF_XPacketGet                    ! get and set values
      public ESMF_XPacketSet
 
!
!EOP

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_XPacket.F90,v 1.3 2003/03/11 23:15:57 nscollins Exp $'

!==============================================================================

      contains

!==============================================================================
!
! This section includes the XPacket Init methods.
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_XPacketInit - Initialize a XPacket 

! !INTERFACE:
      subroutine ESMF_XPacketInit(xpacket, rank, left, right, stride, num, rc)
!
! !ARGUMENTS:
      type(ESMF_XPacket), intent(inout) :: xpacket   
      integer, intent(in) :: rank
      integer, intent(in) :: left
      integer, intent(in) :: right
      integer, intent(in) :: stride(ESMF_MAXDIM)
      integer, intent(in) :: num(ESMF_MAXDIM)
      integer, intent(out), optional :: rc              
!
! !DESCRIPTION:
!     ESMF routine which only initializes {\tt XPacket} values; it does not
!     allocate any resources. 
!
!  The arguments are:
!     \begin{description}
!     \item[xpacket]
!          Class to be initialized.
!     \item[rank] 
!          Argument 1.
!     \item[left]
!          Argument 2.         
!     \item[right] 
!          Argument 3.
!     \item[stride] 
!          Argument 4.
!     \item[num]
!          Argument 5.         
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
 
        xpacket%rank = rank
        xpacket%left = left
        xpacket%right = right
        xpacket%stride = stride
        xpacket%num = num

        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_XPacketInit


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
!     Returns the value of XPacket attribute <Value>.
!
!     The arguments are:
!     \begin{description}
!     \item[xpacket] 
!          Class to be queried.
!     \item[{[value1]}]
!          Value to be retrieved.         
!     \item[{[value2]}]
!          Value to be retrieved.         
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
        !call c_ESMC_XPacketGet(xpacket, value1, value2, status)
        status = ESMF_FAILURE
        if (status .ne. ESMF_SUCCESS) then  
          print *, "XPacket Get error"
          return  
        endif

        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_XPacketGet

!------------------------------------------------------------------------------
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
        call c_ESMC_XPacketSet(xpacket, value1, value2, status)
        status = ESMF_FAILURE
        if (status .ne. ESMF_SUCCESS) then  
          print *, "XPacket Set error"
          return  
        endif

        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_XPacketSet


!------------------------------------------------------------------------------
! TODO: maybe it's worth putting a Print method back at some point.

!------------------------------------------------------------------------------

       end module ESMF_XPacketMod
