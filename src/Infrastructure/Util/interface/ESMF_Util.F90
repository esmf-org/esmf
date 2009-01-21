! $Id: ESMF_Util.F90,v 1.12.2.3 2009/01/21 21:25:24 cdeluca Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2009, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
#define ESMF_FILENAME "ESMF_Util.F90"

!
! ESMF Util Module
!
! (all lines between the !BOP and !EOP markers will be included in the
! automated document processing.)
!------------------------------------------------------------------------------
! one blank line for protex processing - in case all routines here are
! marked internal (BOPI/EOPI), the output file will still have contents.
!BOP

!EOP

!------------------------------------------------------------------------------
! module definition

      module ESMF_UtilMod
 
      ! parameters, types
      use ESMF_UtilTypesMod
      use ESMF_InitMacrosMod
      use ESMF_LogErrMod

#include "ESMF.h"

!BOPI
! !MODULE: ESMF_UtilMod - Interface routines to generic utility functions
!
! !DESCRIPTION:
!
!  Interfaces to, in most cases, the C++ implementation of generic utility
!  functions.
!
! See the ESMF Developers Guide document for more details.
!
!------------------------------------------------------------------------------

! !USES:
      implicit none
!
! !PRIVATE TYPES:
      private

!------------------------------------------------------------------------------
!
! !DESCRIPTION:
!
! !PUBLIC MEMBER FUNCTIONS:
!

! DomainList methods
      public ESMF_DomainListCreate
      public ESMF_DomainListDestroy
      public ESMF_DomainListPrint
      public ESMF_DomainListAdd
 
! AxisIndex methods
      public ESMF_AxisIndexSet
      public ESMF_AxisIndexGet
      public ESMF_AxisIndexPrint

!  Misc methods
      public ESMF_SetPointer
      public ESMF_SetNullPointer
      public ESMF_GetPointer
      public ESMF_StringLowerCase
      public ESMF_StringUpperCase

!  Misc type-to-string methods
      public ESMF_StatusString
      public ESMF_TypeKindString
      public ESMF_LogicalString

!  Overloaded = operator functions
      public operator(.eq.), operator(.ne.), assignment(=)
!
!

!==============================================================================
!
! INTERFACE BLOCKS
!
!==============================================================================
!BOPI
! !INTERFACE:
      interface ESMF_DomainListAdd

! !PRIVATE MEMBER FUNCTIONS:
         module procedure ESMF_DomainListAdd2d
         module procedure ESMF_DomainListAdd3d
         module procedure ESMF_DomainListAddObj
!

! !DESCRIPTION:
!     These functions are meant to ease the task of creating multidimensional
!     domains.
!
!EOPI
      end interface 

!------------------------------------------------------------------------------
!BOPI
! !INTERFACE:
      interface ESMF_AxisIndexPrint

! !PRIVATE MEMBER FUNCTIONS:
         module procedure ESMF_AxisIndexPrintOne
         module procedure ESMF_AxisIndexPrintList
!

! !DESCRIPTION:
!     Print contents of AIs.
!
!EOPI
      end interface 


!------------------------------------------------------------------------------
! leave the following line as-is; it will insert the cvs ident string
! into the object file for tracking purposes.
      character(*), parameter, private :: version = &
               '$Id: ESMF_Util.F90,v 1.12.2.3 2009/01/21 21:25:24 cdeluca Exp $'
!------------------------------------------------------------------------------

      contains

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
! Domain List routines.
!-------------------------------------------------------------------------
!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DomainListCreate"
!BOPI
! !IROUTINE:  ESMF_DomainListCreate - Create domain list
!
! !INTERFACE:
      function ESMF_DomainListCreate(num_domains)
! !RETURN VALUE:
      type(ESMF_DomainList) :: ESMF_DomainListCreate
!
! !ARGUMENTS:
      integer :: num_domains
!
! !DESCRIPTION:
! Create a list of {\tt ESMF\_Domain}s.  
! Initializes the array of domains.  Preallocates storage.
!
!     The arguments are:
!     \begin{description}
!     \item[num_domains]
!	A suggestion on the number of domains the object will hold.
!     \end{description}
!
!EOPI
      integer :: status
      type(ESMF_Domain), dimension(:), pointer :: domains

! Allocate an array of domains of specified size
      allocate(domains(num_domains), stat=status)

! Initialize values and attach domains to the list
      ESMF_DomainListCreate%num_domains  = num_domains
      ESMF_DomainListCreate%current_size = num_domains
      ESMF_DomainListCreate%total_points = 0
      ESMF_DomainListCreate%domains      => domains

      end function ESMF_DomainListCreate

!-------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DomainListDestroy"
!BOPI
! !IROUTINE:  ESMF_DomainListDestroy - Destroy domain list
!
! !INTERFACE:
      subroutine ESMF_DomainListDestroy(domainlist)
!
! !ARGUMENTS:
      type(ESMF_DomainList) :: domainlist
!
! !DESCRIPTION:
! Deallocate memory used by creation routine.
!
!     The arguments are:
!     \begin{description}
!     \item[domainlist]
!       An {\tt ESMF\_DomainList} to destroy.
!     \end{description}
!
!EOPI
      integer :: status

      ESMF_INIT_CHECK_SHALLOW(ESMF_DomainListGetInit,ESMF_DomainListInit,domainList)

      deallocate(domainlist%domains, stat=status)

      end subroutine ESMF_DomainListDestroy

!-------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DomainListPrint"
!BOPI
! !IROUTINE:  ESMF_DomainListPrint - Print domain list
!
! !INTERFACE:
      subroutine ESMF_DomainListPrint(domainlist)
!
! !ARGUMENTS:
      type(ESMF_DomainList) :: domainlist
!
! !DESCRIPTION:
!     Dump the contents of a domain list to screen, i.e. for 
!     debugging during development.
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
!     \item[domainlist]
!       A list of domains to print.
!     \end{description}
!
!EOPI
      integer :: i, j
      integer :: min, max, stride
      !character(len=ESMF_MAXSTR) :: msgbuf

      ESMF_INIT_CHECK_SHALLOW(ESMF_DomainListGetInit,ESMF_DomainListInit,domainList)

    !jw  write (msgbuf, *)  "DomainListPrint"
    !jw  call ESMF_LogWrite(msgbuf, ESMF_LOG_INFO)
      write (*, *)  "DomainListPrint"
    !jw  write (msgbuf, *)  "Number stored domains:", domainlist%num_domains
    !jw  call ESMF_LogWrite(msgbuf, ESMF_LOG_INFO)
      write (*, *)  "Number stored domains:", domainlist%num_domains
    !jw  write (msgbuf, *)  "Total points:", domainlist%total_points
    !jw  call ESMF_LogWrite(msgbuf, ESMF_LOG_INFO)
      write (*, *)  "Total points:", domainlist%total_points

! Now loop through domains and print them out

      do i=1, domainlist%num_domains
    !jw     write (msgbuf, *)  '***Domain.  Rank:', domainlist%domains(i)%rank
    !jw     call ESMF_LogWrite(msgbuf, ESMF_LOG_INFO)
         write (*, *)  '***Domain.  Rank:', domainlist%domains(i)%rank
         do j=1, domainlist%domains(i)%rank
	    call ESMF_AxisIndexGet(domainlist%domains(i)%ai(j), min, max, stride)
    !jw        write (msgbuf, *)  '   axis:min,max,stride3:', min, max, stride
    !jw        call ESMF_LogWrite(msgbuf, ESMF_LOG_INFO)
	    write (*, *)  '   axis:min,max,stride3:', min, max, stride
         enddo
      enddo

      end subroutine ESMF_DomainListPrint

!-------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DomainListAdd2d"
!BOPI
! !IROUTINE:  ESMF_DomainListAdd2d - Add a 2D domainlist
!
! !INTERFACE:
      subroutine ESMF_DomainListAdd2d(domainlist, &
                             min1, max1, stride1, &
                             min2, max2, stride2)
!
! !ARGUMENTS:
     type(ESMF_DomainList), intent(inout) :: domainlist
     integer :: min1
     integer :: max1
     integer :: stride1
     integer :: min2
     integer :: max2
     integer :: stride2
!
! !DESCRIPTION:
!    Convenience function for adding a 2d domain.  Avoids the 
!    unnecessary hassle of creating a domain, etc...
!
!
!     The arguments are:
!     \begin{description}
!     \item[domainlist]
!       The {\tt ESMF\_DomainList}.
!     \item[min1]
!	Minimimun in first direction.
!     \item[max1]
!	Maximum in first direction.
!     \item[stride1]
!	Stride in first direction.
!     \item[min2]
!	Minimimun in second direction.
!     \item[max2]
!	Maximimun in second direction.
!     \item[stride2]
!	Stride in second direction.
!     \end{description}
!
!EOPI
      type(ESMF_Domain) :: newdomain          ! temp variable to use
      
      ESMF_INIT_CHECK_SHALLOW(ESMF_DomainListGetInit,ESMF_DomainListInit,domainList)
      ESMF_INIT_CHECK_SHALLOW(ESMF_DomainGetInit,ESMF_DomainInit,newdomain)

      newdomain%rank = 2
      call ESMF_AxisIndexSet(newdomain%ai(1), min1, max1, stride1)
      call ESMF_AxisIndexSet(newdomain%ai(2), min2, max2, stride2)

      call ESMF_DomainListAdd(domainlist, newdomain)

      end subroutine ESMF_DomainListAdd2d

!-------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DomainListAdd3d"
!BOPI
! !IROUTINE:  ESMF_DomainListAdd3d - Add a 3D domainlist
!
! !INTERFACE:
      subroutine ESMF_DomainListAdd3d(domainlist, &
                            min1, max1, stride1, &
                            min2, max2, stride2, &
                            min3, max3, stride3)
!
! !ARGUMENTS:
     type(ESMF_DomainList), intent(inout) :: domainlist
     integer :: min1
     integer :: max1
     integer :: stride1
     integer :: min2
     integer :: max2 
     integer :: stride2
     integer :: min3
     integer :: max3
     integer :: stride3
!
! !DESCRIPTION:
!    Convenience function for adding a 3d domain.  Avoids the 
!    unnecessary hassle of creating a domain, etc...
!
!     The arguments are:
!     \begin{description}
!     \item[domainlist]
!       The {\tt ESMF\_DomainList}.
!     \item[min1]
!       Minimimun in first direction.
!     \item[max1]
!       Maximum in first direction.
!     \item[stride1]
!       Stride in first direction.
!     \item[min2]
!       Minimimun in second direction.
!     \item[max2]
!       Maximimun in second direction.
!     \item[stride2]
!       Stride in second direction.
!     \item[min3]
!       Minimimun in third direction.
!     \item[max3]
!       Maximimun in third direction.
!     \item[stride3]
!       Stride in third direction.
!     \end{description}
!
!EOPI
      type(ESMF_Domain) :: newdomain          ! temp variable to use
      
      ESMF_INIT_CHECK_SHALLOW(ESMF_DomainListGetInit,ESMF_DomainListInit,domainList)
      ESMF_INIT_CHECK_SHALLOW(ESMF_DomainGetInit,ESMF_DomainInit,newdomain)

      newdomain%rank = 3
      call ESMF_AxisIndexSet(newdomain%ai(1), min1, max1, stride1)
      call ESMF_AxisIndexSet(newdomain%ai(2), min2, max2, stride2)
      call ESMF_AxisIndexSet(newdomain%ai(3), min3, max3, stride3)

      call ESMF_DomainListAdd(domainlist,newdomain)

      end subroutine ESMF_DomainListAdd3d

!-------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DomainListAddObj"
!BOPI
! !IROUTINE:  ESMF_DomainListAddObj - Add a domain object 
!
! !INTERFACE:
      subroutine ESMF_DomainListAddObj(domainlist, newdomain)
!
! !ARGUMENTS:
      type(ESMF_DomainList), intent(inout) :: domainlist
      type(ESMF_Domain), intent(inout) :: newdomain
!
! !DESCRIPTION:
!   The other add routines should end by using this call.  It takes care of
!   the memory management issues, i.e. it reallocs the list if it has grown
!   too large. 
!
!     The arguments are:
!     \begin{description}
!     \item[domainlist]
!       The {\tt ESMF\_DomainList}.
!     \item[newdomain]
!       The {\tt ESMF\_Domain} to add to the list.
!     \end{description}
!
!EOPI
      type(ESMF_Domain), dimension(:), allocatable, target :: temp_domains
      integer :: new_size         ! New number of domains to alloc
      integer :: status, i
      
      ESMF_INIT_CHECK_SHALLOW(ESMF_DomainListGetInit,ESMF_DomainListInit,domainList)
      ESMF_INIT_CHECK_SHALLOW(ESMF_DomainGetInit,ESMF_DomainInit,newdomain)

! One way or another we are going to add the domain, so increment counter
      domainlist%num_domains = domainlist%num_domains + 1

! Check to see if we have room to add this object in the current list
! (Fortran equivalent of a linked list:)

      if (domainlist%num_domains  .gt. domainlist%current_size) then

! The strategy is debatable, but simply double the number of domains
      new_size = domainlist%current_size * 2
      allocate(temp_domains(new_size), stat=status)

! Copy over the old domains
      do i=1, domainlist%current_size
         temp_domains(i) = domainlist%domains(i)
      enddo

! Deallocate the old list and point to the new one
      deallocate(domainlist%domains)

      domainlist%domains => temp_domains
      domainlist%current_size = new_size
          
      endif

! Now add the new domain

      domainlist%domains(domainlist%num_domains) = newdomain

      end subroutine ESMF_DomainListAddObj

!=========================================================================
! Misc utility routines, perhaps belongs in a utility file?
!-------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_AxisIndexSet"
!BOPI
! !IROUTINE:  ESMF_AxisIndexSet - Initialize an AxisIndex object
!
! !INTERFACE:
      subroutine ESMF_AxisIndexSet(ai, min, max, stride, rc)
!
! !ARGUMENTS:
      type(ESMF_AxisIndex), intent(inout) :: ai
      integer, intent(in) :: min, max, stride
      integer, intent(out), optional :: rc  
!
! !DESCRIPTION:
!   Set the contents of an AxisIndex type.
!
!     The arguments are:
!     \begin{description}
!     \item[ai]
!       The {\tt ESMF\_AxisIndex} to set.
!     \item[min]
!       The minimimun.
!     \item[max]
!       The maximum.
!     \item[stride]
!       The stride.
!     \item[{[rc]}]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI

      ESMF_INIT_CHECK_SHALLOW(ESMF_AxisIndexGetInit,ESMF_AxisIndexInit,ai)

      ai%min = min
      ai%max = max
      ai%stride = stride

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_AxisIndexSet

!-------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_AxisIndexGet"
!BOPI
! !IROUTINE:  ESMF_AxisIndexGet - Get contents of an AxisIndex object
!
! !INTERFACE:
      subroutine ESMF_AxisIndexGet(ai, min, max, stride, rc)
!
! !ARGUMENTS:
      type(ESMF_AxisIndex), intent(inout) :: ai
      integer, intent(out), optional :: min, max, stride
      integer, intent(out), optional :: rc  
!
! !DESCRIPTION:
!   Get the contents of an AxisIndex type.
!
!     The arguments are:
!     \begin{description}
!     \item[ai]
!       The {\tt ESMF\_AxisIndex} to query.
!     \item[min]
!       The minimimun.
!     \item[max]
!       The maximum.
!     \item[stride]
!       The stride.
!     \item[{[rc]}]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI

      ESMF_INIT_CHECK_SHALLOW(ESMF_AxisIndexGetInit,ESMF_AxisIndexInit,ai)

      if (present(max)) min = ai%min
      if (present(max)) max = ai%max
      if (present(stride)) stride = ai%stride

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_AxisIndexGet

!-------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_AxisIndexPrintOne"
!BOPI
! !IROUTINE:  ESMF_AxisIndexPrint - Print contents of an AxisIndex object
!
! !INTERFACE:
      ! private interface; callusing ESMF_AxisIndexPrint()
      subroutine ESMF_AxisIndexPrintOne(ai, rc)
!
! !ARGUMENTS:
      type(ESMF_AxisIndex), intent(inout) :: ai
      integer, intent(out), optional :: rc  
!
! !DESCRIPTION:
!     Print the contents of an AxisIndex type.
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
!     \item[ai]
!       The {\tt ESMF\_AxisIndex} to query.
!     \item[{[rc]}]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI


      ESMF_INIT_CHECK_SHALLOW(ESMF_AxisIndexGetInit,ESMF_AxisIndexInit,ai)

      print *, "AI: min, max, stride = ", ai%min, ai%max, ai%stride

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_AxisIndexPrintOne

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_AxisIndexPrintList"
!BOPI
! !IROUTINE:  ESMF_AxisIndexPrint - Print contents of an AxisIndex object
!
! !INTERFACE:
      ! private interface; callusing ESMF_AxisIndexPrint()
      subroutine ESMF_AxisIndexPrintList(ai, rc)
!
! !ARGUMENTS:
      type(ESMF_AxisIndex), intent(inout) :: ai(:)
      integer, intent(out), optional :: rc  
!
! !DESCRIPTION:
!     Print the contents of an AxisIndex type.
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
!     \item[ai]
!       The {\tt ESMF\_AxisIndex} to query.
!     \item[{[rc]}]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI

      integer :: items, i

      items = size(ai) 
      do i=1, items
         ESMF_INIT_CHECK_SHALLOW(ESMF_AxisIndexGetInit,ESMF_AxisIndexInit,ai(i))
 
         print *, "AI num, min, max, stride = ", &
                    i, ai(i)%min, ai(i)%max, ai(i)%stride
      enddo

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_AxisIndexPrintList

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_SetPointer"
!BOPI
! !IROUTINE:  ESMF_SetPointer - Set an opaque value
!
! !INTERFACE:
      subroutine ESMF_SetPointer(ptype, contents, rc)
!
! !ARGUMENTS:
      type(ESMF_Pointer) :: ptype 
      integer*8, intent(in) :: contents
      integer, intent(out), optional :: rc  

!
! !DESCRIPTION:
!   Set the contents of an opaque pointer type.
!
!     The arguments are:
!     \begin{description}
!     \item[ptype]
!       An {\tt ESMF\_Pointer}.
!     \item[contents]
!       The contents to set.
!     \item[{[rc]}]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI

      ptype%ptr = contents
      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_SetPointer

!-------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_SetNullPointer"
!BOPI
! !IROUTINE:  ESMF_SetNullPointer - Set an opaque value
!
! !INTERFACE:
      subroutine ESMF_SetNullPointer(ptype, rc)
!
! !ARGUMENTS:
      type(ESMF_Pointer) :: ptype 
      integer, intent(out), optional :: rc  
!
! !DESCRIPTION:
!   Set the contents of an opaque pointer type.
!
!     The arguments are:
!     \begin{description}
!     \item[ptype]
!       An {\tt ESMF\_Pointer}.
!     \item[{[rc]}]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!
!EOPI

      integer*8, parameter :: nullp = 0

      ptype%ptr = nullp
      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_SetNullPointer
!------------------------------------------------------------------------- 
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GetPointer"
!BOPI
!  !IROUTINE:  ESMF_GetPointer - get an opaque value 
!  
! !INTERFACE: 
      function ESMF_GetPointer(ptype, rc) 
!
! !RETURN VALUE:
      integer*8 :: ESMF_GetPointer

! !ARGUMENTS:
      type(ESMF_Pointer), intent(in) :: ptype 
      integer, intent(out), optional :: rc  

!
! !DESCRIPTION:
!   Get the contents of an opaque pointer type.
!
!     The arguments are:
!     \begin{description}
!     \item[ptype]
!       An {\tt ESMF\_Pointer}.
!     \item[{[rc]}]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      ESMF_GetPointer = ptype%ptr
      if (present(rc)) rc = ESMF_SUCCESS

      end function ESMF_GetPointer

!------------------------------------------------------------------------- 
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StringLowerCase"
!BOPI
!  !IROUTINE:  ESMF_StringLowerCase - convert string to lowercase
!  
! !INTERFACE: 
      subroutine ESMF_StringLowerCase(string, rc) 
!
! !ARGUMENTS:
      character(len=*), intent(inout) :: string
      integer, intent(out), optional  :: rc  

!
! !DESCRIPTION:
!   Converts given string to lowercase.
!
!     The arguments are:
!     \begin{description}
!     \item[string]
!       A character string.
!     \item[{[rc]}]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: shift, i
      character(len=1) :: c

      shift = ichar('a') - ichar('A')
      do i = 1, len(string)
        c = string(i:i)
        if(c .ge. 'A' .and. c .le. 'Z') then
          string(i:i) = char(ichar(c) + shift)
        endif
      enddo

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_StringLowerCase

!------------------------------------------------------------------------- 
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StringUpperCase"
!BOPI
!  !IROUTINE:  ESMF_StringUpperCase - convert string to uppercase
!  
! !INTERFACE: 
      subroutine ESMF_StringUpperCase(string, rc) 
!
! !ARGUMENTS:
      character(len=*), intent(inout) :: string
      integer, intent(out), optional  :: rc  

!
! !DESCRIPTION:
!   Converts given string to uppercase.
!
!     The arguments are:
!     \begin{description}
!     \item[string]
!       A character string.
!     \item[{[rc]}]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: shift, i
      character(len=1) :: c

      shift = ichar('a') - ichar('A')
      do i = 1, len(string)
        c = string(i:i)
        if(c .ge. 'a' .and. c .le. 'z') then
          string(i:i) = char(ichar(c) - shift)
        endif
      enddo

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_StringUpperCase

!------------------------------------------------------------------------- 
!------------------------------------------------------------------------- 
! misc print routines
!------------------------------------------------------------------------- 
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StatusString"
!BOPI 
!  !IROUTINE:  ESMF_StatusString - Return status as a string
!  
! !INTERFACE: 
      subroutine ESMF_StatusString(status, string, rc)
!
! !ARGUMENTS:
      type(ESMF_Status), intent(in) :: status
      character(len=*), intent(out) :: string
      integer, intent(out), optional :: rc  

!
! !DESCRIPTION:
!   Return an {\tt ESMF\_Status} as a string.
!
!     The arguments are:
!     \begin{description}
!     \item[status]
!       The {\tt ESMF\_Status}.
!     \item[string]
!       A printable string.
!     \item[{[rc]}]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      if (status .eq. ESMF_STATUS_UNINIT) string = "Uninitialized"
      if (status .eq. ESMF_STATUS_READY) string = "Ready"
      if (status .eq. ESMF_STATUS_UNALLOCATED) string = "Unallocated"
      if (status .eq. ESMF_STATUS_ALLOCATED) string = "Allocated"
      if (status .eq. ESMF_STATUS_BUSY) string = "Busy"
      if (status .eq. ESMF_STATUS_INVALID) string = "Invalid"
 
      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_StatusString

!------------------------------------------------------------------------- 
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_TypeKindString"
!BOPI 
!  !IROUTINE:  ESMF_TypeKindString - Return TypeKind as a string
!  
! !INTERFACE: 
      subroutine ESMF_TypeKindString(datakind, string, rc)
!
! !ARGUMENTS:
      type(ESMF_TypeKind), intent(in) :: datakind
      character(len=*), intent(out) :: string
      integer, intent(out), optional :: rc  

!
! !DESCRIPTION:
!   Return an {\tt ESMF\_TypeKind} variable as a string.
!
!     The arguments are:
!     \begin{description}
!     \item[datakind]
!       The {\tt ESMF\_TypeKind}.
!     \item[string]
!       The value as a string.
!     \item[{[rc]}]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

#ifndef ESMF_NO_INTEGER_1_BYTE 
      if (datakind .eq. ESMF_TYPEKIND_I1)  string = "Integer*1"
#endif
#ifndef ESMF_NO_INTEGER_2_BYTE 
      if (datakind .eq. ESMF_TYPEKIND_I2)  string = "Integer*2"
#endif
      if (datakind .eq. ESMF_TYPEKIND_I4)  string = "Integer*4"
      if (datakind .eq. ESMF_TYPEKIND_I8)  string = "Integer*8"
      if (datakind .eq. ESMF_TYPEKIND_R4)  string = "Real*4"
      if (datakind .eq. ESMF_TYPEKIND_R8)  string = "Real*8"
      if (datakind .eq. ESMF_C8)  string = "Complex*8"
      if (datakind .eq. ESMF_C16) string = "Complex*16"
 
      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_TypeKindString

!------------------------------------------------------------------------- 
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_LogicalString"
!BOPI 
!  !IROUTINE:  ESMF_LogicalString - Return Logical as a string
!  
! !INTERFACE: 
      subroutine ESMF_LogicalString(tf, string, rc)
!
! !ARGUMENTS:
      type(ESMF_Logical), intent(in) :: tf
      character(len=*), intent(out) :: string
      integer, intent(out), optional :: rc  

!
! !DESCRIPTION:
!   Return an {\tt ESMF\_Logical} as a string.
!
!     The arguments are:
!     \begin{description}
!     \item[tf]
!       An {\tt ESMF\_Logical}.
!     \item[string]
!       The value as a string.
!     \item[{[rc]}]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      if (tf .eq. ESMF_TRUE)  string = "True"
      if (tf .eq. ESMF_FALSE) string = "False"
 
      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_LogicalString

!------------------------------------------------------------------------------


      end module ESMF_UtilMod
