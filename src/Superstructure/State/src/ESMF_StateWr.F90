! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2021, University Corporation for Atmospheric Research, 
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
! Laboratory, University of Michigan, National Centers for Environmental 
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!==============================================================================
!
#define ESMF_FILENAME "ESMF_StateWr.F90"
!
!     ESMF StateAPI module
module ESMF_StateWrMod
!
!==============================================================================
!
! This file contains the StateWrite methods.
!
!------------------------------------------------------------------------------
! INCLUDES
!------------------------------------------------------------------------------
#include "ESMF.h"

!------------------------------------------------------------------------------
!BOPI
! !MODULE: ESMF_StateWrMod - Write a State to a file
!
! !DESCRIPTION:
!
! The code in this file implements the Fortran subroutine 
!  implementations of the {\tt StateWrite} methods.
!
!
! !USES:
      use ESMF_InitMacrosMod
      use ESMF_LogErrMod
      use ESMF_UtilTypesMod

      use ESMF_StateTypesMod
      implicit none
      
!------------------------------------------------------------------------------
! !PRIVATE TYPES:
      private
      
!------------------------------------------------------------------------------

! !PUBLIC MEMBER FUNCTIONS:
      public :: ESMF_StateWrite
      public :: ESMF_StateWriteRestart

!EOPI

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id$'

!------------------------------------------------------------------------------


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


!------------------------------------------------------------------------------
!#undef  ESMF_METHOD
!#define ESMF_METHOD "ESMF_StateWrite"
!BOPI
! !IROUTINE: ESMF_StateWrite -- Write single item from a State
!
! !INTERFACE:
!      subroutine ESMF_StateWrite(state, itemName, rc)
!
! !ARGUMENTS:
!      type(ESMF_State):: state 
!      character (len=*), intent(in), optional :: itemName
!      integer, intent(out), optional :: rc            
!
! !DESCRIPTION:
!      Used to write out all or part of a State object.
!
!     The arguments are:
!     \begin{description}
!     \item[state]
!       The {\tt ESMF\_State} to write.
!     \item[{[itemName]}]
!       Item to be written.
!     \item[{[rc]}]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
!        ! TODO: hardcoded for interoperability test
!        type(ESMF_Field) :: fred
!        integer :: localrc
!
!        localrc = ESMF_RC_NOT_IMPL
!
!        ! check input variables
!        ESMF_INIT_CHECK_DEEP(ESMF_StateGetInit,state,rc)
!
!        if (present(itemName)) then
!            call ESMF_StateGetField(state, itemName=itemName, field=fred, rc=localrc)
!            call ESMF_FieldWrite(fred, rc=localrc) 
!        endif
!
!        if (ESMF_LogFoundError(localrc, &
!                                  ESMF_ERR_PASSTHRU, &
!                                  ESMF_CONTEXT, rcToReturn=rc)) return
!  
!
!        if (present(rc)) rc = ESMF_SUCCESS
!        end subroutine ESMF_StateWrite
!
!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateWrite"
!BOP
! !IROUTINE: ESMF_StateWrite -- Write items from a State to file
!
! !INTERFACE:
      subroutine ESMF_StateWrite(state, fileName, rc)
!
! !ARGUMENTS:
      type(ESMF_State),  intent(in)            :: state 
      character (len=*), intent(in)            :: fileName
      integer,           intent(out), optional :: rc 
!
! !DESCRIPTION:
!     Currently limited to write out all Arrays of a State object to a
!     netCDF file.  Future releases will enable more item types of a State to
!     be written to files of various formats.
!
!     Writing is currently limited to PET 0; future versions of ESMF will allow
!     parallel writing, as well as parallel reading.
!
!     See Section~\ref{example:StateRdWr} for an example.
!
!     Note that the third party NetCDF library must be installed.  For more
!     details, see the "ESMF Users Guide",
!     "Building and Installing the ESMF, Third Party Libraries, NetCDF" and
!     the website http://www.unidata.ucar.edu/software/netcdf.
!
!     The arguments are:
!     \begin{description}
!     \item[state]
!       The {\tt ESMF\_State} from which to write items.  Currently limited to
!       Arrays.
!     \item[fileName]
!       File to be written.  
!     \item[{[rc]}]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!       Equals {\tt ESMF\_RC\_LIB\_NOT\_PRESENT} if the NetCDF library is
!       not present.
!     \end{description}
!
!EOP
!       TODO: use item flag ESMF_STATEITEM_ARRAY<BUNDLE>

        integer :: localrc

        ! Initialize return code; assume failure until success is certain
        if (present(rc)) rc = ESMF_RC_NOT_IMPL
        localrc = ESMF_RC_NOT_IMPL

        ! check input variables
        ESMF_INIT_CHECK_DEEP(ESMF_StateGetInit,state,rc)

        if (fileName == ' ') then
          if (ESMF_LogFoundError (ESMF_RC_ARG_VALUE, msg='File name required',  &
              ESMF_CONTEXT, rcToReturn=rc)) return
        end if

        ! invoke C to C++ entry point 
        call c_ESMC_StateWrite(state, state%statep%base, trim (fileName), localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        if (present(rc)) rc = ESMF_SUCCESS
        end subroutine ESMF_StateWrite

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateWriteRestart"
!BOPI
! !IROUTINE: ESMF_StateWriteRestart -- Save the internal data for a State
!
! !INTERFACE:
      subroutine ESMF_StateWriteRestart(state, keywordEnforcer, rc)
!
! !ARGUMENTS:
      type(ESMF_State), intent(in)            :: state 
    type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
      integer,          intent(out), optional :: rc            
!
! !DESCRIPTION:
!      Used to save all data to disk as quickly as possible.  
!      (see Read/Write for other options).  Internally this routine uses the
!      same I/O interface as Read/Write, but the default options are to
!      select the fastest way to save data to disk.
!
!     The arguments are:
!     \begin{description}
!     \item[state]
!       {\tt ESMF\_State} to save contents of.
!     \item[{[rc]}]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
        integer :: localrc

        localrc = ESMF_RC_NOT_IMPL

        ! check input variables
        ESMF_INIT_CHECK_DEEP(ESMF_StateGetInit,state,rc)
!
! TODO: code goes here
!
! The flags BOP/EOP have been changed to BOPI/EOPI because
! the subroutine has not been implemented. When the code is
! completed change back to BOP/EOP.
!

        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                    ESMF_CONTEXT, rcToReturn=rc))  return

        if (present(rc)) rc = ESMF_RC_NOT_IMPL
        end subroutine ESMF_StateWriteRestart


!------------------------------------------------------------------------------

end module ESMF_StateWrMod
