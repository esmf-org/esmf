! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2021, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================

!==============================================================================
!ESMF_MULTI_PROC_EXAMPLE        String used by test script to count examples.
!==============================================================================

!BOE
! \subsubsection{General Usage Examples}
! \label{info_tutorial}
! General usage examples for the \texttt{ESMF\_Info} class. The demonstrated capabilities are:
! \begin{itemize}
! \item Creating an \texttt{ESMF\_Info} object.
! \item Setting/getting a key-value pair.
! \item Setting/getting a list value and a list value by index.
! \item Printing and dumping \texttt{ESMF\_Info} contents.
! \item Checking for key presence and set state (null value check).
! \item Setting/getting with nesting (hierarchies).
! \item Inquiring the \texttt{ESMF\_Info} object for general item metadata and iteration purposes.
! \item Copying \texttt{ESMF\_Info} contents.
! \item Removing a key-value pair from the \texttt{ESMF\_Info} storage.
! \item Destroying the \texttt{ESMF\_Info} object.
! \end{itemize}
!EOE

    program ESMF_InfoTutorial

!==============================================================================
!==============================================================================

#include "ESMF.h"

    use ESMF
    use ESMF_TestMod

    implicit none

!------------------------------------------------------------------------------

!BOE
! Variable declarations:
!EOE
!BOC
    type(ESMF_Info) :: info, infoCopy, infoFromCh
    type(ESMF_TypeKind_Flag) :: typekind
    character(len=ESMF_MAXSTR) :: ikey
    character(:), allocatable :: output, getCh
    real(ESMF_KIND_R8), dimension(4) :: realList
    real(ESMF_KIND_R8), dimension(:), allocatable :: realListAlloc
    integer(ESMF_KIND_I4) :: getInt
    real(ESMF_KIND_R8) :: getReal
    integer :: rc, infoSize, ii
    logical :: isPresent, isSet
!EOC
    type(ESMF_VM) :: vm
    integer :: finalrc, result, localPet
    character(ESMF_MAXSTR) :: testname
    character(ESMF_MAXSTR) :: failMsg

!------------------------------------------------------------------------------

    write(failMsg, *) "Example ESMF_InfoTutorialEx failure"
    write(testname, *) "Example ESMF_InfoTutorialEx"

    finalrc = ESMF_SUCCESS

!------------------------------------------------------------------------------

!   ! Initialize the Framework, and get the default VM
    call ESMF_Initialize(vm=vm, defaultlogfilename="InfoTutorialEx.Log", &
                    logkindflag=ESMF_LOGKIND_MULTI, rc=rc)
    if (rc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_VMGet(vm, localPet=localPet, rc=rc)
    if (rc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!------------------------------------------------------------------------------

!BOE
! Create an \texttt{ESMF\_Info} object. This object contains an empty key-value
! store called a JSON object \cite{json_for_modern_cpp_object}.
!
! An \texttt{ESMF\_Info} handle may also be retrieved from an ESMF object as opposed to
! creating a standalone \texttt{ESMF\_Info} object. See example \ref{get_info_handle_from_esmf_object}.
!EOE
!BOC
    info = ESMF_InfoCreate(rc=rc)
!EOC
    if (rc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!------------------------------------------------------------------------------

!BOE
! Add an integer value.
!EOE
!BOC
    call ESMF_InfoSet(info, "myIntegerKey", 54, rc=rc)
!EOC
    if (rc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE
! Get the integer value we just set.
!EOE
!BOC
    call ESMF_InfoGet(info, "myIntegerKey", getInt, rc=rc)
!EOC
    if (rc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    if (getInt .ne. 54) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!------------------------------------------------------------------------------
!BOE
! Set a list of reals.
!EOE
!BOC
    call ESMF_InfoSet(info, "myListOfReals", (/ 33.3, 44.4, 0.0, 99.0 /), rc=rc)
!EOC
    if (rc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE
! Set an index in the new list then retrieve the value.
!EOE
!BOC
    call ESMF_InfoSet(info, "myListOfReals", 1234.0, idx=3, rc=rc)
!EOC
    if (rc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
    call ESMF_InfoGet(info, "myListOfReals", getReal, idx=3, rc=rc)
!EOC
    if (rc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    if ((getReal-1234.0) > 0) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE
! Get the values from a list.
!EOE
!BOC
    call ESMF_InfoGet(info, "myListOfReals", realList, rc=rc)
!EOC
    if (rc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    
    if (size(realList) /= 4) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE
! Allocatable lists may be used through a specific interface.
!EOE
!BOC
    call ESMF_InfoGetAlloc(info, "myListOfReals", realListAlloc, rc=rc)
!EOC
    if (rc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    
!------------------------------------------------------------------------------

!BOE
! The storage contents may be printed directly or dumped to a character.
!EOE
!BOC
    call ESMF_InfoPrint(info, indent=4, rc=rc)
!EOC
    if (rc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
    output = ESMF_InfoDump(info, rc=rc)
!EOC
    if (rc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
    print *, "the Info dump: "//output
!EOC

!------------------------------------------------------------------------------

!BOE
! Check if a key is present.
!EOE
!BOC
    isPresent = ESMF_InfoIsPresent(info, "myIntegerKey", rc=rc)
!EOC
    if (rc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
    if (.not. isPresent) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!EOC
!BOE
! Add a null value and check if it is set (has a non-null value).
!EOE
!BOC
    call ESMF_InfoSetNULL(info, "aNullKey", rc=rc)
!EOC
    if (rc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
    isSet = ESMF_InfoIsSet(info, "aNullKey", rc=rc)
!EOC
    if (rc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
    if (isSet) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    isSet = ESMF_InfoIsSet(info, "myIntegerKey", rc=rc)
!EOC
    if (rc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
    if (.not. isSet) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!EOC
!BOE
! The force flag, when set to false, will cause an error if the key exists in the
! map. The force flag is set to true by default.
!EOE
!BOC
    call ESMF_InfoSet(info, "myIntegerKey", 33, force=.false., rc=rc)
    if (rc .ne. ESMC_RC_CANNOT_SET) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!EOC

!------------------------------------------------------------------------------

!BOE
! Nesting uses the JSON Pointer \ref{info_key_format} syntax. All key arguments
! in \texttt{ESMF\_Info} may use this syntax unless noted otherwise. When creating
! a nested object, objects are created if they do not exist. Hence, it is not necessary
! to create the individual nested elements for deep hierarchies.
!EOE
!BOC
    call ESMF_InfoSet(info, "/Universe/Galaxy/Star/Planet", "Venus", rc=rc)
!EOC
    if (rc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!------------------------------------------------------------------------------

!BOE
! Using the get interface, it is possible to iterate over the storage contents.
! In the call below, we are retrieving the number of elements (key-value pairs)
! that exist in our root storage object. We then select the target element in root
! using an index and retrieve some additional metadata for the target object.
!EOE
!BOC
    call ESMF_InfoGet(info, size=infoSize, rc=rc)
!EOC
    if (rc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
    do ii=1,infoSize
      call ESMF_InfoGet(info, idx=ii, ikey=ikey, typekind=typekind, rc=rc)
!EOC
      if (rc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC

      if (localPet == 0) then
        print *, "ESMF_Info inquire loop: "
        print *, "       idx= ", ii
        print *, "      ikey= ", trim(ikey)
        print *, "  typekind= ", typekind
      endif
    enddo
!EOC
    
!------------------------------------------------------------------------------

!BOE
! Copying the \texttt{ESMF\_Info} object requires the copy to be destroyed/deallocated.
!EOE
!BOC
    infoCopy = ESMF_InfoCreate(info, rc=rc)
!EOC
    if (rc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE
! Comparison operators = and /= are implemented for \texttt{ESMF\_Info} objects.
!EOE
!BOC
    if (infoCopy /= info) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!EOC
!BOE
! After removing a key from the copied \texttt{ESMF\_Info} object, the two objects will no
! longer be equal.
!EOE
!BOC
    call ESMF_InfoRemove(infoCopy, "myIntegerKey", rc=rc)
!EOC
    if (rc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
    if (infoCopy == info) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!EOC
!BOE
! Destroy the copied object.
!EOE
!BOC
    call ESMF_InfoDestroy(infoCopy, rc=rc)
!EOC
    if (rc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    
!------------------------------------------------------------------------------

!BOE
! An \texttt{ESMF\_Info} object may be created from a JSON string. Note the usage of quotes
! is required as below.
!EOE
!BOC
    infoFromCh = ESMF_InfoCreate('{"hello":"world"}', rc=rc)
!EOC
    if (rc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE
! The contents of an \texttt{ESMF\_Info} object may be set in another \texttt{ESMF\_Info} object.
!EOE
!BOC
    call ESMF_InfoSet(info, "infoFromCh", infoFromCh, rc=rc)
!EOC
    if (rc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
    call ESMF_InfoDestroy(infoFromCh, rc=rc)
!EOC
    if (rc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE
! An allocatable character get interface is available.
!EOE
!BOC
    call ESMF_InfoGetCharAlloc(info, "/infoFromCh/hello", getCh, rc=rc)
!EOC
    if (rc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    
    if (getCh /= "world") call ESMF_Finalize(endflag=ESMF_END_ABORT)
    
!------------------------------------------------------------------------------

!BOE
! Destroy the \texttt{ESMF\_Info} object.
!EOE
!BOC
    call ESMF_InfoDestroy(info, rc=rc)
!EOC
    if (rc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!------------------------------------------------------------------------------

    ! IMPORTANT: ESMF_STest() prints the PASS string and the # of processors in the log
    ! file that the scripts grep for.
    call ESMF_STest((finalrc.eq.ESMF_SUCCESS), testname, failMsg, result, ESMF_SRCLINE)

    call ESMF_Finalize(rc=rc)
    if (rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE
    if (finalrc .eq. ESMF_SUCCESS) then
        print *, "PASS: ESMF_InfoTutorialEx.F90"
    else
        print *, "FAIL: ESMF_InfoTutorialEx.F90"
    end if

!==============================================================================
!==============================================================================

    end program ESMF_InfoTutorial
