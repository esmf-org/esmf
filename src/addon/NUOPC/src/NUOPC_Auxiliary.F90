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
#define FILENAME "src/addon/NUOPC/src/NUOPC_Auxiliary.F90"
!==============================================================================

module NUOPC_Auxiliary

  use ESMF
  use ESMF_IOScripMod !!!! Needed for the internal NUOPC_SCRIPWrite() method
                      !!!! TODO: Replace this once public Write() available.

  implicit none
  
  private
  
  public NUOPC_Write                      ! method
  
!==============================================================================
! 
! INTERFACE BLOCKS
!
!==============================================================================

  interface NUOPC_Write
    module procedure NUOPC_SCRIPWrite
    module procedure NUOPC_FactorsWrite
    module procedure NUOPC_FieldWrite
    module procedure NUOPC_StateWrite
  end interface
  
  !-----------------------------------------------------------------------------
  
  !-----------------------------------------------------------------------------
  contains
  !-----------------------------------------------------------------------------
  
  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_Write - Write a distributed interpolation matrix to file in SCRIP format
! !INTERFACE:
  ! call using generic interface: NUOPC_Write
  subroutine NUOPC_SCRIPWrite(factorList, factorIndexList, fileName, &
    relaxedflag, rc)
! !ARGUMENTS:
    real(ESMF_KIND_R8), intent(in), target    :: factorList(:)
    integer,            intent(in), target    :: factorIndexList(:,:) 
    character(*),       intent(in)            :: fileName
    logical,            intent(in),  optional :: relaxedflag
    integer,            intent(out), optional :: rc
! !DESCRIPTION:
!   \label{api_NUOPC_SCRIPWrite}
!   Write the destributed interpolaton matrix provided by {\tt factorList} 
!   and {\tt factorIndexList} to a SCRIP formatted NetCDF file. Each PET calls
!   with its local list of factors and indices. The call then writes the 
!   distributed factors into a single file. If the file already exists, the
!   contents is replaced by this call.
!
!   The arguments are:
!   \begin{description}
!   \item[factorList]
!     The distributed factor list.
!   \item[factorIndexList]
!     The distributed list of source and destination indices.
!   \item[fileName]
!     The name of the file to be written to.
!   \item[{[relaxedflag]}]
!     If {\tt .true.}, then no error is returned even if the call cannot write
!     the file due to library limitations. Default is {\tt .false.}.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    logical                 :: ioCapable
    logical                 :: doItFlag

    if (present(rc)) rc = ESMF_SUCCESS
    
    ioCapable = ESMF_IO_NETCDF_PRESENT
    
    doItFlag = .true. ! default
    if (present(relaxedFlag)) then
      doItFlag = .not.relaxedflag .or. (relaxedflag.and.ioCapable)
    endif
    
    if (doItFlag) then
      call ESMF_OutputSimpleWeightFile(fileName, factorList, &
        factorIndexList, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME)) return  ! bail out
    endif
    
  end subroutine
  !-----------------------------------------------------------------------------


  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_Write - Write a distributed factorList to file
! !INTERFACE:
  ! call using generic interface: NUOPC_Write
  subroutine NUOPC_FactorsWrite(factorList, fileName, rc)
! !ARGUMENTS:
    real(ESMF_KIND_R8), pointer               :: factorList(:)
    character(*),       intent(in)            :: fileName
    integer,            intent(out), optional :: rc
! !DESCRIPTION:
!
!   THIS METHOD IS DEPRECATED. Use \ref{api_NUOPC_SCRIPWrite} instead.
! 
!   Write the destributed {\tt factorList} to file. Each PET calls with its 
!   local list of factors. The call then writes the distributed factors into
!   a single file. The order of the factors in the file is first by PET, and 
!   within each PET the PET-local order is preserved. Changing the number of 
!   PETs for the same regrid operation will likely change the order of factors
!   across PETs, and therefore files written will differ.
!
!   The arguments are:
!   \begin{description}
!   \item[factorList]
!     The distributed factor list.
!   \item[fileName]
!     The name of the file to be written to.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    integer, allocatable            :: deBlockList(:,:,:), weightsPerPet(:)
    type(ESMF_VM)                   :: vm
    type(ESMF_DistGrid)             :: dg
    type(ESMF_Array)                :: array
    integer                         :: localPet, petCount
    integer                         :: j
    
    if (present(rc)) rc = ESMF_SUCCESS
    
    call ESMF_VMGetCurrent(vm, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
    call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
    allocate(weightsPerPet(petCount))
    call ESMF_VMAllGather(vm, (/size(factorList)/), weightsPerPet, &
      count=1, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
    allocate(deBlockList(1,2,petCount))
    do j=1, petCount
      if (j==1) then
        deBlockList(1,1,j) = 1
        deBlockList(1,2,j) = weightsPerPet(1)
      else
        deBlockList(1,1,j) = deBlockList(1,2,j-1) + 1
        deBlockList(1,2,j) = deBlockList(1,1,j) + weightsPerPet(j) - 1
      endif
    enddo
    dg = ESMF_DistGridCreate(minIndex=(/1/), &
      maxIndex=(/deBlockList(1,2,petCount)/), &
      deBlockList=deBlockList, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
    array = ESMF_ArrayCreate(dg, factorList, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
    call ESMF_ArrayWrite(array, fileName, variableName="weights", &
      status=ESMF_FILESTATUS_REPLACE, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
    call ESMF_ArrayDestroy(array, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
    call ESMF_DistGridDestroy(dg, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
    deallocate(weightsPerPet, deBlockList)
    
  end subroutine
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_Write - Write Field data to file
! !INTERFACE:
  ! call using generic interface: NUOPC_Write
  subroutine NUOPC_FieldWrite(field, fileName, overwrite, status, timeslice, &
    iofmt, relaxedflag, rc)
! !ARGUMENTS:
    type(ESMF_Field),           intent(in)            :: field
    character(*),               intent(in)            :: fileName
    logical,                    intent(in),  optional :: overwrite
    type(ESMF_FileStatus_Flag), intent(in),  optional :: status
    integer,                    intent(in),  optional :: timeslice
    type(ESMF_IOFmt_Flag),      intent(in),  optional :: iofmt
    logical,                    intent(in),  optional :: relaxedflag
    integer,                    intent(out), optional :: rc
! !DESCRIPTION:
!   Write the data in {\tt field} to {\tt file} under the field's "StandardName" 
!   attribute if supported by the {\tt iofmt}.
!
!   The arguments are:
!   \begin{description}
!   \item[field]
!     The {\tt ESMF\_Field} object whose data is to be written.
!   \item[fileName]
!     The name of the file to write to.
!   \item[{[overwrite]}]
!      A logical flag, the default is .false., i.e., existing Field data may
!      {\em not} be overwritten. If .true., the
!      data corresponding to each field's name will be
!      be overwritten. If the {\tt timeslice} option is given, only data for
!      the given timeslice may be overwritten.
!      Note that it is always an error to attempt to overwrite a NetCDF
!      variable with data which has a different shape.
!   \item[{[status]}]
!      The file status. Valid options are {\tt ESMF\_FILESTATUS\_NEW}, 
!      {\tt ESMF\_FILESTATUS\_OLD}, {\tt ESMF\_FILESTATUS\_REPLACE}, and
!      {\tt ESMF\_FILESTATUS\_UNKNOWN} (default).
!   \item[{[timeslice]}]
!     Time slice counter. Must be positive. The behavior of this
!     option may depend on the setting of the {\tt overwrite} flag:
!     \begin{description}
!     \item[{\tt overwrite = .false.}:]\ If the timeslice value is
!     less than the maximum time already in the file, the write will fail.
!     \item[{\tt overwrite = .true.}:]\ Any positive timeslice value is valid.
!     \end{description}
!     By default, i.e. by omitting the {\tt timeslice} argument, no
!     provisions for time slicing are made in the output file,
!     however, if the file already contains a time axis for the variable,
!     a timeslice one greater than the maximum will be written.
!   \item[{[iofmt]}]
!    The I/O format.  Valid options are  {\tt ESMF\_IOFMT\_BIN} and
!    {\tt ESMF\_IOFMT\_NETCDF}. If not present, file names with a {\tt .bin} 
!    extension will use {\tt ESMF\_IOFMT\_BIN}, and file names with a {\tt .nc}
!    extension will use {\tt ESMF\_IOFMT\_NETCDF}.  Other files default to
!    {\tt ESMF\_IOFMT\_NETCDF}.
!   \item[{[relaxedflag]}]
!     If {\tt .true.}, then no error is returned even if the call cannot write
!     the file due to library limitations. Default is {\tt .false.}.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    character(ESMF_MAXSTR)  :: standardName
    logical                 :: ioCapable
    logical                 :: doItFlag

    if (present(rc)) rc = ESMF_SUCCESS
    
    ioCapable = (ESMF_IO_PIO_PRESENT .and. &
      (ESMF_IO_NETCDF_PRESENT .or. ESMF_IO_PNETCDF_PRESENT))
    
    doItFlag = .true. ! default
    if (present(relaxedFlag)) then
      doItFlag = .not.relaxedflag .or. (relaxedflag.and.ioCapable)
    endif
    
    if (doItFlag) then
      
      call ESMF_AttributeGet(field, name="StandardName", &
        value=standardName, convention="NUOPC", purpose="Instance", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=FILENAME)) &
        return  ! bail out
    
      call ESMF_FieldWrite(field, fileName=fileName, &
        variableName=standardName, overwrite=overwrite, status=status, &
        timeslice=timeslice, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=FILENAME)) &
        return  ! bail out
      
    endif

  end subroutine
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_Write - Write the Fields within a State to NetCDF files
! !INTERFACE:
  ! call using generic interface: NUOPC_Write
  subroutine NUOPC_StateWrite(state, fieldNameList, fileNamePrefix, overwrite, &
    status, timeslice, relaxedflag, rc)
! !ARGUMENTS:
    type(ESMF_State),           intent(in)            :: state
    character(len=*),           intent(in),  optional :: fieldNameList(:)
    character(len=*),           intent(in),  optional :: fileNamePrefix
    logical,                    intent(in),  optional :: overwrite
    type(ESMF_FileStatus_Flag), intent(in),  optional :: status
    integer,                    intent(in),  optional :: timeslice
    logical,                    intent(in),  optional :: relaxedflag
    integer,                    intent(out), optional :: rc
! !DESCRIPTION:
!   Write the data of the fields within a {\tt state} to NetCDF files. Each 
!   field is written to an individual file using the "StandardName" attribute
!   as NetCDF attribute.
!
!   The arguments are:
!   \begin{description}
!   \item[state]
!     The {\tt ESMF\_State} object containing the fields.
!   \item[{[fieldNameList]}]
!     List of names of the fields to be written. By default write all the fields
!     in {\tt state}.
!   \item[{[fileNamePrefix]}]
!     File name prefix, common to all the files written.
!   \item[{[overwrite]}]
!      A logical flag, the default is .false., i.e., existing Field data may
!      {\em not} be overwritten. If .true., the
!      data corresponding to each field's name will be
!      be overwritten. If the {\tt timeslice} option is given, only data for
!      the given timeslice may be overwritten.
!      Note that it is always an error to attempt to overwrite a NetCDF
!      variable with data which has a different shape.
!   \item[{[status]}]
!      The file status. Valid options are {\tt ESMF\_FILESTATUS\_NEW}, 
!      {\tt ESMF\_FILESTATUS\_OLD}, {\tt ESMF\_FILESTATUS\_REPLACE}, and
!      {\tt ESMF\_FILESTATUS\_UNKNOWN} (default).
!   \item[{[timeslice]}]
!     Time slice counter. Must be positive. The behavior of this
!     option may depend on the setting of the {\tt overwrite} flag:
!     \begin{description}
!     \item[{\tt overwrite = .false.}:]\ If the timeslice value is
!     less than the maximum time already in the file, the write will fail.
!     \item[{\tt overwrite = .true.}:]\ Any positive timeslice value is valid.
!     \end{description}
!     By default, i.e. by omitting the {\tt timeslice} argument, no
!     provisions for time slicing are made in the output file,
!     however, if the file already contains a time axis for the variable,
!     a timeslice one greater than the maximum will be written.
!   \item[{[relaxedflag]}]
!     If {\tt .true.}, then no error is returned even if the call cannot write
!     the file due to library limitations. Default is {\tt .false.}.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    integer                         :: i, itemCount
    type(ESMF_Field)                :: field
    type(ESMF_StateItem_Flag)       :: itemType
    character(len=80)               :: fileName
    character(len=80), allocatable  :: fieldNameList_loc(:)

    if (present(rc)) rc = ESMF_SUCCESS

    if (present(fieldNameList)) then
      allocate(fieldNameList_loc(size(fieldNameList)))
      do i=1, size(fieldNameList)
        fieldNameList_loc(i) = trim(fieldNameList(i))
      enddo
    else
      call ESMF_StateGet(state, itemCount=itemCount, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      allocate(fieldNameList_loc(itemCount))
      call ESMF_StateGet(state, itemNameList=fieldNameList_loc, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif

    do i=1, size(fieldNameList_loc)
      call ESMF_StateGet(state, itemName=fieldNameList_loc(i), &
        itemType=itemType, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=FILENAME)) &
        return  ! bail out
      if (itemType == ESMF_STATEITEM_FIELD) then
        ! field is available in the state
        call ESMF_StateGet(state, itemName=fieldNameList_loc(i), field=field, &
          rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=FILENAME)) &
          return  ! bail out
        ! -> output to file
        if (present(fileNamePrefix)) then
          write (fileName,"(A)") fileNamePrefix//trim(fieldNameList_loc(i))//".nc"
        else
          write (fileName,"(A)") trim(fieldNameList_loc(i))//".nc"
        endif
        call NUOPC_FieldWrite(field, fileName=trim(fileName), &
          overwrite=overwrite, status=status, timeslice=timeslice, &
          relaxedflag=relaxedflag, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg="Failed writing file: "// &
          trim(fileName), &
          line=__LINE__, &
          file=FILENAME)) &
          return  ! bail out
      endif
    enddo
    
    deallocate(fieldNameList_loc)

  end subroutine
  !-----------------------------------------------------------------------------

end module
