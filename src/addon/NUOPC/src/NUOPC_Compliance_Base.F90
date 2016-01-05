! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2016, University Corporation for Atmospheric Research, 
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
! Laboratory, University of Michigan, National Centers for Environmental 
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
#define FILENAME "src/addon/NUOPC/src/NUOPC_Compliance_Base.F90"
!==============================================================================

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
! General compliance checks applicable to all types of NUOPC components.
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

module NUOPC_Compliance_Base

    use ESMF

    implicit none

    private


    public NUOPC_CheckComponentMetadata
    public NUOPC_CheckComponentMetadataCIM
    public NUOPC_CheckComponentAttribute
    public NUOPC_CheckState
    public NUOPC_CheckStateAttribute
    public NUOPC_CheckFieldAttribute
    public NUOPC_CheckClockUsageIncoming
    public NUOPC_CheckClockUsageOutgoing
    public NUOPC_CheckInternalClock
    public NUOPC_CheckComponentStatistics
    public NUOPC_CompSearchPhaseMapByIndex

    interface NUOPC_CheckComponentMetadata
        module procedure NUOPC_CheckGridComponentMetadata
        module procedure NUOPC_CheckCplComponentMetadata
    end interface

    interface NUOPC_CheckComponentAttribute
        module procedure NUOPC_CheckGridComponentAttribute
        module procedure NUOPC_CheckCplComponentAttribute
    end interface

    interface NUOPC_CompSearchPhaseMapByIndex
        module procedure NUOPC_GridCompSearchPhaseMapByIndex
        module procedure NUOPC_CplCompSearchPhaseMapByIndex
    end interface

contains

    recursive subroutine NUOPC_CheckGridComponentMetadata(prefix, comp, rc)
        character(*), intent(in)              :: prefix
        type(ESMF_GridComp)                   :: comp
        integer,      intent(out), optional   :: rc

        type(ESMF_CompType_Flag)              :: comptype
        character(ESMF_MAXSTR)                :: attributeName
        character(ESMF_MAXSTR)                :: convention
        character(ESMF_MAXSTR)                :: purpose

        if (present(rc)) rc = ESMF_SUCCESS

        ! get Component type and branch on it
!        call ESMF_GridCompGet(comp, comptype=comptype, rc=rc)
!        if (ESMF_LogFoundError(rc, &
!            line=__LINE__, &
!            file=FILENAME)) &
!            return  ! bail out


        ! set NUOPC convention and purpose specifiers
        convention = "NUOPC"
        purpose = "Instance"

        call ESMF_LogWrite(trim(prefix)//" GridComp level attribute check: "// &
            "convention: '"//trim(convention)//"', purpose: '"//trim(purpose)//"'.", &
            ESMF_LOGMSG_INFO, rc=rc)
        if (ESMF_LogFoundError(rc, &
            line=__LINE__, &
            file=FILENAME)) &
            return  ! bail out

        attributeName = "Verbosity"
        call NUOPC_CheckComponentAttribute(prefix, comp=comp, &
            attributeName=attributeName, convention=convention, purpose=purpose, &
            rc=rc)
        if (ESMF_LogFoundError(rc, &
            line=__LINE__, &
            file=FILENAME)) &
            return  ! bail out

        attributeName = "InitializePhaseMap"
        call NUOPC_CheckComponentAttribute(prefix, comp=comp, &
            attributeName=attributeName, convention=convention, purpose=purpose, &
            rc=rc)
        if (ESMF_LogFoundError(rc, &
            line=__LINE__, &
            file=FILENAME)) &
            return  ! bail out

        ! Applies only to NUOPC Drivers
        !attributeName = "InternalInitializePhaseMap"
        !call NUOPC_CheckComponentAttribute(prefix, comp=comp, &
        !    attributeName=attributeName, convention=convention, purpose=purpose, &
        !    rc=rc)
        !if (ESMF_LogFoundError(rc, &
        !    line=__LINE__, &
        !    file=FILENAME)) &
        !    return  ! bail out

        attributeName = "RunPhaseMap"
        call NUOPC_CheckComponentAttribute(prefix, comp=comp, &
            attributeName=attributeName, convention=convention, purpose=purpose, &
            rc=rc)
        if (ESMF_LogFoundError(rc, &
            line=__LINE__, &
            file=FILENAME)) &
            return  ! bail out

        attributeName = "FinalizePhaseMap"
        call NUOPC_CheckComponentAttribute(prefix, comp=comp, &
            attributeName=attributeName, convention=convention, purpose=purpose, &
            rc=rc)
        if (ESMF_LogFoundError(rc, &
            line=__LINE__, &
            file=FILENAME)) &
            return  ! bail out

        attributeName = "NestingGeneration"
        call NUOPC_CheckComponentAttribute(prefix, comp=comp, &
            attributeName=attributeName, convention=convention, purpose=purpose, &
            rc=rc)
        if (ESMF_LogFoundError(rc, &
            line=__LINE__, &
            file=FILENAME)) &
            return  ! bail out

        attributeName = "Nestling"
        call NUOPC_CheckComponentAttribute(prefix, comp=comp, &
            attributeName=attributeName, convention=convention, purpose=purpose, &
            rc=rc)
        if (ESMF_LogFoundError(rc, &
            line=__LINE__, &
            file=FILENAME)) &
            return  ! bail out

        attributeName = "InitializeDataComplete"
        call NUOPC_CheckComponentAttribute(prefix, comp=comp, &
            attributeName=attributeName, convention=convention, purpose=purpose, &
            rc=rc)
        if (ESMF_LogFoundError(rc, &
            line=__LINE__, &
            file=FILENAME)) &
            return  ! bail out

        attributeName = "InitializeDataProgress"
        call NUOPC_CheckComponentAttribute(prefix, comp=comp, &
            attributeName=attributeName, convention=convention, purpose=purpose, &
            rc=rc)
        if (ESMF_LogFoundError(rc, &
            line=__LINE__, &
            file=FILENAME)) &
            return  ! bail out


    end subroutine


    recursive subroutine NUOPC_CheckCplComponentMetadata(prefix, comp, rc)
        character(*), intent(in)              :: prefix
        type(ESMF_CplComp)                    :: comp
        integer,      intent(out), optional   :: rc

        type(ESMF_CompType_Flag)              :: comptype
        character(ESMF_MAXSTR)                :: attributeName
        character(ESMF_MAXSTR)                :: convention
        character(ESMF_MAXSTR)                :: purpose

        if (present(rc)) rc = ESMF_SUCCESS

        ! get Component type and branch on it
!        call ESMF_GridCompGet(comp, comptype=comptype, rc=rc)
!        if (ESMF_LogFoundError(rc, &
!            line=__LINE__, &
!            file=FILENAME)) &
!            return  ! bail out
!

        ! set NUOPC convention and purpose specifiers
        convention = "NUOPC"
        purpose = "Instance"

        call ESMF_LogWrite(trim(prefix)//" CplComp level attribute check: "// &
            "convention: '"//trim(convention)//"', purpose: '"//trim(purpose)//"'.", &
            ESMF_LOGMSG_INFO, rc=rc)
        if (ESMF_LogFoundError(rc, &
            line=__LINE__, &
            file=FILENAME)) &
            return  ! bail out

!            attributeName = "ComponentLongName"
!            call NUOPC_CheckComponentAttribute(prefix, comp=comp, &
!                attributeName=attributeName, convention=convention, purpose=purpose, &
!                rc=rc)
!            if (ESMF_LogFoundError(rc, &
!                line=__LINE__, &
!                file=FILENAME)) &
!                return  ! bail out

        attributeName = "Verbosity"
        call NUOPC_CheckComponentAttribute(prefix, comp=comp, &
            attributeName=attributeName, convention=convention, purpose=purpose, &
            rc=rc)
        if (ESMF_LogFoundError(rc, &
            line=__LINE__, &
            file=FILENAME)) &
            return  ! bail out

        attributeName = "InitializePhaseMap"
        call NUOPC_CheckComponentAttribute(prefix, comp=comp, &
            attributeName=attributeName, convention=convention, purpose=purpose, &
            rc=rc)
        if (ESMF_LogFoundError(rc, &
            line=__LINE__, &
            file=FILENAME)) &
            return  ! bail out

        attributeName = "RunPhaseMap"
        call NUOPC_CheckComponentAttribute(prefix, comp=comp, &
            attributeName=attributeName, convention=convention, purpose=purpose, &
            rc=rc)
        if (ESMF_LogFoundError(rc, &
            line=__LINE__, &
            file=FILENAME)) &
            return  ! bail out

        attributeName = "FinalizePhaseMap"
        call NUOPC_CheckComponentAttribute(prefix, comp=comp, &
            attributeName=attributeName, convention=convention, purpose=purpose, &
            rc=rc)
        if (ESMF_LogFoundError(rc, &
            line=__LINE__, &
            file=FILENAME)) &
            return  ! bail out

        ! moved to phase-specific check
!        attributeName = "CplList"
!        call NUOPC_CheckComponentAttribute(prefix, comp=comp, &
!            attributeName=attributeName, convention=convention, purpose=purpose, &
!            rc=rc)
!        if (ESMF_LogFoundError(rc, &
!            line=__LINE__, &
!            file=FILENAME)) &
!            return  ! bail out

    end subroutine



    recursive subroutine NUOPC_CheckComponentMetadataCIM(prefix, comp, rc)
        character(*), intent(in)              :: prefix
        type(ESMF_GridComp)                   :: comp
        integer,      intent(out), optional   :: rc

        type(ESMF_CompType_Flag)              :: comptype
        character(ESMF_MAXSTR)                :: attributeName
        character(ESMF_MAXSTR)                :: convention
        character(ESMF_MAXSTR)                :: purpose

        if (present(rc)) rc = ESMF_SUCCESS

        ! get Component type and branch on it
        call ESMF_GridCompGet(comp, comptype=comptype, rc=rc)
        if (ESMF_LogFoundError(rc, &
            line=__LINE__, &
            file=FILENAME)) &
            return  ! bail out

        if (comptype == ESMF_COMPTYPE_GRID) then

            ! set NUOPC convention and purpose specifiers
            convention = "NUOPC"
            purpose = "Instance"

            call ESMF_LogWrite(trim(prefix)//" GridComp level attribute check: "// &
                "convention: '"//trim(convention)//"', purpose: '"//trim(purpose)//"'.", &
                ESMF_LOGMSG_INFO, rc=rc)
            if (ESMF_LogFoundError(rc, &
                line=__LINE__, &
                file=FILENAME)) &
                return  ! bail out

            attributeName = "ShortName"
            call NUOPC_CheckComponentAttribute(prefix, comp=comp, &
                attributeName=attributeName, convention=convention, purpose=purpose, &
                rc=rc)
            if (ESMF_LogFoundError(rc, &
                line=__LINE__, &
                file=FILENAME)) &
                return  ! bail out

            attributeName = "LongName"
            call NUOPC_CheckComponentAttribute(prefix, comp=comp, &
                attributeName=attributeName, convention=convention, purpose=purpose, &
                rc=rc)
            if (ESMF_LogFoundError(rc, &
                line=__LINE__, &
                file=FILENAME)) &
                return  ! bail out

            attributeName = "Description"
            call NUOPC_CheckComponentAttribute(prefix, comp=comp, &
                attributeName=attributeName, convention=convention, purpose=purpose, &
                rc=rc)
            if (ESMF_LogFoundError(rc, &
                line=__LINE__, &
                file=FILENAME)) &
                return  ! bail out

            attributeName = "ModelType"
            call NUOPC_CheckComponentAttribute(prefix, comp=comp, &
                attributeName=attributeName, convention=convention, purpose=purpose, &
                rc=rc)
            if (ESMF_LogFoundError(rc, &
                line=__LINE__, &
                file=FILENAME)) &
                return  ! bail out

            attributeName = "ReleaseDate"
            call NUOPC_CheckComponentAttribute(prefix, comp=comp, &
                attributeName=attributeName, convention=convention, purpose=purpose, &
                rc=rc)
            if (ESMF_LogFoundError(rc, &
                line=__LINE__, &
                file=FILENAME)) &
                return  ! bail out

            attributeName = "PreviousVersion"
            call NUOPC_CheckComponentAttribute(prefix, comp=comp, &
                attributeName=attributeName, convention=convention, purpose=purpose, &
                rc=rc)
            if (ESMF_LogFoundError(rc, &
                line=__LINE__, &
                file=FILENAME)) &
                return  ! bail out

#if 0
            ! skip Citation* attributes as per Cecelia 10/05/10
      attributeName = "ShortTitle"
      call NUOPC_CheckComponentAttribute(prefix, comp=comp, &
        attributeName=attributeName, convention=convention, purpose=purpose, &
        rc=rc)
      if (ESMF_LogFoundError(rc, &
        line=__LINE__, &
        file=FILENAME)) &
        return  ! bail out

      attributeName = "LongTitle"
      call NUOPC_CheckComponentAttribute(prefix, comp=comp, &
        attributeName=attributeName, convention=convention, purpose=purpose, &
        rc=rc)
      if (ESMF_LogFoundError(rc, &
        line=__LINE__, &
        file=FILENAME)) &
        return  ! bail out

      attributeName = "Date"
      call NUOPC_CheckComponentAttribute(prefix, comp=comp, &
        attributeName=attributeName, convention=convention, purpose=purpose, &
        rc=rc)
      if (ESMF_LogFoundError(rc, &
        line=__LINE__, &
        file=FILENAME)) &
        return  ! bail out

      attributeName = "PresentationForm"
      call NUOPC_CheckComponentAttribute(prefix, comp=comp, &
        attributeName=attributeName, convention=convention, purpose=purpose, &
        rc=rc)
      if (ESMF_LogFoundError(rc, &
        line=__LINE__, &
        file=FILENAME)) &
        return  ! bail out

      attributeName = "DOI"
      call NUOPC_CheckComponentAttribute(prefix, comp=comp, &
        attributeName=attributeName, convention=convention, purpose=purpose, &
        rc=rc)
      if (ESMF_LogFoundError(rc, &
        line=__LINE__, &
        file=FILENAME)) &
        return  ! bail out
#endif

            attributeName = "ResponsiblePartyRole"
            call NUOPC_CheckComponentAttribute(prefix, comp=comp, &
                attributeName=attributeName, convention=convention, purpose=purpose, &
                rc=rc)
            if (ESMF_LogFoundError(rc, &
                line=__LINE__, &
                file=FILENAME)) &
                return  ! bail out

            attributeName = "Name"
            call NUOPC_CheckComponentAttribute(prefix, comp=comp, &
                attributeName=attributeName, convention=convention, purpose=purpose, &
                rc=rc)
            if (ESMF_LogFoundError(rc, &
                line=__LINE__, &
                file=FILENAME)) &
                return  ! bail out

            attributeName = "EmailAddress"
            call NUOPC_CheckComponentAttribute(prefix, comp=comp, &
                attributeName=attributeName, convention=convention, purpose=purpose, &
                rc=rc)
            if (ESMF_LogFoundError(rc, &
                line=__LINE__, &
                file=FILENAME)) &
                return  ! bail out

            attributeName = "PhysicalAddress"
            call NUOPC_CheckComponentAttribute(prefix, comp=comp, &
                attributeName=attributeName, convention=convention, purpose=purpose, &
                rc=rc)
            if (ESMF_LogFoundError(rc, &
                line=__LINE__, &
                file=FILENAME)) &
                return  ! bail out

            attributeName = "URL"
            call NUOPC_CheckComponentAttribute(prefix, comp=comp, &
                attributeName=attributeName, convention=convention, purpose=purpose, &
                rc=rc)
            if (ESMF_LogFoundError(rc, &
                line=__LINE__, &
                file=FILENAME)) &
                return  ! bail out

        elseif (comptype == ESMF_COMPTYPE_CPL) then

            ! set NUOPC convention and purpose specifiers
            convention = "NUOPC"
            purpose = "Instance"

            call ESMF_LogWrite(trim(prefix)//" CplComp level attribute check: "// &
                "convention: '"//trim(convention)//"', purpose: '"//trim(purpose)//"'.", &
                ESMF_LOGMSG_INFO, rc=rc)
            if (ESMF_LogFoundError(rc, &
                line=__LINE__, &
                file=FILENAME)) &
                return  ! bail out

            attributeName = "ComponentLongName"
            call NUOPC_CheckComponentAttribute(prefix, comp=comp, &
                attributeName=attributeName, convention=convention, purpose=purpose, &
                rc=rc)
            if (ESMF_LogFoundError(rc, &
                line=__LINE__, &
                file=FILENAME)) &
                return  ! bail out

        else
          ! currently there is no other type by GridComp or CplComp
        endif

    end subroutine




    recursive subroutine NUOPC_CheckState(prefix, referenceName, state, rc)
        character(*), intent(in)              :: prefix
        character(*), intent(in)              :: referenceName
        type(ESMF_State)                      :: state
        integer,      intent(out), optional   :: rc

        logical                               :: stateValid
        integer                               :: itemCount, item
        integer                               :: fieldCount, fitem
        character(ESMF_MAXSTR)                :: name
        type(ESMF_StateIntent_Flag)           :: stateintent
        character(ESMF_MAXSTR)                :: tempString
        character(ESMF_MAXSTR), allocatable   :: itemNameList(:)
        type(ESMF_StateItem_Flag), allocatable :: stateitemtypeList(:)
        type(ESMF_Field), allocatable         :: fields(:)
        type(ESMF_Field)                      :: field
        type(ESMF_FieldBundle)                :: fieldbundle
        type(ESMF_State)                      :: nestedState
        character(ESMF_MAXSTR)                :: nestedPrefix
        character(ESMF_MAXSTR)                :: attributeName
        character(ESMF_MAXSTR)                :: convention
        character(ESMF_MAXSTR)                :: purpose

        if (present(rc)) rc = ESMF_SUCCESS

        stateValid = .true.
        ! Ensure that the State is a valid object
        if (.not. ESMF_StateIsCreated(state)) then
            call ESMF_LogWrite(trim(prefix)//" ==> The "//trim(referenceName)// &
                " is invalid!", &
                ESMF_LOGMSG_WARNING, rc=rc)
            if (ESMF_LogFoundError(rc, &
                line=__LINE__, &
                file=FILENAME)) &
                return  ! bail out
            stateValid = .false.
        endif

        if (stateValid) then
            ! Provide name and type of State
            call ESMF_StateGet(state, name=name, stateintent=stateintent, &
                itemCount=itemCount, rc=rc)
            if (ESMF_LogFoundError(rc, &
                line=__LINE__, &
                file=FILENAME)) &
                return  ! bail out
            call ESMF_LogWrite(trim(prefix)//" "//trim(referenceName)//" name: "// &
                trim(name), ESMF_LOGMSG_INFO, rc=rc)
            if (ESMF_LogFoundError(rc, &
                line=__LINE__, &
                file=FILENAME)) &
                return  ! bail out
            if (stateintent==ESMF_STATEINTENT_IMPORT) then
                tempString = "ESMF_STATEINTENT_IMPORT"
            else if (stateintent==ESMF_STATEINTENT_EXPORT) then
                tempString = "ESMF_STATEINTENT_EXPORT"
            else if (stateintent==ESMF_STATEINTENT_UNSPECIFIED) then
                tempString = "ESMF_STATEINTENT_UNSPECIFIED"
            else
                tempString = "ESMF_STATEINTENT_INVALID"
            endif
            call ESMF_LogWrite(trim(prefix)//" "//trim(referenceName)//" stateintent: "// &
                trim(tempString), ESMF_LOGMSG_INFO, rc=rc)
            if (ESMF_LogFoundError(rc, &
                line=__LINE__, &
                file=FILENAME)) &
                return  ! bail out

            ! set NUOPC convention and purpose specifiers
            convention = "NUOPC"
            purpose = "Instance"

            call ESMF_LogWrite(trim(prefix)//" State level attribute check: "// &
                "convention: '"//trim(convention)//"', purpose: '"//trim(purpose)//"'.", &
                ESMF_LOGMSG_INFO, rc=rc)
            if (ESMF_LogFoundError(rc, &
                line=__LINE__, &
                file=FILENAME)) &
                return  ! bail out

            attributeName = "Namespace"
            call NUOPC_CheckStateAttribute(prefix, state=state, &
                attributeName=attributeName, convention=convention, purpose=purpose, &
                rc=rc)
            if (ESMF_LogFoundError(rc, &
                line=__LINE__, &
                file=FILENAME)) &
                return  ! bail out

            attributeName = "FieldTransferPolicy"
            call NUOPC_CheckStateAttribute(prefix, state=state, &
                attributeName=attributeName, convention=convention, purpose=purpose, &
                rc=rc)
            if (ESMF_LogFoundError(rc, &
                line=__LINE__, &
                file=FILENAME)) &
                return  ! bail out

            write (tempString, *) itemCount
            call ESMF_LogWrite(trim(prefix)//" "//trim(referenceName)//" itemCount: "// &
                trim(tempString), ESMF_LOGMSG_INFO, rc=rc)
            if (ESMF_LogFoundError(rc, &
                line=__LINE__, &
                file=FILENAME)) &
                return  ! bail out

            if (itemCount > 0) then
                allocate(itemNameList(itemCount))
                allocate(stateitemtypeList(itemCount))
                call ESMF_StateGet(state, itemNameList=itemNameList, &
                    itemtypeList=stateitemtypeList, rc=rc)
                if (ESMF_LogFoundError(rc, &
                    line=__LINE__, &
                    file=FILENAME)) &
                    return  ! bail out

                do item=1, itemCount
                    if (stateitemtypeList(item) == ESMF_STATEITEM_FIELD) then
                        write (tempString, *) item, " [FIELD] name: "
                    else if (stateitemtypeList(item) == ESMF_STATEITEM_FIELDBUNDLE) then
                        write (tempString, *) item, " [FIELDBUNDLE] name: "
                    else if (stateitemtypeList(item) == ESMF_STATEITEM_ARRAY) then
                        call ESMF_LogWrite(trim(prefix)//" ==> The "//trim(referenceName)// &
                            " contains an ESMF_Array object!", ESMF_LOGMSG_WARNING, rc=rc)
                        if (ESMF_LogFoundError(rc, &
                            line=__LINE__, &
                            file=FILENAME)) &
                            return  ! bail out
                        write (tempString, *) item, " [ARRAY] name: "
                    else if (stateitemtypeList(item) == ESMF_STATEITEM_ARRAYBUNDLE) then
                        call ESMF_LogWrite(trim(prefix)//" ==> The "//trim(referenceName)// &
                            " contains an ESMF_ArrayBundle object!", ESMF_LOGMSG_WARNING, rc=rc)
                        if (ESMF_LogFoundError(rc, &
                            line=__LINE__, &
                            file=FILENAME)) &
                            return  ! bail out
                        write (tempString, *) item, " [ARRAYBUNDLE] name: "
                    else if (stateitemtypeList(item) == ESMF_STATEITEM_ROUTEHANDLE) then
                        write (tempString, *) item, " [ROUTEHANDLE] name: "
                    else if (stateitemtypeList(item) == ESMF_STATEITEM_STATE) then
                        write (tempString, *) item, " [STATE] name: "
                    else if (stateitemtypeList(item) == ESMF_STATEITEM_UNKNOWN) then
                        write (tempString, *) item, " [UNKNOWN] name: "
                    else if (stateitemtypeList(item) == ESMF_STATEITEM_NOTFOUND) then
                        write (tempString, *) item, " [NOTFOUND] name: "
                    endif

                    call ESMF_LogWrite(trim(prefix)//" "//trim(referenceName)//" item #"// &
                        trim(tempString)//trim(itemNameList(item)), &
                        ESMF_LOGMSG_INFO, rc=rc)
                    if (ESMF_LogFoundError(rc, &
                        line=__LINE__, &
                        file=FILENAME)) &
                        return  ! bail out

                    ! check metadata compliance
                    if (stateitemtypeList(item) == ESMF_STATEITEM_FIELD) then
                       ! Can't check metadata here because the metadata that should
                       ! be set is dependent on the phase we are in

                    !            ! compliance check Field metadata
                    !            call ESMF_StateGet(state, itemName=itemNameList(item), &
                    !              field=field, rc=rc)
                    !            if (ESMF_LogFoundError(rc, &
                    !              line=__LINE__, &
                    !              file=FILENAME)) &
                    !              return  ! bail out
                    !            call checkFieldMetadata(prefix, field=field, rc=rc)
                    !            if (ESMF_LogFoundError(rc, &
                    !              line=__LINE__, &
                    !              file=FILENAME)) &
                    !              return  ! bail out
                    !          else if (stateitemtypeList(item) == ESMF_STATEITEM_FIELDBUNDLE) then
                    !            call ESMF_StateGet(state, itemName=itemNameList(item), &
                    !              fieldbundle=fieldbundle, rc=rc)
                    !            if (ESMF_LogFoundError(rc, &
                    !              line=__LINE__, &
                    !              file=FILENAME)) &
                    !              return  ! bail out
                    !            call ESMF_FieldBundleGet(fieldbundle, fieldCount=fieldCount, rc=rc)
                    !            if (ESMF_LogFoundError(rc, &
                    !              line=__LINE__, &
                    !              file=FILENAME)) &
                    !              return  ! bail out
                    !            allocate(fields(fieldCount))
                    !            call ESMF_FieldBundleGet(fieldbundle, fieldList=fields, rc=rc)
                    !            if (ESMF_LogFoundError(rc, &
                    !              line=__LINE__, &
                    !              file=FILENAME)) &
                    !              return  ! bail out
                    !            do fitem=1, fieldCount
                    !              field = fields(fitem)
                    !              call ESMF_FieldGet(field, name=name, rc=rc)
                    !              if (ESMF_LogFoundError(rc, &
                    !                line=__LINE__, &
                    !                file=FILENAME)) &
                    !                return  ! bail out
                    !              call ESMF_LogWrite(trim(prefix)//" in FieldBundle, Field name: "//&
                    !                trim(name), ESMF_LOGMSG_INFO, rc=rc)
                    !              if (ESMF_LogFoundError(rc, &
                    !                line=__LINE__, &
                    !                file=FILENAME)) &
                    !                return  ! bail out
                    !              call checkFieldMetadata(prefix, field=field, rc=rc)
                    !              if (ESMF_LogFoundError(rc, &
                    !                line=__LINE__, &
                    !                file=FILENAME)) &
                    !                return  ! bail out
                    !            enddo
                    !            deallocate(fields)
                    else if (stateitemtypeList(item) == ESMF_STATEITEM_STATE) then
                        ! recursive call
                        call ESMF_StateGet(state, itemName=itemNameList(item), &
                            nestedState=nestedState, rc=rc)
                        if (ESMF_LogFoundError(rc, &
                            line=__LINE__, &
                            file=FILENAME)) &
                            return  ! bail out
                        nestedPrefix = trim(prefix)//trim(itemNameList(item))//":"
                        call NUOPC_CheckState(nestedPrefix, referenceName, nestedState, rc=rc)
                        if (ESMF_LogFoundError(rc, &
                            line=__LINE__, &
                            file=FILENAME)) &
                            return  ! bail out
                    endif

                enddo

                deallocate(stateitemtypeList)
                deallocate(itemNameList)
            endif
        endif
    end subroutine

    recursive subroutine NUOPC_CheckGridComponentAttribute(prefix, comp, attributeName, &
        convention, purpose, warnIfMissing, rc)
        character(*), intent(in)              :: prefix
        type(ESMF_GridComp)                   :: comp
        character(*), intent(in)              :: attributeName
        character(*), intent(in)              :: convention
        character(*), intent(in)              :: purpose
        logical,      intent(in), optional    :: warnIfMissing
        integer,      intent(out), optional   :: rc

        type(ESMF_AttPack)                    :: attpack
        type(ESMF_TypeKind_Flag)              :: typekind
        integer                               :: itemCount, i
        logical                               :: isPresent
        character(10*ESMF_MAXSTR), pointer    :: valueStringList(:)
        character(ESMF_MAXSTR)                :: iStr, vStr
        integer(ESMF_KIND_I4), pointer        :: valueI4List(:)
        logical                               :: warn

        if (present(warnIfMissing)) then
            warn = warnIfMissing
        else
            warn = .true.
        endif

        call ESMF_AttributeGetAttPack(comp, attpack=attpack, &
            convention=convention, purpose=purpose, isPresent=isPresent, rc=rc)
        if (ESMF_LogFoundError(rc, &
            line=__LINE__, &
            file=FILENAME)) &
            return  ! bail out
        if (.not.isPresent) then
            ! attpack not present
            call ESMF_LogWrite(trim(prefix)//" ==> Component level attpack: <"// &
                "convention: '"//trim(convention)//"', "// &
                "purpose: '"//trim(purpose)//"'> is NOT present!", &
                ESMF_LOGMSG_WARNING, rc=rc)
            if (ESMF_LogFoundError(rc, &
                line=__LINE__, &
                file=FILENAME)) &
                return  ! bail out
        endif
        call ESMF_AttributeGet(comp, name=attributeName, attpack=attpack, &
            typekind=typekind, itemCount=itemCount, isPresent=isPresent, &
            attnestflag=ESMF_ATTNEST_ON, rc=rc)
        if (ESMF_LogFoundError(rc, &
            line=__LINE__, &
            file=FILENAME)) &
            return  ! bail out
        if (.not.warn .and. (.not.isPresent .or. itemCount==0)) then
            return
        else if (.not.isPresent) then
            ! attribute not present
            call ESMF_LogWrite(trim(prefix)//" ==> Component level attribute: <"// &
                trim(attributeName)//"> is NOT present!", &
                ESMF_LOGMSG_WARNING, rc=rc)
            if (ESMF_LogFoundError(rc, &
                line=__LINE__, &
                file=FILENAME)) &
                return  ! bail out
        else if (itemCount == 0) then
            ! attribute present but not set
            call ESMF_LogWrite(trim(prefix)//" ==> Component level attribute: <"// &
                trim(attributeName)//"> present but NOT set!", &
                ESMF_LOGMSG_WARNING, rc=rc)
            if (ESMF_LogFoundError(rc, &
                line=__LINE__, &
                file=FILENAME)) &
                return  ! bail out
        else
            ! attribute present and set
            if (typekind == ESMF_TYPEKIND_CHARACTER) then
                allocate(valueStringList(itemCount))
                call ESMF_AttributeGet(comp, name=attributeName, &
                    valueList=valueStringList, &
                    convention=convention, purpose=purpose, &
                    attnestflag=ESMF_ATTNEST_ON, rc=rc)
                if (itemCount == 1) then
                    ! single valued
                    call ESMF_LogWrite(trim(prefix)//" Component level attribute: <"// &
                        trim(attributeName)//"> "// &
                        "present and set: "// trim(valueStringList(1)), &
                        ESMF_LOGMSG_INFO, rc=rc)
                    if (ESMF_LogFoundError(rc, &
                        line=__LINE__, &
                        file=FILENAME)) &
                        return  ! bail out
                else
                    ! multi valued -> requires loop
                    do i=1, itemCount
                        write(iStr,*) i
                        call ESMF_LogWrite(trim(prefix)//" Component level attribute: <"// &
                            trim(attributeName)//">["//trim(adjustl(iStr))//"] "// &
                            "present and set: "// trim(valueStringList(i)), &
                            ESMF_LOGMSG_INFO, rc=rc)
                        if (ESMF_LogFoundError(rc, &
                            line=__LINE__, &
                            file=FILENAME)) &
                            return  ! bail out
                    enddo
                endif
                deallocate(valueStringList)
            else if (typekind == ESMF_TYPEKIND_I4) then
                allocate(valueI4List(itemCount))
                call ESMF_AttributeGet(comp, name=attributeName, &
                    valueList=valueI4List, &
                    convention=convention, purpose=purpose, &
                    attnestflag=ESMF_ATTNEST_ON, rc=rc)
                if (itemCount == 1) then
                    ! single valued
                    write(vStr,*) valueI4List(1)
                    call ESMF_LogWrite(trim(prefix)//" Component level attribute: <"// &
                        trim(attributeName)//"> "// &
                        "present and set: "// vStr, &
                        ESMF_LOGMSG_INFO, rc=rc)
                    if (ESMF_LogFoundError(rc, &
                        line=__LINE__, &
                        file=FILENAME)) &
                        return  ! bail out
                else
                    ! multi valued -> requires loop
                    do i=1, itemCount
                        write(iStr,*) i
                        write(vStr,*) valueI4List(i)
                        call ESMF_LogWrite(trim(prefix)//" Component level attribute: <"// &
                            trim(attributeName)//">["//trim(adjustl(iStr))//"] "// &
                            "present and set: "// vStr, &
                            ESMF_LOGMSG_INFO, rc=rc)
                        if (ESMF_LogFoundError(rc, &
                            line=__LINE__, &
                            file=FILENAME)) &
                            return  ! bail out
                    enddo
                endif
                deallocate(valueI4List)
            else
                call ESMF_LogWrite(trim(prefix)//" Component level attribute: <"// &
                    trim(attributeName)//"> "// &
                    "present and set: <unsupported data type>", &
                    ESMF_LOGMSG_INFO, rc=rc)
                if (ESMF_LogFoundError(rc, &
                    line=__LINE__, &
                    file=FILENAME)) &
                    return  ! bail out
            endif
        endif

    end subroutine

    ! unfortunately identical to above except for the ESMF_CplComp parameter
    recursive subroutine NUOPC_CheckCplComponentAttribute(prefix, comp, attributeName, &
        convention, purpose, rc)
        character(*), intent(in)              :: prefix
        type(ESMF_CplComp)                    :: comp
        character(*), intent(in)              :: attributeName
        character(*), intent(in)              :: convention
        character(*), intent(in)              :: purpose
        integer,      intent(out), optional   :: rc

        type(ESMF_AttPack)                    :: attpack
        type(ESMF_TypeKind_Flag)              :: typekind
        integer                               :: itemCount, i
        logical                               :: isPresent
        character(10*ESMF_MAXSTR), pointer    :: valueStringList(:)
        character(ESMF_MAXSTR)                :: iStr, vStr
        integer(ESMF_KIND_I4), pointer        :: valueI4List(:)

        call ESMF_AttributeGetAttPack(comp, attpack=attpack, &
            convention=convention, purpose=purpose, isPresent=isPresent, rc=rc)
        if (ESMF_LogFoundError(rc, &
            line=__LINE__, &
            file=FILENAME)) &
            return  ! bail out
        if (.not.isPresent) then
            ! attpack not present
            call ESMF_LogWrite(trim(prefix)//" ==> Component level attpack: <"// &
                "convention: '"//trim(convention)//"', "// &
                "purpose: '"//trim(purpose)//"'> is NOT present!", &
                ESMF_LOGMSG_WARNING, rc=rc)
            if (ESMF_LogFoundError(rc, &
                line=__LINE__, &
                file=FILENAME)) &
                return  ! bail out
        endif
        call ESMF_AttributeGet(comp, name=attributeName, attpack=attpack, &
            typekind=typekind, itemCount=itemCount, isPresent=isPresent, &
            attnestflag=ESMF_ATTNEST_ON, rc=rc)
        if (ESMF_LogFoundError(rc, &
            line=__LINE__, &
            file=FILENAME)) &
            return  ! bail out
        if (.not.isPresent) then
            ! attribute not present
            call ESMF_LogWrite(trim(prefix)//" ==> Component level attribute: <"// &
                trim(attributeName)//"> is NOT present!", &
                ESMF_LOGMSG_WARNING, rc=rc)
            if (ESMF_LogFoundError(rc, &
                line=__LINE__, &
                file=FILENAME)) &
                return  ! bail out
        else if (itemCount == 0) then
            ! attribute present but not set
            call ESMF_LogWrite(trim(prefix)//" ==> Component level attribute: <"// &
                trim(attributeName)//"> present but NOT set!", &
                ESMF_LOGMSG_WARNING, rc=rc)
            if (ESMF_LogFoundError(rc, &
                line=__LINE__, &
                file=FILENAME)) &
                return  ! bail out
        else
            ! attribute present and set
            if (typekind == ESMF_TYPEKIND_CHARACTER) then
                allocate(valueStringList(itemCount))
                call ESMF_AttributeGet(comp, name=attributeName, &
                    valueList=valueStringList, &
                    convention=convention, purpose=purpose, &
                    attnestflag=ESMF_ATTNEST_ON, rc=rc)
                if (itemCount == 1) then
                    ! single valued
                    call ESMF_LogWrite(trim(prefix)//" Component level attribute: <"// &
                        trim(attributeName)//"> "// &
                        "present and set: "// trim(valueStringList(1)), &
                        ESMF_LOGMSG_INFO, rc=rc)
                    if (ESMF_LogFoundError(rc, &
                        line=__LINE__, &
                        file=FILENAME)) &
                        return  ! bail out
                else
                    ! multi valued -> requires loop
                    do i=1, itemCount
                        write(iStr,*) i
                        call ESMF_LogWrite(trim(prefix)//" Component level attribute: <"// &
                            trim(attributeName)//">["//trim(adjustl(iStr))//"] "// &
                            "present and set: "// trim(valueStringList(i)), &
                            ESMF_LOGMSG_INFO, rc=rc)
                        if (ESMF_LogFoundError(rc, &
                            line=__LINE__, &
                            file=FILENAME)) &
                            return  ! bail out
                    enddo
                endif
                deallocate(valueStringList)
            else if (typekind == ESMF_TYPEKIND_I4) then
                allocate(valueI4List(itemCount))
                call ESMF_AttributeGet(comp, name=attributeName, &
                    valueList=valueI4List, &
                    convention=convention, purpose=purpose, &
                    attnestflag=ESMF_ATTNEST_ON, rc=rc)
                if (itemCount == 1) then
                    ! single valued
                    write(vStr,*) valueI4List(1)
                    call ESMF_LogWrite(trim(prefix)//" Component level attribute: <"// &
                        trim(attributeName)//"> "// &
                        "present and set: "// vStr, &
                        ESMF_LOGMSG_INFO, rc=rc)
                    if (ESMF_LogFoundError(rc, &
                        line=__LINE__, &
                        file=FILENAME)) &
                        return  ! bail out
                else
                    ! multi valued -> requires loop
                    do i=1, itemCount
                        write(iStr,*) i
                        write(vStr,*) valueI4List(i)
                        call ESMF_LogWrite(trim(prefix)//" Component level attribute: <"// &
                            trim(attributeName)//">["//trim(adjustl(iStr))//"] "// &
                            "present and set: "// vStr, &
                            ESMF_LOGMSG_INFO, rc=rc)
                        if (ESMF_LogFoundError(rc, &
                            line=__LINE__, &
                            file=FILENAME)) &
                            return  ! bail out
                    enddo
                endif
                deallocate(valueI4List)
            else
                call ESMF_LogWrite(trim(prefix)//" Component level attribute: <"// &
                    trim(attributeName)//"> "// &
                    "present and set: <unsupported data type>", &
                    ESMF_LOGMSG_INFO, rc=rc)
                if (ESMF_LogFoundError(rc, &
                    line=__LINE__, &
                    file=FILENAME)) &
                    return  ! bail out
            endif
        endif

    end subroutine

    recursive subroutine NUOPC_CheckStateAttribute(prefix, state, attributeName, &
        convention, purpose, rc)
        character(*), intent(in)              :: prefix
        type(ESMF_State)                      :: state
        character(*), intent(in)              :: attributeName
        character(*), intent(in)              :: convention
        character(*), intent(in)              :: purpose
        integer,      intent(out), optional   :: rc

        type(ESMF_AttPack)                    :: attpack
        type(ESMF_TypeKind_Flag)              :: typekind
        integer                               :: itemCount, i
        logical                               :: isPresent
        character(10*ESMF_MAXSTR), pointer    :: valueStringList(:)
        character(ESMF_MAXSTR)                :: iStr, vStr
        integer(ESMF_KIND_I4), pointer        :: valueI4List(:)

        call ESMF_AttributeGetAttPack(state, attpack=attpack, &
            convention=convention, purpose=purpose, isPresent=isPresent, rc=rc)
        if (ESMF_LogFoundError(rc, &
            line=__LINE__, &
            file=FILENAME)) &
            return  ! bail out
        if (.not.isPresent) then
            ! attpack not present
            call ESMF_LogWrite(trim(prefix)//" ==> State level attpack: <"// &
                "convention: '"//trim(convention)//"', "// &
                "purpose: '"//trim(purpose)//"'> is NOT present!", &
                ESMF_LOGMSG_WARNING, rc=rc)
            if (ESMF_LogFoundError(rc, &
                line=__LINE__, &
                file=FILENAME)) &
                return  ! bail out
        endif
        call ESMF_AttributeGet(state, name=attributeName, attpack=attpack, &
            typekind=typekind, itemCount=itemCount, isPresent=isPresent, &
            attnestflag=ESMF_ATTNEST_ON, rc=rc)
        if (ESMF_LogFoundError(rc, &
            line=__LINE__, &
            file=FILENAME)) &
            return  ! bail out
        if (.not.isPresent) then
            ! attribute not present
            call ESMF_LogWrite(trim(prefix)//" ==> State level attribute: <"// &
                trim(attributeName)//"> is NOT present!", &
                ESMF_LOGMSG_WARNING, rc=rc)
            if (ESMF_LogFoundError(rc, &
                line=__LINE__, &
                file=FILENAME)) &
                return  ! bail out
        else if (itemCount == 0) then
            ! attribute present but not set
            call ESMF_LogWrite(trim(prefix)//" ==> State level attribute: <"// &
                trim(attributeName)//"> present but NOT set!", &
                ESMF_LOGMSG_WARNING, rc=rc)
            if (ESMF_LogFoundError(rc, &
                line=__LINE__, &
                file=FILENAME)) &
                return  ! bail out
        else
            ! attribute present and set
            if (typekind == ESMF_TYPEKIND_CHARACTER) then
                allocate(valueStringList(itemCount))
                call ESMF_AttributeGet(state, name=attributeName, &
                    valueList=valueStringList, &
                    convention=convention, purpose=purpose, &
                    attnestflag=ESMF_ATTNEST_ON, rc=rc)
                if (itemCount == 1) then
                    ! single valued
                    call ESMF_LogWrite(trim(prefix)//" State level attribute: <"// &
                        trim(attributeName)//"> "// &
                        "present and set: "// trim(valueStringList(1)), &
                        ESMF_LOGMSG_INFO, rc=rc)
                    if (ESMF_LogFoundError(rc, &
                        line=__LINE__, &
                        file=FILENAME)) &
                        return  ! bail out
                else
                    ! multi valued -> requires loop
                    do i=1, itemCount
                        write(iStr,*) i
                        call ESMF_LogWrite(trim(prefix)//" State level attribute: <"// &
                            trim(attributeName)//">["//trim(adjustl(iStr))//"] "// &
                            "present and set: "// trim(valueStringList(i)), &
                            ESMF_LOGMSG_INFO, rc=rc)
                        if (ESMF_LogFoundError(rc, &
                            line=__LINE__, &
                            file=FILENAME)) &
                            return  ! bail out
                    enddo
                endif
                deallocate(valueStringList)
            else if (typekind == ESMF_TYPEKIND_I4) then
                allocate(valueI4List(itemCount))
                call ESMF_AttributeGet(state, name=attributeName, &
                    valueList=valueI4List, &
                    convention=convention, purpose=purpose, &
                    attnestflag=ESMF_ATTNEST_ON, rc=rc)
                if (itemCount == 1) then
                    ! single valued
                    write(vStr,*) valueI4List(1)
                    call ESMF_LogWrite(trim(prefix)//" State level attribute: <"// &
                        trim(attributeName)//"> "// &
                        "present and set: "// vStr, &
                        ESMF_LOGMSG_INFO, rc=rc)
                    if (ESMF_LogFoundError(rc, &
                        line=__LINE__, &
                        file=FILENAME)) &
                        return  ! bail out
                else
                    ! multi valued -> requires loop
                    do i=1, itemCount
                        write(iStr,*) i
                        write(vStr,*) valueI4List(i)
                        call ESMF_LogWrite(trim(prefix)//" State level attribute: <"// &
                            trim(attributeName)//">["//trim(adjustl(iStr))//"] "// &
                            "present and set: "// vStr, &
                            ESMF_LOGMSG_INFO, rc=rc)
                        if (ESMF_LogFoundError(rc, &
                            line=__LINE__, &
                            file=FILENAME)) &
                            return  ! bail out
                    enddo
                endif
                deallocate(valueI4List)
            else
                call ESMF_LogWrite(trim(prefix)//" State level attribute: <"// &
                    trim(attributeName)//"> "// &
                    "present and set: <unsupported data type>", &
                    ESMF_LOGMSG_INFO, rc=rc)
                if (ESMF_LogFoundError(rc, &
                    line=__LINE__, &
                    file=FILENAME)) &
                    return  ! bail out
            endif
        endif

    end subroutine

    !-------------------------------------------------------------------------


    recursive subroutine NUOPC_CheckFieldAttribute(prefix, field, attributeName, &
        convention, purpose, warnIfMissing, rc)
        character(*), intent(in)              :: prefix
        type(ESMF_Field)                      :: field
        character(*), intent(in)              :: attributeName
        character(*), intent(in)              :: convention
        character(*), intent(in)              :: purpose
        logical,      intent(in), optional    :: warnIfMissing
        integer,      intent(out), optional   :: rc

        type(ESMF_AttPack)                    :: attpack
        type(ESMF_TypeKind_Flag)              :: typekind
        integer                               :: itemCount, i
        logical                               :: isPresent
        character(10*ESMF_MAXSTR), pointer    :: valueStringList(:)
        character(ESMF_MAXSTR)                :: iStr, vStr
        integer(ESMF_KIND_I4), pointer        :: valueI4List(:)
        logical                               :: warn

        if (present(warnIfMissing)) then
            warn = warnIfMissing
        else
            warn = .true.
        endif

        call ESMF_AttributeGetAttPack(field, attpack=attpack, &
            convention=convention, purpose=purpose, isPresent=isPresent, rc=rc)
        if (ESMF_LogFoundError(rc, &
            line=__LINE__, &
            file=FILENAME)) &
            return  ! bail out
        if (.not.isPresent) then
            ! attpack not present
            call ESMF_LogWrite(trim(prefix)//" ==> Field level attpack: <"// &
                "convention: '"//trim(convention)//"', "// &
                "purpose: '"//trim(purpose)//"'> is NOT present!", &
                ESMF_LOGMSG_WARNING, rc=rc)
            if (ESMF_LogFoundError(rc, &
                line=__LINE__, &
                file=FILENAME)) &
                return  ! bail out
        endif
        call ESMF_AttributeGet(field, name=attributeName, attpack=attpack, &
            typekind=typekind, itemCount=itemCount, isPresent=isPresent, &
            attnestflag=ESMF_ATTNEST_ON, rc=rc)
        if (ESMF_LogFoundError(rc, &
            line=__LINE__, &
            file=FILENAME)) &
            return  ! bail out
        if (.not.warn .and. (.not.isPresent .or. itemCount==0)) then
            return
        else if (.not.isPresent) then
            ! attribute not present
            call ESMF_LogWrite(trim(prefix)//" ==> Field level attribute: <"// &
                trim(attributeName)//"> is NOT present!", &
                ESMF_LOGMSG_WARNING, rc=rc)
            if (ESMF_LogFoundError(rc, &
                line=__LINE__, &
                file=FILENAME)) &
                return  ! bail out
        else if (itemCount == 0) then
            ! attribute present but not set
            call ESMF_LogWrite(trim(prefix)//" ==> Field level attribute: <"// &
                trim(attributeName)//"> present but NOT set!", &
                ESMF_LOGMSG_WARNING, rc=rc)
            if (ESMF_LogFoundError(rc, &
                line=__LINE__, &
                file=FILENAME)) &
                return  ! bail out
        else
            ! attribute present and set
            if (typekind == ESMF_TYPEKIND_CHARACTER) then
                allocate(valueStringList(itemCount))
                call ESMF_AttributeGet(field, name=attributeName, &
                    valueList=valueStringList, &
                    convention=convention, purpose=purpose, &
                    attnestflag=ESMF_ATTNEST_ON, rc=rc)
                if (itemCount == 1) then
                    ! single valued
                    call ESMF_LogWrite(trim(prefix)//" Field level attribute: <"// &
                        trim(attributeName)//"> "// &
                        "present and set: "// trim(valueStringList(1)), &
                        ESMF_LOGMSG_INFO, rc=rc)
                    if (ESMF_LogFoundError(rc, &
                        line=__LINE__, &
                        file=FILENAME)) &
                        return  ! bail out
                else
                    ! multi valued -> requires loop
                    do i=1, itemCount
                        write(iStr,*) i
                        call ESMF_LogWrite(trim(prefix)//" Field level attribute: <"// &
                            trim(attributeName)//">["//trim(adjustl(iStr))//"] "// &
                            "present and set: "// trim(valueStringList(i)), &
                            ESMF_LOGMSG_INFO, rc=rc)
                        if (ESMF_LogFoundError(rc, &
                            line=__LINE__, &
                            file=FILENAME)) &
                            return  ! bail out
                    enddo
                endif
                deallocate(valueStringList)
            else if (typekind == ESMF_TYPEKIND_I4) then
                allocate(valueI4List(itemCount))
                call ESMF_AttributeGet(field, name=attributeName, &
                    valueList=valueI4List, &
                    convention=convention, purpose=purpose, &
                    attnestflag=ESMF_ATTNEST_ON, rc=rc)
                if (itemCount == 1) then
                    ! single valued
                    write(vStr,*) valueI4List(1)
                    call ESMF_LogWrite(trim(prefix)//" Field level attribute: <"// &
                        trim(attributeName)//"> "// &
                        "present and set: "// vStr, &
                        ESMF_LOGMSG_INFO, rc=rc)
                    if (ESMF_LogFoundError(rc, &
                        line=__LINE__, &
                        file=FILENAME)) &
                        return  ! bail out
                else
                    ! multi valued -> requires loop
                    do i=1, itemCount
                        write(iStr,*) i
                        write(vStr,*) valueI4List(i)
                        call ESMF_LogWrite(trim(prefix)//" Field level attribute: <"// &
                            trim(attributeName)//">["//trim(adjustl(iStr))//"] "// &
                            "present and set: "// vStr, &
                            ESMF_LOGMSG_INFO, rc=rc)
                        if (ESMF_LogFoundError(rc, &
                            line=__LINE__, &
                            file=FILENAME)) &
                            return  ! bail out
                    enddo
                endif
                deallocate(valueI4List)
            else
                call ESMF_LogWrite(trim(prefix)//" Field level attribute: <"// &
                    trim(attributeName)//"> "// &
                    "present and set: <unsupported data type>", &
                    ESMF_LOGMSG_INFO, rc=rc)
                if (ESMF_LogFoundError(rc, &
                    line=__LINE__, &
                    file=FILENAME)) &
                    return  ! bail out
            endif
        endif

    end subroutine



    recursive subroutine NUOPC_CheckClockUsageIncoming(prefix, clock, clockCopy, rc)
        character(*), intent(in)                :: prefix
        type(ESMF_Clock), intent(in)            :: clock
        type(ESMF_Clock), intent(inout)         :: clockCopy
        integer,          intent(out), optional :: rc

        type(ESMF_Pointer)                      :: clockThis
        logical                                 :: clockValid

        if (present(rc)) rc = ESMF_SUCCESS

        clockValid = .true.
        ! Ensure that the Clock is a valid object
        ! Clock has deep C++ implementation, thus must also check this pointer here
        call ESMF_ClockGetThis(clock, clockThis, rc=rc)
        if (ESMF_LogFoundError(rc, &
            line=__LINE__, &
            file=FILENAME)) &
            return  ! bail out
        if (.not.ESMF_ClockIsCreated(clock) .or. &
            (clockThis == ESMF_NULL_POINTER)) then
            call ESMF_LogWrite(trim(prefix)//" ==> The incoming Clock is invalid!", &
                ESMF_LOGMSG_WARNING, rc=rc)
            if (ESMF_LogFoundError(rc, &
                line=__LINE__, &
                file=FILENAME)) &
                return  ! bail out
            clockValid = .false.
        endif

        if (clockValid) then
            clockCopy = ESMF_ClockCreate(clock, rc=rc)
            if (ESMF_LogFoundError(rc, &
                line=__LINE__, &
                file=FILENAME)) &
                return  ! bail out
        endif

    end subroutine

    !-------------------------------------------------------------------------

    recursive subroutine NUOPC_CheckClockUsageOutgoing(prefix, clock, clockCopy, rc)
        character(*), intent(in)                :: prefix
        type(ESMF_Clock), intent(in)            :: clock
        type(ESMF_Clock), intent(inout)         :: clockCopy
        integer,          intent(out), optional :: rc

        type(ESMF_Pointer)                      :: clockThis
        logical                                 :: clockValid
        logical                                 :: clockModified

        character (ESMF_MAXSTR) :: name, nameCopy
        type(ESMF_TimeInterval) :: timeStep, timeStepCopy
        type(ESMF_Time)         :: startTime, startTimeCopy
        type(ESMF_Time)         :: stopTime, stopTimeCopy
        type(ESMF_TimeInterval) :: runDuration, runDurationCopy
        real(ESMF_KIND_R8)      :: runTimeStepCount, runTimeStepCountCopy
        type(ESMF_Time)         :: refTime, refTimeCopy
        type(ESMF_Time)         :: currTime, currTimeCopy
        integer(ESMF_KIND_I8)   :: advanceCount, advanceCountCopy
        type(ESMF_Direction_Flag)    :: direction, directionCopy

        if (present(rc)) rc = ESMF_SUCCESS

        clockValid = .true.
        ! Ensure that the Clock is a valid object
        ! Clock has deep C++ implementation, thus must also check this pointer here
        call ESMF_ClockGetThis(clock, clockThis, rc=rc)
        if (ESMF_LogFoundError(rc, &
            line=__LINE__, &
            file=FILENAME)) &
            return  ! bail out
        if (.not.ESMF_ClockIsCreated(clock) .or. &
            (clockThis == ESMF_NULL_POINTER)) clockValid = .false.
        ! Further ensure that the clockCopy is a valid object
        ! Clock has deep C++ implementation, thus must also check this pointer here
        call ESMF_ClockGetThis(clockCopy, clockThis, rc=rc)
        if (ESMF_LogFoundError(rc, &
            line=__LINE__, &
            file=FILENAME)) &
            return  ! bail out
        if (.not.ESMF_ClockIsCreated(clockCopy) .or. &
            (clockThis == ESMF_NULL_POINTER)) clockValid = .false.

        if (clockValid) then
            clockModified = .false.

            call ESMF_ClockGet(clock, name=name, timeStep=timeStep, &
                startTime=startTime, stopTime=stopTime, runDuration=runDuration, &
                runTimeStepCount=runTimeStepCount, refTime=refTime, currTime=currTime, &
                advanceCount=advanceCount, direction=direction, rc=rc)
            if (ESMF_LogFoundError(rc, &
                line=__LINE__, &
                file=FILENAME)) &
                return  ! bail out

            call ESMF_ClockGet(clockCopy, name=nameCopy, timeStep=timeStepCopy, &
                startTime=startTimeCopy, stopTime=stopTimeCopy, runDuration=runDurationCopy, &
                runTimeStepCount=runTimeStepCountCopy, refTime=refTimeCopy, currTime=currTimeCopy, &
                advanceCount=advanceCountCopy, direction=directionCopy, rc=rc)
            if (ESMF_LogFoundError(rc, &
                line=__LINE__, &
                file=FILENAME)) &
                return  ! bail out

            if (name /= nameCopy) clockModified = .true.
            if (timeStep /= timeStepCopy) clockModified = .true.
            if (startTime /= startTimeCopy) clockModified = .true.
            if (stopTime /= stopTimeCopy) clockModified = .true.
            if (runDuration /= runDurationCopy) clockModified = .true.
            if (runTimeStepCount /= runTimeStepCountCopy) clockModified = .true.
            if (refTime /= refTimeCopy) clockModified = .true.
            if (currTime /= currTimeCopy) clockModified = .true.
            if (advanceCount /= advanceCountCopy) clockModified = .true.
            if (direction /= directionCopy) clockModified = .true.

            if (clockModified) then
                call ESMF_LogWrite(trim(prefix)//" ==> The incoming Clock was modified!", &
                    ESMF_LOGMSG_WARNING, rc=rc)
                if (ESMF_LogFoundError(rc, &
                    line=__LINE__, &
                    file=FILENAME)) &
                    return  ! bail out
            else
                call ESMF_LogWrite(trim(prefix)//" The incoming Clock was not modified.", &
                    ESMF_LOGMSG_INFO, rc=rc)
                if (ESMF_LogFoundError(rc, &
                    line=__LINE__, &
                    file=FILENAME)) &
                    return  ! bail out
            endif

            call ESMF_ClockDestroy(clockCopy, rc=rc)
            if (ESMF_LogFoundError(rc, &
                line=__LINE__, &
                file=FILENAME)) &
                return  ! bail out

        endif

    end subroutine

    recursive subroutine NUOPC_CheckInternalClock(prefix, comp, clock, mustMatchCurr, &
        mustReachStop, rc)
        character(*), intent(in)                :: prefix
        type(ESMF_GridComp)                     :: comp
        type(ESMF_Clock), intent(in)            :: clock
        logical,          intent(in)            :: mustMatchCurr
        logical,          intent(in)            :: mustReachStop
        integer,          intent(out), optional :: rc

        logical                                 :: clockValid
        logical                                 :: clockInternalValid
        type(ESMF_Clock)                        :: clockInternal
        logical                                 :: clockMatch
        logical                                 :: clockIsPresent

        character (ESMF_MAXSTR) :: name, nameInt
        type(ESMF_TimeInterval) :: timeStep, timeStepInt
        type(ESMF_Time)         :: startTime, startTimeInt
        type(ESMF_Time)         :: stopTime, stopTimeInt
        type(ESMF_TimeInterval) :: runDuration, runDurationInt
        real(ESMF_KIND_R8)      :: runTimeStepCount, runTimeStepCountInt
        type(ESMF_Time)         :: refTime, refTimeInt
        type(ESMF_Time)         :: currTime, currTimeInt
        integer(ESMF_KIND_I8)   :: advanceCount, advanceCountInt
        type(ESMF_Direction_Flag)    :: direction, directionInt

        if (present(rc)) rc = ESMF_SUCCESS

        call ESMF_GridCompGet(comp, clockIsPresent=clockIsPresent, rc=rc)
        if (ESMF_LogFoundError(rc, &
            line=__LINE__, &
            file=FILENAME)) &
            return  ! bail out

        if (.not.clockIsPresent) then

            call ESMF_LogWrite(trim(prefix)// &
                " ==> The internal Clock is not present!", &
                ESMF_LOGMSG_WARNING, rc=rc)
            if (ESMF_LogFoundError(rc, &
                line=__LINE__, &
                file=FILENAME)) &
                return  ! bail out
            return

        else

            call ESMF_GridCompGet(comp, clock=clockInternal, rc=rc)
            if (ESMF_LogFoundError(rc, &
                line=__LINE__, &
                file=FILENAME)) &
                return  ! bail out

            clockInternalValid = .true.
            ! Ensure that the internalClock is a valid object
            if (.not.ESMF_ClockIsCreated(clockInternal)) &
                clockInternalValid = .false.

            if (.not.clockInternalValid) then
                call ESMF_LogWrite(trim(prefix)//" ==> The internal Clock is invalid!", &
                    ESMF_LOGMSG_WARNING, rc=rc)
                if (ESMF_LogFoundError(rc, &
                    line=__LINE__, &
                    file=FILENAME)) &
                    return  ! bail out
                return
            endif

        endif

        ! making it to hear means that the internal Clock is present and valid
        ! -> go and try to compare it to the passed in Clock object

        clockValid = .true.
        ! Ensure that the Clock is a valid object
        if (.not.ESMF_ClockIsCreated(clock)) &
            clockValid = .false.

        if (.not.clockValid) then
            call ESMF_LogWrite(trim(prefix)//" ==> No Clock to compare internal Clock!", &
                ESMF_LOGMSG_WARNING, rc=rc)
            if (ESMF_LogFoundError(rc, &
                line=__LINE__, &
                file=FILENAME)) &
                return  ! bail out
            return
        endif

        call ESMF_ClockGet(clock, name=name, timeStep=timeStep, &
            startTime=startTime, stopTime=stopTime, runDuration=runDuration, &
            runTimeStepCount=runTimeStepCount, refTime=refTime, currTime=currTime, &
            advanceCount=advanceCount, direction=direction, rc=rc)
        if (ESMF_LogFoundError(rc, &
            line=__LINE__, &
            file=FILENAME)) &
            return  ! bail out

        call ESMF_ClockGet(clockInternal, name=nameInt, timeStep=timeStepInt, &
            startTime=startTimeInt, stopTime=stopTimeInt, runDuration=runDurationInt, &
            runTimeStepCount=runTimeStepCountInt, refTime=refTimeInt, currTime=currTimeInt, &
            advanceCount=advanceCountInt, direction=directionInt, rc=rc)
        if (ESMF_LogFoundError(rc, &
            line=__LINE__, &
            file=FILENAME)) &
            return  ! bail out

        clockMatch = .true. ! initialize

        if (startTimeInt /= startTime) then
            call ESMF_LogWrite(trim(prefix)//" ==> startTime of internal Clock does not match Clock!", &
                ESMF_LOGMSG_WARNING, rc=rc)
            if (ESMF_LogFoundError(rc, &
                line=__LINE__, &
                file=FILENAME)) &
                return  ! bail out
            clockMatch = .false.
        endif

        if (mustMatchCurr) then
            if (currTimeInt /= currTime) then
                call ESMF_LogWrite(trim(prefix)//" ==> currTime of internal Clock does not match Clock!", &
                    ESMF_LOGMSG_WARNING, rc=rc)
                if (ESMF_LogFoundError(rc, &
                    line=__LINE__, &
                    file=FILENAME)) &
                    return  ! bail out
                clockMatch = .false.
            endif
        endif

        if (clockMatch) then
            call ESMF_LogWrite(trim(prefix)//" The internal Clock matches incoming Clock.", &
                ESMF_LOGMSG_INFO, rc=rc)
            if (ESMF_LogFoundError(rc, &
                line=__LINE__, &
                file=FILENAME)) &
                return  ! bail out
        endif

        if (mustReachStop) then
            if (currTimeInt /= stopTimeInt) then
                call ESMF_LogWrite(trim(prefix)//" ==> The internal Clock has not run to its stopTime!", &
                    ESMF_LOGMSG_WARNING, rc=rc)
                if (ESMF_LogFoundError(rc, &
                    line=__LINE__, &
                    file=FILENAME)) &
                    return  ! bail out
            else
                call ESMF_LogWrite(trim(prefix)//" The internal Clock has run to its stopTime.", &
                    ESMF_LOGMSG_INFO, rc=rc)
                if (ESMF_LogFoundError(rc, &
                    line=__LINE__, &
                    file=FILENAME)) &
                    return  ! bail out
            endif
        endif

    end subroutine

    recursive subroutine NUOPC_CheckComponentStatistics(prefix, comp, rc)
        character(*), intent(in)              :: prefix
        type(ESMF_GridComp)                   :: comp
        integer,      intent(out), optional   :: rc

        integer                 :: fobjCount, objCount
        integer                 :: virtMemPet, physMemPet
        character(ESMF_MAXSTR)  :: output

        if (present(rc)) rc = ESMF_SUCCESS

        ! memory statistics for this PET
        call ESMF_VMGetMemInfo(virtMemPet, physMemPet, rc=rc)
        if (ESMF_LogFoundError(rc, &
            line=__LINE__, &
            file=FILENAME)) &
            return  ! bail out

        write (output, *) virtMemPet
        call ESMF_LogWrite(trim(prefix)//"ESMF Stats: "//&
            "the virtual memory used by this PET (in KB): "// &
            trim(adjustl(output)), ESMF_LOGMSG_INFO, rc=rc)
        if (ESMF_LogFoundError(rc, &
            line=__LINE__, &
            file=FILENAME)) &
            return  ! bail out

        write (output, *) physMemPet
        call ESMF_LogWrite(trim(prefix)//"ESMF Stats: "//&
            "the physical memory used by this PET (in KB): "// &
            trim(adjustl(output)), ESMF_LOGMSG_INFO, rc=rc)
        if (ESMF_LogFoundError(rc, &
            line=__LINE__, &
            file=FILENAME)) &
            return  ! bail out

        ! objects tracked by the ESMF garbage collection
        call ESMF_VMGetCurrentGarbageInfo(fobjCount, objCount, rc=rc)
        if (ESMF_LogFoundError(rc, &
            line=__LINE__, &
            file=FILENAME)) &
            return  ! bail out

        write (output, *) fobjCount
        call ESMF_LogWrite(trim(prefix)//"ESMF Stats: "//&
            "ESMF Fortran objects referenced by the ESMF garbage collection: "// &
            trim(adjustl(output)), ESMF_LOGMSG_INFO, rc=rc)
        if (ESMF_LogFoundError(rc, &
            line=__LINE__, &
            file=FILENAME)) &
            return  ! bail out

        write (output, *) objCount
        call ESMF_LogWrite(trim(prefix)//"ESMF Stats: "//&
            "ESMF objects (F & C++) referenced by the ESMF garbage collection: "// &
            trim(adjustl(output)), ESMF_LOGMSG_INFO, rc=rc)
        if (ESMF_LogFoundError(rc, &
            line=__LINE__, &
            file=FILENAME)) &
            return  ! bail out

    end subroutine

    !
    ! The below could be moved into NUOPC_Comp with appropriate interface definition
    !

    !BOPI
    ! !IROUTINE: NUOPC_CompSearchPhaseMapByIndex - Search the Phase Map of a GridComp
    ! !INTERFACE:
    ! Private name; call using NUOPC_CompSearchPhaseMapByIndex()
    subroutine NUOPC_GridCompSearchPhaseMapByIndex(comp, methodflag, phaseIndex, &
        phaseLabel, attributeToCheck, rc)

        ! remove these when put into NUOPC_Comp module
        use ESMF
        use NUOPC_Base, only : NUOPC_PhaseMapStringLength
        implicit none

        ! !ARGUMENTS:
        type(ESMF_GridComp)                           :: comp
        type(ESMF_Method_Flag), intent(in)            :: methodflag
        integer,                intent(in)            :: phaseIndex
        character(*),           intent(out)           :: phaseLabel
        character(*),           intent(in), optional  :: attributeToCheck
        integer,                intent(out), optional :: rc
        !
        ! !DESCRIPTION:
        ! Return the {\tt phaseLabel} associated with a {\tt methodFlag} and
        ! {\tt phaseIndex}.  If a matching {\tt methodFlag} and {\tt phaseIndex}
        ! are not found, {\tt phaseLabel} is set to an empty string. If multiple
        ! matching {\tt phaseLabel}s are found, the first is returned.  The
        ! default attribute is checked, e.g., "InitializePhaseMap", but this
        ! can be overridden by providing an attributeToCheck argument.
        !
        !EOPI

        !-----------------------------------------------------------------------------
        ! local variables
        integer                   :: i
        integer                   :: itemCount, stat, ind, max, tempPhaseIndex
        character(ESMF_MAXSTR)    :: name
        character(len=40)         :: attributeName
        character(len=NUOPC_PhaseMapStringLength), pointer  :: phases(:)

        rc = ESMF_SUCCESS

        ! query the Component for info
        call ESMF_GridCompGet(comp, name=name, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

        ! determine which phaseMap to deal with
        attributeName = "UnknownPhaseMap" ! initialize to something obvious
        if (methodflag == ESMF_METHOD_INITIALIZE) then
            attributeName = "InitializePhaseMap"
        elseif (methodflag == ESMF_METHOD_RUN) then
            attributeName = "RunPhaseMap"
        elseif (methodflag == ESMF_METHOD_FINALIZE) then
            attributeName = "FinalizePhaseMap"
        endif

        if (present(attributeToCheck)) attributeName=attributeToCheck

        phaseLabel = "none"

        ! access phaseMap info
        call ESMF_AttributeGet(comp, name=trim(attributeName), &
            itemCount=itemCount, &
            convention="NUOPC", purpose="Instance", &
            attnestflag=ESMF_ATTNEST_ON, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

        ! search the phaseMap
        if (itemCount > 0) then
            allocate(phases(itemCount), stat=stat)
            if (ESMF_LogFoundAllocError(statusToCheck=stat, &
                msg="Allocation of temporary data structure.", &
                line=__LINE__, &
                file=trim(name)//":"//FILENAME)) return  ! bail out
            call ESMF_AttributeGet(comp, name=trim(attributeName), valueList=phases, &
                convention="NUOPC", purpose="Instance", &
                attnestflag=ESMF_ATTNEST_ON, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

            do i=1, itemCount
                !print *, "PHASES(i) = " // phases(i)
                ind = index(phases(i), trim("="))
                if (ind==0) cycle
                max = len(phases(i))
                read (phases(i)(ind+1:max), "(i4)") tempPhaseIndex
                !print *, "tempPhaseIndex = ", tempPhaseIndex
                if (tempPhaseIndex==phaseIndex) then
                    phaseLabel = (phases(i)(1:ind-1))
                    exit ! just take first one
                endif
            enddo

            ! clean-up
            deallocate(phases)
        endif

    end subroutine

    !BOPI
    ! !IROUTINE: NUOPC_CompSearchPhaseMapByIndex - Search the Phase Map of a GridComp
    ! !INTERFACE:
    ! Private name; call using NUOPC_CompSearchPhaseMapByIndex()
    subroutine NUOPC_CplCompSearchPhaseMapByIndex(comp, methodflag, phaseIndex, &
        phaseLabel, rc)

        ! remove these when put into NUOPC_Comp module
        use ESMF
        use NUOPC_Base, only : NUOPC_PhaseMapStringLength
        implicit none

        ! !ARGUMENTS:
        type(ESMF_CplComp)                           :: comp
        type(ESMF_Method_Flag), intent(in)            :: methodflag
        integer,                intent(in)            :: phaseIndex
        character(*),           intent(out)           :: phaseLabel
        integer,                intent(out), optional :: rc
        !
        ! !DESCRIPTION:
        ! Return the {\tt phaseLabel} associated with a {\tt methodFlag} and
        ! {\tt phaseIndex}.  If a matching {\tt methodFlag} and {\tt phaseIndex}
        ! are not found, {\tt phaseLabel} is set to an empty string. If multiple
        ! matching {\tt phaseLabel}s are found, the first is returned.
        !EOPI

        !-----------------------------------------------------------------------------
        ! local variables
        integer                   :: i
        integer                   :: itemCount, stat, ind, max, tempPhaseIndex
        character(ESMF_MAXSTR)    :: name
        character(len=40)         :: attributeName
        character(len=NUOPC_PhaseMapStringLength), pointer  :: phases(:)

        rc = ESMF_SUCCESS

        ! query the Component for info
        call ESMF_CplCompGet(comp, name=name, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

        ! determine which phaseMap to deal with
        attributeName = "UnknownPhaseMap" ! initialize to something obvious
        if (methodflag == ESMF_METHOD_INITIALIZE) then
            attributeName = "InitializePhaseMap"
        elseif (methodflag == ESMF_METHOD_RUN) then
            attributeName = "RunPhaseMap"
        elseif (methodflag == ESMF_METHOD_FINALIZE) then
            attributeName = "FinalizePhaseMap"
        endif

        phaseLabel = "none"

        ! access phaseMap info
        call ESMF_AttributeGet(comp, name=trim(attributeName), &
            itemCount=itemCount, &
            convention="NUOPC", purpose="Instance", &
            attnestflag=ESMF_ATTNEST_ON, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

        ! search the phaseMap
        if (itemCount > 0) then
            allocate(phases(itemCount), stat=stat)
            if (ESMF_LogFoundAllocError(statusToCheck=stat, &
                msg="Allocation of temporary data structure.", &
                line=__LINE__, &
                file=trim(name)//":"//FILENAME)) return  ! bail out
            call ESMF_AttributeGet(comp, name=trim(attributeName), valueList=phases, &
                convention="NUOPC", purpose="Instance", &
                attnestflag=ESMF_ATTNEST_ON, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

            do i=1, itemCount
                !print *, "PHASES(i) = " // phases(i)
                ind = index(phases(i), trim("="))
                if (ind==0) cycle
                max = len(phases(i))
                read (phases(i)(ind+1:max), "(i4)") tempPhaseIndex
                !print *, "tempPhaseIndex = ", tempPhaseIndex
                if (tempPhaseIndex==phaseIndex) then
                    phaseLabel = (phases(i)(1:ind-1))
                    exit ! just take first one
                endif
            enddo

            ! clean-up
            deallocate(phases)
        endif

end subroutine


end module NUOPC_Compliance_Base



