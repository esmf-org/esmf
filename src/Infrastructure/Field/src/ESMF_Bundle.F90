! $Id: ESMF_Bundle.F90,v 1.6 2003/05/07 04:34:30 cdeluca Exp $
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
!     ESMF Bundle Module
      module ESMF_BundleMod 
!
!==============================================================================
!------------------------------------------------------------------------------
! INCLUDES
#include "ESMF_Macros.inc"
!------------------------------------------------------------------------------
!
!BOPI
! !MODULE: ESMF_BundleMod
!
! !DESCRIPTION:
! The code in this file implements the {\tt ESMF\_Bundle} class, which 
! represents a set of {\tt Fields} discretized on the same {\tt Grid}.
! {\tt Bundle}s offer the option to pack the data from the {\tt Field}s
! they contain into a single buffer. 
!
!  This type is implemented in Fortran 90 and a corresponding
!  C++ interface is provided.

!
! !USES:
      use ESMF_BaseMod
      use ESMF_IOMod
      use ESMF_DataMapMod
      use ESMF_ArrayMod
      use ESMF_GridMod
      use ESMF_FieldMod
      implicit none
!
! !PRIVATE TYPES:
      private

!------------------------------------------------------------------------------
!     ! ESMF_PackFlag   
!     !
!     ! Data type to set the status of data in this Bundle; it can either be
!     ! simply a collection of Fields which contain the data, or it can also 
!     ! have a private packed data buffer associated directly with the Bundle.

      type ESMF_PackFlag
      sequence
      private
        integer :: packflag
      end type

      type(ESMF_PackFlag), parameter :: ESMF_PACK_FIELD_DATA = ESMF_PackFlag(1), &
                                        ESMF_NO_PACKED_DATA = ESMF_PackFlag(2)

!------------------------------------------------------------------------------
!       ! ESMF_FieldInterleave
!       !
!       !  Data type to record the ordering information for multiple field
!       !  data which is packed in a bundle.  Each has an associated
!       !  {\tt DataMap} object to track the ordering of that Field's data
!       !  in the packed buffer.
        type ESMF_FieldInterleave
        sequence
        private
          integer, pointer :: field_order                 ! index field order
          type(ESMF_DataMap), pointer :: field_dm         ! array of data maps
        end type

!------------------------------------------------------------------------------
!     ! ESMF_LocalBundle
!
      type ESMF_LocalBundle
      sequence
      private
        type(ESMF_GridType), pointer :: gridp  ! local part of grid 
        type(ESMF_Status) :: gridstatus        ! is grid set yet?
        type(ESMF_Status) :: arraystatus       ! init, uninit, etc
        type(ESMF_Array) :: packed_data        ! local packed array
        integer :: accesscount                 ! reserved for future use
      
      end type

!------------------------------------------------------------------------------
!     ESMF_BundleType
!
      type ESMF_BundleType
      sequence
      private
        type(ESMF_Base) :: base                   ! base class object
        type(ESMF_Status) :: bundlestatus         ! uninit, init ok, etc
        type(ESMF_Grid) :: grid                   ! associated global grid
        type(ESMF_GridType), pointer :: gridp     ! shortcut directly to grid data
        type(ESMF_Status) :: gridstatus           ! is grid set yet?
        type(ESMF_LocalBundle) :: localbundle     ! this differs per DE
        type(ESMF_Packflag) :: pack_flag          ! is packed data present?
        type(ESMF_FieldInterleave) :: fil         ! ordering in buffer
        type(ESMF_IOSpec) :: iospec               ! iospec values
        type(ESMF_Status) :: iostatus             ! if unset, inherit from gcomp
        type(ESMF_Field), dimension(:), pointer :: flist  ! list of fields
        integer :: field_count                    ! how many fields in here
      
      end type

!------------------------------------------------------------------------------
!     ! ESMF_Bundle

!     ! The Bundle data structure that is passed between implementation and
!     ! calling languages.

      type ESMF_Bundle
      sequence
      private
        type (ESMF_BundleType), pointer :: btypep    ! pointer to a bundle type
      end type

!------------------------------------------------------------------------------
!
! !PUBLIC TYPES:
      public ESMF_Bundle, ESMF_PackFlag, ESMF_PACK_FIELD_DATA, ESMF_NO_PACKED_DATA
      public ESMF_BundleType   ! intended for internal ESMF use only


! !PUBLIC MEMBER FUNCTIONS:
!
       public ESMF_BundleCreate       ! Create a new Bundle
       public ESMF_BundleDestroy      ! Destroy a Bundle

!      public ESMF_BundleAttachData   ! Associate data with a Bundle -
!                                     !   reference (default) or copy
!      public ESMF_BundleDetachData   ! Disassociate data with a Bundle

       public ESMF_BundleGetName      ! Get Bundle name
!      public ESMF_BundleGetConfig    ! Has associated Grid or data

       public ESMF_BundleGetFieldCount ! Get count of Fields 
       public ESMF_BundleGetFields    ! Get one or more Fields by name or number
       public ESMF_BundleAddFields    ! Add one or more Fields 
!      public ESMF_BundleRemoveFields ! Delete one or more Fields by name or number

       public ESMF_BundlePackData     ! Pack bundle data into a single 
!                                     !   buffer

!      public ESMF_BundleGetGrid           ! Return reference to Grid
!      public ESMF_BundleGetGlobalGridInfo ! Return global grid info
!      public ESMF_BundleGetLocalGridInfo  ! Return local grid info

!      public ESMF_BundleGetGlobalDataInfo ! Return global data info
!      public ESMF_BundleGetLocalDataInfo  ! Return local data info

       public ESMF_BundleValidate     ! Check internal consistency
       public ESMF_BundlePrint        ! Print contents of a Bundle

       public operator(.eq.), operator(.ne.)

!  !subroutine ESMF_BundleGetDataMap
!
!  !subroutine ESMF_BundleCheckpoint(bundle, iospec, rc)
!  !function ESMF_BundleRestore(name, iospec, rc)
!  !subroutine ESMF_BundleWrite(bundle, subarray, iospec, rc)
!  !function ESMF_BundleRead(name, iospec, rc)

! !PRIVATE MEMBER FUNCTIONS:
!  ! additional future signatures of ESMF_BundleCreate() functions:
!  !function ESMF_BundleCreateCopy(bundle, subarray, name, packflag, rc)
!  !function ESMF_BundleCreateRemap(bundle, grid, name, packflag, rc)

!EOPI



!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_BundleCreate - Create a new Bundle
!
! !INTERFACE:
     interface ESMF_BundleCreate

! !PRIVATE MEMBER FUNCTIONS:
        module procedure ESMF_BundleCreateNew
        module procedure ESMF_BundleCreateNoFields

! !DESCRIPTION:
! This interface provides a single entry point for the various
!  types of BundleCreate functions.
!EOP
      end interface

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_BundleConstruct - Construct the internals of a new Bundle
!
! !INTERFACE:
     interface ESMF_BundleConstruct

! !PRIVATE MEMBER FUNCTIONS:
        module procedure ESMF_BundleConstructNew
        module procedure ESMF_BundleConstructNoFields

! !DESCRIPTION:
! This interface provides a single entry point for the various
!  types of BundleConstruct functions.
!EOPI
      end interface

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_BundleGetFields - Retrieve Fields from a Bundle
!
! !INTERFACE:
     interface ESMF_BundleGetFields

! !PRIVATE MEMBER FUNCTIONS:
        module procedure ESMF_BundleGetFieldByName
        module procedure ESMF_BundleGetFieldByNum

! !DESCRIPTION:
! This interface provides a single entry point for the various
!  types of BundleGetField functions.
!EOP
      end interface

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_BundleAddFields - Add Fields to a Bundle
!
! !INTERFACE:
     interface ESMF_BundleAddFields

! !PRIVATE MEMBER FUNCTIONS:
        module procedure ESMF_BundleAddField
        module procedure ESMF_BundleAddFieldList

! !DESCRIPTION:
! This interface provides a single entry point for the various
!  types of BundleAddField functions.
!EOP
      end interface


!------------------------------------------------------------------------------
interface operator (.eq.)
 module procedure ESMF_pfeq
end interface

interface operator (.ne.)
 module procedure ESMF_pfne
end interface


!------------------------------------------------------------------------------

      contains


!------------------------------------------------------------------------------
! function to compare two ESMF_PackFlags to see if they're the same or not

function ESMF_pfeq(pf1, pf2)
 logical ESMF_pfeq
 type(ESMF_PackFlag), intent(in) :: pf1, pf2

 ESMF_pfeq = (pf1%packflag .eq. pf2%packflag)
end function

function ESMF_pfne(pf1, pf2)
 logical ESMF_pfne
 type(ESMF_PackFlag), intent(in) :: pf1, pf2

 ESMF_pfne = (pf1%packflag .ne. pf2%packflag)
end function


!
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!
! This section includes all the Bundle Create and Destroy routines
!
!

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_BundleCreateNew - Create a Bundle from existing Fields
!
! !INTERFACE:
      function ESMF_BundleCreateNew(fieldcount, fields, packflag, name, iospec, rc)
!
! !RETURN VALUE:
      type(ESMF_Bundle) :: ESMF_BundleCreateNew
!
! !ARGUMENTS:
      integer, intent(in) :: fieldcount           
      type(ESMF_Field), dimension (:) :: fields
      type(ESMF_PackFlag), intent(in), optional :: packflag 
      character (len = *), intent(in), optional :: name 
      type(ESMF_IOSpec), intent(in), optional :: iospec
      integer, intent(out), optional :: rc             

!
! !DESCRIPTION:
!     Create a {\tt Bundle} from a list of existing
!     gridded {\tt Fields}.  Optionally create a packed
!     {\tt Array} which collects all {\tt Field} data into
!     a single contiguous memory buffer.  All {\tt Field}s
!     must share a common {\tt Grid}.  Return a new {\tt Bundle}.
!
!     The arguments are:
!     \begin{description}
!     \item [fieldcount]
!           Number of fields to be added to the {\tt Bundle}.
!           Must be equal to or less than the number of 
!           {\tt Field}s in the following argument.
!     \item [fields]
!           Array of existing {\tt Field}s.  The first {\tt fieldcount}
!           items will be added to the {\tt Bundle}.
!     \item [{[packflag]}]
!           If set to {\tt ESMF\_PACK\_FIELD\_DATA}, the {\tt Field}
!           data in individual {\tt Array}s will be collected
!           into a single data {\tt Array} for the entire {\tt Bundle}.
!           The default is {\tt ESMF\_NO\_PACKED\_DATA}.
!     \item [{[name]}]
!           {\tt Bundle} name.  A default name will be generated if
!           one is not specified.
!     \item [{[iospec]}]
!           I/O specification.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!
!     \end{description}
!
!
! !REQUIREMENTS:  
!EOP

      type(ESMF_BundleType), pointer :: btypep         ! Pointer to new bundle
      integer :: status                                ! Error status
      logical :: rcpresent                             ! Return code present

!     Initialize pointers
      nullify(btypep)
      nullify(ESMF_BundleCreateNew%btypep)

!     Initialize return code
      status = ESMF_FAILURE
      if(present(rc)) then
        rcpresent=.TRUE.
        rc = ESMF_FAILURE
      else
        rcpresent = .FALSE.
      endif

      allocate(btypep,  stat=status)
!     If error write message and return.
!     Formal error handling will be added asap.
      if(status .NE. 0) then
        print *, "ERROR in ESMF_BundleCreateNew: Allocate"
        return
      endif

!     Call construction method to allocate and initialize bundle internals.
      call ESMF_BundleConstructNew(btypep, fieldcount, fields, &
                                              packflag, name, iospec, rc)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_BundleCreateNew: Bundle construct"
        return
      endif

!     Set return values.
      ESMF_BundleCreateNew%btypep => btypep
      if(rcpresent) rc = ESMF_SUCCESS


      end function ESMF_BundleCreateNew


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_BundleCreateNoFields - Create an empty Bundle
!
! !INTERFACE:
      function ESMF_BundleCreateNoFields(name, iospec, rc)
!
! !RETURN VALUE:
      type(ESMF_Bundle) :: ESMF_BundleCreateNoFields
!
! !ARGUMENTS:
      character (len = *), intent(in), optional :: name 
      type(ESMF_IOSpec), intent(in), optional :: iospec
      integer, intent(out), optional :: rc             

!
! !DESCRIPTION:
!     Create an empty {\tt Bundle}.
!
!     The arguments are:
!     \begin{description}
!     \item [{[name]}]
!           {\tt Bundle} name.  A default name will be generated if
!           one is not specified.
!     \item [{[iospec]}]
!           I/O specification.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!
!     \end{description}
!
!
! !REQUIREMENTS:  
!EOP

      type(ESMF_BundleType), pointer :: btypep   ! Pointer to new bundle
      integer :: status                          ! Error status
      logical :: rcpresent                       ! Return code present

!     Initialize pointers
      status = ESMF_FAILURE
      rcpresent = .FALSE. 
      nullify(btypep)
      nullify(ESMF_BundleCreateNoFields%btypep)

!     Initialize return code
      if(present(rc)) then
        rcpresent=.TRUE.
        rc = ESMF_FAILURE
      endif

      allocate(btypep, stat=status)
!     If error write message and return.
!     Formal error handling will be added asap.
      if(status .NE. 0) then
        print *, "ERROR in ESMF_BundleCreateNew: Allocate"
        return
      endif

!     Call construction method to allocate and initialize bundle internals.
      call ESMF_BundleConstructNoFields(btypep, name, iospec, rc)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_BundleCreateNoFields: Bundle construct"
        return
      endif

!     Set return values.
      ESMF_BundleCreateNoFields%btypep => btypep
      if(rcpresent) rc = ESMF_SUCCESS



      end function ESMF_BundleCreateNoFields


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_BundleDestroy - Free all resources associated with a Bundle.
!
! !INTERFACE:
      subroutine ESMF_BundleDestroy(bundle, rc)
!
! !ARGUMENTS:
      type(ESMF_Bundle) :: bundle                        ! bundle to get rid of
      integer, intent(out), optional :: rc               ! return code
!
! !DESCRIPTION:
!     Releases all resources associated with the {\tt Bundle}.
!
!     \begin{description}
!     \item [bundle]
!           A {\tt Bundle} object.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!
!     \end{description}
!
!
!
! !REQUIREMENTS:  FLD2.4
!EOP

      ! Local variables
      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present
      type(ESMF_BundleType), pointer :: btype

      ! Initialize return code
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_FAILURE
      endif    

      ! If already destroyed or never created, return ok
      btype => bundle%btypep
      if (.not. associated(btype)) then
        if(rcpresent) rc = ESMF_FAILURE   ! should this really be an error?
        return
      endif

      ! Destruct all bundle internals and then free field memory.
      call ESMF_BundleDestruct(btype, status)
      ! If error write message and return.
      ! Formal error handling will be added asap.
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_BundleDestroy from ESMF_BundleDestruct"
        return
      endif

      deallocate(bundle%btypep, stat=status)
      ! If error write message and return.
      ! Formal error handling will be added asap.
      if(status .NE. 0) then
        print *, "ERROR in ESMF_BundleDestroy: Deallocate of Bundle class"
        return
      endif
      nullify(bundle%btypep)

      if(rcpresent) rc = ESMF_SUCCESS


      end subroutine ESMF_BundleDestroy


!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_BundleConstructNew - Construct the internals of a Bundle
!
! !INTERFACE:
      subroutine ESMF_BundleConstructNew(btype, fieldcount, fields, &
                                              packflag, name, iospec, rc)
!
! !ARGUMENTS:
      type(ESMF_BundleType), pointer :: btype 
      integer, intent(in) :: fieldcount           
      type(ESMF_Field), dimension (:) :: fields
      type(ESMF_PackFlag), intent(in), optional :: packflag 
      character (len = *), intent(in), optional :: name 
      type(ESMF_IOSpec), intent(in), optional :: iospec
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Constructs a {\tt Bundle} from a list of existing
!     gridded {\tt Fields}.  This routine requires an existing
!     {\tt ESMF\_Bundle} type as an input and fills in
!     the internals.  {\tt ESMF\_BundleCreateNew()} does
!     the allocation of an {\tt ESMF\_Bundle} type first and then
!     calls this routine.
!
!     The arguments are:
!     \begin{description}
!     \item [btype]
!           Pointer to a {\tt Bundle} object.
!     \item [fieldcount]
!           Number of fields to be added to the {\tt Bundle}.
!           Must be equal to or less than the number of
!           {\tt Field}s in the following argument.
!     \item [fields]
!           Array of existing {\tt Field}s.  The first {\tt fieldcount}
!           items will be added to the {\tt Bundle}.
!     \item [{[packflag]}]
!           If set to {\tt ESMF\_PACK\_FIELD\_DATA}, the {\tt Field}
!           data in individual {\tt Array}s will be collected
!           into a single data {\tt Array} for the entire {\tt Bundle}.
!           The default is {\tt ESMF\_NO\_PACKED\_DATA}.
!     \item [{[name]}]
!           {\tt Bundle} name.  A default name will be generated if
!           one is not specified.
!     \item [{[iospec]}]
!           I/O specification.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!
!     \end{description}
!
!EOPI
      
      integer :: i                                ! temp var
      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present

!     Initialize return code.  Assume failure until success assured.
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_FAILURE
      endif

!     Initialize the derived type contents.
      call ESMF_BundleConstructNoFields(btype, name, iospec, status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_BundleConstructNew: Bundle construct"
        return
      endif

!     If specified, set packflag
      if(present(packflag)) btype%pack_flag = packflag

!     Add the fields in the list, checking for consistency.
      call ESMF_BundleTypeAddFieldList(btype, fieldcount, fields, status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_BundleConstructNew: Bundle construct"
        return
      endif

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_BundleConstructNew


!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_BundleConstructNoFields - Construct the internals of a Bundle
!
! !INTERFACE:
      subroutine ESMF_BundleConstructNoFields(btype, name, iospec, rc)
!
! !ARGUMENTS:
      type(ESMF_BundleType), pointer :: btype 
      character (len = *), intent(in), optional :: name 
      type(ESMF_IOSpec), intent(in), optional :: iospec
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Constructs the internals of a {\tt Bundle}, given an existing
!     {\tt ESMF\_Bundle} type as an input.
!
!     The arguments are:
!     \begin{description}
!     \item [btype]
!           An existing {\tt Bundle} to be initialized.
!     \item [{[name]}]
!           {\tt Bundle} name.  A default name will be generated if
!           one is not specified.
!     \item [{[iospec]}]
!           I/O specification.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!
!     \end{description}
!
!EOPI
      
      integer :: status                            ! Error status
      logical :: rcpresent                         ! Return code present
      character (len = ESMF_MAXSTR) :: defaultname ! Bundle name if not given

      ! Initialize return code.  Assume failure until success assured.
      status = ESMF_FAILURE
      rcpresent =.FALSE.
      if(present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_FAILURE
      endif


      ! Set the Bundle name.  Construct a default name if one is not given.
      call ESMF_SetName(btype%base, name, "Bundles", status)

      if(status .NE. 0) then
        print *, "ERROR in ESMF_BundleConstructNoFields: SetName"
        return
      endif


      ! Initialize bundle contents
      
      btype%localbundle%gridstatus = ESMF_STATE_UNINIT
      btype%localbundle%arraystatus = ESMF_STATE_UNINIT
      btype%gridstatus = ESMF_STATE_UNINIT
   
      btype%field_count = 0
      nullify(btype%flist)
      
      btype%pack_flag = ESMF_NO_PACKED_DATA
!     nullify(btype%localbundle%packed_data)
      btype%bundlestatus = ESMF_STATE_READY
  

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_BundleConstructNoFields


!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_BundleDestruct - Free contents of a Bundle 
!
! !INTERFACE:
      subroutine ESMF_BundleDestruct(btype, rc)
!
! !ARGUMENTS:
      type(ESMF_BundleType), pointer :: btype 
      integer, intent(out), optional :: rc             ! return code

!
! !DESCRIPTION:
!     Releases all resources except the {\tt Bundle} itself.
!
!     \begin{description}
!     \item [btype]
!           Pointer to a {\tt Bundle} object.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!
!     \end{description}
!
!EOPI

      logical :: rcpresent                        ! Return code present

      ! Initialize return code; assume failure until success is certain
      rcpresent = .FALSE.
      if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
      endif

     btype%bundlestatus = ESMF_STATE_INVALID

     !
     ! TODO: code goes here
     !


     if (rcpresent) rc = ESMF_SUCCESS


     end subroutine ESMF_BundleDestruct


!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!
! This section includes the Field related routines.
!
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_BundleGetFieldByName - Retrieve a Field by name
!
! !INTERFACE:
      subroutine ESMF_BundleGetFieldByName(bundle, name, field, rc)
!
! !ARGUMENTS:
      type(ESMF_Bundle), intent(in) :: bundle            ! bundle to operate on
      character (len = *), intent(in) :: name            ! field name to get
      type(ESMF_Field), intent(out) :: field             ! returned field
      integer, intent(out), optional :: rc               ! return code
!
! !DESCRIPTION:
!      Return a {\tt Field} from a {\tt Bundle} by name.
!
!     The arguments are:
!     \begin{description}
!     \item [bundle]
!           {\tt Bundle} to query for {\tt Field}.
!     \item [name]
!           {\tt Field} name.
!     \item [field]
!           Returned {\tt Field}.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!
!     \end{description}

!
! !REQUIREMENTS:  FLD2.5.6
!EOP

      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present
      integer :: i                                ! temp var
      logical :: found                            ! did we find a match?
      character (len=ESMF_MAXSTR) :: temp_name
      type(ESMF_Field) :: temp_field
      type(ESMF_BundleType), pointer :: btype

!     Initialize return code.  Assume failure until success assured.
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_FAILURE
      endif

      found = .FALSE.

      btype => bundle%btypep

!     Validate bundle before using it.
      if (.not. associated(btype)) then
        print *, "ERROR in ESMF_BundleGetFields: bad Bundle object"
        return
      endif
      if (btype%bundlestatus .ne. ESMF_STATE_READY) then
        print *, "ERROR in ESMF_BundleGetFields: bad Bundle object"
        return
      endif

!     Check for an empty Bundle first
      if(btype%field_count .eq. 0) then
        print *, "ERROR in ESMF_BundleGetFields: Empty Bundle"
        return
      endif

!     Check each field for a match
      do i = 1, btype%field_count
  
       call ESMF_FieldGetName(btype%flist(i), temp_name, status)
       if (status .eq. ESMF_FAILURE) then
         print *, "ERROR in ESMF_BundleGetFields: Error getting Field name from Field ", i
         return
       endif

       if (name .eq. temp_name) then
           field = bundle%btypep%flist(i) 
           found = .TRUE.
           ! found match, exit loop early
           exit
        endif
      enddo

      if (.not. found) then
        print *, "ERROR in ESMF_BundleGetFields: Field not found with name ", name
        return
      endif

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_BundleGetFieldByName

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_BundleGetFieldByNum - Retrieve a Field by index number
!
! !INTERFACE:
      subroutine ESMF_BundleGetFieldByNum(bundle, index, field, rc)
!
! !ARGUMENTS:
      type(ESMF_Bundle), intent(in) :: bundle            ! bundle to operate on
      integer, intent(in) :: index                       ! field index, first=1
      type(ESMF_Field), intent(out) :: field             ! returned field
      integer, intent(out), optional :: rc               ! return code
!
! !DESCRIPTION:
!      Return a {\tt Field} from a {\tt Bundle} by index number.
!
!     The arguments are:
!     \begin{description}
!     \item [bundle]
!           {\tt Bundle} to query for {\tt Field}.
!     \item [index]
!           {\tt Field} index number; first {\tt Field} index is 1.
!     \item [field]
!           Returned {\tt Field}.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!
!     \end{description}

!
! !REQUIREMENTS:  FLD2.5.6
!EOP

      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present
      logical :: found                            ! did we find a match?
      type(ESMF_BundleType), pointer :: btype

!     Initialize return code.  Assume failure until success assured.
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_FAILURE
      endif

      found = .FALSE.

      btype => bundle%btypep

!     Validate bundle before using it.
      if (.not. associated(btype)) then
        print *, "ERROR in ESMF_BundleGetFields: bad Bundle object"
        return
      endif
      if (btype%bundlestatus .ne. ESMF_STATE_READY) then
        print *, "ERROR in ESMF_BundleGetFields: bad Bundle object"
        return
      endif

!     Check for an empty Bundle first
      if(btype%field_count .eq. 0) then
        print *, "ERROR in ESMF_BundleGetFields: Empty Bundle"
        return
      endif

!     Check for out of range index number
      if ((index .lt. 1) .or. (index .gt. btype%field_count)) then
        print *, "ERROR in ESMF_BundleGetFields: Index ", index, &
                        "out of range. Min=1, max=", btype%field_count
        return
      endif

!     Fetch requested field
      field = bundle%btypep%flist(index) 

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_BundleGetFieldByNum


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_BundleGetAllFields - Retrieve an arrays of Fields 
!
! !INTERFACE:
      subroutine ESMF_BundleGetAllFields(bundle, fieldptrs, count, rc)
!
! !ARGUMENTS:
      type(ESMF_Bundle), intent(in) :: bundle          ! bundle to operate on
      type(ESMF_Field), pointer, dimension (:) :: fieldptrs ! field ptr array
      integer, intent(out), optional :: count          ! number of fields in bundle
      integer, intent(out), optional :: rc             ! return code
!
! !DESCRIPTION:
!      Return pointers to all fields in a bundle.
!
! !REQUIREMENTS:  FLD2.5.6
!EOP

!
!  TODO: code goes here
!
      end subroutine ESMF_BundleGetAllFields


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_BundleAddField - Add a Field to a Bundle.
!
! !INTERFACE:
      subroutine ESMF_BundleAddField(bundle, field, rc)
!
! !ARGUMENTS:
      type(ESMF_Bundle), intent(in) :: bundle            ! bundle to operate on
      type(ESMF_Field), intent(in) :: field              ! field to add
      integer, intent(out), optional :: rc               ! return code
!
! !DESCRIPTION:
!      Add a single Field reference to an existing Bundle.  The Field must have the
!      same Grid as the rest of the Fields in the Bundle.   If the Bundle has
!      packed data, this will mean copying the data to add this field.
! 
! !REQUIREMENTS:  FLD2.5.2
!EOP
      type(ESMF_Field) :: temp_list(1)
      type(ESMF_BundleType), pointer :: btype

      temp_list(1) = field

      ! validate bundle before going further
      btype => bundle%btypep
      if (.not. associated(btype)) then
        print *, "ERROR in ESMF_BundleAddField: bad Bundle object"
        return
      endif
      if (btype%bundlestatus .ne. ESMF_STATE_READY) then
        print *, "ERROR in ESMF_BundleAddField: bad Bundle object"
        return
      endif
    
      call ESMF_BundleTypeAddFieldList(btype, 1, temp_list, rc)

      end subroutine ESMF_BundleAddField


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_BundleAddFieldList - Add a list of Fields to a Bundle.
!
! !INTERFACE:
      subroutine ESMF_BundleAddFieldList(bundle, fieldcount, fields, rc)
!
! !ARGUMENTS:
      type(ESMF_Bundle), intent(in) :: bundle        
      integer, intent(in) :: fieldcount
      type(ESMF_Field), dimension(:), intent(in) :: fields
      integer, intent(out), optional :: rc          
!
! !DESCRIPTION:
!      Add a Field reference to an existing Bundle.  The Field must have the
!      same Grid as the rest of the Fields in the Bundle.   If the Bundle has
!      packed data, this will mean copying the data to add this field.
!
!     The arguments are:
!     \begin{description}
!     \item [bundle]
!           {\tt Bundle} to add {\tt Field}s into.
!     \item [fieldcount]
!           Number of fields to be added to the {\tt Bundle}.
!           Must be equal to or less than the number of 
!           {\tt Field}s in the following argument.
!     \item [fields]
!           Array of existing {\tt Field}s.  The first {\tt fieldcount}
!           items will be added to the {\tt Bundle}.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!
!     \end{description}
! 
! !REQUIREMENTS:  FLD2.5.2
!EOP
      type(ESMF_BundleType), pointer :: btype

      ! validate bundle before going further
      btype => bundle%btypep
      if (.not. associated(btype)) then
        print *, "ERROR in ESMF_BundleAddField: bad Bundle object"
        return
      endif
      if (btype%bundlestatus .ne. ESMF_STATE_READY) then
        print *, "ERROR in ESMF_BundleAddField: bad Bundle object"
        return
      endif
    
      call ESMF_BundleTypeAddFieldList(btype, fieldcount, fields, rc)
      
      end subroutine ESMF_BundleAddFieldList


!------------------------------------------------------------------------------
!TODO: decide how to document this.  it's internal only.
! !!IBOP
! !IROUTINE: ESMF_BundleTypeAddFieldList - Add a list of Fields to a Bundle.
!
! !INTERFACE:
      subroutine ESMF_BundleTypeAddFieldList(btype, fieldcount, fields, rc)
!
! !ARGUMENTS:
      type(ESMF_BundleType), pointer :: btype        
      integer, intent(in) :: fieldcount
      type(ESMF_Field), dimension(:), intent(in) :: fields
      integer, intent(out), optional :: rc          
!
! !DESCRIPTION:
!      Add a Field reference to an existing Bundle.  The Field must have the
!      same Grid as the rest of the Fields in the Bundle.   If the Bundle has
!      packed data, this will mean copying the data to add this field.
!
!     The arguments are:
!     \begin{description}
!     \item [btype]
!           {\tt BundleType} to add {\tt Field}s into.
!     \item [fieldcount]
!           Number of fields to be added to the {\tt Bundle}.
!           Must be equal to or less than the number of 
!           {\tt Field}s in the following argument.
!     \item [fields]
!           Array of existing {\tt Field}s.  The first {\tt fieldcount}
!           items will be added to the {\tt Bundle}.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!
!     \end{description}
! 
! !!IEOP
      
      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present
      integer :: i                                ! temp var
      type(ESMF_Field), dimension(:), pointer :: temp_flist  
                                                  ! list of fields

      ! Initialize return code.  Assume failure until success assured.
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_FAILURE
      endif

      ! Initial values
      nullify(temp_flist)
    

! TODO: add consistency checks below
!       loop over field count, get grid and check to see it's the same

!     Add the fields in the list, checking for consistency.
      if(btype%field_count .eq. 0) then
          allocate(btype%flist(fieldcount), stat=status)
          if(status .NE. 0) then
            print *, "ERROR in ESMF_BundleAddFields: Fieldlist allocate"
            return
          endif
         
!         now add the fields to the new list
          do i=1, fieldcount
            btype%flist(i) = fields(i)
          enddo

          btype%field_count = fieldcount
      else
!         make a list the right length
          allocate(temp_flist(btype%field_count + fieldcount), stat=status)
          if(status .NE. 0) then
            print *, "ERROR in ESMF_BundleConstructNew: temporary Fieldlist allocate"
            return
          endif

!         preserve old contents
          do i = 1, btype%field_count
            temp_flist(i) = btype%flist(i)
          enddo

!         and append the new fields to the list
          do i=1, fieldcount
            temp_flist(btype%field_count+i) = fields(i)
          enddo

!         delete old list
          deallocate(btype%flist, stat=status)
          if(status .NE. 0) then
            print *, "ERROR in ESMF_BundleConstructNew: Fieldlist deallocate"
          endif

!         and now make this the permanent list
          btype%flist => temp_flist
          btype%field_count = btype%field_count + fieldcount

      endif

!     If packed data buffer requested, create or update it here.
      if (btype%pack_flag .eq. ESMF_PACK_FIELD_DATA) then

         call ESMF_BundleTypeRepackData(btype, rc=rc)

      endif

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_BundleTypeAddFieldList


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_BundleDeleteField - Remove a Field from a Bundle
!
! !INTERFACE:
      subroutine ESMF_BundleDeleteField(bundle, name, rc)
!
! !ARGUMENTS:
      type(ESMF_Bundle), intent(in) :: bundle            ! bundle to operate on
      character (len = *), intent(in) :: name            ! field name to delete
      integer, intent(out), optional :: rc               ! return code

!
! !DESCRIPTION:
!      Delete a Field reference from an existing Bundle.  If the Bundle 
!      has packed data this will mean copying the data to remove this field.
!
! !REQUIREMENTS:  FLD2.5.2
!EOP

!
!  TODO: code goes here
!
      print *, "ESMF_BundleDeleteField not implemented yet"

      end subroutine ESMF_BundleDeleteField



!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!
! This section includes all the interfaces which manipulate the
!  packed data.  The functions are implemented in the ESMF_Data code.
!
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_BundlePackData - Pack Field data into a single Array
!
! !INTERFACE:
      subroutine ESMF_BundlePackData(bundle, datamap, rc)
!
! !ARGUMENTS:
      type(ESMF_Bundle), intent(in) :: bundle
      type(ESMF_DataMap), intent(in), optional :: datamap 
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!      Packs the {\tt Field} data into a single {\tt Array}.  If new {\tt Field}s
!      are added to a {\tt Bundle} which already has Packed data, the data will
!      have to be copied into a new {\tt Array}.
!
!     The arguments are:
!     \begin{description}
!     \item [bundle]
!           Existing {\tt Bundle}.
!     \item [{[datamap]}]
!           Ordering and Interleaving information.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!
!     \end{description}
!
!
! !REQUIREMENTS:  FLD2.1.1, FLD2.2, FLD2.5.9
!EOP

      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present
      integer :: i                                ! temp var
      type(ESMF_Array) :: pkarray                 ! Array for packed data
      type(ESMF_BundleType), pointer :: btype     ! internal data

!     Initialize return code.  Assume failure until success assured.
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_FAILURE
      endif

      btype => bundle%btypep

!     Validate bundle before using it.
      if (.not. associated(btype)) then
        print *, "ERROR in ESMF_BundlePackGrid: bad Bundle object"
        return
      endif
      if (btype%bundlestatus .ne. ESMF_STATE_READY) then
        print *, "ERROR in ESMF_BundlePackGrid: bad Bundle object"
        return
      endif

!     pkarray = ESMF_ArrayCreate(arrayspec, status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_BundlePackData: packed Array create"
        return
      endif

      btype%pack_flag = ESMF_PACK_FIELD_DATA
!     btype%localbundle%packed_data = pkarray

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_BundlePackData


!------------------------------------------------------------------------------
! !!IBOP
! !IROUTINE: ESMF_BundleTypeRepackData - Pack Field data into a single Array
!
! !INTERFACE:
      subroutine ESMF_BundleTypeRepackData(btype, datamap, rc)
!
! !ARGUMENTS:
      type(ESMF_BundleType), pointer :: btype
      type(ESMF_DataMap), intent(in), optional :: datamap 
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!      Packs the {\tt Field} data into a single {\tt Array}.  If new {\tt Field}s
!      are added to a {\tt Bundle} which already has Packed data, the data will
!      have to be copied into a new {\tt Array}.
!
!     The arguments are:
!     \begin{description}
!     \item [btype]
!           {\tt BundleType} pointer.
!     \item [{[datamap]}]
!           Ordering and Interleaving information.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!
!     \end{description}
!
!
! !REQUIREMENTS:  FLD2.1.1, FLD2.2, FLD2.5.9
! !!IEOP

      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present
      integer :: i                                ! temp var
      type(ESMF_Array) :: pkarray                 ! Array for packed data

!     Initialize return code.  Assume failure until success assured.
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_FAILURE
      endif

!     pkarray = ESMF_ArrayCreate(arrayspec, status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_BundlePackData: packed Array create"
        return
      endif

      btype%pack_flag = ESMF_PACK_FIELD_DATA
!     btype%localbundle%packed_data = pkarray

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_BundleTypeRepackData


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_BundleReorder - Alter memory interleave in Packed data
!
! !INTERFACE:
      subroutine ESMF_BundleReorder(bundle, datamap, rc)
!
! !ARGUMENTS:
      type(ESMF_Bundle), intent(in) :: bundle       ! bundle to operate on
      type(ESMF_DataMap), intent(in) :: datamap     ! the new interleave/order
      integer, intent(out), optional :: rc          ! return code

!
! !DESCRIPTION:
!      Used to alter memory ordering of packed Data array.  Implemented by 
!      setting the desired options in an DataMap type and then passing it in
!      as a parameter to this routine.
!
! !REQUIREMENTS:  FLD2.2, FLD2.3
!EOP

!
!  TODO: code goes here
!
      end subroutine ESMF_BundleReorder


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_BundleSetDataValues - Set contents of Packed array
!
! !INTERFACE:
      subroutine ESMF_BundleSetDataValues(bundle, index, value, rc)
!
! !ARGUMENTS:
      type(ESMF_Bundle), intent(in) :: bundle            ! bundle to operate on
      integer, dimension (:), intent(in) :: index        ! index values to change
      real, dimension (:), intent(in) :: value           ! data value(s) to set
      integer, intent(out), optional :: rc               ! return code
!
! !DESCRIPTION:
!      Allows data values associated with a Bundle to be set through the
!      Bundle interface instead of detaching data and setting it in a loop.
!      Various restrictions on data types may be imposed.
! 
! !REQUIREMENTS:  FLD2.5.5
!EOP

!
!  TODO: code goes here
!
      end subroutine ESMF_BundleSetDataValues


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_BundleDataDetach - Obtain direct data access to Packed data
!
! !INTERFACE:
      subroutine ESMF_BundleDataDetach(bundle, databuffer, rc)
!
! !ARGUMENTS:
      type(ESMF_Bundle), intent(in) :: bundle            ! bundle to operate on
      real, dimension (:), pointer :: databuffer                    ! start of data array
      integer, intent(out), optional :: rc               ! return code
!
! !DESCRIPTION:
!      Returns a data buffer marked as having Read/Write access.  Will be
!      exclusive access from ESMF interfaces. (See comments in the corresponding
!      Field section for more details on access.)
!
! !REQUIREMENTS:  FLD2.5.3
!EOP

!
!  TODO: code goes here
!
      end subroutine ESMF_BundleDataDetach


!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      subroutine ESMF_BundleDataDetachRO(bundle, databuffer, rc)
!
! !ARGUMENTS:
      type(ESMF_Bundle), intent(in) :: bundle            ! bundle to operate on
      real, dimension (:), pointer :: databuffer                    ! start of data array
      integer, intent(out), optional :: rc               ! return code
!
! !DESCRIPTION:
!     Returns a data buffer marked as having ReadOnly access.  Multiple
!     requests for RO access will be accepted by the framework.
!
! !REQUIREMENTS:  FLD2.5.3
!EOP

!
!  TODO: code goes here
!
      end subroutine ESMF_BundleDataDetachRO


!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      subroutine ESMF_BundleDataDetachCopy(bundle, data, subarray, rc)
!
! !ARGUMENTS:
      type(ESMF_Bundle), intent(in) :: bundle            ! bundle to operate on
      type(ESMF_Array), intent(out) :: data              ! output data array
      type(ESMF_Array), pointer, optional :: subarray    ! intent(in), optional subsetting
      integer, intent(out), optional :: rc               ! return code
!
! !DESCRIPTION:
!      Returns a copy of the Bundles's data buffer.
!      The ESMF has allocated space for this copy; the caller can either
!      call the memory management to free the space, or call
!      the DropCopy routine and have the ESMF release the space.
!      The original bundle data is still attached and is left accessible.
!      If specified, the copy can be a subset of the original data.
!
! !REQUIREMENTS:  FLD2.5.4
!EOP

!
!  TODO: code goes here
!
      end subroutine ESMF_BundleDataDetachCopy


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_BundleDataAttach - Return Packed array to Bundle
!
! !INTERFACE:
      subroutine ESMF_BundleDataAttach(bundle, rc)
!
! !ARGUMENTS:
      type(ESMF_Bundle), intent(in) :: bundle            ! bundle to operate on
      integer, intent(out), optional :: rc               ! return code
!
! !DESCRIPTION:
!      Returns responsibility for R/W data back to ESMF.
!
! !REQUIREMENTS:  FLD2.5.3
!EOP

!
!  TODO: code goes here
!
      end subroutine ESMF_BundleDataAttach

!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      subroutine ESMF_BundleDataDrop(bundle, rc)
!
! !ARGUMENTS:
      type(ESMF_Bundle), intent(in) :: bundle            ! bundle to operate on
      integer, intent(out), optional :: rc               ! return code
!
! !DESCRIPTION:
!      Returns responsibility for ReadOnly data back to ESMF.
!
! !REQUIREMENTS:  FLD2.5.3
!EOP

!
!  TODO: code goes here
!
      end subroutine ESMF_BundleDataDrop

!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      subroutine ESMF_BundleDataDropCopy(databuffer, rc)
!
! !ARGUMENTS:
      real, dimension (:), pointer :: databuffer                    ! data space to return
      integer, intent(out), optional :: rc               ! return code
!
! !DESCRIPTION:
!      Convenience routine for freeing memory associated with data copy.
!      Note this routine does not need the Bundle as an input.
!
! !REQUIREMENTS:  FLD2.5.4
!EOP

!
!  TODO: code goes here
!
      end subroutine ESMF_BundleDataDropCopy


!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!
! This section includes all the interfaces which deal with the Grid
!  which is common to all Fields in the bundle.  These are interfaces
!  to code which is actually implemented in the Grid class.
!
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_BundleGetGrid - Return the Grid associated with this Bundle
!
! !INTERFACE:
      subroutine ESMF_BundleGetGrid(bundle, grid, rc)
!
! !ARGUMENTS:
      type(ESMF_Bundle), intent(in) :: bundle
      type(ESMF_Grid), intent(out) :: grid
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!      Returns the Grid associated with this Bundle.
!
!     \begin{description}
!     \item [bundle]
!           A {\tt Bundle} object.
!     \item [grid]
!           The {\tt Grid} associated with all {\tt Field}s in this 
!           {\tt Bundle}.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!
!     \end{description}
!
!
! !REQUIREMENTS: FLD2.5.7
!EOP

      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present
      type(ESMF_BundleType), pointer :: btype     ! internal data

!     Initialize return code; assume failure until success is certain
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
      endif

!     Validate bundle before using it.
      btype => bundle%btypep
      if (.not. associated(btype)) then
        print *, "ERROR in ESMF_BundleGetGrid: bad Bundle object"
        return
      endif
      if (btype%bundlestatus .ne. ESMF_STATE_READY) then
        print *, "ERROR in ESMF_BundleGetGrid: bad Bundle object"
        return
      endif
      if (btype%gridstatus .ne. ESMF_STATE_READY) then
        print *, "ERROR in ESMF_BundleGetGrid: no associated Grid"
        return
      endif

!     OK to return grid
      grid = btype%grid

      if (rcpresent) rc = ESMF_SUCCESS


      end subroutine ESMF_BundleGetGrid

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_BundleGetGridDimCount - Get dimensionality of Grid
!
! !INTERFACE:
      subroutine ESMF_BundleGetGridDimCount(bundle, dimcount, rc)
!
! !ARGUMENTS:
      type(ESMF_Bundle), intent(in) :: bundle              ! query grid from this bundle
      integer, intent(out) :: dimcount                     ! number of dimensions
      integer, intent(out), optional :: rc                 ! return code
!
! !DESCRIPTION:
!      Find out how many dimensions are in the Grid associated with this Bundle.
!
! !REQUIREMENTS: (none. added for completeness)
!EOP

!
!  TODO: code goes here
!
        end subroutine ESMF_BundleGetGridDimCount



!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      subroutine ESMF_BundleGetGridDimSize(bundle, locallist, globallist, rc)
!
! !ARGUMENTS:
      type(ESMF_Bundle), intent(in) :: bundle     ! query grid from this bundle
      integer, intent(out), optional :: locallist(:)  ! size of each dimension
      integer, intent(out), optional :: globallist(:) ! size of each dimension
      integer, intent(out), optional :: rc            ! return code
!
! !DESCRIPTION:
!      For both the local decomposition and the global grid, 
!      return the number of items in each dimension.
!
! !REQUIREMENTS: (none. added for completeness)
!EOP

!
!  TODO: code goes here
!
        end subroutine ESMF_BundleGetGridDimSize


!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      subroutine ESMF_BundleGetIndexOrder(bundle, indexorder, rc)
!
! !ARGUMENTS:
      type(ESMF_Bundle), intent(in) :: bundle         ! bundle to be queried
      integer, dimension(:), intent(out) :: indexorder ! description of ordering
      integer, intent(out), optional :: rc            ! return code
!
! !DESCRIPTION:
!      Return in what order the indicies of the Grid is specified.
!
! !REQUIREMENTS: (none. added for completeness)
!EOP

!
!  TODO: code goes here
!
        end subroutine ESMF_BundleGetIndexOrder


!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      subroutine ESMF_BundleGridGetCellCount(bundle, localcount, globalcount, rc)
!
! !ARGUMENTS:
      type(ESMF_Bundle), intent(in) :: bundle     ! query grid from this bundle
      integer, intent(out), optional :: localcount(:)   ! local cell count
      integer, intent(out), optional :: globalcount(:)  ! global cell count
      integer, intent(out), optional :: rc              ! return code
!
! !DESCRIPTION:
!      For both the local decomposition and the global grid, return the 
!       number of items in each.
!
! !REQUIREMENTS: FLD2.6.4
!EOP

!
!  TODO: code goes here
!
        end subroutine ESMF_BundleGridGetCellCount


!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      subroutine ESMF_BundleGetGridPointCount(bundle, localcount, globalcount, rc)
!
! !ARGUMENTS:
      type(ESMF_Bundle), intent(in) :: bundle     ! query grid from this bundle

      integer, intent(out), optional :: localcount(:) ! local point/vertex count
      integer, intent(out), optional :: globalcount(:)! global point/vertex count
      integer, intent(out), optional :: rc            ! return code
!
! !DESCRIPTION:
!      For both the local decomposition and the global grid, return the 
!      number of items in each.
!
! !REQUIREMENTS: FLD2.6.4
!EOP

!
!  TODO: code goes here
!
        end subroutine ESMF_BundleGetGridPointCount




!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!
! This section contains the Get routines for finding out information
!  about characteristics of a Bundle.
!
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_BundleGetName - Return the name of the Bundle
!
! !INTERFACE:
      subroutine ESMF_BundleGetName(bundle, name, rc)
!
! !ARGUMENTS:
      type(ESMF_Bundle), intent(in) :: bundle            ! bundle to query
      character (len = *), intent(out) :: name           ! name of this bundle
      integer, intent(out), optional :: rc               ! return code
!
! !DESCRIPTION:
!      Returns the name of the Bundle.  If the Bundle was originally created
!      without specifying a name, a unique name will have been generated
!      by the framework.
!
!     \begin{description}
!     \item [bundle]
!           The {\tt Bundle} object to query.
!     \item [name]
!           A character string where the {\tt Bundle} name is returned.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!
!     \end{description}
!
!
! !REQUIREMENTS:  FLD2.6.1 (pri 2)
!EOP

      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present
      type(ESMF_BundleType), pointer :: btype     ! internal data

!     Initialize return code; assume failure until success is certain
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
      endif

! TODO: make this a common subroutine; all entry points need to do this

!     Validate bundle before using it.
      btype => bundle%btypep
      if (.not. associated(btype)) then
        print *, "ERROR in ESMF_BundleGetName: bad Bundle object"
        return
      endif
      if (btype%bundlestatus .ne. ESMF_STATE_READY) then
        print *, "ERROR in ESMF_BundleGetName: bad Bundle object"
        return
      endif

!     OK to query for name
      call ESMF_GetName(btype%base, name, status)
      if(status .NE. 0) then
        print *, "ERROR in ESMF_BundleGetName"
        return
      endif

      if (rcpresent) rc = ESMF_SUCCESS


      end subroutine ESMF_BundleGetName


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_BundleGetDataMap - Get current Packed array ordering
!
! !INTERFACE:
      subroutine ESMF_BundleGetDataMap(bundle, datamap, rc)
!
! !ARGUMENTS:
      type(ESMF_Bundle), intent(in) :: bundle     ! bundle to operate on
      type(ESMF_DataMap), intent(out) :: datamap  ! the current order/interleaf
      integer, intent(out), optional :: rc        ! return code
!
! !DESCRIPTION:
!      For querying current ordering of packed DataArray type.
!      Information is returned in the DataMap type, and then can
!      be queried by DataMap subroutines for details.
!
! !REQUIREMENTS:  FLD2.5.8 (pri 2)
!EOP

!
!  TODO: code goes here
!
      end subroutine ESMF_BundleGetDataMap


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_BundleGetFieldCount - Return a count of the Fields in a Bundle
!
! !INTERFACE:
      subroutine ESMF_BundleGetFieldCount(bundle, count, rc)
!
! !ARGUMENTS:
      type(ESMF_Bundle), intent(in) :: bundle            ! bundle to query
      integer, intent(out) :: count                      ! count of fields
      integer, intent(out), optional :: rc               ! return code
!
! !DESCRIPTION:
!      Returns the count of {\tt Fields} in a {\tt Bundle}.
!
!     The arguments are:
!     \begin{description}
!     \item [bundle]
!           {\tt Bundle} to query.
!     \item [count]
!           Returned {\tt Field} count.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!
!     \end{description}
! 
! !REQUIREMENTS:  FLD2.5.6, FLD2.6.3
!EOP


      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present
      type(ESMF_BundleType), pointer :: btype     ! internal data

!     Initialize return code
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_FAILURE
      endif    

!     Validate bundle before using it.
      btype => bundle%btypep
      if (.not. associated(btype)) then
        print *, "ERROR in ESMF_BundleGetFieldCount: bad Bundle object"
        return
      endif
      if (btype%bundlestatus .ne. ESMF_STATE_READY) then
        print *, "ERROR in ESMF_BundleGetFieldCount: bad Bundle object"
        return
      endif

!     Return Field count
      count = bundle%btypep%field_count

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_BundleGetFieldCount


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_BundleGetFieldNames - Return all Field names in a Bundle

! !INTERFACE:
      subroutine ESMF_BundleGetFieldNames(bundle, namelist, namecount, rc)
!
! !ARGUMENTS:
      type(ESMF_Bundle), intent(in) :: bundle 
      character (len = *), intent(out) :: namelist(:)
      integer, intent(out), optional :: namecount     
      integer, intent(out), optional :: rc     
!
! !DESCRIPTION:
!      Return an array of {\tt Field} names in a {\tt Bundle}.
!
!     \begin{description}
!     \item [bundle]
!           A {\tt Bundle} object.
!     \item [namelist]
!           An array of character strings where each {\tt Field} name
!           is returned. 
!     \item [{[namelist]}]
!           A count of how many {\tt Field} names were returned.  Same as
!           the number of {\tt Field}s in the {\tt Bundle}.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!
!     \end{description}
!
!
! !REQUIREMENTS:  FLD2.6.2
!EOP

!
!  TODO: code goes here
!
      end subroutine ESMF_BundleGetFieldNames



!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!
! This section is the interface to the IO routines for reading
!  and writing Bundles.
!
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_BundleCheckpoint - Save Bundle in the quickest manner possible
!
! !INTERFACE:
      subroutine ESMF_BundleCheckpoint(bundle, iospec, rc)
!
! !ARGUMENTS:
      type(ESMF_Bundle), intent(in) :: bundle              ! bundle to save
      type(ESMF_IOSpec), intent(in), optional :: iospec    ! file specs
      integer, intent(out), optional :: rc                 ! return code
!
! !DESCRIPTION:
!      Used to save all data to disk as quickly as possible.  
!      (see Read/Write for other format options).  Internally will use 
!      same I/O interfaces as Read/Write
!      but will default to the fastest options.
!
! !REQUIREMENTS:  FLD2.5.10
!EOP

!
!  TODO: code goes here
!
      end subroutine ESMF_BundleCheckpoint

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_BundleRestore - Read back in a Checkpointed Bundle
!
! !INTERFACE:
      function ESMF_BundleRestore(name, iospec, rc)
!
! !RETURN VALUE:
      type(ESMF_Bundle) :: ESMF_BundleRestore
!
! !ARGUMENTS:
      character (len = *), intent(in) :: name              ! bundle name to restore
      type(ESMF_IOSpec), intent(in), optional :: iospec    ! file specs
      integer, intent(out), optional :: rc                 ! return code
!
! !DESCRIPTION:
!      Used to reinitialize
!      all data associated with a Bundle from the last call to Checkpoint.
!
! !REQUIREMENTS:  FLD2.5.10
!EOP

!
!  TODO: code goes here
!
      type(ESMF_Bundle) :: b

      b%btypep%bundlestatus = ESMF_STATE_UNINIT

      ESMF_BundleRestore = b

      end function ESMF_BundleRestore

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_BundleWrite - Save a Bundle to an external destination
!
! !INTERFACE:
      subroutine ESMF_BundleWrite(bundle, subarray, iospec, rc)
!
! !ARGUMENTS:
      type(ESMF_Bundle), intent(in) :: bundle            ! bundle to save
      type(ESMF_Array), pointer, optional :: subarray    ! subset to write
      type(ESMF_IOSpec), intent(in), optional :: iospec  ! file specs
      integer, intent(out), optional :: rc               ! return code
!
! !DESCRIPTION:
!      Used to write data to persistent storage in a variety of formats.  
!      (see Checkpoint/Restore for quick data dumps.)
!
! !REQUIREMENTS:  FLD3.1, FLD3.2, FLD3.3, FLD3.4, FLD3.5
!EOP

!
!  TODO: code goes here
!
      end subroutine ESMF_BundleWrite


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_BundleRead - Create a Bundle from an external source
!
! !INTERFACE:
      function ESMF_BundleRead(name, iospec, rc)
!
! !RETURN VALUE:
      type(ESMF_Bundle) :: ESMF_BundleRead
!
! !ARGUMENTS:
      character (len = *), intent(in) :: name              ! bundle name to read
      type(ESMF_IOSpec), intent(in), optional :: iospec    ! file specs
      integer, intent(out), optional :: rc                 ! return code
!
! !DESCRIPTION:
!      Used to read data from persistent storage in a variety of formats.
!
! !REQUIREMENTS:  (which req number is this?)
!EOP

!
!  TODO: code goes here
!
      type(ESMF_Bundle) :: b

      b%btypep%bundlestatus = ESMF_STATE_UNINIT

      ESMF_BundleRead = b

      end function ESMF_BundleRead

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_BundleValidate - Perform internal consistency checks
!
! !INTERFACE:
      subroutine ESMF_BundleValidate(bundle, options, rc)
!
! !ARGUMENTS:
      type(ESMF_Bundle), intent(in) :: bundle            ! bundle to be checked
      character (len=*), intent(in), optional :: options ! validate options
      integer, intent(out), optional :: rc               ! return code
!
! !DESCRIPTION:
!      Validates that the Bundles is internally consistent.
!      Returns error code if problems are found.
!
! !REQUIREMENTS:  FLD4.1
!EOP

      ! Local variables
      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present

      ! Initialize return code; assume failure until success is certain
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
      endif

      if (.not.associated(bundle%btypep)) then 
          print *, "Uninitialized or Destroyed Bundle"
          return
      endif 

      if (bundle%btypep%bundlestatus .ne. ESMF_STATE_READY) then
          print *, "Uninitialized or Destroyed Bundle"
          return
      endif 

      ! TODO: add more code here

      if (rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_BundleValidate


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_BundlePrint - Print diagnostic information about a Bundle
!
! !INTERFACE:
      subroutine ESMF_BundlePrint(bundle, options, rc)
!
! !ARGUMENTS:
      type(ESMF_Bundle), intent(in) :: bundle            ! bundle to be printed
      character (len=*), intent(in) :: options           ! select print options
      integer, intent(out), optional :: rc               ! return code
!
! !DESCRIPTION:
!      Print information about a Bundle.  The options control the
!      type of information and level of detail.
!
! !REQUIREMENTS:  
!EOP

!
!  TODO: code goes here
!
      character(len=ESMF_MAXSTR) :: bname, fname
      type(ESMF_BundleType), pointer :: btypep
      type(ESMF_Field) :: field
      integer :: i
      integer :: status

      print *, "Bundle print:"

      btypep => bundle%btypep
      call ESMF_GetName(btypep%base, bname, status)
      print *, "  Bundle name = ", trim(bname)
    
      print *, "  Field count = ", btypep%field_count
    
      do i = 1, btypep%field_count
  
       call ESMF_FieldGetName(btypep%flist(i), fname, status)
       if (status .eq. ESMF_FAILURE) then
         print *, "ERROR in ESMF_BundlePrint: Error getting Field name from Field ", i
         return
       endif

       print *, "    Field", i, "name = ", trim(fname)
      enddo

      ! TODO: add more code here for printing more info

      end subroutine ESMF_BundlePrint


      end module ESMF_BundleMod

