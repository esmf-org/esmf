! $Id: ESMF_Field.F90,v 1.11 2003/04/08 23:06:07 nscollins Exp $
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
!     ESMF Field module
      module ESMF_FieldMod
!
!==============================================================================
!
! This file contains the Field class definition and all Field
! class methods.
!
!------------------------------------------------------------------------------
! INCLUDES
#include "ESMF_Macros.inc"
!------------------------------------------------------------------------------
!
!BOP
! !MODULE: ESMF_FieldMod - Combine physical field metadata, data and grid
!
! !DESCRIPTION:
! The code in this file implements the {\tt Field} class, which represents a
! single scalar or vector field.  {\tt Field}s associate a metadata description 
! expressed as a set of {\tt Attributes} with a data {\tt Array}, {\tt Grid}, 
! and I/O specification, or {\tt IOSpec}.  A {\tt DataMap} describes the 
! relationship of the {\tt Array} to the {\tt Grid}.  
!
! This type is implemented in Fortran 90 and a corresponding
! C++ interface is provided for access.
!
!------------------------------------------------------------------------------
! !USES:
      use ESMF_BaseMod
      use ESMF_IOMod
      use ESMF_GridMod
      use ESMF_ArrayMod
      use ESMF_DELayoutMod
      use ESMF_XPacketMod
      use ESMF_RouteMod
      use ESMF_DataMapMod
      implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
       private
!------------------------------------------------------------------------------
!     ! ESMF_Access
!
!     ! Internal flag for tracking whether data is attached. 

      type ESMF_Access
      sequence
      private
        integer :: a_type
      end type

      type(ESMF_Access), parameter ::  &
                               ESMF_READWRITE = ESMF_Access(0), &
                               ESMF_READONLY = ESMF_Access(1)

!------------------------------------------------------------------------------
!     ! ESMF_DataAllocate
!
!     ! Interface flag for setting whether Field does the data allocation.

      type ESMF_DataAllocate
      sequence
      private
        integer :: a_type
      end type

      type(ESMF_DataAllocate), parameter ::  &
                               ESMF_DO_ALLOCATE = ESMF_DataAllocate(0), &
                               ESMF_NO_ALLOCATE = ESMF_DataAllocate(1)

!------------------------------------------------------------------------------
!     ! ESMF_Mask
!  
!     ! Class for storing information about masked regions.

      type ESMF_Mask
      sequence
      private
        type (ESMF_Array), pointer :: maskvals   ! same size as data array
      end type

!------------------------------------------------------------------------------
!     ! ESMF_LocalField
!      
!     ! The LocalField class contains information which is associated with the
!     ! local DE.

      type ESMF_LocalField
      sequence
      private
   
        type (ESMF_Array) :: localdata           ! local data for this DE
        type (ESMF_Status) :: gridstatus         ! is grid set yet?
        type (ESMF_Status) :: datastatus         ! is data set yet?
        type (ESMF_Mask) :: mask                 ! may belong in Grid
        integer :: rwaccess                      ! reserved for future use
        integer :: accesscount                   ! reserved for future use

      end type

!------------------------------------------------------------------------------
!     ! ESMF_FieldType
      
!     ! Definition of the Field class.  A Field
!     ! is passed back to the user at Field creation.

      type ESMF_FieldType
      sequence
      private
       
        type (ESMF_Base) :: base                 ! base class object
        type (ESMF_Status) :: fieldstatus        ! uninitialized, init ok, etc
        type (ESMF_Grid) :: grid                 ! save to satisfy query routines
        type (ESMF_GridType), pointer :: gridp   ! pointer directly to grid
        type (ESMF_Status) :: gridstatus         ! uninit, grid ok, etc
        type (ESMF_LocalField) :: localfield     ! this differs per DE
        type (ESMF_DataMap) :: mapping           ! mapping of array indicies to grid
        type (ESMF_IOSpec) :: iospec             ! iospec values
        type (ESMF_Status) :: iostatus           ! if unset, inherit from gcomp

      end type

!------------------------------------------------------------------------------
!     ! ESMF_Field
      
!     ! The Field data structure that is passed between implementation and
!     ! calling languages.

      type ESMF_Field
      sequence
      private       
        type (ESMF_FieldType), pointer :: ftypep    ! pointer to a field type
      end type

!------------------------------------------------------------------------------
! !PUBLIC TYPES:
      public ESMF_Field, ESMF_Mask, ESMF_Access
      public ESMF_FieldType            ! intended for internal lib use only
      public ESMF_DataAllocate, ESMF_NO_ALLOCATE, ESMF_DO_ALLOCATE

!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:
!
   public ESMF_FieldCreate             ! Create a new Field with data

   public ESMF_FieldCreateNoData       ! Create a new Field without data
   public ESMF_FieldDestroy            ! Destroy a Field

   public ESMF_FieldAttachData         ! Associate data with a Field - 
                                       !   reference (default) or copy 
   public ESMF_FieldDetachData         ! Dissociate data from a Field and 
                                       !   return its pointer

   public ESMF_FieldGetName            ! Get Field name
   public ESMF_FieldGetConfig          ! e.g., has associated Grid or data
 
   public ESMF_FieldGetGrid            ! Return a Grid pointer
   public ESMF_FieldGetGlobalGridInfo  ! Return global Grid info
   public ESMF_FieldGetLocalGridInfo   ! Return local Grid info

   public ESMF_FieldGetData            ! Return a data pointer
   public ESMF_FieldGetGlobalDataInfo  ! Return global data info
   public ESMF_FieldGetLocalDataInfo   ! Return local data info
 
   public ESMF_FieldGetDataMap         ! Return a pointer to DataMap object

   public ESMF_FieldSetGrid            ! Set a Grid (may regrid if different
                                       !   Grid is already present)
   public ESMF_FieldSetDataValues      ! Set Field data values 
   public ESMF_FieldSetDataMap         ! Set a DataMap (may reorder if different
                                       !   DataMap is already present)

                                       ! These are mainly entry points:
   public ESMF_FieldReduce             ! Global reduction operations
   !public ESMF_FieldTranspose         ! Transpose operation
   public ESMF_FieldHalo               ! Halo updates
   public ESMF_FieldRegrid             ! Regridding and interpolation
   public ESMF_FieldRoute              ! Redistribute existing array data

   public ESMF_FieldSetAttribute       ! Set and Get Attributes
   public ESMF_FieldGetAttribute       !   interface to Base class

   public ESMF_FieldValidate           ! Check internal consistency
   public ESMF_FieldPrint              ! Print contents of a Field

!  !subroutine ESMF_FieldCheckpoint(field, iospec, rc)
!  !function ESMF_FieldRestore(name, iospec, rc)
!  !subroutine ESMF_FieldWrite(field, subset, iospec, rc)
!  !function ESMF_FieldRead(fname, gname, dnames, iospec, rc)
!
!
!EOP

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_Field.F90,v 1.11 2003/04/08 23:06:07 nscollins Exp $'

!==============================================================================
!
! INTERFACE BLOCKS
!
!==============================================================================
!BOP
! !IROUTINE: ESMF_FieldCreate - Create a new Field with data
!
! !INTERFACE:
      interface ESMF_FieldCreate 
   
! !PRIVATE MEMBER FUNCTIONS:
        module procedure ESMF_FieldCreateNew
        module procedure ESMF_FieldCreateFromArray
        module procedure ESMF_FieldCreateRemap

! !DESCRIPTION:
!     This interface provides an entry point for methods that create a complete
!     {\tt Field}.  These method all contain a Grid and Data.  The variations
!     allow the user to specify the data using either a Fortran array or 
!     an {\tt ESMF\_Array}.
!    
 
!EOP
      end interface
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_FieldCreateNoData - Create a new Field without data
!
! !INTERFACE:
      interface ESMF_FieldCreateNoData
   
! !PRIVATE MEMBER FUNCTIONS:
        module procedure ESMF_FieldCreateNoBuffer
        module procedure ESMF_FieldCreateNoArray
        module procedure ESMF_FieldCreateNoGridArray  

! !DESCRIPTION:
!     This interface provides an entry point for methods that create 
!     a {\tt Field} without allocating or referencing any associated data.
!     The variations allow a {\tt Grid} to be specified or not, and for
!     the data description to be specified or not.
 
!EOP
      end interface
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_FieldConstruct - Construct the internals of a new Field
!
! !INTERFACE:
      interface ESMF_FieldConstruct
   
! !PRIVATE MEMBER FUNCTIONS:
        module procedure ESMF_FieldConstructNew
        module procedure ESMF_FieldConstructNewArray

! !DESCRIPTION:
!     This interface provides an entry point for methods that construct a 
!     complete {\tt Field}.
 
!EOP
      end interface
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_FieldConstructNoData - Construct the internals of a new empty Field
!
! !INTERFACE:
      interface ESMF_FieldConstructNoData
   
! !PRIVATE MEMBER FUNCTIONS:
        module procedure ESMF_FieldConstructNoBuffer
        module procedure ESMF_FieldConstructNoArray
        module procedure ESMF_FieldConstructNoGridArray  

! !DESCRIPTION:
!     This interface provides an entry point for {\tt Field} construction 
!     methods that do not allocate or reference any associated data.
 
!EOP
      end interface
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_FieldAttachData - Associate data with a Field
!
! !INTERFACE:
      interface ESMF_FieldAttachData 
   
! !PRIVATE MEMBER FUNCTIONS:
        module procedure ESMF_FieldAttachBuffer
        module procedure ESMF_FieldAttachArray
        module procedure ESMF_FieldAttachGridArray

! !DESCRIPTION:
!     This interface provides a single entry point for methods that attach
!     data to a {\tt Field}.
 
!EOP
      end interface
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_FieldDetachData - Obtain direct data access from a Field
!
! !INTERFACE:
      interface ESMF_FieldDetachData 
   
! !PRIVATE MEMBER FUNCTIONS:
        module procedure ESMF_FieldDetachBuffer
        module procedure ESMF_FieldDetachArray

! !DESCRIPTION:
!     This interface provides a single entry point for methods that detach
!     data from a {\tt Field}.
 
!EOP
      end interface
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_FieldSetAttribute - Set a Field Attribute
!
! !INTERFACE:
      interface ESMF_FieldSetAttribute 
   
! !PRIVATE MEMBER FUNCTIONS:
        module procedure ESMF_FieldSetIntegerAttribute
        !module procedure ESMF_FieldSetRealAttribute
        !module procedure ESMF_FieldSetCharAttribute

! !DESCRIPTION:
!     This interface provides a single entry point for methods that attach
!     attributes to a {\tt Field}.
 
!EOP
      end interface
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_FieldGetAttribute - Get a Field Attribute
!
! !INTERFACE:
      interface ESMF_FieldGetAttribute 
   
! !PRIVATE MEMBER FUNCTIONS:
        module procedure ESMF_FieldGetIntegerAttribute
        !module procedure ESMF_FieldGetRealAttribute
        !module procedure ESMF_FieldGetCharAttribute

! !DESCRIPTION:
!     This interface provides a single entry point for methods that retrieve
!     attributes from a {\tt Field}.
 
!EOP
      end interface
!
!==============================================================================
!
      contains
!
!==============================================================================
!
! This section includes all the Field Create and Destroy methods.
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE:   ESMF_FieldCreateNew - Create a new Field

! !INTERFACE:
      function ESMF_FieldCreateNew(grid, arrayspec, allocflag, relloc, &
                                    datamap, name, iospec, rc)
!
! !RETURN VALUE:
      type(ESMF_Field) :: ESMF_FieldCreateNew
!
! !ARGUMENTS:
      type(ESMF_Grid) :: grid               
      type(ESMF_ArraySpec), intent(in) :: arrayspec     
      type(ESMF_DataAllocate), intent(in), optional :: allocflag
      type(ESMF_RelLoc), intent(in), optional :: relloc 
      type(ESMF_DataMap), intent(in), optional :: datamap          
      character (len=*), intent(in), optional :: name 
      type(ESMF_IOSpec), intent(in), optional :: iospec 
      integer, intent(out), optional :: rc              
!
! !DESCRIPTION:
!     Create a {\tt Field} and allocate space internally for a
!     gridded {\tt Array}.  Return a new {\tt Field}.
! 
!     The arguments are:
!     \begin{description}
!     \item [grid] 
!           Pointer to a {\tt Grid} object. 
!     \item [arrayspec]
!           Data specification. 
!     \item [{[allocflag]}]
!           Whether to allocate space for the array.  Default is
!           {\tt ESMF\_DO\_ALLOCATE}.  Other option is {\tt ESMF\_NO\_ALLOCATE}.
!     \item [{[relloc]}] 
!           Relative location of data per grid cell/vertex. 
!     \item [{[datamap]}]
!           Describes the mapping of data to the {\tt Grid}.
!     \item [{[name]}] 
!           {\tt Field} name. 
!     \item [{[iospec]}] 
!           I/O specification. 
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!           
!     \end{description}
!
! !REQUIREMENTS: FLD1.1.1, FLD1.5.1
!EOP

      type(ESMF_FieldType), pointer :: ftype      ! Pointer to new field
      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present

!     Initialize pointers
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      nullify(ftype)
      nullify(ESMF_FieldCreateNew%ftypep)

!     Initialize return code   
      if(present(rc)) then
        rcpresent=.TRUE.
        rc = ESMF_FAILURE
      endif

      allocate(ftype, stat=status)
!     If error write message and return.
!     Formal error handling will be added asap.
      if(status .NE. 0) then 
        print *, "ERROR in ESMF_FieldCreateNew: Allocate"
        return
      endif 

!     Call construction method to allocate and initialize field internals.
      call ESMF_FieldConstructNew(ftype, grid, arrayspec, allocflag, relloc, &
                                    datamap, name, iospec, status)
      if(status .NE. ESMF_SUCCESS) then 
        print *, "ERROR in ESMF_FieldCreateNew: Field construct"
        return
      endif 
   
!     Set return values.
      ESMF_FieldCreateNew%ftypep => ftype
      if(rcpresent) rc = ESMF_SUCCESS

      end function ESMF_FieldCreateNew

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_FieldCreateFromArray - Create a Field from an existing ESMF Array

! !INTERFACE:
      function ESMF_FieldCreateFromArray(grid, array, copyflag, relloc, &
                                         datamap, name, iospec, rc)
!
! !RETURN VALUE:
      type(ESMF_Field) :: ESMF_FieldCreateFromArray    
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: grid                
      type(ESMF_Array), intent(in) :: array              
      type(ESMF_CopyFlag), intent(in), optional :: copyflag       
      type(ESMF_RelLoc), intent(in), optional :: relloc
      type(ESMF_DataMap), intent(in), optional :: datamap           
      character (len = *), intent(in), optional :: name   
      type(ESMF_IOSpec), intent(in), optional :: iospec   
      integer, intent(out), optional :: rc                
!
! !DESCRIPTION:
!     This version of creation assumes the data exists already and is being
!     passed in through an {\tt Array}.  
! 
!     \begin{description}
!     \item [grid] 
!           Pointer to a {\tt Grid} object. 
!     \item [array]
!           Includes data specification and allocated memory. 
!     \item [{[copyflag]}]
!           Indicates whether to reference the array or make a 
!           copy of it.  Valid values are {\tt ESMF\_DATA\_COPY} and 
!           {\tt ESMF\_DATA\_REF}, respectively.
!     \item [{[relloc]}] 
!           Relative location of data per grid cell/vertex. 
!     \item [{[datamap]}]
!           Describes the mapping of data to the {\tt Grid}.
!     \item [{[name]}] 
!           {\tt Field} name. 
!     \item [{[iospec]}] 
!           I/O specification. 
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!           
!     \end{description}
!
! !REQUIREMENTS: FLD1.1.2, FLD1.5.1
!EOP

      type(ESMF_FieldType), pointer :: ftype  ! Pointer to new field
      integer :: status                       ! Error status
      logical :: rcpresent                    ! Return code present
      
!     Initialize pointers
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      nullify(ftype)
      nullify(ESMF_FieldCreateFromArray%ftypep)

!     Initialize return code   
      if(present(rc)) then
        rcpresent = .TRUE. 
        rc = ESMF_FAILURE
      endif     

      allocate(ftype, stat=status)
!     If error write message and return.
!     Formal error handling will be added asap.
      if(status .NE. 0) then 
        print *, "ERROR in ESMF_FieldCreateFromArray: Allocate"
        return
      endif 

!     Call construction method to allocate and initialize field internals.
      call ESMF_FieldConstructNewArray(ftype, grid, array, relloc, &
                                       datamap, name, iospec, status)
      if(status .NE. ESMF_SUCCESS) then 
        print *, "ERROR in ESMF_FieldCreateNew: Field construct"
        return
      endif 
   

!     Set return values.
      ESMF_FieldCreateFromArray%ftypep => ftype
      if(rcpresent) rc = ESMF_SUCCESS

      end function ESMF_FieldCreateFromArray

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_FieldCreateNoBuffer - Create a Field with no associated data buffer

! !INTERFACE:
      function ESMF_FieldCreateNoBuffer(grid, arrayspec, relloc, &
                                        datamap, name, iospec, rc)
!
! !RETURN VALUE:
      type(ESMF_Field) :: ESMF_FieldCreateNoBuffer   
!
! !ARGUMENTS:
      type(ESMF_Grid) :: grid                 
      type(ESMF_ArraySpec), intent(in) :: arrayspec    
      type(ESMF_RelLoc), intent(in), optional :: relloc
      type(ESMF_DataMap), intent(in), optional :: datamap    
      character (len=*), intent(in), optional :: name    
      type(ESMF_IOSpec), intent(in), optional :: iospec  
      integer, intent(out), optional :: rc               
!
! !DESCRIPTION:
!     Creates a {\tt Field} in its entirety except for the assignment
!     or allocation of an associated raw data buffer.
!
!     \begin{description}
!     \item [grid] 
!           Pointer to a {\tt Grid} object. 
!     \item [arrayspec]
!           Data specification. 
!     \item [{[relloc]}] 
!           Relative location of data per grid cell/vertex. 
!     \item [{[datamap]}]
!           Describes the mapping of data to the {\tt Grid}.
!     \item [{[name]}] 
!           {\tt Field} name. 
!     \item [{[iospec]}] 
!           I/O specification. 
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!           
!     \end{description}
!
! !REQUIREMENTS: FLD1.1.3, FLD1.5.1
!EOP

      type(ESMF_FieldType), pointer :: ftype      ! Pointer to new field
      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present
   
!     Initialize pointers
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      nullify(ftype)
      nullify(ESMF_FieldCreateNoBuffer%ftypep)

!     Initialize return code   
      if(present(rc)) then
        rcpresent = .TRUE. 
        rc = ESMF_FAILURE
      endif     

      allocate(ftype, stat=status)
!     If error write message and return.
!     Formal error handling will be added asap.
      if(status .NE. 0) then 
        print *, "ERROR in FieldCreateNoBuffer: Allocate"
        return
      endif 

!     Call construction method to build field internals.
      call ESMF_FieldConstructNoBuffer(ftype, grid, arrayspec, relloc, &
                                       datamap, name, iospec, status) 
      if(status .NE. ESMF_SUCCESS) then 
        print *, "ERROR in FieldCreateNoBuffer: Field construct"
        return
      endif 

!     Set return values.
      ESMF_FieldCreateNoBuffer%ftypep => ftype
      if(rcpresent) rc = ESMF_SUCCESS

      end function ESMF_FieldCreateNoBuffer

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_FieldCreateNoArray - Create a Field with no associated Array object

! !INTERFACE:
      function ESMF_FieldCreateNoArray(grid, relloc, datamap, name, iospec, rc)
!
! !RETURN VALUE:
      type(ESMF_Field) :: ESMF_FieldCreateNoArray 
!
! !ARGUMENTS:
      type(ESMF_Grid) :: grid                 
      type(ESMF_RelLoc), intent(in), optional :: relloc
      type(ESMF_DataMap), intent(in), optional :: datamap              
      character (len=*), intent(in), optional :: name    
      type(ESMF_IOSpec), intent(in), optional :: iospec  
      integer, intent(out), optional :: rc               
!
! !DESCRIPTION:
!     This version of {\tt ESMF\_FieldCreate} builds a {\tt Field} 
!     and depends on a later call to add an {\tt Array} to it.  
!
!     \begin{description}
!     \item [grid] 
!           Pointer to a {\tt Grid} object. 
!     \item [{[relloc]}] 
!           Relative location of data per grid cell/vertex. 
!     \item [{[datamap]}]
!           Describes the mapping of data to the {\tt Grid}.
!     \item [{[name]}] 
!           {\tt Field} name. 
!     \item [{[iospec]}] 
!           I/O specification. 
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!           
!     \end{description}
!
! !REQUIREMENTS: FLD1.1.3, FLD1.5.1
!EOP

      type(ESMF_FieldType), pointer :: ftype  ! Pointer to new field
      integer :: status                       ! Error status
      logical :: rcpresent                    ! Return code present
      
!     Initialize pointers
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      nullify(ftype)
      nullify(ESMF_FieldCreateNoArray%ftypep)

!     Initialize return code   
      if(present(rc)) then
        rcpresent = .TRUE. 
        rc = ESMF_FAILURE
      endif     

      allocate(ftype, stat=status)
!     If error write message and return.
!     Formal error handling will be added asap.
      if(status .NE. 0) then 
        print *, "ERROR in FieldCreateNoArray: Allocate"
        return
      endif 

!     Call field construction method
      call ESMF_FieldConstructNoArray(ftype, grid, relloc, datamap, &
                                                 name, iospec, status)
      if(status .NE. 0) then 
        print *, "ERROR in ESMF_FieldCreateNoArray: Construct"
        return
      endif 

!     Set return values.
      ESMF_FieldCreateNoArray%ftypep => ftype
      if(rcpresent) rc = ESMF_SUCCESS

      end function ESMF_FieldCreateNoArray

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_FieldCreateNoGridArray - Create a Field with no Grid or Array

! !INTERFACE:
      function ESMF_FieldCreateNoGridArray(name, iospec, rc)
!
! !RETURN VALUE:
      type(ESMF_Field) :: ESMF_FieldCreateNoGridArray 
!
! !ARGUMENTS:
      character (len = *), intent(in), optional :: name  
      type(ESMF_IOSpec), intent(in), optional :: iospec  
      integer, intent(out), optional :: rc               
!
! !DESCRIPTION:
!     This version of {\tt ESMF\_FieldCreate} builds an empty {\tt Field} 
!     and depends on later calls to add a {\tt Grid} and {\tt Array} to 
!     it.  
!
!     \begin{description}
!     \item [{[name]}] 
!           {\tt Field} name. 
!     \item [{[iospec]}] 
!           I/O specification. 
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!           
!     \end{description}
!
! !REQUIREMENTS: FLD1.1.3, FLD1.5.1
!EOP

      type(ESMF_FieldType), pointer :: ftype  ! Pointer to new field
      integer :: status                       ! Error status
      logical :: rcpresent                    ! Return code present
      
!     Initialize pointers
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      nullify(ftype)
      nullify(ESMF_FieldCreateNoGridArray%ftypep)

!     Initialize return code   
      if(present(rc)) then
        rcpresent = .TRUE. 
        rc = ESMF_FAILURE
      endif     

      allocate(ftype, stat=status)
!     If error write message and return.
!     Formal error handling will be added asap.
      if(status .NE. 0) then 
        print *, "ERROR in ESMF_FieldCreateNoGridArray: Allocate"
        return
      endif 

!     Call field construction method
      call ESMF_FieldConstructNoGridArray(ftype, name, iospec, status)
      if(status .NE. 0) then 
        print *, "ERROR in ESMF_FieldCreateNoGridArray: Construct"
        return
      endif 

!     Set return values.
      ESMF_FieldCreateNoGridArray%ftypep => ftype
      if(rcpresent) rc = ESMF_SUCCESS

      end function ESMF_FieldCreateNoGridArray

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_FieldCreateRemap - Create a Field by remapping another Field

! !INTERFACE:
      function ESMF_FieldCreateRemap(srcfield, grid, datamap, name, &
                                     iospec, rc)
!
! !RETURN VALUE:
      type(ESMF_Field) :: ESMF_FieldCreateRemap
!
! !ARGUMENTS:
      type(ESMF_Field), intent(in) :: srcfield            
      type(ESMF_Grid), intent(in) :: grid                 
      type(ESMF_DataMap), intent(in), optional :: datamap              
      character (len = *), intent(in), optional :: name   
      type(ESMF_IOSpec), intent(in), optional :: iospec   
      integer, intent(out), optional :: rc                
!
! !DESCRIPTION:
!
!     Remaps data between an existing {\tt Grid} on a source {\tt Field}
!     and a new {\tt Grid}.  The {\tt Grid} is referenced by the 
!     new {\tt Field}.  Data is copied.
!
! !REQUIREMENTS: FLD1.1.5, FLD1.5.1, FLD1.6.1
!EOP

      type(ESMF_FieldType), pointer :: ftype  ! Pointer to new field
      integer :: status                       ! Error status
      logical :: rcpresent                    ! Return code present
      
!     Initialize pointers
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      nullify(ftype)
      nullify(ESMF_FieldCreateRemap%ftypep)

!     Initialize return code   
      if(present(rc)) then
        rcpresent = .TRUE. 
        rc = ESMF_FAILURE
      endif     

      allocate(ftype, stat=status)
!     If error write message and return.
!     Formal error handling will be added asap.
      if(status .NE. 0) then 
        print *, "ERROR in ESMF_FieldCreateRemap: Allocate"
        return
      endif 

!     TODO: Insert field construction method

!     Set return values.
      ESMF_FieldCreateRemap%ftypep => ftype
      if(rcpresent) rc = ESMF_SUCCESS

      end function ESMF_FieldCreateRemap

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_FieldDestroy - Free all resources associated with a Field

! !INTERFACE:
      subroutine ESMF_FieldDestroy(field, rc)
!
! !ARGUMENTS:
      type(ESMF_Field) :: field       
      integer, intent(out), optional :: rc     
!
! !DESCRIPTION:
!     Releases all resources associated with the {\tt Field}.
! 
!     \begin{description}
!     \item [field]
!           Pointer to a {\tt Field} object.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!           
!     \end{description}
!
! !REQUIREMENTS: FLD1.4
!EOP
      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present

!     Initialize return code   
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent = .TRUE. 
        rc = ESMF_FAILURE
      endif     

!     Destruct all field internals and then free field memory.
      call ESMF_FieldDestruct(field%ftypep, status)
!     If error write message and return.
!     Formal error handling will be added asap.
      if(status .NE. ESMF_SUCCESS) then 
        print *, "ERROR in ESMF_FieldDestroy from ESMF_FieldDestruct"
        return
      endif 
           
      deallocate(field%ftypep, stat=status)
!     If error write message and return.
!     Formal error handling will be added asap.
      if(status .NE. 0) then 
        print *, "ERROR in ESMF_FieldDestroy: Deallocate of Field class"
        return
      endif 
           
      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldDestroy

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!
! This section includes all the Field Construct and Destruct methods.
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_FieldConstructNew - Construct the internals of a Field

! !INTERFACE:
      subroutine ESMF_FieldConstructNew(ftype, grid, arrayspec, allocflag, &
                                         relloc, datamap, name, iospec, rc)
!
! !ARGUMENTS:
      type(ESMF_FieldType), pointer :: ftype 
      type(ESMF_Grid) :: grid               
      type(ESMF_ArraySpec), intent(in) :: arrayspec     
      type(ESMF_DataAllocate), intent(in), optional :: allocflag
      type(ESMF_RelLoc), intent(in), optional :: relloc
      type(ESMF_DataMap), intent(in), optional :: datamap           
      character (len=*), intent(in), optional :: name
      type(ESMF_IOSpec), intent(in), optional :: iospec 
      integer, intent(out), optional :: rc              
!
! !DESCRIPTION:
! 
!     Constructs all {\tt Field} internals, including the allocation
!     of a data {\tt Array}.  
!
!     \begin{description}
!     \item [ftype]
!           Pointer to a {\tt Field} object.
!     \item [grid] 
!           Pointer to a {\tt Grid} object. 
!     \item [arrayspec]
!           Data specification. 
!     \item [{[allocflag]}]
!           Set to allocate space for data array.  Default is
!           {\tt ESMF\_DO\_ALLOCATE}.  Other option is {\tt ESMF\_NO\_ALLOCATE}.
!     \item [{[relloc]}] 
!           Relative location of data per grid cell/vertex. 
!     \item [{[datamap]}]
!           Describes the mapping of data to the {\tt Grid}.
!     \item [{[name]}] 
!           {\tt Field} name. 
!     \item [{[iospec]}] 
!           I/O specification. 
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!           
!     \end{description}
!
! !REQUIREMENTS: FLD1.1.3, FLD1.5.1
!EOP

      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present
      type(ESMF_Array) :: array                   ! New array
      type(ESMF_AxisIndex), dimension(ESMF_MAXDIM) :: index
      integer, dimension(ESMF_MAXDIM) :: counts
      integer :: i, rank

!     Initialize return code   
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent = .TRUE. 
        rc = ESMF_FAILURE
      endif     

      call ESMF_FieldConstructNoArray(ftype, grid, relloc, datamap, & 
                                      name, iospec, status)
      if(status .NE. ESMF_SUCCESS) then 
        print *, "ERROR in ESMF_FieldConstructNew: Field construct"
        return
      endif 

      call ESMF_ArraySpecGet(arrayspec, rank=rank, rc=status)
      call ESMF_GridGetDE(grid, lcelltot_index=index, rc=status)

      do i=1, rank
        counts(i) = index(i)%r - index(i)%l + 1
      enddo

      array = ESMF_ArrayCreate(arrayspec, counts, status) 
      if(status .NE. ESMF_SUCCESS) then 
        print *, "ERROR in ESMF_FieldConstructNew: Array create"
        return
      endif 

      ftype%mapping = ESMF_DataMapCreate(ESMF_IO_IJ, relloc, status)
      if(status .NE. ESMF_SUCCESS) then 
        print *, "ERROR in ESMF_FieldConstructNew: datamap create"
        return
      endif 
      ftype%localfield%localdata = array

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldConstructNew

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_FieldConstructNewArray - Construct the internals of a Field

! !INTERFACE:
      subroutine ESMF_FieldConstructNewArray(ftype, grid, array, relloc, &
                                             datamap, name, iospec, rc)
!
! !ARGUMENTS:
      type(ESMF_FieldType), pointer :: ftype 
      type(ESMF_Grid) :: grid               
      type(ESMF_Array), intent(in) :: array     
      type(ESMF_RelLoc), intent(in), optional :: relloc
      type(ESMF_DataMap), intent(in), optional :: datamap           
      character (len=*), intent(in), optional :: name
      type(ESMF_IOSpec), intent(in), optional :: iospec 
      integer, intent(out), optional :: rc              
!
! !DESCRIPTION:
! 
!     Constructs all {\tt Field} internals, including the allocation
!     of a data {\tt Array}.  
!
!     \begin{description}
!     \item [ftype]
!           Pointer to a {\tt Field} object.
!     \item [grid] 
!           Pointer to a {\tt Grid} object. 
!     \item [array]
!           Data. 
!     \item [{[relloc]}] 
!           Relative location of data per grid cell/vertex. 
!     \item [{[datamap]}]
!           Describes the mapping of data to the {\tt Grid}.
!     \item [{[name]}] 
!           {\tt Field} name. 
!     \item [{[iospec]}] 
!           I/O specification. 
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!           
!     \end{description}
!
! !REQUIREMENTS: FLD1.1.3, FLD1.5.1
!EOP

      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present

!     Initialize return code   
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent = .TRUE. 
        rc = ESMF_FAILURE
      endif     

      call ESMF_FieldConstructNoArray(ftype, grid, relloc, datamap, & 
                                      name, iospec, status)
      if(status .NE. ESMF_SUCCESS) then 
        print *, "ERROR in ESMF_FieldConstructNew: Field construct"
        return
      endif 

      ftype%mapping = ESMF_DataMapCreate(ESMF_IO_IJ, relloc, status)
      if(status .NE. ESMF_SUCCESS) then 
        print *, "ERROR in ESMF_FieldConstructNew: datamap create"
        return
      endif 
      ftype%localfield%localdata = array
      ftype%localfield%datastatus = ESMF_STATE_READY

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldConstructNewArray

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_FieldConstructNoBuffer - Construct a Field with no associated buffer

! !INTERFACE:
      subroutine ESMF_FieldConstructNoBuffer(ftype, grid, arrayspec, relloc, &
                                             datamap, name, iospec, rc)
!
! !ARGUMENTS:     
      type(ESMF_FieldType), pointer :: ftype                
      type(ESMF_Grid) :: grid               
      type(ESMF_ArraySpec), intent(in) :: arrayspec     
      type(ESMF_RelLoc), intent(in), optional :: relloc
      type(ESMF_DataMap), intent(in), optional :: datamap 
      character (len=*), intent(in), optional :: name
      type(ESMF_IOSpec), intent(in), optional :: iospec 
      integer, intent(out), optional :: rc              
!
! !DESCRIPTION:
! 
!     Constructs all {\tt Field} internals except for the assignment of 
!     an associated data buffer.
!
!     \begin{description}
!     \item [ftype]
!           Pointer to a {\tt Field} object.
!     \item [grid] 
!           Pointer to a {\tt Grid} object. 
!     \item [arrayspec]
!           Data specification. 
!     \item [{[relloc]}] 
!           Relative location of data per grid cell/vertex. 
!     \item [{[datamap]}]
!           Describes the mapping of data to the {\tt Grid}.
!     \item [{[name]}] 
!           {\tt Field} name. 
!     \item [{[iospec]}] 
!           I/O specification. 
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!           
!     \end{description}
!
! !REQUIREMENTS: FLD1.1.3, FLD1.5.1
!EOP

      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present

!     Initialize return code   
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent = .TRUE. 
        rc = ESMF_FAILURE
      endif     
 
!     Construct a default name if one is not given
      call ESMF_SetName(ftype%base, name, "Fields", status)
      if(status .NE. 0) then 
        print *, "ERROR in ESMF_FieldConstructNoBuffer: SetName"
        return
      endif 

      ftype%grid = grid
      ftype%mapping = ESMF_DataMapCreate(ESMF_IO_IJK, relloc, status)
!     call ESMF_ArrayConstructNoBuffer(ftype%array)

!     If I/O spec is present, copy it into the field object; otherwise just 
!     initialize the I/O spec in the field object.
      if(present(iospec)) then
!       ESMF_IOSpecCopyInit(ftype%iospec, iospec, status)
        if(status .NE. ESMF_SUCCESS) then 
          print *, "ERROR in ESMF_FieldConstructNoBuffer: IOSpec init"
          return
        endif 
      else 
!       ESMF_IOSpecInit(ftype%iospec, status)
        if(status .NE. ESMF_SUCCESS) then 
          print *, "ERROR in ESMF_FieldConstructNoBuffer: IOSpec init"
          return
        endif 
      endif

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldConstructNoBuffer

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_FieldConstructNoArray - Construct a Field with no associated Array

! !INTERFACE:
      subroutine ESMF_FieldConstructNoArray(ftype, grid, relloc, datamap, & 
                                            name, iospec, rc)
!
! !ARGUMENTS:     
      type(ESMF_FieldType), pointer :: ftype   
      type(ESMF_Grid) :: grid                 
      type(ESMF_RelLoc), intent(in), optional :: relloc          
      type(ESMF_DataMap), intent(in), optional :: datamap              
      character (len=*), intent(in), optional :: name    
      type(ESMF_IOSpec), intent(in), optional :: iospec  
      integer, intent(out), optional :: rc               
!
! !DESCRIPTION:
! 
!     Constructs a {\tt Field} except for its internal data {\tt Array}.
!
!     \begin{description}
!     \item [ftype]
!           Pointer to a {\tt Field} object.
!     \item [grid] 
!           Pointer to a {\tt Grid} object. 
!     \item [{[relloc]}] 
!           Relative location of data per grid cell/vertex. 
!     \item [{[datamap]}]
!           Describes the mapping of data to the {\tt Grid}.
!     \item [{[name]}] 
!           {\tt Field} name. 
!     \item [{[iospec]}] 
!           I/O specification. 
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!           
!     \end{description}
!
! !REQUIREMENTS: FLD1.1.3, FLD1.5.1
!EOP

      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present

!     Initialize return code
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent=.TRUE.
        rc = ESMF_FAILURE
      endif

!     Construct a default name if one is not given
      call ESMF_SetName(ftype%base, name, "Fields", status)
      if(status .NE. 0) then 
        print *, "ERROR in ESMF_FieldConstructNoArray: SetName"
        return
      endif 

!     Attach grid
      ftype%grid = grid
!
! add more code here
!
     
      if (rcpresent) rc = ESMF_SUCCESS
      
      end subroutine ESMF_FieldConstructNoArray

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_FieldConstructNoGridArray - Construct a Field with no Grid or Array
!
! !INTERFACE:
      subroutine ESMF_FieldConstructNoGridArray(ftype, name, iospec, rc)
!
! !ARGUMENTS:     
      type(ESMF_FieldType), pointer :: ftype  
      character (len = *), intent(in), optional :: name  
      type(ESMF_IOSpec), intent(in), optional :: iospec  
      integer, intent(out), optional :: rc               
!
! !DESCRIPTION:
! 
!     Constructs {\tt Field} internals except those related to {\tt Grid} 
!     and {\tt Data}.
!
! !REQUIREMENTS: FLD1.1.3, FLD1.5.1
!EOP

      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present

!     Initialize return code
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent=.TRUE.
        rc = ESMF_FAILURE
      endif

!     Construct a default name if one is not given
      call ESMF_SetName(ftype%base, name, "Fields", status)
      if(status .NE. 0) then 
        print *, "ERROR in ESMF_FieldConstructNoGridArray: SetName"
        return
      endif 

!     Initialize field contents
      ftype%localfield%gridstatus = ESMF_STATE_UNINIT
      ftype%localfield%datastatus = ESMF_STATE_UNINIT
      ftype%gridstatus = ESMF_STATE_UNINIT

      ftype%fieldstatus = ESMF_STATE_READY

!
! add more code here
!
     
      if (rcpresent) rc = ESMF_SUCCESS
      
      end subroutine ESMF_FieldConstructNoGridArray

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:   ESMF_FieldDestruct - Free any Field memory allocated internally
!
! !INTERFACE:
      subroutine ESMF_FieldDestruct(ftype, rc)
!
! !ARGUMENTS:
      type(ESMF_FieldType), pointer :: ftype        
      integer, intent(out), optional :: rc         
!
! !DESCRIPTION:
!     Releases all resources except the {\tt Field} itself.
!
!     \begin{description}
!     \item [ftype]
!           Pointer to a {\tt Field} object.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!           
!     \end{description}
!
! !REQUIREMENTS: 
!EOP

      logical :: rcpresent                          ! Return code present

!     Initialize return code; assume failure until success is certain
      rcpresent = .FALSE.
      if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
      endif


!
! TODO: more code goes here
!


      if (rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldDestruct
!
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!
!  This section includes all the Data Attach and Detach routines.
!  These are used for user access to data.
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_FieldAttachBuffer - Associate a data buffer with a field

! !INTERFACE:
      subroutine ESMF_FieldAttachBuffer(field, buffer, copyflag, rc)
!
! !ARGUMENTS:
 
      type(ESMF_Field), intent(in) :: field            
      real, dimension (:), pointer :: buffer           
      type(ESMF_CopyFlag), intent(in), optional :: copyflag      
      integer, intent(out), optional :: rc              
!
! !DESCRIPTION:
!     Associates a data buffer with a {\tt Field} and sets a flag in 
!     the {\tt Field} indicating that data is present.  
!
! !REQUIREMENTS: FLD1.6.5
!EOP

        end subroutine ESMF_FieldAttachBuffer

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_FieldAttachArray - Associate an Array object with a Field

! !INTERFACE:
      subroutine ESMF_FieldAttachArray(field, array, copyflag, rc)
!
! !ARGUMENTS:
      type(ESMF_Field), intent(in) :: field
      type(ESMF_Array), intent(in) :: array            
      type(ESMF_CopyFlag), intent(in), optional :: copyflag      
      integer, intent(out), optional :: rc             
!
! !DESCRIPTION:
!     Associates an {\tt Array} with a Field and sets a flag in the 
!     {\tt Field} indicating that data is present.
!
! !REQUIREMENTS: FLD1.6.5
!EOP

        end subroutine ESMF_FieldAttachArray

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_FieldAttachGridArray - Associate a Grid and an Array with a Field

! !INTERFACE:
      subroutine ESMF_FieldAttachGridArray(field, grid, array, copyflag, rc)
!
! !ARGUMENTS:
      type(ESMF_Field), intent(in) :: field            
      type(ESMF_Grid) :: grid
      type(ESMF_Array), intent(in) :: array
      type(ESMF_CopyFlag), intent(in) :: copyflag      
      integer, intent(out), optional :: rc             
!
! !DESCRIPTION:
!     Associates an {\tt Array} and a {\tt Grid} with a Field and sets a 
!     flag in the {\tt Field} indicating that data is present.
!
! !REQUIREMENTS: FLD1.6.5
!EOP

        end subroutine ESMF_FieldAttachGridArray

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_FieldDetachBuffer - Disassociate a buffer from a Field

! !INTERFACE:
      subroutine ESMF_FieldDetachBuffer(field, buffer, access, rc)
!
! !ARGUMENTS:
      type(ESMF_Field), intent(in) :: field             
      integer, intent(out) :: buffer                    
      type(ESMF_Access), intent(in), optional :: access 
      integer, intent(out), optional :: rc              
!
! !DESCRIPTION:
!     Returns a pointer to the {\tt Field}'s data buffer and marks the 
!     {\tt Field} as not having any associated data.
!
! !REQUIREMENTS: FLD1.6.5
!EOP

!
! TODO: code goes here
!
        end subroutine ESMF_FieldDetachBuffer

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_FieldDetachArray - Disassociate an Array from a Field

! !INTERFACE:
      subroutine ESMF_FieldDetachArray(field, array, access, rc)
!
! !ARGUMENTS:
      type(ESMF_Field), intent(in) :: field             ! field to operate on
      type(ESMF_Array), intent(out) :: array            ! data array
      type(ESMF_Access), intent(in), optional :: access ! default is r/w 
      integer, intent(out), optional :: rc              ! return code
!
! !DESCRIPTION:
!     Returns a pointer to the {\tt Field}'s {\tt Array} and marks the 
!     {\tt Field} as not having any associated data.
!
! !REQUIREMENTS: FLD1.6.5
!EOP

!
! TODO: code goes here
!
        end subroutine ESMF_FieldDetachArray

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!
! Set and get Field name, attributes, Grid and Data values.
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_FieldGetName - Retrieve the name of a Field
!
! !INTERFACE:
      subroutine ESMF_FieldGetName(field, name, rc)

!
! !ARGUMENTS:
      type(ESMF_Field), intent(in) :: field                ! field to be queried
      character (len = *), intent(out) :: name             ! field name
      integer, intent(out), optional :: rc                 ! return code

!
! !DESCRIPTION:
!      Returns the name of the field.  If the field was created without 
!      specifying a name, the framework will have assigned it a unique one.
!
! !REQUIREMENTS: FLD1.5.1, FLD1.7.1
!EOP
      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present

!     Initialize return code; assume failure until success is certain
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
      endif

      call ESMF_GetName(field%ftypep%base, name, status)
      if(status .NE. 0) then 
        print *, "ERROR in ESMF_FieldGetName"
        return
      endif 

      if (rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldGetName

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_FieldGetIntegerAttribute - Retrieve an Attribute from a Field
!
! !INTERFACE:
      subroutine ESMF_FieldGetIntegerAttribute(field, name, value, rc)
!
! !ARGUMENTS:
      type(ESMF_Field), intent(in) :: field  
      character (len = *), intent(in) :: name
      integer, intent(out) :: value
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Returns an integer attribute from a {\tt Field}.
!
! 
!     \begin{description}
!     \item [field]
!           A {\tt Field} object.
!     \item [name]
!           The name of the Attribute to retrieve.
!     \item [value]
!           The integer value of the named Attribute.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!           
!     \end{description}
!
!
! !REQUIREMENTS: FLD1.5.1, FLD1.7.1
!EOP
      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present

!     Initialize return code; assume failure until success is certain
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
      endif

      !call ESMF_GetIntegerAttribute(field%ftypep%base, name, value, status)
      !if(status .NE. 0) then 
      !  print *, "ERROR in ESMF_FieldGetAttribute"
      !  return
      !endif 

      if (rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldGetIntegerAttribute

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_FieldGetConfig - Return information about a Field
!
! !INTERFACE:
      subroutine ESMF_FieldGetConfig(field, hasgrid, hasarray, hasbuffer, rc)
!
! !ARGUMENTS:
       type(ESMF_Field), intent(in) :: field      
       logical, intent(out), optional :: hasgrid            
       logical, intent(out), optional :: hasarray            
       logical, intent(out), optional :: hasbuffer           
       integer, intent(out), optional :: rc       
!
! !DESCRIPTION:
!      Returns whether a {\tt Field} has a grid, array, or buffer 
!      associated with it.
!
! !REQUIREMENTS: FLD1.6.2, FLD1.1.3
!EOP

!
! TODO: code goes here
!
        end subroutine ESMF_FieldGetConfig

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_FieldGetGrid - Return the Grid associated with a Field
!
! !INTERFACE:
      subroutine ESMF_FieldGetGrid(field, grid, rc)
!
! !ARGUMENTS:
      type(ESMF_Field), intent(in) :: field    
      type(ESMF_Grid), intent(out) :: grid     
      integer, intent(out), optional :: rc     
!
! !DESCRIPTION:
!      Returns a reference to the Grid associated with this Field.
!
! !REQUIREMENTS: FLD1.6.2
!EOP

        grid = field%ftypep%grid

        end subroutine ESMF_FieldGetGrid

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_FieldGetGlobalGridInfo - Get information about the Global Grid
!
! !INTERFACE:
      subroutine ESMF_FieldGetGlobalGridInfo(field, ndim, ncell, nvertex, &
        size, rc)
!
! !ARGUMENTS:
      type(ESMF_Field), intent(in) :: field      
      integer, intent(out), optional :: ndim            
      integer, intent(out), optional :: ncell            
      integer, intent(out), optional :: nvertex           
      integer, intent(out), optional :: size(:)  
      integer, intent(out), optional :: rc       
!
! !DESCRIPTION:
!     Return global {\tt Grid} information. 
!
! !REQUIREMENTS: FLD1.7.2
!EOP

!
! TODO: code goes here
!
        end subroutine ESMF_FieldGetGlobalGridInfo

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_FieldGetLocalGridInfo - Get information about the Local Grid
!
! !INTERFACE:
      subroutine ESMF_FieldGetLocalGridInfo(field, ndim, ncell, nvertex, & 
        size, rc)
!
! !ARGUMENTS:
      type(ESMF_Field), intent(in) :: field   
      integer, intent(out), optional :: ndim            
      integer, intent(out), optional :: ncell            
      integer, intent(out), optional :: nvertex           
      integer, intent(out), optional :: size(:)  
      integer, intent(out), optional :: rc       
!
! !DESCRIPTION:
!      Get {\tt Grid} information specific to the local {\tt DE}.
!
! !REQUIREMENTS: FLD1.7.2
!EOP

!
! TODO: code goes here
!
        end subroutine ESMF_FieldGetLocalGridInfo

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_FieldGetData - Get Data associated with the Field
!
! !INTERFACE:
      subroutine ESMF_FieldGetData(field, array, buffer, rc)

!
! !ARGUMENTS:
      type(ESMF_Field), intent(in) :: field      
      type(ESMF_Array), intent(out), optional :: array
      integer, intent(out), optional :: buffer
      integer, intent(out), optional :: rc           

!
! !DESCRIPTION:
!     Get data either in {\tt Array} or buffer form.

!
! !REQUIREMENTS: FLD1.3, FLD1.6.4 (pri 2?), FLD1.7.2
!EOP

!
! TODO: code goes here
!
      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present
      logical :: apresent                         ! Array present
      logical :: bpresent                         ! Buffer present

!     ! Initialize return code   
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      apresent = .FALSE.
      bpresent = .FALSE.
      if(present(rc)) then
        rcpresent=.TRUE.
        rc = ESMF_FAILURE
      endif

!     ! Set codes depending on what the caller specified
      if(present(array)) apresent=.TRUE.
      if(present(buffer)) bpresent=.TRUE.

      if(apresent) then
          ! TODO: check that an array is associated with the field
          !  if (field%ptr%localfield%datastatus .eq. ...)

          array = field%ftypep%localfield%localdata
      endif 
   
      if(bpresent) then
          ! TODO: check that an array is associated with the field
          !  if (field%ptr%localfield%datastatus .eq. ...)

          ! TODO: and extract data also
          ! array = field%ptr%localfield%localdata
      endif 

!     Set return values.
      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldGetData

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_FieldGetGlobalDataInfo - Get information about Field Data
!
! !INTERFACE:
      subroutine ESMF_FieldGetGlobalDataInfo(field, size, &
        indexorder, datatype, interleave, rc)
!
! !ARGUMENTS:
      type(ESMF_Field), intent(in) :: field            
      integer, intent(out), optional :: size(:)        
      integer, dimension(ESMF_MAXDIM), intent(out) :: indexorder 
      type(ESMF_DataType), intent(out) :: datatype
      type(ESMF_Interleave), intent(out) :: interleave   
      integer, intent(out), optional :: rc           

!
! !DESCRIPTION:
!     Retrieve global {\tt Field} data information.

!
! !REQUIREMENTS: FLD1.3, FLD1.6.4 (pri 2?), FLD1.7.2
!EOP

!
! TODO: code goes here
!
        end subroutine ESMF_FieldGetGlobalDataInfo

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_FieldGetLocalDataInfo - Get information about Field Data
!
! !INTERFACE:
      subroutine ESMF_FieldGetLocalDataInfo(field, size, &
        indexorder, datatype, interleave, rc)
!
! !ARGUMENTS:
      type(ESMF_Field), intent(in) :: field            
      integer, intent(out), optional :: size(:)        
      integer, dimension(ESMF_MAXDIM), intent(out) :: indexorder 
      type(ESMF_DataType), intent(out) :: datatype
      type(ESMF_Interleave), intent(out) :: interleave   
      integer, intent(out), optional :: rc           

!
! !DESCRIPTION:
!     Retrieve {\tt Field} data information specific to the local
!     {\tt DE}.
!
! !REQUIREMENTS: FLD1.3, FLD1.6.4 (pri 2?), FLD1.7.2
!EOP

!
! TODO: code goes here
!
        end subroutine ESMF_FieldGetLocalDataInfo

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_FieldGetDataMap - Get DataMap associated with Field
! !INTERFACE:
      subroutine ESMF_FieldGetDataMap(field, datamap, rc)
!
! !ARGUMENTS:
      type(ESMF_Field), intent(in) :: field          
      type(ESMF_DataMap), intent(out) :: datamap     
      integer, intent(out), optional :: rc           
!
! !DESCRIPTION:
!      Returns a description of the actual ordering of data in the
!      memory buffer, e.g. row-major/column-major.

!
! !REQUIREMENTS: FLD1.2, FLD1.6.3 (pri 2?)
!EOP

!
! TODO: code goes here
!
        end subroutine ESMF_FieldGetDataMap

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!
! Entry points for fuctionality which will happen mostly below at the Grid
!   level, but needs a Data Pointer as well as grid info to operate.
!   These include Reduction operations, Halo, and Transpose.
!
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_FieldReduce - Reduction operation on a Field

! !INTERFACE:
      subroutine ESMF_FieldReduce(field, rtype, result, rc)
!
!
! !ARGUMENTS:
      type(ESMF_Field) :: field                 
      integer :: rtype
      integer :: result
      integer, intent(out), optional :: rc               
!
! !DESCRIPTION:
!     Call {\tt Grid} routines to perform a Reduction operation over the data
!       in a {\tt Field}.
!
!     \begin{description}
!     \item [field] 
!           Field containing data to be reduced.
!     \item [rtype]
!           Type of reduction operation to perform.  Options include: ...
!     \item [result] 
!           Numeric result (may be single number, may be array)
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!           
!     \end{description}
!
!EOP
! !REQUIREMENTS: 

      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present
   
!     Initialize return code   
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent = .TRUE. 
        rc = ESMF_FAILURE
      endif     

!     Call Grid method to perform actual work
      !call ESMF_GridReduce(field%ftypep%grid, &
      !                     field%ftypep%localfield%localdata, &
      !                     rtype, result, status)
      !if(status .NE. ESMF_SUCCESS) then 
      !  print *, "ERROR in FieldReduce: Grid reduce"
      !  return
      !endif 

!     Set return values.
      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldReduce


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_FieldHalo - Data Halo operation on a Field

! !INTERFACE:
      subroutine ESMF_FieldHalo(field, rc)
!
!
! !ARGUMENTS:
      type(ESMF_Field) :: field                 
      integer, intent(out), optional :: rc               
!
! !DESCRIPTION:
!     Call {\tt Grid} routines to perform a Halo operation over the data
!     in a {\tt Field}.  This routine updates the data inside the {\tt Field}
!     so there is no separate return argument.
!
!     \begin{description}
!     \item [field] 
!           Field containing data to be haloed.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!           
!     \end{description}
!
!EOP
! !REQUIREMENTS: 

      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present
      type(ESMF_FieldType) :: ftypep              ! field type info
      type(ESMF_AxisIndex) :: axis(ESMF_MAXDIM)   ! Size info for Grid
      integer :: i, gridrank, datarank, thisdim
      integer :: dimorder(ESMF_MAXDIM)   
      integer :: dimlengths(ESMF_MAXDIM)   
   
      ! Initialize return code   
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent = .TRUE. 
        rc = ESMF_FAILURE
      endif     

      ftypep = field%ftypep

      ! Query the datamap and set info for grid so it knows how to
      !  match up the array indicies and the grid indicies.
      call ESMF_DataMapGet(ftypep%mapping, gridrank=gridrank, &
                                               dimlist=dimorder, rc=status)
      if(status .NE. ESMF_SUCCESS) then 
        print *, "ERROR in FieldHalo: DataMapGet returned failure"
        return
      endif 

      ! And get the Array sizes
      call ESMF_ArrayGet(ftypep%localfield%localdata, rank=datarank, &
                                               lengths=dimlengths, rc=status)
      if(status .NE. ESMF_SUCCESS) then 
        print *, "ERROR in FieldHalo: ArrayGet returned failure"
        return
      endif 

      ! Set the axis info on the array to pass thru to DistGrid
      do i=1, gridrank
          thisdim = dimorder(i)
          if (thisdim .eq. 0) cycle
     
          ! TODO: this needs to be hidden behind a method to get & set
          axis(i)%l = 0
          axis(i)%r = dimlengths(thisdim)
          axis(i)%max = dimlengths(thisdim)
          axis(i)%decomp = thisdim
          axis(i)%gstart = 0
     
      enddo

      ! Attach this info to the array
      call ESMF_ArraySetAxisIndex(ftypep%localfield%localdata, axis, status)
      if(status .NE. ESMF_SUCCESS) then 
        print *, "ERROR in FieldHalo: ArraySetAxisIndex returned failure"
        return
      endif 

      ! Call Grid method to perform actual work
      call ESMF_GridHalo(field%ftypep%grid, field%ftypep%localfield%localdata, &
                                                        status)
      if(status .NE. ESMF_SUCCESS) then 
        print *, "ERROR in FieldHalo: Grid Halo returned failure"
        return
      endif 

      ! Set return values.
      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldHalo



!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_FieldRegrid - Data Regrid operation on a Field

! !INTERFACE:
      subroutine ESMF_FieldRegrid(srcfield, dstfield, rc)
!
!
! !ARGUMENTS:
      type(ESMF_Field) :: srcfield                 
      type(ESMF_Field) :: dstfield                 
      integer, intent(out), optional :: rc               
!
! !DESCRIPTION:
!     Call {\tt Grid} routines to perform a Regrid operation over the data
!     in a {\tt Field}.  This routine reads the source field and leaves the
!     data untouched.  It reads the Grid from the destination field and
!     updates the array data in the destination.
!
!     \begin{description}
!     \item [srcfield] 
!           Field containing source data.
!     \item [dstfield] 
!           Field containing destination grid.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!           
!     \end{description}
!
!EOP
! !REQUIREMENTS: 

      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present
      type(ESMF_FieldType) :: fp                  ! field type info
      integer :: i, gridrank, datarank, thisdim
      integer :: dimorder(ESMF_MAXDIM)   
      integer :: dimlengths(ESMF_MAXDIM)   
   
      ! Initialize return code   
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent = .TRUE. 
        rc = ESMF_FAILURE
      endif     

      fp = srcfield%ftypep

      ! Query the datamap and set info for grid so it knows how to
      !  match up the array indicies and the grid indicies.
      call ESMF_DataMapGet(fp%mapping, gridrank=gridrank, &
                                               dimlist=dimorder, rc=status)
      if(status .NE. ESMF_SUCCESS) then 
        print *, "ERROR in FieldRegrid: DataMapGet returned failure"
        return
      endif 

      ! And get the Array sizes
      call ESMF_ArrayGet(fp%localfield%localdata, rank=datarank, &
                                               lengths=dimlengths, rc=status)
      if(status .NE. ESMF_SUCCESS) then 
        print *, "ERROR in FieldRegrid: ArrayGet returned failure"
        return
      endif 

      ! TODO: add code here to call Regrid correctly

      ! Call Grid method to perform actual work
      ! call ESMF_GridRegrid(fp%grid, fp%localfield%localdata, status)
      status = ESMF_FAILURE

      if(status .NE. ESMF_SUCCESS) then 
        print *, "ERROR in FieldRegrid: Grid Regrid returned failure"
        return
      endif 

      ! Set return values.
      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldRegrid

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_FieldRoute - Data Route operation on a Field

! !INTERFACE:
      subroutine ESMF_FieldRoute(srcfield, dstfield, parentlayout, rc)
!
!
! !ARGUMENTS:
      type(ESMF_Field) :: srcfield                 
      type(ESMF_Field) :: dstfield                 
      type(ESMF_DELayout) :: parentlayout
      integer, intent(out), optional :: rc               
!
! !DESCRIPTION:
!     Call routines to perform a Route operation over the data
!     in a {\tt Field}.  This routine reads the source field and leaves the
!     data untouched.  It reads the Grid from the destination field and
!     updates the array data in the destination.
!
!     \begin{description}
!     \item [srcfield] 
!           Field containing source data.
!     \item [dstfield] 
!           Field containing destination grid.
!     \item [parentlayout] 
!           Layout which encompasses both Fields, most commonly the layout
!           of the Coupler if the route is inter-component, but could 
!           also be the individual layout for a component if the Route 
!           is intra-component.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!           
!     \end{description}
!
!EOP
! !REQUIREMENTS: 

      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present
      type(ESMF_FieldType) :: stypep, dtypep      ! field type info
      type(ESMF_Route) :: route
      type(ESMF_XPacket) :: srclxp, dstlxp        ! src/dst localdata xps
      type(ESMF_XPacket) :: srcgxp, dstgxp        ! src/dst global xp
      type(ESMF_DELayout) :: srclayout, dstlayout
      logical :: hassrcdata        ! does this DE contain localdata from src?
      logical :: hasdstdata        ! does this DE contain localdata from dst?
      logical :: hascachedroute    ! can we reuse an existing route?
      integer :: i, gridrank, datarank, thisdim
      integer :: nx, ny
      integer :: dimorder(ESMF_MAXDIM)   
      integer :: dimlengths(ESMF_MAXDIM)   
      integer :: my_src_DE, my_dst_DE, my_DE
      type(ESMF_AxisIndex), dimension(:,:), pointer :: src_AI, dst_AI
      integer :: AI_snd_count, AI_rcv_count

   
      ! Initialize return code   
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent = .TRUE. 
        rc = ESMF_FAILURE
      endif     

      stypep = srcfield%ftypep
      dtypep = dstfield%ftypep

      ! Our DE number in the parent layout
      call ESMF_DELayoutGetDEid(parentlayout, my_DE, status)

      ! TODO: we need not only to know if this DE has data in the field,
      !   but also the de id for both src & dest fields

      ! This routine is called on every processor in the parent layout.
      !  It is quite possible that the source and destination fields do
      !  not completely cover every processor on that layout.  Make sure
      !  we do not go lower than this on the processors which are uninvolved
      !  in this communication.

      ! if srclayout ^ parentlayout == NULL, nothing to send from this DE id.
      call ESMF_GridGetDELayout(stypep%grid, srclayout, status)
      call ESMF_DELayoutGetDEExists(parentlayout, my_DE, srclayout, hassrcdata)
      hassrcdata = .true.   ! temp for now
      if (hassrcdata) then
          ! don't ask for our de number if this de isn't part of the layout
          call ESMF_DELayoutGetDEid(srclayout, my_src_DE, status)
      endif

      ! if dstlayout ^ parentlayout == NULL, nothing to recv on this DE id.
      call ESMF_GridGetDELayout(dtypep%grid, dstlayout, status)
      call ESMF_DELayoutGetDEExists(parentlayout, my_DE, dstlayout, hasdstdata)
      hasdstdata = .true.   ! temp for now
      if (hasdstdata) then
          ! don't ask for our de number if this de isn't part of the layout
          call ESMF_DELayoutGetDEid(dstlayout, my_dst_DE, status)
      endif

      ! if neither are true this DE cannot be involved in the communication
      !  and it can just return now.
      if ((.not. hassrcdata) .and. (.not. hasdstdata)) then
          if (rcpresent) rc = ESMF_SUCCESS
          return
      endif

      ! if src field exists on this DE, query it for information
      if (hassrcdata) then
          ! Query the datamap and set info for grid so it knows how to
          !  match up the array indicies and the grid indicies.
          call ESMF_DataMapGet(stypep%mapping, gridrank=gridrank, &
                                               dimlist=dimorder, rc=status)
          if(status .NE. ESMF_SUCCESS) then 
            print *, "ERROR in FieldRoute: DataMapGet returned failure"
            return
          endif 

          ! And get the Array sizes
          call ESMF_ArrayGet(stypep%localfield%localdata, rank=datarank, &
                                               lengths=dimlengths, rc=status)
          if(status .NE. ESMF_SUCCESS) then 
             print *, "ERROR in FieldRoute: ArrayGet returned failure"
             return
          endif 
      endif 

      ! if dst field exists on this DE, query it for information
      if (hasdstdata) then
          ! Query the datamap and set info for grid so it knows how to
          !  match up the array indicies and the grid indicies.
          call ESMF_DataMapGet(dtypep%mapping, gridrank=gridrank, &
                                               dimlist=dimorder, rc=status)
          if(status .NE. ESMF_SUCCESS) then 
            print *, "ERROR in FieldRoute: DataMapGet returned failure"
            return
          endif 

          ! And get the Array sizes
          call ESMF_ArrayGet(dtypep%localfield%localdata, rank=datarank, &
                                               lengths=dimlengths, rc=status)
          if(status .NE. ESMF_SUCCESS) then 
             print *, "ERROR in FieldRoute: ArrayGet returned failure"
             return
          endif 
      endif

      ! set up things we need to find a cached route or precompute one
      if (hassrcdata) then
          call ESMF_GridGetAllAxisIndex(stypep%grid, src_AI)
          call ESMF_DELayoutGetSize(srclayout, nx, ny);
          AI_snd_count = nx * ny
      else
          AI_snd_count = 0
      endif
      if (hasdstdata) then
          call ESMF_GridGetAllAxisIndex(dtypep%grid, dst_AI)
          call ESMF_DELayoutGetSize(dstlayout, nx, ny);
          AI_rcv_count = nx * ny
      else
          AI_rcv_count = 0
      endif
          
      ! Does this same route already exist?  If so, then we can drop
      ! down immediately to RouteRun.
      call ESMF_RouteGetCached(datarank, my_dst_DE, dst_AI, &
                                  AI_rcv_count, dstlayout, my_src_DE, &
                                  src_AI, AI_snd_count, srclayout, &
                                  hascachedroute, route, rc=status)

      if (.not. hascachedroute) then
          ! Create the route object.  This needs to be the parent layout which
          ! includes the DEs from both fields.
          route = ESMF_RouteCreate(parentlayout, rc) 

          call ESMF_RoutePrecompute(route, datarank, my_dst_DE, dst_AI, &
                                   AI_rcv_count, dstlayout, my_src_DE, &
                                   src_AI, AI_snd_count, srclayout)

      endif

      ! Once table is full, execute the communications it represents.

      ! There are 3 possible cases - src+dst, src only, dst only
      !  (if both are false then we've already returned.)
      if ((hassrcdata) .and. (.not. hasdstdata)) then
          call ESMF_RouteRun(route, srcarray=stypep%localfield%localdata, &
                                                                     rc=status) 

      else if ((.not. hassrcdata) .and. (hasdstdata)) then
          call ESMF_RouteRun(route, dstarray=dtypep%localfield%localdata, &
                                                                     rc=status)

      else
          call ESMF_RouteRun(route, stypep%localfield%localdata, &
                                    dtypep%localfield%localdata, status)
      endif
      if(status .NE. ESMF_SUCCESS) then 
        print *, "ERROR in FieldRoute: RouteRun returned failure"
        return
      endif 

      ! Set return values.
      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldRoute


!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!
! Set Field objects and values.
!
!------------------------------------------------------------------------------

!BOP
! !IROUTINE: ESMF_FieldSetGrid - Set Grid associated with the Field
!
! !INTERFACE:
      subroutine ESMF_FieldSetGrid(field, grid, rc)
!
! !ARGUMENTS:
      type(ESMF_Field), intent(in) :: field            ! field to add grid to
      type(ESMF_Grid), intent(in) :: grid              ! grid to be added
      integer, intent(out), optional :: rc             ! return code
!
! !DESCRIPTION:
!      Used only with the version of FieldCreate which creates an empty 
!      Field and allows the Grid to be specified later.  Otherwise it is 
!      an error to try to change the Grid associated with a Field.
!
! !REQUIREMENTS: FLD1.1.3
!EOP

!
! TODO: code goes here
!
        end subroutine ESMF_FieldSetGrid

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_FieldSetDataValues - Set contents of Data array
!
! !INTERFACE:
      subroutine ESMF_FieldSetDataValues(field, index, value, rc)
!
!
! !ARGUMENTS:
      type(ESMF_Field), intent(in) :: field           ! field to operate on
      integer, dimension (:), intent(in) :: index     ! index or range to set
      real, dimension (:), intent(in) :: value        ! data value(s) to set
      integer, intent(out), optional :: rc            ! return code
!
! !DESCRIPTION:
!      Allows specified data values associated with a Field to be set 
!      through the Field interface instead of detaching data and setting 
!      it outside the framework.
!
! !REQUIREMENTS: FLD1.6.7
!EOP

!
! TODO: code goes here
!
        end subroutine ESMF_FieldSetDataValues

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_FieldSetDataMap - Set DataMap assocated with a Field
!
! !INTERFACE:
      subroutine ESMF_FieldSetDataMap(field, datamap, rc)
!
! !ARGUMENTS:
      type(ESMF_Field), intent(in) :: field      ! reorder data in this field
      type(ESMF_DataMap), intent(in) :: datamap  ! new memory order of data
      integer, intent(out), optional :: rc       ! return code
!
! !DESCRIPTION:
!     Used to set the ordering of a Field.  If an initialized {\tt DataMap}
!     and associated data are already in the {\tt Field}, the data will be 
!     reordered according to the new speciification.
!
! !REQUIREMENTS: FLD1.2
!EOP

!
! TODO: code goes here
!
        end subroutine ESMF_FieldSetDataMap

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_FieldSetIntegerAttribute - Set an Attribute on a Field
!
! !INTERFACE:
      subroutine ESMF_FieldSetIntegerAttribute(field, name, value, rc)
!
! !ARGUMENTS:
      type(ESMF_Field), intent(in) :: field  
      character (len = *), intent(in) :: name
      integer, intent(in) :: value
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Returns an integer attribute from a {\tt Field}.
!
! 
!     \begin{description}
!     \item [field]
!           A {\tt Field} object.
!     \item [name]
!           The name of the Attribute to set.
!     \item [value]
!           The integer value of the named Attribute.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!           
!     \end{description}
!
!
! !REQUIREMENTS: FLD1.5.1, FLD1.7.1
!EOP
      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present

!     Initialize return code; assume failure until success is certain
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
      endif

      !call ESMF_SetIntegerAttribute(field%ftypep%base, name, value, status)
      !if(status .NE. 0) then 
      !  print *, "ERROR in ESMF_FieldSetAttribute"
      !  return
      !endif 

      if (rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldSetIntegerAttribute

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!
! Base class methods Validate and Print
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_FieldValidate - Check the internal consistency of a Field

! !INTERFACE:
      subroutine ESMF_FieldValidate(field, opt, rc)
!
! !ARGUMENTS:
      type(ESMF_Field), intent(in) :: field 
      character (len = *), intent(in) :: opt 
      integer, intent(out), optional :: rc   
!
! !DESCRIPTION:
!     Routine to validate the internal state of a {\tt Field}.
!
! !REQUIREMENTS:  FLD4.1
!EOP

!
! TODO: code goes here
!
      end subroutine ESMF_FieldValidate

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_FieldPrint - Print the contents of a Field

! !INTERFACE:
      subroutine ESMF_FieldPrint(field, options, rc)
!
!
! !ARGUMENTS:
      type(ESMF_Field), intent(in) :: field 
      character (len = *), intent(in), optional :: options
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Routine to print information about a field.
!
!EOP
! !REQUIREMENTS:

        character(len=ESMF_MAXSTR) :: name
        integer :: status


        if (present(rc)) rc = ESMF_FAILURE

        if (.not. associated(field%ftypep)) then
          print *, "Empty or Uninitialized Field"
          if (present(rc)) rc = ESMF_SUCCESS
          return
        endif

        call ESMF_GetName(field%ftypep%base, name, status)
        if(status .NE. ESMF_SUCCESS) then 
          print *, "ERROR in ESMF_FieldGetName"
          return
        endif 

        print *, "Field Print:"
        print *, "  Name = ",  trim(name)

        !TODO: add more code here to print more info

        if (present(rc)) rc = ESMF_SUCCESS

        end subroutine ESMF_FieldPrint
!
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!
! I/O methods
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_FieldCheckpoint - Save Field in the quickest manner possible
!
! !INTERFACE:
      subroutine ESMF_FieldCheckpoint(field, iospec, rc)
!
! !ARGUMENTS:
      type(ESMF_Field), intent(in) :: field                ! field to save
      type(ESMF_IOSpec), intent(in), optional :: iospec    ! file specs
      integer, intent(out), optional :: rc                 ! return code
!
! !DESCRIPTION:
!      Used to save all data to disk as quickly as possible.  
!      (see Read/Write for other options).  Internally this routine uses the
!      same I/O interface as Read/Write, but the default options are to
!      select the fastest way to save data to disk.
!
! !REQUIREMENTS: FLD1.6.8
!EOP

!
! TODO: code goes here
!
        end subroutine ESMF_FieldCheckpoint


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_FieldRestore - Read back in a Checkpointed Field
!
! !INTERFACE:
      function ESMF_FieldRestore(name, iospec, rc)
!
! !RETURN VALUE:
      type(ESMF_Field) :: ESMF_FieldRestore
!
!
! !ARGUMENTS:
      character (len = *), intent(in) :: name              ! field name to restore
      type(ESMF_IOSpec), intent(in), optional :: iospec    ! file specs
      integer, intent(out), optional :: rc                 ! return code
!
! !DESCRIPTION:
!      Used to reinitialize
!      all data associated with a Field from the last call to Checkpoint.
!
! !REQUIREMENTS: FLD1.6.8
!EOP

!
! TODO: code goes here; this is just filler to make the compiler not complain
!
        type (ESMF_Field) :: a
     
        nullify(a%ftypep)

        ESMF_FieldRestore = a

        end function ESMF_FieldRestore


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_FieldWrite - Write a Field to external storage
!
! !INTERFACE:
      subroutine ESMF_FieldWrite(field, & ! subset, 
                                 iospec, rc)
!
! !ARGUMENTS:
      type(ESMF_Field), intent(in) :: field                ! field to save
!!      type(ESMF_Subset), intent(in), optional :: subset    ! subset on write
      type(ESMF_IOSpec), intent(in), optional :: iospec    ! file specs
      integer, intent(out), optional :: rc                 ! return code
!
! !DESCRIPTION:
!      Used to write data to persistent storage in a variety of formats.  
!      (see Checkpoint/Restore for quick data dumps.)  Details of I/O 
!      options specified in the IOSpec derived type. 
!
!
! !REQUIREMENTS: FLD3.1, FLD3.2, FLD3.3, FLD3.4, FLD3.5
!EOP

!
! TODO: code goes here
!
        ! call ESMF_Log(?, 'entry into ESMF_FieldWrite');
     
        

        end subroutine ESMF_FieldWrite


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_FieldRead - Read in a Field from external storage
!
! !INTERFACE:
      function ESMF_FieldRead(fname, gname, dnames, iospec, rc)
!
! !RETURN VALUE:
      type(ESMF_Field) :: ESMF_FieldRead
!
! !ARGUMENTS:
      character (len = *), intent(in) :: fname            ! field name to read
      character (len = *), intent(in), optional :: gname   ! grid name
      character (len = *), intent(in), optional :: dnames  ! data name
      type(ESMF_IOSpec), intent(in), optional :: iospec   ! file specs
      integer, intent(out), optional :: rc                ! return code
!
! !DESCRIPTION:
!      Used to read data from persistent storage in a variety of formats.
!      This includes creating the Grid associated with this Field.
!      To share a single Grid betwen multiple Fields, see the FieldCreate calls.
!
!
! !REQUIREMENTS: 
!EOP

!
! TODO: code goes here.  this is filler to keep the compiler from complaining.
!
        type (ESMF_Field) :: a
     
        nullify(a%ftypep)

        ESMF_FieldRead = a


        end function ESMF_FieldRead


        end module ESMF_FieldMod
