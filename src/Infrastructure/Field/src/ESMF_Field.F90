! $Id: ESMF_Field.F90,v 1.100 2004/01/28 21:46:48 nscollins Exp $
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
#include "ESMF.h"

!------------------------------------------------------------------------------
!
!BOPI
! !MODULE: ESMF_FieldMod - Combine physical field metadata, data and grid
!
! !DESCRIPTION:
! The code in this file implements the {\tt ESMF\_Field} class, which 
! represents a
! single scalar or vector field.  {\tt ESMF\_Field}s associate a metadata 
! description 
! expressed as a set of {\tt ESMF\_Attributes} with a data {\tt ESMF\_Array}, 
! {\tt ESMF\_Grid}, and I/O specification, or {\tt ESMF\_IOSpec}.  
! A {\tt ESMF\_DataMap} describes the 
! relationship of the {\tt ESMF\_Array} to the {\tt ESMF\_Grid}.  
!
! This type is implemented in Fortran 90 and a corresponding
! C++ interface is provided for access.
!
!------------------------------------------------------------------------------
! !USES:
      use ESMF_BaseMod
      use ESMF_IOMod
      use ESMF_LocalArrayMod
      use ESMF_DataMapMod
      use ESMF_DELayoutMod
      use ESMF_GridTypesMod
      use ESMF_GridMod
      use ESMF_ArrayBaseMod
      use ESMF_ArrayExpandMod
      use ESMF_ArrayCommMod
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
!     ! ESMF_LocalField
!      
!     ! The LocalField class contains information which is associated with the
!     ! local DE.

      type ESMF_LocalField
      sequence
      !private
   
        type (ESMF_Array) :: localdata           ! local data for this DE
        !type (ESMF_Status) :: gridstatus         ! is grid set yet?
        !type (ESMF_Status) :: datastatus         ! is data set yet?
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
      !private
       
        type (ESMF_Base) :: base             ! base class object
#if !defined(ESMF_NO_INITIALIZERS) && !defined(ESMF_AIX_8_INITBUG)
        type (ESMF_Status) :: fieldstatus = ESMF_STATE_UNINIT
        type (ESMF_Status) :: gridstatus = ESMF_STATE_UNINIT
        type (ESMF_Status) :: datastatus = ESMF_STATE_UNINIT
        type (ESMF_GridType), pointer :: gridp => NULL()  ! for faster access
#else
        type (ESMF_Status) :: fieldstatus
        type (ESMF_Status) :: gridstatus
        type (ESMF_Status) :: datastatus
        type (ESMF_GridType), pointer :: gridp
#endif
        type (ESMF_Grid) :: grid             ! save to satisfy query routines
        type (ESMF_LocalField) :: localfield ! this differs per DE
        type (ESMF_DataMap) :: mapping       ! mapping of array indices to grid
        type (ESMF_IOSpec) :: iospec         ! iospec values
        type (ESMF_Status) :: iostatus       ! if unset, inherit from gcomp

      end type

!------------------------------------------------------------------------------
!     ! ESMF_Field
      
!     ! The Field data structure that is passed between implementation and
!     ! calling languages.

      type ESMF_Field
      sequence
      !private       
#if !defined(ESMF_NO_INITIALIZERS) && !defined(ESMF_AIX_8_INITBUG)
        type (ESMF_FieldType), pointer :: ftypep => NULL()
#else
        type (ESMF_FieldType), pointer :: ftypep
#endif
      end type

!------------------------------------------------------------------------------
! !PUBLIC TYPES:
      public ESMF_Field, ESMF_Access
      public ESMF_FieldType, ESMF_LocalField  ! for internal lib use only
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

   public ESMF_FieldGet                ! Generic Get() routine, replaces others

   public ESMF_FieldGetName            ! Get Field name
 
   public ESMF_FieldGetGrid            ! Return a Grid pointer
   public ESMF_FieldGetGlobalGridInfo  ! Return global Grid info
   public ESMF_FieldGetLocalGridInfo   ! Return local Grid info

   public ESMF_FieldGetData            ! Return a data pointer
   public ESMF_FieldGetGlobalDataInfo  ! Return global data info
   public ESMF_FieldGetLocalDataInfo   ! Return local data info
   public ESMF_FieldGetRelLoc          ! Return relative location
 
   public ESMF_FieldGetDataMap         ! Return a pointer to DataMap object

   public ESMF_FieldSetGrid            ! Set a Grid (may regrid if different
                                       !   Grid is already present)
   public ESMF_FieldSetDataValues      ! Set Field data values 
   public ESMF_FieldSetDataMap         ! Set a DataMap (may reorder if different
                                       !   DataMap is already present)


   public ESMF_FieldSetAttribute       ! Set and Get Attributes
   public ESMF_FieldGetAttribute       !   interface to Base class

   public ESMF_FieldValidate           ! Check internal consistency
   public ESMF_FieldPrint              ! Print contents of a Field
   public ESMF_FieldBoxIntersect       ! Intersect bounding boxes

   public ESMF_FieldWrite              ! Write data and grid from a Field

!  !subroutine ESMF_FieldWriteRestart(field, iospec, rc)
!  !function ESMF_FieldReadRestart(name, iospec, rc)
   !subroutine ESMF_FieldWrite(field, subset, iospec, rc)
!  !function ESMF_FieldRead(fname, gname, dnames, iospec, rc)
!
!
!EOPI

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_Field.F90,v 1.100 2004/01/28 21:46:48 nscollins Exp $'

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
!     {\tt ESMF\_Field}.  These method all contain a {\tt ESMF\_Grid} and {\tt ESMF\_Data}.  The variations
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
!     a {\tt ESMF\_Field} without allocating or referencing any associated data.
!     The variations allow a {\tt ESMF\_Grid} to be specified or not, and for
!     the data description to be specified or not.
 
!EOP
      end interface
!
!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_FieldConstruct - Construct the internals of a new Field
!
! !INTERFACE:
      interface ESMF_FieldConstruct
   
! !PRIVATE MEMBER FUNCTIONS:
        module procedure ESMF_FieldConstructNew
        module procedure ESMF_FieldConstructNewArray

! !DESCRIPTION:
!     This interface provides an entry point for methods that construct a 
!     complete {\tt ESMF\_Field}.
 
!EOPI
      end interface
!
!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_FieldConstructNoData - Construct the internals of a new empty Field
!
! !INTERFACE:
      interface ESMF_FieldConstructNoData
   
! !PRIVATE MEMBER FUNCTIONS:
        module procedure ESMF_FieldConstructNoBuffer
        module procedure ESMF_FieldConstructNoArray
        module procedure ESMF_FieldConstructNoGridArray  

! !DESCRIPTION:
!     This interface provides an entry point for {\tt ESMF\_Field} construction 
!     methods that do not allocate or reference any associated data.
 
!EOPI
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
!     data to a {\tt ESMF\_Field}.
 
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
!     data from a {\tt ESMF\_Field}.
 
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
        module procedure ESMF_FieldSetIntAttr
        !module procedure ESMF_FieldSetIntListAttr
        !module procedure ESMF_FieldSetRealAttr
        !module procedure ESMF_FieldSetRealListAttr
        !module procedure ESMF_FieldSetCharAttr

! !DESCRIPTION:
!     This interface provides a single entry point for methods that attach
!     attributes to an {\tt ESMF\_Field}.
 
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
        module procedure ESMF_FieldGetIntAttr
        !module procedure ESMF_FieldGetIntListAttr
        !module procedure ESMF_FieldGetRealAttr
        !module procedure ESMF_FieldGetRealListAttr
        !module procedure ESMF_FieldGetCharAttr

! !DESCRIPTION:
!     This interface provides a single entry point for methods that retrieve
!     attributes from an {\tt ESMF\_Field}.
 
!EOP
      end interface

!
!
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
                                   haloWidth, datamap, name, iospec, rc)
!
! !RETURN VALUE:
      type(ESMF_Field) :: ESMF_FieldCreateNew
!
! !ARGUMENTS:
      type(ESMF_Grid) :: grid               
      type(ESMF_ArraySpec), intent(in) :: arrayspec     
      type(ESMF_DataAllocate), intent(in), optional :: allocflag
      type(ESMF_RelLoc), intent(in), optional :: relloc 
      integer, intent(in), optional :: haloWidth
      type(ESMF_DataMap), intent(in), optional :: datamap          
      character (len=*), intent(in), optional :: name 
      type(ESMF_IOSpec), intent(in), optional :: iospec 
      integer, intent(out), optional :: rc              
!
! !DESCRIPTION:
!     Create a {\tt ESMF\_Field} and allocate space internally for a
!     gridded {\tt ESMF\_Array}.  Return a new {\tt ESMF\_Field}.
! 
!     The arguments are:
!     \begin{description}
!     \item [grid] 
!           Pointer to a {\tt ESMF\_Grid} object. 
!     \item [arrayspec]
!           {\tt ESMF\_Data} specification. 
!     \item [{[allocflag]}]
!           Whether to allocate space for the array.  Default is
!           {\tt ESMF\_DO\_ALLOCATE}.  Other option is {\tt ESMF\_NO\_ALLOCATE}.
!     \item [{[relloc]}] 
!           Relative location of data per grid cell/vertex. 
!     \item [{[haloWidth]}] 
!           Maximum halo depth along all edges.  Default is 0.
!     \item [{[datamap]}]
!           Describes the mapping of data to the {\tt ESMF\_Grid}.
!     \item [{[name]}] 
!           {\tt Field} name. 
!     \item [{[iospec]}] 
!           I/O specification. 
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!           
!     \end{description}
!
!EOP
! !REQUIREMENTS: FLD1.1.1, FLD1.5.1


      type(ESMF_FieldType), pointer :: ftype      ! Pointer to new field
      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present

      ! Initialize pointers
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      nullify(ftype)
      nullify(ESMF_FieldCreateNew%ftypep)

      ! Initialize return code   
      if(present(rc)) then
        rcpresent=.TRUE.
        rc = ESMF_FAILURE
      endif

      allocate(ftype, stat=status)
      ! If error write message and return.
      ! Formal error handling will be added asap.
      if(status .NE. 0) then 
        print *, "ERROR in ESMF_FieldCreateNew: Allocate"
        return
      endif 

      ! Call construction method to allocate and initialize field internals.
      call ESMF_FieldConstructNew(ftype, grid, arrayspec, allocflag, relloc, &
                                  haloWidth, datamap, name, iospec, status)
      if(status .NE. ESMF_SUCCESS) then 
        print *, "ERROR in ESMF_FieldCreateNew: Field construct new asp"
        return
      endif 
   
      ! Set return values.
      ESMF_FieldCreateNew%ftypep => ftype
      if(rcpresent) rc = ESMF_SUCCESS

      end function ESMF_FieldCreateNew

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_FieldCreateFromArray - Create a Field from an existing ESMF Array

! !INTERFACE:
      function ESMF_FieldCreateFromArray(grid, array, copyflag, relloc, &
                                         haloWidth, datamap, name, iospec, rc)
!
! !RETURN VALUE:
      type(ESMF_Field) :: ESMF_FieldCreateFromArray    
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: grid                
      type(ESMF_Array), intent(in) :: array              
      type(ESMF_CopyFlag), intent(in), optional :: copyflag       
      type(ESMF_RelLoc), intent(in), optional :: relloc
      integer, intent(in), optional :: haloWidth
      type(ESMF_DataMap), intent(in), optional :: datamap           
      character (len = *), intent(in), optional :: name   
      type(ESMF_IOSpec), intent(in), optional :: iospec   
      integer, intent(out), optional :: rc                
!
! !DESCRIPTION:
!     This version of creation assumes the data exists already and is being
!     passed in through an {\tt ESMF\_Array}.  
! 
!     \begin{description}
!     \item [grid] 
!           Pointer to a {\tt ESMF\_Grid} object. 
!     \item [array]
!           Includes data specification and allocated memory. 
!     \item [{[copyflag]}]
!           Indicates whether to reference the array or make a 
!           copy of it.  Valid values are {\tt ESMF\_DATA\_COPY} and 
!           {\tt ESMF\_DATA\_REF}, respectively.
!     \item [{[relloc]}] 
!           Relative location of data per grid cell/vertex. 
!     \item [{[haloWidth]}] 
!           Maximum halo depth along all edges.  Default is 0.
!     \item [{[datamap]}]
!           Describes the mapping of data to the {\tt ESMF\_Grid}.
!     \item [{[name]}] 
!           {\tt Field} name. 
!     \item [{[iospec]}] 
!           I/O specification. 
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!           
!     \end{description}
!
!EOP
! !REQUIREMENTS: FLD1.1.2, FLD1.5.1


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
                                       haloWidth, datamap, name, iospec, status)
      if(status .NE. ESMF_SUCCESS) then 
        print *, "ERROR in ESMF_FieldCreateNew: Field construct NewArray"
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
                                        haloWidth, datamap, name, iospec, rc)
!
! !RETURN VALUE:
      type(ESMF_Field) :: ESMF_FieldCreateNoBuffer   
!
! !ARGUMENTS:
      type(ESMF_Grid) :: grid                 
      type(ESMF_ArraySpec), intent(in) :: arrayspec    
      type(ESMF_RelLoc), intent(in), optional :: relloc
      integer, intent(in), optional :: haloWidth
      type(ESMF_DataMap), intent(in), optional :: datamap    
      character (len=*), intent(in), optional :: name    
      type(ESMF_IOSpec), intent(in), optional :: iospec  
      integer, intent(out), optional :: rc               
!
! !DESCRIPTION:
!     Creates a {\tt ESMF\_Field} in its entirety except for the assignment
!     or allocation of an associated raw data buffer.
!
!     \begin{description}
!     \item [grid] 
!           Pointer to a {\tt ESMF\_Grid} object. 
!     \item [arrayspec]
!           Data specification. 
!     \item [{[relloc]}] 
!           Relative location of data per grid cell/vertex. 
!     \item [{[haloWidth]}] 
!           Maximum halo depth along all edges.  Default is 0.
!     \item [{[datamap]}]
!           Describes the mapping of data to the {\tt ESMF\_Grid}.
!     \item [{[name]}] 
!           {\tt Field} name. 
!     \item [{[iospec]}] 
!           I/O specification. 
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!           
!     \end{description}
!
!EOP
! !REQUIREMENTS: FLD1.1.3, FLD1.5.1


      type(ESMF_FieldType), pointer :: ftype      ! Pointer to new field
      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present
   
      ! Initialize pointers
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      nullify(ftype)
      nullify(ESMF_FieldCreateNoBuffer%ftypep)

      ! Initialize return code   
      if(present(rc)) then
        rcpresent = .TRUE. 
        rc = ESMF_FAILURE
      endif     

      allocate(ftype, stat=status)
      ! If error write message and return.
      ! Formal error handling will be added asap.
      if(status .NE. 0) then 
        print *, "ERROR in FieldCreateNoBuffer: Allocate"
        return
      endif 

      ! Call construction method to build field internals.
      call ESMF_FieldConstructNoBuffer(ftype, grid, arrayspec, relloc, &
                                       haloWidth, datamap, name, iospec, status) 
      if(status .NE. ESMF_SUCCESS) then 
        print *, "ERROR in FieldCreateNoBuffer: Field construct NoBuf"
        return
      endif 

      ! Set return values.
      ESMF_FieldCreateNoBuffer%ftypep => ftype
      if(rcpresent) rc = ESMF_SUCCESS

      end function ESMF_FieldCreateNoBuffer

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_FieldCreateNoArray - Create a Field with no associated Array object

! !INTERFACE:
      function ESMF_FieldCreateNoArray(grid, relloc, haloWidth, datamap, &
                                           name, iospec, rc)
!
! !RETURN VALUE:
      type(ESMF_Field) :: ESMF_FieldCreateNoArray 
!
! !ARGUMENTS:
      type(ESMF_Grid) :: grid                 
      type(ESMF_RelLoc), intent(in), optional :: relloc
      integer, intent(in), optional :: haloWidth
      type(ESMF_DataMap), intent(in), optional :: datamap              
      character (len=*), intent(in), optional :: name    
      type(ESMF_IOSpec), intent(in), optional :: iospec  
      integer, intent(out), optional :: rc               
!
! !DESCRIPTION:
!     This version of {\tt ESMF\_FieldCreate} builds a {\tt ESMF\_Field} 
!     and depends on a later call to add an {\tt ESMF\_Array} to it.  
!
!     \begin{description}
!     \item [grid] 
!           Pointer to a {\tt ESMF\_Grid} object. 
!     \item [{[relloc]}] 
!           Relative location of data per grid cell/vertex. 
!     \item [{[haloWidth]}] 
!           Maximum halo depth along all edges.  Default is 0.
!     \item [{[datamap]}]
!           Describes the mapping of data to the {\tt ESMF\_Grid}.
!     \item [{[name]}] 
!           {\tt Field} name. 
!     \item [{[iospec]}] 
!           I/O specification. 
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!           
!     \end{description}
!
!EOP
! !REQUIREMENTS: FLD1.1.3, FLD1.5.1


      type(ESMF_FieldType), pointer :: ftype  ! Pointer to new field
      integer :: status                       ! Error status
      logical :: rcpresent                    ! Return code present
      
      ! Initialize pointers
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      nullify(ftype)
      nullify(ESMF_FieldCreateNoArray%ftypep)

      ! Initialize return code   
      if(present(rc)) then
        rcpresent = .TRUE. 
        rc = ESMF_FAILURE
      endif     

      allocate(ftype, stat=status)
      ! If error write message and return.
      ! Formal error handling will be added asap.
      if(status .NE. 0) then 
        print *, "ERROR in FieldCreateNoArray: Allocate"
        return
      endif 

      ! Call field construction method
      call ESMF_FieldConstructNoArray(ftype, grid, relloc, haloWidth, &
                                       datamap, name, iospec, status)
      if(status .ne. ESMF_SUCCESS) then 
        print *, "ERROR in ESMF_FieldCreateNoArray: Construct"
        return
      endif 

      ! Set return values.
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
!     This version of {\tt ESMF\_FieldCreate} builds an empty {\tt ESMF\_Field} 
!     and depends on later calls to add a {\tt ESMF\_Grid} and {\tt ESMF\_Array} to 
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
!EOP
! !REQUIREMENTS: FLD1.1.3, FLD1.5.1


      type(ESMF_FieldType), pointer :: ftype  ! Pointer to new field
      integer :: status                       ! Error status
      logical :: rcpresent                    ! Return code present
      
      ! Initialize pointers
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      nullify(ftype)
      nullify(ESMF_FieldCreateNoGridArray%ftypep)

      ! Initialize return code   
      if(present(rc)) then
        rcpresent = .TRUE. 
        rc = ESMF_FAILURE
      endif     

      allocate(ftype, stat=status)
      ! If error write message and return.
      ! Formal error handling will be added asap.
      if(status .NE. 0) then 
        print *, "ERROR in ESMF_FieldCreateNoGridArray: Allocate"
        return
      endif 

      ! Call field construction method
      call ESMF_FieldConstructNoGridArray(ftype, name, iospec, status)
      if(status .ne. ESMF_SUCCESS) then 
        print *, "ERROR in ESMF_FieldCreateNoGridArray: Construct"
        return
      endif 

      ! Set return values.
      ESMF_FieldCreateNoGridArray%ftypep => ftype
      if(rcpresent) rc = ESMF_SUCCESS

      end function ESMF_FieldCreateNoGridArray

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_FieldCreateRemap - Create a Field by remapping another Field

! !INTERFACE:
      function ESMF_FieldCreateRemap(srcfield, grid, relloc, haloWidth, &
                                      datamap, name, iospec, rc)
!
! !RETURN VALUE:
      type(ESMF_Field) :: ESMF_FieldCreateRemap
!
! !ARGUMENTS:
      type(ESMF_Field), intent(in) :: srcfield            
      type(ESMF_Grid), intent(in) :: grid                 
      type(ESMF_RelLoc), intent(in), optional :: relloc
      integer, intent(in), optional :: haloWidth
      type(ESMF_DataMap), intent(in), optional :: datamap              
      character (len = *), intent(in), optional :: name   
      type(ESMF_IOSpec), intent(in), optional :: iospec   
      integer, intent(out), optional :: rc                
!
! !DESCRIPTION:
!
! Remaps data between an existing {\tt ESMF\_Grid} on a source {\tt ESMF\_Field}
! and a new {\tt ESMF\_Grid}.  The {\tt ESMF\_Grid} is referenced by the 
! new {\tt ESMF\_Field}.  Data is copied.
!
!EOP
! !REQUIREMENTS: FLD1.1.5, FLD1.5.1, FLD1.6.1


      type(ESMF_FieldType), pointer :: ftype  ! Pointer to new field
      integer :: status                       ! Error status
      logical :: rcpresent                    ! Return code present
      
      ! Initialize pointers
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      nullify(ftype)
      nullify(ESMF_FieldCreateRemap%ftypep)

      ! Initialize return code   
      if(present(rc)) then
        rcpresent = .TRUE. 
        rc = ESMF_FAILURE
      endif     

      allocate(ftype, stat=status)
      ! If error write message and return.
      ! Formal error handling will be added asap.
      if(status .NE. 0) then 
        print *, "ERROR in ESMF_FieldCreateRemap: Allocate"
        return
      endif 

      ! TODO: Insert field construction method

      ! Set return values.
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
!     Releases all resources associated with the {\tt ESMF\_Field}.
! 
!     \begin{description}
!     \item [field]
!           Pointer to a {\tt ESMF\_Field} object.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!           
!     \end{description}
!
!EOP
! !REQUIREMENTS: FLD1.4

      ! Local variables
      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present

      ! Initialize return code   
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent = .TRUE. 
        rc = ESMF_FAILURE
      endif     

      ! If already destroyed or never created, return ok
      if (.not. associated(field%ftypep)) then
        print *, "FieldDestroy called on uninitialized or destroyed Field"
        if(rcpresent) rc = ESMF_FAILURE   ! should this really be an error?
        return
      endif

      ! Destruct all field internals and then free field memory.
      call ESMF_FieldDestruct(field%ftypep, status)
      ! If error write message and return.
      ! Formal error handling will be added asap.
      if(status .ne. ESMF_SUCCESS) then 
        print *, "ERROR in ESMF_FieldDestroy from ESMF_FieldDestruct"
        return
      endif 
           
      if (associated(field%ftypep)) deallocate(field%ftypep, stat=status)
      ! If error write message and return.
      ! Formal error handling will be added asap.
      if(status .ne. 0) then 
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
!BOPI
! !IROUTINE: ESMF_FieldConstructNew - Construct the internals of a Field

! !INTERFACE:
      subroutine ESMF_FieldConstructNew(ftype, grid, arrayspec, allocflag, &
                                 relloc, haloWidth, datamap, name, iospec, rc)
!
! !ARGUMENTS:
      type(ESMF_FieldType), pointer :: ftype 
      type(ESMF_Grid) :: grid               
      type(ESMF_ArraySpec), intent(in) :: arrayspec     
      type(ESMF_DataAllocate), intent(in), optional :: allocflag
      type(ESMF_RelLoc), intent(in), optional :: relloc
      integer, intent(in), optional :: haloWidth
      type(ESMF_DataMap), intent(in), optional :: datamap           
      character (len=*), intent(in), optional :: name
      type(ESMF_IOSpec), intent(in), optional :: iospec 
      integer, intent(out), optional :: rc              
!
! !DESCRIPTION:
! 
!     Constructs all {\tt ESMF\_Field} internals, including the allocation
!     of a data {\tt ESMF\_Array}.  
!
!     \begin{description}
!     \item [ftype]
!           Pointer to a {\tt ESMF\_Field} object.
!     \item [grid] 
!           Pointer to a {\tt ESMF\_Grid} object. 
!     \item [arrayspec]
!           Data specification. 
!     \item [{[allocflag]}]
!           Set to allocate space for data array.  Default is
!           {\tt ESMF\_DO\_ALLOCATE}.  Other option is {\tt ESMF\_NO\_ALLOCATE}.
!     \item [{[relloc]}] 
!           Relative location of data per grid cell/vertex. 
!     \item [{[haloWidth]}] 
!           Maximum halo depth along all edges.  Default is 0.
!     \item [{[datamap]}]
!           Describes the mapping of data to the {\tt ESMF\_Grid}.
!     \item [{[name]}] 
!           {\tt ESMF\_Field} name. 
!     \item [{[iospec]}] 
!           I/O specification. 
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!           
!     \end{description}
!
!EOPI
! !REQUIREMENTS: FLD1.1.3, FLD1.5.1


      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present
      type(ESMF_Array) :: array                   ! New array
      type(ESMF_AxisIndex), dimension(ESMF_MAXDIM) :: index
      integer, dimension(ESMF_MAXDIM) :: counts
      integer :: hwidth
      integer :: i, rank, gridRank

      ! Initialize return code   
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent = .TRUE. 
        rc = ESMF_FAILURE
      endif     

      ! make sure hwidth has a value here.
      if (present(haloWidth)) then
          hwidth = haloWidth
      else
          hwidth = 0
      endif

      call ESMF_FieldConstructNoArray(ftype, grid, relloc, hwidth, &
                                      datamap, name, iospec, status)
      if(status .ne. ESMF_SUCCESS) then 
        print *, "ERROR in ESMF_FieldConstructNew: Field construct NoA"
        return
      endif 

      call ESMF_ArraySpecGet(arrayspec, rank=rank, rc=status)
      call ESMF_GridGetDE(grid, globalAIPerDim=index, rc=status)
      if(status .ne. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_GridGetDE"
        return
      endif 

      do i=1, rank
        counts(i) = index(i)%max - index(i)%min + (2*hwidth) + 1
      enddo

      array = ESMF_ArrayCreate(arrayspec, counts, hwidth, rc=status) 
      if(status .NE. ESMF_SUCCESS) then 
        print *, "ERROR in ESMF_FieldConstructNew: Array create"
        return
      endif 

      if (present(datamap)) ftype%mapping = datamap
      
      ftype%localfield%localdata = array
      ftype%datastatus = ESMF_STATE_READY

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldConstructNew

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_FieldConstructNewArray - Construct the internals of a Field

! !INTERFACE:
      subroutine ESMF_FieldConstructNewArray(ftype, grid, array, relloc, &
                                       haloWidth, datamap, name, iospec, rc)
!
! !ARGUMENTS:
      type(ESMF_FieldType), pointer :: ftype 
      type(ESMF_Grid) :: grid               
      type(ESMF_Array), intent(in) :: array     
      type(ESMF_RelLoc), intent(in), optional :: relloc
      integer, intent(in), optional :: haloWidth
      type(ESMF_DataMap), intent(in), optional :: datamap           
      character (len=*), intent(in), optional :: name
      type(ESMF_IOSpec), intent(in), optional :: iospec 
      integer, intent(out), optional :: rc              
!
! !DESCRIPTION:
! 
!     Constructs all {\tt ESMF\_Field} internals, including the allocation
!     of a data {\tt ESMF\_Array}.  
!
!     \begin{description}
!     \item [ftype]
!           Pointer to a {\tt ESMF\_Field} object.
!     \item [grid] 
!           Pointer to a {\tt ESMF\_Grid} object. 
!     \item [array]
!           Data. 
!     \item [{[relloc]}] 
!           Relative location of data per grid cell/vertex. 
!     \item [{[haloWidth]}] 
!           Maximum halo depth along all edges.  Default is 0.
!     \item [{[datamap]}]
!           Describes the mapping of data to the {\tt ESMF\_Grid}.
!     \item [{[name]}] 
!           {\tt ESMF\_Field} name. 
!     \item [{[iospec]}] 
!           I/O specification. 
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!           
!     \end{description}
!
!EOPI
! !REQUIREMENTS: FLD1.1.3, FLD1.5.1


      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present

      ! Initialize return code   
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent = .TRUE. 
        rc = ESMF_FAILURE
      endif     

      call ESMF_FieldConstructNoArray(ftype, grid, relloc, haloWidth, &
                                      datamap, name, iospec, status)
      if(status .ne. ESMF_SUCCESS) then 
        print *, "ERROR in ESMF_FieldConstructNew: Field construct NoA 2"
        return
      endif 

      call ESMF_ArrayValidate(array, "", status)
      if (status .ne. ESMF_SUCCESS) then
        print *, "Error uninitialized or invalid array"
        return
      endif
      ftype%localfield%localdata = array
      !ftype%localfield%datastatus = ESMF_STATE_READY
      ftype%datastatus = ESMF_STATE_READY

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldConstructNewArray

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_FieldConstructNoBuffer - Construct a Field with no associated buffer

! !INTERFACE:
      subroutine ESMF_FieldConstructNoBuffer(ftype, grid, arrayspec, relloc, &
                                     haloWidth, datamap, name, iospec, rc)
!
! !ARGUMENTS:     
      type(ESMF_FieldType), pointer :: ftype                
      type(ESMF_Grid) :: grid               
      type(ESMF_ArraySpec), intent(in) :: arrayspec     
      type(ESMF_RelLoc), intent(in), optional :: relloc
      integer, intent(in), optional :: haloWidth
      type(ESMF_DataMap), intent(in), optional :: datamap 
      character (len=*), intent(in), optional :: name
      type(ESMF_IOSpec), intent(in), optional :: iospec 
      integer, intent(out), optional :: rc              
!
! !DESCRIPTION:
! 
!     Constructs all {\tt ESMF\_Field} internals except for the assignment of 
!     an associated data buffer.
!
!     \begin{description}
!     \item [ftype]
!           Pointer to a {\tt ESMF\_Field} object.
!     \item [grid] 
!           Pointer to a {\tt ESMF\_Grid} object. 
!     \item [arrayspec]
!           Data specification. 
!     \item [{[relloc]}] 
!           Relative location of data per grid cell/vertex. 
!     \item [{[haloWidth]}] 
!           Maximum halo depth along all edges.  Default is 0.
!     \item [{[datamap]}]
!           Describes the mapping of data to the {\tt ESMF\_Grid}.
!     \item [{[name]}] 
!           {\tt ESMF\_Field} name. 
!     \item [{[iospec]}] 
!           I/O specification. 
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!           
!     \end{description}
!
!EOPI
! !REQUIREMENTS: FLD1.1.3, FLD1.5.1


      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present
      integer :: gridRank

      ! Initialize return code   
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent = .TRUE. 
        rc = ESMF_FAILURE
      endif     
 
      ! Construct a default name if one is not given
      call ESMF_BaseCreate(ftype%base, "Field", name, 0, status)
      if(status .ne. ESMF_SUCCESS) then 
        print *, "ERROR in ESMF_FieldConstructNoBuffer: BaseCreate"
        return
      endif 

      ! TODO: Check to see grid is valid first.

      call ESMF_GridValidate(grid, "", status)
      if (status .ne. ESMF_SUCCESS) then
        print *, "Error uninitialized or invalid grid"
        return
      endif
      ftype%grid = grid
      ftype%gridstatus = ESMF_STATE_READY

      call ESMF_GridGet(grid, numDims=gridRank, rc=status)
      if (present(datamap)) then
        ftype%mapping = datamap   ! copy, datamap can be deleted by user afterwards
      else
        if (gridRank .eq. 1) then
          ftype%mapping = ESMF_DataMapCreate(ESMF_IO_I, relloc, gridRank, status)
        else if (gridRank .eq. 2) then
          ftype%mapping = ESMF_DataMapCreate(ESMF_IO_IJ, relloc, gridRank, status)
        else if (gridRank .eq. 3) then
          ftype%mapping = ESMF_DataMapCreate(ESMF_IO_IJK, relloc, gridRank, status)
        endif
      endif

!     call ESMF_ArrayConstructNoBuffer(ftype%array)

      ! If I/O spec is present, copy it into the field object; otherwise just 
      ! initialize the I/O spec in the field object.
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

      ftype%fieldstatus = ESMF_STATE_READY

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldConstructNoBuffer

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_FieldConstructNoArray - Construct a Field with no associated Array

! !INTERFACE:
      subroutine ESMF_FieldConstructNoArray(ftype, grid, relloc, haloWidth, &
                                            datamap, name, iospec, rc)
!
! !ARGUMENTS:     
      type(ESMF_FieldType), pointer :: ftype   
      type(ESMF_Grid) :: grid                 
      type(ESMF_RelLoc), intent(in), optional :: relloc          
      integer, intent(in), optional :: haloWidth
      type(ESMF_DataMap), intent(in), optional :: datamap              
      character (len=*), intent(in), optional :: name    
      type(ESMF_IOSpec), intent(in), optional :: iospec  
      integer, intent(out), optional :: rc               
!
! !DESCRIPTION:
! 
! Constructs a {\tt ESMF\_Field} except for its internal data {\tt ESMF\_Array}.
!
!     \begin{description}
!     \item [ftype]
!           Pointer to a {\tt ESMF\_Field} object.
!     \item [grid] 
!           Pointer to a {\tt ESMF\_Grid} object. 
!     \item [{[relloc]}] 
!           Relative location of data per grid cell/vertex. 
!     \item [{[haloWidth]}] 
!           Maximum halo depth along all edges.  Default is 0.
!     \item [{[datamap]}]
!           Describes the mapping of data to the {\tt ESMF\_Grid}.
!     \item [{[name]}] 
!           {\tt ESMF\_Field} name. 
!     \item [{[iospec]}] 
!           I/O specification. 
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!           
!     \end{description}
!
!EOPI
! !REQUIREMENTS: FLD1.1.3, FLD1.5.1


      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present
      integer :: gridRank

      ! Initialize return code
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent=.TRUE.
        rc = ESMF_FAILURE
      endif

      ! Construct a default name if one is not given
      call ESMF_BaseCreate(ftype%base, "Field", name, 0, status)
      if(status .ne. ESMF_SUCCESS) then 
        print *, "ERROR in ESMF_FieldConstructNoArray: BaseCreate"
        return
      endif 

      ! Attach grid
      call ESMF_GridValidate(grid, "", status)
      if (status .ne. ESMF_SUCCESS) then
        print *, "Error uninitialized or invalid grid"
        return
      endif
      ftype%grid = grid
      ftype%gridstatus = ESMF_STATE_READY

      call ESMF_GridGet(grid, numDims=gridRank, rc=status)
      if (present(datamap)) then
        ftype%mapping = datamap   ! copy, datamap can be deleted by user afterwards
      else
        if (gridRank .eq. 1) then
          ftype%mapping = ESMF_DataMapCreate(ESMF_IO_I, relloc, gridRank, status)
        else if (gridRank .eq. 2) then
          ftype%mapping = ESMF_DataMapCreate(ESMF_IO_IJ, relloc, gridRank, status)
        else if (gridRank .eq. 3) then
          ftype%mapping = ESMF_DataMapCreate(ESMF_IO_IJK, relloc, gridRank, status)
        endif
      endif

!
! add more code here
!
     
      ftype%fieldstatus = ESMF_STATE_READY

      if (rcpresent) rc = ESMF_SUCCESS
      
      end subroutine ESMF_FieldConstructNoArray

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_FieldConstructNoGridArray - Construct a Field with no Grid or Array
!
! !INTERFACE:
      subroutine ESMF_FieldConstructNoGridArray(ftypep, name, iospec, rc)
!
! !ARGUMENTS:     
      type(ESMF_FieldType), pointer :: ftypep
      character (len = *), intent(in), optional :: name  
      type(ESMF_IOSpec), intent(in), optional :: iospec  
      integer, intent(out), optional :: rc               
!
! !DESCRIPTION:
! 
!     Constructs {\tt ESMF\_Field} internals except those related to {\tt ESMF\_Grid} 
!     and {\tt ESMF\_Data}.
!
!EOPI
! !REQUIREMENTS: FLD1.1.3, FLD1.5.1


      ! Local variables
      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present

      ! Initialize return code
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent=.TRUE.
        rc = ESMF_FAILURE
      endif

      ! Construct a default name if one is not given
      call ESMF_BaseCreate(ftypep%base, "Field", name, 0, status)
      if(status .ne. ESMF_SUCCESS) then 
        print *, "ERROR in ESMF_FieldConstructNoGridArray: BaseCreate"
        return
      endif 

      ! Initialize field contents
      !ftypep%localfield%gridstatus = ESMF_STATE_UNINIT
      !ftypep%localfield%datastatus = ESMF_STATE_UNINIT
      ftypep%gridstatus = ESMF_STATE_UNINIT
      ftypep%datastatus = ESMF_STATE_UNINIT

      ! Set the mapping as unknown/invalid
      call ESMF_DataMapSetInvalid(ftypep%mapping, status)

      ftypep%fieldstatus = ESMF_STATE_READY

!
! add more code here
!
     
      if (rcpresent) rc = ESMF_SUCCESS
      
      end subroutine ESMF_FieldConstructNoGridArray

!------------------------------------------------------------------------------
!BOPI
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
!     Releases all resources except the {\tt ESMF\_Field} itself.
!
!     \begin{description}
!     \item [ftype]
!           Pointer to a {\tt ESMF\_Field} object.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!           
!     \end{description}
!
!EOPI
! !REQUIREMENTS: 

      integer :: status
      logical :: rcpresent                          ! Return code present

      ! Initialize return code; assume failure until success is certain
      rcpresent = .FALSE.
      if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
      endif


      print *, "Field Destruct called"

      ! release the base class resources
      call ESMF_BaseDestroy(ftype%base, status)
      if(status .ne. ESMF_SUCCESS) then 
        print *, "ERROR in ESMF_FieldDestruct: BaseDestroy failed"
        return
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
!     Associates a data buffer with a {\tt ESMF\_Field} and sets a flag in 
!     the {\tt ESMF\_Field} indicating that data is present.  
!
!EOP
! !REQUIREMENTS: FLD1.6.5


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
! Associates an {\tt ESMF\_Array} with a {\tt ESMF\_Field} and sets a 
!  flag in the {\tt ESMF\_Field} indicating that data is present.
!
!EOP
! !REQUIREMENTS: FLD1.6.5


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
!     Associates an {\tt ESMF\_Array} and a {\tt ESMF\_Grid} with a 
!     {\tt ESMF\_Field} and sets a flag in 
!     the {\tt ESMF\_Field} indicating that data is present.
!
!EOP
! !REQUIREMENTS: FLD1.6.5


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
!     Returns a pointer to the {\tt ESMF\_Field}'s data buffer and marks the 
!     {\tt ESMF\_Field} as not having any associated data.

!EOP
! !REQUIREMENTS: FLD1.6.5


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
!     Returns a pointer to the {\tt ESMF\_Field}'s {\tt ESMF\_Array} and marks the 
!     {\tt ESMF\_Field} as not having any associated data.
!
!EOP
! !REQUIREMENTS: FLD1.6.5


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
! !IROUTINE: ESMF_FieldGet - Return info associated with a Field
!
! !INTERFACE:
      subroutine ESMF_FieldGet(field, grid, array, datamap, relloc, name, rc)
!
! !ARGUMENTS:
      type(ESMF_Field), intent(in) :: field    
      type(ESMF_Grid), intent(out), optional :: grid     
      type(ESMF_Array), intent(out), optional :: array     
      type(ESMF_DataMap), intent(out), optional :: datamap     
      type(ESMF_RelLoc), intent(out), optional :: relloc
      character(len=*), intent(out), optional :: name
      integer, intent(out), optional :: rc     
!
! !DESCRIPTION:
!      Query a {\tt ESMF\_Field} for various things.  All arguments after
!      the {\tt Field} are optional.  To select individual items use the
!      named\_argument=value syntax.
!
!EOP
! !REQUIREMENTS: FLD1.6.2

        type(ESMF_FieldType), pointer :: ftype
        integer :: status

        ! assume failure
        if (present(rc)) rc = ESMF_FAILURE

        ! Minimal error checking
        if (.not.associated(field%ftypep)) then
          print *, "ERROR: Invalid or Destroyed Field"
          return
        endif
 
        ftype => field%ftypep
        if (ftype%fieldstatus .ne. ESMF_STATE_READY) then
          print *, "ERROR: Field not ready"
          return
        endif

        if (present(grid)) then
            if (ftype%gridstatus .ne. ESMF_STATE_READY) then
              print *, "ERROR: No grid attached to Field"
              return
            endif
            grid = ftype%grid
        endif

        if (present(array)) then
            if (ftype%datastatus .ne. ESMF_STATE_READY) then
              print *, "ERROR: No data attached to Field"
              return
            endif
            array = ftype%localfield%localdata
        endif

        if (present(datamap)) then
            ! TODO: what's the proper test here?  you could have a map w/ no data yet
            !if (ftype%datastatus .ne. ESMF_STATE_READY) then
            !  print *, "ERROR: No data attached to Field"
            !  return
            !endif
            datamap = ftype%mapping
        endif

        if (present(relloc)) then
            ! TODO: what's the proper test here?  ditto code above.
            !if (ftype%datastatus .ne. ESMF_STATE_READY) then
            !  print *, "ERROR: No data attached to Field"
            !  return
            !endif
            call ESMF_DataMapGet(ftype%mapping, relloc=relloc, rc=status)
            if (status .ne. ESMF_SUCCESS) then
                print *, "ERROR in getting RelLoc in ESMF_DataMapGet"
                rc = status
                return
            endif
        endif

        if (present(name)) then
            call ESMF_GetName(ftype%base, name, status)
            if (status .ne. ESMF_SUCCESS) then
                print *, "ERROR in getting Field name in ESMF_FieldGet"
                rc = status
                return
            endif
        endif

        if (present(rc)) rc = ESMF_SUCCESS

        end subroutine ESMF_FieldGet

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
!EOP
! !REQUIREMENTS: FLD1.5.1, FLD1.7.1

      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present

      ! Initialize return code; assume failure until success is certain
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
      endif

      ! Minimal error checking 
      if (.not.associated(field%ftypep)) then
        print *, "Invalid or Destroyed Field"
        if (present(rc)) rc = ESMF_FAILURE
        return
      endif

      if (field%ftypep%fieldstatus .ne. ESMF_STATE_READY) then
        print *, "Field not ready"
        if (present(rc)) rc = ESMF_FAILURE
        return
      endif

      call ESMF_GetName(field%ftypep%base, name, status)
      if(status .ne. ESMF_SUCCESS) then 
        print *, "ERROR in ESMF_FieldGetName"
        return
      endif 

      if (rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldGetName

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_FieldGetIntAttr - Retrieve an Attribute from a Field
!
! !INTERFACE:
      subroutine ESMF_FieldGetIntAttr(field, name, value, rc)
!
! !ARGUMENTS:
      type(ESMF_Field), intent(in) :: field  
      character (len = *), intent(in) :: name
      integer, intent(out) :: value
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Returns an integer attribute from a {\tt ESMF\_Field}.
!
! 
!     \begin{description}
!     \item [field]
!           A {\tt ESMF\_Field} object.
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
!EOP
! !REQUIREMENTS: FLD1.5.1, FLD1.7.1

      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present

      ! Initialize return code; assume failure until success is certain
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
      endif

      call c_ESMC_AttributeGetInt(field%ftypep%base, name, value, status)
      if(status .ne. ESMF_SUCCESS) then 
        print *, "ERROR in ESMF_FieldGetAttribute"
        return
      endif 

      if (rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldGetIntAttr

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
!      Returns a reference to the {\t ESMF\_Grid} associated with this {\tt ESMF\_Field}.
!
!EOP
! !REQUIREMENTS: FLD1.6.2


        ! assume failure
        if (present(rc)) rc = ESMF_FAILURE

        ! Minimal error checking
        if (.not.associated(field%ftypep)) then
          print *, "ERROR: Invalid or Destroyed Field"
          return
        endif
 
        if (field%ftypep%fieldstatus .ne. ESMF_STATE_READY) then
          print *, "ERROR: Field not ready"
          return
        endif

        if (field%ftypep%gridstatus .ne. ESMF_STATE_READY) then
          print *, "ERROR: No grid attached to Field"
          return
        endif

        grid = field%ftypep%grid

        if (present(rc)) rc = ESMF_SUCCESS

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
!     Return global {\tt ESMF\_Grid} information. 
!
!EOP
! !REQUIREMENTS: FLD1.7.2


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
!      Get {\tt ESMF\_Grid} information specific to the local {\tt ESMF\_DE}.
!
!EOP
! !REQUIREMENTS: FLD1.7.2


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
!     Get data either in {\tt ESMF\_Array} or buffer form.

!
!EOP
! !REQUIREMENTS: FLD1.3, FLD1.6.4 (pri 2?), FLD1.7.2


!
! TODO: code goes here
!
      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present
      logical :: apresent                         ! Array present
      logical :: bpresent                         ! Buffer present
      character(len=ESMF_MAXSTR) :: str
      type(ESMF_FieldType), pointer :: ftypep

      ! Initialize return code   
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      apresent = .FALSE.
      bpresent = .FALSE.
      if(present(rc)) then
        rcpresent=.TRUE.
        rc = ESMF_FAILURE
      endif

      ! Minimal error checking 
      if (.not.associated(field%ftypep)) then
        print *, "ESMF_FieldGetData: Invalid or Destroyed Field"
        return
      endif

      ftypep => field%ftypep

      if (ftypep%fieldstatus .ne. ESMF_STATE_READY) then
        print *, "ESMF_FieldGetData: Field not ready"
        return
      endif

      ! Set codes depending on what the caller specified
      if(present(array)) apresent=.TRUE.
      if(present(buffer)) bpresent=.TRUE.

      if(apresent) then
          if (ftypep%datastatus .ne. ESMF_STATE_READY) then
              print *, "ESMF_FieldGetData: no data associated with field"
              return
          endif

          !call ESMF_StatusString(ftypep%datastatus, str, rc)
          !print *, "getting array data, status = ", trim(str)
          array = ftypep%localfield%localdata
      endif 
   
      if(bpresent) then
          ! TODO: check that an array is associated with the field
          !  if (field%ptr%localfield%datastatus .eq. ...)

          ! TODO: and extract data also
          ! array = field%ptr%localfield%localdata
      endif 

      ! Set return values.
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
!     Retrieve global {\tt ESMF\_Field} data information.

!
!EOP
! !REQUIREMENTS: FLD1.3, FLD1.6.4 (pri 2?), FLD1.7.2


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
!     Retrieve {\tt ESMF\_Field} data information specific to the local
!     {\tt ESMF\_DE}.
!
!EOP
! !REQUIREMENTS: FLD1.3, FLD1.6.4 (pri 2?), FLD1.7.2


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


!EOP
! !REQUIREMENTS: FLD1.2, FLD1.6.3 (pri 2?)



        ! assume failure
        if (present(rc)) rc = ESMF_FAILURE

        ! Minimal error checking
        if (.not.associated(field%ftypep)) then
          print *, "ERROR: Invalid or Destroyed Field"
          return
        endif
 
        if (field%ftypep%fieldstatus .ne. ESMF_STATE_READY) then
          print *, "ERROR: Field not ready"
          return
        endif

        if (field%ftypep%datastatus .ne. ESMF_STATE_READY) then
          print *, "ERROR: No data attached to Field"
          return
        endif

        datamap = field%ftypep%mapping

        if (present(rc)) rc = ESMF_SUCCESS

        end subroutine ESMF_FieldGetDataMap

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
!      Used only with the version of {\tt ESMF\_FieldCreate} which creates an empty 
!      {\tt ESMF\_Field} and allows the {\tt ESMF\_Grid} to be specified later.  Otherwise it is 
!      an error to try to change the {\tt ESMF\_Grid} associated with a {\tt ESMF\_Field}.
!
!EOP
! !REQUIREMENTS: FLD1.1.3


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
!      Allows specified data values associated with a {\tt ESMF\_Field} to be set 
!      through the {\tt ESMF\_Field} interface instead of detaching data and setting 
!      it outside the framework.
!
!EOP
! !REQUIREMENTS: FLD1.6.7


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
!     Used to set the ordering of a {\tt ESMF\_Field}.  If an initialized {\tt ESMF\_DataMap}
!     and associated data are already in the {\tt ESMF\_Field}, the data will be 
!     reordered according to the new speciification.
!
!EOP
! !REQUIREMENTS: FLD1.2


!
! TODO: code goes here
!
        end subroutine ESMF_FieldSetDataMap

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_FieldSetIntAttr - Set an Attribute on a Field
!
! !INTERFACE:
      subroutine ESMF_FieldSetIntAttr(field, name, value, rc)
!
! !ARGUMENTS:
      type(ESMF_Field), intent(in) :: field  
      character (len = *), intent(in) :: name
      integer, intent(in) :: value
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Attaches an integer attribute to a {\tt ESMF\_Field}.
!
! 
!     \begin{description}
!     \item [field]
!           A {\tt ESMF\_Field} object.
!     \item [name]
!           The name of the Attribute to set.
!     \item [value]
!           The integer value of the Attribute.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!           
!     \end{description}
!
!
!EOP
! !REQUIREMENTS: FLD1.5.1, FLD1.7.1

      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present

!     Initialize return code; assume failure until success is certain
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
      endif

      call c_ESMC_AttributeSetInt(field%ftypep%base, name, value, status)
      if(status .ne. ESMF_SUCCESS) then 
        print *, "ERROR in ESMF_FieldSetAttribute"
        return
      endif 

      if (rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldSetIntAttr

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
!     Routine to validate the internal state of a {\tt ESMF\_Field}.
!
!EOP
! !REQUIREMENTS:  FLD4.1

      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present

!     Initialize return code; assume failure until success is certain
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
      endif

      if (.not.associated(field%ftypep)) then 
          print *, "Uninitialized or Destroyed Field"
          return
      endif 

      if (field%ftypep%fieldstatus .ne. ESMF_STATE_READY) then
          print *, "Uninitialized or Destroyed Field"
          return
      endif 

      ! TODO: add more code here

      if (rcpresent) rc = ESMF_SUCCESS

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

        character(len=ESMF_MAXSTR) :: name, str
        type(ESMF_FieldType), pointer :: fp 
        integer :: status


        if (present(rc)) rc = ESMF_FAILURE

        print *, "Field Print:"
        if (.not. associated(field%ftypep)) then
          print *, "Empty or Uninitialized Field"
          if (present(rc)) rc = ESMF_SUCCESS
          return
        endif

        fp => field%ftypep
        call ESMF_BasePrint(fp%base, str, status)
        if(status .NE. ESMF_SUCCESS) then 
          if (present(rc)) rc = ESMF_FAILURE
          return
        endif

        call ESMF_StatusString(fp%fieldstatus, str, status)
        print *, "Field status = ", trim(str)

        if (fp%fieldstatus .ne. ESMF_STATE_READY) then
          if (present(rc)) rc = ESMF_FAILURE
          return
        endif

        call ESMF_GetName(fp%base, name, status)
        if(status .NE. ESMF_SUCCESS) then 
          print *, "ERROR in ESMF_FieldGetName"
          return
        endif 
        print *, "  Name = '",  trim(name), "'"

        call ESMF_StatusString(fp%gridstatus, str, status)
        print *, "Grid status = ", trim(str)
        if (fp%gridstatus .eq. ESMF_STATE_READY) then 
           call ESMF_GridPrint(fp%grid, "", status)
        endif

        call ESMF_StatusString(fp%datastatus, str, status)
        print *, "Data status = ", trim(str)
        if (fp%datastatus .eq. ESMF_STATE_READY) then 
           call ESMF_ArrayPrint(fp%localfield%localdata, "", status)
        endif

        call ESMF_DataMapPrint(fp%mapping, "", status)

        !TODO: add code here to print more info

        ! global field contents
        !type (ESMF_IOSpec) :: iospec             ! iospec values
        !type (ESMF_Status) :: iostatus           ! if unset, inherit from gcomp

        ! local field contents
        !type (ESMF_Mask) :: mask                 ! may belong in Grid

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
! !IROUTINE: ESMF_FieldWriteRestart - Save Field in the quickest manner possible
!
! !INTERFACE:
      subroutine ESMF_FieldWriteRestart(field, iospec, rc)
!
! !ARGUMENTS:
      type(ESMF_Field), intent(in) :: field 
      type(ESMF_IOSpec), intent(in), optional :: iospec
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!      Used to save all data to disk as quickly as possible.  
!      (see Read/Write for other options).  Internally this routine uses the
!      same I/O interface as Read/Write, but the default options are to
!      select the fastest way to save data to disk.
!
!EOP
! !REQUIREMENTS: FLD1.6.8


!
! TODO: code goes here
!
        end subroutine ESMF_FieldWriteRestart


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_FieldReadRestart - Read back in a saved Field
!
! !INTERFACE:
      function ESMF_FieldReadRestart(name, iospec, rc)
!
! !RETURN VALUE:
      type(ESMF_Field) :: ESMF_FieldReadRestart
!
!
! !ARGUMENTS:
      character (len = *), intent(in) :: name
      type(ESMF_IOSpec), intent(in), optional :: iospec
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!      Used to reinitialize
!      all data associated with a {\tt ESMF\_Field} from the 
!      last call to WriteRestart.
!
!EOP
! !REQUIREMENTS: FLD1.6.8


!
! TODO: code goes here; this is just filler to make the compiler not complain
!
        type (ESMF_Field) :: a
     
        nullify(a%ftypep)

        ESMF_FieldReadRestart = a

        end function ESMF_FieldReadRestart


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_FieldWrite - Write a Field to external storage
!
! !INTERFACE:
      subroutine ESMF_FieldWrite(field, & ! subset, 
                                 iospec, rc)
!
! !ARGUMENTS:
      type(ESMF_Field), intent(inout) :: field 
!     type(ESMF_Subset), intent(in), optional :: subset
      type(ESMF_IOSpec), intent(in), optional :: iospec
      integer, intent(out), optional :: rc  
!
! !DESCRIPTION:
!      Used to write data to persistent storage in a variety of formats.  
!      (see WriteRestart/ReadRestart for quick data dumps.)  Details of I/O 
!      options specified in the IOSpec derived type. 
!
!
!EOP

        ! Local variables
        integer :: status, de_id
        logical :: rcpresent
        type(ESMF_Array) :: outarray
        type(ESMF_Grid) :: grid
        type(ESMF_DELayout) :: layout
        character(len=ESMF_MAXSTR) :: filename


        ! TODO: revised code goes here


        ! call ESMF_Log(?, 'entry into ESMF_FieldWrite');

        ! Set initial values
        status = ESMF_FAILURE 
        rcpresent = .FALSE.

        ! Initialize return code
        if(present(rc)) then
            rcpresent=.TRUE.
            rc = ESMF_FAILURE      
        endif
           
        ! Get filename out of IOSpec, if specified.  Otherwise use the
        ! name of the Field.
        if (present(IOSpec)) then
            call ESMF_IOSpecGet(IOSpec, filename=filename, rc=status)
        else
            call ESMF_FieldGet(field, name=filename, rc=status)
        endif

        ! Collect results on DE 0 and output to a file
        call ESMF_FieldGet(field, grid=grid, rc=status)
        call ESMF_GridGetDELayout(grid, layout, rc=status)
        call ESMF_DELayoutGetDEID(layout, de_id, status)

        ! Output to file
        call ESMF_ArrayAllGather(field%ftypep%localfield%localdata, &
                                 field%ftypep%grid, field%ftypep%mapping, &
                                 outarray, rc=status)
        !call ESMF_FieldAllGather(field, outarray, rc=status)
        if (de_id .eq. 0) then       
            call ESMF_ArrayWrite(outarray, filename=filename, rc=status)
        endif
        call ESMF_ArrayDestroy(outarray, status)

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
!      This includes creating the {\tt ESMF\_Grid} associated with this {\tt ESMF\_Field}.
!      To share a single {\tt ESMF\_Grid} betwen multiple {\tt ESMF\_Field}s, see the {\tt ESMF\_FieldCreate} calls.
!
!
!EOP
! !REQUIREMENTS: 


!
! TODO: code goes here.  this is filler to keep the compiler from complaining.
!
        type (ESMF_Field) :: a
     
        nullify(a%ftypep)

        ESMF_FieldRead = a


        end function ESMF_FieldRead

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_FieldGetRelLoc - Return Relative location
!
! !INTERFACE:
      subroutine ESMF_FieldGetRelLoc(field, relloc, rc)
!
! !ARGUMENTS:
      type(ESMF_Field), intent(in) :: field             ! field to cover
      type(ESMF_RelLoc), intent(out) :: relloc          ! RelLoc to return
      integer, intent(out), optional :: rc              ! return code
!
! !DESCRIPTION:
!     Finds and returns the relative location of the field. Use
!     DataMap access routines to get relloc.
!
!EOP
! !REQUIREMENTS:


      call ESMF_DataMapGet(field%ftypep%mapping, relloc=relloc, rc=rc)

      end subroutine ESMF_FieldGetRelLoc

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_FieldBoxIntersect - Intersect bounding boxes
!
! !INTERFACE:
      subroutine ESMF_FieldBoxIntersect(src_field, dst_field, recvDomainlist, &
                                        sendDomainList, rc)
!
! !ARGUMENTS:
      type(ESMF_Field), intent(in) :: src_field         ! field to cover
      type(ESMF_Field), intent(in) :: dst_field         ! field to find a cover in
      type(ESMF_DomainList), intent(inout) :: recvDomainlist
                                                        ! receive domain list
      type(ESMF_DomainList), intent(inout) :: sendDomainlist
                                                        ! send domain list
      integer, intent(out), optional :: rc              ! return code
!
! !DESCRIPTION:
!      Clips the src\_field physgrid box against the clip\_field, i.e. returns
!      a description of the area in clip\_field which is necessary to cover the
!      desired area in src\_field.  This procedure is mostly an entry point;
!      most of the work is done in the {\tt ESMF\_Grid} class.
!
!EOP
! !REQUIREMENTS:

      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present
      integer :: my_DE, my_dst_DE, my_src_DE
      real(ESMF_KIND_R8), dimension(ESMF_MAXGRIDDIM) :: dst_min, dst_max
      real(ESMF_KIND_R8), dimension(ESMF_MAXGRIDDIM) :: src_min, src_max
      logical :: hassrcdata        ! does this DE contain localdata from src?
      logical :: hasdstdata        ! does this DE contain localdata from dst?
      type(ESMF_AxisIndex), dimension(ESMF_MAXGRIDDIM) :: myAI
      type(ESMF_DELayout) :: parentlayout, srclayout, dstlayout
      type(ESMF_FieldType) :: stypep, dtypep      ! field type info
      type(ESMF_Grid) :: src_grid, dst_grid
      type(ESMF_Logical) :: hasdata        ! does this DE contain localdata?

      ! Initialize return code
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_FAILURE
      endif

      stypep = src_field%ftypep
      dtypep = dst_field%ftypep

      ! Our DE number in the parent layout  TODO: for now, just use one of the
      !                                           grid layouts.  In the future,
      !                                           there will be proxy grids on all
      !                                           DEs of the coupler layout and a
      !                                           different method for determining
      !                                           if my_DE is part of a layout
      call ESMF_GridGetDELayout(stypep%grid, parentlayout, status)
      call ESMF_DELayoutGetDEID(parentlayout, my_DE, status)

      ! TODO: we need not only to know if this DE has data in the field,
      !   but also the de id for both src & dest fields

      ! This routine is called on every processor in the parent layout.
      !  It is quite possible that the source and destination fields do
      !  not completely cover every processor on that layout.  Make sure
      !  we do not go lower than this on the processors which are uninvolved
      !  in this communication.

      ! if srclayout ^ parentlayout == NULL, nothing to send from this DE id.
      call ESMF_GridGetDELayout(stypep%grid, srclayout, status)
      call ESMF_DELayoutGetDEExists(parentlayout, my_DE, srclayout, hasdata)
      hassrcdata = (hasdata .eq. ESMF_TRUE)
      hassrcdata = .true.   ! temp for now
      if (hassrcdata) then
        ! don't ask for our de number if this de isn't part of the layout
        call ESMF_DELayoutGetDEID(srclayout, my_src_DE, status)
        call ESMF_GridGetDE(stypep%grid, globalAIPerDim=myAI, rc=status)
      endif

      ! if dstlayout ^ parentlayout == NULL, nothing to recv on this DE id.
      call ESMF_GridGetDELayout(dtypep%grid, dstlayout, status)
      call ESMF_DELayoutGetDEExists(parentlayout, my_DE, dstlayout, hasdata)
      hasdstdata = (hasdata .eq. ESMF_TRUE)
      hasdstdata = .true.   ! temp for now
      if (hasdstdata) then
        ! don't ask for our de number if this de isn't part of the layout
        call ESMF_DELayoutGetDEID(dstlayout, my_dst_DE, status)
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
        ! match up the array indices and the grid indices.
        call ESMF_FieldGetGrid(src_field, src_grid, status)
        if(status .NE. ESMF_SUCCESS) then
          print *, "ERROR in FieldBoxIntersect: FieldGetGrid returned failure"
          return
        endif
        ! From the grid get the bounding box on this DE
        call ESMF_GridGet(src_grid, minLocalCoordPerDim=src_min, &
                         maxLocalCoordPerDim=src_max, rc=status)
        call ESMF_GridBoxIntersectSend(dst_grid, src_grid, src_min, src_max, &
                                       myAI, sendDomainList, status)
      endif

      ! if dst field exists on this DE, query it for information
      if (hasdstdata) then
        ! Query the datamap and set info for grid so it knows how to
        ! match up the array indices and the grid indices.
        call ESMF_FieldGetGrid(dst_field, dst_grid, status)
        if(status .NE. ESMF_SUCCESS) then
          print *, "ERROR in FieldBoxIntersect: FieldGetGrid returned failure"
          return
        endif
        ! From the grid get the bounding box on this DE
        call ESMF_GridGet(dst_grid, minLocalCoordPerDim=dst_min, &
                          maxLocalCoordPerDim=dst_max, rc=status)
        call ESMF_GridBoxIntersectRecv(src_grid, dst_min, dst_max, &
                                       recvDomainList, rc=status)
      endif

      if (rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldBoxIntersect


      end module ESMF_FieldMod
