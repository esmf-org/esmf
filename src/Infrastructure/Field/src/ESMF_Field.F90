! $Id: ESMF_Field.F90,v 1.173 2004/07/21 21:20:12 nscollins Exp $
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
#define ESMF_FILENAME "ESMF_Field.F90"
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
! An {\tt ESMF\_FieldDataMap} describes the 
! relationship of the {\tt ESMF\_Array} to the {\tt ESMF\_Grid}.  
!
! This type is implemented in Fortran 90 and a corresponding
! C++ interface is provided for access.
!
!------------------------------------------------------------------------------
! !USES:
      use ESMF_BaseTypesMod    ! ESMF base class
      use ESMF_BaseMod
      use ESMF_VMMod
      use ESMF_LogErrMod
      use ESMF_IOSpecMod
      use ESMF_ArraySpecMod
      use ESMF_LocalArrayMod
      use ESMF_ArrayDataMapMod
      use ESMF_DELayoutMod
      use ESMF_GridTypesMod
      use ESMF_GridMod
      use ESMF_ArrayMod
      use ESMF_ArrayCreateMod
      use ESMF_ArrayGetMod
      use ESMF_ArrayCommMod
      use ESMF_TimeMod
      use ESMF_FieldDataMapMod
      use wrf_data

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
!     ! ESMF_AllocFlag
!
!     ! Interface flag for setting whether Field does the data allocation.

      type ESMF_AllocFlag
      sequence
      private
        integer :: a_type
      end type

      type(ESMF_AllocFlag), parameter ::  &
                               ESMF_ALLOC = ESMF_AllocFlag(0), &
                               ESMF_NO_ALLOC = ESMF_AllocFlag(1)

!------------------------------------------------------------------------------
!     ! ESMF_IndexFlag
!
!     ! Interface flag for setting whether Field data has global index bounds

      type ESMF_IndexFlag
      sequence
      private
        integer :: i_type
      end type

      type(ESMF_IndexFlag), parameter ::  &
                               ESMF_INDEX_DELOCAL  = ESMF_IndexFlag(0), &
                               ESMF_INDEX_GLOBAL = ESMF_IndexFlag(1)

!------------------------------------------------------------------------------
!     ! ESMF_LocalField
!      
!     ! The LocalField class contains information which is associated with the
!     ! local DE.

      type ESMF_LocalField
      sequence
      !private
   
        type (ESMF_Array) :: localdata           ! local data for this DE
        type (ESMF_ArraySpec) :: arrayspec       ! so field can allocate
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
        type (ESMF_Status) :: fieldstatus = ESMF_STATUS_UNINIT
        type (ESMF_Status) :: gridstatus = ESMF_STATUS_UNINIT
        type (ESMF_Status) :: datastatus = ESMF_STATUS_UNINIT
        type (ESMF_GridClass), pointer :: gridp => NULL()  ! for faster access
#else
        type (ESMF_Status) :: fieldstatus
        type (ESMF_Status) :: gridstatus
        type (ESMF_Status) :: datastatus
        type (ESMF_GridClass), pointer :: gridp
#endif
        type (ESMF_Grid) :: grid             ! save to satisfy query routines
        type (ESMF_LocalField) :: localfield ! this differs per DE
        type (ESMF_FieldDataMap) :: mapping  ! mapping of array indices to grid
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
      public ESMF_AllocFlag, ESMF_NO_ALLOC, ESMF_ALLOC
      public ESMF_IndexFlag, ESMF_INDEX_DELOCAL, ESMF_INDEX_GLOBAL

!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:
!

   public ESMF_FieldCreateNoData       ! Create a new Field without data
   public ESMF_FieldDestroy            ! Destroy a Field

   public ESMF_FieldGet                ! Generic Get() routine, replaces others

   public ESMF_FieldGetGlobalGridInfo  ! Return global Grid info
   public ESMF_FieldGetLocalGridInfo   ! Return local Grid info

   public ESMF_FieldGetArray           ! Return the data Array
   public ESMF_FieldGetGlobalDataInfo  ! Return global data info
   public ESMF_FieldGetLocalDataInfo   ! Return local data info

   public ESMF_FieldSetGrid            ! Set a Grid (may regrid if different
                                       !   Grid is already present)
   public ESMF_FieldSetArray           ! Set a data Array in a Field
   public ESMF_FieldSetDataValues      ! Set Field data values 

   public ESMF_FieldSetDataMap         ! Set a DataMap (may reorder if different
                                       !   DataMap is already present)

   public ESMF_FieldSetAttribute       ! Set and Get attributes
   public ESMF_FieldGetAttribute       !  

   public ESMF_FieldGetAttributeCount  ! number of attribs
   public ESMF_FieldGetAttributeInfo   ! get type, length by name or number

   public ESMF_FieldValidate           ! Check internal consistency
   public ESMF_FieldPrint              ! Print contents of a Field
   public ESMF_FieldBoxIntersect       ! Intersect bounding boxes

   public ESMF_FieldWrite              ! Write data and grid from a Field

   public ESMF_FieldConstruct          ! Only public for internal use

!  !subroutine ESMF_FieldWriteRestart(field, iospec, rc)
!  !function ESMF_FieldReadRestart(name, iospec, rc)
!  !subroutine ESMF_FieldWrite(field, subset, iospec, rc)
!  !function ESMF_FieldRead(fname, gname, dnames, iospec, rc)
!
!
!EOPI

! !PRIVATE MEMBER FUNCTIONS:

   private ESMF_FieldWriteFileASCII
!!$   private ESMF_FieldWriteFileNetCDF
!!$        private ESMF_FieldWriteFileNetCDF1DI1
!!$        private ESMF_FieldWriteFileNetCDF2DI1
!!$        private ESMF_FieldWriteFileNetCDF3DI1
!!$        private ESMF_FieldWriteFileNetCDF4DI1
!!$        private ESMF_FieldWriteFileNetCDF5DI1
!!$        private ESMF_FieldWriteFileNetCDF6DI1
!!$        private ESMF_FieldWriteFileNetCDF7DI1
!!$        private ESMF_FieldWriteFileNetCDF1DI2
!!$        private ESMF_FieldWriteFileNetCDF2DI2
!!$        private ESMF_FieldWriteFileNetCDF3DI2
!!$        private ESMF_FieldWriteFileNetCDF4DI2
!!$        private ESMF_FieldWriteFileNetCDF5DI2
!!$        private ESMF_FieldWriteFileNetCDF6DI2
!!$        private ESMF_FieldWriteFileNetCDF7DI2
!!$        private ESMF_FieldWriteFileNetCDF1DI4
!!$        private ESMF_FieldWriteFileNetCDF2DI4
!!$        private ESMF_FieldWriteFileNetCDF3DI4
!!$        private ESMF_FieldWriteFileNetCDF4DI4
!!$        private ESMF_FieldWriteFileNetCDF5DI4
!!$        private ESMF_FieldWriteFileNetCDF6DI4
!!$        private ESMF_FieldWriteFileNetCDF7DI4
!!$        private ESMF_FieldWriteFileNetCDF1DI8
!!$        private ESMF_FieldWriteFileNetCDF2DI8
!!$        private ESMF_FieldWriteFileNetCDF3DI8
!!$        private ESMF_FieldWriteFileNetCDF4DI8
!!$        private ESMF_FieldWriteFileNetCDF5DI8
!!$        private ESMF_FieldWriteFileNetCDF6DI8
!!$        private ESMF_FieldWriteFileNetCDF7DI8
!!$        private ESMF_FieldWriteFileNetCDF1DR4
        private ESMF_FieldWriteFileNetCDF2DR4
        private ESMF_FieldWriteFileNetCDF3DR4
!!$        private ESMF_FieldWriteFileNetCDF4DR4
!!$        private ESMF_FieldWriteFileNetCDF5DR4
!!$        private ESMF_FieldWriteFileNetCDF6DR4
!!$        private ESMF_FieldWriteFileNetCDF7DR4
!!$        private ESMF_FieldWriteFileNetCDF1DR8
!!$        private ESMF_FieldWriteFileNetCDF2DR8
!!$        private ESMF_FieldWriteFileNetCDF3DR8
!!$        private ESMF_FieldWriteFileNetCDF4DR8
!!$        private ESMF_FieldWriteFileNetCDF5DR8
!!$        private ESMF_FieldWriteFileNetCDF6DR8
!!$        private ESMF_FieldWriteFileNetCDF7DR8
   
!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_Field.F90,v 1.173 2004/07/21 21:20:12 nscollins Exp $'

!==============================================================================
!
! INTERFACE BLOCKS
!
!==============================================================================
!
!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_FieldCreateNoData - Create a new Field without data
!
! !INTERFACE:
      interface ESMF_FieldCreateNoData
   
! !PRIVATE MEMBER FUNCTIONS:
        module procedure ESMF_FieldCreateNoDataPtr
        module procedure ESMF_FieldCreateNoArray
        module procedure ESMF_FieldCreateNoGridArray  

! !DESCRIPTION:
!     This interface provides an entry point for methods that create 
!     an {\tt ESMF\_Field} without allocating or referencing any associated data.
!     The variations allow an {\tt ESMF\_Grid} to be specified or not, and for
!     the data description to be specified or not.
 
!EOPI
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
        module procedure ESMF_FieldConstructNoDataPtr
        module procedure ESMF_FieldConstructNoArray
        module procedure ESMF_FieldConstructNoGridArray  

! !DESCRIPTION:
!     This interface provides an entry point for {\tt ESMF\_Field} construction 
!     methods that do not allocate or reference any associated data.
 
!EOPI
      end interface
!
!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_FieldSetAttribute  - Set Field attributes
!
! !INTERFACE:
      interface ESMF_FieldSetAttribute 
   
! !PRIVATE MEMBER FUNCTIONS:
        module procedure ESMF_FieldSetInt4Attr
        module procedure ESMF_FieldSetInt4ListAttr
        module procedure ESMF_FieldSetInt8Attr
        module procedure ESMF_FieldSetInt8ListAttr
        module procedure ESMF_FieldSetReal4Attr
        module procedure ESMF_FieldSetReal4ListAttr
        module procedure ESMF_FieldSetReal8Attr
        module procedure ESMF_FieldSetReal8ListAttr
        module procedure ESMF_FieldSetLogicalAttr
        module procedure ESMF_FieldSetLogicalListAttr
        module procedure ESMF_FieldSetCharAttr

! !DESCRIPTION:
!     This interface provides a single entry point for methods that attach
!     attributes to an {\tt ESMF\_Field}.
 
!EOPI
      end interface
!
!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_FieldGetAttribute  - Get Field attributes
!
! !INTERFACE:
      interface ESMF_FieldGetAttribute 
   
! !PRIVATE MEMBER FUNCTIONS:
        module procedure ESMF_FieldGetInt4Attr
        module procedure ESMF_FieldGetInt4ListAttr
        module procedure ESMF_FieldGetInt8Attr
        module procedure ESMF_FieldGetInt8ListAttr
        module procedure ESMF_FieldGetReal4Attr
        module procedure ESMF_FieldGetReal4ListAttr
        module procedure ESMF_FieldGetReal8Attr
        module procedure ESMF_FieldGetReal8ListAttr
        module procedure ESMF_FieldGetLogicalAttr
        module procedure ESMF_FieldGetLogicalListAttr
        module procedure ESMF_FieldGetCharAttr

! !DESCRIPTION:
!     This interface provides a single entry point for methods that retrieve
!     attributes from an {\tt ESMF\_Field}.
 
!EOPI
      end interface

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_FieldGetAttributeInfo - Get type, count from a Field attribute
!
! !INTERFACE:
      interface ESMF_FieldGetAttributeInfo
   
! !PRIVATE MEMBER FUNCTIONS:
        module procedure ESMF_FieldGetAttrInfoByName
        module procedure ESMF_FieldGetAttrInfoByNum

! !DESCRIPTION:
!     This interface provides a single entry point for methods that retrieve
!     information about attributes from an {\tt ESMF\_Field}.
 
!EOPI
      end interface
!
!
!
!==============================================================================
!
      contains
!
!==============================================================================


#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldCreateNoDataPtr"

!BOP
! !IROUTINE: ESMF_FieldCreateNoData - Create a Field with no associated data buffer

! !INTERFACE:
      ! Private name; call using ESMF_FieldCreateNoData()
      function ESMF_FieldCreateNoDataPtr(grid, arrayspec, horzRelloc, vertRelloc, &
                                        haloWidth, datamap, name, iospec, rc)
!
! !RETURN VALUE:
      type(ESMF_Field) :: ESMF_FieldCreateNoDataPtr   
!
! !ARGUMENTS:
      type(ESMF_Grid) :: grid                 
      type(ESMF_ArraySpec), intent(in) :: arrayspec    
      type(ESMF_RelLoc), intent(in), optional :: horzRelloc 
      type(ESMF_RelLoc), intent(in), optional :: vertRelloc 
      integer, intent(in), optional :: haloWidth
      type(ESMF_FieldDataMap), intent(in), optional :: datamap    
      character (len=*), intent(in), optional :: name    
      type(ESMF_IOSpec), intent(in), optional :: iospec  
      integer, intent(out), optional :: rc               
!
! !DESCRIPTION:
!     An interface function to {\tt ESMF\_FieldCreateNoData()}.
!     Creates an {\tt ESMF\_Field} in its entirety except for the assignment
!     or allocation of an associated raw data buffer.
!
!     The arguments are:
!     \begin{description}
!     \item [grid] 
!           Pointer to an {\tt ESMF\_Grid} object. 
!     \item [arrayspec]
!           Data specification. 
!     \item [{[horzRelloc]}] 
!           Relative location of data per grid cell/vertex in the horizontal
!           grid.
!     \item [{[vertRelloc]}] 
!           Relative location of data per grid cell/vertex in the vertical grid.
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
!     \end{description}
!
!EOP


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
      if (ESMF_LogMsgFoundAllocError(status, "Allocating Field information", &
                                       ESMF_CONTEXT, rc)) return

      ! Call field construction method
      call ESMF_FieldConstructNoArray(ftype, grid, horzRelloc, vertRelloc, &
                                      haloWidth, datamap, name, iospec, status)
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      ! Set return values.
      ESMF_FieldCreateNoArray%ftypep => ftype
      if(rcpresent) rc = ESMF_SUCCESS

      end function ESMF_FieldCreateNoArray

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldCreateNoGridArray"

!BOP
! !IROUTINE: ESMF_FieldCreateNoData - Create a Field with no Grid or Array

! !INTERFACE:
      ! Private name; call using ESMF_FieldCreateNoData()
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
!     An interface function to {\tt ESMF\_FieldCreateNoData()}.
!     This version of {\tt ESMF\_FieldCreate} builds an empty {\tt ESMF\_Field} 
!     and depends on later calls to add an {\tt ESMF\_Grid} and {\tt ESMF\_Array} to 
!     it.  
!
!     The arguments are:
!     \begin{description}
!     \item [{[name]}] 
!           {\tt Field} name. 
!     \item [{[iospec]}] 
!           I/O specification. 
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP


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
      if (ESMF_LogMsgFoundAllocError(status, "Allocating Field information", &
                                       ESMF_CONTEXT, rc)) return

      ! Call field construction method
      call ESMF_FieldConstructNoGridArray(ftype, name, iospec, status)
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      ! Set return values.
      ESMF_FieldCreateNoGridArray%ftypep => ftype
      if(rcpresent) rc = ESMF_SUCCESS

      end function ESMF_FieldCreateNoGridArray

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldDestroy"

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
!     The arguments are:
!     \begin{description}
!     \item [field]
!           Pointer to an {\tt ESMF\_Field} object.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP

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
         if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                "Uninitialized or already destroyed Field", &
                                 ESMF_CONTEXT, rc)) return
      endif

      ! Destruct all field internals and then free field memory.
      call ESMF_FieldDestruct(field%ftypep, status)
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return
           
      if (associated(field%ftypep)) then
         deallocate(field%ftypep, stat=status)
         if (ESMF_LogMsgFoundAllocError(status, "Deallocating Field", &
                                       ESMF_CONTEXT, rc)) return
      endif 
           
      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldDestroy

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldGet"

!BOP
! !IROUTINE: ESMF_FieldGet - Return info associated with a Field
!
! !INTERFACE:
      subroutine ESMF_FieldGet(field, grid, array, datamap, horzRelloc, &
                               vertRelloc, name, rc)
!
! !ARGUMENTS:
      type(ESMF_Field), intent(in) :: field    
      type(ESMF_Grid), intent(out), optional :: grid     
      type(ESMF_Array), intent(out), optional :: array     
      type(ESMF_FieldDataMap), intent(out), optional :: datamap     
      type(ESMF_RelLoc), intent(out), optional :: horzRelloc 
      type(ESMF_RelLoc), intent(out), optional :: vertRelloc 
      character(len=*), intent(out), optional :: name
      integer, intent(out), optional :: rc     
!
! !DESCRIPTION:
!      Query an {\tt ESMF\_Field} for various things.  All arguments after
!      the {\tt Field} are optional.  To select individual items use the
!      named\_argument=value syntax.
!
!
!     The arguments are:
!     \begin{description}
!     \item [ftype]
!           Pointer to an {\tt ESMF\_Field} object.
!     \item [{[grid]}]
!           {\tt ESMF\_Grid}.
!     \item [{[array]}]
!           {\tt ESMF\_Array}.
!     \item [{[datamap]}]
!           {\tt ESMF\_FieldDataMap}.
!     \item [{[horzRelloc}]]
!           Relative location of data per grid cell/vertex in the horizontal
!           grid.
!     \item [{[vertRelloc]}]
!           Relative location of data per grid cell/vertex in the vertical grid.
!     \item [{[name]}]
!           Name of queried item.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP

        type(ESMF_FieldType), pointer :: ftype
        integer :: status

        ! assume failure
        if (present(rc)) rc = ESMF_FAILURE

        ! Minimal error checking
        if (.not.associated(field%ftypep)) then
          if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                "Uninitialized or already destroyed Field", &
                                 ESMF_CONTEXT, rc)) return
        endif
 
        ftype => field%ftypep
        if (ftype%fieldstatus .ne. ESMF_STATUS_READY) then
          if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                "Uninitialized or already destroyed Field", &
                                 ESMF_CONTEXT, rc)) return
        endif

        if (present(grid)) then
            if (ftype%gridstatus .ne. ESMF_STATUS_READY) then
                if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                "No Grid or Bad Grid attached to Field", &
                                 ESMF_CONTEXT, rc)) return
            endif
            grid = ftype%grid
        endif

        if (present(array)) then
            if (ftype%datastatus .ne. ESMF_STATUS_READY) then
                if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                "No data attached to Field", &
                                 ESMF_CONTEXT, rc)) return
            endif
            array = ftype%localfield%localdata
        endif

        if (present(datamap)) then
            ! TODO: what's the proper test here?  you could have a map w/ no data yet
            !    if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
            !                    "No data attached to Field", &
            !                     ESMF_CONTEXT, rc)) return
            datamap = ftype%mapping
        endif

        if (present(horzRelloc)) then
            ! TODO: what's the proper test here?  ditto code above.
            !if (ftype%datastatus .ne. ESMF_STATUS_READY) then
            !    if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
            !                    "No data attached to Field", &
            !                     ESMF_CONTEXT, rc)) return
            !endif
            call ESMF_FieldDataMapGet(ftype%mapping, horzRelloc=horzRelloc, rc=status)
            if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return
        endif

        if (present(vertRelloc)) then
            ! TODO: what's the proper test here?  ditto code above.
            !if (ftype%datastatus .ne. ESMF_STATUS_READY) then
            !  print *, "ERROR: No data attached to Field"
            !  return
            !endif
            call ESMF_FieldDataMapGet(ftype%mapping, vertRelloc=vertRelloc, rc=status)
            if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return
        endif

        if (present(name)) then
            call c_ESMC_GetName(ftype%base, name, status)
            if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return
        endif

        if (present(rc)) rc = ESMF_SUCCESS

        end subroutine ESMF_FieldGet

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldGetArray"

!BOP
! !IROUTINE: ESMF_FieldGetArray - Get data Array associated with the Field
!
! !INTERFACE:
      subroutine ESMF_FieldGetArray(field, array, rc)

!
! !ARGUMENTS:
      type(ESMF_Field), intent(in) :: field      
      type(ESMF_Array), intent(out) :: array
      integer, intent(out), optional :: rc           

!
! !DESCRIPTION:
!     Get data in {\tt ESMF\_Array} form.
!
!     The arguments are:
!     \begin{description}
!     \item [field]
!           An {\tt ESMF\_Field} object.
!     \item [{[array]}]
!           Field {\tt ESMF\_Array}.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP


      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present
      !character(len=ESMF_MAXSTR) :: str
      type(ESMF_FieldType), pointer :: ftypep

      ! Initialize return code   
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent=.TRUE.
        rc = ESMF_FAILURE
      endif

      ! Minimal error checking 
      if (.not.associated(field%ftypep)) then
         if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                "Uninitialized or already destroyed Field", &
                                 ESMF_CONTEXT, rc)) return
      endif

      ftypep => field%ftypep

      if (ftypep%fieldstatus .ne. ESMF_STATUS_READY) then
         if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                "Uninitialized or already destroyed Field", &
                                 ESMF_CONTEXT, rc)) return
      endif

      if (ftypep%datastatus .ne. ESMF_STATUS_READY) then
           if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                "No data associated with Field", &
                                 ESMF_CONTEXT, rc)) return
      endif

      !call ESMF_StatusString(ftypep%datastatus, str, rc)
      !print *, "getting array data, status = ", trim(str)
      array = ftypep%localfield%localdata
   
      ! Set return values.
      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldGetArray

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldGetInt4Attr"

!BOP
! !IROUTINE: ESMF_FieldGetAttribute  - Retrieve a 4-byte integer attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_FieldGetAttribute()
      subroutine ESMF_FieldGetInt4Attr(field, name, value, rc)
!
! !ARGUMENTS:
      type(ESMF_Field), intent(in) :: field  
      character (len = *), intent(in) :: name
      integer(ESMF_KIND_I4), intent(out) :: value
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!     Returns an integer attribute from the {\tt field}.
!
!     The arguments are:
!     \begin{description}
!     \item [field]
!           An {\tt ESMF\_Field} object.
!     \item [name]
!           The name of the attribute to retrieve.
!     \item [value]
!           The integer value of the named attribute.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP

      integer :: status                           ! Error status

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_FAILURE

      call c_ESMC_AttributeGetValue(field%ftypep%base, name, &
                                    ESMF_DATA_INTEGER, ESMF_I4, 1, &
                                    value, status)
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldGetInt4Attr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldGetInt4ListAttr"

!BOP
! !IROUTINE: ESMF_FieldGetAttribute - Retrieve a 4-byte integer list attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_FieldGetAttribute()
      subroutine ESMF_FieldGetInt4ListAttr(field, name, count, valueList, rc)
!
! !ARGUMENTS:
      type(ESMF_Field), intent(in) :: field  
      character (len = *), intent(in) :: name
      integer, intent(in) :: count   
      integer(ESMF_KIND_I4), dimension(:), intent(out) :: valueList
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Returns a 4-byte integer list attribute from the {\tt field}.
! 
!     The arguments are:
!     \begin{description}
!     \item [field]
!           An {\tt ESMF\_Field} object.
!     \item [name]
!           The name of the attribute to retrieve.
!     \item [count]
!           The number of values in the attribute.
!     \item [valueList]
!           The integer values of the named attribute.
!           The list must be at least {\tt count} items long.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP

      integer :: status                           ! Error status
      integer :: limit

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_FAILURE

      limit = size(valueList)
      if (count > limit) then
          if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                "count longer than valueList", &
                                 ESMF_CONTEXT, rc)) return
      endif

      call c_ESMC_AttributeGetValue(field%ftypep%base, name, &
                                    ESMF_DATA_INTEGER, ESMF_I4, count, &
                                    valueList, status)
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldGetInt4ListAttr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldGetInt8Attr"

!BOP
! !IROUTINE: ESMF_FieldGetAttribute  - Retrieve a 8-byte integer attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_FieldGetAttribute()
      subroutine ESMF_FieldGetInt8Attr(field, name, value, rc)
!
! !ARGUMENTS:
      type(ESMF_Field), intent(in) :: field  
      character (len = *), intent(in) :: name
      integer(ESMF_KIND_I8), intent(out) :: value
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!     Returns an integer attribute from the {\tt field}.
!
!     The arguments are:
!     \begin{description}
!     \item [field]
!           An {\tt ESMF\_Field} object.
!     \item [name]
!           The name of the attribute to retrieve.
!     \item [value]
!           The integer value of the named attribute.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP

      integer :: status                           ! Error status

      call c_ESMC_AttributeGetValue(field%ftypep%base, name, &
                                    ESMF_DATA_INTEGER, ESMF_I8, 1, &
                                    value, status)
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldGetInt8Attr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldGetInt8ListAttr"

!BOP
! !IROUTINE: ESMF_FieldGetAttribute - Retrieve a 8-byte integer list attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_FieldGetAttribute()
      subroutine ESMF_FieldGetInt8ListAttr(field, name, count, valueList, rc)
!
! !ARGUMENTS:
      type(ESMF_Field), intent(in) :: field  
      character (len = *), intent(in) :: name
      integer, intent(in) :: count   
      integer(ESMF_KIND_I8), dimension(:), intent(out) :: valueList
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Returns a 8-byte integer list attribute from the {\tt field}.
! 
!     The arguments are:
!     \begin{description}
!     \item [field]
!           An {\tt ESMF\_Field} object.
!     \item [name]
!           The name of the attribute to retrieve.
!     \item [count]
!           The number of values in the attribute.
!     \item [valueList]
!           The integer values of the named attribute.
!           The list must be at least {\tt count} items long.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP

      integer :: status                           ! Error status
      integer :: limit

      limit = size(valueList)
      if (count > limit) then
          if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                "count longer than valueList", &
                                 ESMF_CONTEXT, rc)) return
      endif

      call c_ESMC_AttributeGetValue(field%ftypep%base, name, &
                                    ESMF_DATA_INTEGER, ESMF_I8, count, &
                                    valueList, status)
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldGetInt8ListAttr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldGetReal4Attr"

!BOP
! !IROUTINE: ESMF_FieldGetAttribute - Retrieve a 4-byte real attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_FieldGetAttribute()
      subroutine ESMF_FieldGetReal4Attr(field, name, value, rc)
!
! !ARGUMENTS:
      type(ESMF_Field), intent(in) :: field  
      character (len = *), intent(in) :: name
      real(ESMF_KIND_R4), intent(out) :: value
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Returns a 4-byte real attribute from the {\tt field}.
!
!     The arguments are:
!     \begin{description}
!     \item [field]
!           An {\tt ESMF\_Field} object.
!     \item [name]
!           The name of the attribute to retrieve.
!     \item [value]
!           The real value of the named attribute.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP

      integer :: status                           ! Error status

      call c_ESMC_AttributeGetValue(field%ftypep%base, name, &
                                    ESMF_DATA_REAL, ESMF_R4, 1, value, status)
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldGetReal4Attr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldGetReal4ListAttr"

!BOP
! !IROUTINE: ESMF_FieldGetAttribute - Retrieve a 4-byte real list attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_FieldGetAttribute()
      subroutine ESMF_FieldGetReal4ListAttr(field, name, count, valueList, rc)
!
! !ARGUMENTS:
      type(ESMF_Field), intent(in) :: field  
      character (len = *), intent(in) :: name
      integer, intent(in) :: count   
      real(ESMF_KIND_R4), dimension(:), intent(out) :: valueList
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Returns a 4-byte real attribute from an {\tt ESMF\_Field}.
! 
!     The arguments are:
!     \begin{description}
!     \item [field]
!           An {\tt ESMF\_Field} object.
!     \item [name]
!           The name of the attribute to retrieve.
!     \item [count]
!           The number of values in the attribute.
!     \item [valueList]
!           The real values of the named attribute.
!           The list must be at least {\tt count} items long.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP

      integer :: status                           ! Error status
      integer :: limit

      limit = size(valueList)
      if (count > limit) then
          if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                "count longer than valueList", &
                                 ESMF_CONTEXT, rc)) return
      endif

      call c_ESMC_AttributeGetValue(field%ftypep%base, name, &
                                    ESMF_DATA_REAL, ESMF_R4, count, &
                                    valueList, status)
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldGetReal4ListAttr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldGetReal8Attr"

!BOP
! !IROUTINE: ESMF_FieldGetAttribute - Retrieve a 8-byte real attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_FieldGetAttribute()
      subroutine ESMF_FieldGetReal8Attr(field, name, value, rc)
!
! !ARGUMENTS:
      type(ESMF_Field), intent(in) :: field  
      character (len = *), intent(in) :: name
      real(ESMF_KIND_R8), intent(out) :: value
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Returns a 8-byte real attribute from the {\tt field}.
!
!     The arguments are:
!     \begin{description}
!     \item [field]
!           An {\tt ESMF\_Field} object.
!     \item [name]
!           The name of the attribute to retrieve.
!     \item [value]
!           The real value of the named attribute.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP

      integer :: status                           ! Error status

      call c_ESMC_AttributeGetValue(field%ftypep%base, name, &
                                    ESMF_DATA_REAL, ESMF_R8, 1, value, status)
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldGetReal8Attr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldGetReal8ListAttr"

!BOP
! !IROUTINE: ESMF_FieldGetAttribute - Retrieve a 8-byte real list attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_FieldGetAttribute()
      subroutine ESMF_FieldGetReal8ListAttr(field, name, count, valueList, rc)
!
! !ARGUMENTS:
      type(ESMF_Field), intent(in) :: field  
      character (len = *), intent(in) :: name
      integer, intent(in) :: count   
      real(ESMF_KIND_R8), dimension(:), intent(out) :: valueList
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Returns a 8-byte real attribute from an {\tt ESMF\_Field}.
! 
!     The arguments are:
!     \begin{description}
!     \item [field]
!           An {\tt ESMF\_Field} object.
!     \item [name]
!           The name of the attribute to retrieve.
!     \item [count]
!           The number of values in the attribute.
!     \item [valueList]
!           The real values of the named attribute.
!           The list must be at least {\tt count} items long.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP

      integer :: status                           ! Error status
      integer :: limit

      limit = size(valueList)
      if (count > limit) then
          if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                "count longer than valueList", &
                                 ESMF_CONTEXT, rc)) return
      endif

      call c_ESMC_AttributeGetValue(field%ftypep%base, name, &
                                    ESMF_DATA_REAL, ESMF_R8, count, &
                                    valueList, status)
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldGetReal8ListAttr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldGetLogicalAttr"

!BOP
! !IROUTINE: ESMF_FieldGetAttribute - Retrieve a logical attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_FieldGetAttribute()
      subroutine ESMF_FieldGetLogicalAttr(field, name, value, rc)
!
! !ARGUMENTS:
      type(ESMF_Field), intent(in) :: field  
      character (len = *), intent(in) :: name
      type(ESMF_Logical), intent(out) :: value
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Returns a logical attribute from the {\tt field}.
! 
!     The arguments are:
!     \begin{description}
!     \item [field]
!           An {\tt ESMF\_Field} object.
!     \item [name]
!           The name of the attribute to retrieve.
!     \item [value]
!           The logical value of the named attribute.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP

      integer :: status                           ! Error status

      call c_ESMC_AttributeGetValue(field%ftypep%base, name, &
                                    ESMF_DATA_LOGICAL, ESMF_NOKIND, 1, &
                                    value, status)
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldGetLogicalAttr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldGetLogicalListAttr"

!BOP
! !IROUTINE: ESMF_FieldGetAttribute - Retrieve a logical list attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_FieldGetAttribute()
      subroutine ESMF_FieldGetLogicalListAttr(field, name, count, valueList, rc)
!
! !ARGUMENTS:
      type(ESMF_Field), intent(in) :: field  
      character (len = *), intent(in) :: name
      integer, intent(in) :: count   
      type(ESMF_Logical), dimension(:), intent(out) :: valueList
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Returns a logical list attribute from the {\tt field}.
! 
!     The arguments are:
!     \begin{description}
!     \item [field]
!           An {\tt ESMF\_Field} object.
!     \item [name]
!           The name of the attribute to retrieve.
!     \item [count]
!           The number of values in the attribute.
!     \item [valueList]
!           The logical values of the named attribute.
!           The list must be at least {\tt count} items long.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP

      integer :: status                           ! Error status
      integer :: limit

      limit = size(valueList)
      if (count > limit) then
          if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                    "count longer than valueList", &
                                     ESMF_CONTEXT, rc)) return
      endif

      call c_ESMC_AttributeGetValue(field%ftypep%base, name, &
                                    ESMF_DATA_LOGICAL, ESMF_NOKIND, count, &
                                    valueList, status)
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldGetLogicalListAttr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldGetCharAttr"

!BOP
! !IROUTINE: ESMF_FieldGetAttribute - Retrieve a character attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_FieldGetAttribute()
      subroutine ESMF_FieldGetCharAttr(field, name, value, rc)
!
! !ARGUMENTS:
      type(ESMF_Field), intent(in) :: field  
      character (len = *), intent(in) :: name
      character (len = *), intent(out) :: value
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Returns a character attribute from the {\tt field}.
! 
!     The arguments are:
!     \begin{description}
!     \item [field]
!           An {\tt ESMF\_Field} object.
!     \item [name]
!           The name of the attribute to retrieve.
!     \item [value]
!           The character value of the named attribute.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP

      integer :: status                           ! Error status

      call c_ESMC_AttributeGetChar(field%ftypep%base, name, value, status)
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldGetCharAttr


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldGetAttributeCount"

!BOP
! !IROUTINE: ESMF_FieldGetAttributeCount - Query the number of attributes
!
! !INTERFACE:
      subroutine ESMF_FieldGetAttributeCount(field, count, rc)
!
! !ARGUMENTS:
      type(ESMF_Field), intent(in) :: field  
      integer, intent(out) :: count   
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!     Returns the number of attributes associated with the given {\tt field} 
!     in the argument {\tt count}.
! 
!     The arguments are:
!     \begin{description}
!     \item [field]
!           An {\tt ESMF\_Field} object.
!     \item [count]
!           The number of attributes associated with this object.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP

      integer :: status                           ! Error status

      call c_ESMC_AttributeGetCount(field%ftypep%base, count, status)
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldGetAttributeCount

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldGetAttrInfoByName"

!BOP
! !IROUTINE: ESMF_FieldGetAttributeInfo - Query Field attributes by name
!
! !INTERFACE:
      ! Private name; call using ESMF_FieldGetAttributeInfo()
      subroutine ESMF_FieldGetAttrInfoByName(field, name, datatype, &
                                             datakind, count, rc)
!
! !ARGUMENTS:
      type(ESMF_Field), intent(in) :: field  
      character(len=*), intent(in) :: name
      type(ESMF_DataType), intent(out), optional :: datatype
      type(ESMF_DataKind), intent(out), optional :: datakind
      integer, intent(out), optional :: count   
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!     Returns information associated with the named attribute, 
!     including {\tt datatype} and {\tt count}.
! 
!     The arguments are:
!     \begin{description}
!     \item [field]
!           An {\tt ESMF\_Field} object.
!     \item [name]
!           The name of the attribute to query.
!     \item [{[datatype]}]
!           The data type of the attribute. One of the values
!           {\tt ESMF\_DATA\_INTEGER}, {\tt ESMF\_DATA\_REAL},
!           {\tt ESMF\_DATA\_LOGICAL}, or {\tt ESMF\_DATA\_CHARACTER}.
!     \item [{[datakind]}]
!           The datakind of the attribute, if attribute is type
!           {\tt ESMF\_DATA\_INTEGER} or {\tt ESMF\_DATA\_REAL}.
!           One of the values {\tt ESMF\_I4}, {\tt ESMF\_I8}, {\tt ESMF\_R4},
!           or {\tt ESMF\_R8}.
!           For all other types the value {\tt ESMF\_NOKIND} is returned.
!     \item [{[count]}]
!           The number of items in this attribute.  For character types,
!           the length of the character string.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP

      integer :: status                           ! Error status
      type(ESMF_DataType) :: localDt
      type(ESMF_DataKind) :: localDk
      integer :: localCount

      call c_ESMC_AttributeGetAttrInfoName(field%ftypep%base, name, &
                                           localDt, localDk, localCount, status)
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(datatype)) datatype = localDt
      if (present(datakind)) datakind = localDk
      if (present(count)) count = localCount

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldGetAttrInfoByName

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldGetAttrInfoByNum"

!BOP
! !IROUTINE: ESMF_FieldGetAttributeInfo - Query Field attributes by index number
!
! !INTERFACE:
      ! Private name; call using ESMF_FieldGetAttributeInfo()
      subroutine ESMF_FieldGetAttrInfoByNum(field, attributeIndex, name, &
                                            datatype, datakind, count, rc)
!
! !ARGUMENTS:
      type(ESMF_Field), intent(in) :: field  
      integer, intent(in) :: attributeIndex
      character(len=*), intent(out), optional :: name
      type(ESMF_DataType), intent(out), optional :: datatype
      type(ESMF_DataKind), intent(out), optional :: datakind
      integer, intent(out), optional :: count   
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Returns information associated with the indexed attribute, 
!      including {\tt name}, {\tt datatype}, {\tt datakind} (if applicable)
!      and {\tt count}.
! 
!     The arguments are:
!     \begin{description}
!     \item [field]
!           An {\tt ESMF\_Field} object.
!     \item [attributeIndex]
!           The index number of the attribute to query.
!     \item [name]
!           Returns the name of the attribute.
!     \item [{[datatype]}]        
!           The data type of the attribute. One of the values
!           {\tt ESMF\_DATA\_INTEGER}, {\tt ESMF\_DATA\_REAL},
!           {\tt ESMF\_DATA\_LOGICAL}, or {\tt ESMF\_DATA\_CHARACTER}.
!     \item [{[datakind]}]
!           The datakind of the attribute, if attribute is type
!           {\tt ESMF\_DATA\_INTEGER} or {\tt ESMF\_DATA\_REAL}.
!           One of the values {\tt ESMF\_I4}, {\tt ESMF\_I8}, {\tt ESMF\_R4},
!           or {\tt ESMF\_R8}.
!           For all other types the value {\tt ESMF\_NOKIND} is returned.
!     \item [{[count]}]
!           Returns the number of items in this attribute.  For character types,
!           this is the length of the character string.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP

      integer :: status                           ! Error status
      character(len=ESMF_MAXSTR) :: localName
      type(ESMF_DataType) :: localDt
      type(ESMF_DataKind) :: localDk
      integer :: localCount

      call c_ESMC_AttributeGetAttrInfoNum(field%ftypep%base, attributeIndex, &
                                         localName, localDt, localDk, &
                                         localCount, status)
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(name)) name = localName
      if (present(datatype)) datatype = localDt
      if (present(datakind)) datakind = localDk
      if (present(count)) count = localCount

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldGetAttrInfoByNum

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldGetGlobalGridInfo"

!BOPI
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
!
!     The arguments are:
!     \begin{description}
!     \item [field]
!           An {\tt ESMF\_Field} object.
!     \item [{[ndim]}]
!           Number of dimensions.
!     \item [{[ncell]}]
!           Number of cells.
!     \item [{[nvertex]}]
!           Number of vertex.
!     \item [{[size]}]
!           Size of grid.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI


!
! TODO: code goes here.  This marked BOPI because it isn't implemented yet,
!   and it may be that a lot of the query routines are not actually
!   implemented at the field level - only the most commonly used ones.
!   the rest are to be implemented by GridGet() - the caller gets the grid
!   from the field and then queries the grid directly.
!
        end subroutine ESMF_FieldGetGlobalGridInfo

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldGetLocalGridInfo"

!BOPI
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
!     The arguments are:
!     \begin{description}
!     \item [field]
!           An {\tt ESMF\_Field} object.
!     \item [{[ndim]}]
!           Number of dimensions.
!     \item [{[ncell]}]
!           Number of cells.
!     \item [{[nvertex]}]
!           Number of vertex.
!     \item [{[size]}]
!           Size of grid.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI


!
! TODO: code goes here
!
        end subroutine ESMF_FieldGetLocalGridInfo

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldGetGlobalDataInfo"

!BOPI
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
      type(ESMF_InterleaveFlag), intent(out) :: interleave   
      integer, intent(out), optional :: rc           

!
! !DESCRIPTION:
!     Retrieve global {\tt ESMF\_Field} data information.
!
!     The arguments are:
!     \begin{description}
!     \item [field]
!           An {\tt ESMF\_Field} object.
!     \item [{[size]}]
!           Field size.
!     \item [indexorder]
!           Field index order.
!     \item [datatype]
!           Field data type.
!     \item [interleave]
!           Data may be ESMF\_IL\_BLOCK or ESMF\_IL\_ITEM.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI


!
! TODO: code goes here.  BOPI because not implemented yet.
!
        end subroutine ESMF_FieldGetGlobalDataInfo

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldGetLocalDataInfo"

!BOPI
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
      type(ESMF_InterleaveFlag), intent(out) :: interleave   
      integer, intent(out), optional :: rc           

!
! !DESCRIPTION:
!     Retrieve {\tt ESMF\_Field} data information specific to the local
!     {\tt ESMF\_DE}.
!
!     The arguments are:
!     \begin{description}
!     \item [field]
!           An {\tt ESMF\_Field} object.
!     \item [{[size]}]
!           Field size.
!     \item [indexorder]
!           Data index order.
!     \item [datatype]
!           Field data type.
!     \item [interleave]
!           Data may be ESMF\_IL\_BLOCK or ESMF\_IL\_ITEM.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI


!
! TODO: code goes here
!
        end subroutine ESMF_FieldGetLocalDataInfo

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldPrint"

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
!     Prints information about the {\tt field} to {\tt stdout}.
!
!     The arguments are:
!     \begin{description}
!     \item [field]
!     \item [{[options]}]
!           Print options are not yet supported.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP

        character(len=ESMF_MAXSTR) :: name, str
        type(ESMF_FieldType), pointer :: fp 
        integer :: status
        logical :: dummy
        character(len=ESMF_MAXSTR) :: msgbuf


        if (present(rc)) rc = ESMF_FAILURE

        !nsc dummy = ESMF_LogWrite("Field Print:", ESMF_LOG_INFO)
        write(*,*) "Field Print:"
        if (.not. associated(field%ftypep)) then
        !jw  dummy = ESMF_LogWrite("Empty or Uninitialized Field", ESMF_LOG_INFO)
          write(*,*) "Empty or Uninitialized Field"
          if (present(rc)) rc = ESMF_SUCCESS
          return
        endif

        fp => field%ftypep
        call ESMF_BasePrint(fp%base, str, status)
        if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

        call ESMF_StatusString(fp%fieldstatus, str, status)
      !jw  write(msgbuf, *)  "Field status = ", trim(str)
      !jw  dummy = ESMF_LogWrite(msgbuf, ESMF_LOG_INFO)
        write(*, *)  "Field status = ", trim(str)

        if (fp%fieldstatus .ne. ESMF_STATUS_READY) then
          if (present(rc)) rc = ESMF_SUCCESS
          return
        endif

        call c_ESMC_GetName(fp%base, name, status)
        if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return
      !jw  write(msgbuf, *)  "  Name = '",  trim(name), "'"
      !jw  dummy = ESMF_LogWrite(msgbuf, ESMF_LOG_INFO)
        write(*, *)  "  Name = '",  trim(name), "'"

        call ESMF_StatusString(fp%gridstatus, str, status)
      !jw  write(msgbuf, *)  "Grid status = ", trim(str)
      !jw  dummy = ESMF_LogWrite(msgbuf, ESMF_LOG_INFO)
        write(*, *)  "Grid status = ", trim(str)
        if (fp%gridstatus .eq. ESMF_STATUS_READY) then 
           call ESMF_GridPrint(fp%grid, "", status)
        endif


        call ESMF_StatusString(fp%datastatus, str, status)
      !jw  write(msgbuf, *)  "Data status = ", trim(str)
      !jw  dummy = ESMF_LogWrite(msgbuf, ESMF_LOG_INFO)
        write(*, *)  "Data status = ", trim(str)
        !TODO: add code here to print more info
        if (fp%datastatus .eq. ESMF_STATUS_READY) then 
           call ESMF_ArrayPrint(fp%localfield%localdata, "", status)
        endif

        call ESMF_FieldDataMapPrint(fp%mapping, "", status)



        ! global field contents
        !type (ESMF_IOSpec) :: iospec             ! iospec values
        !type (ESMF_Status) :: iostatus           ! if unset, inherit from gcomp

        ! local field contents
        !type (ESMF_Mask) :: mask                 ! may belong in Grid

        if (present(rc)) rc = ESMF_SUCCESS

        end subroutine ESMF_FieldPrint
!
!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldRead"

!BOPI
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
!     The arguments are:
!     \begin{description}
!     \item [name]
!           An {\tt ESMF\_Field} name.
!     \item [{[gname]}]
!            {\tt ESMF\_Grid} name.
!     \item [{[dnames]}]
!            Data name.
!     \item [{[iospec]}]
!            I/O specification.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI


!
! TODO: code goes here.  this is filler to keep the compiler from complaining.
!
        type (ESMF_Field) :: a
     
        nullify(a%ftypep)

        ESMF_FieldRead = a
!       BOP/EOP have been changed to BOPI/EOPI until the subroutine is implemented.


        end function ESMF_FieldRead

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldReadRestart"

!BOPI
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
!      all data associated with an {\tt ESMF\_Field} from the 
!      last call to WriteRestart.
!
!     The arguments are:
!     \begin{description}
!     \item [name]
!           An {\tt ESMF\_Field} name.
!     \item [{[iospec]}]
!            I/O specification.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI

!       BOP/EOP have been changed to BOPI/EOPI until the subroutine is implemented.

!
! TODO: code goes here; this is just filler to make the compiler not complain
!
        type (ESMF_Field) :: a
     
        nullify(a%ftypep)

        ESMF_FieldReadRestart = a

        end function ESMF_FieldReadRestart


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldSetArray"

!BOP
! !IROUTINE: ESMF_FieldSetArray - Set data Array associated with the Field
!
! !INTERFACE:
      subroutine ESMF_FieldSetArray(field, array, rc)

!
! !ARGUMENTS:
      type(ESMF_Field), intent(inout) :: field      
      type(ESMF_Array), intent(in) :: array
      integer, intent(out), optional :: rc           

!
! !DESCRIPTION:
!     Set data in {\tt ESMF\_Array} form.
!
!     The arguments are:
!     \begin{description}
!     \item [field]
!           An {\tt ESMF\_Field} object.
!     \item [{[array]}]
!           {\tt ESMF\_Array} containing data.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP


      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present
      !character(len=ESMF_MAXSTR) :: str
      type(ESMF_FieldType), pointer :: ftypep

      ! Initialize return code   
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent=.TRUE.
        rc = ESMF_FAILURE
      endif

      ! Minimal error checking 
      if (.not.associated(field%ftypep)) then
         if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                "Uninitialized or already destroyed Field", &
                                 ESMF_CONTEXT, rc)) return
      endif

      ftypep => field%ftypep

      if (ftypep%fieldstatus .ne. ESMF_STATUS_READY) then
         if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                "Uninitialized or already destroyed Field", &
                                 ESMF_CONTEXT, rc)) return
      endif

      ! TODO: do we allow this?  if so, do we just destroy the old array?
      !if (ftypep%datastatus .eq. ESMF_STATUS_READY) then
      !   if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
      !                          "Data already associated with Field", &
      !                           ESMF_CONTEXT, rc)) return
      !endif

      ftypep%localfield%localdata = array
      ftypep%datastatus = ESMF_STATUS_READY
   
      ! TODO: add some validation here to be sure the array is the right
      ! size for the grid decomposition

      ! Set return values.
      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldSetArray

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldSetInt4Attr"

!BOP
! !IROUTINE: ESMF_FieldSetAttribute - Set a 4-byte integer attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_FieldSetAttribute()
      subroutine ESMF_FieldSetInt4Attr(field, name, value, rc)
!
! !ARGUMENTS:
      type(ESMF_Field), intent(inout) :: field  
      character (len = *), intent(in) :: name
      integer(ESMF_KIND_I4), intent(in) :: value
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Attaches a 4-byte integer attribute to the {\tt field}.
!      The attribute has a {\tt name} and a {\tt value}.
! 
!     The arguments are:
!     \begin{description}
!     \item [field]
!           An {\tt ESMF\_Field} object.
!     \item [name]
!           The name of the attribute to add.
!     \item [value]
!           The integer value of the attribute to add.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP

      integer :: status                           ! Error status

      call c_ESMC_AttributeSetValue(field%ftypep%base, name, &
                                    ESMF_DATA_INTEGER, ESMF_I4, 1, &
                                    value, status)
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldSetInt4Attr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldSetInt4ListAttr"

!BOP
! !IROUTINE: ESMF_FieldSetAttribute - Set a 4-byte integer list attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_FieldSetAttribute()
      subroutine ESMF_FieldSetInt4ListAttr(field, name, count, valueList, rc)
!
! !ARGUMENTS:
      type(ESMF_Field), intent(in) :: field  
      character (len = *), intent(in) :: name
      integer, intent(in) :: count   
      integer(ESMF_KIND_I4), dimension(:), intent(in) :: valueList
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!     Attaches a 4-byte integer list attribute to the {\tt field}.
!     The attribute has a {\tt name} and a {\tt valueList}.
!     The number of integer items in the {\tt valueList} is
!     given by {\tt count}.
!
!     The arguments are:
!     \begin{description}
!     \item [field]
!           An {\tt ESMF\_Field} object.
!     \item [name]
!           The name of the attribute to add.
!     \item [count]
!           The number of integers in the {\tt valueList}.
!     \item [valueList]
!           The integer values of the attribute to add.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP

      integer :: status                           ! Error status
      integer :: limit

      limit = size(valueList)
      if (count > limit) then
          if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                "count longer than valueList", &
                                 ESMF_CONTEXT, rc)) return
      endif

      call c_ESMC_AttributeSetValue(field%ftypep%base, name, &
                                    ESMF_DATA_INTEGER, ESMF_I4, count, &
                                    valueList, status)
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldSetInt4ListAttr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldSetInt8Attr"

!BOP
! !IROUTINE: ESMF_FieldSetAttribute - Set a 8-byte integer attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_FieldSetAttribute()
      subroutine ESMF_FieldSetInt8Attr(field, name, value, rc)
!
! !ARGUMENTS:
      type(ESMF_Field), intent(inout) :: field  
      character (len = *), intent(in) :: name
      integer(ESMF_KIND_I8), intent(in) :: value
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Attaches a 8-byte integer attribute to the {\tt field}.
!      The attribute has a {\tt name} and a {\tt value}.
! 
!     The arguments are:
!     \begin{description}
!     \item [field]
!           An {\tt ESMF\_Field} object.
!     \item [name]
!           The name of the attribute to add.
!     \item [value]
!           The integer value of the attribute to add.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP

      integer :: status                           ! Error status

      call c_ESMC_AttributeSetValue(field%ftypep%base, name, &
                                    ESMF_DATA_INTEGER, ESMF_I8, 1, &
                                    value, status)
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldSetInt8Attr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldSetInt8ListAttr"

!BOP
! !IROUTINE: ESMF_FieldSetAttribute - Set a 8-byte integer list attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_FieldSetAttribute()
      subroutine ESMF_FieldSetInt8ListAttr(field, name, count, valueList, rc)
!
! !ARGUMENTS:
      type(ESMF_Field), intent(in) :: field  
      character (len = *), intent(in) :: name
      integer, intent(in) :: count   
      integer(ESMF_KIND_I8), dimension(:), intent(in) :: valueList
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!     Attaches a 8-byte integer list attribute to the {\tt field}.
!     The attribute has a {\tt name} and a {\tt valueList}.
!     The number of integer items in the {\tt valueList} is
!     given by {\tt count}.
!
!     The arguments are:
!     \begin{description}
!     \item [field]
!           An {\tt ESMF\_Field} object.
!     \item [name]
!           The name of the attribute to add.
!     \item [count]
!           The number of integers in the {\tt valueList}.
!     \item [valueList]
!           The integer values of the attribute to add.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP

      integer :: status                           ! Error status
      integer :: limit

      limit = size(valueList)
      if (count > limit) then
          if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                "count longer than valueList", &
                                 ESMF_CONTEXT, rc)) return
      endif

      call c_ESMC_AttributeSetValue(field%ftypep%base, name, &
                                    ESMF_DATA_INTEGER, ESMF_I8, count, &
                                    valueList, status)
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldSetInt8ListAttr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldSetReal4Attr"

!BOP
! !IROUTINE: ESMF_FieldSetAttribute - Set a 4-byte real attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_FieldSetAttribute()
      subroutine ESMF_FieldSetReal4Attr(field, name, value, rc)
!
! !ARGUMENTS:
      type(ESMF_Field), intent(in) :: field  
      character (len = *), intent(in) :: name
      real(ESMF_KIND_R4), intent(in) :: value
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Attaches a 4-byte real attribute to the {\tt field}.
!      The attribute has a {\tt name} and a {\tt value}.
! 
!     The arguments are:
!     \begin{description}
!     \item [field]
!           An {\tt ESMF\_Field} object.
!     \item [name]
!           The name of the attribute to add.
!     \item [value]
!           The real value of the attribute to add.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP

      integer :: status                           ! Error status

      call c_ESMC_AttributeSetValue(field%ftypep%base, name, &
                                    ESMF_DATA_REAL, ESMF_R4, 1, value, status)
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldSetReal4Attr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldSetReal4ListAttr"

!BOP
! !IROUTINE: ESMF_FieldSetAttribute - Set a 4-byte real list attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_FieldSetAttribute()
      subroutine ESMF_FieldSetReal4ListAttr(field, name, count, valueList, rc)
!
! !ARGUMENTS:
      type(ESMF_Field), intent(in) :: field  
      character (len = *), intent(in) :: name
      integer, intent(in) :: count   
      real(ESMF_KIND_R4), dimension(:), intent(in) :: valueList
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!     Attaches a 4-byte real list attribute to the {\tt field}.
!     The attribute has a {\tt name} and a {\tt valueList}.
!     The number of real items in the {\tt valueList} is
!     given by {\tt count}.
!
!     The arguments are:
!     \begin{description}
!     \item [field]
!           An {\tt ESMF\_Field} object.
!     \item [name]
!           The name of the attribute to add.
!     \item [count]
!           The number of reals in the {\tt valueList}.
!     \item [value]
!           The real values of the attribute to add.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP

      integer :: status                           ! Error status
      integer :: limit

      limit = size(valueList)
      if (count > limit) then
          if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                "count longer than valueList", &
                                 ESMF_CONTEXT, rc)) return
      endif

      call c_ESMC_AttributeSetValue(field%ftypep%base, name, &
                                    ESMF_DATA_REAL, ESMF_R4, count, &
                                    valueList, status)
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldSetReal4ListAttr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldSetReal8Attr"

!BOP
! !IROUTINE: ESMF_FieldSetAttribute - Set a 8-byte real attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_FieldSetAttribute()
      subroutine ESMF_FieldSetReal8Attr(field, name, value, rc)
!
! !ARGUMENTS:
      type(ESMF_Field), intent(in) :: field  
      character (len = *), intent(in) :: name
      real(ESMF_KIND_R8), intent(in) :: value
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Attaches a 8-byte real attribute to the {\tt field}.
!      The attribute has a {\tt name} and a {\tt value}.
! 
!     The arguments are:
!     \begin{description}
!     \item [field]
!           An {\tt ESMF\_Field} object.
!     \item [name]
!           The name of the attribute to add.
!     \item [value]
!           The real value of the attribute to add.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP

      integer :: status                           ! Error status

      call c_ESMC_AttributeSetValue(field%ftypep%base, name, &
                                    ESMF_DATA_REAL, ESMF_R8, 1, value, status)
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldSetReal8Attr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldSetReal8ListAttr"

!BOP
! !IROUTINE: ESMF_FieldSetAttribute - Set a 8-byte real list attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_FieldSetAttribute()
      subroutine ESMF_FieldSetReal8ListAttr(field, name, count, valueList, rc)
!
! !ARGUMENTS:
      type(ESMF_Field), intent(in) :: field  
      character (len = *), intent(in) :: name
      integer, intent(in) :: count   
      real(ESMF_KIND_R8), dimension(:), intent(in) :: valueList
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!     Attaches a 8-byte real list attribute to the {\tt field}.
!     The attribute has a {\tt name} and a {\tt valueList}.
!     The number of real items in the {\tt valueList} is
!     given by {\tt count}.
!
!     The arguments are:
!     \begin{description}
!     \item [field]
!           An {\tt ESMF\_Field} object.
!     \item [name]
!           The name of the attribute to add.
!     \item [count]
!           The number of reals in the {\tt valueList}.
!     \item [value]
!           The real values of the attribute to add.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP

      integer :: status                           ! Error status
      integer :: limit

      limit = size(valueList)
      if (count > limit) then
          if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                "count longer than valueList", &
                                 ESMF_CONTEXT, rc)) return
      endif

      call c_ESMC_AttributeSetValue(field%ftypep%base, name, &
                                    ESMF_DATA_REAL, ESMF_R8, count, &
                                    valueList, status)
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldSetReal8ListAttr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldSetLogicalAttr"

!BOP
! !IROUTINE: ESMF_FieldSetAttribute - Set a logical attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_FieldSetAttribute()
      subroutine ESMF_FieldSetLogicalAttr(field, name, value, rc)
!
! !ARGUMENTS:
      type(ESMF_Field), intent(in) :: field  
      character (len = *), intent(in) :: name
      type(ESMF_Logical), intent(in) :: value
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!     Attaches a logical attribute to the {\tt field}.
!     The attribute has a {\tt name} and a {\tt value}.
!
!     The arguments are:
!     \begin{description}
!     \item [field]
!           An {\tt ESMF\_Field} object.
!     \item [name]
!           The name of the attribute to add.
!     \item [value]
!           The logical true/false value of the attribute to add.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP

      integer :: status                           ! Error status

      call c_ESMC_AttributeSetValue(field%ftypep%base, name, &
                                    ESMF_DATA_LOGICAL, ESMF_NOKIND, 1, &
                                    value, status)
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldSetLogicalAttr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldSetLogicalListAttr"

!BOP
! !IROUTINE: ESMF_FieldSetAttribute - Set a logical list attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_FieldSetAttribute()
      subroutine ESMF_FieldSetLogicalListAttr(field, name, count, valueList, rc)
!
! !ARGUMENTS:
      type(ESMF_Field), intent(in) :: field  
      character (len = *), intent(in) :: name
      integer, intent(in) :: count   
      type(ESMF_Logical), dimension(:), intent(in) :: valueList
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!     Attaches a logical list attribute to the {\tt field}.
!     The attribute has a {\tt name} and a {\tt valueList}.
!     The number of logical items in the {\tt valueList} is
!     given by {\tt count}.
! 
!     The arguments are:
!     \begin{description}
!     \item [field]
!           An {\tt ESMF\_Field} object.
!     \item [name]
!           The name of the attribute to add.
!     \item [count]
!           The number of logicals in the {\tt valueList}.
!     \item [value]
!           The logical true/false values of the attribute.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP

      integer :: status                           ! Error status
      integer :: limit

      limit = size(valueList)
      if (count > limit) then
          if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                "count longer than valueList", &
                                 ESMF_CONTEXT, rc)) return
      endif

      call c_ESMC_AttributeSetValue(field%ftypep%base, name, &
                                    ESMF_DATA_LOGICAL, ESMF_NOKIND, &
                                    count, valueList, status)
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldSetLogicalListAttr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldSetCharAttr"

!BOP
! !IROUTINE: ESMF_FieldSetAttribute - Set a character attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_FieldSetAttribute()
      subroutine ESMF_FieldSetCharAttr(field, name, value, rc)
!
! !ARGUMENTS:
      type(ESMF_Field), intent(in) :: field  
      character (len = *), intent(in) :: name
      character (len = *), intent(in) :: value
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Attaches a character attribute to the {\tt field}.
!     The attribute has a {\tt name} and a {\tt value}.
!
!     The arguments are:
!     \begin{description}
!     \item [field]
!           An {\tt ESMF\_Field} object.
!     \item [name]
!           The name of the attribute to add.
!     \item [value]
!           The character value of the attribute to add.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP

      integer :: status                           ! Error status

      call c_ESMC_AttributeSetChar(field%ftypep%base, name, value, status)
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldSetCharAttr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldSetGrid"

!BOP
! !IROUTINE: ESMF_FieldSetGrid - Set Grid associated with the Field
!
! !INTERFACE:
      subroutine ESMF_FieldSetGrid(field, grid, rc)
!
! !ARGUMENTS:
      type(ESMF_Field), intent(inout) :: field  
      type(ESMF_Grid), intent(in) :: grid      
      integer, intent(out), optional :: rc    
!
! !DESCRIPTION:
!  Used only with the version of {\tt ESMF\_FieldCreate} which creates an empty 
!  {\tt ESMF\_Field} and allows the {\tt ESMF\_Grid} to be specified later.  
!  Otherwise it is an error to try to change the {\tt ESMF\_Grid} 
!  associated with an {\tt ESMF\_Field}.
!
!     The arguments are:
!     \begin{description}
!     \item [field]
!           An {\tt ESMF\_Field} object.
!     \item [grid]
!           {\tt ESMF\_Grid} to be added.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP

        logical :: had_grid

        ! assume failure
        if (present(rc)) rc = ESMF_FAILURE

        ! Minimal error checking
        if (.not.associated(field%ftypep)) then
          if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                "Uninitialized or already destroyed Field", &
                                 ESMF_CONTEXT, rc)) return
        endif
 
        if (field%ftypep%fieldstatus .ne. ESMF_STATUS_READY) then
          if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                "Uninitialized or already destroyed Field", &
                                 ESMF_CONTEXT, rc)) return
        endif

        ! decide if we're regridding or just adding a grid to a partially
        ! created field.
        had_grid = .FALSE.
        if (field%ftypep%gridstatus .eq. ESMF_STATUS_READY) had_grid = .TRUE.

        if (.not. had_grid) then
           ! if no grid, just add it
           field%ftypep%grid = grid
           field%ftypep%gridstatus = ESMF_STATUS_READY
        else
           ! this could be considered a request to regrid the data
           if (ESMF_LogWrite("Replacing existing grid not yet supported", &
                               ESMF_LOG_WARNING, &
                               ESMF_CONTEXT)) continue
           if (ESMF_LogWrite("Will be considered a regrid request", &
                               ESMF_LOG_WARNING, &
                               ESMF_CONTEXT)) continue
        endif

        if (present(rc)) rc = ESMF_SUCCESS

        end subroutine ESMF_FieldSetGrid

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldSetDataMap"

!BOP
! !IROUTINE: ESMF_FieldSetDataMap - Set DataMap assocated with a Field
!
! !INTERFACE:
      subroutine ESMF_FieldSetDataMap(field, datamap, rc)
!
! !ARGUMENTS:
      type(ESMF_Field), intent(inout) :: field
      type(ESMF_FieldDataMap), intent(in) :: datamap
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!  Used to set the ordering of an {\tt ESMF\_Field}.  If an initialized 
!  {\tt ESMF\_FieldDataMap} and associated data are already in the 
!  {\tt ESMF\_Field}, the data will be reordered according to the new 
!  specification.
!
!     The arguments are:
!     \begin{description}
!     \item [field]
!           An {\tt ESMF\_Field} object.
!     \item [datamap]
!           New memory order of data.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP

        logical :: had_data

        ! assume failure
        if (present(rc)) rc = ESMF_FAILURE

        ! Minimal error checking
        if (.not.associated(field%ftypep)) then
          if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                "Uninitialized or already destroyed Field", &
                                 ESMF_CONTEXT, rc)) return
        endif
 
        if (field%ftypep%fieldstatus .ne. ESMF_STATUS_READY) then
          if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                "Uninitialized or already destroyed Field", &
                                 ESMF_CONTEXT, rc)) return
        endif

        ! decide if we're reordering data or just setting an initial map
        ! created field.
        had_data = .FALSE.
        if (field%ftypep%datastatus .eq. ESMF_STATUS_READY) had_data = .TRUE.

        if (.not. had_data) then
           ! if no datamap, just add it
           field%ftypep%mapping = datamap
        else
           ! this could be considered a request to reorder the data
           if (ESMF_LogWrite("Replacing existing datamap not yet supported", &
                               ESMF_LOG_WARNING, &
                               ESMF_CONTEXT)) continue
           if (ESMF_LogWrite("Will be considered a data reorder request", &
                               ESMF_LOG_WARNING, &
                               ESMF_CONTEXT)) continue
           return
        endif

        if (present(rc)) rc = ESMF_SUCCESS

        end subroutine ESMF_FieldSetDataMap

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldSetDataValues"

!BOPI
! !IROUTINE: ESMF_FieldSetDataValues - Set contents of Data array
!
! !INTERFACE:
      subroutine ESMF_FieldSetDataValues(field, index, value, rc)
!
!
! !ARGUMENTS:
      type(ESMF_Field), intent(inout) :: field
      integer, dimension (:), intent(in) :: index
      real, dimension (:), intent(in) :: value
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!      Allows specified data values associated with an {\tt ESMF\_Field} to be set 
!      through the {\tt ESMF\_Field} interface instead of detaching data and setting 
!      it outside the framework.
!
!     The arguments are:
!     \begin{description}
!     \item [field]
!           An {\tt ESMF\_Field} object.
!     \item [index]
!           Index or range to set.
!     \item [values]
!           Data values to set.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI


!       BOP/EOP have been changed to BOPI/EOPI until the subroutine is implemented.
!
! TODO: code goes here
!
        end subroutine ESMF_FieldSetDataValues

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldValidate"

!BOP
! !IROUTINE:  ESMF_FieldValidate - Check validity of a Field

! !INTERFACE:
      subroutine ESMF_FieldValidate(field, options, rc)
!
! !ARGUMENTS:
      type(ESMF_Field), intent(in) :: field 
      character (len = *), intent(in) :: options 
      integer, intent(out), optional :: rc   
!
! !DESCRIPTION:
!      Validates that the {\tt field} is internally consistent.
!      Currently this method determines if the {\tt field} is uninitialized 
!      or already destroyed.  The method returns an error code if problems 
!      are found.  
!
!     The arguments are:
!     \begin{description}
!     \item [field]
!           {\tt ESMF\_Field} to validate.
!     \item [options]
!           Validation options are not yet supported.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if the {\tt field} 
!           is valid.
!     \end{description}
!
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

      if (.not.associated(field%ftypep)) then 
         if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                "Uninitialized or already destroyed Field", &
                                 ESMF_CONTEXT, rc)) return
      endif 

      if (field%ftypep%fieldstatus .ne. ESMF_STATUS_READY) then
         if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                "Uninitialized or already destroyed Field", &
                                 ESMF_CONTEXT, rc)) return
      endif 

      ! TODO: add more code here

      if (rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldValidate

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldWrite"

!BOP
! !IROUTINE: ESMF_FieldWrite - Write a Field to external storage
!
! !INTERFACE:
      subroutine ESMF_FieldWrite(field, iospec, timestamp, rc)
!
! !ARGUMENTS:
        type(ESMF_Field), intent(in) :: field
        type(ESMF_IOSpec), intent(in), optional :: iospec
        type(ESMF_Time), intent(in), optional :: timestamp 
        integer, intent(out), optional :: rc               ! return code

!
! !DESCRIPTION:
!      Used to write data to persistent storage in a variety of formats.  
!      (see WriteRestart/ReadRestart for quick data dumps.)  Details of I/O 
!      options specified in the IOSpec derived type. 
!
!
!     The arguments are:
!     \begin{description}
!     \item [name]
!           An {\tt ESMF\_Field} name.
!     \item [{[iospec]}]
!            I/O specification.
!     \item [{[timestamp]}]
!            {\tt ESMF\_Time}.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP

        ! Local variables
        integer :: status, de_id
        logical :: rcpresent
        type(ESMF_Array) :: out_array
        type(ESMF_DataType) arr_type
        type(ESMF_DataKind) arr_kind
        integer out_rank
        integer out_kind
        integer out_type
        integer, dimension(:), pointer :: out_counts
        integer, dimension(:), pointer :: out_lbounds
        integer, dimension(:), pointer :: out_ubounds
        integer, dimension(:), pointer :: out_strides
        type(ESMF_Grid) :: grid
        type(ESMF_DELayout) :: delayout
        type (ESMF_IOFileFormat) :: fileformat
        type(ESMF_Time) :: ts
        character (19) Date
      
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
           call ESMF_IOSpecGet(IOSpec, iofileformat=fileformat, rc=status)
           if (fileformat == ESMF_IO_FILEFORMAT_HDF) then
              print*, "HDF output is not currently supported."
              return
           else if (fileformat == ESMF_IO_FILEFORMAT_UNSPECIFIED) then
              call ESMF_FieldWriteFileASCII(field, iospec, rc=status)
           else if (fileformat == ESMF_IO_FILEFORMAT_NETCDF) then
#if (ESMF_NO_IOCODE)
              print*, "netCDF support not configured in."
              return
#else
              continue
#endif
           else
              print*, "Unrecognized IO Fileformat."
              return
           endif
        else ! No IOSpec passed in, so check in the Field
           call ESMF_IOSpecGet(field%ftypep%iospec, iofileformat=fileformat, rc=status)
           if (fileformat == ESMF_IO_FILEFORMAT_HDF) then
              print*, "HDF output is not currently supported."
              return
           else if (fileformat == ESMF_IO_FILEFORMAT_UNSPECIFIED) then
           call ESMF_FieldWriteFileASCII(field, iospec, rc=status)
           else if (fileformat == ESMF_IO_FILEFORMAT_NETCDF) then
#if (ESMF_NO_IOCODE)
              print*, "netCDF support not configured in."
              return
#else
              continue
#endif
           else
              print*, "Unrecognized IO Fileformat."
              return
           endif
        endif

        if ( present(timestamp) ) then
           ts = timestamp
        else
           ! as a default, set the date/time as the current real time.
           call ESMF_TimeSyncToRealTime(ts, status)
        endif
        ! get the date from the timestamp.
        call ESMF_TimeGet(ts, timeString=Date, rc=status)
        Date = Date(1:10)//'_'//Date(12:19)//'.0000'
        print*, 'Date = ', Date
!!$        Date = '2000-09-18_16:42:01'

        ! Collect results on DE 0 and output to a file
        call ESMF_FieldGet(field, grid=grid, rc=status)
!!$        call ESMF_FieldGet( field, name=fieldname, rc=status)
        call ESMF_GridGet(grid, delayout=delayout, rc=status)
        call ESMF_DELayoutGet(delayout, localDE=de_id, rc=status)

        ! Output to file, from de_id 0 only
!!$        call ESMF_FieldAllGather(field, out_array, rc=status)
        call ESMF_ArrayGather(field%ftypep%localfield%localdata, &
                              field%ftypep%grid, field%ftypep%mapping, &
                              0, out_array, rc=status)


        if (de_id .eq. 0) then       
        call ESMF_ArrayGet(out_array, out_rank, arr_type, arr_kind, rc=rc)
        allocate(out_counts (out_rank), &
                 out_lbounds(out_rank), &
                 out_ubounds(out_rank), &
                 out_strides(out_rank), stat=rc)
        call ESMF_ArrayGet(out_array, counts=out_counts, lbounds=out_lbounds, &
                           ubounds=out_ubounds, strides=out_strides, rc=rc)

        out_type = arr_type%dtype
        out_kind = arr_kind%dkind

        !! macros which are expanded by the preprocessor
        select case (out_type)
          case (ESMF_DATA_INTEGER%dtype)
            select case (out_rank)
              case (1)
                select case (out_kind)
!!$                  case (ESMF_I1%dkind)
!!$                    call ESMF_FieldWriteFileNetCDF1DI1(field, grid, out_array, out_counts, &
!!$                                    out_lbounds, out_ubounds, date, iospec, rc=status)
!!$                  case (ESMF_I2%dkind)
!!$                    call ESMF_FieldWriteFileNetCDF1DI2(field, grid, out_array, out_counts, &
!!$                                    out_lbounds, out_ubounds, date, iospec, rc=status)
!!$                  case (ESMF_I4%dkind)
!!$                    call ESMF_FieldWriteFileNetCDF1DI4(field, grid, out_array, out_counts, &
!!$                                    out_lbounds, out_ubounds, date, iospec, rc=status)
!!$                  case (ESMF_I8%dkind)
!!$                    call ESMF_FieldWriteFileNetCDF1DI8(field, grid, out_array, out_counts, &
!!$                                    out_lbounds, out_ubounds, date, iospec, rc=status)
                  case default
                    print *, "unsupported kind"
                end select

              case (2)
                select case (out_kind)
                  case (ESMF_I1%dkind)
!!$                    call ESMF_FieldWriteFileNetCDF2DI1(field, grid, out_array, out_counts, &
!!$                                    out_lbounds, out_ubounds, date, iospec, rc=status)
!!$                  case (ESMF_I2%dkind)
!!$                    call ESMF_FieldWriteFileNetCDF2DI2(field, grid, out_array, out_counts, &
!!$                                    out_lbounds, out_ubounds, date, iospec, rc=status)
!!$                  case (ESMF_I4%dkind)
!!$                    call ESMF_FieldWriteFileNetCDF2DI4(field, grid, out_array, out_counts, &
!!$                                    out_lbounds, out_ubounds, date, iospec, rc=status)
!!$                  case (ESMF_I8%dkind)
!!$                    call ESMF_FieldWriteFileNetCDF2DI8(field, grid, out_array, out_counts, &
!!$                                    out_lbounds, out_ubounds, date, iospec, rc=status)
                  case default
                    print *, "unsupported kind"
                end select

              case (3)
                select case (out_kind)
!!$                  case (ESMF_I1%dkind)
!!$                    call ESMF_FieldWriteFileNetCDF3DI1(field, grid, out_array, out_counts, &
!!$                                    out_lbounds, out_ubounds, date, iospec, rc=status)
!!$                  case (ESMF_I2%dkind)
!!$                    call ESMF_FieldWriteFileNetCDF3DI2(field, grid, out_array, out_counts, &
!!$                                    out_lbounds, out_ubounds, date, iospec, rc=status)
!!$                  case (ESMF_I4%dkind)
!!$                    call ESMF_FieldWriteFileNetCDF3DI4(field, grid, out_array, out_counts, &
!!$                                    out_lbounds, out_ubounds, date, iospec, rc=status)
!!$                  case (ESMF_I8%dkind)
!!$                    call ESMF_FieldWriteFileNetCDF3DI8(field, grid, out_array, out_counts, &
!!$                                    out_lbounds, out_ubounds, date, iospec, rc=status)
                  case default
                    print *, "unsupported kind"
                end select

              case (4)
                select case (out_kind)
!!$                  case (ESMF_I1%dkind)
!!$                    call ESMF_FieldWriteFileNetCDF4DI1(field, grid, out_array, out_counts, &
!!$                                    out_lbounds, out_ubounds, date, iospec, rc=status)
!!$                  case (ESMF_I2%dkind)
!!$                    call ESMF_FieldWriteFileNetCDF4DI2(field, grid, out_array, out_counts, &
!!$                                    out_lbounds, out_ubounds, date, iospec, rc=status)
!!$                  case (ESMF_I4%dkind)
!!$                    call ESMF_FieldWriteFileNetCDF4DI4(field, grid, out_array, out_counts, &
!!$                                    out_lbounds, out_ubounds, date, iospec, rc=status)
!!$                  case (ESMF_I8%dkind)
!!$                    call ESMF_FieldWriteFileNetCDF4DI8(field, grid, out_array, out_counts, &
!!$                                    out_lbounds, out_ubounds, date, iospec, rc=status)
                  case default
                    print *, "unsupported kind"
                end select

              case (5)
                select case (out_kind)
!!$                  case (ESMF_I1%dkind)
!!$                    call ESMF_FieldWriteFileNetCDF5DI1(field, grid, out_array, out_counts, &
!!$                                    out_lbounds, out_ubounds, date, iospec, rc=status)
!!$                  case (ESMF_I2%dkind)
!!$                    call ESMF_FieldWriteFileNetCDF5DI2(field, grid, out_array, out_counts, &
!!$                                    out_lbounds, out_ubounds, date, iospec, rc=status)
!!$                  case (ESMF_I4%dkind)
!!$                    call ESMF_FieldWriteFileNetCDF5DI4(field, grid, out_array, out_counts, &
!!$                                    out_lbounds, out_ubounds, date, iospec, rc=status)
!!$                  case (ESMF_I8%dkind)
!!$                    call ESMF_FieldWriteFileNetCDF5DI8(field, grid, out_array, out_counts, &
!!$                                    out_lbounds, out_ubounds, date, iospec, rc=status)
                  case default
                    print *, "unsupported kind"
                end select

              case (6)
                select case (out_kind)
!!$                  case (ESMF_I1%dkind)
!!$                    call ESMF_FieldWriteFileNetCDF6DI1(field, grid, out_array, out_counts, &
!!$                                    out_lbounds, out_ubounds, date, iospec, rc=status)
!!$                  case (ESMF_I2%dkind)
!!$                    call ESMF_FieldWriteFileNetCDF6DI2(field, grid, out_array, out_counts, &
!!$                                    out_lbounds, out_ubounds, date, iospec, rc=status)
!!$                  case (ESMF_I4%dkind)
!!$                    call ESMF_FieldWriteFileNetCDF6DI4(field, grid, out_array, out_counts, &
!!$                                    out_lbounds, out_ubounds, date, iospec, rc=status)
!!$                  case (ESMF_I8%dkind)
!!$                    call ESMF_FieldWriteFileNetCDF6DI8(field, grid, out_array, out_counts, &
!!$                                    out_lbounds, out_ubounds, date, iospec, rc=status)
                  case default
                    print *, "unsupported kind"
                end select

              case (7)
                select case (out_kind)
!!$                  case (ESMF_I1%dkind)
!!$                    call ESMF_FieldWriteFileNetCDF7DI1(field, grid, out_array, out_counts, &
!!$                                    out_lbounds, out_ubounds, date, iospec, rc=status)
!!$                  case (ESMF_I2%dkind)
!!$                    call ESMF_FieldWriteFileNetCDF7DI2(field, grid, out_array, out_counts, &
!!$                                    out_lbounds, out_ubounds, date, iospec, rc=status)
!!$                  case (ESMF_I4%dkind)
!!$                    call ESMF_FieldWriteFileNetCDF7DI4(field, grid, out_array, out_counts, &
!!$                                    out_lbounds, out_ubounds, date, iospec, rc=status)
!!$                  case (ESMF_I8%dkind)
!!$                    call ESMF_FieldWriteFileNetCDF7DI8(field, grid, out_array, out_counts, &
!!$                                    out_lbounds, out_ubounds, date, iospec, rc=status)
                  case default
                    print *, "unsupported kind"
                end select

              case default
                print *, "unsupported rank"
            end select

           case (ESMF_DATA_REAL%dtype)
            select case (out_rank)
              case (1)
                select case (out_kind)
!!$                  case (ESMF_R4%dkind)
!!$                    call ESMF_FieldWriteFileNetCDF1DR4(field, grid, out_array, out_counts, &
!!$                                    out_lbounds, out_ubounds, date, iospec, rc=status)
!!$                  case (ESMF_R8%dkind)
!!$                    call ESMF_FieldWriteFileNetCDF1DR8(field, grid, out_array, out_counts, &
!!$                                    out_lbounds, out_ubounds, date, iospec, rc=status)
                  case default
                    print *, "unsupported kind"
                end select

              case (2)
                select case (out_kind)
                  case (ESMF_R4%dkind)
                    call ESMF_FieldWriteFileNetCDF2DR4(field, grid, out_array, out_counts, &
                                    out_lbounds, out_ubounds, date, iospec, rc=status)
!!$                  case (ESMF_R8%dkind)
!!$                    call ESMF_FieldWriteFileNetCDF2DR8(field, grid, out_array, out_counts, &
!!$                                    out_lbounds, out_ubounds, date, iospec, rc=status)
                  case default
                    print *, "unsupported kind"
                end select

              case (3)
                select case (out_kind)
                  case (ESMF_R4%dkind)
                    call ESMF_FieldWriteFileNetCDF3DR4(field, grid, out_array, out_counts, &
                                    out_lbounds, out_ubounds, date, iospec, rc=status)
!!$                  case (ESMF_R8%dkind)
!!$                    call ESMF_FieldWriteFileNetCDF3DR8(field, grid, out_array, out_counts, &
!!$                                    out_lbounds, out_ubounds, date, iospec, rc=status)
                  case default
                    print *, "unsupported kind"
                end select

              case (4)
                select case (out_kind)
!!$                  case (ESMF_R4%dkind)
!!$                    call ESMF_FieldWriteFileNetCDF4DR4(field, grid, out_array, out_counts, &
!!$                                    out_lbounds, out_ubounds, date, iospec, rc=status)
!!$                  case (ESMF_R8%dkind)
!!$                    call ESMF_FieldWriteFileNetCDF4DR8(field, grid, out_array, out_counts, &
!!$                                    out_lbounds, out_ubounds, date, iospec, rc=status)
                  case default
                    print *, "unsupported kind"
                end select

              case (5)
                select case (out_kind)
!!$                  case (ESMF_R4%dkind)
!!$                    call ESMF_FieldWriteFileNetCDF5DR4(field, grid, out_array, out_counts, &
!!$                                    out_lbounds, out_ubounds, date, iospec, rc=status)
!!$                  case (ESMF_R8%dkind)
!!$                    call ESMF_FieldWriteFileNetCDF5DR8(field, grid, out_array, out_counts, &
!!$                                    out_lbounds, out_ubounds, date, iospec, rc=status)
                  case default
                    print *, "unsupported kind"
                end select

              case (6)
                select case (out_kind)
!!$                  case (ESMF_R4%dkind)
!!$                    call ESMF_FieldWriteFileNetCDF6DR4(field, grid, out_array, out_counts, &
!!$                                    out_lbounds, out_ubounds, date, iospec, rc=status)
!!$                  case (ESMF_R8%dkind)
!!$                    call ESMF_FieldWriteFileNetCDF6DR8(field, grid, out_array, out_counts, &
!!$                                    out_lbounds, out_ubounds, date, iospec, rc=status)
                  case default
                    print *, "unsupported kind"
                end select

              case (7)
                select case (out_kind)
!!$                  case (ESMF_R4%dkind)
!!$                    call ESMF_FieldWriteFileNetCDF7DR4(field, grid, out_array, out_counts, &
!!$                                    out_lbounds, out_ubounds, date, iospec, rc=status)
!!$                  case (ESMF_R8%dkind)
!!$                    call ESMF_FieldWriteFileNetCDF7DR8(field, grid, out_array, out_counts, &
!!$                                    out_lbounds, out_ubounds, date, iospec, rc=status)
                  case default
                    print *, "unsupported kind"
                end select

              case default
                print *, "unsupported rank"
            end select
          case default
            print *, "unsupported type"
         end select

      endif ! (de_id .eq. 0) then  

        call ESMF_ArrayDestroy(out_array, status)
        if (rcpresent) rc = ESMF_SUCCESS
        
      end subroutine ESMF_FieldWrite


!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_FieldWriteFileNetCDF2DR4 - Write a Field to external storage
!
! !INTERFACE:
      subroutine ESMF_FieldWriteFileNetCDF2DR4(field, grid, array, counts, & 
                                 lbounds, ubounds, date, iospec, rc)
!
! !ARGUMENTS:
      type(ESMF_Field), intent(in) :: field 
      type(ESMF_Grid), intent(in) :: grid
      type(ESMF_Array), intent(in) :: array
        integer, dimension(:), intent(in) :: counts
        integer, dimension(:), intent(in) :: lbounds
        integer, dimension(:), intent(in) :: ubounds
      character (19) date
      type(ESMF_IOSpec), intent(in), optional :: iospec
      integer, intent(out), optional :: rc  
!
! !DESCRIPTION:
!      Used to write data to persistent storage in a variety of formats.  
!      (see WriteRestart/ReadRestart for quick data dumps.)  Details of I/O 
!      options specified in the IOSpec derived type. 
!
!
!     The arguments are:
!     \begin{description}
!     \item [name]
!           An {\tt ESMF\_Field} name.
!     \item [{[subset]}]
!            {\tt ESMF\_Subset}.
!     \item [{[iospec]}]
!            I/O specification.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

        ! Local variables
        logical :: rcpresent
      integer Comm
      integer IOComm
      type(ESMF_CoordOrder) :: order
        type(ESMF_VM) :: vm
        character (80) SysDepInfo
        integer     :: DataHandle
        integer DomDesc
        character*2 MemOrd
        character*2 Stagger
        character*31, dimension(2) :: DimNames
        character(len=ESMF_MAXSTR) :: filename
        real(kind=ESMF_KIND_R4), dimension(:,:), pointer :: data_ptr
        integer wrf_type
        integer status

        ! call ESMF_Log(?, 'entry into ESMF_FieldWrite');

        ! Set initial values
        status = ESMF_FAILURE 
        rcpresent = .FALSE.

        wrf_type = WRF_REAL

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

! For now we set the communicator to be MPI_COMM_WORLD.  Eventually,
! we will pull it from the DE.  
! We are also not taking advantage of the ability to have a different
! communicator just for IO.  So we set the IO communicator to that of
! the field DE.
        call ESMF_VMGetGlobal(vm, rc=Status)
        call ESMF_VMGet(vm, mpiCommunicator=comm, rc=rc)
!!$        Comm = MPI_COMM_WORLD
        IOComm = Comm
  
        Stagger = ''

! This is a WRF string used to pass additional control information to
! the I/O interface.  Currently it's only setting is to describe if the
! DATASET is a RESTART, HISTORY, BOUNDARY condition or INITIAL condition
! field.
! FieldWrite does not currently have a use for this feature and always
! uses the same I/O stream.         
        SysDepInfo = 'DATASET=HISTORY'

! This is a required WRF variable to "that may be used to pass a
! communication package specific domain."  
! For now it is set to 0 which is a null setting.
        DomDesc = 0
  
! Possibly add this as an ESMF_IOSpec item.
! Hardwire it for now.
! The default should probably be based off of Grid coord_order
        call ESMF_GridGet(grid, coordOrder=order)
        if (order == ESMF_COORD_ORDER_XYZ) then
           MemOrd = "XY"
           DimNames(1) = 'X'
           DimNames(2) = 'Y'
        
        elseif (order == ESMF_COORD_ORDER_XZY) then
           MemOrd = "XZ"
           DimNames(1) = 'X'
           DimNames(2) = 'Z'
        
        elseif (order == ESMF_COORD_ORDER_YXZ) then
           MemOrd = "YX"
           DimNames(1) = 'Y'
           DimNames(2) = 'X'
        
        elseif (order == ESMF_COORD_ORDER_YZX) then
           MemOrd = "YZ"
           DimNames(1) = 'Y'
           DimNames(2) = 'Z'
        
        elseif (order == ESMF_COORD_ORDER_ZXY) then
           MemOrd = "ZX"
           DimNames(1) = 'Z'
           DimNames(2) = 'X'
        
        elseif (order == ESMF_COORD_ORDER_ZYX) then
           MemOrd = "ZY"
           DimNames(1) = 'Z'
           DimNames(2) = 'Y'
        
        elseif (order == ESMF_COORD_ORDER_UNKNOWN) then
           print*, 'Assuming XY coordinate ordering.'
           MemOrd = "XY"
           DimNames(1) = 'X'
           DimNames(2) = 'Y'

        else
           print*, "Error getting coordinate order for output."
           return
        endif

        call ESMF_ArrayGetData( array, data_ptr, ESMF_DATA_REF, rc)

! Initialize the output stream.
           call ext_ncd_ioinit(Status)
           print *,'After call ext_ncd_ioinit, Status =',Status
      
           print*,'!!!!!!!!!!!!!!!!!!!!!!! ext_ncd_open_for_write_begin'

! To write multiple times into the same file, the I/O stream will
! probably need a separate initialize routine.  Upon initialization,
! 'DataHandle' will be carried around, perhaps in ESMF_IOSpec, to
! reaccess the file.
           call ext_ncd_open_for_write_begin( FileName, Comm, IOComm, SysDepInfo, DataHandle, Status)
           print *, ' ext_ncd_open_for_write_begin Status = ',Status,DataHandle
           
! There needs to be a block of code or function that converts the
! ESMF variable types into the corresponding WRF types. 
! i.e. ESMF_KIND_R4 => WRF_REAL
! This call 'trains' the output library but does not write out any data.
           call ext_ncd_write_field(DataHandle,Date,filename,data_ptr,wrf_type,Comm,IOComm,DomDesc,&
                &'MemOrd',Stagger,DimNames,lbounds,ubounds,lbounds,ubounds,&
                &lbounds,ubounds,Status)
           print *,'             dry run : ext_ncd_write_field Status = ',Status
  
           call ext_ncd_open_for_write_commit(DataHandle, Status)
           print *, '             ext_ncd_open_for_write_commit Status = ', Status,DataHandle
  
! This call does output the data.
           call ext_ncd_write_field(DataHandle,Date,filename,data_ptr,wrf_type,Comm,IOComm,DomDesc,&
                &'MemOrd',Stagger,DimNames,lbounds,ubounds,lbounds,ubounds,&
                &lbounds,ubounds,Status)
           print *,'              first write: ext_ncd_write_field Status = ',Status
  
! For writing multiple times to the same file, these calls will have
! to be placed in a separate close routine to release the handle on
! the IO stream.
           call ext_ncd_ioclose( DataHandle, Status)
           print *, '             After ext_ncd_ioclose, Status = ',Status
           call ext_ncd_ioexit(Status)
           print *,'              After ext_ncd_ioexit, Status = ',Status

         end subroutine ESMF_FieldWriteFileNetCDF2DR4
        

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_FieldWriteFileNetCDF3DR4 - Write a Field to external storage
!
! !INTERFACE:
      subroutine ESMF_FieldWriteFileNetCDF3DR4(field, grid, array, counts, & 
                                 lbounds, ubounds, date, iospec, rc)
!
! !ARGUMENTS:
      type(ESMF_Field), intent(in) :: field 
      type(ESMF_Grid), intent(in) :: grid
      type(ESMF_Array), intent(in) :: array
        integer, dimension(:), intent(in) :: counts
        integer, dimension(:), intent(in) :: lbounds
        integer, dimension(:), intent(in) :: ubounds
      character (19) date
      type(ESMF_IOSpec), intent(in), optional :: iospec
      integer, intent(out), optional :: rc  
!
! !DESCRIPTION:
!      Used to write data to persistent storage in a variety of formats.  
!      (see WriteRestart/ReadRestart for quick data dumps.)  Details of I/O 
!      options specified in the IOSpec derived type. 
!
!
!     The arguments are:
!     \begin{description}
!     \item [name]
!           An {\tt ESMF\_Field} name.
!     \item [{[subset]}]
!            {\tt ESMF\_Subset}.
!     \item [{[iospec]}]
!            I/O specification.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

        ! Local variables
        logical :: rcpresent
      integer Comm
      integer IOComm
      type(ESMF_CoordOrder) :: order
        type(ESMF_VM) :: vm
        character (80) SysDepInfo
        integer     :: DataHandle
        integer DomDesc
        character*3 MemOrd
        character*3 Stagger
        character*31, dimension(3) :: DimNames
        character(len=ESMF_MAXSTR) :: filename
        real(kind=ESMF_KIND_R4), dimension(:,:,:), pointer :: data_ptr
        integer wrf_type
        integer status

        ! call ESMF_Log(?, 'entry into ESMF_FieldWrite');

        ! Set initial values
        status = ESMF_FAILURE 
        rcpresent = .FALSE.

        wrf_type = WRF_REAL

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

! For now we set the communicator to be MPI_COMM_WORLD.  Eventually,
! we will pull it from the DE.  
! We are also not taking advantage of the ability to have a different
! communicator just for IO.  So we set the IO communicator to that of
! the field DE.
        call ESMF_VMGetGlobal(vm, rc=Status)
        call ESMF_VMGet(vm, mpiCommunicator=comm, rc=rc)
!!$        Comm = MPI_COMM_WORLD
        IOComm = Comm
  
        Stagger = ''

! This is a WRF string used to pass additional control information to
! the I/O interface.  Currently it's only setting is to describe if the
! DATASET is a RESTART, HISTORY, BOUNDARY condition or INITIAL condition
! field.
! FieldWrite does not currently have a use for this feature and always
! uses the same I/O stream.         
        SysDepInfo = 'DATASET=HISTORY'

! This is a required WRF variable to "that may be used to pass a
! communication package specific domain."  
! For now it is set to 0 which is a null setting.
        DomDesc = 0
  
! Possibly add this as an ESMF_IOSpec item.
! Hardwire it for now.
! The default should probably be based off of Grid coord_order
        call ESMF_GridGet(grid, coordOrder=order)
        if (order == ESMF_COORD_ORDER_XYZ) then
           MemOrd = "XYZ"
           DimNames(1) = 'X'
           DimNames(2) = 'Y'
           DimNames(3) = 'Z'
        
        elseif (order == ESMF_COORD_ORDER_XZY) then
           MemOrd = "XZY"
           DimNames(1) = 'X'
           DimNames(2) = 'Z'
           DimNames(3) = 'Y'
        
        elseif (order == ESMF_COORD_ORDER_YXZ) then
           MemOrd = "YXZ"
           DimNames(1) = 'Y'
           DimNames(2) = 'X'
           DimNames(3) = 'Z'
        
        elseif (order == ESMF_COORD_ORDER_YZX) then
           MemOrd = "YZX"
           DimNames(1) = 'Y'
           DimNames(2) = 'Z'
           DimNames(3) = 'X'
        
        elseif (order == ESMF_COORD_ORDER_ZXY) then
           MemOrd = "ZXY"
           DimNames(1) = 'Z'
           DimNames(2) = 'X'
           DimNames(3) = 'Y'
        
        elseif (order == ESMF_COORD_ORDER_ZYX) then
           MemOrd = "ZYX"
           DimNames(1) = 'Z'
           DimNames(2) = 'Y'
           DimNames(3) = 'X'
        
        elseif (order == ESMF_COORD_ORDER_UNKNOWN) then
           print*, 'Assuming XYZ coordinate ordering.'
           MemOrd = "XYZ"
           DimNames(1) = 'X'
           DimNames(2) = 'Y'
           DimNames(3) = 'Z'

        else
           print*, "Error getting coordinate order for output."
           return
        endif

        call ESMF_ArrayGetData( array, data_ptr, ESMF_DATA_REF, rc)

! Initialize the output stream.
           call ext_ncd_ioinit(Status)
           print *,'After call ext_ncd_ioinit, Status =',Status
      
           print*,'!!!!!!!!!!!!!!!!!!!!!!! ext_ncd_open_for_write_begin'

! To write multiple times into the same file, the I/O stream will
! probably need a separate initialize routine.  Upon initialization,
! 'DataHandle' will be carried around, perhaps in ESMF_IOSpec, to
! reaccess the file.
           call ext_ncd_open_for_write_begin( FileName, Comm, IOComm, SysDepInfo, DataHandle, Status)
           print *, ' ext_ncd_open_for_write_begin Status = ',Status,DataHandle
           
! There needs to be a block of code or function that converts the
! ESMF variable types into the corresponding WRF types. 
! i.e. ESMF_KIND_R4 => WRF_REAL
! This call 'trains' the output library but does not write out any data.
           call ext_ncd_write_field(DataHandle,Date,filename,data_ptr,wrf_type,Comm,IOComm,DomDesc,&
                &'MemOrd',Stagger,DimNames,lbounds,ubounds,lbounds,ubounds,&
                &lbounds,ubounds,Status)
           print *,'             dry run : ext_ncd_write_field Status = ',Status
  
           call ext_ncd_open_for_write_commit(DataHandle, Status)
           print *, '             ext_ncd_open_for_write_commit Status = ', Status,DataHandle
  
! This call does output the data.
           call ext_ncd_write_field(DataHandle,Date,filename,data_ptr,wrf_type,Comm,IOComm,DomDesc,&
                &'MemOrd',Stagger,DimNames,lbounds,ubounds,lbounds,ubounds,&
                &lbounds,ubounds,Status)
           print *,'              first write: ext_ncd_write_field Status = ',Status
  
! For writing multiple times to the same file, these calls will have
! to be placed in a separate close routine to release the handle on
! the IO stream.
           call ext_ncd_ioclose( DataHandle, Status)
           print *, '             After ext_ncd_ioclose, Status = ',Status
           call ext_ncd_ioexit(Status)
           print *,'              After ext_ncd_ioexit, Status = ',Status

         end subroutine ESMF_FieldWriteFileNetCDF3DR4
        

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_FieldWriteFileASCII - Write a Field to external storage
!
! !INTERFACE:
      subroutine ESMF_FieldWriteFileASCII(field, & ! subset, 
                                 iospec, rc)
!
! !ARGUMENTS:
      type(ESMF_Field), intent(in) :: field 
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
!     The arguments are:
!     \begin{description}
!     \item [name]
!           An {\tt ESMF\_Field} name.
!     \item [{[subset]}]
!            {\tt ESMF\_Subset}.
!     \item [{[iospec]}]
!            I/O specification.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

        ! Local variables
        integer :: status, de_id
        logical :: rcpresent
        type(ESMF_Array) :: outarray
        type(ESMF_Grid) :: grid
        type(ESMF_DELayout) :: delayout
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
        call ESMF_GridGet(grid, delayout=delayout, rc=status)
        call ESMF_DELayoutGet(delayout, localDE=de_id, rc=status)

        ! Output to file, from de_id 0 only
        call ESMF_ArrayGather(field%ftypep%localfield%localdata, &
                              field%ftypep%grid, field%ftypep%mapping, &
                              0, outarray, rc=status)
        !call ESMF_FieldAllGather(field, outarray, rc=status)
        if (de_id .eq. 0) then       
            call ESMF_ArrayWrite(outarray, filename=filename, rc=status)
            call ESMF_ArrayDestroy(outarray, status)
        endif

      end subroutine ESMF_FieldWriteFileASCII
        

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldWriteRestart"

!BOPI
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
!     The arguments are:
!     \begin{description}
!     \item [field]
!           An {\tt ESMF\_Field} object.
!     \item [{[iospec]}]
!            I/O specification.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI


!       BOP/EOP have been changed to BOPI/EOPI until the subroutine is implemented.
!
! TODO: code goes here
!
        end subroutine ESMF_FieldWriteRestart


!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!
! This section includes all Field internal methods.
!
!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldConstructNew"

!BOPI
! !IROUTINE: ESMF_FieldConstructNew - Construct the internals of a Field

! !INTERFACE:
      subroutine ESMF_FieldConstructNew(ftype, grid, arrayspec, allocflag, &
                                        horzRelloc, vertRelloc, haloWidth, &
                                        datamap, name, iospec, rc)
!
! !ARGUMENTS:
      type(ESMF_FieldType), pointer :: ftype 
      type(ESMF_Grid) :: grid               
      type(ESMF_ArraySpec), intent(in) :: arrayspec     
      type(ESMF_AllocFlag), intent(in), optional :: allocflag
      type(ESMF_RelLoc), intent(in), optional :: horzRelloc 
      type(ESMF_RelLoc), intent(in), optional :: vertRelloc 
      integer, intent(in), optional :: haloWidth
      type(ESMF_FieldDataMap), intent(in), optional :: datamap           
      character (len=*), intent(in), optional :: name
      type(ESMF_IOSpec), intent(in), optional :: iospec 
      integer, intent(out), optional :: rc              
!
! !DESCRIPTION:
! 
!     Constructs all {\tt ESMF\_Field} internals, including the allocation
!     of a data {\tt ESMF\_Array}.  
!
!     The arguments are:
!     \begin{description}
!     \item [ftype]
!           Pointer to an {\tt ESMF\_Field} object.
!     \item [grid] 
!           Pointer to an {\tt ESMF\_Grid} object. 
!     \item [arrayspec]
!           Data specification. 
!     \item [{[allocflag]}]
!           Allocate space for data array or not.  For possible values
!           see Section~\ref{opt:allocflag}.
!     \item [{[horzRelloc]}] 
!           Relative location of data per grid cell/vertex in the horizontal
!           grid.  If a relative location is specified both as an argument
!           here as well as set in the {\tt datamap}, this takes priority.
!     \item [{[vertRelloc]}] 
!           Relative location of data per grid cell/vertex in the vertical grid.
!           If a relative location is specified both as an argument
!           here as well as set in the {\tt datamap}, this takes priority.
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


      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present
      type(ESMF_Array) :: array                   ! New array
      type(ESMF_RelLoc) :: hRelLoc
      integer, dimension(ESMF_MAXDIM) :: gridcounts, arraycounts
      integer, dimension(ESMF_MAXDIM) :: dimorder, counts
      integer :: hwidth
      integer :: i, j, arrayRank, gridRank

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

      call ESMF_FieldConstructNoArray(ftype, grid, horzRelloc, vertRelloc, &
                                      hwidth, datamap, name, iospec, status)
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      call ESMF_ArraySpecGet(arrayspec, rank=arrayRank, rc=status)
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return
      call ESMF_GridGet(grid, dimCount=gridRank, rc=status)
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      ! make sure hRelLoc has a value before GridGetDELocalInfo call
      if (present(horzRelLoc)) then
          hRelLoc = horzRelloc
      else
          if (present(datamap)) then
              call ESMF_FieldDataMapGet(datamap, horzRelLoc=hRelLoc, rc=status)
          else
               if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                       "no valid RelLoc in either argument list or datamap", &
                                 ESMF_CONTEXT, rc)) return
          endif
      endif
      call ESMF_GridGetDELocalInfo(grid, horzRelLoc=hRelLoc, &
                                   vertRelLoc=vertRelLoc, &
                                   localCellCountPerDim=gridcounts(1:gridRank), &
                                   rc=status)
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      ! get information back from datamap
      call ESMF_FieldDataMapGet(ftype%mapping, dataIndexList=dimorder, &
                                                    counts=counts, rc=status)

      arraycounts(:) = 1
      j = 1
      do i=1, arrayRank
         if (dimorder(i) .eq. 0) then
            arraycounts(i) = counts(j) 
            j = j + 1
         else
            arraycounts(i) = gridcounts(dimorder(i)) + (2 * hwidth)
         endif
      enddo

      array = ESMF_ArrayCreate(arrayspec, arraycounts, hwidth, rc=status) 
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      ftype%localfield%localdata = array
      ftype%datastatus = ESMF_STATUS_READY

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldConstructNew

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldConstructNewArray"

!BOPI
! !IROUTINE: ESMF_FieldConstructNewArray - Construct the internals of a Field

! !INTERFACE:
      subroutine ESMF_FieldConstructNewArray(ftype, grid, array, horzRelloc, &
                                             vertRelloc, haloWidth, datamap, &
                                             name, iospec, rc)
!
! !ARGUMENTS:
      type(ESMF_FieldType), pointer :: ftype 
      type(ESMF_Grid) :: grid               
      type(ESMF_Array), intent(in) :: array     
      type(ESMF_RelLoc), intent(in), optional :: horzRelloc 
      type(ESMF_RelLoc), intent(in), optional :: vertRelloc 
      integer, intent(in), optional :: haloWidth
      type(ESMF_FieldDataMap), intent(in), optional :: datamap           
      character (len=*), intent(in), optional :: name
      type(ESMF_IOSpec), intent(in), optional :: iospec 
      integer, intent(out), optional :: rc              
!
! !DESCRIPTION:
! 
!     Constructs all {\tt ESMF\_Field} internals, including the allocation
!     of a data {\tt ESMF\_Array}.  
!
!     The arguments are:
!     \begin{description}
!     \item [ftype]
!           Pointer to an {\tt ESMF\_Field} object.
!     \item [grid] 
!           Pointer to an {\tt ESMF\_Grid} object. 
!     \item [array]
!           Data. 
!     \item [{[horzRelloc]}] 
!           Relative location of data per grid cell/vertex in the horizontal
!           grid.
!     \item [{[vertRelloc]}] 
!           Relative location of data per grid cell/vertex in the vertical grid.
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
!     \end{description}
!
!EOPI


      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present

      ! Initialize return code   
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent = .TRUE. 
        rc = ESMF_FAILURE
      endif     

      call ESMF_FieldConstructNoArray(ftype, grid, horzRelloc, vertRelloc, &
                                      haloWidth, datamap, name, iospec, status)
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      call ESMF_ArrayValidate(array, "", status)
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return
      ftype%localfield%localdata = array
      !ftype%localfield%datastatus = ESMF_STATUS_READY
      ftype%datastatus = ESMF_STATUS_READY

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldConstructNewArray

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldConstructNoDataPtr"

!BOPI
! !IROUTINE: ESMF_FieldConstructNoDataPtr - Construct a Field with no associated buffer

! !INTERFACE:
      subroutine ESMF_FieldConstructNoDataPtr(ftype, grid, arrayspec, &
                                           horzRelloc, vertRelloc, haloWidth, &
                                           datamap, name, iospec, rc)
!
! !ARGUMENTS:     
      type(ESMF_FieldType), pointer :: ftype                
      type(ESMF_Grid) :: grid               
      type(ESMF_ArraySpec), intent(in) :: arrayspec     
      type(ESMF_RelLoc), intent(in), optional :: horzRelloc 
      type(ESMF_RelLoc), intent(in), optional :: vertRelloc 
      integer, intent(in), optional :: haloWidth
      type(ESMF_FieldDataMap), intent(in), optional :: datamap 
      character (len=*), intent(in), optional :: name
      type(ESMF_IOSpec), intent(in), optional :: iospec 
      integer, intent(out), optional :: rc              
!
! !DESCRIPTION:
! 
!     Constructs all {\tt ESMF\_Field} internals except for the assignment of 
!     an associated data buffer.
!
!     The arguments are:
!     \begin{description}
!     \item [ftype]
!           Pointer to an {\tt ESMF\_Field} object.
!     \item [grid] 
!           Pointer to an {\tt ESMF\_Grid} object. 
!     \item [arrayspec]
!           Data specification. 
!     \item [{[horzRelloc]}] 
!           Relative location of data per grid cell/vertex in the horizontal
!           grid.
!     \item [{[vertRelloc]}] 
!           Relative location of data per grid cell/vertex in the vertical grid.
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
!     \end{description}
!
!EOPI


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
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      ! TODO: Check to see grid is valid first.

      call ESMF_GridValidate(grid, "", status)
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return
      ftype%grid = grid
      ftype%gridstatus = ESMF_STATUS_READY

      call ESMF_GridGet(grid, dimCount=gridRank, rc=status)
      if (present(datamap)) then
        ftype%mapping = datamap   ! copy, datamap can be reused by user now
        ! if specified as explicit args to create, they override anything
        ! in the existing datamap
        call ESMF_FieldDataMapSet(ftype%mapping, horzRelloc=horzRelloc, &
                             vertRelloc=vertRelloc, rc=status)
      else
        if (gridRank .eq. 1) then
          call ESMF_FieldDataMapSetDefault(ftype%mapping, ESMF_INDEX_I, &
                                horzRelloc=horzRelloc, &
                                vertRelloc=vertRelloc, rc=status)
        else if (gridRank .eq. 2) then
          call ESMF_FieldDataMapSetDefault(ftype%mapping, ESMF_INDEX_IJ, &
                                horzRelloc=horzRelloc, &
                                vertRelloc=vertRelloc, rc=status)
        else if (gridRank .eq. 3) then
          call ESMF_FieldDataMapSetDefault(ftype%mapping, ESMF_INDEX_IJK, &
                                horzRelloc=horzRelloc, &
                                vertRelloc=vertRelloc, rc=status)
        endif
      endif

      !call ESMF_ArrayConstructNoDataPtr(ftype%array)

      ! If I/O spec is present, copy it into the field object; otherwise just 
      ! initialize the I/O spec in the field object.
      if(present(iospec)) then
        !ESMF_IOSpecCopyInit(ftype%iospec, iospec, status)
        !if (ESMF_LogMsgFoundError(status, &
        !                          ESMF_ERR_PASSTHRU, &
        !                          ESMF_CONTEXT, rc)) return
      else 
        !ESMF_IOSpecInit(ftype%iospec, status)
        !if (ESMF_LogMsgFoundError(status, &
        !                          ESMF_ERR_PASSTHRU, &
        !                          ESMF_CONTEXT, rc)) return
      endif

      ftype%fieldstatus = ESMF_STATUS_READY

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldConstructNoDataPtr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldConstructNoArray"

!BOPI
! !IROUTINE: ESMF_FieldConstructNoArray - Construct a Field with no associated Array

! !INTERFACE:
      subroutine ESMF_FieldConstructNoArray(ftype, grid, horzRelloc, &
                                            vertRelloc, haloWidth, &
                                            datamap, name, iospec, rc)
!
! !ARGUMENTS:     
      type(ESMF_FieldType), pointer :: ftype   
      type(ESMF_Grid) :: grid                 
      type(ESMF_RelLoc), intent(in), optional :: horzRelloc 
      type(ESMF_RelLoc), intent(in), optional :: vertRelloc 
      integer, intent(in), optional :: haloWidth
      type(ESMF_FieldDataMap), intent(in), optional :: datamap              
      character (len=*), intent(in), optional :: name    
      type(ESMF_IOSpec), intent(in), optional :: iospec  
      integer, intent(out), optional :: rc               
!
! !DESCRIPTION:
! 
! Constructs an {\tt ESMF\_Field} except for its internal data {\tt ESMF\_Array}.
!
!     The arguments are:
!     \begin{description}
!     \item [ftype]
!           Pointer to an {\tt ESMF\_Field} object.
!     \item [grid] 
!           Pointer to an {\tt ESMF\_Grid} object. 
!     \item [{[horzRelloc]}] 
!           Relative location of data per grid cell/vertex in the horizontal
!           grid.
!     \item [{[vertRelloc]}] 
!           Relative location of data per grid cell/vertex in the vertical grid.
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
!     \end{description}
!
!EOPI


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
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      ! Attach grid
      call ESMF_GridValidate(grid, "", status)
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return
      ftype%grid = grid
      ftype%gridstatus = ESMF_STATUS_READY

      call ESMF_GridGet(grid, dimCount=gridRank, rc=status)
      if (present(datamap)) then
        ! this does a copy, datamap ok for user to delete now
        ftype%mapping = datamap   

        ! take care of override horz and vert rellocs.  if specified both in
        ! the datamap and as explicit args, the arguments take priority.
        call ESMF_FieldDataMapSet(ftype%mapping, horzRelloc=horzRelloc, &
                             vertRelloc=vertRelloc, rc=status)
      else
        ! create default datamap with 1-for-1 correspondence to grid
        if (gridRank .eq. 1) then
          call ESMF_FieldDataMapSetDefault(ftype%mapping, ESMF_INDEX_I, &
                                horzRelloc=horzRelloc, &
                                vertRelloc=vertRelloc, rc=status)
        else if (gridRank .eq. 2) then
          call ESMF_FieldDataMapSetDefault(ftype%mapping, ESMF_INDEX_IJ, &
                                horzRelloc=horzRelloc, &
                                vertRelloc=vertRelloc, rc=status)
        else if (gridRank .eq. 3) then
          call ESMF_FieldDataMapSetDefault(ftype%mapping, ESMF_INDEX_IJK, &
                                horzRelloc=horzRelloc, &
                                vertRelloc=vertRelloc, rc=status)
        endif
      endif

!
! add more code here
!
     
      ftype%fieldstatus = ESMF_STATUS_READY

      if (rcpresent) rc = ESMF_SUCCESS
      
      end subroutine ESMF_FieldConstructNoArray

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldConstructNoGridArray"

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
!
!     The arguments are:
!     \begin{description}
!     \item [ftypep]
!           Pointer to an {\tt ESMF\_Field} object.
!     \item [{[name]}]
!           {\tt ESMF\_Field} name.
!     \item [{[iospec]}]
!           {\tt ESMF\_Field} I/O specification.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI


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
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      ! Initialize field contents
      !ftypep%localfield%gridstatus = ESMF_STATUS_UNINIT
      !ftypep%localfield%datastatus = ESMF_STATUS_UNINIT
      ftypep%gridstatus = ESMF_STATUS_UNINIT
      ftypep%datastatus = ESMF_STATUS_UNINIT

      ! Set the mapping as unknown/invalid
      call ESMF_FieldDataMapSetInvalid(ftypep%mapping, status)

      ftypep%fieldstatus = ESMF_STATUS_READY

!
! add more code here
!
     
      if (rcpresent) rc = ESMF_SUCCESS
      
      end subroutine ESMF_FieldConstructNoGridArray

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldDestruct"

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
!     The arguments are:
!     \begin{description}
!     \item [ftype]
!           Pointer to an {\tt ESMF\_Field} object.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI

      integer :: status
      logical :: rcpresent                          ! Return code present

      ! Initialize return code; assume failure until success is certain
      rcpresent = .FALSE.
      if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
      endif


      ! release the base class resources
      call ESMF_BaseDestroy(ftype%base, status)
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

!
! TODO: more code goes here
!


      if (rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldDestruct
!
!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldBoxIntersect"

!BOPI
! !IROUTINE: ESMF_FieldBoxIntersect - Intersect bounding boxes
!
! !INTERFACE:
      subroutine ESMF_FieldBoxIntersect(srcField, dstField, recvDomainlist, &
                                        sendDomainList, rc)
!
! !ARGUMENTS:
      type(ESMF_Field), intent(in) :: srcField 
      type(ESMF_Field), intent(in) :: dstField
      type(ESMF_DomainList), intent(inout) :: recvDomainlist
      type(ESMF_DomainList), intent(inout) :: sendDomainlist
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!      Clips the src\_field physgrid box against the clip\_field, i.e. returns
!      a description of the area in clip\_field which is necessary to cover the
!      desired area in src\_field.  This procedure is mostly an entry point;
!      most of the work is done in the {\tt ESMF\_Grid} class.
!
!     The arguments are:
!     \begin{description}
!     \item [srcField]
!           Source {\tt ESMF\_Field} object.
!     \item [dstField]
!           Destination {\tt ESMF\_Field} object.
!     \item [recvDomainlist]
!           Receive domain list.
!     \item [sendDomainlist]
!           Send domain list.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI

      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present
      integer :: my_DE, my_dst_DE, my_src_DE, gridrank
      real(ESMF_KIND_R8), dimension(ESMF_MAXGRIDDIM) :: dst_min, dst_max
      real(ESMF_KIND_R8), dimension(ESMF_MAXGRIDDIM) :: src_min, src_max
      logical :: hassrcdata        ! does this DE contain localdata from src?
      logical :: hasdstdata        ! does this DE contain localdata from dst?
      type(ESMF_AxisIndex), dimension(ESMF_MAXGRIDDIM) :: myAI
      type(ESMF_DELayout) :: parentDElayout, srcDElayout, dstDElayout
      type(ESMF_FieldType) :: stypep, dtypep      ! field type info
      type(ESMF_Grid) :: srcGrid, dstGrid
      type(ESMF_Logical) :: hasdata        ! does this DE contain localdata?
      type(ESMF_RelLoc) :: horzRelLoc, vertRelLoc 

      ! Initialize return code
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_FAILURE
      endif

      stypep = srcField%ftypep
      dtypep = dstField%ftypep

      ! Our DE number in the parent layout  TODO: for now, just use one of the
      !                                           grid layouts.  In the future,
      !                                           there will be proxy grids on all
      !                                           DEs of the coupler layout and a
      !                                           different method for determining
      !                                           if my_DE is part of a layout
      call ESMF_GridGet(stypep%grid, delayout=parentDElayout, rc=status)
      call ESMF_DELayoutGet(parentDElayout, localDE=my_DE, rc=status)

      ! TODO: we need not only to know if this DE has data in the field,
      !   but also the de id for both src & dest fields

      ! This routine is called on every processor in the parent layout.
      !  It is quite possible that the source and destination fields do
      !  not completely cover every processor on that layout.  Make sure
      !  we do not go lower than this on the processors which are uninvolved
      !  in this communication.

      ! if srclayout ^ parentlayout == NULL, nothing to send from this DE id.
      call ESMF_GridGet(stypep%grid, delayout=srcDElayout, rc=status)
  !jw    call ESMF_DELayoutGetDEExists(parentDElayout, my_DE, srcDElayout, hasdata)
      hassrcdata = (hasdata .eq. ESMF_TRUE)
      hassrcdata = .true.   ! temp for now
      if (hassrcdata) then
        ! don't ask for our de number if this de isn't part of the layout
        call ESMF_DELayoutGet(srcDElayout, localDE=my_src_DE, rc=status)
        call ESMF_GridGet(stypep%grid, dimCount=gridrank, rc=status)
        call ESMF_FieldGet(srcField, horzRelloc=horzRelLoc, &
                           vertRelloc=vertRelLoc, rc=rc)
        call ESMF_GridGetDELocalAI(stypep%grid, myAI(1:gridrank), horzRelLoc, &
                                   vertRelLoc=vertRelLoc, rc=status)
      endif

      ! if dstlayout ^ parentlayout == NULL, nothing to recv on this DE id.
      call ESMF_GridGet(dtypep%grid, delayout=dstDElayout, rc=status)
   !jw   call ESMF_DELayoutGetDEExists(parentlayout, my_DE, dstlayout, hasdata)
      hasdstdata = (hasdata .eq. ESMF_TRUE)
      hasdstdata = .true.   ! temp for now
      if (hasdstdata) then
        ! don't ask for our de number if this de isn't part of the layout
        call ESMF_DELayoutGet(dstDElayout, localDE=my_dst_DE, rc=status)
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
        call ESMF_FieldGet(srcField, grid=srcGrid, rc=status)
        if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return
        ! From the grid get the bounding box on this DE
        call ESMF_FieldGet(srcField, horzRelloc=horzRelLoc, &
                           vertRelloc=vertRelLoc, rc=rc)
        call ESMF_GridGetDELocalInfo(srcGrid, &
                                     horzRelLoc=horzRelLoc, &
                                     vertRelLoc=vertRelLoc, &
                                     minLocalCoordPerDim=src_min, &
                                     maxLocalCoordPerDim=src_max, rc=status)
        call ESMF_GridBoxIntersectSend(dstGrid, srcGrid, src_min, src_max, &
                                       myAI, sendDomainList, status)
      endif

      ! if dst field exists on this DE, query it for information
      if (hasdstdata) then
        ! Query the datamap and set info for grid so it knows how to
        ! match up the array indices and the grid indices.
        call ESMF_FieldGet(dstField, grid=dstGrid, rc=status)
        if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return
        ! From the grid get the bounding box on this DE
        call ESMF_FieldGet(dstField, horzRelloc=horzRelLoc, &
                           vertRelloc=vertRelLoc, rc=rc)
        call ESMF_GridGetDELocalInfo(dstGrid, &
                                     horzRelLoc=horzRelLoc, &
                                     vertRelLoc=vertRelLoc, &
                                     minLocalCoordPerDim=dst_min, &
                                     maxLocalCoordPerDim=dst_max, rc=status)
        call ESMF_GridBoxIntersectRecv(srcGrid, dst_min, dst_max, &
                                       recvDomainList, rc=status)
      endif

      if (rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldBoxIntersect

!------------------------------------------------------------------------------

      end module ESMF_FieldMod
