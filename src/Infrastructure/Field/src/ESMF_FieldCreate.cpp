! $Id: ESMF_FieldCreate.cpp,v 1.20 2007/03/01 19:10:01 theurich Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2008, University Corporation for Atmospheric Research, 
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
! Laboratory, University of Michigan, National Centers for Environmental 
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
^define ESMF_FILENAME "ESMF_FieldCreate.F90"
!
!     ESMF FieldCreate module
      module ESMF_FieldCreateMod
!
!==============================================================================
!
! This file contains the Field class methods which are automatically
!  generated from macros to handle the type/kind/rank overloading.
!  See ESMF_Field.F90 for non-macroized functions and subroutines.
!
!------------------------------------------------------------------------------
! INCLUDES
! < ignore blank lines below.  they are created by the files which
!   define various macros. >
^include "ESMF.h"
#include "ESMF_StdCppMacros.h"
#include "ESMF_FieldCreateMacros.h"

!------------------------------------------------------------------------------
! !USES:
      use ESMF_UtilTypesMod
      use ESMF_BaseMod
      use ESMF_LogErrMod
      use ESMF_IOSpecMod
      use ESMF_ArraySpecMod
      use ESMF_LocalArrayMod
      use ESMF_InternArrayDataMapMod
      use ESMF_DELayoutMod
      use ESMF_GridTypesMod
      use ESMF_GridMod
      use ESMF_InternArrayMod
      use ESMF_InternArrayGetMod
      use ESMF_InternArrayCreateMod
      use ESMF_InternArrayCommMod
      use ESMF_FieldDataMapMod
      use ESMF_FieldMod
      use ESMF_FieldGetMod
      implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
      private

!------------------------------------------------------------------------------
! !PUBLIC MEMBER FUNCTIONS:

      public ESMF_FieldCreate
 
!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_FieldCreate.cpp,v 1.20 2007/03/01 19:10:01 theurich Exp $'

!==============================================================================
! 
! INTERFACE BLOCKS
!
!==============================================================================


!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_FieldCreate - Create a new Field with data
!
! !INTERFACE:
      interface ESMF_FieldCreate 
   
! !PRIVATE MEMBER FUNCTIONS:
!
        module procedure ESMF_FieldCreateNew
        module procedure ESMF_FieldCreateFromArray
        module procedure ESMF_FieldCreateRemap

      ! < declarations of interfaces for each T/K/R >
InterfaceMacro(FieldCreateDPtr)

InterfaceMacro(FieldCreateEPtr)

! !DESCRIPTION:
!   This interface provides an entry point for methods that create a complete
!   {\tt ESMF\_Field}.  These method all contain an {\tt ESMF\_Grid} and 
!   {\tt ESMF\_Data}.  The variations allow the user to specify the data 
!   using either a Fortran array or an {\tt ESMF\_Array}.
!    
      end interface
!EOPI

!
!==============================================================================
!
      contains
!
!==============================================================================

!------------------------------------------------------------------------------
^undef  ESMF_METHOD
^define ESMF_METHOD "ESMF_FieldCreateNew"
!BOP
! !IROUTINE:   ESMF_FieldCreate - Create a new Field

! !INTERFACE:
      ! Private name; call using ESMF_FieldCreate()
      function ESMF_FieldCreateNew(grid, arrayspec, allocflag, horzRelloc, &
                                   vertRelloc, haloWidth, datamap, name, &
                                   iospec, rc)
!
! !RETURN VALUE:
      type(ESMF_Field) :: ESMF_FieldCreateNew
!
! !ARGUMENTS:
      type(ESMF_Grid) :: grid               
      type(ESMF_ArraySpec), intent(inout) :: arrayspec     
      type(ESMF_AllocFlag), intent(in), optional :: allocflag
      type(ESMF_RelLoc), intent(in), optional :: horzRelloc 
      type(ESMF_RelLoc), intent(in), optional :: vertRelloc 
      integer, intent(in), optional :: haloWidth
      type(ESMF_FieldDataMap), intent(inout), optional :: datamap          
      character (len=*), intent(in), optional :: name 
      type(ESMF_IOSpec), intent(in), optional :: iospec 
      integer, intent(out), optional :: rc              
!
! !DESCRIPTION:
!     An interface function to {\tt ESMF\_FieldCreate()}.
!     Create an {\tt ESMF\_Field} and allocate space internally for a
!     gridded {\tt ESMF\_Array}.  Return a new {\tt ESMF\_Field}.
! 
!     The arguments are:
!     \begin{description}
!     \item [grid] 
!           Pointer to an {\tt ESMF\_Grid} object. 
!     \item [arrayspec]
!           {\tt ESMF\_Data} specification. 
!     \item [{[allocflag]}]
!           Whether to allocate space for the array.  See 
!           Section~\ref{opt:allocflag} for possible values.  Default is
!           {\tt ESMF\_ALLOC}.
!     \item [{[horzRelloc]}] 
!           Relative location of data per grid cell/vertex in the horizontal
!           grid.  
!           If specified here, takes precedence over the same setting
!           in the {\tt datamap} argument.
!     \item [{[vertRelloc]}] 
!           Relative location of data per grid cell/vertex in the vertical grid.
!           If specified here, takes precedence over the same setting
!           in the {\tt datamap} argument.
!     \item [{[haloWidth]}] 
!           Maximum halo depth along all edges.  Default is 0.
!     \item [{[datamap]}]
!           An {\tt ESMF\_FieldDataMap} which describes the mapping of 
!           data to the {\tt ESMF\_Grid}.
!     \item [{[name]}] 
!           {\tt Field} name. 
!     \item [{[iospec]}] 
!           I/O specification. 
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
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

      ! check variables
      ESMF_INIT_CHECK_SHALLOW(ESMF_FieldDataMapGetInit,ESMF_FieldDataMapInit,datamap)

      allocate(ftype, stat=status)
      ! If error write message and return.
      ! Formal error handling will be added asap.
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      ! Call construction method to allocate and initialize field internals.
      call ESMF_FieldConstructIA(ftype, grid, arrayspec, allocflag, &
                                  horzRelloc, vertRelloc, haloWidth, &
                                  datamap, name, iospec, status)
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return
   
      ! Set return values.
      ESMF_FieldCreateNew%ftypep => ftype

      ESMF_INIT_SET_CREATED(ESMF_FieldCreateNew)
      if(rcpresent) rc = ESMF_SUCCESS

      end function ESMF_FieldCreateNew

!------------------------------------------------------------------------------
^undef  ESMF_METHOD
^define ESMF_METHOD "ESMF_FieldCreateFromArray"
!BOP
! !IROUTINE: ESMF_FieldCreate - Create a Field from an existing ESMF Array

! !INTERFACE:
      ! Private name; call using ESMF_FieldCreate()
      function ESMF_FieldCreateFromArray(grid, array, copyflag, horzRelloc, &
                                         vertRelloc, haloWidth, datamap, name, &
                                         iospec, rc)
!
! !RETURN VALUE:
      type(ESMF_Field) :: ESMF_FieldCreateFromArray    
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: grid                
      type(ESMF_InternArray), intent(in) :: array              
      type(ESMF_CopyFlag), intent(in), optional :: copyflag       
      type(ESMF_RelLoc), intent(in), optional :: horzRelloc 
      type(ESMF_RelLoc), intent(in), optional :: vertRelloc 
      integer, intent(in), optional :: haloWidth
      type(ESMF_FieldDataMap), intent(inout), optional :: datamap           
      character (len = *), intent(in), optional :: name   
      type(ESMF_IOSpec), intent(in), optional :: iospec   
      integer, intent(out), optional :: rc                
!
! !DESCRIPTION:
!     An interface function to {\tt ESMF\_FieldCreate()}.
!     This version of creation assumes the data exists already and is being
!     passed in through an {\tt ESMF\_Array}.  
! 
!     The arguments are:
!     \begin{description}
!     \item [grid] 
!           Pointer to an {\tt ESMF\_Grid} object. 
!     \item [array]
!           Includes data specification and allocated memory. 
!           It must already include space for the halo regions.
!     \item [{[copyflag]}]
!           Indicates whether to reference the array or make a 
!           copy of it.  Valid values are {\tt ESMF\_DATA\_COPY} and 
!           {\tt ESMF\_DATA\_REF}, respectively.
!     \item [{[horzRelloc]}] 
!           Relative location of data per grid cell/vertex in the horizontal
!           grid.
!           If specified here, takes precedence over the same setting
!           in the {\tt datamap} argument.
!     \item [{[vertRelloc]}] 
!           Relative location of data per grid cell/vertex in the vertical grid.
!           If specified here, takes precedence over the same setting
!           in the {\tt datamap} argument.
!     \item [{[datamap]}]
!           An {\tt ESMF\_FieldDataMap} which describes the mapping of 
!           data to the {\tt ESMF\_Grid}.
!     \item [{[name]}] 
!           {\tt Field} name. 
!     \item [{[iospec]}] 
!           I/O specification. 
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS: FLD1.1.2, FLD1.5.1


      type(ESMF_FieldType), pointer :: ftype  ! Pointer to new field
      integer :: status                       ! Error status
      logical :: rcpresent                    ! Return code present
      
      ! Initialize pointers
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      nullify(ftype)
      nullify(ESMF_FieldCreateFromArray%ftypep)

      ! Initialize return code   
      if(present(rc)) then
        rcpresent = .TRUE. 
        rc = ESMF_FAILURE
      endif     

      ! check variables
      ESMF_INIT_CHECK_SHALLOW(ESMF_FieldDataMapGetInit,ESMF_FieldDataMapInit,datamap)

      allocate(ftype, stat=status)
      if (ESMF_LogMsgFoundAllocError(status, "Allocating Field information", &
                                       ESMF_CONTEXT, rc)) return

      ! Call construction method to allocate and initialize field internals.
      call ESMF_FieldConstructIA(ftype, grid, array, horzRelloc, &
                                       vertRelloc, datamap, name, &
                                       iospec, status)
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return
   

      ! Set return values.
      ESMF_FieldCreateFromArray%ftypep => ftype
      ESMF_INIT_SET_CREATED(ESMF_FieldCreateFromArray)
      if(rcpresent) rc = ESMF_SUCCESS

      end function ESMF_FieldCreateFromArray

!------------------------------------------------------------------------------
^undef  ESMF_METHOD
^define ESMF_METHOD "ESMF_FieldCreateRemap"
!BOP
! !IROUTINE: ESMF_FieldCreate - Create a Field by remapping another Field

! !INTERFACE:
      ! Private name; call using ESMF_FieldCreate()
      function ESMF_FieldCreateRemap(srcField, grid, horzRelloc, vertRelloc, &
                                     haloWidth, datamap, name, iospec, rc)
!
! !RETURN VALUE:
      type(ESMF_Field) :: ESMF_FieldCreateRemap
!
! !ARGUMENTS:
      type(ESMF_Field), intent(in) :: srcField            
      type(ESMF_Grid), intent(in) :: grid                 
      type(ESMF_RelLoc), intent(in), optional :: horzRelloc 
      type(ESMF_RelLoc), intent(in), optional :: vertRelloc 
      integer, intent(in), optional :: haloWidth
      type(ESMF_FieldDataMap), intent(inout), optional :: datamap              
      character (len = *), intent(in), optional :: name   
      type(ESMF_IOSpec), intent(in), optional :: iospec   
      integer, intent(out), optional :: rc                
!
! !DESCRIPTION:
!
! An interface function to {\tt ESMF\_FieldCreate()}.
! Remaps data between an existing {\tt ESMF\_Grid} on a source {\tt ESMF\_Field}
! and a new {\tt ESMF\_Grid}.  The {\tt ESMF\_Grid} is referenced by the 
! new {\tt ESMF\_Field}.  Data is copied.
!
!
!     The arguments are:
!     \begin{description}
!     \item [srcField]
!           Source {\tt ESMF\_Field}.
!     \item [grid]
!           {\tt ESMF\_Grid} of source {\tt ESMF\_Field}.
!     \item [horzRelLoc]
!           Relative location of data per grid cell/vertex in the horizontal
!           grid.
!     \item [vertRelLoc]
!           Relative location of data per grid cell/vertex in the vertical grid.
!     \item [{[halowidth]}]
!           Halo width.
!     \item [{[datamap]}]
!           {\tt ESMF\_FieldDataMap}
!     \item [{[name]}]
!       {\tt ESMF\_Field} name.
!     \item [{[iospec]}]
!       {\tt ESMF\_Field} {\tt ESMF\_IOSpec}.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
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

      ! check variables
      ESMF_INIT_CHECK_SHALLOW(ESMF_FieldDataMapGetInit,ESMF_FieldDataMapInit,datamap)

      allocate(ftype, stat=status)
      if (ESMF_LogMsgFoundAllocError(status, "Allocating Field information", &
                                       ESMF_CONTEXT, rc)) return

      ! TODO: Insert field construction method

      ! Set return values.
      ESMF_FieldCreateRemap%ftypep => ftype
      ESMF_INIT_SET_CREATED(ESMF_FieldCreateRemap)
      if(rcpresent) rc = ESMF_SUCCESS

      end function ESMF_FieldCreateRemap

!------------------------------------------------------------------------------

      ! < declarations of subroutines for each T/K/R >
DeclarationMacro(FieldCreateDPtr)

DeclarationMacro(FieldCreateEPtr)

!------------------------------------------------------------------------------


      end module ESMF_FieldCreateMod

