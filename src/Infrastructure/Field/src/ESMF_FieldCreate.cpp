! $Id: ESMF_FieldCreate.cpp,v 1.25 2007/06/22 23:21:30 cdeluca Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2007, University Corporation for Atmospheric Research, 
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
#include "ESMF_TypeKindRankMacros.hcppF90"
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
      use ESMF_InternGridTypesMod
      use ESMF_InternGridMod
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
      '$Id: ESMF_FieldCreate.cpp,v 1.25 2007/06/22 23:21:30 cdeluca Exp $'

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
TypeKindRankInterfaceMacro(FieldCreateDPtr)

TypeKindRankInterfaceMacro(FieldCreateEPtr)

! !DESCRIPTION:
!   This interface provides an entry point for methods that create a complete
!   {\tt ESMF\_Field}.  These method all contain an {\tt ESMF\_InternGrid} and 
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
      function ESMF_FieldCreateNew(interngrid, arrayspec, allocflag, horzRelloc, &
                                   vertRelloc, haloWidth, datamap, name, &
                                   iospec, rc)
!
! !RETURN VALUE:
      type(ESMF_Field) :: ESMF_FieldCreateNew
!
! !ARGUMENTS:
      type(ESMF_InternGrid) :: interngrid               
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
!     interngridded {\tt ESMF\_Array}.  Return a new {\tt ESMF\_Field}.
! 
!     The arguments are:
!     \begin{description}
!     \item [interngrid] 
!           Pointer to an {\tt ESMF\_InternGrid} object. 
!     \item [arrayspec]
!           {\tt ESMF\_Data} specification. 
!     \item [{[allocflag]}]
!           Whether to allocate space for the array.  See 
!           Section~\ref{opt:allocflag} for possible values.  Default is
!           {\tt ESMF\_ALLOC}.
!     \item [{[horzRelloc]}] 
!           Relative location of data per interngrid cell/vertex in the horizontal
!           interngrid.  
!           If specified here, takes precedence over the same setting
!           in the {\tt datamap} argument.
!     \item [{[vertRelloc]}] 
!           Relative location of data per interngrid cell/vertex in the vertical interngrid.
!           If specified here, takes precedence over the same setting
!           in the {\tt datamap} argument.
!     \item [{[haloWidth]}] 
!           Maximum halo depth along all edges.  Default is 0.
!     \item [{[datamap]}]
!           An {\tt ESMF\_FieldDataMap} which describes the mapping of 
!           data to the {\tt ESMF\_InternGrid}.
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
      integer :: localrc                           !  Local error code
      logical :: rcpresent                        ! Return code present

      ! Initialize pointers
      localrc = ESMF_RC_NOT_IMPL
      rcpresent = .FALSE.
      nullify(ftype)
      nullify(ESMF_FieldCreateNew%ftypep)

      ! Initialize return code   
      if(present(rc)) then
        rcpresent=.TRUE.
        rc = ESMF_RC_NOT_IMPL
      endif

      ! check variables
      ESMF_INIT_CHECK_SHALLOW(ESMF_FieldDataMapGetInit,ESMF_FieldDataMapInit,datamap)

      allocate(ftype, stat=localrc)
      ! If error write message and return.
      ! Formal error handling will be added asap.
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      ! Call construction method to allocate and initialize field internals.
      call ESMF_FieldConstructIA(ftype, interngrid, arrayspec, allocflag, &
                                  horzRelloc, vertRelloc, haloWidth, &
                                  datamap, name, iospec, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
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
      function ESMF_FieldCreateFromArray(interngrid, array, copyflag, horzRelloc, &
                                         vertRelloc, haloWidth, datamap, name, &
                                         iospec, rc)
!
! !RETURN VALUE:
      type(ESMF_Field) :: ESMF_FieldCreateFromArray    
!
! !ARGUMENTS:
      type(ESMF_InternGrid), intent(in) :: interngrid                
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
!     \item [interngrid] 
!           Pointer to an {\tt ESMF\_InternGrid} object. 
!     \item [array]
!           Includes data specification and allocated memory. 
!           It must already include space for the halo regions.
!     \item [{[copyflag]}]
!           Indicates whether to reference the array or make a 
!           copy of it.  Valid values are {\tt ESMF\_DATA\_COPY} and 
!           {\tt ESMF\_DATA\_REF}, respectively.
!     \item [{[horzRelloc]}] 
!           Relative location of data per interngrid cell/vertex in the horizontal
!           interngrid.
!           If specified here, takes precedence over the same setting
!           in the {\tt datamap} argument.
!     \item [{[vertRelloc]}] 
!           Relative location of data per interngrid cell/vertex in the vertical interngrid.
!           If specified here, takes precedence over the same setting
!           in the {\tt datamap} argument.
!     \item [{[datamap]}]
!           An {\tt ESMF\_FieldDataMap} which describes the mapping of 
!           data to the {\tt ESMF\_InternGrid}.
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
      integer :: localrc                       !  Local error code
      logical :: rcpresent                    ! Return code present
      
      ! Initialize pointers
      localrc = ESMF_RC_NOT_IMPL
      rcpresent = .FALSE.
      nullify(ftype)
      nullify(ESMF_FieldCreateFromArray%ftypep)

      ! Initialize return code   
      if(present(rc)) then
        rcpresent = .TRUE. 
        rc = ESMF_RC_NOT_IMPL
      endif     

      ! check variables
      ESMF_INIT_CHECK_SHALLOW(ESMF_FieldDataMapGetInit,ESMF_FieldDataMapInit,datamap)

      allocate(ftype, stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, "Allocating Field information", &
                                       ESMF_CONTEXT, rc)) return

      ! Call construction method to allocate and initialize field internals.
      call ESMF_FieldConstructIA(ftype, interngrid, array, horzRelloc, &
                                       vertRelloc, datamap, name, &
                                       iospec, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
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
      function ESMF_FieldCreateRemap(srcField, interngrid, horzRelloc, vertRelloc, &
                                     haloWidth, datamap, name, iospec, rc)
!
! !RETURN VALUE:
      type(ESMF_Field) :: ESMF_FieldCreateRemap
!
! !ARGUMENTS:
      type(ESMF_Field), intent(in) :: srcField            
      type(ESMF_InternGrid), intent(in) :: interngrid                 
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
! Remaps data between an existing {\tt ESMF\_InternGrid} on a source {\tt ESMF\_Field}
! and a new {\tt ESMF\_InternGrid}.  The {\tt ESMF\_InternGrid} is referenced by the 
! new {\tt ESMF\_Field}.  Data is copied.
!
!
!     The arguments are:
!     \begin{description}
!     \item [srcField]
!           Source {\tt ESMF\_Field}.
!     \item [interngrid]
!           {\tt ESMF\_InternGrid} of source {\tt ESMF\_Field}.
!     \item [horzRelLoc]
!           Relative location of data per interngrid cell/vertex in the horizontal
!           interngrid.
!     \item [vertRelLoc]
!           Relative location of data per interngrid cell/vertex in the vertical interngrid.
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
      integer :: localrc                       !  Local error code
      logical :: rcpresent                    ! Return code present
      
      ! Initialize pointers
      localrc = ESMF_RC_NOT_IMPL
      rcpresent = .FALSE.
      nullify(ftype)
      nullify(ESMF_FieldCreateRemap%ftypep)

      ! Initialize return code   
      if(present(rc)) then
        rcpresent = .TRUE. 
        rc = ESMF_RC_NOT_IMPL
      endif     

      ! check variables
      ESMF_INIT_CHECK_SHALLOW(ESMF_FieldDataMapGetInit,ESMF_FieldDataMapInit,datamap)

      allocate(ftype, stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, "Allocating Field information", &
                                       ESMF_CONTEXT, rc)) return

      ! TODO: Insert field construction method

      ! Set return values.
      ESMF_FieldCreateRemap%ftypep => ftype
      ESMF_INIT_SET_CREATED(ESMF_FieldCreateRemap)
      if(rcpresent) rc = ESMF_SUCCESS

      end function ESMF_FieldCreateRemap

!------------------------------------------------------------------------------

      ! < declarations of subroutines for each T/K/R >
TypeKindRankDeclarationMacro(FieldCreateDPtr)

TypeKindRankDeclarationMacro(FieldCreateEPtr)

!------------------------------------------------------------------------------


      end module ESMF_FieldCreateMod

