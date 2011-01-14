! $Id: ESMF_Field.F90,v 1.355 2011/01/14 01:09:12 rokuingh Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2011, University Corporation for Atmospheric Research, 
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
! Laboratory, University of Michigan, National Centers for Environmental 
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
#define ESMF_FILENAME "ESMF_Field.F90"
!==============================================================================
!
!     ESMF Field module
module ESMF_FieldMod
!
!==============================================================================
!
! This file contains the Field class definition and all Field
! class method.
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
! represents a single scalar or vector field.  {\tt ESMF\_Field}s associate
! a metadata description expressed as a set of {\tt ESMF\_Attributes} with
! a data {\tt ESMF\_Array} and an {\tt ESMF\_Grid}.
! 
! A gridToFieldMap describes the relationship of the {\tt ESMF\_Array} to
! the {\tt ESMF\_Grid}.  
!
! This type is implemented in Fortran 90.
!
!------------------------------------------------------------------------------
! !USES:
  use ESMF_UtilTypesMod
  use ESMF_UtilMod
  use ESMF_BaseMod
  use ESMF_LogErrMod
  use ESMF_ArraySpecMod
  use ESMF_LocalArrayMod
  use ESMF_DELayoutMod
  use ESMF_StaggerLocMod
  use ESMF_DistGridMod
  use ESMF_GridMod
  use ESMF_GeomBaseMod
  use ESMF_ArrayMod
  use ESMF_ArrayCreateMod
  use ESMF_ArrayGetMod
  use ESMF_TimeMod
  use ESMF_InitMacrosMod

  implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
  private

!------------------------------------------------------------------------------
! ! ESMF_FieldType
! ! Definition of the Field class.

  type ESMF_FieldType
    sequence
    !private
    type (ESMF_Base)              :: base             ! base class object
    type (ESMF_Array)             :: array
    type (ESMF_GeomBase)          :: geombase
    type (ESMF_Status)            :: gridstatus
    type (ESMF_Status)            :: datastatus
    type (ESMF_Status)            :: iostatus         ! if unset, inherit from gcomp
    logical                       :: array_internal   ! .true. if field%array is
                                                      ! internally allocated
    logical                       :: is_proxy         ! .true. for a proxy field
    integer                       :: dimCount         ! field dimension count
    integer                       :: gridToFieldMap(ESMF_MAXDIM)
    integer                       :: ungriddedLBound(ESMF_MAXDIM)
    integer                       :: ungriddedUBound(ESMF_MAXDIM)
    integer                       :: totalLWidth(ESMF_MAXDIM)
    integer                       :: totalUWidth(ESMF_MAXDIM)
    ESMF_INIT_DECLARE
  end type

!------------------------------------------------------------------------------
! ! ESMF_Field    
! ! The Field data structure that is passed between implementation and
! ! calling languages.

  type ESMF_Field
    sequence
    !private       
    type (ESMF_FieldType), pointer :: ftypep
    ESMF_INIT_DECLARE
  end type

!------------------------------------------------------------------------------
! !PUBLIC TYPES:
  public ESMF_Field
  public ESMF_FieldType ! For internal use only

!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:
!
! - ESMF-public methods:
   public ESMF_FieldValidate           ! Check internal consistency

   public operator(==)
   public operator(/=)

! - ESMF-internal methods:
   public ESMF_FieldGetInit            ! For Standardized Initialization
   public ESMF_FieldSerialize
   public ESMF_FieldDeserialize
   public ESMF_FieldInitialize         ! Default initiailze field member variables

!
!
!EOPI
   
!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
  character(*), parameter, private :: version = &
    '$Id: ESMF_Field.F90,v 1.355 2011/01/14 01:09:12 rokuingh Exp $'

!==============================================================================
!
! INTERFACE BLOCKS
!
!==============================================================================

!===============================================================================
! FieldOperator() interfaces
!===============================================================================

! -------------------------- ESMF-public method -------------------------------
!BOP
! !IROUTINE: ESMF_FieldAssignment(=) - Field assignment
!
! !INTERFACE:
!   interface assignment(=)
!   field1 = field2
!
! !ARGUMENTS:
!   type(ESMF_Field) :: field1
!   type(ESMF_Field) :: field2
!
!
! !DESCRIPTION:
!   Assign field1 as an alias to the same ESMF Field object in memory
!   as field2. If field2 is invalid, then field1 will be equally invalid after
!   the assignment.
!
!   The arguments are:
!   \begin{description}
!   \item[field1]
!     The {\tt ESMF\_Field} object on the left hand side of the assignment.
!   \item[field2]
!     The {\tt ESMF\_Field} object on the right hand side of the assignment.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
!BOP
! !IROUTINE: ESMF_FieldOperator(==) - Field equality operator
!
! !INTERFACE:
  interface operator(==)
!   if (field1 == field2) then ... endif
!             OR
!   result = (field1 == field2)
! !RETURN VALUE:
!   logical :: result
!
! !ARGUMENTS:
!   type(ESMF_Field), intent(in) :: field1
!   type(ESMF_Field), intent(in) :: field2
!
!
! !DESCRIPTION:
!   Test whether field1 and field2 are valid aliases to the same ESMF
!   Field object in memory. For a more general comparison of two ESMF Fields,
!   going beyond the simple alias test, the ESMF\_FieldMatch() function (not yet
!   implemented) must be used.
!
!   The arguments are:
!   \begin{description}
!   \item[field1]
!     The {\tt ESMF\_Field} object on the left hand side of the equality
!     operation.
!   \item[field2]
!     The {\tt ESMF\_Field} object on the right hand side of the equality
!     operation.
!   \end{description}
!
!EOP
    module procedure ESMF_FieldEQ

  end interface
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
!BOP
! !IROUTINE: ESMF_FieldOperator(/=) - Field not equal operator
!
! !INTERFACE:
  interface operator(/=)
!   if (field1 == field2) then ... endif
!             OR
!   result = (field1 == field2)
! !RETURN VALUE:
!   logical :: result
!
! !ARGUMENTS:
!   type(ESMF_Field), intent(in) :: field1
!   type(ESMF_Field), intent(in) :: field2
!
!
! !DESCRIPTION:
!   Test whether field1 and field2 are {\it not} valid aliases to the
!   same ESMF Field object in memory. For a more general comparison of two ESMF
!   Fields, going beyond the simple alias test, the ESMF\_FieldMatch() function
!   (not yet implemented) must be used.
!
!   The arguments are:
!   \begin{description}
!   \item[field1]
!     The {\tt ESMF\_Field} object on the left hand side of the non-equality
!     operation.
!   \item[field2]
!     The {\tt ESMF\_Field} object on the right hand side of the non-equality
!     operation.
!   \end{description}
!
!EOP
    module procedure ESMF_FieldNE

  end interface
!------------------------------------------------------------------------------


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


!-------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldEQ()"
!BOPI
! !IROUTINE:  ESMF_FieldEQ - Compare two Fields for equality
!
! !INTERFACE:
  function ESMF_FieldEQ(field1, field2)
! 
! !RETURN VALUE:
    logical :: ESMF_FieldEQ

! !ARGUMENTS:
    type(ESMF_Field), intent(in) :: field1
    type(ESMF_Field), intent(in) :: field2

! !DESCRIPTION:
!   Test if both {\tt field1} and {\tt field2} alias the same ESMF Field 
!   object.
!
!EOPI
!-------------------------------------------------------------------------------

    ESMF_INIT_TYPE finit1, finit2
    integer :: localrc1, localrc2
    logical :: lval1, lval2

    ! Use the following logic, rather than "ESMF-INIT-CHECK-DEEP", to gain 
    ! init checks on both args, and in the case where both are uninitialized,
    ! to distinguish equality based on uninitialized type (uncreated,
    ! deleted).

    ! TODO: Consider moving this logic to C++: use Base class? status?
    !       Or replicate logic for C interface also.

    ! check inputs
    finit1 = ESMF_FieldGetInit(field1)
    finit2 = ESMF_FieldGetInit(field2)

    ! TODO: this line must remain split in two for SunOS f90 8.3 127000-03
    if (finit1 .eq. ESMF_INIT_CREATED .and. &
      finit2 .eq. ESMF_INIT_CREATED) then
      ESMF_FieldEQ = associated(field1%ftypep,field2%ftypep)
    else
      ESMF_FieldEQ = ESMF_FALSE
    endif

  end function ESMF_FieldEQ
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldNE()"
!BOPI
! !IROUTINE:  ESMF_FieldNE - Compare two Fields for non-equality
!
! !INTERFACE:
  function ESMF_FieldNE(field1, field2)
! 
! !RETURN VALUE:
    logical :: ESMF_FieldNE

! !ARGUMENTS:
    type(ESMF_Field), intent(in) :: field1
    type(ESMF_Field), intent(in) :: field2

! !DESCRIPTION:
!   Test if both {\tt field1} and {\tt field2} alias the same ESMF Field 
!   object.
!
!EOPI
!-------------------------------------------------------------------------------

    ESMF_INIT_TYPE finit1, finit2
    integer :: localrc1, localrc2
    logical :: lval1, lval2

    ! Use the following logic, rather than "ESMF-INIT-CHECK-DEEP", to gain 
    ! init checks on both args, and in the case where both are uninitialized,
    ! to distinguish equality based on uninitialized type (uncreated,
    ! deleted).
    
    ESMF_FieldNE = .not.ESMF_FieldEQ(field1, field2)

  end function ESMF_FieldNE
!-------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldValidate"

!BOP
! !IROUTINE:  ESMF_FieldValidate - Check validity of a Field

! !INTERFACE:
      subroutine ESMF_FieldValidate(field, rc)
!
! !ARGUMENTS:
      type(ESMF_Field), intent(in) :: field 
      integer,          intent(out), optional :: rc   
!
! !DESCRIPTION:
!      Validates that the {\tt field} is internally consistent.
!      Currently this method determines if the {\tt field} is uninitialized 
!      or already destroyed. It validates the contained array and grid objects.
!      The code also checks if the array and grid sizes agree.
!      This check compares the distgrid contained in array and grid; 
!      then it proceeds to compare the computational bounds contained 
!      in array and grid. 
!
!      The method returns an error code if problems are found.  
!
!     The arguments are:
!     \begin{description}
!     \item [field]
!           {\tt ESMF\_Field} to validate.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if the {\tt field} 
!           is valid.
!     \end{description}
!
!EOP

      integer :: localrc

      type(ESMF_FieldType), pointer :: ftypep
      integer :: exclLBounds(ESMF_MAXDIM)  ! exclusive grid lower bounds
      integer :: exclUBounds(ESMF_MAXDIM)  ! exclusive grid upper bounds
      integer :: gridrank, arrayrank, gridrank_norep
      integer :: i, lDE                        ! helper variables to verify bounds
      integer :: localDECount, dimCount        ! and distgrid
      integer, allocatable :: distgridToGridMap(:)
      integer, allocatable :: distgridToPackedArrayMap(:)
      integer, allocatable :: arrayCompUBnd(:, :), arrayCompLBnd(:, :)
      type(ESMF_DistGrid)  :: arrayDistGrid, gridDistGrid
      type(ESMF_GridDecompType) :: decompType
      type(ESMF_GeomType) :: geomType
      type(ESMF_Grid) :: grid
      type(ESMF_Status) :: fieldstatus

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit,field,rc)

      if (.not.associated(field%ftypep)) then 
         call ESMF_LogSetError(ESMF_RC_OBJ_BAD, &
            "Uninitialized or already destroyed Field: ftypep unassociated", &
             ESMF_CONTEXT, rc)
         return
      endif 

      ftypep => field%ftypep


      ! make sure the field is ready before trying to look at contents
      call ESMF_BaseGetStatus(ftypep%base, fieldstatus, rc=localrc)
      if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rc)) return
      if (fieldstatus .ne. ESMF_STATUS_READY) then
         call ESMF_LogSetError(ESMF_RC_OBJ_BAD, &
            "Uninitialized or already destroyed Field: fieldstatus not ready", &
             ESMF_CONTEXT, rc)
         return
      endif 

      ! make sure there is a grid before asking it questions.
      if (ftypep%gridstatus .eq. ESMF_STATUS_READY) then
          call ESMF_GeomBaseValidate(ftypep%geombase, rc=localrc)
          if (ESMF_LogFoundError(localrc, &
                                    ESMF_ERR_PASSTHRU, &
                                    ESMF_CONTEXT, rc)) return

	  ! get the grid decomp type if geombase is grid
      decompType = ESMF_GRID_NONARBITRARY
          call ESMF_GeomBaseGet(ftypep%geombase, geomType=geomType, rc=localrc)
          if (ESMF_LogFoundError(localrc, &  
            ESMF_ERR_PASSTHRU, &  
            ESMF_CONTEXT, rc)) return  
          
          if (geomType .eq. ESMF_GEOMTYPE_GRID) then
             call ESMF_GeomBaseGet(ftypep%geombase, grid=grid, rc=localrc)
             if (ESMF_LogFoundError(localrc, &  
          	    ESMF_ERR_PASSTHRU, &  
           	    ESMF_CONTEXT, rc)) return  
             call ESMF_GridGetDecompType(grid, decompType, rc=localrc)
             if (ESMF_LogFoundError(localrc, &  
          	    ESMF_ERR_PASSTHRU, &  
           	    ESMF_CONTEXT, rc)) return  
          endif   
          ! get grid dim and extents for the local piece
          call ESMF_GeomBaseGet(ftypep%geombase, dimCount=gridrank, &
                            distgrid=gridDistGrid, localDECount=localDECount, rc=localrc)
          if (localrc .ne. ESMF_SUCCESS) then
             call ESMF_LogSetError(ESMF_RC_OBJ_BAD, &
                "Cannot retrieve distgrid, gridrank, localDECount from ftypep%grid", &
                 ESMF_CONTEXT, rc)
             return
          endif 
          ! Bounds only valid if there are local DE's
          do lDE=0, localDECount-1
             call ESMF_GeomBaseGetPLocalDe(ftypep%geombase, localDE=lDE,  &
                               exclusiveLBound=exclLBounds, &
                               exclusiveUBound=exclUBounds, &
                               rc=localrc)
              if (localrc .ne. ESMF_SUCCESS) then
                 call ESMF_LogSetError(ESMF_RC_OBJ_BAD, &
                    "Cannot retrieve exclusive bounds from ftypep%grid", &
                     ESMF_CONTEXT, rc)
                 return
              endif 
          enddo
      endif
      ! make sure there is data before asking it questions.
      if (ftypep%datastatus .eq. ESMF_STATUS_READY) then
          call ESMF_ArrayValidate(array=ftypep%array, rc=localrc)
          if (localrc .ne. ESMF_SUCCESS) then
             call ESMF_LogSetError(ESMF_RC_OBJ_BAD, &
                "Cannot validate ftypep%array", &
                 ESMF_CONTEXT, rc)
             return
          endif 
          call ESMF_ArrayGet(ftypep%array, dimCount=dimCount, localDECount=localDECount, &
              distgrid=arrayDistGrid, rank=arrayrank, rc=localrc)
          if (localrc .ne. ESMF_SUCCESS) then
             call ESMF_LogSetError(ESMF_RC_OBJ_BAD, &
                "Cannot retrieve dimCount, localDECount, arrayDistGrid, arrayrank from ftypep%array", &
                 ESMF_CONTEXT, rc)
             return
          endif 
          
          ! Verify the distgrids in array and grid match.
          if(.not. ESMF_DistGridMatch(gridDistGrid, arrayDistGrid, rc=localrc)) then
              call ESMF_LogSetError(ESMF_RC_OBJ_BAD, &
                 "grid DistGrid does not match array DistGrid", &
                  ESMF_CONTEXT, rc)
              return
          endif

           ! cannot use distgridToGridMap to index arrayCompBnds and compare with
          ! gridCompBnds, skip the check for arb. array for now, need to figure
          ! out how to validate -- P.Li
          if (decompType .eq. ESMF_GRID_NONARBITRARY) then

             ! Verify that array rank is greater than or equal to grid rank + ungridded bound rank
             allocate(distgridToPackedArrayMap(dimCount))

             call ESMF_ArrayGet(ftypep%array, &
                  distgridToPackedArrayMap=distgridToPackedArrayMap, &
                  rc=localrc)
             if (localrc .ne. ESMF_SUCCESS) then
                 call ESMF_LogSetError(ESMF_RC_OBJ_BAD, &
                 "Cannot retrieve distgridToPackedArrayMap from ftypep%array", &
                 ESMF_CONTEXT, rc)
                return
             endif 

            ! Verify that array rank is greater than or equal to grid rank + ungridded bound rank      
            gridrank_norep = gridrank
            do i = 1, dimCount
               if( distgridToPackedArrayMap(i) == 0) gridrank_norep = gridrank_norep - 1
            enddo

            if ( arrayrank .lt. gridrank_norep) then
                call ESMF_LogSetError(ESMF_RC_OBJ_BAD, &
                   "grid rank + ungridded Bound rank not equal to array rank", &
                    ESMF_CONTEXT, rc)
                return
            endif

            deallocate(distgridToPackedArrayMap)
        endif
      endif ! skip for arbitrary grid case

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldValidate


!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!
! This section includes all Field internal methods.
!
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldSerialize"

!BOPI
! !IROUTINE: ESMF_FieldSerialize - Serialize field info into a byte stream
!
! !INTERFACE:
      subroutine ESMF_FieldSerialize(field, buffer, length, offset, &
                                    attreconflag, inquireflag, rc) 
!
! !ARGUMENTS:
      type(ESMF_Field), intent(inout) :: field 
      character, pointer, dimension(:) :: buffer
      integer, intent(inout) :: length
      integer, intent(inout) :: offset
      type(ESMF_AttReconcileFlag), intent(in), optional :: attreconflag
      type(ESMF_InquireFlag), intent(in), optional :: inquireflag
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!      Takes an {\tt ESMF\_Field} object and adds all the information needed
!      to save the information to a file or recreate the object based on this
!      information.   Expected to be used by {\tt ESMF\_StateReconcile()}.
!
!     The arguments are:
!     \begin{description}
!     \item [field]
!           {\tt ESMF\_Field} object to be serialized.
!     \item [buffer]
!           Data buffer which will hold the serialized information.
!     \item [length]
!           Current length of buffer, in bytes.  If the serialization
!           process needs more space it will allocate it and update
!           this length.
!     \item [offset]
!           Current write offset in the current buffer.  This will be
!           updated by this routine and return pointing to the next
!           available byte in the buffer.
!     \item[{[attreconflag]}]
!           Flag to tell if Attribute serialization is to be done
!     \item[{[inquireflag]}]
!           Flag to tell if serialization is to be done (ESMF_NOINQUIRE)
!           or if this is simply a size inquiry (ESMF_INQUIREONLY)
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI

      integer :: localrc
      type(ESMF_FieldType), pointer :: fp    ! field type
      type(ESMF_AttReconcileFlag) :: lattreconflag
      type(ESMF_InquireFlag) :: linquireflag

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit,field,rc)

      ! deal with optional attreconflag and inquireflag
      if (present(attreconflag)) then
        lattreconflag = attreconflag
      else
        lattreconflag = ESMF_ATTRECONCILE_OFF
      endif

      if (present (inquireflag)) then
        linquireflag = inquireflag
      else
        linquireflag = ESMF_NOINQUIRE
      end if

      ! shortcut to internals
      fp => field%ftypep

      call c_ESMC_BaseSerialize(fp%base, buffer(1), length, offset, &
                                 lattreconflag, linquireflag, localrc)
      if (ESMF_LogFoundError(localrc, &
                                 ESMF_ERR_PASSTHRU, &
                                 ESMF_CONTEXT, rc)) return

      call c_ESMC_FieldSerialize(fp%gridstatus, &
                                 fp%datastatus, fp%iostatus, & 
                                 fp%dimCount, fp%gridToFieldMap, &
                                 fp%ungriddedLBound, fp%ungriddedUBound, &
                                 fp%totalLWidth, fp%totalUWidth, &
                                 buffer(1), length, offset, linquireflag, localrc)
      if (ESMF_LogFoundError(localrc, &
                                 ESMF_ERR_PASSTHRU, &
                                 ESMF_CONTEXT, rc)) return


      if (fp%gridstatus .eq. ESMF_STATUS_READY) then
        call ESMF_GeomBaseSerialize(fp%geombase, buffer, length, offset, &
                                    lattreconflag, linquireflag, localrc)
        if (ESMF_LogFoundError(localrc, &
                                     ESMF_ERR_PASSTHRU, &
                                     ESMF_CONTEXT, rc)) return
      endif

      if (fp%datastatus .eq. ESMF_STATUS_READY) then
          call c_ESMC_ArraySerialize(fp%array, buffer(1), length, offset, &
                                     lattreconflag, linquireflag, localrc)
          if (ESMF_LogFoundError(localrc, &
                                     ESMF_ERR_PASSTHRU, &
                                     ESMF_CONTEXT, rc)) return
      endif

      if  (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldSerialize

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldDeserialize"

!BOPI
! !IROUTINE: ESMF_FieldDeserialize - Deserialize a byte stream into a Field
!
! !INTERFACE:
      function ESMF_FieldDeserialize(buffer, offset, &
                                    attreconflag, rc) 
!
! !RETURN VALUE:
      type(ESMF_Field) :: ESMF_FieldDeserialize   
!
! !ARGUMENTS:
      character, pointer, dimension(:) :: buffer
      integer, intent(inout) :: offset
      type(ESMF_AttReconcileFlag), optional :: attreconflag
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!      Takes a byte-stream buffer and reads the information needed to
!      recreate a Field object.  Recursively calls the deserialize routines
!      needed to recreate the subobjects.
!      Expected to be used by {\tt ESMF\_StateReconcile()} and
!      by {\tt ESMF\_FieldWrite()} and {\tt ESMF\_FieldRead()}.
!
!     The arguments are:
!     \begin{description}
!     \item [buffer]
!           Data buffer which holds the serialized information.
!     \item [offset]
!           Current read offset in the current buffer.  This will be
!           updated by this routine and return pointing to the next
!           unread byte in the buffer.
!     \item[{[attreconflag]}]
!           Flag to tell if Attribute serialization is to be done
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI

      integer :: localrc
      type(ESMF_FieldType), pointer :: fp    ! field type
      integer staggerloc
      type(ESMF_AttReconcileFlag) :: lattreconflag
      type(ESMF_Logical) :: linkChange

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if  (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! deal with optional attreconflag
      if (present(attreconflag)) then
        lattreconflag = attreconflag
      else
        lattreconflag = ESMF_ATTRECONCILE_OFF
      endif

      ! In case of error, make sure this is invalid.
      nullify(ESMF_FieldDeserialize%ftypep)

      ! Shortcut to internals
      allocate(fp, stat=localrc)
      if (ESMF_LogFoundAllocError(localrc, &
                                     "space for new Field object", &
                                     ESMF_CONTEXT, rc)) return

      ! Deserialize Base
      call c_ESMC_BaseDeserialize(fp%base, buffer(1), offset, lattreconflag, localrc)
      if (ESMF_LogFoundError(localrc, &
                                 ESMF_ERR_PASSTHRU, &
                                 ESMF_CONTEXT, rc)) return
                                 
      call ESMF_BaseSetInitCreated(fp%base, rc=localrc)
      if (ESMF_LogFoundError(localrc, &
                                 ESMF_ERR_PASSTHRU, &
                                 ESMF_CONTEXT, rc)) return

      ! Deserialize other Field members

      call c_ESMC_FieldDeserialize(fp%gridstatus, &
                                   fp%datastatus, fp%iostatus, &
                                   fp%dimCount, fp%gridToFieldMap, &
                                   fp%ungriddedLBound, fp%ungriddedUBound, &
                                   fp%totalLWidth, fp%totalUWidth, &
                                   buffer(1), offset, localrc)
      if (ESMF_LogFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (fp%gridstatus .eq. ESMF_STATUS_READY) then
          fp%geombase=ESMF_GeomBaseDeserialize(buffer, offset, &
                                              lattreconflag, localrc)
          if (ESMF_LogFoundError(localrc, &
                                     ESMF_ERR_PASSTHRU, &
                                     ESMF_CONTEXT, rc)) return
          
          !  here we relink the Field Attribute hierarchy to the Attribute
          !  hierarchy of the object in the GeomBase, Grid for now
          if (lattreconflag%value == ESMF_ATTRECONCILE_ON%value .and. & 
              fp%geombase%gbcp%type%type == ESMF_GEOMTYPE_GRID%type) then
            linkChange = ESMF_TRUE
            call c_ESMC_AttributeLink(fp%base, fp%geombase%gbcp%grid, linkChange, localrc)
            if (ESMF_LogFoundError(localrc, &
                                    ESMF_ERR_PASSTHRU, &
                                    ESMF_CONTEXT, rc)) return
          endif

      endif

      if (fp%datastatus .eq. ESMF_STATUS_READY) then
          call c_ESMC_ArrayDeserialize(fp%array, buffer(1), offset, &
                                      lattreconflag, localrc)
          if (ESMF_LogFoundError(localrc, &
                                     ESMF_ERR_PASSTHRU, &
                                     ESMF_CONTEXT, rc)) return

          call ESMF_ArraySetInitCreated(fp%array,rc=localrc)
          if (ESMF_LogFoundError(localrc, &
                                     ESMF_ERR_PASSTHRU, &
                                     ESMF_CONTEXT, rc)) return
      endif
    
      fp%is_proxy = .true.
      ESMF_FieldDeserialize%ftypep => fp
      
      ! Add copy of this object into ESMF garbage collection table
      call c_ESMC_VMAddFObject(ESMF_FieldDeserialize, ESMF_ID_FIELD%objectID)
      
      ESMF_INIT_SET_CREATED(ESMF_FieldDeserialize)

      if  (present(rc)) rc = ESMF_SUCCESS

      end function ESMF_FieldDeserialize

!----------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldGetInit"
!BOPI
! !IROUTINE:  ESMF_FieldGetInit - Get initialization status.

! !INTERFACE:
    function ESMF_FieldGetInit(d)
!
! !ARGUMENTS:
       type(ESMF_Field), intent(in), optional :: d
       ESMF_INIT_TYPE :: ESMF_FieldGetInit
!
! !DESCRIPTION:
!      Get the initialization status of the Deep class {\tt field}.
!
!     The arguments are:
!     \begin{description}
!     \item [s]
!           {\tt ESMF\_Field} from which to retrieve status.
!     \end{description}
!
!EOPI


       if (present(d)) then
         ESMF_FieldGetInit = ESMF_INIT_GET(d)
       else
         ESMF_FieldGetInit = ESMF_INIT_CREATED
       endif

    end function ESMF_FieldGetInit

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldInitialize"

!BOPI
! !IROUTINE: ESMF_FieldInitialize - Default initialize field member variables
!
! !INTERFACE:
      subroutine ESMF_FieldInitialize(ftypep, rc) 
!
! !ARGUMENTS:
      type(ESMF_FieldType), pointer :: ftypep 
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!      Takes an {\tt ESMF\_Field} object and default initialize its
!      auxiliary data members. Only to be used by other Field Create methods.
!
!     The arguments are:
!     \begin{description}
!     \item [ftypep]
!           {\tt ESMF\_FieldType} object to be default initialized.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
        ftypep%gridstatus  = ESMF_STATUS_UNINIT
        ftypep%datastatus  = ESMF_STATUS_UNINIT
        ftypep%iostatus    = ESMF_STATUS_UNINIT
       
        ftypep%array_internal = .false. 
        ftypep%is_proxy       = .false. 
        ftypep%gridToFieldMap = -1
        ftypep%ungriddedLBound = -1
        ftypep%ungriddedUBound = -1
        ftypep%totalLWidth   = -1
        ftypep%totalUWidth   = -1

        if(present(rc)) rc = ESMF_SUCCESS

    end subroutine ESMF_FieldInitialize

!------------------------------------------------------------------------------

end module ESMF_FieldMod
