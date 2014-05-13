! +-======-+ 
!  Copyright (c) 2003-2007 United States Government as represented by 
!  the Admistrator of the National Aeronautics and Space Administration.  
!  All Rights Reserved.
!  
!  THIS OPEN  SOURCE  AGREEMENT  ("AGREEMENT") DEFINES  THE  RIGHTS  OF USE,
!  REPRODUCTION,  DISTRIBUTION,  MODIFICATION AND REDISTRIBUTION OF CERTAIN 
!  COMPUTER SOFTWARE ORIGINALLY RELEASED BY THE UNITED STATES GOVERNMENT AS 
!  REPRESENTED BY THE GOVERNMENT AGENCY LISTED BELOW ("GOVERNMENT AGENCY").  
!  THE UNITED STATES GOVERNMENT, AS REPRESENTED BY GOVERNMENT AGENCY, IS AN 
!  INTENDED  THIRD-PARTY  BENEFICIARY  OF  ALL  SUBSEQUENT DISTRIBUTIONS OR 
!  REDISTRIBUTIONS  OF THE  SUBJECT  SOFTWARE.  ANYONE WHO USES, REPRODUCES, 
!  DISTRIBUTES, MODIFIES  OR REDISTRIBUTES THE SUBJECT SOFTWARE, AS DEFINED 
!  HEREIN, OR ANY PART THEREOF,  IS,  BY THAT ACTION, ACCEPTING IN FULL THE 
!  RESPONSIBILITIES AND OBLIGATIONS CONTAINED IN THIS AGREEMENT.
!  
!  Government Agency: National Aeronautics and Space Administration
!  Government Agency Original Software Designation: GSC-15354-1
!  Government Agency Original Software Title:  GEOS-5 GCM Modeling Software
!  User Registration Requested.  Please Visit http://opensource.gsfc.nasa.gov
!  Government Agency Point of Contact for Original Software:  
!  			Dale Hithon, SRA Assistant, (301) 286-2691
!  
! +-======-+ 
#ifdef NAME_
#undef NAME_
#endif

#define NAME_ ESMFL_StateGetPtrToData

#include "overload.macro"

 subroutine SUB_(STATE, PTR, NAME, alloc, notFoundOK, RC)
   type(ESMF_State),  intent(INOUT) :: STATE
   real(KIND=EKIND_), pointer :: PTR DIMENSIONS_
   character(len=*),  intent(IN   ) :: NAME
   logical, optional, intent(IN   ) :: alloc
   logical, optional, intent(IN   ) :: notFoundOK
   integer, optional, intent(  OUT) :: RC
   
   character(len=ESMF_MAXSTR)      :: IAm=SUBNAME__( SUB_ )
   integer                         :: STATUS

   type (ESMF_FieldBundle)         :: bundle
   type (ESMF_Field)               :: field
   logical                         :: IsOk
   integer                         :: loc
   integer                         :: DIMS
   type(ESMF_FieldStatus_Flag)     :: fieldStatus

   NULLIFY(ptr)
   if (present(notFoundOK)) then
      IsOk = notFoundOK
   else
      IsOk = .false.
   end if

! Get Field from state

   loc = index(name,';;')

   if(loc/=0) then
      call ESMF_StateGet(state, name(:loc-1), Bundle, rc=status)
      if(STATUS /= ESMF_SUCCESS .and. IsOk) then
         RETURN_(ESMF_SUCCESS)
      end if
      VERIFY_(STATUS)
      call ESMF_StateGet(state, name(loc+2:), Field, rc=status)
      if(STATUS /= ESMF_SUCCESS .and. IsOk) then
         RETURN_(ESMF_SUCCESS)
      end if
      VERIFY_(STATUS)
   else
      call ESMF_StateGet(state, name, Field, rc=status)
      if(STATUS /= ESMF_SUCCESS .and. IsOk) then
         RETURN_(ESMF_SUCCESS)
      end if
      VERIFY_(STATUS)
   end if

   call ESMF_FieldGet(field, status=fieldStatus, rc=status)
   VERIFY_(STATUS)
     if (fieldStatus == ESMF_FIELDSTATUS_COMPLETE) then
      call ESMF_FieldGet(field, 0, Ptr, rc=status)
      VERIFY_(STATUS)
   end if

   if (present(alloc)) then
      if (alloc .and. (.not. associated(PTR))) then
         call MAPL_AllocateCoupling(Field, rc=STATUS)
         VERIFY_(STATUS)
         call ESMF_FieldGet(field, 0, Ptr, rc=status)
         VERIFY_(STATUS)
      endif
   endif

!ALT I dont think the next lines are needed anymore
#if 0
   call ESMF_AttributeGet(field, name='VLOCATION', value=DIMS, rc=status)
   if (STATUS==ESMF_SUCCESS) then
      if (DIMS == MAPL_VLocationEdge .and. associated(ptr)) then
         call AdjustPtrBounds(ptr, ptr, 1, size(ptr,1), 1, size(ptr,2), 0, size(ptr,3)-1)
      end if
   end if
#endif
   
   RETURN_(ESMF_SUCCESS)
   
 end subroutine SUB_

#undef DIMENSIONS_
#undef RANK_
