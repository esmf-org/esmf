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

!----------------------------------------------------------------------------
!BOP

! !MODULE: MAPL_MaxMinMod --- Global Max/Min of Arrays

! !INTERFACE:

   module MAPL_MaxMinMod


! !USES:

      Use ESMF
      Use MAPL_CommsMod

      implicit None

! !PUBLIC MEMBER FUNCTIONS:
!
      private
      public  MAPL_MaxMin

! !DESCRIPTION:  This module implements functions for calculating/printing out the global min/max
!                 of fortran arrays. Derived from GEOS-4 pmaxmin() functions.

!EOP

      interface MAPL_MaxMin

         module procedure pmaxmin3d_r4
         module procedure pmaxmin2d_r4
         module procedure pmaxmin1d_r4

         module procedure pmaxmin3d_r8
         module procedure pmaxmin2d_r8
         module procedure pmaxmin1d_r8

      end interface MAPL_MaxMin

CONTAINS

  subroutine pmaxmin3d_r4 ( qname, a, pmin, pmax, fac )
      implicit none
      character*(*),                intent(in)  :: qname        ! label to print
      real(ESMF_KIND_R4),           intent(in)  :: a(:,:,:)     ! input array
      real(ESMF_KIND_R4), optional, intent(in)  :: fac          ! multiplication factor
      real(ESMF_KIND_R4), optional, intent(out) :: pmax, pmin   ! min/max value
!                         ---
      integer im, jt
      im = size(a,1) * size(a,2)
      jt = size(a,3)
      call pmaxmin2d_r4 ( qname, reshape(a,(/ im, jt /)), pmin, pmax, fac )
    end subroutine pmaxmin3d_r4

    subroutine pmaxmin2d_r4 ( qname, a, pmin_, pmax_, fac_ )

      implicit none
      character*(*),                intent(in)  :: qname        ! label to print
      real(ESMF_KIND_R4),           intent(in)  :: a(:,:)       ! input array
      real(ESMF_KIND_R4), optional, intent(in)  :: fac_         ! multiplication factor
      real(ESMF_KIND_R4), optional, intent(out) :: pmax_, pmin_ ! min/max value
!                                           ---

      real(ESMF_KIND_R4) :: pmax, pmin, fac

      integer :: im, jt

      integer :: i, j, two=2

      real, allocatable :: qmin(:), qmax(:)
      real pm1(2)
      real pm_res(2)
      type(ESMF_VM) :: vm

      character(len=16) :: name
      integer :: status

      im = size(a,1)
      jt = size(a,2)
      allocate(qmin(jt),qmax(jt))

      if ( present(fac_) ) then
         fac = fac_
      else
         fac = 1.0
      end if

      call ESMF_VmGetCurrent(vm=vm, rc=status)

      do j=1,jt
         pmax = a(1,j)
         pmin = a(1,j)
         do i=2,im
            pmax = max(pmax, a(i,j))
            pmin = min(pmin, a(i,j))
         enddo
         qmax(j) = pmax
         qmin(j) = pmin
      enddo
!
! Now find max/min of amax/amin
!
      pmax = qmax(1)
      pmin = qmin(1)
      do j=2,jt
         pmax = max(pmax, qmax(j))
         pmin = min(pmin, qmin(j))
      enddo

      pm1(1) = pmax
      pm1(2) = -pmin
      call MAPL_CommsAllReduceMax(vm, sendbuf=pm1, recvbuf=pm_res, cnt=two, RC=status)
      pmax=pm_res(1)
      pmin=-pm_res(2)
     
      if ( present(pmax_) ) pmax_ = pmax
      if ( present(pmin_) ) pmin_ = pmin
      deallocate(qmax,qmin)

      if ( fac /= 0.0 ) then  ! trick to prevent printing
         if ( MAPL_am_I_root() ) then
            name = '            '
            name(1:len(qname)) = qname
            write(*,*) name, ' max = ', pmax*fac, ' min = ', pmin*fac
            return
         end if
      end if

      return

    end subroutine pmaxmin2d_r4

    subroutine pmaxmin1d_r4 ( qname, a, pmin, pmax, fac )
      implicit none
      character*(*),                intent(in)  :: qname        ! label to print
      real(ESMF_KIND_R4),           intent(in)  :: a(:)         ! input array
      real(ESMF_KIND_R4), optional, intent(in)  :: fac          ! multiplication factor
      real(ESMF_KIND_R4), optional, intent(out) :: pmax, pmin   ! min/max value

      integer :: im, jt
      im = size(a)
      jt = 1
      call pmaxmin2d_r4 ( qname, reshape(a,(/ im, jt /)), pmin, pmax, fac )
    end subroutine pmaxmin1d_r4

!---

  subroutine pmaxmin3d_r8 ( qname, a, pmin, pmax, fac )
      implicit none
      character*(*),                intent(in)  :: qname        ! label to print
      real(ESMF_KIND_R8),           intent(in)  :: a(:,:,:)     ! input array
      real(ESMF_KIND_R8), optional, intent(in)  :: fac          ! multiplication factor
      real(ESMF_KIND_R8), optional, intent(out) :: pmax, pmin   ! min/max value
!                         ---
      real(ESMF_KIND_R4) :: pmin_r4, pmax_r4, fac_r4 
      if ( present(fac) ) then
         fac_r4 = fac
      else
         fac_r4 = 1.0
      end if
      call pmaxmin3d_r4 ( qname, real(a,kind=ESMF_KIND_R4), pmin_r4, pmax_r4, fac_r4 )
      if ( present(pmin) ) pmin = pmin_r4
      if ( present(pmax) ) pmax = pmax_r4
   end subroutine pmaxmin3d_r8

  subroutine pmaxmin2d_r8 ( qname, a, pmin, pmax, fac )
      implicit none
      character*(*),                intent(in)  :: qname        ! label to print
      real(ESMF_KIND_R8),           intent(in)  :: a(:,:)     ! input array
      real(ESMF_KIND_R8), optional, intent(in)  :: fac          ! multiplication factor
      real(ESMF_KIND_R8), optional, intent(out) :: pmax, pmin   ! min/max value
!                         ---
      real(ESMF_KIND_R4) :: pmin_r4, pmax_r4, fac_r4 
      if ( present(fac) ) then
         fac_r4 = fac
      else
         fac_r4 = 1.0
      end if
      call pmaxmin2d_r4 ( qname, real(a,kind=ESMF_KIND_R4), pmin_r4, pmax_r4, fac_r4 )
      if ( present(pmin) ) pmin = pmin_r4
      if ( present(pmax) ) pmax = pmax_r4
   end subroutine pmaxmin2d_r8

  subroutine pmaxmin1d_r8 ( qname, a, pmin, pmax, fac )
      implicit none
      character*(*),                intent(in)  :: qname        ! label to print
      real(ESMF_KIND_R8),           intent(in)  :: a(:)         ! input array
      real(ESMF_KIND_R8), optional, intent(in)  :: fac          ! multiplication factor
      real(ESMF_KIND_R8), optional, intent(out) :: pmax, pmin   ! min/max value
!                         ---
      real(ESMF_KIND_R4) :: pmin_r4, pmax_r4, fac_r4 
      if ( present(fac) ) then
         fac_r4 = fac
      else
         fac_r4 = 1.0
      end if
      call pmaxmin1d_r4 ( qname, real(a,kind=ESMF_KIND_R4), pmin_r4, pmax_r4, fac_r4 )
      if ( present(pmin) ) pmin = pmin_r4
      if ( present(pmax) ) pmax = pmax_r4
   end subroutine pmaxmin1d_r8


 end module MAPL_MaxMinMod
