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
subroutine cube2latlon(npx, npy, nlon, nlat, data_cs, data_ll)

  implicit none
  
  integer, intent(in) :: npx, npy, nlon, nlat
  real, dimension(npx , npy ), intent(in ) :: data_cs
  real, dimension(nlon, nlat), intent(out) :: data_ll
  
end subroutine cube2latlon

subroutine latlon2cube(npx, npy, nlon, nlat, data_ll, data_cs)

 implicit none

 integer, intent(in) :: npx, npy, nlon, nlat
 real, dimension(npx , npy ), intent(out) :: data_cs
 real, dimension(nlon, nlat), intent(in ) :: data_ll

end subroutine latlon2cube

subroutine GetWeightsC2C(npx, npy, npxout, npyout, index, weight) 
  implicit none
  integer,  intent(in   ) :: npx,  npy
  integer,  intent(in   ) :: npxout, npyout
  integer,  intent(  out) :: index(:,:,:,:)
  real(8),  intent(  out) :: weight(:,:,:,:)
end subroutine GetWeightsC2C

!!!!!!!!!!!!!!!%%%%%%%%%%%%%%%%%%%%%%%!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
function AppGridCreateF(IM_WORLD, JM_WORLD, LM, NX, NY, rc) result(grid)
#include "MAPL_Generic.h"

  use ESMF
  use MAPL_BaseMod

  implicit none
  
! !ARGUMENTS:
  integer,           intent(IN)    :: IM_WORLD, JM_WORLD, LM
  integer,           intent(IN)    :: NX, NY
  integer, optional, intent(OUT)   :: rc
  type (ESMF_Grid)                 :: grid
  
! ErrLog variables
!-----------------

  integer                      :: STATUS
  character(len=ESMF_MAXSTR), parameter :: Iam="AppGridCreateF"

! Local variables
!-----------------

  RETURN_(STATUS)
end function AppGridCreateF

subroutine AppGridCreate (META, GRID, RC)

#include "MAPL_Generic.h"

  use ESMF
  use MAPL_Mod

  implicit none

! !ARGUMENTS:

  type(MAPL_MetaComp), intent(INOUT) :: META
  type (ESMF_Grid),    intent(  OUT) :: grid
  integer, optional,   intent(  OUT) :: rc

! ErrLog variables
!-----------------

  integer                      :: STATUS
  character(len=ESMF_MAXSTR), parameter :: Iam="AppGridCreate"

! Local variables
!-----------------
  RETURN_(ESMF_SUCCESS)

end subroutine AppGridCreate

#define R8  8
subroutine GetWeights_init (in_ntiles,in_ncnst,in_npx,in_npy,in_npz,&
                              in_nx,in_ny,in_hydro,in_mknh,comm)
  implicit none
  integer,intent(in) :: in_ntiles,in_ncnst
  integer,intent(in) :: in_npx,in_npy,in_npz
  integer,intent(in) :: in_nx,in_ny
  logical,intent(in) :: in_hydro,in_mknh
  integer            :: comm
end subroutine GetWeights_init
subroutine GetWeights(npx, npy, nlat, nlon, &
          index, weight, id1, id2, jdc, l2c,&
          ee1, ee2, ff1, ff2, gg1, gg2)
       integer,  intent(in   ) :: npx,  npy
       integer,  intent(in   ) :: nlon, nlat
       integer,  intent(inout) :: index(3,nlon,nlat)
       real(R8), intent(inout) :: weight(4,nlon,nlat)
       integer,  intent(inout) :: id1(npx,npy)
       integer,  intent(inout) :: id2(npx,npy)
       integer,  intent(inout) :: jdc(npx,npy)
       real(R8), intent(inout) :: l2c(4,npx,npy)
       real(R8), pointer       :: ee1(:,:,:) 
       real(R8), pointer       :: ee2(:,:,:) 
       real(R8), pointer       :: ff1(:,:,:) 
       real(R8), pointer       :: ff2(:,:,:) 
       real(R8), pointer, optional :: gg1(:,:,:) 
       real(R8), pointer, optional :: gg2(:,:,:) 
end subroutine GetWeights
