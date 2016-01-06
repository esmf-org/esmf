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
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! NASA/GSFC, Global Modeling and Assimilation Office, 900.3, GEOS/DAS  !
!BOP -------------------------------------------------------------------
!
! !MODULE: m_fpe - a module handling floating-point-exceptions
!
! !DESCRIPTION:
!
!   Examples:
!
!	use m_fpe,only : isNaN, isInf, isDen
!	real :: v
!	[...]
!	if(isNaN(v)) write(*,*) 'v is not-a-number.'
!	if(isInf(v)) write(*,*) 'v is infinity.'
!	if(isDen(v)) write(*,*) 'v is denormal.'
!
! !INTERFACE:
!#include "regime.H"

    module m_fpe
      implicit none
      private	! except

  public :: isNaN  ! is a NaN (not-a-number)

  public :: isInf  ! is an Inf (infinity), either -inf or +inf
	public :: ispInf ! is a +inf
	public :: isnInf ! is a -inf

  public :: isDen  ! is a denormal or underflow
	public :: ispDen ! is +.underflow.
	public :: isnDen ! is -.underflow.

	! Interfaces below are defined to support tests only.  Their
	! return values do not assume to be standard conforming.

  public :: aNaN ! a NaN value.  Its sign could be either + or -
  public :: aInf ! a +Inf value.  It is expected to be >0.
  public :: aDen ! a +Den value.  It is expected to be >0.

! !REVISION HISTORY:
! 	04Jan07	- Jing Guo <guo@gmao.gsfc.nasa.gov>
!		- initial prototype/prolog/code
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname='m_fpe'

  interface isNaN; module procedure &
    isNaNd_, &
    isNaNr_; end interface

  interface isInf; module procedure &
    isInfd_, &
    isInfr_; end interface

  interface ispInf; module procedure &
    ispInfd_, &
    ispInfr_; end interface

  interface isnInf; module procedure &
    isnInfd_, &
    isnInfr_; end interface

  interface isDen; module procedure &
    isDend_, &
    isDenr_; end interface

  interface ispDen; module procedure &
    ispDend_, &
    ispDenr_; end interface

  interface isnDen; module procedure &
    isnDend_, &
    isnDenr_; end interface

  interface aNaN; module procedure aNaNr_,aNaNd_; end interface
  interface aInf; module procedure aInfr_,aInfd_; end interface
  interface aDen; module procedure aDenr_,aDend_; end interface

  integer,parameter :: SP=kind(1.E0)
  integer,parameter :: DP=kind(1.D0)

  real(SP),parameter :: zero_SP=0._SP
  real(DP),parameter :: zero_DP=0._DP
  real(SP),parameter ::  one_SP=1._SP
  real(DP),parameter ::  one_DP=1._DP
  real(SP),parameter ::  two_SP=2._SP
  real(DP),parameter ::  two_DP=2._DP

  real(SP),parameter :: pHUGE_SP=+huge(1._SP)
  real(SP),parameter :: nHUGE_SP=-huge(1._SP)
  real(DP),parameter :: pHUGE_DP=+huge(1._DP)
  real(DP),parameter :: nHUGE_DP=-huge(1._DP)

  real(SP),parameter :: pTINY_SP=+tiny(1._SP)
  real(SP),parameter :: nTINY_SP=-tiny(1._SP)
  real(DP),parameter :: pTINY_DP=+tiny(1._DP)
  real(DP),parameter :: nTINY_DP=-tiny(1._DP)

contains
function isNaNr_(v) result(isNaN_)
  implicit none; logical :: isNaN_; real(SP),intent(in) :: v
  isNaN_ = .not. (v==zero_SP .or. v<zero_SP .or. v>zero_SP)
  end function isNaNr_

function isInfr_(v) result(isInf_)
  implicit none; logical :: isInf_; real(SP),intent(in) :: v
  isInf_=v<nHUGE_SP.or.v>pHUGE_SP
  ! isInf_ = .not. (nHUGE <= v .and. v <=pHUGE)
  end function isInfr_
function ispInfr_(v) result(isInf_)
  implicit none; logical :: isInf_; real(SP),intent(in) :: v
  isInf_=v>pHUGE_SP
  end function ispInfr_
function isnInfr_(v) result(isInf_)
  implicit none; logical :: isInf_; real(SP),intent(in) :: v
  isInf_=v<nHUGE_SP
  end function isnInfr_

function isDenr_(v) result(isDen_)
  implicit none; logical :: isDen_; real(SP),intent(in) :: v
  isDen_=nTINY_SP<v.and.v<pTINY_SP
  end function isDenr_
function ispDenr_(v) result(isDen_)
  implicit none; logical :: isDen_; real(SP),intent(in) :: v
  isDen_=zero_SP<v.and.v<pTINY_SP
  end function ispDenr_
function isnDenr_(v) result(isDen_)
  implicit none; logical :: isDen_; real(SP),intent(in) :: v
  isDen_=nTINY_SP<v.and.v<zero_SP
  end function isnDenr_

function isNaNd_(v) result(isNaN_)
  implicit none; logical :: isNaN_; real(DP),intent(in) :: v
  isNaN_=.not.(v==zero_DP .or. v<zero_DP .or. v>zero_DP)
  end function isNaNd_

function isInfd_(v) result(isInf_)
  implicit none; logical :: isInf_; real(DP),intent(in) :: v
  isInf_=v<nHUGE_DP.or.v>pHUGE_DP
  end function isInfd_
function ispInfd_(v) result(isInf_)
  implicit none; logical :: isInf_; real(DP),intent(in) :: v
  isInf_=v>pHUGE_DP
  end function ispInfd_
function isnInfd_(v) result(isInf_)
  implicit none; logical :: isInf_; real(DP),intent(in) :: v
  isInf_=v<nHUGE_DP
  end function isnInfd_

function isDend_(v) result(isDen_)
  implicit none; logical :: isDen_; real(DP),intent(in) :: v
  isDen_=nTINY_DP<v.and.v<pTINY_DP
  end function isDend_
function ispDend_(v) result(isDen_)
  implicit none; logical :: isDen_; real(DP),intent(in) :: v
  isDen_=zero_DP<v.and.v<pTINY_DP
  end function ispDend_
function isnDend_(v) result(isDen_)
  implicit none; logical :: isDen_; real(DP),intent(in) :: v
  isDen_=nTINY_DP<v.and.v<zero_DP
  end function isnDend_

!! Functions below are defined to generate specific cases of floating-
!! point-exceptions for unit-test purposes, which may not be used to
!! compare with any "value" of floaing-point-exception.

function aNaNr_(v)
  implicit none; real(SP) :: aNaNr_
  real(SP),intent(in) :: v ! a mode
  aNaNr_=zero_SP/zero_SP
  end function aNaNr_
function aNaNd_(v)
  implicit none; real(DP) :: aNaNd_
  real(DP),intent(in) :: v ! a mode
  aNaNd_=zero_DP/zero_DP
  end function aNaNd_

function aInfr_(v)
  implicit none; real(SP) :: aInfr_
  real(SP),intent(in) :: v ! a mode
  aInfr_=sign(one_SP/zero_SP,one_SP)
  end function aInfr_
function aInfd_(v)
  implicit none; real(DP) :: aInfd_
  real(DP),intent(in) :: v ! a mode
  aInfd_=sign(one_DP/zero_DP,one_DP)
  end function aInfd_

function aDenr_(v)
  implicit none; real(SP) :: aDenr_
  real(SP),intent(in) :: v ! a mode
  aDenr_=tiny(v)/two_SP
  end function aDenr_
function aDend_(v)
  implicit none; real(DP) :: aDend_
  real(DP),intent(in) :: v ! a mode
  aDend_=tiny(v)/two_DP
  end function aDend_
end module m_fpe
