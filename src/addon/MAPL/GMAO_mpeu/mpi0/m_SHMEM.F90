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
#if defined(HAVE_SHMEM)

Subroutine SHPALLOC(addr, length, errcode, abort)
  Integer(Kind=8), Intent(Out) :: addr
  Integer,         Intent(In)  :: length
  Integer,         Intent(Out) :: errcode
  Integer,         Intent(In)  :: abort
  
  addr = MALLOC(4*length)
  errcode = 0
  
End Subroutine SHPALLOC

Subroutine SHPDEALLC(addr, errcode, abort)
  Integer(Kind=8), Intent(InOut) :: addr
  Integer,         Intent(Out)   :: errcode
  Integer,         Intent(In)    :: abort
  
  Call Free(addr)
  errcode = 0
  
End Subroutine SHPDEALLC

Subroutine SHPCLMOVE(addr, length, status, abort)
  Integer(Kind=8), Intent(Out) :: addr
  Integer,         Intent(In)  :: length
  Integer,         Intent(Out) :: status
  Integer,         Intent(In)  :: abort
  
  ! Warning - should _move_ the existing memory, but we don't need to.
  Call Free(addr)
  addr = MALLOC(4*length)
  status = 0
  
End Subroutine SHPCLMOVE

Function SHMEM_PTR(target, pe) Result(addr)
  Integer, Intent(In) :: target
  Integer, Intent(In) :: pe
  Integer(Kind=8) :: addr
  
  addr = LOC(target)
  
End Function SHMEM_PTR

Subroutine SHMEM_BARRIER_ALL()
End Subroutine SHMEM_BARRIER_ALL

Integer Function NUM_PES()
  num_pes = 1
End Function NUM_PES

Integer Function MY_PE()
  my_pe = 1
End Function MY_PE

#else
	! Well, some compilers just don't like the idea of an empty
	! file.
subroutine SHMEM_dummy()
end subroutine SHMEM_dummy
#endif
Subroutine dummy0()
End Subroutine dummy0
