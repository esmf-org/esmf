
module MAPL_InterpMod

  use ESMF

implicit none
private

public MAPL_Interp

interface MAPL_Interp
   module procedure INTERP_LIN_0011_1
   module procedure INTERP_LIN_0011_2
   module procedure INTERP_LIN_1111_1
   module procedure INTERP_LIN_1111_2
   module procedure INTERP_LIN_2111_1
   module procedure INTERP_LIN_2111_2
   module procedure INTERP_LIN_2121_1
   module procedure INTERP_LIN_2121_2
   module procedure INTERP_LIN_3321_1
   module procedure INTERP_LIN_3321_2
end interface

contains

!BOP

! !IROUTINE: INTERP_LIN_0011

! !DESCRIPTION: Interpolates linearly in a 1-D table
! \newline
!

! !INTERFACE:

subroutine INTERP_LIN_0011_1( OY, OX, IY, IX)

! !ARGUMENTS:

  real,     intent(OUT) :: OY
  real,     intent(IN ) :: OX
  real,     intent(IN ) :: IY(:)
  real,     intent(IN ) :: IX(:)

!EOP

  integer    J

  J        = min(max(count(IX<=OX), 1), size(IX)-1)
  if   (IX(J+1)/=IX(J)) then
     OY = IY(J) + ((OX-IX(J)) / (IX(J+1)-IX(J)))*(IY(J+1) - IY(J))
  else
     OY = IY(J)
  end if

  return
end subroutine INTERP_LIN_0011_1

subroutine INTERP_LIN_0011_2( OY, OX, IY, IX)

! !ARGUMENTS:

  real(kind=ESMF_KIND_R8),     intent(OUT) :: OY
  real(kind=ESMF_KIND_R8),     intent(IN ) :: OX
  real(kind=ESMF_KIND_R8),     intent(IN ) :: IY(:)
  real(kind=ESMF_KIND_R8),     intent(IN ) :: IX(:)

!EOP

  integer    J

  J        = min(max(count(IX<=OX), 1), size(IX)-1)
  if   (IX(J+1)/=IX(J)) then
     OY = IY(J) + ((OX-IX(J)) / (IX(J+1)-IX(J)))*(IY(J+1) - IY(J))
  else
     OY = IY(J)
  end if

  return
end subroutine INTERP_LIN_0011_2

!=========================================================================

subroutine INTERP_LIN_1111_1( OY, OX, IY, IX)

  real,     intent(OUT) :: OY(:)
  real,     intent(IN ) :: OX(:)
  real,     intent(IN ) :: IY(:)
  real,     intent(IN ) :: IX(:)

  integer J

  do J=1,size(OY)
     call INTERP_LIN_0011_1(oy(j),ox(j),iy,ix)
  end do

  return
end subroutine INTERP_LIN_1111_1

!=========================================================================

subroutine INTERP_LIN_1111_2( OY, OX, IY, IX)

  real(kind=ESMF_KIND_R8),     intent(OUT) :: OY(:)
  real(kind=ESMF_KIND_R8),     intent(IN ) :: OX(:)
  real(kind=ESMF_KIND_R8),     intent(IN ) :: IY(:)
  real(kind=ESMF_KIND_R8),     intent(IN ) :: IX(:)

  integer J

  do J=1,size(OY)
     call INTERP_LIN_0011_2(oy(j),ox(j),iy,ix)
  end do

  return
end subroutine INTERP_LIN_1111_2

!=========================================================================

subroutine INTERP_LIN_2121_1( OY, OX, IY, IX)
  real,     intent(OUT) :: OY(:,:)
  real,     intent(IN ) :: OX(:)
  real,     intent(IN ) :: IY(:,:)
  real,     intent(IN ) :: IX(:)

  integer J

  do J=1,size(OY,2)
     call INTERP_LIN_1111_1(oy(:,j),ox,iy(:,j),ix)
  end do

  return
end subroutine INTERP_LIN_2121_1

!=========================================================================

subroutine INTERP_LIN_2121_2( OY, OX, IY, IX)
  real(kind=ESMF_KIND_R8),     intent(OUT) :: OY(:,:)
  real(kind=ESMF_KIND_R8),     intent(IN ) :: OX(:)
  real(kind=ESMF_KIND_R8),     intent(IN ) :: IY(:,:)
  real(kind=ESMF_KIND_R8),     intent(IN ) :: IX(:)

  integer J
  
  do J=1,size(OY,2)
     call INTERP_LIN_1111_2(oy(:,j),ox,iy(:,j),ix)
  end do

  return
end subroutine INTERP_LIN_2121_2
  
!=========================================================================

subroutine INTERP_LIN_2111_1( OY, OX, IY, IX)

  real,     intent(OUT) :: OY(:,:)
  real,     intent(IN ) :: OX(:)
  real,     intent(IN ) :: IY(:)
  real,     intent(IN ) :: IX(:)

  integer J

  do J=1,size(OY,2)
     call INTERP_LIN_1111_1( OY(:,J), OX, IY, IX)
  end do

  return
end subroutine INTERP_LIN_2111_1

!=========================================================================

subroutine INTERP_LIN_2111_2( OY, OX, IY, IX)

  real(kind=ESMF_KIND_R8),     intent(OUT) :: OY(:,:)
  real(kind=ESMF_KIND_R8),     intent(IN ) :: OX(:)
  real(kind=ESMF_KIND_R8),     intent(IN ) :: IY(:)
  real(kind=ESMF_KIND_R8),     intent(IN ) :: IX(:)

  integer J

  do J=1,size(OY,2)
     call INTERP_LIN_1111_2( OY(:,J), OX, IY, IX)
  end do

  return
end subroutine INTERP_LIN_2111_2

!=========================================================================

subroutine INTERP_LIN_3321_1( OY, OX, IY, IX)

  real,     intent(OUT) :: OY(:,:,:)
  real,     intent(IN ) :: OX(:,:,:)
  real,     intent(IN ) :: IY(:,:)
  real,     intent(IN ) :: IX(:)

  integer I,J

  do J=1,size(OY,2)
     do I=1,size(OY,1)
        call INTERP_LIN_1111_1( OY(I,J,:), OX(I,J,:), IY(J,:), IX(:))
     end do
  end do

  return
end subroutine INTERP_LIN_3321_1

!=========================================================================

subroutine INTERP_LIN_3321_2( OY, OX, IY, IX)

  real(kind=ESMF_KIND_R8),     intent(OUT) :: OY(:,:,:)
  real(kind=ESMF_KIND_R8),     intent(IN ) :: OX(:,:,:)
  real(kind=ESMF_KIND_R8),     intent(IN ) :: IY(:,:)
  real(kind=ESMF_KIND_R8),     intent(IN ) :: IX(:)

  integer I,J

  do J=1,size(OY,2)
     do I=1,size(OY,1)
        call INTERP_LIN_1111_2( OY(I,J,:), OX(I,J,:), IY(J,:), IX(:))
     end do
  end do

  return
end subroutine INTERP_LIN_3321_2

end module MAPL_InterpMod
