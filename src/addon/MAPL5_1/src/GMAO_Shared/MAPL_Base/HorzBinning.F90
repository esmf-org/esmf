

#define VERIFY_(A)   if(  A/=0) then; if(present(rc)) rc=A; PRINT *, __LINE__; return; endif
#define ASSERT_(A)   if(.not.A) then; if(present(rc)) rc=1; PRINT *, __LINE__; return; endif
#define RETURN_(A)   if(present(rc)) rc=A; return
#define SUCCESS      0
#define DEALOC_(A)   if(associated(A)) deallocate(A)


!BOP

! !MODULE: HorzTransMod
!    A Module to do linear transformations on 2-dimensional arrays


! !INTERFACE:

module MAPL_HorzTransMod

!  $Id$

  implicit none
  private

! PUBLIC TYPES:

  public MAPL_HorzTransform

! PUBLIC MEMBER FUNCTIONS:

  public MAPL_HorzTransformCreate
  public MAPL_HorzTransformDestroy
  public MAPL_HorzTransformRun

! !DESCRIPTION:

! This package performs a serial linear transformation with a
!   previously computed sparse matrix stored in a HorzBinTransform
!   object.  Currently the Transform objects that can be created
!   are limited to ``binning'' or conservative transformations
!   and bilinear intepolation of 2-dimensional (LogicalRectangular)
!   grids that span the same domain.
!
! Either dimension of the array can be cyclic or bounded. If bounded
!   the first and last elements in that dimension are assumed to be on
!   common domain boundaries, and so there is one fewer grid interval
!   than the size of that dimension. If not cyclic there are as many grid
!   intervals as the dimension's size.
!

!EOP

  type Mapping
     integer       :: n0,n1
     real, pointer :: f(:)
  end type Mapping

  type MAPL_HorzTransform
     private
     integer :: im_in, jm_in, im_out, jm_out
     logical :: cyclic(2)
     real    :: InOfOneAsOut(2)
     real, pointer :: dx_in (:), dy_in (:)
     real, pointer :: dx_out(:), dy_out(:)
     type(Mapping), pointer :: weightx(:)
     type(Mapping), pointer :: weighty(:)
  end type MAPL_HorzTransform

  interface MAPL_HorzTransformCreate
     module procedure HorzTransformCreateBySize
  end interface

  
  interface MAPL_HorzTransformRun
     module procedure HorzBinTransformRunOnly
     module procedure HorzBinTransformTransAndRun
  end interface

contains

  subroutine HorzTransformCreateBySize(Trans,         &
                                       im_in , jm_in, &
                                       im_out, jm_out,&
                                       cyclic,        &
                                       InOfOneAsout,  &
                                                    rc)
    type (MAPL_HorzTransform), intent(OUT) :: Trans
    integer,              intent(IN ) :: im_in, jm_in, im_out, jm_out
    logical, optional,    intent(IN ) :: cyclic(2) 
    real,    optional,    intent(IN ) :: InOfOneAsOut(2)
    integer, optional,    intent(OUT) :: rc

    integer :: status
    integer :: i, j

    Trans%im_in  = im_in
    Trans%jm_in  = jm_in
    Trans%im_out = im_out
    Trans%jm_out = jm_out

    if(present(cyclic)) then
       Trans%cyclic = cyclic
    else
       Trans%cyclic(1) = .true.
       Trans%cyclic(2) = .false.
    end if

    if(present(InOfOneAsOut)) then
       Trans%InOfOneAsOut = InOfOneAsOut
    else
       Trans%InOfOneAsOut(1) = 1
       Trans%InOfOneAsOut(2) = 1
    end if

    if(Trans%cyclic(1)) then
       ASSERT_(Trans%InOfOneAsOut(1)>=0       )
       ASSERT_(Trans%InOfOneAsOut(1)< im_out+1)
    end if

    if(Trans%cyclic(2)) then
       ASSERT_(Trans%InOfOneAsOut(2)>=0       )
       ASSERT_(Trans%InOfOneAsOut(2)< jm_out+1)
    end if

    allocate(Trans%weightx(im_out), stat=STATUS)
    VERIFY_(STATUS)
    allocate(Trans%weighty(jm_out), stat=STATUS)
    VERIFY_(STATUS)

    call CreateMappingUniform(Trans, rc=status)
    VERIFY_(STATUS)

    RETURN_(SUCCESS)
  end subroutine HorzTransformCreateBySize


  subroutine MAPL_HorzTransformDestroy(Trans, rc)
    type (MAPL_HorzTransform), intent(INOUT) :: Trans
    integer, optional,    intent(  OUT) :: rc

    integer :: status
    integer :: i, j

    do i=1,Trans%IM_out
       call DestroyMapping(Trans%weightx(i), RC=STATUS)
       VERIFY_(STATUS)
    end do

    do j=1,Trans%JM_out
       call DestroyMapping(Trans%weighty(j), RC=STATUS)
       VERIFY_(STATUS)
    end do

    DEALOC_(Trans%weightx)
    DEALOC_(Trans%weighty)
    DEALOC_(Trans%dx_out )
    DEALOC_(Trans%dx_in  )
    DEALOC_(Trans%dy_out )
    DEALOC_(Trans%dy_in  )

    RETURN_(SUCCESS)
  end subroutine MAPL_HorzTransformDestroy

  subroutine CreateMappingUniform(Trans, rc)
    type (MAPL_HorzTransform), intent(INOUT) :: Trans
    integer, optional,    intent(  OUT) :: rc

    integer :: status
    integer :: i, j
    integer :: i0,i1,j0,j1
    integer :: ishft, jshft
    real    :: dx, dy, dx2, dy2, xu, xl, ff, x0, xx
    integer :: i_out, j_out

    real,     pointer :: x(:), y(:)
    real,     pointer :: a(:), b(:)

    if(Trans%cyclic(1)) then
       dx    = float(Trans%IM_OUT  )/float(Trans%IM_IN  )
       ISHFT = (Trans%IM_IN-1)/(2*Trans%IM_OUT) + 1
    else
       dx    = float(Trans%IM_OUT-1)/float(Trans%IM_IN-1)
       ISHFT = 0
    end if

    if(Trans%cyclic(2)) then
       dy    = float(Trans%JM_OUT  )/float(Trans%JM_IN  )
       JSHFT = (Trans%JM_IN-1)/(2*Trans%JM_OUT) + 1
    else
       dy    = float(Trans%JM_OUT-1)/float(Trans%JM_IN-1)
       JSHFT = 0
    end if

    dx2   = 0.5*dx
    dy2   = 0.5*dy

    allocate(X(1-ISHFT:Trans%IM_in+ISHFT), stat=STATUS)
    VERIFY_(STATUS)
    allocate(Y(1-JSHFT:Trans%JM_in+JSHFT), stat=STATUS)
    VERIFY_(STATUS)


    do i=1-ISHFT, Trans%IM_in+ISHFT
       X(i) = Trans%InOfOneAsOut(1) + (i-1)*dx
    end do

    do j=1,Trans%JM_in
       Y(j) = Trans%InOfOneAsOut(2) + (j-1)*dy
    end do

!  Compute the J weights
!-----------------------

    do j_out=1,Trans%jm_out

       if(.not.Trans%cyclic(2)) then
          if(j_out==1) then
             j0 = 1
             j1 = 1
          elseif(j_out==Trans%JM_OUT) then
             j0 = Trans%JM_in
             j1 = j0
          else
             xx = float(j_out)
             xu = xx + 0.5
             xl = xx - 0.5
             do j0=1,Trans%JM_in
                if(y(j0)+dy2 <= xl) cycle
                exit
             end do

             do j1=1,Trans%JM_in
                if(y(j1)-dy2 <  xu) cycle
                exit
             end do
             j1=j1-1
          end if
       else
          xx = Trans%InOfOneAsOut(2) + float(j_out-1)
          xu = xx + 0.5
          xl = xx - 0.5

          do j0=1-JSHFT,Trans%JM_in+JSHFT
             if(y(j0)+dy2 <= xl) cycle
             exit
          end do

          do j1=1-JSHFT,Trans%IM_in+JSHFT
             if(y(j1)-dy2 <  xu) cycle
             exit
          end do
          j1=j1-1
       end if

       ASSERT_(j0 >=           1 )
       ASSERT_(j1 <= Trans%JM_in )
       ASSERT_(j1 >=          j0 )

       allocate(b(j0:j1), stat=STATUS)
       VERIFY_(STATUS)

       ff = 0.0
       do j=j0,j1
          if    (xu<y(j)+dy2 .and. xu>y(j)-dy2) then
             b(j) = xu-y(j)+dy2
          elseif(xl<y(j)+dy2 .and. xl>y(j)-dy2) then
             b(j) = y(j)+dy2-xl
          else
             b(j) = dy
          end if
          ff = ff + b(j)
       end do
       b(j0:j1) = b(j0:j1)/ff

       Trans%Weighty(j_out)%n0 =  j0
       Trans%Weighty(j_out)%n1 =  j1
       Trans%Weighty(j_out)%f  => b
    end do

!  Compute the I weights
!-----------------------
    
    do i_out=1,Trans%IM_out
       if(Trans%cyclic(1)) then
          xx = Trans%InOfOneAsOut(1) + float(i_out-1)
          xu = xx + 0.5
          xl = xx - 0.5

          do i0=1-ISHFT,Trans%IM_in+ISHFT
             if(x(i0)+dx2 <= xl) cycle
             exit
          end do

          do i1=1-ISHFT,Trans%IM_in+ISHFT
             if(x(i1)-dx2 <  xu) cycle
             exit
          end do
          i1=i1-1
       else
          if(i_out==1) then
             i0 = 1
             i1 = 1
          elseif(i_out==Trans%IM_OUT) then
             i0 = Trans%IM_in
             i1 = i0
          else
             xx = float(i_out)
             xu = xx + 0.5
             xl = xx - 0.5
             do i0=1,Trans%IM_in
                if(x(i0)+dx2 <= xl) cycle
                exit
             end do

             do i1=1,Trans%IM_in
                if(x(i1)-dx2 <  xu) cycle
                exit
             end do
             i1=i1-1
          end if
       end if

       ASSERT_(i0 >=           1-ISHFT)
       ASSERT_(i1 <= Trans%IM_in+ISHFT)
       ASSERT_(i1 >=                i0)

       allocate(a(i0:i1), stat=STATUS)
       VERIFY_(STATUS)

       ff = 0.0
       do i=i0,i1
          if    (xu<x(I)+dx2 .and. xu>x(i)-dx2) then
             a(i) = xu-x(i)+dx2
          elseif(xl<x(I)+dx2 .and. xl>x(i)-dx2) then
             a(i) = x(i)+dx2-xl
          else
             a(i) = dx
          end if
          ff = ff + a(i)
       end do
       a(i0:i1) = a(i0:i1)/ff

       Trans%Weightx(I_out)%n0 =  i0
       Trans%Weightx(I_out)%n1 =  i1
       Trans%Weightx(I_out)%f  => a

    end do

    DEALOC_(x)
    DEALOC_(y)

    RETURN_(SUCCESS)
  end subroutine CreateMappingUniform

  subroutine DestroyMapping(MAP, rc)
    type (Mapping),    intent(INOUT) :: Map
    integer, optional, intent(  OUT) :: rc

    DEALOC_(Map%f)

    RETURN_(SUCCESS)
  end subroutine DestroyMapping

  subroutine HorzBinTransformRunOnly (Trans, qin, qout, undef, rc)

!    Trans ....  Precomputed transform
!      qin ....  Input Variable
!      qout....  Output Variable
!    undef ....  UNDEF Value

    type (MAPL_HorzTransform), intent(IN   ) :: Trans
    real,                    intent(IN   ) :: qin (:,:)
    real,                    intent(OUT  ) :: qout(:,:)
    real,    optional,       intent(IN   ) :: undef
    integer, optional,       intent(  OUT) :: rc

    real    :: w, ww, ff
    integer :: STATUS
    integer :: i,j
    integer :: ii,jj,i0,i1,j0,j1
    integer :: im, ishft
    integer, allocatable :: IX(:)
    real, pointer        :: fx(:), fy(:)

    if(present(rc)) rc = 0

    ASSERT_(size(qin ,1) == Trans%IM_in )
    ASSERT_(size(qin ,2) == Trans%JM_in )
    ASSERT_(size(qout,1) == Trans%IM_out)
    ASSERT_(size(qout,2) == Trans%JM_out)

    IM    = Trans%IM_in
    ISHFT = (IM - 1)/(2*Trans%IM_OUT) + 1

    allocate(IX(1-ISHFT:IM+ISHFT), stat=STATUS)
    VERIFY_(STATUS)

    do I=1-ISHFT, IM+ISHFT
       ix(i) = mod(I+IM-1,IM) + 1
    enddo

    do j=1,Trans%JM_out
       j0        = Trans%Weighty(j)%n0
       j1        = Trans%Weighty(j)%n1
       fy        =>Trans%Weighty(j)%f

       do i=1,Trans%IM_out
          qout(i,j) = 0.0
          i0        = Trans%Weightx(I)%n0
          i1        = Trans%Weightx(I)%n1
          fx        =>Trans%Weightx(I)%f

          if(present(undef)) then
             w = 0.0
             do jj=j0,j1
                ff = 0.0
                ww = 0.0
                do II=i0,i1
                   if(qin(ix(ii),jj) /= undef) then
                      ff        = ff + qin(ix(ii),jj)*fx(ii)
                      ww        = ww +                fx(ii)
                   end if
                end do
                qout(i,j) = qout(i,j) + ff*fy(jj)
                w         = w         + ww*fy(jj)
             end do
          else
             do jj=j0,j1
                ff = 0.0
                do II=i0,i1
                   ff = ff + qin(ix(ii),jj)*fx(ii)
                end do
                qout(i,j) = qout(i,j) + ff*fy(jj)
             end do
          end if

          ! average if necessary

          if(present(undef)) then
             if ( w /= 0.0 ) then
                qout(i,j) = qout(i,j) / w
             else
                qout(i,j) = undef
             end if
          end if

       end do
    end do

    deallocate(ix)

    RETURN_(SUCCESS)
  end subroutine HorzBinTransformRunOnly

  subroutine HorzBinTransformTransAndRun (qin, qout, undef, rc)

    !      qin ....  Input Variable
    !      qout....  Output Variable
    !    undef ....  UNDEF Value

    real,                    intent(IN   ) :: qin (:,:)
    real,                    intent(OUT  ) :: qout(:,:)
    real,    optional,       intent(IN   ) :: undef
    integer, optional,       intent(  OUT) :: rc

    integer :: STATUS

    type (MAPL_HorzTransform) :: Trans

    call MAPL_HorzTransformCreate (Trans, size(qin,1),size(qin,2), size(qout,1),size(qout,2), rc=STATUS)
    VERIFY_(STATUS)
    call HorzBinTransformRunOnly(Trans, qin, qout, undef=undef                            , rc=STATUS)
    VERIFY_(STATUS)
    call MAPL_HorzTransformDestroy (Trans                                                    , rc=STATUS)
    VERIFY_(STATUS)
   
    RETURN_(SUCCESS)

  end subroutine HorzBinTransformTransAndRun

end module MAPL_HorzTransMod


