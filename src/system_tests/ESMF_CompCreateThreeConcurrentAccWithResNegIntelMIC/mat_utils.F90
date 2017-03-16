module mat_utils
    implicit none
    public :: smmul2d, imic_mmul2d
contains
    integer function smmul2d(a, b, c) result(rc)
        implicit none
        real, dimension(:,:), intent(in) :: a
        real, dimension(:,:), intent(in) :: b
        real, dimension(:,:), intent(inout) :: c
        
        integer, parameter :: FAIL = -1
        integer, parameter :: SUCCESS = 0
        integer, parameter :: NDIMS = 2

        integer :: dims(NDIMS)

        real :: asum
        integer :: m, k, n, i, j, l
        
        rc = SUCCESS

        if((size(a) == 0) .or. (size(b) == 0) .or. (size(c) == 0)) then
            print *, "Size of arrays must be > 0"
            rc = FAIL
            return
        end if
        dims = shape(a)
        m = dims(1)
        k = dims(2)
        dims = shape(b)
        n = dims(2)

        if(dims(1) /= k) then
            print *, "Arrays a & b are not compatible"
            rc = FAIL
            return
        end if
        dims = shape(c)
        if((dims(1) /= m) .or. (dims(2) /= n)) then
            print *, "Dims of result array is not compatible"
            rc = FAIL
            return
        end if

        do i=1,m
            do j=1,n
                asum = 0.0
                do l=1,k
                    asum = asum + a(i,l) * b(l,j)
                end do
                c(i,j) = asum
            end do
        end do

        return
    end function smmul2d

    integer function imic_mmul2d(deviceid, a, b, c) result(rc)
        implicit none
        integer, intent(in) :: deviceid
        real, dimension(:,:), intent(in) :: a
        real, dimension(:,:), intent(in) :: b
        real, dimension(:,:), intent(inout) :: c
        
        integer, parameter :: FAIL = -1
        integer, parameter :: SUCCESS = 0
        integer, parameter :: NDIMS = 2

        integer :: dims(NDIMS)

        real :: asum
        integer :: m, k, n, i, j, l

        rc = SUCCESS

        if((size(a) == 0) .or. (size(b) == 0) .or. (size(c) == 0)) then
            print *, "Size of arrays must be > 0"
            rc = FAIL
            return
        end if
        dims = shape(a)
        m = dims(1)
        k = dims(2)
        dims = shape(b)
        n = dims(2)

        if(dims(1) /= k) then
            print *, "Arrays a & b are not compatible"
            rc = FAIL
            return
        end if
        dims = shape(c)
        if((dims(1) /= m) .or. (dims(2) /= n)) then
            print *, "Dims of result array is not compatible"
            rc = FAIL
            return
        end if

        !dir$ offload begin target(mic:deviceid) in(i,j,l,m,k,n) in(a:length(m*k)) in(b:length(k*n)) out(c:length(m*n))
        do i=1,m
            do j=1,n
                asum = 0.0
                do l=1,k
                    asum = asum + a(i,l) * b(l,j)
                end do
                c(i,j) = asum
            end do
        end do
        !dir$ end offload

        return
    end function imic_mmul2d
end module mat_utils
