
!
! Simple test program to test rekind strategies.
!

     Program utDownBit

     use ESMF_CFIOMod, only: ESMF_CFIODownBit

     implicit NONE

     integer, parameter :: im = 288, jm=181, lu=10
     real :: x(im,jm),  xmin, xmax
     real*4 :: xr(im,jm)
     integer :: nbits, rc

     character(len=80) :: ifname, fname

     print *, 'Enter 2D sequential data set name: '
     read *, ifname

     open(lu,file=trim(ifname),form='unformatted')
     read(lu) x
     close(lu)

     xmin = minval(x)
     xmax = maxval(x)

     print *, 'File name: ', trim(ifname)
     print *, ' -- Min: ',xmin, ' -- Max: ',xmax, &
              ' -- Range: ', xmax-xmin

     call system ( 'mkdir -p done' )

     print *

!    Slow flops
!    ----------
     do nbits = 16, 8, -1

        call ESMF_CFIODownBit ( x, xr, nbits, 1.0E15, flops=.true., rc=rc )
        if ( rc /= 0 ) then
             print *, 'Error: rc = ', rc
             call exit(1)
        endif
  
        call stats_ ( 'Mean/max error [F]: ', &
                       nbits, xr-x, 100*(xr-x)/x, im, jm )

        write(fname,"('done/',a2,'_f',I2.2,'.bin')") ifname(1:2), nbits
        open(lu,file=trim(fname),form='unformatted')
        write(lu) xr
        close(lu)

     end do

     print *

!    Fast bit shifts
!    ---------------
     do nbits = 16, 8, -1

        call ESMF_CFIODownBit ( x, xr, nbits, 1.0E15, rc=rc )
        if ( rc /= 0 ) then
             print *, 'Error: rc = ', rc
             call exit(1)
        endif
  
  
        call stats_ ( 'Mean/max error [C]: ', &
                       nbits, xr-x, 100*(xr-x)/x, im, jm )

        write(fname,"('done/',a2,'_c',I2.2,'.bin')") ifname(1:2), nbits
        open(lu,file=trim(fname),form='unformatted')
        write(lu) xr
        close(lu)

     end do

   end Program utDownBit

!.......................................................................

   subroutine stats_ ( title, bits, aerr, perr, im, jm )
         character(len=*) :: title
         real, intent(in) :: aerr(im,jm), perr(im,jm)
         integer :: n, bits
         real    :: amean, pmean
         n = im * jm
         amean = sum(aerr) / n 
         pmean = sum(perr) / n 
         print 10, title, bits, amean, maxval(abs(aerr)), &
                                pmean, maxval(abs(perr))
10       format(a20,i3,'  |'2(E14.6),'   |',f14.6,' %',f14.6,' %  |')
!         print *, '   Mean: ', emean
!         print *, '   Supp:',  maxval(abs(err))
!         print *, '   Stdv:',  sqrt(sum((err-emean)**2)/(n-1)) 
    end subroutine stats_
