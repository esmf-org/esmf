! $Id: ESMF_SysTest70385.F90,v 1.4 2003/03/10 03:23:15 cdeluca Exp $
!
! System test code #70385

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

!BOP
!
! !DESCRIPTION:
! System test number 70385.
!
!
!\begin{verbatim}

    program ESMF_SysTest70385

#include "ESMF.h"

!   ! Modules needed
!   TODO: (these will be collapsed into a single ESMF_Mod soon)
    use ESMF_BaseMod
    use ESMF_IOMod
    use ESMF_DELayoutMod
    use ESMF_ArrayMod
    use ESMF_GridMod
    use ESMF_DataMapMod
    use ESMF_FieldMod
    use ESMF_CompMod
    
    implicit none
    
!   Local variables
    integer :: nx, ny, i, j, k, ni, nj, rc
    integer, dimension(12) :: delist
    integer :: timestep
    integer :: de_id
    integer :: i_max, j_max
    integer :: horz_gridtype, vert_gridtype, halo_width
    integer :: horz_stagger, vert_stagger
    integer :: horz_coord_system, vert_coord_system
    integer :: status
    real :: x_min, x_max, y_min, y_max
    real :: temp, temp1
    integer(ESMF_IKIND_I4), dimension(:,:), pointer :: idata, idata2, &
                                                       ldata
    character(len=ESMF_MAXSTR) :: cname, gname, fname
    type(ESMF_AxisIndex), dimension(ESMF_MAXGRIDDIM) :: index
    type(ESMF_DELayout) :: layout1 
    type(ESMF_Grid) :: grid1
    type(ESMF_Array) :: array1, array2
    type(ESMF_Field) :: field1
    type(ESMF_Comp) :: comp1
        
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

    print *, "System Test #70385:"

!
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!    Create section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!

!   Create a DELayout for the Component
    delist = (/ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11 /)
    layout1 = ESMF_DELayoutCreate(3, 4, delist, ESMF_XFAST, rc)

    cname = "System Test #70385"
    comp1 = ESMF_CompCreate(cname, layout=layout1, ctype=ESMF_GRIDCOMP, &
                            mtype=ESMF_ATM, filepath="/usr/local", rc=rc)

    print *, "Comp Create finished, name = ", trim(cname)

!
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!  Init section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!
      call ESMF_CompInitialize(comp1, rc)

      print *, "Comp Init finished, name = ", trim(cname)

!     The user creates a simple horizontal Grid internally by passing all
!     necessary information through the CreateInternal argument list.

      i_max = 30
      j_max = 36
      horz_gridtype = ESMF_GridType_XY
      horz_stagger = ESMF_GridStagger_A
      horz_coord_system = ESMF_CoordSystem_Cartesian
      x_min = 0.0
      x_max = 15.0
      y_min = 0.0
      y_max = 12.0
      halo_width = 2
      gname = "test grid 1"

      grid1 = ESMF_GridCreate(i_max=i_max, j_max=j_max, &
                             layout=layout1, &
                             horz_gridtype=horz_gridtype, &
                             horz_stagger=horz_stagger, &
                             horz_coord_system=horz_coord_system, &
                             halo_width=halo_width, &
                             x_min=x_min, x_max=x_max, &
                             y_min=y_min, y_max=y_max, &
                             name=gname, rc=status)

      print *, "Grid Create returned"

!     Figure out our local processor id
      call ESMF_DELayoutGetDEId(layout1, de_id, rc)

!     Allocate arrays.
      call ESMF_GridGetDE(grid1, lcelltot_index=index, rc=rc)
      ni = index(1)%r - index(1)%l + 1
      nj = index(2)%r - index(2)%l + 1
      print *, "allocating", ni, " by ",nj," cells on DE", de_id
      allocate(idata(ni,nj))
      allocate(ldata(ni,nj))

!     Set initial data values over whole array to -1
      do j=1,nj
        do i=1,ni
          ldata(i,j) = -1
        enddo
      enddo

!     Set initial data values over exclusive domain to the de identifier
      call ESMF_GridGetDE(grid1, lcellexc_index=index, rc=rc)
      do j=index(2)%l,index(2)%r
        do i=index(1)%l,index(1)%r
          ldata(i,j) =de_id
        enddo
      enddo

!     Create Array based on an existing, allocated F90 pointer.
!     Data is type Integer, 2D.
      array1 = ESMF_ArrayCreate(ldata, ESMF_NO_COPY, rc)
      print *, "Array Create returned"

!     No deallocate() is needed for idata, it will be freed when the
!     Array is destroyed.  TODO:  it seems delete need a 'delete data' 
!     option so the user can choose whether to have esmf delete the space 
!     or continue to use the f90 array after the Array is gone?

!     Create a Field using the Grid and Arrays created above
      fname = "DE id"
      field1 = ESMF_FieldCreate(grid1, array1, relloc=ESMF_CELL_CENTER, &
                                name=fname, rc=rc)

      print *, "Field Create returned"

      print *, "Comp Init finished"

!
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!     Run section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!

      timestep = 1
      call ESMF_CompRun(comp1, rc)

!     Call Field method to halo
      call ESMF_FieldHalo(field1, rc)

!     Get a pointer to the data Array in the Field
      call ESMF_FieldGetData(field1, array2, rc=rc)
      print *, "data back from field"

!     Get a pointer to the start of the data
      call ESMF_ArrayGetData(array2, idata2, ESMF_NO_COPY, rc)

      print *, "Comp Run returned"

!
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!     Finalize section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!     Print result

!     First, kill some time on all processes
      do i = 1,10000
        call random_number(temp)
        temp1 = sin(temp) * cos(temp)
      enddo
!     Then print out results, one process at a time -- other processes
!     do busy work to keep output clean
      do k = 0,size(delist)-1
        if (de_id.eq.k) then
          print *, "------------------------------------------------------"
          write(*,*) 'de_id = ',de_id
          do j = nj,1,-1
            write(*,10) (ldata(i,j), i=1,ni)
   10       format(20(1x,i2))
          enddo
          print *, "------------------------------------------------------"
        else
          do i = 1,10000
            call random_number(temp)
            temp1 = sin(temp) * cos(temp)
          enddo
        endif
      enddo
!     At the end, kill some time on all processes again before continuing
!     TODO:  should be a Comm method to sync up all processes
      do i = 1,10000
        call random_number(temp)
        temp1 = sin(temp) * cos(temp)
      enddo

      call ESMF_CompFinalize(comp1, rc)

      print *, "Comp Finalize returned"

!
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!     Destroy section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!     Clean up

      call ESMF_CompDestroy(comp1, rc)
      call ESMF_FieldDestroy(field1, rc)
      call ESMF_GridDestroy(grid1, rc)
      call ESMF_ArrayDestroy(array1, rc)
      call ESMF_DELayoutDestroy(layout1, rc)
      print *, "All Destroy routines done"

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
      print *, "System Test #70385 complete!"

      end program ESMF_SysTest70385
    
!\end{verbatim}
    
