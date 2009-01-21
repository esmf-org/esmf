! $Id: ESMF_FieldEx.F90,v 1.1.2.8 2009/01/21 21:25:20 cdeluca Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2009, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
!
    program ESMF_FieldEx

!------------------------------------------------------------------------------
!ESMF_EXAMPLE        String used by test script to count examples.
!==============================================================================
! !PROGRAM: ESMF_FieldEx - Field Examples
!
! !DESCRIPTION:
!
! This program shows examples of Field get data pointer methods
!-----------------------------------------------------------------------------
#include "ESMF.h"
    ! ESMF Framework module
    use ESMF_TestMod
    use ESMF_Mod
    implicit none
    
    ! Local variables
    integer :: rc

    type(ESMF_DistGrid) :: distgrid
    type(ESMF_ArraySpec) :: arrayspec
    real(ESMF_KIND_R8), dimension(:,:,:), pointer :: farray
    real(ESMF_KIND_R8), dimension(:,:,:), pointer :: farray1
    type(ESMF_Field)  :: field, field1, field3, field4
    type(ESMF_Grid)   :: grid3d, grid, grid2d
    type(ESMF_DistGrid) :: distgrid3d
    type(ESMF_Array)  :: array3d, array, array2d
    integer           :: xdim, ydim, zdim

    real(ESMF_KIND_R4), dimension(:,:), pointer :: farray2dd
    real(ESMF_KIND_R4), dimension(:,:), pointer :: farray2d
    integer, dimension(2) :: compEdgeLWdith
    integer, dimension(2) :: compEdgeUWdith
    integer             :: ftlb(2), ftub(2), ftc(2)

    integer :: compLBnd(1:3), compUBnd(1:3)
    integer :: exclLBnd(1:3), exclUBnd(1:3)
    integer :: totalLBnd(1:3), totalUBnd(1:3)

    integer :: comp_count(1:3)
    integer :: excl_count(1:3)
    integer :: total_count(1:3)

    type(ESMF_TypeKind)        :: typekind
    integer                    :: dimCount
    type(ESMF_StaggerLoc)      :: staggerloc 
    integer                    :: gridToFieldMap(ESMF_MAXDIM)    
    integer                    :: ungriddedLBound(ESMF_MAXDIM)
    integer                    :: ungriddedUBound(ESMF_MAXDIM)
    integer                    :: maxHaloLWidth(ESMF_MAXDIM)
    integer                    :: maxHaloUWidth(ESMF_MAXDIM)
    integer                    :: fa_shape(3)
    character(len=32)          :: name

    real(ESMF_KIND_R8), dimension(:,:,:,:,:,:,:), pointer :: farray7d
    real(ESMF_KIND_R8), dimension(:,:,:,:,:,:,:), pointer :: farray7d2
    type(ESMF_Field)    :: field7d, field7d2
    type(ESMF_Grid)     :: grid5d
    type(ESMF_DistGrid) :: distgrid5d
    integer             :: fsize(7)
    integer             :: flbound(7), fubound(7)

    real(4) :: PI=3.14159265
    integer :: finalrc, i, j, k
!   !Set finalrc to success
    finalrc = ESMF_SUCCESS

    call ESMF_Initialize(rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

    if (.not. ESMF_TestMinPETs(4, ESMF_SRCLINE)) &
        call ESMF_Finalize(terminationflag=ESMF_ABORT)

!>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%
!-------------------------------- Example -----------------------------
!>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%
!BOE
!\subsubsection{Get Fortran data pointer, bounds, and counts information from a Field}
!\label{sec:field:usage:field_get_dataptr}
!
!  A user can get bounds and counts information from an {\tt ESMF\_Field}
!  through the {\tt ESMF\_FieldGet()} interface.  Also available through this interface
!  is the intrinsic
!  Fortran data pointer contained in the internal {\tt ESMF\_Array} object
!  of an {\tt ESMF\_Field}. The bounds and counts information are DE specific
!  for the associated Fortran data pointer.
!
!  In this example, we first create a 3D Field based on a 3D Grid and Array.
!  Then we use the {\tt ESMF\_FieldGet()} interface to retrieve the data pointer,
!  potentially updating or verifying its values. We also retrieve the bounds and counts
!  information of the 3D Field to assist in data element iteration.
!
!EOE
!BOC
    xdim = 180
    ydim = 90
    zdim = 50

    ! create a 3D data Field from a Grid and Array.
    ! first create a Grid 
    grid3d = ESMF_GridCreateShapeTile(minIndex=(/1,1,1/), maxIndex=(/xdim,ydim,zdim/), &
                              regDecomp=(/2,2,1/), name="grid", rc=rc)
    if(rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE

    call ESMF_GridGet(grid3d, distgrid=distgrid3d, rc=rc)
    if(rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE

    call ESMF_FieldGet(grid3d, localDe=0, staggerloc=ESMF_STAGGERLOC_CENTER, &
        totalCount=fa_shape, rc=rc)
    if(rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE

    allocate(farray(fa_shape(1), fa_shape(2), fa_shape(3)) )

    ! create an Array 
    array3d = ESMF_ArrayCreate(farray, distgrid=distgrid3d, &
        indexflag=ESMF_INDEX_DELOCAL, staggerloc=0, &
        computationalEdgeLWidth=(/0,0,0/), &
        computationalEdgeUWidth=(/-1,-1,-1/), rc=rc) 
    if(rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE

    ! create a Field
    field = ESMF_FieldCreate(grid3d, array3d, rc=rc)
    if(rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE
  
    ! retrieve the Fortran data pointer from the Field
    call ESMF_FieldGet(field, 0, farray1, rc=rc)
    if(rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE

    ! retrieve the Fortran data pointer from the Field
    call ESMF_FieldGet(field, 0, farray1, &
        computationalLBound=compLBnd, computationalUBound=compUBnd, &
        exclusiveLBound=exclLBnd, exclusiveUBound=exclUBnd, &
        totalLBound=totalLBnd, totalUBound=totalUBnd, &
        computationalCount=comp_count, &
        exclusiveCount=excl_count, &
        totalCount=total_count, &
        rc=rc)   

    do k = totalLBnd(3), totalUBnd(3)
        do j = totalLBnd(2), totalUBnd(2)
            do i = totalLBnd(1), totalUBnd(1)
                farray1(i, j, k) = sin(2*i/total_count(1)*PI) + &
                    sin(4*j/total_count(2)*PI) + &
                    sin(8*k/total_count(2)*PI)
            enddo
        enddo
    enddo
!EOC
    if(rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE
    print *, "Field Get Data Pointer example returned"

!>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%
!-------------------------------- Example -----------------------------
!>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%
!BOE
!\subsubsection{Get Grid and Array and other information from a Field}
!\label{sec:field:usage:field_get_default}
!
!  A user can get the internal {\tt ESMF\_Grid} and {\tt ESMF\_Array} 
!  from a {\tt ESMF\_Field}.  Note that the user should not issue any destroy command
!  on the retrieved grid or array object since they are referenced
!  from within the {\tt ESMF\_Field}. The retrieved objects should be used
!  in a read-only fashion to query additional information not directly
!  available through the {\tt ESMF\_FieldGet()} interface.
!
!EOE

!BOC
    call ESMF_FieldGet(field, grid=grid, array=array, &
        typekind=typekind, dimCount=dimCount, staggerloc=staggerloc, &
        gridToFieldMap=gridToFieldMap, &
        ungriddedLBound=ungriddedLBound, ungriddedUBound=ungriddedUBound, &
        maxHaloLWidth=maxHaloLWidth, maxHaloUWidth=maxHaloUWidth, & 
        name=name, &
        rc=rc)
!EOC
    if(rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE
    print *, "Field Get Grid and Array example returned"

    call ESMF_FieldDestroy(field, rc=rc)
    if(rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE
!>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%
!-------------------------------- Example -----------------------------
!>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%
!BOE
!\subsubsection{Create Field with Grid and Arrayspec}
!\label{sec:field:usage:create_grid_arrayspec}
!
!  A user can create an {\tt ESMF\_Field} from an {\tt ESMF\_Grid} and a
!  {\tt ESMF\_Arrayspec} with corresponding rank and type.  
!  This create method associates the two objects.  
! 
!  We first create a Grid with a regular distribution that is
!  10x20 index in 2x2 DEs.  This version of Field create simply
!  associates the data with the Grid.  The data is referenced
!  explicitly on a regular 2x2 uniform grid. 
!  Then we create an ArraySpec.  Finally we create a Field from
!  the Grid, ArraySpec, and a user specified StaggerLoc.
!
!  This example also illustrates a typical use of this Field creation
!  method. By creating a Field from a Grid and an ArraySpec, the
!  user allows the ESMF library to create a internal Array in the Field.
!  Then the user can use {\tt ESMF\_FieldGet()} to retrieve the Fortran
!  data array
!  and necessary bounds information to assign initial values to it.
!EOE

!BOC
    ! create a grid
    grid = ESMF_GridCreateShapeTile(minIndex=(/1,1/), maxIndex=(/10,20/), &
          regDecomp=(/2,2/), name="atmgrid", rc=rc)
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

    ! retrieve distgrid from the Grid
    !call ESMF_GridGet(grid, distgrid=distgrid, rc=rc)
    !if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

    ! setup arrayspec
    call ESMF_ArraySpecSet(arrayspec, 2, ESMF_TYPEKIND_R4, rc)
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

    ! create a Field from the Grid and arrayspec
    field1 = ESMF_FieldCreate(grid, arrayspec, ESMF_INDEX_DELOCAL, &
         staggerloc=ESMF_STAGGERLOC_CENTER, name="pressure", rc=rc)
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

    call ESMF_FieldGet(field1, localDe=0, farray=farray2dd, &
        totalLBound=ftlb, totalUBound=ftub, totalCount=ftc, rc=rc)

    do i = ftlb(1), ftub(1)
        do j = ftlb(2), ftub(2)
            farray2dd(i, j) = sin(i/ftc(1)*PI) * cos(j/ftc(2)*PI) 
        enddo
    enddo

    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE
!EOC

    call ESMF_FieldDestroy(field1, rc=rc)
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOE
!   A user can also create an ArraySpec that has a different rank
!   from the Grid, For example, the following code shows creation of 
!   of 3D Field from a 2D Grid using a 3D ArraySpec.
!
!   This example also demonstrates the technique to create a typical
!   3D data Field that has 2 gridded dimensions and 1 ungridded
!   dimension. 
!
!   First we create a 2D grid with an index space of 180x360 equivalent to
!   180x360 Grid cells (note that for a distributed memory computer, this
!   means each 
!   grid cell will be on a separate PE!). In the FieldCreate call, we use gridToFieldMap
!   to indicate the mapping between Grid dimension and Field dimension.
!   For the ungridded dimension (typically the altitude), we use
!   ungriddedLBound and ungriddedUBound to describe its bounds. Internally
!   the ungridded dimension has a stride of 1, so the number of elements
!   of the ungridded dimension is ungriddedUBound - ungriddedLBound + 1.
!
!   Note that gridToFieldMap in this specific example is (/1,2/) which
!   is the default value
!   so the user can neglect this argument for the FieldCreate call.
!EOE

!BOC
    grid2d = ESMF_GridCreateShapeTile(minIndex=(/1,1/), maxIndex=(/180,360/), &
          regDecomp=(/2,2/), name="atmgrid", rc=rc)
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

    call ESMF_ArraySpecSet(arrayspec, 3, ESMF_TYPEKIND_R4, rc)
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

    field1 = ESMF_FieldCreate(grid2d, arrayspec, ESMF_INDEX_DELOCAL, &
         staggerloc=ESMF_STAGGERLOC_CENTER, &
         gridToFieldMap=(/1,2/), &
         ungriddedLBound=(/1/), ungriddedUBound=(/50/), &
         name="pressure", rc=rc)
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE
!EOC

    print *, "Field creation from Grid and Arrayspec returned"

!>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%
!-------------------------------- Example -----------------------------
!>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%
!BOE
!\subsubsection{Create Field with Grid and Array}
!\label{sec:field:usage:create_grid_array}
!
!  A user can create an {\tt ESMF\_Field} from an {\tt ESMF\_Grid} and a 
!  {\tt ESMF\_Array}. The Grid was created in the previous example.
!  
!  This example creates a 2D {\tt ESMF\_Field} from a 2D {\tt ESMF\_Grid}
!  and a 2D {\tt ESMF\_Array}.
!EOE

!BOC
    ! Get necessary information from the Grid
    call ESMF_GridGet(grid, staggerloc=ESMF_STAGGERLOC_CENTER, &
        computationalEdgeLWidth=compEdgeLWdith, &
        computationalEdgeUWidth=compEdgeUWdith, rc=rc)
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

    call ESMF_GridGet(grid, distgrid=distgrid, rc=rc)
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

    ! Create a 2D ESMF_TYPEKIND_R4 arrayspec
    call ESMF_ArraySpecSet(arrayspec, 2, ESMF_TYPEKIND_R4, rc)
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

    ! Create a ESMF_Array from the arrayspec and distgrid
    array2d = ESMF_ArrayCreate(arrayspec=arrayspec, distgrid=distgrid, staggerLoc=0, &
            computationalEdgeLWidth=compEdgeLWdith, &
            computationalEdgeUWidth=compEdgeUWdith, rc=rc)
    if(rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE

    ! Create a ESMF_Field from the grid and array
    field4 = ESMF_FieldCreate(grid, array2d, rc=rc)
    if(rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE
!EOC

    print *, "Field Create from a Grid and an Array returned"

!>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%
!-------------------------------- Example -----------------------------
!>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%
!BOE
!\subsubsection{Create an empty Field and finish it with FieldSetCommit}
!\label{sec:field:usage:create_empty_setcommit}
!
!  A user can create an empty {\tt ESMF\_Field}.
!  Then the user can finalize the empty {\tt ESMF\_Field} from a {\tt ESMF\_Grid} 
!  and a intrinsic 
!  Fortran data array. This interface is overloaded for typekind and rank
!  of the Fortran data array.
!
!  In this example, both grid and Fortran array pointer are 2 dimensional
!  and each dimension index maps in order, i.e. 1st dimension of grid maps to
!  1st dimension of Fortran array pointer, 2nd dimension of grid maps to 2nd dimension of
!  Fortran array pointer, so on and so forth. 
!
!  In order to create or finish a Field from a Grid and a Fortran array pointer, 
!  certain rules of the Fortran array bounds must be obeyed. We will discuss these
!  rules as we progress in Field creation examples.  We will make
!  frequent reference to the terminologies for bounds and widths in ESMF. 
!  For a better discussion of
!  these terminologies and concepts behind them, 
!  e.g. exclusive, computational, total bounds
!  for the lower and upper corner of data region, etc.., users can refer to 
!  the explanation of these concepts for Grid and Array in their respective sections 
!  in the {\it Reference Manual}, e.g. Section \ref{Array_regions_and_default_bounds} on Array
!  and Section \ref{sec:grid:usage:bounds} on Grid.
!  The examples here are designed to help a user to get up to speed with
!  creating Fields for typical use.
!
!  This example introduces a helper method, part of the {\tt ESMF\_FieldGet}
!  interface that facilitates the computation of Fortran data array bounds
!  and shape to assist {\tt ESMF\_FieldSetCommit} finalizing a Field from a
!  instrinsic Fortran data array and a Grid.
!
!EOE

!BOC
    ! create an empty Field
    field3 = ESMF_FieldCreateEmpty("precip", rc=rc)
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

    ! use FieldGet to retrieve total counts 
    call ESMF_FieldGet(grid2d, localDe=0, staggerloc=ESMF_STAGGERLOC_CENTER, &
        totalCount=ftc, rc=rc)
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

    ! allocate the 2d Fortran array based on retrieved total counts
    allocate(farray2d(ftc(1), ftc(2)))

    ! finalize the Field
    call ESMF_FieldSetCommit(field3, grid2d, farray2d, rc=rc)
!EOC
    print *, "Finish a Field created by ESMF_FieldCreateEmpty returned"

!>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%
!-------------------------------- Example -----------------------------
!>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%
!BOE
!\subsubsection{Create 7D Field with 5D Grid and 2D ungridded bounds
! from Fortran data array}
!\label{sec:field:usage:create_5dgrid_7dptr_2dungridded}
!
! In this example, we will show how to create a 7D Field from a 5D {\tt
! ESMF\_Grid} and 2D ungridded bounds with arbitrary halo widths and 
! gridToFieldMap.
!
! We first create a 5D DistGrid and a 5D Grid based on the DistGrid; then
! {\tt ESMF\_FieldGet} computes the shape of a 7D array in fsize. We can then
! create a 7D Field from the 5D Grid and the 7D Fortran data array with
! other assimilating parameters.
!EOE

!BOC
    ! create a 5d distgrid
    distgrid5d = ESMF_DistGridCreate(minIndex=(/1,1,1,1,1/), maxIndex=(/10,4,10,4,6/), &
        regDecomp=(/2,1,2,1,1/), rc=rc)
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

    ! Create a 5d Grid
    grid5d = ESMF_GridCreate(distgrid=distgrid5d, name="grid", rc=rc)
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

    ! use FieldGet to retrieve total counts 
    call ESMF_FieldGet(grid5d, localDe=0, ungriddedLBound=(/1,2/), &
        ungriddedUBound=(/4,5/), &
        maxHaloLWidth=(/1,1,1,2,2/), maxHaloUWidth=(/1,2,3,4,5/), &
        gridToFieldMap=(/3,2,5,4,1/), &
        totalCount=fsize, &
        rc=rc)
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

    ! allocate the 7d Fortran array based on retrieved total counts
    allocate(farray7d(fsize(1), fsize(2), fsize(3), fsize(4), fsize(5), fsize(6), fsize(7)))

    ! create the Field
    field7d = ESMF_FieldCreate(grid5d, farray7d, ESMF_INDEX_DELOCAL, &
        ungriddedLBound=(/1,2/), ungriddedUBound=(/4,5/), &
        maxHaloLWidth=(/1,1,1,2,2/), maxHaloUWidth=(/1,2,3,4,5/), &
        gridToFieldMap=(/3,2,5,4,1/), &
        rc=rc)
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE
!EOC
!BOE
!  A user can allocate the Fortran array in a different manner using the lower and
!  upper bounds returned from FieldGet through the optional totalLBound and totalUBound
!  arguments. In the following example, we create another 7D Field by retrieving the bounds
!  and allocate the Fortran array with this approach. In this scheme, indexing the
!  Fortran array is sometimes more convenient than using the shape directly.
!EOE
!BOC
    call ESMF_FieldGet(grid5d, localDe=0, ungriddedLBound=(/1,2/), &
        ungriddedUBound=(/4,5/), &
        maxHaloLWidth=(/1,1,1,2,2/), maxHaloUWidth=(/1,2,3,4,5/), &
        gridToFieldMap=(/3,2,5,4,1/), &
        totalLBound=flbound, totalUBound=fubound, &
        rc=rc)
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

    allocate(farray7d2(flbound(1):fubound(1), flbound(2):fubound(2), flbound(3):fubound(3), &
                       flbound(4):fubound(4), flbound(5):fubound(5), flbound(6):fubound(6), &
                       flbound(7):fubound(7)) )

    field7d2 = ESMF_FieldCreate(grid5d, farray7d2, ESMF_INDEX_DELOCAL, &
        ungriddedLBound=(/1,2/), ungriddedUBound=(/4,5/), &
        maxHaloLWidth=(/1,1,1,2,2/), maxHaloUWidth=(/1,2,3,4,5/), &
        gridToFieldMap=(/3,2,5,4,1/), &
        rc=rc)
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE
!EOC

    print *, "Field Create from a Grid and a Fortran data pointer returned"
    call ESMF_FieldDestroy(field7d)
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE
    call ESMF_FieldDestroy(field7d2)
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE
    call ESMF_GridDestroy(grid5d)
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE
    call ESMF_DistGridDestroy(distgrid5d)
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE
    deallocate(farray7d)
    deallocate(farray7d2)

! TODO: remove this subsection
!!>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%
!!-------------------------------- Example -----------------------------
!!>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%
!!BremoveOE
!!\subsubsection{Destroy a Field}
!!\label{sec:field:usage:destroy}
!!
!!  When finished with an {\tt ESMF\_Field}, the destroy method
!!  removes it.  However, the objects inside the {\tt ESMF\_Field}
!!  but created externally should be destroyed separately, 
!!  since objects can be added to
!!  more than one {\tt ESMF\_Field}. For example, the same {\tt ESMF\_Grid}
!!  can be used in multiple {\tt ESMF\_Field}s.
!!  
!!EremoveOE
!!-------------------------------------------------------------------------
!
!!BremoveOC
    call ESMF_FieldDestroy(field1, rc=rc)
!!EremoveOC

    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE
!-------------------------------------------------------------------------
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE
    if(rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE
    call ESMF_GridDestroy(grid3d, rc=rc)
    if(rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE
    call ESMF_ArrayDestroy(array3d, rc=rc)
    if(rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE
    deallocate(farray)

!-------------------------------------------------------------------------
    call ESMF_Finalize(rc=rc)
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE
!-------------------------------------------------------------------------

    if (finalrc.EQ.ESMF_SUCCESS) then
        print *, "PASS: ESMF_FieldEx.F90"
    else
        print *, "FAIL: ESMF_FieldEx.F90"
    end if
end program ESMF_FieldEx
