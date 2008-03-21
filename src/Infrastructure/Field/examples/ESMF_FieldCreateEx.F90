! $Id: ESMF_FieldCreateEx.F90,v 1.55.2.13 2008/03/21 22:20:23 feiliu Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2007, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
!
    program ESMF_FieldCreateEx

!------------------------------------------------------------------------------
!ESMF_EXAMPLE        String used by test script to count examples.
!==============================================================================
! !PROGRAM: ESMF_FieldCreateEx - Field creation
!
! !DESCRIPTION:
!
! This program shows examples of Field initialization and manipulation
!-----------------------------------------------------------------------------

    ! ESMF Framework module
    use ESMF_Mod
    use ESMF_FieldGetMod
    implicit none
    
    ! Local variables
    integer :: rc, localPet, petCount
    type(ESMF_Grid) :: grid, grid3d
    type(ESMF_DistGrid) :: distgrid
    type(ESMF_ArraySpec) :: arrayspec
    type(ESMF_Array) :: array1, array2
    type(ESMF_DELayout) :: layout
    type(ESMF_VM) :: vm

    type(ESMF_Field) :: field1, field2, field3, field4, field3d
    real (ESMF_KIND_R8), dimension(2) :: origin

    real(ESMF_KIND_R8), dimension(:,:), pointer :: farray
    real(ESMF_KIND_R8), dimension(:,:), pointer :: farray1
    real(ESMF_KIND_R8), dimension(:,:,:), pointer :: farray3d
    integer, dimension(2) :: compEdgeLWdith
    integer, dimension(2) :: compEdgeUWdith
    integer, dimension(ESMF_MAXDIM) :: gcc, gec, fa_shape
    integer, dimension(2) :: gridToFieldMap2d
    integer, dimension(2) :: maxHaloLWidth2d, maxHaloUWidth2d

    integer :: i, j, k

    integer :: finalrc       
!   !Set finalrc to success
    finalrc = ESMF_SUCCESS

    call ESMF_Initialize(vm=vm, rc=rc)
    call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)


!>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%
!-------------------------------- Example -----------------------------
!>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%
!BOE
!\subsubsection{Create Field with Grid and Arrayspec}
!  The user first creates an {\tt ESMF\_Grid} and an
!  {\tt ESMF\_Arrayspec} with corresponding rank and type.  
!  This create associates the two objects.  
!EOE
!-------------------------------------------------------------------------
!   !
!   !  We first create a Grid with a regular distribution that is
!   !  10x20 DEs.  This version of create simply
!   !  associates the data with the Grid.  The data is referenced
!   !  explicitly on a regular 2x2 uniform grid. 
!   !  Then we create an arrayspec. With grid and arrayspec,
!   !  we then create a field.

!BOC
    grid = ESMF_GridCreateShapeTile(minIndex=(/1,1/), maxIndex=(/10,20/), &
          regDecomp=(/2,2/), name="atmgrid", rc=rc)
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

    call ESMF_GridGet(grid, distgrid=distgrid, rc=rc)
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

    call ESMF_ArraySpecSet(arrayspec, 2, ESMF_TYPEKIND_R4, rc)
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

    field1 = ESMF_FieldCreate(grid, arrayspec, &
         staggerloc=ESMF_STAGGERLOC_CENTER, name="pressure", rc=rc)
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE
!EOC

    call ESMF_FieldDestroy(field1, rc=rc)
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOE
!   The user can also create an ArraySpec that has a different rank
!   from the Grid, For example, the following code shows creation of 
!   of 3d Field from a 2d grid by using a 3d ArraySpec. We use the
!   {\tt ESMF\_Grid} grid created from the preceeding example.
!EOE

!BOC
    call ESMF_ArraySpecSet(arrayspec, 3, ESMF_TYPEKIND_R4, rc)
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

    field1 = ESMF_FieldCreate(grid, arrayspec, &
         staggerloc=ESMF_STAGGERLOC_CENTER, name="pressure", rc=rc)
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE
!EOC

    print *, "Field creation from Grid and Arrayspec returned"

    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%
!-------------------------------- Example -----------------------------
!>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%
!BOE
!\subsubsection{Use ESMF\_ArrayCreate to reset Field internal Array}
!  It's often necessary to reset the data array contained within a field.
!  The following examples demonstrate different ways of creating {\tt ESMF\_Array}
!  and reset the existing {\tt ESMF\_Array} of a {\tt ESMF\_Field}.
!  User can reset the {\tt ESMF\_Array} inside an existing Field by construct a proper
!  shape {\tt ESMF\_Array} 
!  arrayspec, distgrid are objects created from previous examples.
!EOE
!-------------------------------------------------------------------------
!   !
!   ! The user can substitute another array created by ArrayCreate in field1.
!   ! This example
!   ! makes it clear that field1's array has a computational region smaller
!   ! than its exclusive region.
!BOC
    call ESMF_GridGet(grid, staggerloc=ESMF_STAGGERLOC_CENTER, &
        computationalEdgeLWidth=compEdgeLWdith, &
        computationalEdgeUWidth=compEdgeUWdith, rc=rc)
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

    call ESMF_ArraySpecSet(arrayspec, 2, ESMF_TYPEKIND_R4, rc)
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

    array2 = ESMF_ArrayCreate(arrayspec=arrayspec, distgrid=distgrid, staggerLoc=0, &
            computationalEdgeLWidth=compEdgeLWdith, &
            computationalEdgeUWidth=compEdgeUWdith, rc=rc)
    if(rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE

    call ESMF_FieldSetArray(field1, array2, rc=rc)
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE
!EOC
    print *, "Field reset internal array through ArrayCreate returned"

    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%
!-------------------------------- Example -----------------------------
!>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%
!BOE
!\subsubsection{Create Field with Grid and Array}
!  User can create a {\tt ESMF\_Field} from a {\tt ESMF\_Grid} and a 
!  {\tt ESMF\_Array}. grid and array2 are objects created in previous examples.
!EOE

!BOC
    field4 = ESMF_FieldCreate(grid, array2, rc=rc)
    if(rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE
!EOC
    print *, "Field Create from a Grid and a ESMF_Array returned"
    if(rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE

!>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%
!-------------------------------- Example -----------------------------
!>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%
!BOE
!\subsubsection{Create Empty Field and Finish an empty Field 
!  with FieldSetCommit}

!  The user creates an empty {\tt ESMF\_Field} object.
!  Then the user can finalize a {\tt ESMF\_Field} from a {\tt ESMF\_Grid} and a intrinsic 
!  Fortran data pointer. This interface is overloaded for type, kind, rank of
!  of the fortran data pointer.
!
!  In this example, both grid and fortran array pointer are 2 dimensional
!  and each dimension index maps in order, i.e. 1st dimension of grid maps to
!  1st dimension of fortran array pointer, 2nd dimension of grid maps to 2nd dimension of
!  fortran array pointer, so on and so forth. 
!
!  In order for a Field to be created/finalized from a Grid and a fortran array pointer, 
!  certain rules of the fortran array bounds must be obeyed. We will talk through these
!  rules as we progress in Field creation examples. In general, we will make
!  frequent references to a few key terminologies in the ESMF framework. These terminologies
!  reflects the fact that a {\tt ESMF\_Field} is a higher level data structure consisting of
!  a {\tt ESMF\_Grid} and a {\tt ESMF\_Array}. For a better discussion of
!  these terminologies and concepts behind them, 
!  e.g. exclusive, computational, total bounds
!  for the lower and upper corner of data region, etc.., user can refer to 
!  the explanation of these concepts for Grid and Array in their respective chapters 
!  in the reference manual. The examples here aim to help a user to get up to speed to
!  create Fields for most use scenarios.
!
!  In the following example, each dimension size of the fortran array must be no greater 
!  than the maximum value of the computational and exclusive bounds of its corresponding 
!  Grid dimension queried from the Grid through {\tt ESMF\_GridGet} public interface.
!
!  Formally let fa\_shape(i=1...N) be the shape of i-th dimension of user supplied fortran array,
!  then rule 1 states:  
!  \begin{verbatim}
! 
!  (1) fa_shape(i) = max(computationalCount(i), exclusiveCount(i))         
! 
!  \end{verbatim}
! 
!  fa\_shape(i) defines the shape of i-th dimension that needs to be allocated if a user
!  intends to use a fortran array pointer instead of explicit-shape array.
!  
!  Fortran array dimension size (formally called shape in most Fortran language books), sometimes
!  this manual also uses the terminology bounds, counts because the following equation holds: 
!  \begin{verbatim}
! 
!  fa_shape(i) = shape(i) = counts(i) = upper_bound(i) - lower_bound(i) + 1
! 
!  \end{verbatim}
!
!  These typically means the same concept unless specifically explained to stand for something else.
!  For example, ESMF uses DimCount very often to mean number of dimensions instead of its meaning
!  implied in the above equation. We'll clarify the meaning of a word when ambiguity could occur.
!  
!  rule 1 is most useful for a user working with Field creation from a Grid and a fortran
!  array pointer in most scenarios. It extends to higher dimension count, 3D, 4D, etc...
!  Typically, as the code example demonstrates, a user first creates a Grid (here we
!  reuse the grid created in the preceeding example), then the user use {\tt ESMF\_GridGet}
!  to retrieve the computational and exclusive counts, next the user calculates the shape
!  of each fortran array dimension according to rule 1. The fortran array pointer is allocated
!  and initialized based on the computed shape and a Field can either be created or finalized
!  from an empty field.
!
!  There are important details that can be skipped but good to know for {\tt ESMF\_FieldSetCommit}
!  and {\tt ESMF\_FieldCreate} from a fortran data pointer. 1) these methods require each PET contains
!  exactly one DE. This implies that a code using FieldCreate from a data pointer or FieldSetCommit must
!  have the same number of DEs and PETs, formally $n_{DE} = n_{PET}$. Violation of this condition
!  generally induces run time failure. 2) the bounds and counts retrieved from GridGet are DE specific
!  or equivalently PET specific, which means the fortran array shape could be different from one
!  PET to another. 
!EOE

!BOC
    field3 = ESMF_FieldCreateEmpty("precip", rc=rc)
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

    call ESMF_GridGet(grid, localDE=0, staggerloc=ESMF_STAGGERLOC_CENTER, &
        computationalCount=gcc, exclusiveCount=gec, rc=rc)
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

    allocate(farray(max(gec(1), gcc(1)), max(gec(2), gcc(2))) )

    call ESMF_FieldSetCommit(field3, grid, farray, rc=rc)
!EOC
    print *, "Finish a Field created by ESMF_FieldCreateEmpty returned"
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%
!-------------------------------- Example -----------------------------
!>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%
!BOE
!\subsubsection{Create 2D Field with 2D Grid and Fortran data pointer}
!  User can create a {\tt ESMF\_Field} directly from a {\tt ESMF\_Grid} and a intrinsic 
!  Fortran data pointer. This interface is overloaded for type, kind, rank of
!  of the fortran data pointer. grid and farray are created in previous examples.
!  
!  In this example, we use the same farray as in previous example created based on
!  rule 1.
!EOE

!BOC
    field2 = ESMF_FieldCreate(grid, farray, rc=rc)
    if(rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE
!EOC
    print *, "Field Create from a Grid and a Fortran data pointer returned"
    if(rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE

!>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%
!-------------------------------- Example -----------------------------
!>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%
!BOE
!\subsubsection{Create 3D Field with 2D Grid and 3D Fortran data pointer}
!  User can create a {\tt ESMF\_Field} from a {\tt ESMF\_Grid} and a intrinsic 
!  Fortran data pointer. This interface is overloaded for type, kind, rank of
!  of the fortran data pointer.
!  
!  This example demonstrates a typical use of {\tt ESMF\_Field} combining
!  a 2D grid and 3D data pointer. One immediate problem follows: 
!  how does one define the bounds of the ungridded dimension? This is
!  achieved by the optional arguments ungriddedLBound and ungriddedUBound
!  of FieldCreate interface. By definition, ungriddedLBound and ungriddedUBound
!  are both 1 dimensional integer fortran arrays.
!
!  Formally, let fa\_shape(j=1...FieldDimCount-GridDimCount) be the shape of 
!  ungridded dimensions of a Field relative to the Grid used in Field creation,
!  here FieldDimCount is the number of dimensions of the fortran array, which
!  equals to the number of dimensions of the result Field. GridDimCount is
!  the number of dimensions of the Grid. 
! 
!  fa\_shape(j) is computed as:
!  \begin{verbatim}
! 
!  fa_shape(j) = ungriddedUBound(j) - ungriddedLBound(j) + 1
! 
!  \end{verbatim}
!  
!  fa\_shape is easy to compute when the gridded and ungridded dimensions do not
!  mix. However, it's conceivable at higher dimension count, gridded and ungridded
!  dimensions can interleave. To aid the computation of ungridded dimension shape
!  we formally introduce the mapping concept.
!
!  Let $map_{A,B}(i=1...n_A) = i_B$, and $i_B \in [1, n_B]$. $n_A$ is the number
!  of elements in set A, $n_B$ is the number of elements in set B. $map_{A,B}(i)$ defines
!  a mapping between i-th element of set A to $i_B$-th element in set B.
!
!  Suppose we have a mapping between dimension index of ungriddedLBound (or
!  ungriddedUBound) and fortran array dimension index, called ugb2fa. 
!  By defintion, $n_A$ equals to the dimension count of
!  ungriddedLBound (or ungriddedUBound), $n_B$ equals to the dimension count of
!  the fortran array. We can now formulate the computation of ungridded
!  dimension shape as rule 2:
!  \begin{verbatim}
! 
!  (2) fa_shape(ugb2fa(j)) = ungriddedUBound(j) - ungriddedLBound(j) + 1 
!                        j = 1..FortranArrayDimCount - GridDimCount 
!  \end{verbatim}
!
!  The mapping can be computed in linear time proportional to the
!  fortran array dimension count (rank) using the following algorithm in pseudocode:
!  \begin{verbatim}
!
!  map_index = 1
!  do i = 1, farray_rank
!      if i-th dimension of farray is ungridded
!          ugb2fa(map_index) = i
!          map_index = map_index + 1
!      endif
!  enddo
! 
!  \end{verbatim}
!
!  Here we use rank and dimension count interchangably. These 2 terminologies are typically
!  equivalent. But there are subtle differences
!  under certain conditions. Rank is the total number of dimensions of a tensor object.
!  Dimension count allows a finer description of the heterogeneous dimensions in that object.
!  For example, A Field of rank 5 can have 3 gridded dimensions and 2 ungridded dimensions.
!  Rank is precisely the summation of dimension count of all exclusive types of dimensions 
!  of a tensor object. 
! 
!  For example, if a 5D array is used with 3D Grid, there are 2 ungridded dimensions.
!  And ungriddedLBound=(/1,2/), ungriddedUBound=(/5,7/).
!  Suppose, the distribution of dimensions look like (O, X, O, X, O), O means gridded,
!  X means ungridded. Then the mapping from ungridded bounds to fortran array is
!  ugb2fa=(/2, 4/). And the shape of 2nd and 4th dimension of fortran array should equal to
!  (5, 8).
! 
!  Back to our 3D Field created from a 2D Grid and 3D fortran array example, suppose the 3rd
!  Field dimension is ungridded, ungriddedLBound=(/3/), ungriddedUBound=(/9/).
!  First we use rule 1 to compute shapes of the gridded fortran array dimension,
!  then we use rule 2 to compute shapes of the ungridded fortran array dimension.
!  In this example, we used the computational and exclusive bounds obtained in preceeding
!  example.
!EOE

!BOC
    do i = 1, 2
        fa_shape(i) = max(gec(i), gcc(i))
    end do
    fa_shape(3) = 7
    allocate(farray3d(fa_shape(1), fa_shape(2), fa_shape(3)))
    field2 = ESMF_FieldCreate(grid, farray3d, &
        ungriddedLBound=(/3/), ungriddedUBound=(/9/), &
        rc=rc)
    if(rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE
!EOC
    print *, "Field Create from a Grid and a Fortran data pointer returned"
    if(rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE
    call ESMF_FieldDestroy(field2,rc=rc)
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE
    deallocate(farray3d)

!>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%
!-------------------------------- Example -----------------------------
!>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%
!BOE
!\subsubsection{Create 3D Field with 2D Grid and 3D Fortran data pointer with gridToFieldMap}
!  Building upon the previous example, we will create a 3D Field from
!  2D grid and 3D array but with a slight twist. In this example, we
!  introduce the gridToFieldMap argument that allows a user to map Grid 
!  dimension index to Field dimension index, captured in the mapping
!  gridToFieldMap.
!
!  In this example, both dimensions of the Grid are distributed and the
!  mapping between DistGrid and Grid is (/1,2/). We will introduce rule 3
!  assuming distgridToGridMap=(/1,2,3...gridDimCount/), and distgridDimCount equals
!  to gridDimCount. This is a reasonable assumption in typical Field use.
!
!  We apply the mapping gridToFieldMap on rule 1 to create rule 3:
!  \begin{verbatim}
! 
!  (3) fa_shape(gridToFieldMap(i)) = max(computationalCount(i), exclusiveCount(i)        
!                                i = 1,..GridDimCount.
! 
!  \end{verbatim}
!
!  Back to our example, suppose the 2nd
!  Field dimension is ungridded, ungriddedLBound=(/3/), ungriddedUBound=(/9/).
!  gridToFieldMap=(/3,1/), meaning the 1st Grid dimension maps to 3rd Field dimension,
!  and 2nd Grid dimension maps to 1st Field dimension.
!
!  First we use rule 3 to compute shapes of the gridded fortran array dimension,
!  then we use rule 2 to compute shapes of the ungridded fortran array dimension.
!  In this example, we used the computational and exclusive bounds obtained in preceeding
!  example.
!EOE
!BOC
    gridToFieldMap2d(1) = 3
    gridToFieldMap2d(2) = 1
    do i = 1, 2
        fa_shape(gridToFieldMap2d(i)) = max(gec(i), gcc(i))
    end do
    fa_shape(2) = 7
    allocate(farray3d(fa_shape(1), fa_shape(2), fa_shape(3)))
    field2 = ESMF_FieldCreate(grid, farray3d, &
        ungriddedLBound=(/3/), ungriddedUBound=(/9/), &
        gridToFieldMap=gridToFieldMap2d, &
        rc=rc)
    if(rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE
!EOC
    print *, "Field Create from a Grid and a Fortran data pointer returned"
    if(rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE
    call ESMF_FieldDestroy(field2,rc=rc)
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE
    deallocate(farray3d)

!>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%
!-------------------------------- Example -----------------------------
!>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%
!BOE
!\subsubsection{Create 3D Field with 2D Grid and 3D Fortran data pointer with halos}
!  This example is similar to example 18.2.7, in addition we will show
!  a user can associate different halo width to a fortran array to create
!  a Field through the maxHaloLWidth and maxHaloUWdith optional arguments.
!  
!  The {\tt ESMF\_FieldCreate} interface supports creating a Field from a Grid and a
!  fortran array padded with halos on the distributed dimensions of the fortran
!  array. Using this technique one can avoid passing non-contiguous fortran array
!  slice to FieldCreate. It gurantees same computational region;
!  and by using halos, it also defines a bigger total region to contain 
!  the entire contiguous memory block of the fortran array.
!
!  The elements of maxHaloLWidth and maxHaloUWdith are applied in the order
!  distributed dimensions appear in the fortran array. By definition, 
!  maxHaloLWidth and maxHaloUWdith are 1 dimensional arrays of non-negative 
!  integer values. The size of haloWidth arrays equal to the number of distributed
!  dimensions of the fortran array, which is also equal to the number of
!  distributed dimensions of the Grid used in the Field creation.
!
!  Because the order of maxHaloWidth (representing both maxHaloLWidth and
!  maxHaloUWdith) element is applied to the order distributed dimensions
!  appear in the fortran array dimensions, it's quite simple to compute
!  the shape of distributed dimensions of fortran array. They are done
!  in the similar manner when applying ungriddedLBound and ungriddedUBound 
!  to ungridded dimensions of the fortran array defined by rule 2.
!
!  Assume we have the mapping between the dimension index of maxHaloWidth
!  and the dimension index of fortran array, called mhw2fa; and we also
!  have a mapping between dimension index of fortran array and dimension
!  index of the DistGrid contained in the Grid, called fa2dg. The shape of
!  distributed dimensions of a fortran array can be computed by rule 4: 
! 
!  \begin{verbatim}
!
!  (4) fa_shape(mhw2fa(k)) = max((exclusiveCount(fa2dg(mhw2fa(k))), 
!                            computationalCount(fa2dg(mhw2fa(k))) +
!                            maxHaloUWidth(k) + maxHaloLWidth(k))
!                        k = 1...size(maxHaloWidth) 
!
!  \end{verbatim}
!  
!  This may seem a little daunting but algorithmically the computation
!  can be done a lot easily shown by the following pseudocode:
!
!  \begin{verbatim}
!
!  fa_index = 1
!  do i = 1, farray_rank
!     if i-th index of fortran array is distributed
!         fa_shape(i) = max(exclusiveCount(fa2dg(i)), 
!                       computationalCount(fa2dg(i)) +
!                       maxHaloUWidth(fa_index) + maxHaloLWidth(fa_index))
!         fa_index = fa_index + 1
!     endif
!  enddo
!
!  \end{verbatim}
!
!  The only complication then is to figure out the mapping from fortran
!  array dimension index to DistGrid dimension index. This process can
!  be done by first compute reverse mapping from  Field to Grid, and from
!  Grid to DistGrid, then by chaining the reverse mappings, one can obtain
!  the mapping from fortran array to DistGrid.
!
!  Typically, we don't have to consider these complications if the following
!  conditions are met: 1) all Grid dimensions are distributed; 2) DistGrid
!  in the Grid has a dimension index mapping to the Grid in the form of 
!  natural order (/1,2,3,.../). This natural order mapping is the
!  default mapping between various objects throughout ESMF; 3) Grid to Field
!  mapping is in the form of natural order, i.e. default mapping. These
!  seem like a lot of conditions but they are the default case in the interaction
!  between DistGrid, Grid, and Field. When these conditions are met, which
!  is typically true, the shape of distributed dimensions of fortran array
!  follows rule 5 in a simple form:
!
!  \begin{verbatim}
!
!  (5) fa_shape(k) = max(exclusiveCount(k), computationalCount(k) + 
!                    maxHaloUWidth(k) + maxHaloLWidth(k)) 
!                k = 1...size(maxHaloWidth)
!
!  \end{verbatim}
!
!  Let's examine an example on how to apply rule 5. Suppose we have a
!  5D array and a 3D Grid that has its first 3 dimensions mapped to the first
!  3 dimensions of the fortran array. maxHaloLWidth=(/1,2,3/), 
!  maxHaloUWdith=(/7,9,10/), then by rule 5, the following pseudo code
!  can be used to compute the shape of the first 3 dimensions of the fortran
!  array. The shape of the remaining two ungridded dimensions are to be
!  computed according to rule 2.
!
!  \begin{verbatim}
!
!  do k = 1, 3
!      fa_shape(k) = max(exclusiveCount(k), computationalCount(k) + 
!                    maxHaloUWidth(k) + maxHaloLWidth(k)) 
!  enddo
!
!  \end{verbatim}
!
!  Suppose now the gridToFieldMap=(/2,3,4/) instead which says
!  the first dimension of Grid maps to the 2nd dimension of Field (or 
!  fortran array) and so on and so forth, we can obtain a more general form 
!  of rule 5 by introducing first_distdim_index shift when Grid to Field
!  map (gridToFieldMap) is in the form of (/a,a+1,a+2.../).
!
!  \begin{verbatim}
!
!  (6) fa_shape(k+first_distdim_index-1) = max(exclusiveCount(k),
!                 computationalCount(k)) +  maxHaloUWidth(k) + maxHaloLWidth(k) )
!                                      k = 1...size(maxHaloWidth)
!
!  \end{verbatim}
!
!  It's instictive that first_distdim_index=a. If the first dimension of the fortran
!  array is distributed, then rule 6 degenerates into rule 5, which is
!  the typical case.
!
!  Back to our example creating a 3D Field from a 2D Grid and a 3D intrinsic
!  fortran array, we will use the Grid created from preceeding example
!  that satisfies condition 1 and 2. We'll also use a simple gridToFieldMap
!  (1,2) which is the default mapping that satisfies condtion 3. 
!  First we use rule 5 to compute
!  the shape of distributed dimensions then we use rule 2 to compute the shape
!  of the ungridded dimensions, in this example 7=9-3+1.
!EOE  

!BOC
    gridToFieldMap2d(1) = 1
    gridToFieldMap2d(2) = 2
    maxHaloLWidth2d(1) = 3
    maxHaloLWidth2d(2) = 4
    maxHaloUWidth2d(1) = 3
    maxHaloUWidth2d(2) = 5
    do k = 1, 2
        fa_shape(k) = max(gec(k), gcc(k)+maxHaloLWidth2d(k)+maxHaloUWidth2d(k) )
    end do
    fa_shape(3) = 7
    allocate(farray3d(fa_shape(1), fa_shape(2), fa_shape(3)))
    field2 = ESMF_FieldCreate(grid, farray3d, &
        ungriddedLBound=(/3/), ungriddedUBound=(/9/), &
        maxHaloLWidth=maxHaloLWidth2d, maxHaloUWidth=maxHaloUWidth2d, &
        gridToFieldMap=gridToFieldMap2d, &
        rc=rc)
    if(rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE
!EOC
    print *, "Field Create from a Grid and a Fortran data pointer returned"
    if(rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE
    call ESMF_FieldDestroy(field2,rc=rc)
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE
    deallocate(farray3d)

!>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%
!-------------------------------- Example -----------------------------
!>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%
!BOE
!\subsubsection{Destroy a Field}

!  When finished with an {\tt ESMF\_Field}, the destroy method
!  removes it.  However, the objects inside the {\tt ESMF\_Field}
!  that has external reference should be deleted separately, 
!  since objects can be added to
!  more than one {\tt ESMF\_Field}, for example the same {\tt ESMF\_Grid}
!  can be used in multiple {\tt ESMF\_Field}s.
!  
!  For example, a single Grid reference
!  can be shared by multiple Fields, the internal Grid is not deleted by
!  this call. Field provides copy behavior through creation and
!  set interface, the internally created {\tt ESMF\_Array} object
!  will be deleted upon Field destruction.

!EOE
!-------------------------------------------------------------------------

!BOC
    call ESMF_FieldDestroy(field1, rc=rc)
!EOC

    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

! Destroy objects
    call ESMF_FieldDestroy(field4,rc=rc)
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE
    call ESMF_FieldDestroy(field3,rc=rc)
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE
    call ESMF_ArrayDestroy(array2, rc=rc)
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE
    call ESMF_GridDestroy(grid, rc=rc)
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE
    deallocate(farray)

!-------------------------------------------------------------------------
     call ESMF_Finalize(rc=rc)
!-------------------------------------------------------------------------

    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

    if (finalrc.EQ.ESMF_SUCCESS) then
	print *, "PASS: ESMF_FieldCreateEx.F90"
    else
	print *, "FAIL: ESMF_FieldCreateEx.F90"
    end if
end program ESMF_FieldCreateEx
