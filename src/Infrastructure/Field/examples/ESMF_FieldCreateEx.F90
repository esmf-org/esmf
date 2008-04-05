! $Id: ESMF_FieldCreateEx.F90,v 1.70 2008/04/05 03:38:15 cdeluca Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2008, University Corporation for Atmospheric Research,
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

    real(ESMF_KIND_R8), dimension(:,:,:,:,:,:,:), pointer :: farray7d
    type(ESMF_Field)    :: field7d
    type(ESMF_Grid)     :: grid5d
    type(ESMF_DistGrid) :: distgrid5d
    integer             :: fsize(7)

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
! 
!  We first create a Grid with a regular distribution that is
!  10x20 DEs.  This version of field create simply
!  associates the data with the Grid.  The data is referenced
!  explicitly on a regular 2x2 uniform grid. 
!  Then we create an arrayspec. With grid and arrayspec,
!  finally we create a field from the grid, arrayspec, and a
!  user specified staggerloc.
!EOE

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
!   User can also create an ArraySpec that has a different rank
!   from the Grid, For example, the following code shows creation of 
!   of 3D Field from a 2D Grid using 3D ArraySpec. We use the
!   {\tt ESMF\_Grid} grid created from the previous example.
!EOE

!BOC
    call ESMF_ArraySpecSet(arrayspec, 3, ESMF_TYPEKIND_R4, rc)
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

    field1 = ESMF_FieldCreate(grid, arrayspec, &
         staggerloc=ESMF_STAGGERLOC_CENTER, name="pressure", rc=rc)
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE
!EOC

    print *, "Field creation from Grid and Arrayspec returned"

!>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%
!-------------------------------- Example -----------------------------
!>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%
!BremoveOE
!\subsubsection{Use ESMF\_ArrayCreate to reset Field internal Array}
!  It's often necessary to reset the data array contained within a field.
!  The following example demonstrates how to create a compliant {\tt ESMF\_Array}
!  and reset the existing {\tt ESMF\_Array} of a {\tt ESMF\_Field}.
!  User can reset the {\tt ESMF\_Array} inside an existing Field by construct a proper
!  shape {\tt ESMF\_Array}. 
!  arrayspec, distgrid are objects created from previous examples.
!EremoveOE
!-------------------------------------------------------------------------
!   !
!   ! The user can substitute another array created by ArrayCreate in field1.
!   ! This example
!   ! makes it clear that field1's array has a computational region smaller
!   ! than its exclusive region.
!BremoveOC
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

    call ESMF_FieldSet(field1, array2, rc=rc)
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE
!EremoveOC
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
!  Fortran data array. This interface is overloaded for type, kind, rank of
!  of the Fortran data array.
!
!  In this example, both grid and Fortran array pointer are 2 dimensional
!  and each dimension index maps in order, i.e. 1st dimension of grid maps to
!  1st dimension of Fortran array pointer, 2nd dimension of grid maps to 2nd dimension of
!  Fortran array pointer, so on and so forth. 
!
!  In order to create or finalize a Field from a Grid and a Fortran array pointer, 
!  certain rules of the Fortran array bounds must be obeyed. We will discuss these
!  rules as we progress in Field creation examples. In general, we will make
!  frequent references to a few terminologies in the ESMF framework. These terminologies
!  reflects the fact that a {\tt ESMF\_Field} is a higher level data structure consisting of
!  a {\tt ESMF\_Grid} and a {\tt ESMF\_Array}. For a better discussion of
!  these terminologies and concepts behind them, 
!  e.g. exclusive, computational, total bounds
!  for the lower and upper corner of data region, etc.., user can refer to 
!  the explanation of these concepts for Grid and Array in their respective chapters 
!  in the reference manual. The examples here aim to help a user to get up to speed to
!  create Fields for typical use.
!
!  In the following example, each dimension size of the Fortran array must be no greater 
!  than the maximum value of the computational and exclusive bounds of its corresponding 
!  Grid dimension queried from the Grid through {\tt ESMF\_GridGet} public interface.
!
!  Formally let fa\_shape(i) be the shape of i-th dimension of user supplied Fortran array,
!  then rule 1 states:  
!  \begin{verbatim}
! 
!  (1) fa_shape(i) = max(computationalCount(i), exclusiveCount(i))         
!                i = 1...GridDimCount
! 
!  \end{verbatim}
! 
!  fa\_shape(i) defines the shape of i-th dimension of the Fortran array. computationalCount and 
!  exclusiveCount are the number of data elements of i-th dimension in the computational and 
!  exclusive regions queried
!  from {\tt ESMF\_GridGet} interface. {\em Rule 1 assumes that the Grid and the Fortran intrinsic
!  array have same number of dimensions; all Grid dimensions are distributed; and optional arguments
!  of FieldCreate from Fortran array are left unspecified using default setup}. These assumptions 
!  are true for most typical use of FieldCreate from Fortran data array. This is the easiest way
!  to create a Field from a Grid and Fortran intrinsic data array.
!  
!  Fortran array dimension size (formally called shape in most Fortran language books) are equivalent
!  to bounds, counts used in this manual because the following equation holds: 
!  \begin{verbatim}
! 
!  fa_shape(i) = shape(i) = counts(i) = upper_bound(i) - lower_bound(i) + 1
! 
!  \end{verbatim}
!
!  These typically mean the same concept unless specifically explained to mean something else.
!  For example, ESMF uses DimCount very often to mean number of dimensions instead of its meaning
!  implied in the above equation. We'll clarify the meaning of a word when ambiguity could occur.
!  
!  Rule 1 is most useful for a user working with Field creation from a Grid and a Fortran
!  array pointer in most scenarios. It extends to higher dimension count, 3D, 4D, etc...
!  Typically, as the code example demonstrates, a user first creates a Grid (here we
!  reuse the grid created in the previous example), then the user use {\tt ESMF\_GridGet}
!  to retrieve the computational and exclusive counts, next the user calculates the shape
!  of each Fortran array dimension according to rule 1. The Fortran array pointer is allocated
!  and initialized based on the computed shape and a Field can either be created or finalized
!  from an empty field.
!
!  There are important details that can be skipped but good to know for {\tt ESMF\_FieldSetCommit}
!  and {\tt ESMF\_FieldCreate} from a Fortran data array. 1) these methods require {\em each PET contains
!  exactly one DE}. This implies that a code using FieldCreate from a data array or FieldSetCommit must
!  have the same number of DEs and PETs, formally $n_{DE} = n_{PET}$. Violation of this condition
!  will cause run time failures. 2) the bounds and counts retrieved from GridGet are DE specific
!  or equivalently PET specific, which means that {\em the Fortran array shape could be different from one
!  PET to another}. 
!EOE

!BOC
    field3 = ESMF_FieldCreate("precip", rc=rc)
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

    call ESMF_GridGet(grid, localDE=0, staggerloc=ESMF_STAGGERLOC_CENTER, &
        computationalCount=gcc, exclusiveCount=gec, rc=rc)
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

    allocate(farray(max(gec(1), gcc(1)), max(gec(2), gcc(2))) )

    call ESMF_FieldSetCommit(field3, grid, farray, rc=rc)
!EOC
    print *, "Finish a Field created by ESMF_FieldCreate returned"
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%
!-------------------------------- Example -----------------------------
!>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%
!BOE
!\subsubsection{Create 2D Field with 2D Grid and Fortran data pointer}
!  User can create a {\tt ESMF\_Field} directly from a {\tt ESMF\_Grid} and a intrinsic 
!  Fortran data array. This interface is overloaded for type, kind, rank of
!  of the Fortran data array. grid and farray are created in previous examples.
!  
!  In this example, we use the same farray as in previous example created based on
!  rule 1. To reiterate, all the assumptions of rule 1 are met for typical use
!  of FieldCreate from a Grid and a Fortran data array.
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
!  Fortran data array. This interface is overloaded for type, kind, rank of
!  of the Fortran data array.
!  
!  This example demonstrates a typical use of {\tt ESMF\_Field} combining
!  a 2D grid and 3D data pointer. One immediate problem follows: 
!  how does one define the bounds of the ungridded dimension? This is
!  solved by the optional arguments ungriddedLBound and ungriddedUBound
!  of FieldCreate interface. By definition, ungriddedLBound and ungriddedUBound
!  are both 1 dimensional integer Fortran arrays.
!
!  Formally, let fa\_shape(j=1...FieldDimCount-GridDimCount) be the shape of 
!  ungridded dimensions of a Field relative to the Grid used in Field creation,
!  here FieldDimCount is the number of dimensions of the Fortran array, which
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
!  mix. However, it's conceivable that at higher dimension count, gridded and ungridded
!  dimensions can interleave. To aid the computation of ungridded dimension shape
!  we formally introduce the mapping concept.
!
!  Let $map_{A,B}(i=1...n_A) = i_B$, and $i_B \in [\phi, 1...n_B]$. $n_A$ is the number
!  of elements in set A, $n_B$ is the number of elements in set B. $map_{A,B}(i)$ defines
!  a mapping from i-th element of set A to $i_B$-th element in set B. $i_B = \phi$ 
!  indicates there does not exist a mapping from i-th element of set A to set B.
!
!  Suppose we have a mapping from dimension index of ungriddedLBound (or
!  ungriddedUBound) to Fortran array dimension index, called ugb2fa. 
!  By defintion, $n_A$ equals to the dimension count of
!  ungriddedLBound (or ungriddedUBound), $n_B$ equals to the dimension count of
!  the Fortran array. We can now formulate the computation of ungridded
!  dimension shape as rule 2:
!  \begin{verbatim}
! 
!  (2) fa_shape(ugb2fa(j)) = ungriddedUBound(j) - ungriddedLBound(j) + 1 
!                        j = 1..FortranArrayDimCount - GridDimCount 
!  \end{verbatim}
!
!  The mapping can be computed in linear time proportional to the
!  Fortran array dimension count (or rank) using the following algorithm in pseudocode:
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
!  X means ungridded. Then the mapping from ungridded bounds to Fortran array is
!  ugb2fa=(/2, 4/). And the shape of 2nd and 4th dimension of Fortran array should equal to
!  (5, 8).
! 
!  Back to our 3D Field created from a 2D Grid and 3D Fortran array example, suppose the 3rd
!  Field dimension is ungridded, ungriddedLBound=(/3/), ungriddedUBound=(/9/).
!  First we use rule 1 to compute shapes of the gridded Fortran array dimension,
!  then we use rule 2 to compute shapes of the ungridded Fortran array dimension.
!  In this example, we used the computational and exclusive bounds obtained in previous
!  example.
!EOE

!BOC
    do i = 1, 2
        fa_shape(i) = max(gec(i), gcc(i))
    end do
    fa_shape(3) = 7 ! 9-3+1
    allocate(farray3d(fa_shape(1), fa_shape(2), fa_shape(3)))
    field2 = ESMF_FieldCreate(grid, farray3d, &
        ungriddedLBound=(/3/), ungriddedUBound=(/9/), &
        rc=rc)
    if(rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE
!EOC
    print *, "Field Create from a Grid and a Fortran data pointer returned"
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
!  dimension index to Field dimension index.
!
!  In this example, both dimensions of the Grid are distributed and the
!  mapping from DistGrid to Grid is (/1,2/). We will introduce rule 3
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
!  First we use rule 3 to compute shapes of the gridded Fortran array dimension,
!  then we use rule 2 to compute shapes of the ungridded Fortran array dimension.
!  In this example, we used the computational and exclusive bounds obtained in previous
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
    call ESMF_FieldDestroy(field2,rc=rc)
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE
    deallocate(farray3d)

!>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%
!-------------------------------- Example -----------------------------
!>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%
!BOE
!\subsubsection{Create 3D Field with 2D Grid and 3D Fortran data pointer with halos}
!  This example is similar to example 18.2.7, in addition we will show
!  a user can associate different halo width to a Fortran array to create
!  a Field through the maxHaloLWidth and maxHaloUWdith optional arguments.
!  
!  The {\tt ESMF\_FieldCreate} interface supports creating a Field from a Grid and a
!  Fortran array padded with halos on the distributed dimensions of the Fortran
!  array. Using this technique one can avoid passing non-contiguous Fortran array
!  slice to FieldCreate. It gurantees same computational region;
!  and by using halos, it also defines a bigger total region to contain 
!  the entire contiguous memory block of the Fortran array.
!
!  The elements of maxHaloLWidth and maxHaloUWdith are applied in the order
!  distributed dimensions appear in the Fortran array. By definition, 
!  maxHaloLWidth and maxHaloUWdith are 1 dimensional arrays of non-negative 
!  integer values. The size of haloWidth arrays equal to the number of distributed
!  dimensions of the Fortran array, which is also equal to the number of
!  distributed dimensions of the Grid used in the Field creation.
!
!  Because the order of maxHaloWidth (representing both maxHaloLWidth and
!  maxHaloUWdith) element is applied to the order distributed dimensions
!  appear in the Fortran array dimensions, it's quite simple to compute
!  the shape of distributed dimensions of Fortran array. They are done
!  in a similar manner when applying ungriddedLBound and ungriddedUBound 
!  to ungridded dimensions of the Fortran array defined by rule 2.
!
!  Assume we have the mapping from the dimension index of maxHaloWidth
!  to the dimension index of Fortran array, called mhw2fa; and we also
!  have the mapping from dimension index of Fortran array to dimension
!  index of the Grid, called fa2g. The shape of
!  distributed dimensions of a Fortran array can be computed by rule 4: 
! 
!  \begin{verbatim}
!
!  (4) fa_shape(mhw2fa(k)) = max((exclusiveCount(fa2g(mhw2fa(k))), 
!                            computationalCount(fa2g(mhw2fa(k))) +
!                            maxHaloUWidth(k) + maxHaloLWidth(k))
!                        k = 1...size(maxHaloWidth) 
!
!  \end{verbatim}
!  
!  This rule may seem confusing but algorithmically the computation
!  can be done by the following pseudocode:
!
!  \begin{verbatim}
!
!  fa_index = 1
!  do i = 1, farray_rank
!     if i-th dimension of Fortran array is distributed
!         fa_shape(i) = max(exclusiveCount(fa2g(i)), 
!                       computationalCount(fa2g(i)) +
!                       maxHaloUWidth(fa_index) + maxHaloLWidth(fa_index))
!         fa_index = fa_index + 1
!     endif
!  enddo
!
!  \end{verbatim}
!
!  The only complication then is to figure out the mapping from Fortran
!  array dimension index to Grid dimension index. This process can
!  be done by compute reverse mapping from Field to Grid.
!
!  Typically, we don't have to consider these complications if the following
!  conditions are met: 1) all Grid dimensions are distributed; 2) DistGrid
!  in the Grid has a dimension index mapping to the Grid in the form of 
!  natural order (/1,2,3,.../). This natural order mapping is the
!  default mapping between various objects throughout ESMF; 3) Grid to Field
!  mapping is in the form of natural order, i.e. default mapping. These
!  seem like a lot of conditions but they are the default case in the interaction
!  among DistGrid, Grid, and Field. When these conditions are met, which
!  is typically true, the shape of distributed dimensions of Fortran array
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
!  3 dimensions of the Fortran array. maxHaloLWidth=(/1,2,3/), 
!  maxHaloUWdith=(/7,9,10/), then by rule 5, the following pseudo code
!  can be used to compute the shape of the first 3 dimensions of the Fortran
!  array. The shape of the remaining two ungridded dimensions can be
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
!  Suppose now gridToFieldMap=(/2,3,4/) instead which says
!  the first dimension of Grid maps to the 2nd dimension of Field (or 
!  Fortran array) and so on and so forth, we can obtain a more general form 
!  of rule 5 by introducing first\_distdim\_index shift when Grid to Field
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
!  It's obvious that first\_distdim\_index=a. If the first dimension of the Fortran
!  array is distributed, then rule 6 degenerates into rule 5, which is
!  the typical case.
!
!  Back to our example creating a 3D Field from a 2D Grid and a 3D intrinsic
!  Fortran array, we will use the Grid created from previous example
!  that satisfies condition 1 and 2. We'll also use a simple gridToFieldMap
!  (1,2) which is the default mapping that satisfies condition 3. 
!  First we use rule 5 to compute
!  the shape of distributed dimensions then we use rule 2 to compute the shape
!  of the ungridded dimensions.
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
    fa_shape(3) = 7          ! 9-3+1
    allocate(farray3d(fa_shape(1), fa_shape(2), fa_shape(3)))
    field2 = ESMF_FieldCreate(grid, farray3d, &
        ungriddedLBound=(/3/), ungriddedUBound=(/9/), &
        maxHaloLWidth=maxHaloLWidth2d, maxHaloUWidth=maxHaloUWidth2d, &
        gridToFieldMap=gridToFieldMap2d, &
        rc=rc)
    if(rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE
!EOC
    print *, "Field Create from a Grid and a Fortran data pointer returned"
    call ESMF_FieldDestroy(field2,rc=rc)
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE
    deallocate(farray3d)

!>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%
!-------------------------------- Example -----------------------------
!>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%
!BOE
!\subsubsection{Create 7D Field with 5D Grid and 2D ungridded bounds}
! In this example, we will show how to create a 7D Field from a 5D {\tt
! ESMF\_Grid} and 2D ungridded bounds with arbitrary halo widths and 
! gridToFieldMap.
!
! This example introduces a helper method, part of the {\tt ESMF\_FieldGet}
! interface that facilitates the computation of Fortran data array bounds
! and shape to assist {\tt ESMF\_FieldCreate} creating a Field from a
! instrinsic Fortran data array following the rules discussed in previous
! examples.
!
! We first create a 5D DistGrid and a 5D Grid based on the DistGrid; then
! {\tt ESMF\_FieldGet} computes the shape of a 7D array in fsize. We can then
! create a 7D Field from the 5D grid and the 7D Fortran data array with
! other assimilating parameters.
!EOE

!BOC
    distgrid5d = ESMF_DistGridCreate(minIndex=(/1,1,1,1,1/), maxIndex=(/10,4,10,4,6/), &
        regDecomp=(/2,1,2,1,1/), rc=rc)
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

    grid5d = ESMF_GridCreate(distgrid=distgrid5d, name="grid", rc=rc)

    call ESMF_FieldGet(grid5d, localDe=0, ungriddedLBound=(/1,2/), &
        ungriddedUBound=(/4,5/), &
        maxHaloLWidth=(/1,1,1,2,2/), maxHaloUWidth=(/1,2,3,4,5/), &
        gridToFieldMap=(/3,2,5,4,1/), &
        allocCount=fsize, &
        rc=rc)
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

    allocate(farray7d(fsize(1), fsize(2), fsize(3), fsize(4), fsize(5), fsize(6), fsize(7)))

    field7d = ESMF_FieldCreate(grid5d, farray7d, &
        ungriddedLBound=(/1,2/), ungriddedUBound=(/4,5/), &
        maxHaloLWidth=(/1,1,1,2,2/), maxHaloUWidth=(/1,2,3,4,5/), &
        gridToFieldMap=(/3,2,5,4,1/), &
        rc=rc)
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE
!EOC

    print *, "Field Create from a Grid and a Fortran data pointer returned"
    call ESMF_FieldDestroy(field7d)
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE
    call ESMF_GridDestroy(grid5d)
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE
    call ESMF_DistGridDestroy(distgrid5d)
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE
    deallocate(farray7d)

!>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%
!-------------------------------- Example -----------------------------
!>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%
!BOE
!\subsubsection{Destroy a Field}

!  When finished with an {\tt ESMF\_Field}, the destroy method
!  removes it.  However, the objects inside the {\tt ESMF\_Field}
!  but created externally should be deleted separately, 
!  since objects can be added to
!  more than one {\tt ESMF\_Field}, for example the same {\tt ESMF\_Grid}
!  can be used in multiple {\tt ESMF\_Field}s.
!  
!  For example, a single Grid reference
!  can be shared by multiple Fields, the internal Grid is not deleted by
!  this call. Field provides copy behavior through creation and
!  set interface, the internally created {\tt ESMF\_Array} object
!  will be deleted upon Field destruction to prevent memory leak.

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
