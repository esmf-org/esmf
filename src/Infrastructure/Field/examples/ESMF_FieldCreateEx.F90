! $Id: ESMF_FieldCreateEx.F90,v 1.55.2.8 2008/03/20 21:04:41 feiliu Exp $
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
!  In order for a Field to be created/finalized from the Grid and the fortran array pointer, 
!  certain rules of the fortran array bounds must be followed. We will talk through these
!  rules as we progress in Field creation examples. In general, we will make
!  frequent references to a few key terminologies in the ESMF framework. These terminologies
!  reflects the fact that a {\tt ESMF\_Field} is a higher level data structure consisting of
!  a {\tt ESMF\_Grid} and a {\tt ESMF\_Array}. Therefore
!  to better understand these terminologies and concepts behind them, 
!  e.g. exclusive, computational, total bounds
!  for the lower and upper corner of data region, a user is encouraged to refer to 
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
!  intends to use a fortran array pointer instead of explicit-shape array as target of a
!  fortran array pointer.
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
!  Typically, as the code example demonstrates, the user first creates a Grid (here we
!  reuse the grid created in the preceeding example), then the user use {\tt ESMF\_GridGet}
!  to retrieve the computational and exclusive counts, next the user calculates the shape
!  of each fortran array dimension according to rule 1. The fortran array pointer is allocated
!  and initialized based on the computed shape and a Field can either be created or finalized
!  if it was a empty field.
!
!  There are important details that can be skipped but good to know for {\tt ESMF\_FieldSetCommit}
!  and {\tt ESMF\_FieldCreate}. 1) these methods require each PET contains
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
!  of FieldCreate interface. By definition, ungriddedUBound and ungriddedUBound
!  are both 1 dimensional integer fortran arrays.
!
!  Formally, let fa\_shape(j=1...FieldDimCount-GridDimCount) be the size of 
!  ungridded dimension of a Field relative to the Grid used in Field creation,
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
!  of element in set A, $n_B$ is the number of element in set B. $map_{A,B}(i)$ defines
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
! 
!  \end{verbatim}
!
!  The mapping can be computed in linear time proportional to the
!  fortran array dimension count using the following algorithm in pseudocode:
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
!  assuming distgridToGrid=(/1,2,3...gridDimCount/), and distgridDimCount equals
!  to gridDimCount.
!
!  We apply the mapping gridToFieldMap on rule 1 to create rule 3:
!  \begin{verbatim}
! 
!  (3) fa_shape(gridToFieldMap(i)) = max(computationalCount(i), exclusiveCount(i)        
!                                 i = 1,..gridDimCount.
! 
!  \end{verbatim}
!
!  Back to our example, suppose the 2nd
!  Field dimension is ungridded, ungriddedLBound=(/3/), ungriddedUBound=(/9/).
!  gridToFieldMap=(/3,1/), meaning the 1st Grid dimension maps to 3rd Field dimension,
!  and 2nd grid dimension maps to 1st Field dimension.
!
!  First we use rule 3 to compute shapes of the gridded fortran array dimension,
!  then we use rule 2 to compute shapes of the ungridded fortran array dimension.
!  In this example, we used the computational and exclusive bounds obtained in preceeding
!  example.
!EOE
!BOC
    gridToFieldMap2d(1) = 1
    gridToFieldMap2d(2) = 3
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
