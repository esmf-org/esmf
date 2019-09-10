! $Id$
!
! Example/test code which creates a new bundle.

!-------------------------------------------------------------------------
!ESMF_EXAMPLE        String used by test script to count examples.
!-------------------------------------------------------------------------


program ESMF_FieldBundleCreateEx
#include "ESMF.h"

    ! ESMF Framework module
    use ESMF
    use ESMF_TestMod

    implicit none
    
!   ! Local variables
    type(ESMF_VM)   :: vm
    integer :: i, rc, fieldcount, petCount, localPet
    type(ESMF_Grid) :: grid
    type(ESMF_ArraySpec) :: arrayspec
    character (len = ESMF_MAXSTR) :: bname1, fname1, fname2
    type(ESMF_Field) :: field(10), returnedfield1, returnedfield2, r_fields(3)
    type(ESMF_Field) :: simplefield
    type(ESMF_FieldBundle) :: bundle1, bundle2, bundle3
    type(ESMF_Grid)             :: gridxy
    type(ESMF_FieldBundle)      :: packedFB
    real(ESMF_KIND_R8), pointer :: packedPtr(:,:,:,:)
    real(ESMF_KIND_R8), pointer :: packedPtr3D(:,:,:)
    integer                     :: fieldDim
    character(len = ESMF_MAXSTR), dimension(10) :: fieldNameList
    type(ESMF_Mesh) :: meshEx
    integer, pointer :: nodeIds(:),nodeOwners(:)
    real(ESMF_KIND_R8), pointer :: nodeCoords(:)
    integer, pointer :: elemIds(:),elemTypes(:),elemConn(:)
    integer :: numNodes, numElems

    integer :: finalrc, result

    character(ESMF_MAXSTR) :: testname
    character(ESMF_MAXSTR) :: failMsg

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

    write(failMsg, *) "Example failure"
    write(testname, *) "Example ESMF_FieldBundleCreateEx"


! ------------------------------------------------------------------------------
! ------------------------------------------------------------------------------

    finalrc = ESMF_SUCCESS
        
!-------------------------------------------------------------------------
    ! Initialize framework
    call ESMF_Initialize(defaultlogfilename="FieldBundleCreateEx.Log", &
                    logkindflag=ESMF_LOGKIND_MULTI, rc=rc)
    
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_VMGetCurrent(vm=vm, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_VMGet(vm, petCount=petCount, localPet=localPet, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!------------------------------------------------------------------------------
!BOE
! \subsubsection{Creating a FieldBundle from a list of Fields}
! \label{sec:fieldbundle:usage:create_list}
! A user can create a FieldBundle from a predefined list of Fields. In the following
! example, we first create an {\tt ESMF\_Grid}, then build 3 different {\tt ESMF\_Field}s with 
! different names. The {\tt ESMF\_FieldBundle} is created from the list of 3 Fields.
!
!EOE


!BOC
!-------------------------------------------------------------------------
!   !  Create several Fields and add them to a new FieldBundle.
 
    grid = ESMF_GridCreateNoPeriDim(minIndex=(/1,1/), maxIndex=(/100,200/), &
                                  regDecomp=(/2,2/), name="atmgrid", rc=rc)
!EOC
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOC

    call ESMF_ArraySpecSet(arrayspec, 2, ESMF_TYPEKIND_R8, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    field(1) = ESMF_FieldCreate(grid, arrayspec, &
                                staggerloc=ESMF_STAGGERLOC_CENTER, &
                                name="temperature", rc=rc)
!EOC
    
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOC
    field(2) = ESMF_FieldCreate(grid, arrayspec, &
                                staggerloc=ESMF_STAGGERLOC_CENTER, &
                                name="pressure", rc=rc)
!EOC

    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOC
    field(3) = ESMF_FieldCreate(grid, arrayspec, &
                                staggerloc=ESMF_STAGGERLOC_CENTER, &
                                name="heat flux", rc=rc)
!EOC

    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOC
    bundle1 = ESMF_FieldBundleCreate(fieldList=field(1:3), &
                                name="atmosphere data", rc=rc)

    print *, "FieldBundle example 1 returned"
!EOC

    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!-------------------------------------------------------------------------
!BOE
! \subsubsection{Creating an empty FieldBundle then add one Field to it}
! \label{sec:fieldbundle:usage:create_empty}
! A user can create an empty FieldBundle then add Fields to the empty FieldBundle.
! In the following example, we use the previously defined {\tt ESMF\_Grid}
! to build an {\tt ESMF\_Field}.
! An empty {\tt ESMF\_FieldBundle} is created, then the Field is added
! to the FieldBundle.
!EOE
!BOC
!-------------------------------------------------------------------------
!   !  Create an empty FieldBundle and then add a single field to it.


    simplefield = ESMF_FieldCreate(grid, arrayspec, &
                  staggerloc=ESMF_STAGGERLOC_CENTER, name="rh", rc=rc)
!EOC

    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOC
    bundle2 = ESMF_FieldBundleCreate(name="time step 1", rc=rc)
!EOC

    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    
!BOC
    call ESMF_FieldBundleAdd(bundle2, (/simplefield/), rc=rc)
!EOC

    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOC
    call ESMF_FieldBundleGet(bundle2, fieldCount=fieldcount, rc=rc)

    print *, "FieldBundle example 2 returned, fieldcount =", fieldcount
!EOC

    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)


!-------------------------------------------------------------------------
!BOE
! \subsubsection{Creating an empty FieldBundle then add a list of Fields to it}
! \label{sec:fieldbundle:usage:create_emptylist}
! A user can create an empty FieldBundle then add multiple 
! Fields to the empty FieldBundle.
! In the following example, we use the previously defined {\tt ESMF\_Grid}
! and {\tt ESMF\_Field}s.
! An empty {\tt ESMF\_FieldBundle} is created, then three Fields are added
! to the FieldBundle.
!EOE
!BOC
!-------------------------------------------------------------------------
!   !  Create an empty FieldBundle and then add multiple fields to it.

    bundle3 = ESMF_FieldBundleCreate(name="southern hemisphere", rc=rc)
!EOC

    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOC
    call ESMF_FieldBundleAdd(bundle3, field(1:3), rc=rc)
!EOC

    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOC
    call ESMF_FieldBundleGet(bundle3, fieldCount=fieldcount, rc=rc)

    print *, "FieldBundle example 3 returned, fieldcount =", fieldcount
!EOC

    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!-------------------------------------------------------------------------
!BOE
! \subsubsection{Query a Field stored in the FieldBundle by name or index}
! \label{sec:fieldbundle:usage:get}
! Users can query a Field stored in a FieldBundle by the Field's name or index.
! In the following example, the pressure Field stored in FieldBundle
! is queried by its name then by its index through {\tt ESMF\_FieldBundleGet()}
! method.
!EOE
!BOC
!-------------------------------------------------------------------------
!   !  Get a Field back from a FieldBundle, first by name and then by index.
!   !  Also get the FieldBundle name.

    call ESMF_FieldBundleGet(bundle1, "pressure", field=returnedfield1, rc=rc)
!EOC

    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOC
    call ESMF_FieldGet(returnedfield1, name=fname1, rc=rc)
!EOC

    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOC
    call ESMF_FieldBundleGet(bundle1, 2, returnedfield2, rc=rc)
!EOC

    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOC
    call ESMF_FieldGet(returnedfield2, name=fname2, rc=rc)
!EOC

    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOC
    call ESMF_FieldBundleGet(bundle1, name=bname1, rc=rc)

    print *, "FieldBundle example 4 returned, field names = ", &
                   trim(fname1), ", ", trim(fname2)
    print *, "FieldBundle name = ", trim(bname1)
!EOC
 
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!-------------------------------------------------------------------------
!BOE
! \subsubsection{Query FieldBundle for Fields list either alphabetical or in order of addition}
! \label{sec:fieldbundle:usage:getlist}
! Users can query the list of Fields stored in a FieldBundle.
! By default the returned list of Fields are ordered alphabetically by
! the Field names. Users can also retrieve the list of Fields in the order by which
! the Fields were added to the FieldBundle.
!EOE

!BOC
    call ESMF_FieldBundleGet(bundle1, fieldList=r_fields, rc=rc)
!EOC
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOC
    do i = 1, 3
      call ESMF_FieldGet(r_fields(i), name=fname1, rc=rc)
!EOC
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
      print *, fname1
    enddo
!EOC

!BOC
    call ESMF_FieldBundleGet(bundle1, fieldList=r_fields, &
      itemorderflag=ESMF_ITEMORDER_ADDORDER, rc=rc)
!EOC
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOC
    do i = 1, 3
      call ESMF_FieldGet(r_fields(i), name=fname1, rc=rc)
!EOC
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
      print *, fname1
    enddo
!EOC

  if(petCount == 4) then
!-------------------------------------------------------------------------
!BOE
! \subsubsection{Create a packed FieldBundle on a Grid}
! \label{sec:fieldbundle:usage:packedFBGrid}
! Create a packed fieldbundle from user supplied 
! field names and a packed Fortran array pointer that contains
! the data of the packed fields on a Grid. 
!EOE
  do i = 1, 10
  write(fieldNameList(i), '(A,I2)') 'field', i
  enddo
!BOE
! Create a 2D grid of 4x1 regular decomposition on 4 PETs, each PET has 10x50 elements.
! The index space of the entire Grid is 40x50.
!EOE
!BOC
  gridxy = ESMF_GridCreateNoPeriDim(maxIndex=(/40,50/), regDecomp=(/4,1/), rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOE
! Allocate a packed Fortran array pointer containing 10 packed fields, each field has
! 3 time slices and uses the 2D grid created. Note that gridToFieldMap uses the position
! of the grid dimension as elements, 3rd element of the packedPtr is 10, 4th element
! of the packedPtr is 50.
!EOE
!BOC
  allocate(packedPtr(10, 3, 10, 50)) ! fieldDim, time, y, x
  fieldDim = 1
  packedFB = ESMF_FieldBundleCreate(fieldNameList, packedPtr, gridxy, fieldDim, &
  gridToFieldMap=(/3,4/), staggerloc=ESMF_Staggerloc_Center, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  deallocate(packedPtr)
  call ESMF_FieldBundleDestroy(packedFB, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!-------------------------------------------------------------------------
!BOE
! \subsubsection{Create a packed FieldBundle on a Mesh}
! \label{sec:fieldbundle:usage:packedFBMesh}
! Similarly we could create a packed fieldbundle from user supplied 
! field names and a packed Fortran array pointer that contains
! the data of the packed fields on a Mesh. 
! 
! Due to the verbosity of the MeshCreate process, the code for MeshCreate is
! not shown below, user can either refer to the MeshCreate section
! \ref{sec:mesh:usage:meshCreation}
! or examine the FieldBundleCreate example source code contained
! in the ESMF source distribution directly.
! A ESMF Mesh on 4 PETs with one mesh element on each PET is created.
!EOE
     ! Setup mesh data depending on PET
     if (localPet .eq. 0) then
        ! Fill in node data
        numNodes=4

       !! node ids
       allocate(nodeIds(numNodes))
       nodeIds=(/1,2,4,5/) 

       !! node Coords
       allocate(nodeCoords(numNodes*2))
       nodeCoords=(/0.0,0.0, &
                    1.0,0.0, &
                    0.0,1.0, &
                    1.0,1.0/)

       !! node owners
       allocate(nodeOwners(numNodes))
       nodeOwners=(/0,0,0,0/) ! everything on proc 0

       ! Fill in elem data
       numElems=1

       !! elem ids
       allocate(elemIds(numElems))
       elemIds=(/1/) 

       !! elem type
       allocate(elemTypes(numElems))
       elemTypes=ESMF_MESHELEMTYPE_QUAD

       !! elem conn
       allocate(elemConn(numElems*4))
       elemConn=(/1,2,4,3/)
     else if (localPet .eq. 1) then
        ! Fill in node data
        numNodes=4

       !! node ids
       allocate(nodeIds(numNodes))
       nodeIds=(/2,3,5,6/) 

       !! node Coords
       allocate(nodeCoords(numNodes*2))
       nodeCoords=(/1.0,0.0, &
                    2.0,0.0, &
                    1.0,1.0, &
                    2.0,1.0/)

       !! node owners
       allocate(nodeOwners(numNodes))
       nodeOwners=(/0,1,0,1/) 

       ! Fill in elem data
       numElems=1

       !! elem ids
       allocate(elemIds(numElems))
       elemIds=(/2/) 

       !! elem type
       allocate(elemTypes(numElems))
       elemTypes=ESMF_MESHELEMTYPE_QUAD

       !! elem conn
       allocate(elemConn(numElems*4))
       elemConn=(/1,2,4,3/)
     else if (localPet .eq. 2) then
        ! Fill in node data
        numNodes=4

       !! node ids
       allocate(nodeIds(numNodes))
       nodeIds=(/4,5,7,8/) 

       !! node Coords
       allocate(nodeCoords(numNodes*2))
       nodeCoords=(/0.0,1.0, &
                    1.0,1.0, &
                    0.0,2.0, &
                    1.0,2.0/)

       !! node owners
       allocate(nodeOwners(numNodes))
       nodeOwners=(/0,0,2,2/) 

       ! Fill in elem data
       numElems=1

       !! elem ids
       allocate(elemIds(numElems))
       elemIds=(/3/) 

       !! elem type
       allocate(elemTypes(numElems))
       elemTypes=ESMF_MESHELEMTYPE_QUAD

       !! elem conn
       allocate(elemConn(numElems*4))
       elemConn=(/1,2,4,3/)  
     else 
        ! Fill in node data
        numNodes=4

       !! node ids
       allocate(nodeIds(numNodes))
       nodeIds=(/5,6,8,9/) 

       !! node Coords
       allocate(nodeCoords(numNodes*2))
       nodeCoords=(/1.0,1.0, &
                    2.0,1.0, &
                    1.0,2.0, &
                    2.0,2.0/)

       !! node owners
       allocate(nodeOwners(numNodes))
       nodeOwners=(/0,1,2,3/) 

       ! Fill in elem data
       numElems=1

       !! elem ids
       allocate(elemIds(numElems))
       elemIds=(/4/) 

       !! elem type
       allocate(elemTypes(numElems))
       elemTypes=ESMF_MESHELEMTYPE_QUAD

       !! elem conn
       allocate(elemConn(numElems*4))
       elemConn=(/1,2,4,3/)  
     endif

    ! Create Mesh structure in 1 step
    meshEx=ESMF_MeshCreate(parametricDim=2,spatialDim=2, &
         nodeIds=nodeIds, nodeCoords=nodeCoords, &
         nodeOwners=nodeOwners, elementIds=elemIds,&
         elementTypes=elemTypes, elementConn=elemConn, &
         rc=rc)
    if (rc.ne. ESMF_SUCCESS) rc=ESMF_FAILURE


    ! deallocate node data
    deallocate(nodeIds)
    deallocate(nodeCoords)
    deallocate(nodeOwners)

    ! deallocate elem data
    deallocate(elemIds)
    deallocate(elemTypes)
    deallocate(elemConn)
!BOE
! Allocate the packed Fortran array pointer, the first dimension
! is fieldDim; second dimension is the data associated with mesh element,
! since there is only one mesh element on each processor in this example,
! the allocation is 1; last dimension is the time dimension which contains
! 3 time slices.
!EOE
!BOC
      allocate(packedPtr3D(10, 1, 3))
      fieldDim = 1
      packedFB = ESMF_FieldBundleCreate(fieldNameList, packedPtr3D, meshEx, fieldDim, &
        gridToFieldMap=(/2/), meshloc=ESMF_MESHLOC_ELEMENT, rc=rc)
!EOC
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
      call ESMF_FieldBundleDestroy(packedFB, rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
      deallocate(packedPtr3D)
      call ESMF_MeshDestroy(meshEx, rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOE
!\subsubsection{Destroy a FieldBundle}
!
!The user must call {\tt ESMF\_FieldBundleDestroy()} before 
!deleting any of the Fields it contains.  Because Fields
!can be shared by multiple FieldBundles and States, they are
!not deleted by this call.
!EOE
  endif ! petCount = 4


!BOC
!-------------------------------------------------------------------------

     call ESMF_FieldBundleDestroy(bundle1, rc=rc)
!EOC

    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

     call ESMF_FieldBundleDestroy(bundle2, rc=rc)

    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

     call ESMF_FieldBundleDestroy(bundle3, rc=rc)

    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

     do i=1, 3
         call ESMF_FieldDestroy(field(i),rc=rc)

         if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

     enddo

     call ESMF_FieldDestroy(simplefield, rc=rc)

    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_GridDestroy(grid, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    ! IMPORTANT: ESMF_STest() prints the PASS string and the # of processors in the log
    ! file that the scripts grep for.
    call ESMF_STest((finalrc.eq.ESMF_SUCCESS), testname, failMsg, result, ESMF_SRCLINE)


    if (finalrc.EQ.ESMF_SUCCESS) then
       print *, "PASS: ESMF_FieldBundleCreateEx.F90"
    else
       print *, "FAIL: ESMF_FieldBundleCreateEx.F90"
    end if

      call ESMF_Finalize(rc=rc)

     end program ESMF_FieldBundleCreateEx
    
