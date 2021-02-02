! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2021, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================

!==============================================================================
!ESMF_MULTI_PROC_EXAMPLE        String used by test script to count examples.
!==============================================================================

program ESMF_ArrayHaloEx
#include "ESMF.h"

  use ESMF
  use ESMF_TestMod
  
  implicit none
  
  ! local variables
  integer:: rc, petCount, localPet
  type(ESMF_VM):: vm
  type(ESMF_DistGrid):: distgrid
  type(ESMF_Array):: array
  type(ESMF_ArraySpec):: arrayspec
  type(ESMF_RouteHandle):: haloHandle, haloHandle2
  integer :: finalrc, result
  character(ESMF_MAXSTR) :: testname
  character(ESMF_MAXSTR) :: failMsg
  
  integer                     :: counter,i,j,step
  integer                     :: eLB(2,1), eUB(2,1)
  real(ESMF_KIND_R8), pointer :: farrayPtr(:,:)
  real(ESMF_KIND_R8)          :: a, b
  
  real(ESMF_KIND_R8)          :: farrayGather(10,20)
  real(ESMF_KIND_R8), allocatable :: farrayTemp(:,:)
  type(ESMF_DistGridConnection), allocatable        :: connectionList(:)

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

  write(failMsg, *) "Example failure"
  write(testname, *) "Example ESMF_ArrayHaloEx"

! ------------------------------------------------------------------------------
! ------------------------------------------------------------------------------
  finalrc = ESMF_SUCCESS
  call ESMF_Initialize(vm=vm, defaultlogfilename="ArrayHaloEx.Log", &
                    logkindflag=ESMF_LOGKIND_MULTI, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
  if (petCount /= 4) then
    finalrc = ESMF_FAILURE
    goto 10
  endif
  
! ------------------------------------------------------------------------------
! ------------------------------------------------------------------------------

!BOE
!
! \subsubsection{Communication -- Halo}
! \label{Array:Halo}
! 
! One of the most fundamental communication pattern in domain decomposition
! codes is the {\em halo} operation. The ESMF Array class supports halos
! by allowing memory for extra elements to be allocated on each DE. See
! sections \ref{Array:fpadding} and \ref{Array:padding} for examples and
! details on how to create an Array with extra DE-local elements.
!
! Here we consider an Array object that is created on a DistGrid that 
! defines a 10 x 20 index space, decomposed into 4 DEs using a regular
! 2 x 2 decomposition.
!EOE
!BOC
  distgrid = ESMF_DistGridCreate(minIndex=(/1,1/), maxIndex=(/10,20/), &
    regDecomp=(/2,2/), rc=rc)
!EOC  
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE
! The Array holds 2D double precision float data.
!EOE
!BOC
  call ESMF_ArraySpecSet(arrayspec, typekind=ESMF_TYPEKIND_R8, rank=2, rc=rc)
!EOC  
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE
! The {\tt totalLWidth} and {\tt totalUWidth} arguments are used during Array
! creation to allocate 2 extra elements along every direction outside the 
! exclusive region defined by the DistGrid for every DE. (The {\tt indexflag}
! set to {\tt ESMF\_INDEX\_GLOBAL} in this example does not affect the halo
! behavior of Array. The setting is simply more convenient for the following
! code.)
!EOE
!BOC
  array = ESMF_ArrayCreate(arrayspec=arrayspec, distgrid=distgrid, &
    totalLWidth=(/2,2/), totalUWidth=(/2,2/), indexflag=ESMF_INDEX_GLOBAL, &
    rc=rc)
!EOC  
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE
! Without the explicit definition of boundary conditions in the DistGrid
! the following inner connections are defined.
!
! \begin{verbatim}
! 
!          +-------------------+       +-------------------+
!          | \       2       / |       | \       2       / |
!          |  +-------------+  |       |  +-------------+  |
!          |  |     DE 0    |  |       |  |     DE 2    |  |
!          |  |             |  |       |  |             |  |
!          |2 |    5 x 10   | 2|  <->  |2 |    5 x 10   | 2|
!          |  |             |  |       |  |             |  |
!          |  |             |  |       |  |             |  |
!          |  +-------------+  |       |  +-------------+  |
!          | /       2       \ |       | /       2       \ |
!          +-------------------+       +-------------------+
!
!                    ^            \/             ^
!                    |            /\             |
!                    v                           v
!
!          +-------------------+       +-------------------+
!          | \       2       / |       | \       2       / |
!          |  +-------------+  |       |  +-------------+  |
!          |  |     DE 1    |  |       |  |     DE 3    |  |
!          |  |             |  |       |  |             |  |
!          |2 |    5 x 10   | 2|  <->  |2 |    5 x 10   | 2|
!          |  |             |  |       |  |             |  |
!          |  |             |  |       |  |             |  |
!          |  +-------------+  |       |  +-------------+  |
!          | /       2       \ |       | /       2       \ |
!          +-------------------+       +-------------------+
!
! \end{verbatim}
!
! The exclusive region on each DE is of shape 5 x 10, while the total region
! on each DE is of shape (5+2+2) x (10+2+2) = 9 x 14. In a typical application
! the elements in the exclusive region are updated exclusively by the PET that
! owns the DE. In this example the exclusive elements on every DE are
! initialized to the value $f(i,j)$ of the geometric function
! \begin{equation}
! f(i,j) = \sin(\alpha i)\cos(\beta j),
! \end{equation}
! where
! \begin{equation}
! \alpha = 2\pi/N_i, i=1,...N_i
! \end{equation}
! and
! \begin{equation}
! \beta = 2\pi/N_j, j=1,...N_j,
! \end{equation}
! with $N_i = 10$ and $N_j = 20$.
!EOE
!BOC

  a = 2. * 3.14159 / 10.
  b = 2. * 3.14159 / 20.
  
  call ESMF_ArrayGet(array, farrayPtr=farrayPtr, rc=rc)
!EOC  
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC  
  
  call ESMF_ArrayGet(array, exclusiveLBound=eLB, exclusiveUBound=eUB, rc=rc)
!EOC  
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC  
  
  do j=eLB(2,1), eUB(2,1)
    do i=eLB(1,1), eUB(1,1)
      farrayPtr(i,j) = sin(a*i) * cos(b*j)  ! test function
    enddo
  enddo
!EOC  
!BOE
! The above loop only initializes the exclusive elements on each DE. The extra
! elements, outside the exclusive region, are left untouched, holding undefined
! values. Elements outside the exclusive region that correspond to 
! exclusive elements in neighboring DEs can be filled with the data values 
! in those neighboring elements. This is the definition of the halo operation.
!
! In ESMF the halo communication pattern is first precomputed and stored in
! a RouteHandle object. This RouteHandle can then be used repeatedly to 
! perform the same halo operation in the most efficient way.
!
! The default halo operation for an Array is precomputed by the following call.
!EOE
!BOC
  call ESMF_ArrayHaloStore(array=array, routehandle=haloHandle, rc=rc)
!EOC  
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE
! The {\tt haloHandle} now holds the default halo operation for {\tt array}, 
! which matches as many elements as possible outside the exclusive region to 
! their corresponding halo source elements in neighboring DEs. Elements that
! could not be matched, e.g. at the edge of the global domain with open
! boundary conditions, will not be updated by the halo operation.
!
! The {\tt haloHandle} is applied through the {\tt ESMF\_ArrayHalo()} method.
!EOE
!BOC
  call ESMF_ArrayHalo(array=array, routehandle=haloHandle, rc=rc)
!EOC  
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE
! Finally the resources held by {\tt haloHandle} need to be released.
!EOE
!BOC
  call ESMF_ArrayHaloRelease(routehandle=haloHandle, rc=rc)
!EOC  
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOE
! The {\tt array} object created above defines a 2 element wide rim around the
! exclusive region on each DE. Consequently the default halo operation used
! above will have resulted in updating both elements along the inside edges.
! For simple numerical kernels often a single halo element is 
! sufficient. One way to achieve this would be to reduce the size of the 
! rim surrounding the exclusive region to 1 element along each direction. 
! However, if the same Array object is also used for higher order kernels
! during a different phase of the calculation, a larger element rim is
! required. For this case {\tt ESMF\_ArrayHaloStore()} offers two optional
! arguments {\tt haloLDepth} and {\tt haloUDepth}. Using these arguments a
! reduced halo depth can be specified.
!EOE
!BOC
  call ESMF_ArrayHaloStore(array=array, routehandle=haloHandle, &
    haloLDepth=(/1,1/), haloUDepth=(/1,1/), rc=rc)
!EOC  
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOE
! This halo operation with a depth of 1 is sufficient to support a simple
! quadratic differentiation kernel.
!EOE
!BOC
  allocate(farrayTemp(eLB(1,1):eUB(1,1), eLB(2,1):eUB(2,1)))

  do step=1, 4
    call ESMF_ArrayHalo(array=array, routehandle=haloHandle, rc=rc)
!EOC    
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
    do j=eLB(2,1), eUB(2,1)
      do i=eLB(1,1), eUB(1,1)
        if (i==1) then
          ! global edge
          farrayTemp(i,j) = 0.5 * (-farrayPtr(i+2,j) + 4.*farrayPtr(i+1,j) &
            - 3.*farrayPtr(i,j)) / a
        else if (i==10) then
          ! global edge
          farrayTemp(i,j) = 0.5 * (farrayPtr(i-2,j) - 4.*farrayPtr(i-1,j) &
            + 3.*farrayPtr(i,j)) / a
        else
          farrayTemp(i,j) = 0.5 * (farrayPtr(i+1,j) - farrayPtr(i-1,j)) / a
        endif
      enddo
    enddo
    farrayPtr(eLB(1,1):eUB(1,1), eLB(2,1):eUB(2,1)) = farrayTemp
  enddo
  
  deallocate(farrayTemp)

  call ESMF_ArrayHaloRelease(routehandle=haloHandle, rc=rc)
!EOC  
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOE
! The special treatment of the global edges in the above kernel is due to the 
! fact that the underlying DistGrid object does not define any special 
! boundary conditions. By default open global boundaries are assumed which
! means that the rim elements on the global edges are untouched during
! the halo operation, and cannot be used in the symmetric numerical derivative
! formula. The kernel can be simplified (and the calculation is more precise)
! with periodic boundary conditions along the first Array dimension.
!
! First destroy the current Array and DistGrid objects.
!EOE
!BOC
  call ESMF_ArrayDestroy(array, rc=rc)
!EOC  
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
  call ESMF_DistGridDestroy(distgrid, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOE
! Create a DistGrid with periodic boundary condition along the first dimension.
!EOE
!BOC
  allocate(connectionList(1))  ! one connection
  call ESMF_DistGridConnectionSet(connection=connectionList(1), &
     tileIndexA=1, tileIndexB=1, positionVector=(/10, 0/), rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
  distgrid = ESMF_DistGridCreate(minIndex=(/1,1/), maxIndex=(/10,20/), &
    regDecomp=(/2,2/), connectionList=connectionList, rc=rc)
!EOC  
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
  deallocate(connectionList)
  array = ESMF_ArrayCreate(arrayspec=arrayspec, distgrid=distgrid, &
    totalLWidth=(/2,2/), totalUWidth=(/2,2/), indexflag=ESMF_INDEX_GLOBAL, &
    rc=rc)
!EOC  
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
!BOE
! Initialize the exclusive elements to the same geometric function as before.
!EOE
!BOC
  call ESMF_ArrayGet(array, farrayPtr=farrayPtr, rc=rc)
!EOC  
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC  
  
  call ESMF_ArrayGet(array, exclusiveLBound=eLB, exclusiveUBound=eUB, rc=rc)
!EOC  
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC  
  
  do j=eLB(2,1), eUB(2,1)
    do i=eLB(1,1), eUB(1,1)
      farrayPtr(i,j) = sin(a*i) * cos(b*j)  ! test function
    enddo
  enddo
!EOC  
!BOE
! The numerical kernel only operates along the first dimension. An
! asymmetric halo depth can be used to take this fact into account.
!EOE
!BOC
  call ESMF_ArrayHaloStore(array=array, routehandle=haloHandle, &
    haloLDepth=(/1,0/), haloUDepth=(/1,0/), rc=rc)
!EOC  
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
!BOE
! Now the same numerical kernel can be used without special treatment of
! global edge elements. The symmetric derivative formula can be used for
! all exclusive elements.
!EOE
!BOC
  allocate(farrayTemp(eLB(1,1):eUB(1,1), eLB(2,1):eUB(2,1)))

  do step=1, 4
    call ESMF_ArrayHalo(array=array, routehandle=haloHandle, rc=rc)
!EOC    
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
    do j=eLB(2,1), eUB(2,1)
      do i=eLB(1,1), eUB(1,1)
        farrayTemp(i,j) = 0.5 * (farrayPtr(i+1,j) - farrayPtr(i-1,j)) / a
      enddo
    enddo
    farrayPtr(eLB(1,1):eUB(1,1), eLB(2,1):eUB(2,1)) = farrayTemp
  enddo
  
!BOE
! The precision of the above kernel can be improved by going to 
! a higher order interpolation. Doing so requires that the halo depth must be
! increased. The following code resets the exclusive Array elements
! to the test function, precomputes a RouteHandle for a halo operation
! with depth 2 along the first dimension, and finally uses the deeper halo
! in the higher order kernel.
!EOE
!BOC  
  
  do j=eLB(2,1), eUB(2,1)
    do i=eLB(1,1), eUB(1,1)
      farrayPtr(i,j) = sin(a*i) * cos(b*j)  ! test function
    enddo
  enddo

  call ESMF_ArrayHaloStore(array=array, routehandle=haloHandle2, &
    haloLDepth=(/2,0/), haloUDepth=(/2,0/), rc=rc)
!EOC  
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOC  

  do step=1, 4
    call ESMF_ArrayHalo(array=array, routehandle=haloHandle2, rc=rc)
!EOC    
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
    do j=eLB(2,1), eUB(2,1)
      do i=eLB(1,1), eUB(1,1)
        farrayTemp(i,j) = (-farrayPtr(i+2,j) + 8.*farrayPtr(i+1,j) &
          - 8.*farrayPtr(i-1,j) + farrayPtr(i-2,j)) / (12.*a)
      enddo
    enddo
    farrayPtr(eLB(1,1):eUB(1,1), eLB(2,1):eUB(2,1)) = farrayTemp
  enddo
  
  deallocate(farrayTemp)

!EOC
!BOE
! ESMF supports having multiple halo operations defined on the same Array
! object at the same time. Each operation can be accessed through its unique
! RouteHandle. The above kernel could have made {\tt ESMF\_ArrayHalo()} calls
! with a depth of 1 along the first dimension using the previously precomputed
! {\tt haloHandle} if it needed to. Both RouteHandles need to release their
! resources when no longer used.
!EOE
!BOC
  

  call ESMF_ArrayHaloRelease(routehandle=haloHandle, rc=rc)
!EOC  
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOC
  call ESMF_ArrayHaloRelease(routehandle=haloHandle2, rc=rc)
!EOC  
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
!--- gather and output for plotting
  call ESMF_ArrayGather(array, farray=farrayGather, rootPet=0, rc=rc)
  if (localPet==0) then
    do j=1,20
      do i=1,10
        print *, i, j, farrayGather(i,j), sin(a*i) * cos(b*j)
      enddo
      print *
    enddo
  endif
!----------------------------------
  
!BOE
! Finally the Array and DistGrid objects can be destroyed.
!EOE
!BOC
  call ESMF_ArrayDestroy(array, rc=rc)
!EOC  
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
  call ESMF_DistGridDestroy(distgrid, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
  
! ------------------------------------------------------------------------------
! ------------------------------------------------------------------------------
10 continue
  ! IMPORTANT: ESMF_STest() prints the PASS string and the # of processors in the log
  ! file that the scripts grep for.
  call ESMF_STest((finalrc.eq.ESMF_SUCCESS), testname, failMsg, result, ESMF_SRCLINE)

  call ESMF_Finalize(rc=rc)
  
  if (rc/=ESMF_SUCCESS) finalrc = ESMF_FAILURE
  if (finalrc==ESMF_SUCCESS) then
    print *, "PASS: ESMF_ArrayHaloEx.F90"
  else
    print *, "FAIL: ESMF_ArrayHaloEx.F90"
  endif
! ------------------------------------------------------------------------------
! ------------------------------------------------------------------------------
  
end program
