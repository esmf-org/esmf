! $Id: ESMF_ArrayFarrayEx.F90,v 1.6.2.7 2009/01/21 21:25:19 cdeluca Exp $
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

!==============================================================================
!ESMF_EXAMPLE        String used by test script to count examples.
!==============================================================================

!BOE
! \subsubsection{Array from native Fortran array with 1 DE per PET}
! 
! The create call of the {\tt ESMF\_Array} class has been overloaded
! extensively to facilitate the need for generality while keeping simple
! cases simple. The following program demonstrates one of the simpler
! cases, where existing local Fortran arrays are to be used to provide
! the PET-local memory allocations for the Array object.
!
!EOE
!BOC
program ESMF_ArrayFarrayEx

  use ESMF_Mod
  
  implicit none
  
!EOC
!BOE
! The Fortran language provides a variety of ways to define and allocate
! an array. Actual Fortran array objects must either be explicit-shape or
! deferred-shape. In the first case the memory allocation and deallocation is 
! automatic from the user's perspective and the details of the allocation 
! (static or dynamic, heap or stack) are left to the compiler. (Compiler flags
! may be used to control some of the details). In the second case, i.e. for 
! deferred-shape actual objects, the array definition must include the pointer 
! or allocatable attribute and it is the user's responsibility to allocate 
! memory. While it is also the user's responsibility to deallocate memory for
! arrays with pointer attribute the compiler will automatically deallocate
! allocatable arrays under certain circumstances defined by the Fortran
! standard.
!
! The {\tt ESMF\_ArrayCreate()} interface has been written to accept native
! Fortran arrays of any flavor as a means to allow user-contolled memory
! management. The Array create call will check on each PET if sufficient 
! memory has been provided by the specified Fortran arrays and will indicate 
! an error if a problem is detected. However, the Array create call cannot
! validate the lifetime of the provided memory allocations. If, for instance,
! an Array object was created in a subroutine from an automatic explicit-shape
! array or an allocatable array, the memory allocations referenced by the Array 
! object will be automatically deallocated on return from the subroutine unless
! provissions are made by the application writer to prevent such behavior. The
! Array object cannot contol when memory that has been provided by the user
! during Array creation becomes deallocated, however, the Array will indicate
! an error if it's memory references have been invalidated. 

! The easiest, portable way to provide safe native Fortran memory allocations
! to Array create is to use arrays with the pointer attribute. Memory allocated
! for an array pointer will not be deallocated automatically. However, in this
! case the possibility of memory leaks becomes an issue of concern. The 
! deallocation of memory provided to an Array in form of a native Fortran
! allocation will remain the users responsibility.
! 
! None of the concerns discussed above are an issue in this example where the
! native Fortran array {\tt farray} is defined in the main program. All
! different types of array memory allocation are demonstrated in this example.
! First {\tt farrayE} is defined as a 2D explicit-shape array on each PET which 
! will automatically provide memory for $10\times 10$ elements.
!EOE
!BOC
  ! local variables
  real(ESMF_KIND_R8)          :: farrayE(10,10)     ! explicit shape Fortran array
!EOC
!BOE
! Then an allocatable array {\tt farrayA} is declared which will be used
! to show user-controlled dynamic memory allocation.
!EOE
!BOC
  real(ESMF_KIND_R8), allocatable :: farrayA(:,:)   ! allocatable Fortran array
!EOC
!BOE
! Finally an array with pointer attribute {\tt farrayP} is declared, also used
! for user-controlled dynamic memory allocation.
!EOE
!BOC
  real(ESMF_KIND_R8), pointer :: farrayP(:,:)       ! Fortran array pointer 
!EOC
!BOE
! A matching array pointer must also be available to gain access to the arrays
! held by an Array object.
!EOE
!BOC
  real(ESMF_KIND_R8), pointer :: farrayPtr(:,:)     ! matching Fortran array pointer 
  type(ESMF_DistGrid)         :: distgrid           ! DistGrid object
  type(ESMF_Array)            :: array              ! Array object
  integer                     :: rc
  
!EOC
  type(ESMF_VM):: vm
  integer:: petCount
  
  ! result code
  integer :: finalrc
  
  finalrc = ESMF_SUCCESS
  
!BOC
  call ESMF_Initialize(rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
!EOC
  call ESMF_VMGetGlobal(vm, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  call ESMF_VMGet(vm, petCount=petCount, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  
  if (petCount /= 4) goto 10 ! TODO: use EXAMPLES_MULTI_ONLY once available

!BOE
! On each PET {\tt farrayE} can be accessed directly to initialize the entire
! PET-local array.
!EOE
!BOC
  farrayE = 12.45d0 ! initialize to some value
!EOC
!BOE
! In order to create an Array object a DistGrid must first be created that 
! describes the total index space and how it is decomposed and distributed.
! In the simplest case only the {\tt minIndex} and {\tt maxIndex} of the 
! total space must be provided.
!EOE
!BOC
  distgrid = ESMF_DistGridCreate(minIndex=(/1,1/), maxIndex=(/40,10/), rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
!BOE
! This example is assumed to run on 4 PETs. The default 2D decomposition will 
! then be into 4 x 1 DEs as to ensure 1 DE per PET. 
! 
! Now the Array object can be created using the {\tt farrayE} and the DistGrid
! just created.
!EOE
!BOC
  array = ESMF_ArrayCreate(farray=farrayE, distgrid=distgrid, &
    indexflag=ESMF_INDEX_DELOCAL, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
!call ESMF_ArrayPrint(array)
!BOE
! The 40 x 10 index space defined by the {\tt minIndex} and {\tt maxIndex} 
! arguments paired with the default decomposition will result in the following
! distributed Array.
!
! \begin{verbatim}
!
!       +---------------------------> 2nd dimension
!       |   (1,1)-------+
!       |     |         |
!       |     |   DE 0  |   <--- farray on PET 0
!       |     |         |
!       |     +------(10,10)
!       |  (11,1)-------+
!       |     |         |
!       |     |   DE 1  |   <--- farray on PET 1
!       |     |         |
!       |     +------(20,10)
!       |  (21,1)-------+
!       |     |         |
!       |     |   DE 2  |   <--- farray on PET 2
!       |     |         |
!       |     +------(30,10)
!       |  (31,1)-------+
!       |     |         |
!       |     |   DE 3  |   <--- farray on PET 3
!       |     |         |
!       |     +------(40,10)
!       v
!     1st dimension
!
! \end{verbatim}
!
! Providing {\tt farrayE} during Array creation does not change anything about
! the actual {\tt farrayE} object. This means that each PET can use its
! local {\tt farrayE} directly to access the memory referenced by the Array 
! object.
!EOE
!BOC
  print *, farrayE
!EOC
!BOE
!
! Another way of accessing the memory associated with an Array object is to 
! use {\tt ArrayGet()} to obtain an Fortran pointer that references the
! PET-local array.
!EOE
!BOC
  call ESMF_ArrayGet(array, farrayPtr=farrayPtr, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
!BOC
  print *, farrayPtr
!EOC
!BOE
! Finally the Array object must be destroyed. The PET-local memory of the
! {\tt farrayE}s will remain in user control and will not be altered by 
! {\tt ArrayDestroy()}.
!EOE
!BOC
  call ESMF_ArrayDestroy(array, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
!BOE
! Since the memory allocation for each {\tt farrayE} is automatic there is
! nothing more to do.
!
! The interaction between {\tt farrayE} and the Array class is representative
! also for the two other cases {\tt farrayA} and {\tt farrayP}. The only
! difference is in the handling of memory allocations.
!EOE
!BOC
  allocate(farrayA(10,10))    ! user controlled allocation
  farrayA = 23.67d0           ! initialize to some value
  array = ESMF_ArrayCreate(farray=farrayA, distgrid=distgrid, &
    indexflag=ESMF_INDEX_DELOCAL, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
!BOC
  print *, farrayA            ! print PET-local farrayA directly
  call ESMF_ArrayGet(array, farrayPtr=farrayPtr, rc=rc)! obtain array pointer
  print *, farrayPtr          ! print PET-local piece of Array through pointer
  call ESMF_ArrayDestroy(array, rc=rc) ! destroy the Array
  deallocate(farrayA)         ! user controlled de-allocation
!EOC
!BOE
! The {\tt farrayP} case is identical.
!EOE
!BOC
  allocate(farrayP(10,10))    ! user controlled allocation
  farrayP = 56.81d0           ! initialize to some value
  array = ESMF_ArrayCreate(farray=farrayP, distgrid=distgrid, &
    indexflag=ESMF_INDEX_DELOCAL, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
!BOC
  print *, farrayP            ! print PET-local farrayA directly
  call ESMF_ArrayGet(array, farrayPtr=farrayPtr, rc=rc)! obtain array pointer
  print *, farrayPtr          ! print PET-local piece of Array through pointer
  call ESMF_ArrayDestroy(array, rc=rc) ! destroy the Array
  deallocate(farrayP)         ! user controlled de-allocation
!EOC
!BOE
! To wrap things up the DistGrid object is destroyed and ESMF can be finalized.
!EOE
!BOC

  call ESMF_DistGridDestroy(distgrid, rc=rc) ! destroy the DistGrid
  
!EOC
10 continue
!BOC
  call ESMF_Finalize(rc=rc)
!EOC
  
  if (rc/=ESMF_SUCCESS) finalrc = ESMF_FAILURE
  if (finalrc==ESMF_SUCCESS) then
    print *, "PASS: ESMF_ArrayFarrayEx.F90"
  else
    print *, "FAIL: ESMF_ArrayFarrayEx.F90"
  endif

!BOC
end program
!EOC
