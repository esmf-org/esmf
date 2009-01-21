! $Id: ESMF_GridCreateFromF90ArraysEx.F90,v 1.1.2.2 2009/01/21 21:25:21 cdeluca Exp $
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

program ESMF_GridCreateEx

!==============================================================================
!ESMF_EX_NOTWORKING_AMPLE        String used by test script to count examples.
!==============================================================================

!  !PROGRAM: ESMF_GridCreateEx - Examples of Grid creation.
!
!  !DESCRIPTION: 
!
! This program shows examples of Grid creation

!BOE
! \subsubsection{Example: Grid Creation from Existing F90 Arrays}~\label{sec:example5}
!
! This example illustrates the creation of a simple 2D Grid from coordinate data
!  contained in fortan arrays.  The new Grid contains just the center stagger location.
!  Each processor contains a pair of 10x10 fortran 90 arrays named fptrX and fptrY. 
!  These arrays contain the coordinates for the piece of the global Grid held by each
!  processor. The final global Grid will be 20x20 and the pieces of this Grid held
! by each processor are as follows:
!
! \begin{verbatim}
!
!  
!       20  +--------------+--------------+
!           |              |              |                       
!           |              |              |                       
!           |     PET3     |     PET4     |                       
!           |              |              |                       
!           |              |              |  
!       10  +--------------+--------------+ 
!           |              |              |                       
!           |              |              |                       
!           |     PET 1    |     PET2     |                       
!           |              |              |  
!           |              |              |                      
!        1  +--------------+--------------+
!           1             10             20  
!
!
! \end{verbatim}
!
!   As illustrated by the diagram, the arrays on processor 1 hold piece (1,1)-(10,10) of the 
!   global index space. The arrays on processor 2 hold piece (11,1)-(20,10). The arrays on 
!   processor 3 hold piece (1,11)-(10,20), and the arrays on processor 4 hold piece (11,11)-(20,20).
!
!EOE


!BOC
      ! Use ESMF framework module
      use ESMF_Mod
      implicit none

      ! Local variables  
      integer:: rc, finalrc
      integer:: myPet, npets, rootPet
      type(ESMF_VM):: vm
      type(ESMF_Config) :: config
      type(ESMF_DistGrid) :: distgrid
      type(ESMF_Array) :: gridCoordArrays(1,1)
      type(ESMF_StaggerLoc) :: staggerLocs(1)
      type(ESMF_ArraySpec) ::  arrayspec
      real(ESMF_KIND_R8), pointer :: fptrX(:,:),fptrY(:,:)
      integer  ::  deBlockList(2,3,4)
      integer :: petList(4)
!EOC         

      finalrc = ESMF_SUCCESS
      call ESMF_Initialize(vm=vm, rc=rc)
      call ESMF_VMGet(vm, localPet=myPet, petCount=npets, rc=rc)

      rootPet = zero

!BOE
! Create a distgrid to describe how the arrays are to be joined together into the
! global Grid. The {\it deBlockList} actually describes the location of
! DEs, but the default mapping between PETs and DEs is DE m <-> PET m, so
! this is essentially the same thing (assuming the number of PETs and DEs is the 
! same).
!EOE


!BOC

      ! Describe how DEs are arranged
      deBlockList(:,1,1)  = (/1,1/)   ! min corner of PET 1
      deBlockList(:,2,1)  = (/1,10/) ! max corner of PET 1
      deBlockList(:,1,2)  = (/1,11/)   ! min corner of PET 2
      deBlockList(:,2,2)  = (/1,20/)  ! max corner of PET 2
      deBlockList(:,1,3)  = (/11,1/)   ! min corner of PET 3
      deBlockList(:,2,3)  = (/11,10/)  ! max corner of PET 3
      deBlockList(:,1,4)  = (/11,11/)   ! min corner of PET 4
      deBlockList(:,2,4)  = (/11,20/)  ! max corner of PET 4
      
      ! Construct distgrid
      distgrid = ESMF_DistGridCreate( minCorner=(/1,1/),  &
                          maxCorner=(/20,20/),    &
                          deBlockList=deBlockList, rc=rc)  

!EOC

!BOE
!  Create coordinate arrays from the fortan pointers and the distgrid.

!EOE
!BOC
      gridCoordArrays(1,1)=ESMF_ArrayCreate(fptrX,  &
                             distgrid=distgrid, rc=rc)

      gridCoordArrays(1,2)=ESMF_ArrayCreate(fptrY,  &
                             distgrid=distgrid, rc=rc)

!EOC


!BOE
! Load Stagger location corresponding to coordinate arrays.
!EOE

!BOC
     staggerlocs(1)=ESMF_STAGGERLOC_CENTER
!EOC

!BOE
!    Create a Grid from the coordinate arrays and the stagger location. 
!EOE

!BOC 
     patchGrid = ESMF_GridCreate(arrays=gridCoordArrays, &
                               staggerLocs=staggerlocs,rc=rc)
!EOC  


10 continue
  call ESMF_Finalize(rc=rc)
  
  if (rc/=ESMF_SUCCESS) finalrc = ESMF_FAILURE
  if (finalrc==ESMF_SUCCESS) then
    print *, "PASS: ESMF_GridEx.F90"
  else
    print *, "FAIL: ESMF_GridEx.F90"
  endif
  
end program
