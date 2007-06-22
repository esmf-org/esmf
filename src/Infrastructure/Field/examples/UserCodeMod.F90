! $Id: UserCodeMod.F90,v 1.6 2007/06/22 23:21:29 cdeluca Exp $
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

    module UserCodeMod

    use ESMF_Mod

    implicit none
    private :: countX, countY, haloWidth, nPEsX, nPEsY, distInternGridX, distInternGridY
    integer :: countX=50, countY=30
    integer :: haloWidth=2
    integer :: nPEsX=3, nPEsY=2
    integer, dimension(3) :: distInternGridX = (/ 10, 15, 25 /)
    integer, dimension(2) :: distInternGridY = (/ 12, 18 /)

    public UserGetPEDecomposition
    public UserGetInternGridCoords
    public UserGetInternGridDistribution
    public UserGetHalo
    public UserGetPointer2D
    public UserGetPointer3D

    contains


    subroutine UserGetPEDecomposition(x, y)
    ! dummy routine to return a logical decomposition of PEs

    integer, intent(out) :: x
    integer, intent(out) :: y

    x = nPEsX
    y = nPEsY

    end subroutine UserGetPEDecomposition


    subroutine UserGetInternGridCoords(coordX, coordY)
    ! Dummy routine to return interngrid axes coordinates.
    ! Please note that currently all of the ESMF_InternGridCreate functions refer to
    ! counts as the number of interngrid cells and not vertices, but the coordinates
    ! are defined at vertex points.  So there should be count+1 number of
    ! coordinates.

    real(ESMF_KIND_R8), dimension(:), pointer :: coordX
    real(ESMF_KIND_R8), dimension(:), pointer :: coordY

    integer :: i

    allocate(coordX(countX+1), &
             coordY(countY+1))

    coordX(1) = 0.0d0
    do i = 2, countX+1
      coordX(i) = coordX(i-1) + 1.0d0
    enddo
    coordY(1) = 0.0d0
    do i = 2, countY+1
      coordY(i) = coordY(i-1) + 2.0d0
    enddo

    end subroutine UserGetInternGridCoords


    subroutine UserGetInternGridDistribution(distX, distY)
    ! Dummy routine to return a distribution of interngrid cells.

    integer, dimension(:), pointer :: distX
    integer, dimension(:), pointer :: distY

    allocate(distX(size(distInternGridX)), &
             distY(size(distInternGridY)))

    distX = distInternGridX
    distY = distInternGridY

    end subroutine UserGetInternGridDistribution


    subroutine UserGetPointer2D(f90ptr, myX, myY)
    ! Dummy routine to return a fortran pointer, sized appropriately for this
    ! PE according to the set distribution of interngrid cells and the prescribed
    ! haloWidth.

    real(ESMF_KIND_R8), dimension(:,:), pointer :: f90ptr
    integer, intent(in) :: myX
    integer, intent(in) :: myY

    allocate(f90ptr(distInternGridX(myX)+2*haloWidth,distInternGridY(myY)+2*haloWidth))
    f90ptr = 1.0d0

    end subroutine UserGetPointer2D


    subroutine UserGetPointer3D(f90ptr, myX, myY)
    ! Dummy routine to return a fortran pointer, sized appropriately for this
    ! PE according to the set distribution of interngrid cells and the prescribed
    ! haloWidth.

    real(ESMF_KIND_R8), dimension(:,:,:), pointer :: f90ptr
    integer, intent(in) :: myX
    integer, intent(in) :: myY

    allocate(f90ptr(5,distInternGridX(myX)+2*haloWidth,distInternGridY(myY)+2*haloWidth))
    f90ptr = 2.0d0

    end subroutine UserGetPointer3D


    subroutine UserGetHalo(halo)
    ! Dummy routine to return a prescribed halo width.

    integer, intent(out) :: halo

    halo = haloWidth
    
    end subroutine UserGetHalo

    end module UserCodeMod


