! $Id: ESMF_PhysCoord.F90,v 1.5 2004/01/07 00:04:09 jwolfe Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2003, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the GPL.
!
!==============================================================================
!
! ESMF PhysCoord Module

  module ESMF_PhysCoordMod

!
!==============================================================================
!
! This file contains the PhysCoord class definition and all PhysCoord class
! methods.
!
!------------------------------------------------------------------------------
! INCLUDES
#include "ESMF.h"
!==============================================================================
!BOPI
! !MODULE: ESMF_PhysCoordMod - Description of a physical coordinate axis
!
! !DESCRIPTION:
!
! The code in this file implements the {\tt ESMF\_PhysCoord} class and is 
! responsible for describing a coordinate axis in the physical domain.
! A physical coordinate carries information describing coordinate attributes
! like names and flags for special properties of a coordinate axis.
! This information is used by PhysGrid and Grid to help describe the complete 
! physical properties of a grid.
!
!------------------------------------------------------------------------------
! !USES:
      use ESMF_BaseMod

      implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
      private

!------------------------------------------------------------------------------
!
!     ! ESMF_CoordKind
!
!     ! An enum for identifying a type of coordinate

      type ESMF_CoordKind
      sequence
      private
        integer :: kind
      end type

!------------------------------------------------------------------------------
!     !  ESMF_PhysCoordType
!

      type ESMF_PhysCoordType
      sequence
      private

         type (ESMF_Base) :: base   ! contains coordinate name 
         type (ESMF_CoordKind) :: kind
                                    ! type of coordinate
         character (len=ESMF_MAXSTR) :: units
                                    ! units of coord (eg 'degrees')

! flags for special cases
         logical :: aligned         ! coord is aligned with logical axis
         logical :: equalSpaced     ! coord is equally spaced
         logical :: cyclic          ! coord is cyclic

! axis extents
         real (ESMF_KIND_R8) :: minVal
                                    ! minimum value of coordinate
         real (ESMF_KIND_R8) :: maxVal
                                    ! maximum value of coordinate
         real (ESMF_KIND_R8) :: originOffset
                                    ! use for grids in same coord system but
                                    ! differ by simple shift, rotation
                                 
      end type

!------------------------------------------------------------------------------
!     !  ESMF_PhysCoord
!
!     !  The PhysCoord data structure that is passed between languages.

      type ESMF_PhysCoord
      sequence
        type (ESMF_PhysCoordType), pointer :: ptr ! pointer to a physdomain
      end type

!------------------------------------------------------------------------------
!
!     ! ESMF_CoordSystem
!
!     ! An enum for identifying a type of coordinate system

      type ESMF_CoordSystem
      sequence
      private
        integer :: coordSystem
      end type

!------------------------------------------------------------------------------
! !PUBLIC TYPES:

      public ESMF_PhysCoord, ESMF_PhysCoordType, ESMF_CoordSystem, ESMF_CoordKind

!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:
!
      public ESMF_PhysCoordCreate
      public ESMF_PhysCoordDestroy

      public ESMF_PhysCoordSet
      public ESMF_PhysCoordGet
      public ESMF_PhysCoordGetExtents
      public ESMF_PhysCoordIsAligned
      public ESMF_PhysCoordIsEqualSpaced
      public ESMF_PhysCoordIsCyclic
      public ESMF_PhysCoordPointInRange

      public ESMF_PhysCoordPrint
      public operator(==), operator(/=) ! for overloading CoordSystem
                                        ! comparison functions

!------------------------------------------------------------------------------
!
! !PUBLIC DATA MEMBERS:

      ! Supported ESMF Coordinate Systems
      !   Unknown     = unknown or undefined coord system
      !   User        = user-defined coordinate system
      !   Spherical   = spherical coordinates (lon,lat)
      !   Cartesian   = Cartesian coordinates (x,y)
      !   Cylindrical = cylindrical coordinates
      !   LatFourier  = mixed latitude/Fourier spectral space
      !   Spectral    = wavenumber space
      !   Depth       = vertical z coord. depth (0 at top surface)
      !   Height      = vertical z coord. height (0 at bottom)
      !   Pressure    = vertical pressure coordinate
      !   Sigma       = vertical sigma coordinate
      !   Theta       = vertical theta coordinate
      !   Eta         = vertical eta coordinate
      !   Isopycnal   = vertical density coordinate
      !   Hybrid      = hybrid vertical coordinates
      !   Lagrangian  = Lagrangian coordinates

      type (ESMF_CoordSystem), parameter, public :: &! coord systems
         ESMF_CoordSystem_Unknown        = ESMF_CoordSystem( 0), &
         ESMF_CoordSystem_User           = ESMF_CoordSystem( 1), &
         ESMF_CoordSystem_Spherical      = ESMF_CoordSystem( 2), &
         ESMF_CoordSystem_Cartesian      = ESMF_CoordSystem( 3), &
         ESMF_CoordSystem_Cylindrical    = ESMF_CoordSystem( 4), &
         ESMF_CoordSystem_LatFourier     = ESMF_CoordSystem( 5), &
         ESMF_CoordSystem_Spectral       = ESMF_CoordSystem( 6), &
         ESMF_CoordSystem_Depth          = ESMF_CoordSystem( 7), &
         ESMF_CoordSystem_Height         = ESMF_CoordSystem( 8), &
         ESMF_CoordSystem_Pressure       = ESMF_CoordSystem( 9), &
         ESMF_CoordSystem_Sigma          = ESMF_CoordSystem(10), &
         ESMF_CoordSystem_Theta          = ESMF_CoordSystem(11), &
         ESMF_CoordSystem_Eta            = ESMF_CoordSystem(12), &
         ESMF_CoordSystem_Isopycnal      = ESMF_CoordSystem(13), &
         ESMF_CoordSystem_Hybrid         = ESMF_CoordSystem(14), &
         ESMF_CoordSystem_Lagrangian     = ESMF_CoordSystem(15)   

      ! Supported ESMF Coordinate Kinds
      !   Unknown     = unknown or undefined coord system
      !   User        = user-defined coordinate system
      !   Lat         = latitude  (spherical coordinates)
      !   Lon         = longitude (spherical coordinates)
      !   Radius      = radius (spherical, cylindrical, polar coords)
      !   X           = Cartesian x coordinate
      !   Y           = Cartesian y coordinate
      !   Z           = Cartesian (or cylindrical) z coord
      !   Fourier     = Fourier wavenumber (for spectral space)
      !   Legendre    = Legendre wavenumber (for spherical spectral space)
      !   Azimuth     = Azimuthal angle (for polar, cylindrical, not longitude)
      !   Depth       = vertical z coord. depth (0 at top surface)
      !   Height      = vertical z coord. height (0 at bottom)
      !   Pressure    = vertical pressure coordinate
      !   Sigma       = vertical sigma coordinate
      !   Theta       = vertical theta coordinate
      !   Eta         = vertical eta coordinate
      !   Isopycnal   = vertical density coordinate
      !   Hybrid      = hybrid vertical coordinates
      !   Lagrangian  = Lagrangian coordinates

      type (ESMF_CoordKind), parameter, public :: &! coord kinds
         ESMF_CoordKind_Unknown     = ESMF_CoordKind( 1), &
         ESMF_CoordKind_User        = ESMF_CoordKind( 2), &
         ESMF_CoordKind_Lat         = ESMF_CoordKind( 3), &
         ESMF_CoordKind_Lon         = ESMF_CoordKind( 4), &
         ESMF_CoordKind_Radius      = ESMF_CoordKind( 5), &
         ESMF_CoordKind_X           = ESMF_CoordKind( 6), &
         ESMF_CoordKind_Y           = ESMF_CoordKind( 7), &
         ESMF_CoordKind_Z           = ESMF_CoordKind( 8), &
         ESMF_CoordKind_Fourier     = ESMF_CoordKind( 9), &
         ESMF_CoordKind_Legendre    = ESMF_CoordKind(10), &
         ESMF_CoordKind_Azimuth     = ESMF_CoordKind(11), &
         ESMF_CoordKind_Depth       = ESMF_CoordKind(12), &
         ESMF_CoordKind_Height      = ESMF_CoordKind(13), &
         ESMF_CoordKind_Pressure    = ESMF_CoordKind(14), &
         ESMF_CoordKind_Sigma       = ESMF_CoordKind(15), &
         ESMF_CoordKind_Theta       = ESMF_CoordKind(16), &
         ESMF_CoordKind_Eta         = ESMF_CoordKind(17), &
         ESMF_CoordKind_Isopycnal   = ESMF_CoordKind(18), &
         ESMF_CoordKind_Hybrid      = ESMF_CoordKind(19), &
         ESMF_CoordKind_Lagrangian  = ESMF_CoordKind(20)

!EOPI

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_PhysCoord.F90,v 1.5 2004/01/07 00:04:09 jwolfe Exp $'

!==============================================================================
!
! INTERFACE BLOCKS
!
!==============================================================================
!
!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      interface operator (==)

! !PRIVATE MEMBER FUNCTIONS:
         module procedure ESMF_CoordSystemEqual
         module procedure ESMF_CoordKindEqual

! !DESCRIPTION:
!     This interface overloads the equality operator for the specific
!     ESMF grid coordinate data types.  It is provided for easy
!     comparisons of coordinate systems or kinds with defined values.
!
!EOP
      end interface
!
!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      interface operator (/=)

! !PRIVATE MEMBER FUNCTIONS:
         module procedure ESMF_CoordSystemNotEqual
         module procedure ESMF_CoordKindNotEqual

! !DESCRIPTION:
!     This interface overloads the inequality operator for the specific
!     ESMF grid coordinate data types.  It is provided for easy
!     comparisons of coordinate systems or kinds with defined values.
!
!EOP
      end interface
!
!------------------------------------------------------------------------------
!==============================================================================

      contains

!==============================================================================
!
! This section includes the PhysCoord Create and Destroy methods.
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_PhysCoordCreate - One stop interface for creating new coord

! !INTERFACE:
      function ESMF_PhysCoordCreate(coordKind, name, units,       &
                                    aligned, equalSpaced, cyclic, &
                                    minVal, maxVal, originOffset, &
                                    rc)
!
! !RETURN VALUE:
      type(ESMF_PhysCoord) :: ESMF_PhysCoordCreate
!
! !ARGUMENTS:

      type (ESMF_CoordKind), intent(in) :: &
         coordKind                    ! type of coordinate being created

      character (len=ESMF_MAXSTR), intent(in), optional :: &
         name                         ! name of coordinate axis (eg 'latitude')

      character (len=ESMF_MAXSTR), intent(in), optional :: &
         units                        ! units of coord (eg 'degrees')

      logical, intent(in), optional :: &
         aligned,                    &! coord is aligned with logical axis
         equalSpaced,                &! coord is equally spaced
         cyclic                       ! coord is cyclic

      real (ESMF_KIND_R8), intent(in), optional :: &! axis extents
         minVal,                     &! minimum value of coordinate
         maxVal,                     &! maximum value of coordinate
         originOffset                 ! offset from origin of full axis

      integer, intent(out), optional :: &
         rc                           ! return code

! !DESCRIPTION:
!     Creates a new {\tt ESMF\_PhysCoord} object and constructs its
!     internals.  Returns a pointer to a new {\tt ESMF\_PhysCoord}.  Any
!     values not known at time of this call can be set later using
!     {\tt ESMF\_PhysCoordSet}.
!
!     The arguments are:
!     \begin{description}
!     \item[coordKind]
!          Type of coordinate being defined.  These must be one of
!          the supported ESMF\_CoordKind values to allow the framework
!          to treat certain coordinates correctly (e.g. special treatment
!          of longitude near multi-valued boundary).
!     \item[{[name]}]
!          Name to use for this coordinate.  Generally, these should
!          follow standard CF conventions.
!     \item[{[units]}]
!          Units used for this coordinate (e.g. degrees).
!     \item[{[aligned]}]
!          True if physical coordinate is aligned with logical coordinate.
!     \item[{[equalSpaced]}]
!          True when coordinate points are equally-spaced along axis.
!     \item[{[cyclic]}]
!          True if coordinate is cyclic.
!     \item[{[minVal]}]
!          Minimum value for grid point along this coordinate.
!     \item[{[maxVal]}]
!          Maximum value for grid point along this coordinate.
!     \item[{[originOffset]}]
!          If the grid origin is offset or rotated from coordinate system
!          origin, this specifies the offset in this coordinate direction.
!          Note that this is the offset for the global grid, so if this
!          is being defined for a local DE, the offset refers only to the
!          origin of the full grid.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
! !REQUIREMENTS:  TODO
!EOP

      type(ESMF_PhysCoordType), pointer :: physCoord ! Pointer to new PhysCoord
      integer :: status                              ! Error status
      logical :: rcpresent                           ! Return code present

!     Initialize pointers
      nullify(physCoord)
      nullify(ESMF_PhysCoordCreate%ptr)

!     Initialize return code
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent=.TRUE.
        rc = ESMF_FAILURE
      endif

      allocate(physCoord, stat=status)
!     If error write message and return.
!     Formal error handling will be added asap.
      if(status /= ESMF_SUCCESS) then
        print *, "ERROR in ESMF_PhysCoordCreate: Allocate"
        return
      endif

      physCoord%kind = coordKind

      if (present(name)) then
         call ESMF_SetName(physCoord%base, name, "PhysCoord", status)
         if(status /= ESMF_SUCCESS) then
            print *, "ERROR in ESMF_PhysCoordCreate: set name"
            return
         endif
      endif

      if (present(units)) then
         physCoord%units = units
      else
         physCoord%units = 'unknown'
      endif

      if (present(aligned)) then
         physCoord%aligned = aligned
      else
         physCoord%aligned = .false.
      endif

      if (present(equalSpaced)) then
         physCoord%equalSpaced = equalSpaced
      else
         physCoord%equalSpaced = .false.
      endif

      if (present(cyclic)) then
         physCoord%cyclic = cyclic
      else
         physCoord%cyclic = .false.
      endif

      if (present(minVal)) then
         physCoord%minVal = minVal
      else
         physCoord%minVal = -HUGE(physCoord%maxVal)
      endif

      if (present(maxVal)) then
         physCoord%maxVal = maxVal
      else
         physCoord%maxVal = HUGE(physCoord%maxVal)
      endif

      if (present(originOffset)) then
         physCoord%originOffset = originOffset
      else
     !    physCoord%originOffset = 0.0_ESMF_R8
         physCoord%originOffset = 0.0_ESMF_KIND_R8
      endif

!     Set return values.
      ESMF_PhysCoordCreate%ptr => physCoord
      if(rcpresent) rc = ESMF_SUCCESS

      end function ESMF_PhysCoordCreate

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_PhysCoordDestroy - Deallocates a PhysCoord

! !INTERFACE:
      subroutine ESMF_PhysCoordDestroy(physCoord, rc)
!
! !ARGUMENTS:

      type(ESMF_PhysCoord), intent(inout) :: &
         physCoord                    ! physical coordinate to destroy

      integer, intent(out), optional :: &
         rc                           ! return code

! !DESCRIPTION:
!     Deallocates a {\tt ESMF\_PhysCoord} object to free up space.
!
!     The arguments are:
!     \begin{description}
!     \item[physCoord]
!          The {\tt ESMF\_PhysCoord} object which is to be destroyed.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
! !REQUIREMENTS:  TODO
!EOP

      integer :: status                              ! Error status
      logical :: rcpresent                           ! Return code present

!     Initialize return code
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent=.TRUE.
        rc = ESMF_FAILURE
      endif

!     Deallocate physcoord
      deallocate(physCoord%ptr, stat=status)
      if(status /= ESMF_SUCCESS) then
        print *, "ERROR in ESMF_PhysCoordDestroy: Deallocate"
        return
      endif

!     Nullify pointer
      nullify(physCoord%ptr)

!     Set return values.
      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_PhysCoordDestroy

!------------------------------------------------------------------------------
!
! This section includes the PhysCoord Get and Set methods.
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_PhysCoordSet - Sets attributes of a physical coordinate

! !INTERFACE:
      subroutine ESMF_PhysCoordSet(physCoord, coordKind, name, units, &
                                    aligned, equalSpaced, cyclic,     &
                                    minVal, maxVal, originOffset,     &
                                    rc)
!
! !ARGUMENTS:

      type(ESMF_PhysCoord), intent(inout) :: &
         physCoord                    ! PhysCoord object for which attributes
                                      ! are to be assigned

      type(ESMF_CoordKind), intent(in), optional :: &
         coordKind                    ! type of ESMF coordinate
                                      ! overrides kind defined during creation

      character (len=ESMF_MAXSTR), intent(in), optional :: &
         name,                       &! name of coordinate axis (eg 'latitude')
         units                        ! units of coord (eg 'degrees')

      logical, intent(in), optional :: &
         aligned,                    &! coord is aligned with logical axis
         equalSpaced,                &! coord is equally spaced
         cyclic                       ! coord is cyclic

      real (ESMF_KIND_R8), intent(in), optional :: &! axis extents
         minVal,                     &! minimum value of coordinate
         maxVal,                     &! maximum value of coordinate
         originOffset                 ! offset from origin of full axis

      integer, intent(out), optional :: &
         rc                           ! return code

! !DESCRIPTION:
!     Sets attributes of a {\tt ESMF\_PhysCoord} object.
!
!     The arguments are:
!     \begin{description}
!     \item[physCoord]
!          The {\tt ESMF\_PhysCoord} object for which attributes are
!          to be set.
!     \item[{[coord\_kind]}]
!          Standard ESMF\_CoordKind specifying the type of coordinate.
!          This value would reset the kind defined when coordinate was created.
!     \item[{[name]}]
!          Name to use for this coordinate.  Generally, these should
!          follow standard CF conventions to allow the framework to
!          treat certain coordinates correctly (e.g. multi-valued longitude).
!     \item[{[units]}]
!          Units used for this coordinate (e.g. degrees).
!     \item[{[aligned]}]
!          True if physical coordinate is aligned with logical coordinate.
!     \item[{[equalSpaced]}]
!          True when coordinate points are equally-spaced along axis.
!     \item[{[cyclic]}]
!          True if coordinate is cyclic.
!     \item[{[minVal]}]
!          Minimum value for grid point along this coordinate.
!     \item[{[maxVal]}]
!          Maximum value for grid point along this coordinate.
!     \item[{[originOffset]}]
!          If the grid origin is offset or rotated from coordinate system
!          origin, this specifies the offset in this coordinate direction.
!          Note that this is the offset for the global grid, so if this
!          is being defined for a local DE, the offset refers only to the
!          origin of the full grid.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
! !REQUIREMENTS:  TODO
!EOP

      integer :: status                              ! Error status
      logical :: rcpresent                           ! Return code present

!     Initialize return code
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent=.TRUE.
        rc = ESMF_FAILURE
      endif

!     Check to see if physcoord is valid
      if (.not. associated(physCoord%ptr)) then
        print *, "ERROR in ESMF_PhysCoordSet: physcoord not yet created"
        return
      endif

      if (present(coordKind)) then
         physCoord%ptr%kind = coordKind
      endif
      
      if (present(name)) then
         call ESMF_SetName(physCoord%ptr%base, name, "PhysCoord", status)
         if(status /= ESMF_SUCCESS) then
            print *, "ERROR in ESMF_PhysCoordSet: set name"
            return
         endif
      endif

      if (present(units)) then
         physCoord%ptr%units = units
      endif

      if (present(aligned)) then
         physCoord%ptr%aligned = aligned
      endif

      if (present(equalSpaced)) then
         physCoord%ptr%equalSpaced = equalSpaced
      endif

      if (present(cyclic)) then
         physCoord%ptr%cyclic = cyclic
      endif

      if (present(minVal)) then
         physCoord%ptr%minVal = minVal
      endif

      if (present(maxVal)) then
         physCoord%ptr%maxVal = maxVal
      endif

      if (present(originOffset)) then
         physCoord%ptr%originOffset = originOffset
      endif

!     Set return values.
      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_PhysCoordSet

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_PhysCoordGet - Retrieves attributes of physical coordinate

! !INTERFACE:
      subroutine ESMF_PhysCoordGet(physCoord, coordKind, name, units, rc)
!
! !ARGUMENTS:

      type(ESMF_PhysCoord), intent(in) :: &
         physCoord                    ! PhysCoord object for which attributes
                                      ! are to be retrieved

      type(ESMF_CoordKind), intent(out), optional :: &
         coordKind                    ! type of ESMF coordinate

      character (len=ESMF_MAXSTR), intent(out), optional :: &
         name,                       &! name of coordinate axis (eg 'latitude')
         units                        ! units of coord (eg 'degrees')

      integer, intent(out), optional :: &
         rc                           ! return code

! !DESCRIPTION:
!     Retrieves some attributes of a {\tt ESMF\_PhysCoord} object.
!     This interface only retrieves coordinate kind, name and unit
!     attributes.  Other attributes have specific interfaces.
!
!     The arguments are:
!     \begin{description}
!     \item[physCoord]
!          The {\tt ESMF\_PhysCoord} object for which attributes are
!          to be set.
!     \item[{[coordKind]}]
!          Standard ESMF\_CoordKind specifying the type of coordinate.
!     \item[{[name]}]
!          Name assigned to this coordinate.
!     \item[{[units]}]
!          Units used for this coordinate (e.g. degrees).
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
! !REQUIREMENTS:  TODO
!EOP

      integer :: status                              ! Error status
      logical :: rcpresent                           ! Return code present

!     Initialize return code
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent=.TRUE.
        rc = ESMF_FAILURE
      endif

!     Check for valid physcoord
      if (.not. associated(physCoord%ptr)) then
         print *, "ERROR in ESMF_PhysCoordGetName: PhysCoord does not exist "
         return
      endif

!     Get coordinate kind if requested.
      if (present(coordKind)) then
         coordKind = physCoord%ptr%kind
      endif
      
!     Get name from base object if requested
      if (present(name)) then
         call ESMF_GetName(physCoord%ptr%base, name, status)
         if (status /= ESMF_SUCCESS) then
            print *, "ERROR in ESMF_PhysCoordGet: error gettting name"
            return
         endif
      endif

!     Get units if requested
      if (present(units)) then
         units = physCoord%ptr%units
      endif

!     Set return values.
      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_PhysCoordGet

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_PhysCoordGetExtents - Retrieves extents of a PhysCoord.

! !INTERFACE:
      subroutine ESMF_PhysCoordGetExtents(physCoord, minVal, maxVal, &
                                          originOffset, rc)
!
! !ARGUMENTS:
      type (ESMF_PhysCoord), intent(in) :: physCoord

      real (ESMF_KIND_R8), intent(out), optional :: &
         minVal,                    &! min coordinate value for this coord
         maxVal,                    &! max coordinate value for this coord
         originOffset                ! non-zero if origin different from
                                     !   coordinate system origin

      integer, intent(out), optional :: rc               

! !DESCRIPTION:
!     Retrieves the units of a {\tt ESMF\_PhysCoord}.
!
!     The arguments are:
!     \begin{description}
!     \item[physCoord]
!          Existing {\tt ESMF\_PhysCoord} for which units are requested.
!     \item[{[minVal]}]
!          Minimum extent of this coordinate.
!     \item[{[maxVal]}]
!          Maximum extent of this coordinate.
!     \item[{[originOffset]}]
!          Used if coordinates have different origin from the coordinate
!          system origin.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
! !REQUIREMENTS:  TODO
!EOP

      integer :: status                              ! Error status
      logical :: rcpresent                           ! Return code present

!     Initialize return code
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent=.TRUE.
        rc = ESMF_FAILURE
      endif

!     Check for valid physcoord
      if (.not. associated(physCoord%ptr)) then
         print *, "ERROR in ESMF_PhysCoordGetExtents: PhysCoord does not exist "
         return
      endif

!     Now get requested extents
      if (present(minVal)) minVal = physCoord%ptr%minVal
      if (present(minVal)) maxVal = physCoord%ptr%maxVal
      if (present(originOffset)) originOffset = physCoord%ptr%originOffset

!     Set return values.
      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_PhysCoordGetExtents

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_PhysCoordIsAligned - Checks alignment of physical,logical coordinate.

! !INTERFACE:
      function ESMF_PhysCoordIsAligned(physCoord, rc)
!
! !RETURN VALUE:
      logical :: &
         ESMF_PhysCoordIsAligned     ! true if physical,logical coord aligned
!
! !ARGUMENTS:
      type (ESMF_PhysCoord), intent(in) :: physCoord

      integer, intent(out), optional :: rc               

! !DESCRIPTION:
!     Checks to see whether the physical coordinate defined in an
!     {\tt ESMF\_PhysCoord} object is aligned with the logical axis.
!     Such an alignment allows for some optimization of grid operations.
!
!     The arguments are:
!     \begin{description}
!     \item[physCoord]
!          Existing {\tt ESMF\_PhysCoord} to check for alignment.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
! !REQUIREMENTS:  TODO
!EOP

      integer :: status                              ! Error status
      logical :: rcpresent                           ! Return code present

!     Initialize return code
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent=.TRUE.
        rc = ESMF_FAILURE
      endif

!     Check for valid physcoord
      if (.not. associated(physCoord%ptr)) then
         print *, "ERROR in ESMF_PhysCoordIsAligned: PhysCoord does not exist "
         return
      endif

!     Now get units
      ESMF_PhysCoordIsAligned = physCoord%ptr%aligned

!     Set return values.
      if(rcpresent) rc = ESMF_SUCCESS

      end function ESMF_PhysCoordIsAligned

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_PhysCoordIsEqualSpaced - Checks for equally-spaced coordinates

! !INTERFACE:
      function ESMF_PhysCoordIsEqualSpaced(physCoord, rc)
!
! !RETURN VALUE:
      logical :: &
         ESMF_PhysCoordIsEqualSpaced     ! true if physical,logical coord aligned
!
! !ARGUMENTS:
      type (ESMF_PhysCoord), intent(in) :: physCoord

      integer, intent(out), optional :: rc               

! !DESCRIPTION:
!     Checks to see whether the physical coordinate defined in an
!     {\tt ESMF\_PhysCoord} object is equally-spaced.
!     Such a condition allows for some optimization of grid operations.
!
!     The arguments are:
!     \begin{description}
!     \item[physCoord]
!          Existing {\tt ESMF\_PhysCoord} to check for equal spacing.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
! !REQUIREMENTS:  TODO
!EOP

      integer :: status                              ! Error status
      logical :: rcpresent                           ! Return code present

!     Initialize return code
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent=.TRUE.
        rc = ESMF_FAILURE
      endif

!     Check for valid physcoord
      if (.not. associated(physCoord%ptr)) then
         print *, "ERROR in ESMF_PhysCoordIsEqualSpaced: Coord does not exist "
         return
      endif

!     Now get units
      ESMF_PhysCoordIsEqualSpaced = physCoord%ptr%equalSpaced

!     Set return values.
      if(rcpresent) rc = ESMF_SUCCESS

      end function ESMF_PhysCoordIsEqualSpaced

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_PhysCoordIsCyclic - Checks alignment of physical,logical coordinate.

! !INTERFACE:
      function ESMF_PhysCoordIsCyclic(physCoord, rc)
!
! !RETURN VALUE:
      logical :: &
         ESMF_PhysCoordIsCyclic     ! true if physical,logical coord aligned
!
! !ARGUMENTS:
      type (ESMF_PhysCoord), intent(in) :: physCoord

      integer, intent(out), optional :: rc               

! !DESCRIPTION:
!     Checks to see whether the physical coordinate defined in an
!     {\tt ESMF\_PhysCoord} object is cyclic.
!
!     The arguments are:
!     \begin{description}
!     \item[physCoord]
!          Existing {\tt ESMF\_PhysCoord} to check for cyclic attribute.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
! !REQUIREMENTS:  TODO
!EOP

      integer :: status                              ! Error status
      logical :: rcpresent                           ! Return code present

!     Initialize return code
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent=.TRUE.
        rc = ESMF_FAILURE
      endif

!     Check for valid physcoord
      if (.not. associated(physCoord%ptr)) then
         print *, "ERROR in ESMF_PhysCoordIsCyclic: PhysCoord does not exist "
         return
      endif

!     Now get units
      ESMF_PhysCoordIsCyclic = physCoord%ptr%cyclic

!     Set return values.
      if(rcpresent) rc = ESMF_SUCCESS

      end function ESMF_PhysCoordIsCyclic

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_PhysCoordPointInRange - Checks whether coordinate contains point
!
! !INTERFACE:
      function ESMF_PhysCoordPointInRange(point, physCoord, rc)

!
! !RETURN VALUE:
      logical :: ESMF_PhysCoordPointInRange ! true if point in coordinate range
!
! !ARGUMENTS:

      real (ESMF_KIND_R8), intent(in) :: &
         point           ! coordinate value of point to check

      type (ESMF_PhysCoord), intent(in) :: &
         physCoord       ! physical coordinate to check if point contained

      integer, intent(out), optional :: rc  ! return code
!
! !DESCRIPTION:
!     This routine checks to see whether a coordinate value lies within
!     the coordinate extents in the {\tt ESMF\_PhysCoord} object.
!
!     The arguments are:
!     \begin{description}
!     \item[point]
!          Coordinate value of search point.
!     \item[physCoord]
!          Physical coordinate object to check whether point lies within
!          coordinate axis extents.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:  SSSn.n, GGGn.n

      integer :: status                              ! Error status
      logical :: rcpresent                           ! Return code present
      type(ESMF_CoordKind) :: coordKind              ! kind of coord
      character (len=ESMF_MAXSTR) :: units           ! units of coord
      real (ESMF_KIND_R8) :: i, minlon, maxlon, pi   
                                ! for treating double-value issue in longitude

!     Initialize return code
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent=.TRUE.
        rc = ESMF_FAILURE
      endif
      ESMF_PhysCoordPointInRange = .false.

!     Check for valid physcoord
      if (.not. associated(physCoord%ptr)) then
         print *, "ERROR in ESMF_PhysCoordPointInRange: PhysCoord non-existent "
         return
      endif

!     Check coordinate range
      if (point >= physCoord%ptr%minVal .and. &
          point <= physCoord%ptr%maxVal) then
         ESMF_PhysCoordPointInRange = .true.
      endif

!     If not in range but axis is longitude, check for longitude range problems.
      if (.not. ESMF_PhysCoordPointInRange) then
         call ESMF_PhysCoordGet(physCoord, &
                                coordKind=coordKind, units=units, rc=status)
         if (status /= ESMF_SUCCESS) then
            print *,'ERROR in PhysCoordPointInRange: Get coord failed'
         endif
         
         if (coordKind == ESMF_CoordKind_Lon) then

            minlon = physCoord%ptr%minVal
            maxlon = physCoord%ptr%maxVal

            if (units == 'degrees') then
               if (minlon - point > 270.0d0) then
                  minlon = minlon - 360.0d0
                  maxlon = maxlon - 360.0d0
               else if (minlon - point < -270.0d0) then
                  minlon = minlon + 360.0d0
                  maxlon = maxlon + 360.0d0
               endif
            else if (units == 'radians') then
               pi = 4.0d0*atan(1.0d0)
               if (minlon - point > 1.5*pi) then
                  minlon = minlon - 2.0d0*pi
                  maxlon = maxlon - 2.0d0*pi
               else if (minlon - point < -1.5*pi) then
                  minlon = minlon + 2.0d0*pi
                  maxlon = maxlon + 2.0d0*pi
               endif
            endif

            if (point >= minlon .and. point <= maxlon) then
               ESMF_PhysCoordPointInRange = .true.
            endif
         endif
      endif
      
!     Set return value
      if (present(rc)) rc = ESMF_SUCCESS

      end function ESMF_PhysCoordPointInRange

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_PhysCoordPrint - Print the contents of a PhysCoord

! !INTERFACE:
      subroutine ESMF_PhysCoordPrint(physCoord, opt, rc)
!
! !ARGUMENTS:
      type(ESMF_PhysCoord), intent(in) :: physCoord      
      character (len=*), intent(in) :: opt      
      integer, intent(out), optional :: rc           
!
! !DESCRIPTION:
!      Print information about a {\tt ESMF\_PhysCoord}.  
!
!     The arguments are:
!     \begin{description}
!     \item[physCoord] 
!          PhysCoord whose info is to be printed.
!     \item[{[opt]}]
!          Print ptions that control the type of information and level of 
!          detail.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:  SSSn.n, GGGn.n
      integer :: status
      character (len=ESMF_MAXSTR) :: char_tmp

! This code will surely change, but for development purposes it
! is necessary to have some information available currently.

      if (present(rc)) rc = ESMF_SUCCESS

      print *, 'PhysCoord:'

!     print name, units

      call ESMF_GetName(physCoord%ptr%base, char_tmp, status)
      print *, '  Name   :', trim(char_tmp)
      print *, '  Units  :', trim(physCoord%ptr%units)
      if (present(rc)) rc = status

!     coordinate extents

      print *, '  Extents:'
      print *, '  Minval :', physCoord%ptr%minVal
      print *, '  Maxval :', physCoord%ptr%maxVal
      if (physCoord%ptr%originOffset /= 0.0) then
         print *,'  Origin offset: ',physCoord%ptr%originOffset
      endif

!     other attributes

      if (physCoord%ptr%aligned) then
         print *,'  Coordinate is     aligned with logical axis'
      else
         print *,'  Coordinate is not aligned with logical axis'
      endif

      if (physCoord%ptr%equalSpaced) then
         print *,'  Coordinate is     equally-spaced'
      else
         print *,'  Coordinate is not equally-spaced'
      endif

      if (physCoord%ptr%cyclic) then
         print *,'  Coordinate is     cyclic.'
      else
         print *,'  Coordinate is not cyclic.'
      endif

      end subroutine ESMF_PhysCoordPrint

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_CoordSystemEqual - determines equality of coord systems
!
! !INTERFACE:
      function ESMF_CoordSystemEqual(CoordSystem1, CoordSystem2)

! !RETURN VALUE:
      logical :: ESMF_CoordSystemEqual

! !ARGUMENTS:

      type (ESMF_CoordSystem), intent(in) :: &
         CoordSystem1,      &! Two coordinate systems to compare for
         CoordSystem2        ! equality

! !DESCRIPTION:
!     This routine compares two ESMF Coordinate System types to see if 
!     they are equivalent.
!
!     The arguments are:
!     \begin{description}
!     \item[CoordSystem1, CoordSystem2]
!          Two coordinate systems to compare for equality
!     \end{description}
!
!EOP
! !REQUIREMENTS:  SSSn.n, GGGn.n

      ESMF_CoordSystemEqual = (CoordSystem1%coordSystem == &
                               CoordSystem2%coordSystem)

      end function ESMF_CoordSystemEqual

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_CoordSystemNotEqual - non-equality of coord systems
!
! !INTERFACE:
      function ESMF_CoordSystemNotEqual(CoordSystem1, CoordSystem2)

! !RETURN VALUE:
      logical :: ESMF_CoordSystemNotEqual

! !ARGUMENTS:

      type (ESMF_CoordSystem), intent(in) :: &
         CoordSystem1,      &! Two coordinate systems to compare for 
         CoordSystem2        ! inequality

! !DESCRIPTION:
!     This routine compares two ESMF Coordinate System types to see if 
!     they are unequal.
!
!     The arguments are:
!     \begin{description}
!     \item[CoordSystem1, CoordSystem2]
!          Two kinds of coordinate systems to compare for inequality
!     \end{description}
!
!EOP
! !REQUIREMENTS:  SSSn.n, GGGn.n

      ESMF_CoordSystemNotEqual = (CoordSystem1%coordSystem /= &
                                  CoordSystem2%coordSystem)

      end function ESMF_CoordSystemNotEqual

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_CoordKindEqual - determines equality of coord kinds
!
! !INTERFACE:
      function ESMF_CoordKindEqual(CoordKind1, CoordKind2)

! !RETURN VALUE:
      logical :: ESMF_CoordKindEqual

! !ARGUMENTS:

      type (ESMF_CoordKind), intent(in) :: &
         CoordKind1,      &! Two coordinate Kinds to compare for
         CoordKind2        ! equality

! !DESCRIPTION:
!     This routine compares two ESMF Coordinate Kind types to see if 
!     they are equivalent.
!
!     The arguments are:
!     \begin{description}
!     \item[CoordKind1, CoordKind2]
!          Two coordinate kinds to compare for equality
!     \end{description}
!
!EOP
! !REQUIREMENTS:  SSSn.n, GGGn.n

      ESMF_CoordKindEqual = (CoordKind1%kind == CoordKind2%kind)

      end function ESMF_CoordKindEqual

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_CoordKindNotEqual - non-equality of coord kinds
!
! !INTERFACE:
      function ESMF_CoordKindNotEqual(CoordKind1, CoordKind2)

! !RETURN VALUE:
      logical :: ESMF_CoordKindNotEqual

! !ARGUMENTS:

      type (ESMF_CoordKind), intent(in) :: &
         CoordKind1,      &! Two coordinate Kinds to compare for 
         CoordKind2        ! inequality

! !DESCRIPTION:
!     This routine compares two ESMF Coordinate Kind types to see if 
!     they are unequal.
!
!     The arguments are:
!     \begin{description}
!     \item[CoordKind1, CoordKind2]
!          Two coordinate kinds to compare for inequality
!     \end{description}
!
!EOP
! !REQUIREMENTS:  SSSn.n, GGGn.n

      ESMF_CoordKindNotEqual = (CoordKind1%kind /= CoordKind2%kind)

      end function ESMF_CoordKindNotEqual

!------------------------------------------------------------------------------

      end module ESMF_PhysCoordMod

