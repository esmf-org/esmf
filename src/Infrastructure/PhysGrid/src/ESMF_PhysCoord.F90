! $Id: ESMF_PhysCoord.F90,v 1.1 2003/10/09 22:59:12 jwolfe Exp $
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
!     !  ESMF_PhysCoordType
!

      type ESMF_PhysCoordType
      sequence
      private

         type (ESMF_Base) :: base        ! contains coordinate name 

         character (len=ESMF_MAXSTR) :: &
            units                        ! units of coord (eg 'degrees')

         logical ::                     &! flags for special cases
            aligned,                    &! coord is aligned with logical axis
            equal_spaced,               &! coord is equally spaced
            cyclic                       ! coord is cyclic

         real (ESMF_KIND_R8) ::         &! axis extents
            min_val,                    &! minimum value of coordinate
            max_val,                    &! maximum value of coordinate
            origin_offset                ! use for grids in same coord system 
                                         ! but differ by simple shift,rotation
                                 
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
        integer :: coord_system
      end type

!------------------------------------------------------------------------------
! !PUBLIC TYPES:

      public ESMF_PhysCoord, ESMF_PhysCoordType, ESMF_CoordSystem

!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:
!
      public ESMF_PhysCoordCreate
      public ESMF_PhysCoordDestroy

      public ESMF_PhysCoordSet
      public ESMF_PhysCoordGetName
      public ESMF_PhysCoordGetUnits
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

!EOPI

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_PhysCoord.F90,v 1.1 2003/10/09 22:59:12 jwolfe Exp $'

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

! !DESCRIPTION:
!     This interface overloads the equality operator for the specific
!     ESMF grid coordinate system data type.  It is provided for easy
!     comparisons of coordinate systems with defined values.
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

! !DESCRIPTION:
!     This interface overloads the inequality operator for the specific
!     ESMF grid coordinate system data type.  It is provided for easy
!     comparisons of coordinate systems with defined values.
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
      function ESMF_PhysCoordCreate(name, units,                     &
                                    aligned, equal_spaced, cyclic,   &
                                    min_val, max_val, origin_offset, &
                                    rc)
!
! !RETURN VALUE:
      type(ESMF_PhysCoord) :: ESMF_PhysCoordCreate
!
! !ARGUMENTS:

      character (len=ESMF_MAXSTR), intent(in), optional :: &
         name                         ! name of coordinate axis (eg 'latitude')

      character (len=ESMF_MAXSTR), intent(in), optional :: &
         units                        ! units of coord (eg 'degrees')

      logical, intent(in), optional :: &
         aligned,                    &! coord is aligned with logical axis
         equal_spaced,               &! coord is equally spaced
         cyclic                       ! coord is cyclic

      real (ESMF_KIND_R8), intent(in), optional :: &! axis extents
         min_val,                    &! minimum value of coordinate
         max_val,                    &! maximum value of coordinate
         origin_offset                ! offset from origin of full axis

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
!     \item[name]
!          Name to use for this coordinate.  Generally, these should
!          follow standard CF conventions to allow the framework to
!          treat certain coordinates correctly (e.g. multi-valued longitude).
!     \item[[units]]
!          Units used for this coordinate (e.g. degrees).
!     \item[[aligned]]
!          True if physical coordinate is aligned with logical coordinate.
!     \item[[equal\_spaced]]
!          True when coordinate points are equally-spaced along axis.
!     \item[[cyclic]]
!          True if coordinate is cyclic.
!     \item[[min\_val]]
!          Minimum value for grid point along this coordinate.
!     \item[[max\_val]]
!          Maximum value for grid point along this coordinate.
!     \item[[origin\_offset]]
!          If the grid origin is offset or rotated from coordinate system
!          origin, this specifies the offset in this coordinate direction.
!          Note that this is the offset for the global grid, so if this
!          is being defined for a local DE, the offset refers only to the
!          origin of the full grid.
!     \item[[rc]] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
! !REQUIREMENTS:  TODO
!EOP

      type(ESMF_PhysCoordType), pointer :: physcoord ! Pointer to new PhysCoord
      integer :: status=ESMF_FAILURE                 ! Error status
      logical :: rcpresent=.FALSE.                   ! Return code present

!     Initialize pointers
      nullify(physcoord)
      nullify(ESMF_PhysCoordCreate%ptr)

!     Initialize return code
      if(present(rc)) then
        rcpresent=.TRUE.
        rc = ESMF_FAILURE
      endif


      allocate(physcoord, stat=status)
!     If error write message and return.
!     Formal error handling will be added asap.
      if(status /= ESMF_SUCCESS) then
        print *, "ERROR in ESMF_PhysCoordCreate: Allocate"
        return
      endif

      if (present(name)) then
         call ESMF_SetName(physcoord%base, name, "PhysCoord", status)
         if(status /= ESMF_SUCCESS) then
            print *, "ERROR in ESMF_PhysCoordCreate: set name"
            return
         endif
      endif

      if (present(units)) then
         physcoord%units = units
      else
         physcoord%units = 'unknown'
      endif

      if (present(aligned)) then
         physcoord%aligned = aligned
      else
         physcoord%aligned = .false.
      endif

      if (present(equal_spaced)) then
         physcoord%equal_spaced = equal_spaced
      else
         physcoord%equal_spaced = .false.
      endif

      if (present(cyclic)) then
         physcoord%cyclic = cyclic
      else
         physcoord%cyclic = .false.
      endif

      if (present(min_val)) then
         physcoord%min_val = min_val
      else
         physcoord%min_val = -HUGE(physcoord%max_val)
      endif

      if (present(max_val)) then
         physcoord%max_val = max_val
      else
         physcoord%max_val = HUGE(physcoord%max_val)
      endif

      if (present(origin_offset)) then
         physcoord%origin_offset = origin_offset
      else
     !    physcoord%origin_offset = 0.0_ESMF_R8
         physcoord%origin_offset = 0.0_ESMF_KIND_R8
      endif

!     Set return values.
      ESMF_PhysCoordCreate%ptr => physcoord
      if(rcpresent) rc = ESMF_SUCCESS

      end function ESMF_PhysCoordCreate

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_PhysCoordDestroy - Deallocates a PhysCoord

! !INTERFACE:
      subroutine ESMF_PhysCoordDestroy(physcoord, rc)
!
! !ARGUMENTS:

      type(ESMF_PhysCoord), intent(inout) :: &
         physcoord                    ! physical coordinate to destroy

      integer, intent(out), optional :: &
         rc                           ! return code

! !DESCRIPTION:
!     Deallocates a {\tt ESMF\_PhysCoord} object to free up space.
!
!     The arguments are:
!     \begin{description}
!     \item[physcoord]
!          The {\tt ESMF\_PhysCoord} object which is to be destroyed.
!     \item[[rc]] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
! !REQUIREMENTS:  TODO
!EOP

      integer :: status=ESMF_FAILURE                 ! Error status
      logical :: rcpresent=.FALSE.                   ! Return code present

!     Initialize return code
      if(present(rc)) then
        rcpresent=.TRUE.
        rc = ESMF_FAILURE
      endif

!     Deallocate physcoord
      deallocate(physcoord%ptr, stat=status)
      if(status /= ESMF_SUCCESS) then
        print *, "ERROR in ESMF_PhysCoordDestroy: Deallocate"
        return
      endif

!     Nullify pointer
      nullify(physcoord%ptr)

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
      subroutine ESMF_PhysCoordSet(physcoord, name, units,           &
                                    aligned, equal_spaced, cyclic,   &
                                    min_val, max_val, origin_offset, &
                                    rc)
!
! !ARGUMENTS:

      type(ESMF_PhysCoord), intent(inout) :: &
         physcoord                    ! PhysCoord object for which attributes
                                      ! are to be assigned

      character (len=ESMF_MAXSTR), intent(in), optional :: &
         name,                       &! name of coordinate axis (eg 'latitude')
         units                        ! units of coord (eg 'degrees')

      logical, intent(in), optional :: &
         aligned,                    &! coord is aligned with logical axis
         equal_spaced,               &! coord is equally spaced
         cyclic                       ! coord is cyclic

      real (ESMF_KIND_R8), intent(in), optional :: &! axis extents
         min_val,                    &! minimum value of coordinate
         max_val,                    &! maximum value of coordinate
         origin_offset                ! offset from origin of full axis

      integer, intent(out), optional :: &
         rc                           ! return code

! !DESCRIPTION:
!     Sets attributes of a {\tt ESMF\_PhysCoord} object.
!
!     The arguments are:
!     \begin{description}
!     \item[physcoord]
!          The {\tt ESMF\_PhysCoord} object for which attributes are
!          to be set.
!     \item[[name]]
!          Name to use for this coordinate.  Generally, these should
!          follow standard CF conventions to allow the framework to
!          treat certain coordinates correctly (e.g. multi-valued longitude).
!     \item[[units]]
!          Units used for this coordinate (e.g. degrees).
!     \item[[aligned]]
!          True if physical coordinate is aligned with logical coordinate.
!     \item[[equal\_spaced]]
!          True when coordinate points are equally-spaced along axis.
!     \item[[cyclic]]
!          True if coordinate is cyclic.
!     \item[[min\_val]]
!          Minimum value for grid point along this coordinate.
!     \item[[max\_val]]
!          Maximum value for grid point along this coordinate.
!     \item[[origin\_offset]]
!          If the grid origin is offset or rotated from coordinate system
!          origin, this specifies the offset in this coordinate direction.
!          Note that this is the offset for the global grid, so if this
!          is being defined for a local DE, the offset refers only to the
!          origin of the full grid.
!     \item[[rc]] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
! !REQUIREMENTS:  TODO
!EOP

      integer :: status=ESMF_FAILURE                 ! Error status
      logical :: rcpresent=.FALSE.                   ! Return code present

!     Initialize return code
      if(present(rc)) then
        rcpresent=.TRUE.
        rc = ESMF_FAILURE
      endif

!     Check to see if physcoord is valid
      if (.not. associated(physcoord%ptr)) then
        print *, "ERROR in ESMF_PhysCoordSet: physcoord not yet created"
        return
      endif

      if (present(name)) then
         call ESMF_SetName(physcoord%ptr%base, name, "PhysCoord", status)
         if(status /= ESMF_SUCCESS) then
            print *, "ERROR in ESMF_PhysCoordSet: set name"
            return
         endif
      endif

      if (present(units)) then
         physcoord%ptr%units = units
      endif

      if (present(aligned)) then
         physcoord%ptr%aligned = aligned
      endif

      if (present(equal_spaced)) then
         physcoord%ptr%equal_spaced = equal_spaced
      endif

      if (present(cyclic)) then
         physcoord%ptr%cyclic = cyclic
      endif

      if (present(min_val)) then
         physcoord%ptr%min_val = min_val
      endif

      if (present(max_val)) then
         physcoord%ptr%max_val = max_val
      endif

      if (present(origin_offset)) then
         physcoord%ptr%origin_offset = origin_offset
      endif

!     Set return values.
      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_PhysCoordSet

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_PhysCoordGetName - Retrieves name assigned to PhysCoord.

! !INTERFACE:
      function ESMF_PhysCoordGetName(physcoord, rc)
!
! !RETURN VALUE:
      character (len=ESMF_MAXSTR) :: &
         ESMF_PhysCoordGetName       ! name of the input physical coordinate
!
! !ARGUMENTS:
      type (ESMF_PhysCoord), intent(in) :: physcoord

      integer, intent(out), optional :: rc               

! !DESCRIPTION:
!     Retrieves the name that has been assigned to a {\tt ESMF\_PhysCoord}.
!
!     The arguments are:
!     \begin{description}
!     \item[physcoord]
!          Existing {\tt ESMF\_PhysCoord} for which name is requested.
!     \item[[rc]] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
! !REQUIREMENTS:  TODO
!EOP

      integer :: status=ESMF_FAILURE                 ! Error status
      logical :: rcpresent=.FALSE.                   ! Return code present

!     Initialize return code
      if(present(rc)) then
        rcpresent=.TRUE.
        rc = ESMF_FAILURE
      endif

!     Check for valid physcoord
      if (.not. associated(physcoord%ptr)) then
         print *, "ERROR in ESMF_PhysCoordGetName: PhysCoord does not exist "
         return
      endif

!     Now get name from base object
      call ESMF_GetName(physcoord%ptr%base, ESMF_PhysCoordGetName, status)
      if (status /= ESMF_SUCCESS) then
         print *, "ERROR in ESMF_PhysCoordGetName: error gettting name"
         return
      endif

!     Set return values.
      if(rcpresent) rc = ESMF_SUCCESS

      end function ESMF_PhysCoordGetName

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_PhysCoordGetUnits - Retrieves units of a PhysCoord.

! !INTERFACE:
      function ESMF_PhysCoordGetUnits(physcoord, rc)
!
! !RETURN VALUE:
      character (len=ESMF_MAXSTR) :: &
         ESMF_PhysCoordGetUnits       ! units of the input physical coordinate
!
! !ARGUMENTS:
      type (ESMF_PhysCoord), intent(in) :: physcoord

      integer, intent(out), optional :: rc               

! !DESCRIPTION:
!     Retrieves the units of a {\tt ESMF\_PhysCoord}.
!
!     The arguments are:
!     \begin{description}
!     \item[physcoord]
!          Existing {\tt ESMF\_PhysCoord} for which units are requested.
!     \item[[rc]] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
! !REQUIREMENTS:  TODO
!EOP

      integer :: status=ESMF_FAILURE                 ! Error status
      logical :: rcpresent=.FALSE.                   ! Return code present

!     Initialize return code
      if(present(rc)) then
        rcpresent=.TRUE.
        rc = ESMF_FAILURE
      endif

!     Check for valid physcoord
      if (.not. associated(physcoord%ptr)) then
         print *, "ERROR in ESMF_PhysCoordGetUnits: PhysCoord does not exist "
         return
      endif

!     Now get units
      ESMF_PhysCoordGetUnits = physcoord%ptr%units

!     Set return values.
      if(rcpresent) rc = ESMF_SUCCESS

      end function ESMF_PhysCoordGetUnits

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_PhysCoordGetExtents - Retrieves extents of a PhysCoord.

! !INTERFACE:
      subroutine ESMF_PhysCoordGetExtents(physcoord, min_val, max_val, &
                                          origin_offset, rc)
!
! !ARGUMENTS:
      type (ESMF_PhysCoord), intent(in) :: physcoord

      real (ESMF_KIND_R8), intent(out), optional :: &
         min_val,                   &! min coordinate value for this coord
         max_val,                   &! max coordinate value for this coord
         origin_offset               ! non-zero if origin different from
                                     !   coordinate system origin

      integer, intent(out), optional :: rc               

! !DESCRIPTION:
!     Retrieves the units of a {\tt ESMF\_PhysCoord}.
!
!     The arguments are:
!     \begin{description}
!     \item[physcoord]
!          Existing {\tt ESMF\_PhysCoord} for which units are requested.
!     \item[[min\_val]]
!          Minimum extent of this coordinate.
!     \item[[max\_val]]
!          Maximum extent of this coordinate.
!     \item[[origin\_offset]]
!          Used if coordinates have different origin from the coordinate
!          system origin.
!     \item[[rc]] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
! !REQUIREMENTS:  TODO
!EOP

      integer :: status=ESMF_FAILURE                 ! Error status
      logical :: rcpresent=.FALSE.                   ! Return code present

!     Initialize return code
      if(present(rc)) then
        rcpresent=.TRUE.
        rc = ESMF_FAILURE
      endif

!     Check for valid physcoord
      if (.not. associated(physcoord%ptr)) then
         print *, "ERROR in ESMF_PhysCoordGetExtents: PhysCoord does not exist "
         return
      endif

!     Now get requested extents
      if (present(min_val)) min_val = physcoord%ptr%min_val
      if (present(min_val)) max_val = physcoord%ptr%max_val
      if (present(origin_offset)) origin_offset = physcoord%ptr%origin_offset

!     Set return values.
      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_PhysCoordGetExtents

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_PhysCoordIsAligned - Checks alignment of physical,logical coordinate.

! !INTERFACE:
      function ESMF_PhysCoordIsAligned(physcoord, rc)
!
! !RETURN VALUE:
      logical :: &
         ESMF_PhysCoordIsAligned     ! true if physical,logical coord aligned
!
! !ARGUMENTS:
      type (ESMF_PhysCoord), intent(in) :: physcoord

      integer, intent(out), optional :: rc               

! !DESCRIPTION:
!     Checks to see whether the physical coordinate defined in an
!     {\tt ESMF\_PhysCoord} object is aligned with the logical axis.
!     Such an alignment allows for some optimization of grid operations.
!
!     The arguments are:
!     \begin{description}
!     \item[physcoord]
!          Existing {\tt ESMF\_PhysCoord} to check for alignment.
!     \item[[rc]] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
! !REQUIREMENTS:  TODO
!EOP

      integer :: status=ESMF_FAILURE                 ! Error status
      logical :: rcpresent=.FALSE.                   ! Return code present

!     Initialize return code
      if(present(rc)) then
        rcpresent=.TRUE.
        rc = ESMF_FAILURE
      endif

!     Check for valid physcoord
      if (.not. associated(physcoord%ptr)) then
         print *, "ERROR in ESMF_PhysCoordIsAligned: PhysCoord does not exist "
         return
      endif

!     Now get units
      ESMF_PhysCoordIsAligned = physcoord%ptr%aligned

!     Set return values.
      if(rcpresent) rc = ESMF_SUCCESS

      end function ESMF_PhysCoordIsAligned

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_PhysCoordIsEqualSpaced - Checks for equally-spaced coordinates

! !INTERFACE:
      function ESMF_PhysCoordIsEqualSpaced(physcoord, rc)
!
! !RETURN VALUE:
      logical :: &
         ESMF_PhysCoordIsEqualSpaced     ! true if physical,logical coord aligned
!
! !ARGUMENTS:
      type (ESMF_PhysCoord), intent(in) :: physcoord

      integer, intent(out), optional :: rc               

! !DESCRIPTION:
!     Checks to see whether the physical coordinate defined in an
!     {\tt ESMF\_PhysCoord} object is equally-spaced.
!     Such a condition allows for some optimization of grid operations.
!
!     The arguments are:
!     \begin{description}
!     \item[physcoord]
!          Existing {\tt ESMF\_PhysCoord} to check for equal spacing.
!     \item[[rc]] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
! !REQUIREMENTS:  TODO
!EOP

      integer :: status=ESMF_FAILURE                 ! Error status
      logical :: rcpresent=.FALSE.                   ! Return code present

!     Initialize return code
      if(present(rc)) then
        rcpresent=.TRUE.
        rc = ESMF_FAILURE
      endif

!     Check for valid physcoord
      if (.not. associated(physcoord%ptr)) then
         print *, "ERROR in ESMF_PhysCoordIsEqualSpaced: Coord does not exist "
         return
      endif

!     Now get units
      ESMF_PhysCoordIsEqualSpaced = physcoord%ptr%equal_spaced

!     Set return values.
      if(rcpresent) rc = ESMF_SUCCESS

      end function ESMF_PhysCoordIsEqualSpaced

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_PhysCoordIsCyclic - Checks alignment of physical,logical coordinate.

! !INTERFACE:
      function ESMF_PhysCoordIsCyclic(physcoord, rc)
!
! !RETURN VALUE:
      logical :: &
         ESMF_PhysCoordIsCyclic     ! true if physical,logical coord aligned
!
! !ARGUMENTS:
      type (ESMF_PhysCoord), intent(in) :: physcoord

      integer, intent(out), optional :: rc               

! !DESCRIPTION:
!     Checks to see whether the physical coordinate defined in an
!     {\tt ESMF\_PhysCoord} object is cyclic.
!
!     The arguments are:
!     \begin{description}
!     \item[physcoord]
!          Existing {\tt ESMF\_PhysCoord} to check for cyclic attribute.
!     \item[[rc]] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
! !REQUIREMENTS:  TODO
!EOP

      integer :: status=ESMF_FAILURE                 ! Error status
      logical :: rcpresent=.FALSE.                   ! Return code present

!     Initialize return code
      if(present(rc)) then
        rcpresent=.TRUE.
        rc = ESMF_FAILURE
      endif

!     Check for valid physcoord
      if (.not. associated(physcoord%ptr)) then
         print *, "ERROR in ESMF_PhysCoordIsCyclic: PhysCoord does not exist "
         return
      endif

!     Now get units
      ESMF_PhysCoordIsCyclic = physcoord%ptr%cyclic

!     Set return values.
      if(rcpresent) rc = ESMF_SUCCESS

      end function ESMF_PhysCoordIsCyclic

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_PhysCoordPointInRange - Checks whether coordinate contains point
!
! !INTERFACE:
      function ESMF_PhysCoordPointInRange(point, physcoord, rc)

!
! !RETURN VALUE:
      logical :: ESMF_PhysCoordPointInRange ! true if point in coordinate range
!
! !ARGUMENTS:

      real (ESMF_KIND_R8), intent(in) :: &
         point           ! coordinate value of point to check

      type (ESMF_PhysCoord), intent(in) :: &
         physcoord       ! physical coordinate to check if point contained

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
!     \item[physcoord]
!          Physical coordinate object to check whether point lies within
!          coordinate axis extents.
!     \item[[rc]]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:  SSSn.n, GGGn.n

      integer :: status=ESMF_FAILURE                 ! Error status
      logical :: rcpresent=.FALSE.                   ! Return code present

      real (ESMF_KIND_R8) :: &
         minlon, maxlon, pi   ! for treating double-value issue in longitude

!     Initialize return code
      if(present(rc)) then
        rcpresent=.TRUE.
        rc = ESMF_FAILURE
      endif
      ESMF_PhysCoordPointInRange = .false.

!     Check for valid physcoord
      if (.not. associated(physcoord%ptr)) then
         print *, "ERROR in ESMF_PhysCoordPointInRange: PhysCoord non-existent "
         return
      endif

!     Check coordinate range
      if (point >= physcoord%ptr%min_val .and. &
          point <= physcoord%ptr%max_val) then
         ESMF_PhysCoordPointInRange = .true.
      endif

!     If not in range but axis is longitude, check for longitude range problems.
      if (.not. ESMF_PhysCoordPointInRange .and. &
          ESMF_PhysCoordGetName(physcoord) == 'longitude') then

         minlon = physcoord%ptr%min_val
         maxlon = physcoord%ptr%max_val

         if (ESMF_PhysCoordGetUnits(physcoord) == 'degrees') then
            if (minlon - point > 270.0d0) then
               minlon = minlon - 360.0d0
               maxlon = maxlon - 360.0d0
            else if (minlon - point < -270.0d0) then
               minlon = minlon + 360.0d0
               maxlon = maxlon + 360.0d0
            endif
         else if (ESMF_PhysCoordGetUnits(physcoord) == 'radians') then
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

!     Set return value
      if (present(rc)) rc = ESMF_SUCCESS

      end function ESMF_PhysCoordPointInRange

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_PhysCoordPrint - Print the contents of a PhysCoord

! !INTERFACE:
      subroutine ESMF_PhysCoordPrint(physcoord, opt, rc)
!
! !ARGUMENTS:
      type(ESMF_PhysCoord), intent(in) :: physcoord      
      character (len=*), intent(in) :: opt      
      integer, intent(out), optional :: rc           
!
! !DESCRIPTION:
!      Print information about a {\tt ESMF\_PhysCoord}.  
!
!     The arguments are:
!     \begin{description}
!     \item[physcoord] 
!          Physcoord whose info is to be printed.
!     \item[[opt]]
!          Print ptions that control the type of information and level of 
!          detail.
!     \item[[rc]] 
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

      call ESMF_GetName(physcoord%ptr%base, char_tmp, status)
      print *, '  Name   :', trim(char_tmp)
      print *, '  Units  :', trim(physcoord%ptr%units)
      if (present(rc)) rc = status

!     coordinate extents

      print *, '  Extents:'
      print *, '  Minval :', physcoord%ptr%min_val
      print *, '  Maxval :', physcoord%ptr%max_val
      if (physcoord%ptr%origin_offset /= 0.0) then
         print *,'  Origin offset: ',physcoord%ptr%origin_offset
      endif

!     other attributes

      if (physcoord%ptr%aligned) then
         print *,'  Coordinate is     aligned with logical axis'
      else
         print *,'  Coordinate is not aligned with logical axis'
      endif

      if (physcoord%ptr%equal_spaced) then
         print *,'  Coordinate is     equally-spaced'
      else
         print *,'  Coordinate is not equally-spaced'
      endif

      if (physcoord%ptr%cyclic) then
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

      ESMF_CoordSystemEqual = (CoordSystem1%coord_system == &
                               CoordSystem2%coord_system)

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

      ESMF_CoordSystemNotEqual = (CoordSystem1%coord_system /= &
                                  CoordSystem2%coord_system)

      end function ESMF_CoordSystemNotEqual

!------------------------------------------------------------------------------

      end module ESMF_PhysCoordMod

