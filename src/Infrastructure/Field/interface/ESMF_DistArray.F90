! $Id: ESMF_DistArray.F90,v 1.1 2003/07/10 14:55:58 nscollins Exp $
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
! ESMF Distributed Array module
      module ESMF_DistArrayMod
!
!==============================================================================
!
! This file contains the advanced Array interfaces for those
!  arrays which correspond to a distributed regular grid.  For the
!  base Array class and the more basic Array functions, see the Array
!  module in the Infrastructure/Data directory.
!
!------------------------------------------------------------------------------
! INCLUDES
!
!------------------------------------------------------------------------------
!BOP
! !MODULE: ESMF_DistArrayMod - Manage arrays corresponding to a distributed Grid
!
! !DESCRIPTION:
!
! The code in this file implements the higher level {\tt ESMF\_Array} 
!  methods which operate on data arrays which must correspond to an
!  existing distributed {\tt ESMF\_Grid} object.
!EOP
!
!------------------------------------------------------------------------------
! !USES:
      use ESMF_ArrayMod
      use ESMF_GridMod
      use ESMF_FieldMod
      implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
      private

!------------------------------------------------------------------------------
! !PUBLIC TYPES:

!------------------------------------------------------------------------------

! !PUBLIC MEMBER FUNCTIONS:

      public ESMF_ArrayRedist, ESMF_ArrayHalo
      public ESMF_ArrayAllGather, ESMF_ArrayGather, ESMF_ArrayScatter

!EOP

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_DistArray.F90,v 1.1 2003/07/10 14:55:58 nscollins Exp $'

!==============================================================================
!
! INTERFACE BLOCKS
!
!==============================================================================

!==============================================================================

      contains

!==============================================================================



!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      subroutine ESMF_ArrayRedist(array, layout, rank_trans, olddecompids, &
                                  decompids, redistarray, rc)
!
! !ARGUMENTS:
      type(ESMF_Array), intent(inout) :: array
      type(ESMF_DELayout) :: layout
      integer, dimension(:), intent(in) :: rank_trans
      integer, dimension(:), intent(in) :: olddecompids
      integer, dimension(:), intent(in) :: decompids
      type(ESMF_Array), intent(in) :: redistarray
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
! Used to redistribute an {\tt ESMF\_Array}.
!
!
!EOP
! !REQUIREMENTS:
        integer :: status ! local error status
        logical :: rcpresent ! did user specify rc?
        integer :: size_rank_trans
        integer :: size_decomp

! initialize return code; assume failure until success is certain
        status = ESMF_FAILURE
        rcpresent = .FALSE.
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
        endif

! call c routine to query index
        size_rank_trans = size(rank_trans)
        size_decomp = size(decompids)
        call c_ESMC_ArrayRedist(array, layout, rank_trans, size_rank_trans, &
                                olddecompids, decompids, size_decomp, &
                                redistarray, status)
        if (status .ne. ESMF_SUCCESS) then
          print *, "c_ESMC_ArrayRedist returned error"
          return
        endif

! set return code if user specified it
        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_ArrayRedist

!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      subroutine ESMF_ArrayHalo(array, layout, decompids, AI_exc, AI_tot, rc)
!
! !ARGUMENTS:
      type(ESMF_Array), intent(inout) :: array
      type(ESMF_DELayout) :: layout
      integer, dimension(:), intent(in) :: decompids
      type(ESMF_AxisIndex), dimension(:), intent(inout) :: AI_exc
      type(ESMF_AxisIndex), dimension(:), intent(inout) :: AI_tot
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
! Used to halo an {\tt ESMF\_Array}.
!
!
!EOP
! !REQUIREMENTS:
        integer :: status ! local error status
        logical :: rcpresent ! did user specify rc?
        integer :: size_decomp, size_AI
        integer :: i

! initialize return code; assume failure until success is certain
        status = ESMF_FAILURE
        rcpresent = .FALSE.
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
        endif

! subtract one from location parts of indices to translate to C++
        size_AI = size(AI_tot)
        do i = 1,size_AI
          AI_exc(i)%l = AI_exc(i)%l - 1
          AI_exc(i)%r = AI_exc(i)%r - 1
          AI_tot(i)%l = AI_tot(i)%l - 1
          AI_tot(i)%r = AI_tot(i)%r - 1
        enddo

! call c routine to halo
        size_decomp = size(decompids)
        call c_ESMC_ArrayHalo(array, layout, decompids, size_decomp, &
                              AI_exc, AI_tot, status)
        if (status .ne. ESMF_SUCCESS) then
          print *, "c_ESMC_ArrayHalo returned error"
          return
        endif

! add one back to location parts of indices to translate from C++
        size_AI = size(AI_tot)
        do i = 1,size_AI
          AI_exc(i)%l = AI_exc(i)%l + 1
          AI_exc(i)%r = AI_exc(i)%r + 1
          AI_tot(i)%l = AI_tot(i)%l + 1
          AI_tot(i)%r = AI_tot(i)%r + 1
        enddo

! set return code if user specified it
        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_ArrayHalo

!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      subroutine ESMF_ArrayAllGather(array, layout, decompids, &
                                     AI_exc, AI_tot, array_out, rc)
!
! !ARGUMENTS:
      type(ESMF_Array), intent(inout) :: array
      type(ESMF_DELayout), intent(in) :: layout
      integer, dimension(:), intent(in) :: decompids
      type(ESMF_AxisIndex), dimension(:), intent(inout) :: AI_exc
      type(ESMF_AxisIndex), dimension(:), intent(inout) :: AI_tot
      type(ESMF_Array), intent(out) :: array_out
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
! Used to gather a distributed {\tt ESMF\_Array} into a global {\tt ESMF\_Array} on all {\tt ESMF\_DE}s.
!
!
!EOP
! !REQUIREMENTS:
        integer :: status ! local error status
        logical :: rcpresent ! did user specify rc?
        integer :: size_decomp, size_AI
        integer :: i

! initialize return code; assume failure until success is certain
        status = ESMF_FAILURE
        rcpresent = .FALSE.
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
        endif

! subtract one from location parts of indices to translate to C++
        size_AI = size(AI_tot)
        do i = 1,size_AI
          AI_exc(i)%l = AI_exc(i)%l - 1
          AI_exc(i)%r = AI_exc(i)%r - 1
          AI_tot(i)%l = AI_tot(i)%l - 1
          AI_tot(i)%r = AI_tot(i)%r - 1
        enddo

! call c routine to allgather
        size_decomp = size(decompids)
        call c_ESMC_ArrayAllGather(array, layout, decompids, size_decomp, &
                                   AI_exc, AI_tot, array_out, status)

        if (status .ne. ESMF_SUCCESS) then
          print *, "c_ESMC_ArrayAllGather returned error"
          return
        endif

! add one back to location parts of indices to translate from C++
        size_AI = size(AI_tot)
        do i = 1,size_AI
          AI_exc(i)%l = AI_exc(i)%l + 1
          AI_exc(i)%r = AI_exc(i)%r + 1
          AI_tot(i)%l = AI_tot(i)%l + 1
          AI_tot(i)%r = AI_tot(i)%r + 1
        enddo

! set return code if user specified it
        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_ArrayAllGather

!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      subroutine ESMF_ArrayGather(array, layout, decompids, &
                                     AI_exc, AI_tot, deid, array_out, rc)
!
! !ARGUMENTS:
      type(ESMF_Array), intent(inout) :: array
      type(ESMF_DELayout), intent(in) :: layout
      integer, dimension(:), intent(in) :: decompids
      type(ESMF_AxisIndex), dimension(:), intent(inout) :: AI_exc
      type(ESMF_AxisIndex), dimension(:), intent(inout) :: AI_tot
      integer, intent(in) :: deid
      type(ESMF_Array), intent(out) :: array_out
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
! Used to gather a distributed {\tt ESMF\_Array} into a global {\tt ESMF\_Array} on all {\tt ESMF\_DE}s.
!
!
!EOP
! !REQUIREMENTS:
        integer :: status ! local error status
        logical :: rcpresent ! did user specify rc?
        integer :: size_decomp, size_AI
        integer :: i

! initialize return code; assume failure until success is certain
        status = ESMF_FAILURE
        rcpresent = .FALSE.
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
        endif

! subtract one from location parts of indices to translate to C++
        size_AI = size(AI_tot)
        do i = 1,size_AI
          AI_exc(i)%l = AI_exc(i)%l - 1
          AI_exc(i)%r = AI_exc(i)%r - 1
          AI_tot(i)%l = AI_tot(i)%l - 1
          AI_tot(i)%r = AI_tot(i)%r - 1
        enddo

! call c routine to allgather
        size_decomp = size(decompids)
        call c_ESMC_ArrayGather(array, layout, decompids, size_decomp, &
                                   AI_exc, AI_tot, deid, array_out, status)

        if (status .ne. ESMF_SUCCESS) then
          print *, "c_ESMC_ArrayGather returned error"
          return
        endif

! add one back to location parts of indices to translate from C++
        size_AI = size(AI_tot)
        do i = 1,size_AI
          AI_exc(i)%l = AI_exc(i)%l + 1
          AI_exc(i)%r = AI_exc(i)%r + 1
          AI_tot(i)%l = AI_tot(i)%l + 1
          AI_tot(i)%r = AI_tot(i)%r + 1
        enddo

! set return code if user specified it
        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_ArrayGather

!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      subroutine ESMF_ArrayScatter(array, layout, decompids, &
                                     AI_exc, AI_tot, deid, array_out, rc)
!
! !ARGUMENTS:
      type(ESMF_Array), intent(inout) :: array
      type(ESMF_DELayout), intent(in) :: layout
      integer, dimension(:), intent(in) :: decompids
      type(ESMF_AxisIndex), dimension(:), intent(inout) :: AI_exc
      type(ESMF_AxisIndex), dimension(:), intent(inout) :: AI_tot
      integer, intent(in) :: deid
      type(ESMF_Array), intent(out) :: array_out
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
! Used to scatter a single {\tt ESMF\_Array} into a distributed {\tt ESMF\_Array} across all {\tt ESMF\_DE}s.
!
!
!EOP
! !REQUIREMENTS:
        integer :: status ! local error status
        logical :: rcpresent ! did user specify rc?
        integer :: size_decomp, size_AI
        integer :: i

! initialize return code; assume failure until success is certain
        status = ESMF_FAILURE
        rcpresent = .FALSE.
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
        endif

! subtract one from location parts of indices to translate to C++
        size_AI = size(AI_tot)
        do i = 1,size_AI
          AI_exc(i)%l = AI_exc(i)%l - 1
          AI_exc(i)%r = AI_exc(i)%r - 1
          AI_tot(i)%l = AI_tot(i)%l - 1
          AI_tot(i)%r = AI_tot(i)%r - 1
        enddo

! call c routine to allgather
        size_decomp = size(decompids)
        call c_ESMC_ArrayScatter(array, layout, decompids, size_decomp, &
                                   AI_exc, AI_tot, deid, array_out, status)

        if (status .ne. ESMF_SUCCESS) then
          print *, "c_ESMC_ArrayScatter returned error"
          return
        endif

! add one back to location parts of indices to translate from C++
        size_AI = size(AI_tot)
        do i = 1,size_AI
          AI_exc(i)%l = AI_exc(i)%l + 1
          AI_exc(i)%r = AI_exc(i)%r + 1
          AI_tot(i)%l = AI_tot(i)%l + 1
          AI_tot(i)%r = AI_tot(i)%r + 1
        enddo

! set return code if user specified it
        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_ArrayScatter

!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      subroutine ESMF_ArrayReorder(array, newarrayspec, newarray, rc)
!
! !ARGUMENTS:
      type(ESMF_Array), intent(inout) :: array
      type(ESMF_ArraySpec), intent(in) :: newarrayspec
      type(ESMF_Array), intent(out):: newarray
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
! Used to alter the local memory ordering (layout) of this {\tt ESMF\_Array}.
!
! !TODO: remove this note before generating user documentation
!
! (i am not sure this makes sense now, or that the routine should be
! in this class. but i am leaving this here as a reminder that we
! might need some low level reorder functions. maybe the argument
! should be another array or an arrayspec which describes what you
! want, and the input array is what exists, and this routine can then
! make one into the other. is this a type of create? or is this
! a copy?)
!
!EOP
! !REQUIREMENTS:

!
! TODO: code goes here
!
        end subroutine ESMF_ArrayReorder


        end module ESMF_DistArrayMod
