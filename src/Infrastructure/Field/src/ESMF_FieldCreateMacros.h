#if 0
! $Id: ESMF_FieldCreateMacros.h,v 1.25.2.11 2008/03/12 23:02:35 theurich Exp $
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
#endif
#if 0
!------------------------------------------------------------------------------
! Macros for the Field Create functions.
!------------------------------------------------------------------------------
#endif

#if 0
!------------------------------------------------------------------------------
! Documentation for the general FieldSetCommit<> macros.
!------------------------------------------------------------------------------
#endif

#define FieldSetCommitDoc() \
!------------------------------------------------------------------------------ @\
! <Created by macro - do not edit directly > @\
!BOP @\
! !IROUTINE: ESMF_FieldSetCommit - Finishes creating Field started with FieldCreateEmpty @\
! @\
! !INTERFACE: @\
!   ! Private name; call using ESMF_FieldSetCommit() @\
!   subroutine ESMF_FieldSetCommit<rank><type><kind>(field, grid, & @\
!                                         farray, copyflag, staggerloc, & @\
!                                         gridToFieldMap, ungriddedLBound, & @\
!                                         ungriddedUBound, maxHaloLWidth, & @\
!                                         maxHaloUWidth, rc) @\
! @\
! !DESCRIPTION: @\
!     This call completes an {\tt ESMF\_Field} allocated with the @\
!     {\tt ESMF\_FieldCreateEmpty()} call. @\
! @\
!     The arguments are: @\
!     \begin{description} @\
!     \item [field]  @\
!           Points to the {\tt ESMF\_Field} object to be completed and @\
!           committed in this call.  The {\tt field} will have the same dimension @\
!           (dimCount) as the rank of the {\tt farray}.  @\
!     \item [grid]  @\
!           Pointer to an {\tt ESMF\_Grid} object.  The dimCount of the @\
!           Grid must be smaller than or equal to the rank of the {\tt farray}. @\
!     \item [farray] @\
!           Native fortran data array to be copied/referenced in the {\tt field}. @\
!           The {\tt field} dimension (dimCount) will be the same as the dimCount @\
!           for the farray. @\
!     \item [copyflag] @\
!           Indicates whether to copy the {\tt farray} or reference it directly. @\
!           For valid values see \ref{opt:copyflag}.  The default is @\
!           {\tt ESMF\_DATA\_REF}. @\
!     \item [{[staggerloc]}] @\
!           Stagger location of data in grid cells.  For valid  @\
!           predefined values see Section \ref{sec:opt:staggerloc}. @\
!           To create a custom stagger location see Section @\
!           \ref{sec:usage:staggerloc:adv}. The default @\
!           value is ESMF\_STAGGERLOC\_CENTER. @\
!     \item [{[gridToFieldMap]}] @\
!           List with number of elements equal to the @\
!           {\tt grid}'s dimCount.  The list elements map each dimension @\
!           of the {\tt grid} to a dimension in the {\tt farray} by @\
!           specifying the appropriate {\tt farray} dimension index. The default is to @\
!           map all of the {\tt grid}'s dimensions against the lowest dimensions of @\
!           the {\tt farray} in sequence, i.e. {\tt gridToFieldMap} = (/1,2,3,.../). @\
!           The values of all {\tt gridToFieldMap} entries must be greater than or equal @\
!           to one and smaller than or equal to the {\tt farray} rank. @\
!           It is erroneous to specify the same {\tt gridToFieldMap} entry @\
!           multiple times. The total ungridded dimensions in the {\tt field} @\
!           are the total {\tt farray} dimensions less @\
!           the total (distributed + undistributed) dimensions in @\
!           the {\tt grid}.  Ungridded dimensions must be in the same order they are @\
!           stored in the {\t farray}.  Permutations of the order of @\
!           dimensions are handled via individual communication methods.  For example, @\
!           an undistributed dimension can be remapped to a distributed dimension @\
!           as part of the ESMF\_ ArrayRedist() operation. @\
!     \item [{[ungriddedLBound]}] @\
!           Lower bounds of the ungridded dimensions of the {\tt field}. @\
!           The number of elements in the {\tt ungriddedLBound} is equal to the number of ungridded @\
!           dimensions in the {\tt field}.  All ungridded dimensions of the @\
!           {\tt field} are also undistributed. If neither ungriddedLBounds or @\
!           ungriddedUBounds are specified, the ungriddedLBound defaults to 1, @\
!           and the ungriddedUBound defaults to the size of the dimension. @\
!           If either ungriddedLBounds OR ungriddedUBounds are specified, the @\
!           other will be calculated.  If BOTH are specified the values are checked @\
!           for consistency.  Note that the the ordering of @\
!           these ungridded dimensions is the same as their order in the {\tt farray}. @\
!           Note also that the bounds for undistributed dimensions included in the {\tt grid} are set @\
!           in the {\tt grid}. @\
!     \item [{[ungriddedUBound]}] @\
!           Upper bounds of the ungridded dimensions of the {\tt field}. @\
!           The number of elements in the {\tt ungriddedUBound} is equal to the number of ungridded @\
!           dimensions in the {\tt field}.  All ungridded dimensions of the @\
!           {\tt field} are also undistributed. If neither ungriddedLBounds or @\
!           ungriddedUBounds are specified, the ungriddedLBound defaults to 1, @\
!           and the ungriddedUBound defaults to the size of the dimension. @\
!           If either ungriddedLBounds OR ungriddedUBounds are specified, the @\
!           other will be calculated.  If BOTH are specified the values are checked @\
!           for consistency.  Note that the the ordering of @\
!           these ungridded dimensions is the same as their order in the {\tt farray}. @\
!           Note also that the bounds for undistributed dimensions included in the {\tt grid} are set @\
!           in the {\tt grid}. @\
!     \item [{[maxHaloLWidth]}] @\
!           Lower bound of halo region.  The size of this array is the number @\
!           of distributed dimensions in the {\tt grid}.  However, ordering of the elements @\
!           needs to be the same as they appear in the {\tt farray}.  Values default @\
!           to 0.  If values for maxHaloLWidth are specified they must be reflected in @\
!           the size of the {\tt farray}.  That is, for each distributed dimension the @\
!           {\tt farray} size should be {\tt maxHaloLWidth} + {\tt maxHaloUWidth} @\
!           + {\tt computationalCount}. Although the halo operation is not @\
!           implemented, the {\tt minHaloLWidth} is checked for validity and stored @\
!           in preparation for the implementation of the halo method. @\
!           HALO OPERATION NOT IMPLEMENTED @\
!     \item [{[maxHaloUWidth]}] @\
!           Upper bound of halo region.  The size of this array is the number @\
!           of distributed dimensions in the {\tt grid}.  However, ordering of the elements @\
!           needs to be the same as they appear in the {\tt farray}.  Values default @\
!           to 0.  If values for maxHaloUWidth are specified they must be reflected in @\
!           the size of the {\tt farray}.  That is, for each distributed dimension the @\
!           {\tt farray} size should {\tt maxHaloLWidth} + {\tt maxHaloUWidth} @\
!           + {\tt computationalCount}.  Although the halo operation is not @\
!           implemented, the {\tt maxHaloUWidth} is checked for validity and stored @\
!           in preparation for the implementation of the halo method.  @\
!           HALO OPERATION NOT IMPLEMENTED @\
!     \item [{[rc]}]  @\
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. @\
!     \end{description} @\
! @\
!EOP @\

#if 0
!------------------------------------------------------------------------------
! Finishes Field created with FieldCreateEmpty
!------------------------------------------------------------------------------
#endif

#define FieldSetCommitMacro(mname, mtypekind, mrank, mdim, mlen, mrng, mloc) \
!------------------------------------------------------------------------------ @\
! <Created by macro - do not edit directly > @\
^undef  ESMF_METHOD @\
^define ESMF_METHOD "ESMF_FieldSetCommit" @\
    subroutine ESMF_FieldSetCommit##mrank##D##mtypekind(field, grid, farray, & @\
                                         copyflag, staggerloc, & @\
                                         gridToFieldMap, ungriddedLBound, & @\
                                         ungriddedUBound, maxHaloLWidth, & @\
                                         maxHaloUWidth, rc) @\
@\
! input arguments @\
       type(ESMF_Field) :: field @\
       type(ESMF_Grid) :: grid                  @\
       mname (ESMF_KIND_##mtypekind), dimension(mdim), target :: farray @\
       type(ESMF_CopyFlag), intent(in), optional   :: copyflag @\
       type(ESMF_StaggerLoc), intent(in), optional ::staggerloc  @\
       integer, intent(in), optional :: gridToFieldMap(:)     @\
       integer, intent(in), optional :: ungriddedLBound(:) @\
       integer, intent(in), optional :: ungriddedUBound(:) @\
       integer, intent(in), optional :: maxHaloLWidth(:) @\
       integer, intent(in), optional :: maxHaloUWidth(:) @\
       integer, intent(out), optional :: rc                @\
! local variables @\
       mname (ESMF_KIND_##mtypekind), dimension(mdim), pointer :: fpointer @\
       type(ESMF_StaggerLoc)          :: localStaggerLoc  @\
       integer                        :: localrc, i, j, count @\
       integer                        :: fieldDimCount, fieldUngriddedDimCount @\
       integer                        :: gridDimCount, gridDistDimCount @\
       integer                        :: ungriddedIndex(ESMF_MAXDIM) @\
       integer                        :: distgridToArrayMap (ESMF_MAXDIM) @\
       integer                        :: undistLBound(ESMF_MAXDIM), undistUBound(ESMF_MAXDIM) @\
       integer                        :: localUngriddedLBound (ESMF_MAXDIM) @\
       integer                        :: localUngriddedUBound (ESMF_MAXDIM) @\
       integer                        :: localGridToFieldMap (ESMF_MAXDIM) @\
       integer                        :: localMaxHaloLWidth (ESMF_MAXDIM) @\
       integer                        :: localMaxHaloUWidth (ESMF_MAXDIM) @\
       integer                        :: localRemapMaxHaloLWidth (ESMF_MAXDIM) @\
       integer                        :: localRemapMaxHaloUWidth (ESMF_MAXDIM) @\
       integer                        :: localTempMaxHaloLWidth (ESMF_MAXDIM) @\
       integer                        :: localTempMaxHaloUWidth (ESMF_MAXDIM) @\
       logical                        :: isGridded(ESMF_MAXDIM) @\
       integer                        :: distgridToGridMap(ESMF_MAXDIM) @\
       integer                        :: indexGrid, indexMap @\
       logical                        :: isDistributed(ESMF_MAXDIM) @\
       type(ESMF_Array)               :: array, newarray @\
       type(ESMF_DistGrid)            :: distgrid @\
       integer                        :: compEUWidth(ESMF_MAXDIM), compELWidth(ESMF_MAXDIM) @\
       integer                        :: fieldUndistDimCount                         @\
       logical                        :: flipflop(ESMF_MAXDIM) @\
@\
       if (present(rc)) then @\
         rc = ESMF_RC_NOT_IMPL @\
       endif @\
       localrc = ESMF_RC_NOT_IMPL @\
@\
       ! make sure field, grid, farray are properly initialized @\
       ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit,field,rc) @\
       ESMF_INIT_CHECK_DEEP(ESMF_GridGetInit,grid,rc) @\
       ! The following use of fptr is a bit of trickery to get all F90 @\
       ! compilers to cooperate. For some compilers the associated() test @\
       ! will return .false. for farray of size 0. Some of those compilers @\
       ! will produce a run-time error in size(fptr). Other compilers will @\
       ! return .true. for the associated() test but return 0 in size(). @\
       fpointer => farray @\
       if(.not. associated(fpointer,farray)) then @\
         call ESMF_LogMsgSetError(ESMF_RC_OBJ_INIT, &  @\
           "- farray is not associated with memory allocation)", & @\
           ESMF_CONTEXT, rc)  @\
         return @\
       endif @\
@\
       if (size(fpointer)==0) then @\
         call ESMF_LogMsgSetError(ESMF_RC_OBJ_INIT, &  @\
           "- farray is not associated with memory allocation)", & @\
           ESMF_CONTEXT, rc)  @\
         return @\
       endif @\
@\
       ! Check the size of the native array. @\
       fieldDimCount = mrank @\
@\
       ! Get number of grid dimensions, number @\
       ! of distributed grid dimensions, distgrid, @\
       ! number of ungridded Field dimensions, @\
       ! and number of undistributed Field Dimensions @\
       call ESMF_GridGet(grid, dimCount=gridDimCount, distDimCount=gridDistDimCount, & @\
             distgridToGridMap=distgridToGridMap, & @\
             distgrid=distgrid, rc=localrc) @\
       if (ESMF_LogMsgFoundError(localrc, & @\
           ESMF_ERR_PASSTHRU, & @\
           ESMF_CONTEXT, rc)) return @\
@\
       if(fieldDimCount .lt. gridDimCount) then @\
            call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, &  @\
                "- farray rank must be greater than or equal to grid rank", & @\
                  ESMF_CONTEXT, rc)  @\
       endif @\
@\
       fieldUngriddedDimCount = fieldDimCount-gridDimCount @\
       fieldUndistDimCount = fieldDimCount-gridDistDimCount @\
@\
       ! Error Check Input @\
          if (present(gridToFieldMap)) then  @\
               if (size(gridToFieldMap) .ne. gridDimCount) then @\
          call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, &  @\
                "- gridToFieldMap size must equal to grid_rank", & @\
                  ESMF_CONTEXT, rc)  @\
          return @\
               endif @\
          endif @\
@\
          if (present(ungriddedLBound)) then  @\
               if (size(ungriddedLBound) .ne. fieldUngriddedDimCount) then @\
          call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, &  @\
                "- ungriddedLBound size must equal to array_rank-grid_rank", & @\
                  ESMF_CONTEXT, rc)  @\
          return @\
               endif @\
          endif @\
@\
          if (present(ungriddedUBound)) then  @\
               if (size(ungriddedUBound) .ne. fieldUngriddedDimCount) then @\
          call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, &  @\
                "- ungriddedUBound size must equal to array_rank-grid_rank", & @\
                  ESMF_CONTEXT, rc)  @\
          return @\
               endif @\
          endif @\
@\
          if (present(maxHaloLWidth)) then  @\
               if (size(maxHaloLWidth) .ne. gridDistDimCount) then @\
          call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, &  @\
                "- maxHaloLWidth must equal to distgrid_rank", & @\
                  ESMF_CONTEXT, rc)  @\
          return @\
               endif @\
          endif @\
@\
          if (present(maxHaloUWidth)) then  @\
               if (size(maxHaloUWidth) .ne. gridDistDimCount) then @\
          call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, &  @\
                "- maxHaloUWidth must equal to distgrid_rank", & @\
                  ESMF_CONTEXT, rc)  @\
          return @\
               endif @\
          endif @\
@\
       ! Set default values. @\
       if (present(staggerloc)) then @\
         localStaggerloc = staggerloc @\
       else @\
         localStaggerLoc = ESMF_STAGGERLOC_CENTER @\
       endif @\
@\
       if (present(gridToFieldMap)) then @\
         localGridToFieldMap(1:gridDimCount) = & @\
            gridToFieldMap (1:gridDimCount) @\
       else @\
         do i = 1, gridDimCount @\
           localGridToFieldMap(i) = i @\
         enddo @\
       endif @\
       ! gridToFieldMap elements must be in range 1...fieldRank and unique @\
       ! algorithm to check element uniqueness: @\
       !   run time: O(ESMF_MAXDIM) @\
       !   memory:   O(2*ESMF_MAXDIM) @\
       !          or O(ESMF_MAXDIM+ESMF_MAXDIM/sizeof(integer)) with bitvector @\
       flipflop = .false. @\
       do i = 1, gridDimCount @\
          if(localGridToFieldMap(i) .lt. 1 .or. & @\
            localGridToFieldMap(i) .gt. fieldDimCount) then @\
              call ESMF_LogMsgSetError(ESMF_RC_ARG_VALUE, &  @\
                    "- gridToFieldMap element must be within range 1...array rank", & @\
                      ESMF_CONTEXT, rc)  @\
              return @\
          endif @\
          if(flipflop(localGridToFieldMap(i))) then @\
              call ESMF_LogMsgSetError(ESMF_RC_ARG_VALUE, &  @\
                    "- gridToFieldMap element must be unique", & @\
                      ESMF_CONTEXT, rc)  @\
              return @\
          endif @\
          flipflop(localGridToFieldMap(i)) = .true. @\
       enddo @\
@\
       if(present(maxHaloLWidth)) then @\
         localMaxHaloLWidth(1:gridDistDimCount) = & @\
            maxHaloLWidth (1:gridDistDimCount) @\
       else @\
            localMaxHaloLWidth = 0 @\
       endif @\
@\
       if(present(maxHaloUWidth)) then @\
         localMaxHaloUWidth(1:gridDistDimCount) = & @\
            maxHaloUWidth (1:gridDistDimCount) @\
       else @\
            localMaxHaloUWidth = 0 @\
       endif @\
@\
       ! Here we get the lbounds and ubounds for ungridded @\
       ! dimensions from the native array, if it isn't input @\
       ! through the argument list.  First we need to set up @\
       ! an index array that holds the ungridded dimensions of @\
       ! the native array. @\
       @\
       ! Since we're saving the ungriddedIndex calculate it even  @\
       ! if ungridded bounds are present @\
       @\
       ! Figure out which dims are ungridded @\
       isGridded=.false. @\
       do i=1, gridDimCount @\
          isGridded(localGridToFieldMap(i))=.true. @\
       enddo @\
@\
       ! Use ungridded info to figure out the map from ungridded to field dims @\
       count=1 @\
       do i=1,fieldDimCount @\
         if (.not. isGridded(i)) then @\
             ungriddedIndex(count)=i  @\
             count=count+1            @\
         endif                        @\
       enddo                          @\
@\
       ! set Array ungridded bounds depending on what user provides@\
       if (present(ungriddedLBound)) then @\
          if(present(ungriddedUBound)) then @\
            ! Both present so copy @\
            localUngriddedLBound(1:fieldUngriddedDimCount) = & @\
                 ungriddedLBound(1:fieldUngriddedDimCount) @\
            localUngriddedUBound(1:fieldUngriddedDimCount) = & @\
                ungriddedUBound(1:fieldUngriddedDimCount) @\
          else  @\
            ! Copy lower bound and make upper bound high enough to fit @\
            localUngriddedLBound(1:fieldUngriddedDimCount) = & @\
                ungriddedLBound(1:fieldUngriddedDimCount) @\
            do i=1, fieldUngriddedDimCount        @\
              localUngriddedUBound(i) = ungriddedLBound(i)+ & @\
                size (farray,ungriddedIndex(i))-1 @\
            enddo   @\
          endif @\
        else  @\
          if(present(ungriddedUBound)) then @\
            ! Copy upper bound and make lower bound low enough to fit @\
            do i=1, fieldUngriddedDimCount        @\
              localUngriddedLBound(i) = ungriddedUBound(i)- & @\
                size (farray,ungriddedIndex(i))+1 @\
            enddo   @\
            localUngriddedUBound(1:fieldUngriddedDimCount) = & @\
                ungriddedUBound(1:fieldUngriddedDimCount) @\
          else  @\
            ! No user info copy array bounds @\
            ! Note: assumed shape bounds will be 1...size @\
            do i=1, fieldUngriddedDimCount        @\
               localUngriddedLBound(i) = lbound(farray,ungriddedIndex(i)) @\
               localUngriddedUBound(i) = ubound(farray,ungriddedIndex(i)) @\
           enddo   @\
         endif @\
       endif @\
@\
       ! Get computationalEdgeWidths                                        @\
       call ESMF_GridGet(grid, staggerloc=localStaggerloc, & @\
             computationalEdgeLWidth=compELWidth, & @\
             computationalEdgeUWidth=compEUWidth, & @\
             rc=localrc)                                                   @\
          if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &          @\
             ESMF_CONTEXT, rcToReturn=rc)) return                          @\
@\
       ! The undistributed info from the Grid needs to be @\
       ! combined with the ungridded info from the Field in order @\
       ! to create the Array for the Field. @\
       call ESMF_GridGetArrayUndistInfo(grid, & @\
            staggerloc=localStaggerLoc, & @\
            gridToArrayMap=localGridToFieldMap, & @\
            ungriddedLBound=localUngriddedLBound (1:fieldUngriddedDimCount), & @\
            ungriddedUBound=localUngriddedUBound (1:fieldUngriddedDimCount), & @\
            distgridToArrayMap=distgridToArrayMap, & @\
            undistLBound=undistLBound, undistUBound=undistUBound, & @\
            rc=localrc) @\
       if (ESMF_LogMsgFoundError(localrc, & @\
           ESMF_ERR_PASSTHRU, & @\
           ESMF_CONTEXT, rc)) return @\
@\
!       ! Change the order of haloWidth wrt the order of distgridToArrayMap @\
!       ! let user supplied maxHaloWidth be mhw(i, i=1...distgridRank) @\
!       ! let user supplied fortran array be fa(j, j=1...arrayRank) @\
!       ! Then: localRemapMaxHaloWidth[j=distgridToArrayMap(i)] = localMaxHaloWidth(i) @\
!       localRemapMaxHaloLWidth = -1 @\
!       localRemapMaxHaloUWidth = -1 @\
!       do i = 1, gridDistDimCount @\
!           localRemapMaxHaloLWidth(distgridToArrayMap(i)) = localMaxHaloLWidth(i)  @\
!           localRemapMaxHaloUWidth(distgridToArrayMap(i)) = localMaxHaloUWidth(i)  @\
!       enddo @\
!       ! pack these, people might do crazy mapping like (2,4,1) (1,5,3) etc... @\
!       ! complexity of the following loop (linear time packing) is: @\
!       !    run time: O(ESMF_MAXDIM) = sum(delta(i)*delta(count(i))) @\
!       !    memory: O(ESMF_MAXDIM) @\
!       ! algorithm correctness can be proven by induction @\
!       ! do we allow negative halo width? @\
!       ! if not, we will have to use another logic array to mark holes @\
!       count = 1 @\
!       do i = 1, gridDistDimCount @\
!            count = max(i, count) @\
!            if(localRemapMaxHaloLWidth(i) .eq. -1) then @\
!                do while(localRemapMaxHaloLWidth(count) .eq. -1 .and. & @\
!                    count .le. ESMF_MAXDIM) @\
!                    count = count + 1 @\
!                enddo @\
!                if(count .ge. (ESMF_MAXDIM+1)) exit @\
!                localRemapMaxHaloLWidth(i) = localRemapMaxHaloLWidth(count) @\
!                localRemapMaxHaloLWidth(count) = -1 @\
!            endif @\
!        enddo @\
!       count = 1 @\
!       do i = 1, gridDistDimCount @\
!            count = max(i, count) @\
!            if(localRemapMaxHaloUWidth(i) .eq. -1) then @\
!                do while(localRemapMaxHaloUWidth(count) .eq. -1 .and. & @\
!                    count .lt. ESMF_MAXDIM) @\
!                    count = count + 1 @\
!                enddo @\
!                if(count .ge. (ESMF_MAXDIM+1)) exit @\
!                localRemapMaxHaloUWidth(i) = localRemapMaxHaloUWidth(count) @\
!                localRemapMaxHaloUWidth(count) = -1 @\
!            endif @\
!        enddo @\
@\
       ! Create Array with undistributed dimensions                                @\
       array = ESMF_ArrayCreate(farray, distgrid=distgrid, & @\
               distgridToArrayMap=distgridToArrayMap (1:gridDistDimCount), & @\
               undistLBound=undistLBound(1:fieldUndistDimCount), & @\
               undistUBound=undistUBound(1:fieldUndistDimCount), & @\
               computationalEdgeLWidth=compELWidth(1:gridDistDimCount), & @\
               computationalEdgeUWidth=compEUWidth(1:gridDistDimCount), & @\
               totalLWidth=localMaxHaloLWidth(1:gridDistDimCount), & @\
               totalUWidth=localMaxHaloUWidth(1:gridDistDimCount), & @\
               staggerloc=localStaggerLoc%staggerloc, rc=localrc) @\
               if (ESMF_LogMsgFoundError(localrc, & @\
                      ESMF_ERR_PASSTHRU, & @\
                      ESMF_CONTEXT, rc)) return @\
@\
     ! Set Values in Field structure @\
@\
     ! default copyflag value is ESMF_DATA_REF @\
     ! set array_internal to .true. because field%array is internal @\
     field%ftypep%array_internal = .true. @\
     if(.not. present(copyflag)) then @\
         field%ftypep%array = array @\
     else @\
         if(copyflag == ESMF_DATA_REF) then @\
             field%ftypep%array = array @\
         else @\
             newarray = ESMF_ArrayCreate(array, rc=localrc) @\
             if (ESMF_LogMsgFoundError(localrc, & @\
                                     ESMF_ERR_PASSTHRU, & @\
                                     ESMF_CONTEXT, rc)) return @\
             field%ftypep%array = newarray @\
             call ESMF_ArrayDestroy(array, rc=localrc) @\
             if (ESMF_LogMsgFoundError(localrc, & @\
                                     ESMF_ERR_PASSTHRU, & @\
                                     ESMF_CONTEXT, rc)) return @\
         endif @\
     endif @\
@\
     ! Should call a common FieldSetCommitConstructor here instead @\
     ! of just setting things up ourselves @\
     ! (The field Sets were all moved here in preparation for this) @\
     field%ftypep%staggerloc = localStaggerLoc @\
     field%ftypep%gridToFieldMap(1:gridDimCount) = & @\
        localGridToFieldMap(1:gridDimCount) @\
     field%ftypep%maxHaloLWidth(1:gridDistDimCount) = & @\
        localMaxHaloLWidth (1:gridDistDimCount) @\
     field%ftypep%maxHaloUWidth(1:gridDistDimCount) = & @\
        localMaxHaloUWidth (1:gridDistDimCount) @\
     field%ftypep%ungriddedLBound(1:fieldUngriddedDimCount) = & @\
        localUngriddedLBound(1:fieldUngriddedDimCount) @\
     field%ftypep%ungriddedUBound(1:fieldUngriddedDimCount) = & @\
        localUngriddedUBound(1:fieldUngriddedDimCount) @\
     field%ftypep%datastatus = ESMF_STATUS_READY @\
     field%ftypep%grid  = grid @\
     field%ftypep%gridstatus = ESMF_STATUS_READY @\
     field%ftypep%fieldstatus = ESMF_STATUS_READY  @\
@\
     call ESMF_FieldValidate(field, rc=localrc) @\
     if (ESMF_LogMsgFoundError(localrc, & @\
       ESMF_ERR_PASSTHRU, & @\
       ESMF_CONTEXT, rc)) return @\
@\
     if (present(rc)) rc = ESMF_SUCCESS @\
   end subroutine ESMF_FieldSetCommit##mrank##D##mtypekind  @\
@\
! < end macro - do not edit directly >  @\
!------------------------------------------------------------------------------ @\

#if 0
!------------------------------------------------------------------------------
! Documentation for the general FieldCreateFromDataPtr<> macros.
!------------------------------------------------------------------------------
#endif

#define FieldCreateFromDataPtrDoc() \
!------------------------------------------------------------------------------ @\
! <Created by macro - do not edit directly > @\
!BOP @\
! !IROUTINE: ESMF_FieldCreateFromDataPtr - Creates a Field from Fortran data array @\
! @\
! !INTERFACE: @\
!   ! Private name; call using ESMF_FieldCreateFromDataPtr() @\
!   function ESMF_FieldCreateFromDataPtr<rank><type><kind>(grid, & @\
!                                         farray, copyflag, staggerloc, & @\
!                                         gridToFieldMap, ungriddedLBound, & @\
!                                         ungriddedUBound, maxHaloLWidth, & @\
!                                         maxHaloUWidth, name, iospec, rc) @\
! @\
! !RETURN VALUE: @\
!      type(ESMF_Field) :: ESMF_FieldCreateFromDataPtr<rank><type><kind> @\
! @\
! !ARGUMENTS: @\
!      type(ESMF_Grid) :: grid                  @\
!      <type> (ESMF_KIND_<kind>), dimension(<rank>), target :: farray @\
!      type(ESMF_CopyFlag), intent(in), optional   :: copyflag @\
!      type(ESMF_StaggerLoc), intent(in), optional ::staggerloc  @\
!      integer, intent(in), optional :: gridToFieldMap(:)     @\
!      integer, intent(in), optional :: ungriddedLBound(:) @\
!      integer, intent(in), optional :: ungriddedUBound(:) @\
!      integer, intent(in), optional :: maxHaloLWidth(:) @\
!      integer, intent(in), optional :: maxHaloUWidth(:) @\
!      character (len=*), intent(in), optional :: name  @\
!      type(ESMF_IOSpec), intent(in), optional :: iospec @\
!      integer, intent(out), optional :: rc                @\
! @\
! !DESCRIPTION: @\
!     An interface function to {\tt ESMF\_FieldCreateFromDataPtr()}. @\
!     Creates a {\tt ESMF\_Field} from a fortran data array and grid. @\
! @\
!     The arguments are: @\
!     \begin{description} @\
!     \item [grid]  @\
!           Pointer to an {\tt ESMF\_Grid} object.  The dimCount of the @\
!           Grid muust be smaller than or equal to the rank of the {\tt farray}. @\
!     \item [farray] @\
!           Native fortran data array to be copied/referenced in the {\tt field} @\
!           The {\tt field} dimension (dimCount) will be the same as the dimCount @\
!           for the farray. @\
!   \item [copyflag] @\
!           Whether to copy the {\tt farray} or reference directly. @\
!           For valid values see \ref{opt:copyflag}.  The default is @\
!           {\tt ESMF\_DATA\_REF}. @\
!     \item [{[staggerloc]}] @\
!           Stagger location of data in grid cells.  For valid  @\
!           predefined values see Section \ref{sec:opt:staggerloc}. @\
!           To create a custom stagger location see Section @\
!           \ref{sec:usage:staggerloc:adv}.  The default @\
!           value is ESMF\_STAGGERLOC\_CENTER. @\
!     \item [{[gridToFieldMap]}] @\
!           List with number of elements equal to the @\
!           {\tt grid}'s dimCount.  The list elements map each dimension @\
!           of the {\tt grid} to a dimension in the {\tt farray} by @\
!           specifying the appropriate {\tt farray} dimension index. The default is to @\
!           map all of the {\tt grid}'s dimensions against the lowest dimensions of @\
!           the {\tt farray} in sequence, i.e. {\tt gridToFieldMap} = (/1,2,3,.../). @\
!           The values of all {\tt gridToFieldMap} entries must be greater than or equal @\
!           to one and smaller than or equal to the {\tt farray} rank. @\
!           It is erroneous to specify the same {\tt gridToFieldMap} entry @\
!           multiple times. The total ungridded dimensions in the {\tt field} @\
!           are the total {\tt farray} dimensions less @\
!           the total (distributed + undistributed) dimensions in @\
!           the {\tt grid}.  Ungridded dimensions must be in the same order they are @\
!           stored in the {\t farray}.  Permutations of the order of @\
!           dimensions are handled via individual communication methods.  For example, @\
!           an undistributed dimension can be remapped to a distributed dimension @\
!           as part of the ESMF\_ ArrayRedist() operation. @\
!     \item [{[ungriddedLBound]}] @\
!           Lower bounds of the ungridded dimensions of the {\tt field}. @\
!           The number of elements in the {\tt ungriddedLBound} is equal to the number of ungridded @\
!           dimensions in the {\tt field}.  All ungridded dimensions of the @\
!           {\tt field} are also undistributed. If neither ungriddedLBounds or @\
!           ungriddedUBounds are specified, the ungriddedLBound defaults to 1, @\
!           and the ungriddedUBound defaults to the size of the dimension. @\
!           If either ungriddedLBounds OR ungriddedUBounds are specified, the @\
!           other will be calculated.  If BOTH are specified the values are checked @\
!           for consistency.  Note that the the ordering of @\
!           these ungridded dimensions is the same as their order in the {\tt farray}. @\
!           Note also that the bounds for undistributed dimensions included in the {\tt grid} are set @\
!           in the {\tt grid}. @\
!     \item [{[ungriddedUBound]}] @\
!           Upper bounds of the ungridded dimensions of the {\tt field}. @\
!           The number of elements in the {\tt ungriddedUBound} is equal to the number of ungridded @\
!           dimensions in the {\tt field}.  All ungridded dimensions of the @\
!           {\tt field} are also undistributed. If neither ungriddedLBounds or @\
!           ungriddedUBounds are specified, the ungriddedLBound defaults to 1, @\
!           and the ungriddedUBound defaults to the size of the dimension. @\
!           If either ungriddedLBounds OR ungriddedUBounds are specified, the @\
!           other will be calculated.  If BOTH are specified the values are checked @\
!           for consistency.  Note that the the ordering of @\
!           these ungridded dimensions is the same as their order in the {\tt farray}. @\
!           Note also that the bounds for undistributed dimensions included in the {\tt grid} are set @\
!           in the {\tt grid}. @\
!     \item [{[maxHaloLWidth]}] @\
!           Lower bound of halo region.  The size of this array is the number @\
!           of distributed dimensions in the {\tt grid}.  However, ordering of the elements @\
!           needs to be the same as they appear in the {\tt farray}.  Values default @\
!           to 0.  If values for maxHaloLWidth are specified they must be reflected in @\
!           the size of the {\tt farray}.  That is, for each distributed dimension the @\
!           {\tt farray} size should be {\tt maxHaloLWidth} + {\tt maxHaloUWidth} @\
!           + {\tt computationalCount}. Although the halo operation is not @\
!           implemented, the {\tt minHaloLWidth} is checked for validity and stored @\
!           in preparation for the implementation of the halo method. @\
!           HALO OPERATION NOT IMPLEMENTED @\
!     \item [{[maxHaloUWidth]}] @\
!           Upper bound of halo region.  The size of this array is the number @\
!           of distributed dimensions in the {\tt grid}.  However, ordering of the elements @\
!           needs to be the same as they appear in the {\tt farray}.  Values default @\
!           to 0.  If values for maxHaloUWidth are specified they must be reflected in @\
!           the size of the {\tt farray}.  That is, for each distributed dimension the @\
!           {\tt farray} size should {\tt maxHaloLWidth} + {\tt maxHaloUWidth} @\
!           + {\tt computationalCount}.  Although the halo operation is not @\
!           implemented, the {\tt maxHaloUWidth} is checked for validity and stored @\
!           in preparation for the implementation of the halo method.  @\
!           HALO OPERATION NOT IMPLEMENTED @\
!     \item [{[name]}]  @\
!           {\tt Field} name.  @\
!     \item [{[iospec]}]  @\
!           I/O specification. NOT IMPLEMENTED @\
!     \item [{[rc]}]  @\
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. @\
!     \end{description} @\
! @\
!EOP @\

#if 0
!------------------------------------------------------------------------------
! Create a field from fortran data array
!------------------------------------------------------------------------------
#endif

#define FieldCreateFromDataPtrMacro(mname, mtypekind, mrank, mdim, mlen, mrng, mloc) \
!------------------------------------------------------------------------------ @\
! <Created by macro - do not edit directly > @\
^undef  ESMF_METHOD @\
^define ESMF_METHOD "ESMF_FieldCreateFromDataPtr" @\
    function ESMF_FieldCreateFromDataPtr##mrank##D##mtypekind(grid, & @\
                                         farray, copyflag, staggerloc, & @\
                                         gridToFieldMap, ungriddedLBound, & @\
                                         ungriddedUBound, maxHaloLWidth, & @\
                                         maxHaloUWidth, name, iospec, rc) @\
! return value @\
 @\
      type(ESMF_Field) :: ESMF_FieldCreateFromDataPtr##mrank##D##mtypekind @\
 @\
! input arguments @\
      type(ESMF_Grid) :: grid                  @\
      mname (ESMF_KIND_##mtypekind), dimension(mdim), target :: farray @\
      type(ESMF_CopyFlag), intent(in), optional   :: copyflag @\
      type(ESMF_StaggerLoc), intent(in), optional ::staggerloc  @\
      integer, intent(in), optional :: gridToFieldMap(:)     @\
      integer, intent(in), optional :: ungriddedLBound(:) @\
      integer, intent(in), optional :: ungriddedUBound(:) @\
      integer, intent(in), optional :: maxHaloLWidth(:) @\
      integer, intent(in), optional :: maxHaloUWidth(:) @\
      character (len=*), intent(in), optional :: name  @\
      type(ESMF_IOSpec), intent(in), optional :: iospec @\
      integer, intent(out), optional :: rc                @\
 @\
! local variables @\
      integer          :: localrc @\
 @\
      if (present(rc)) rc = ESMF_RC_NOT_IMPL @\
      localrc = ESMF_RC_NOT_IMPL @\
 @\
      ESMF_FieldCreateFromDataPtr##mrank##D##mtypekind = & @\
          ESMF_FieldCreateEmpty(name, iospec, rc=localrc) @\
 @\
      if (ESMF_LogMsgFoundError(localrc, & @\
          ESMF_ERR_PASSTHRU, & @\
          ESMF_CONTEXT, rc)) return @\
 @\
      call ESMF_FieldSetCommit##mrank##D##mtypekind( & @\
          ESMF_FieldCreateFromDataPtr##mrank##D##mtypekind, & @\
          grid, farray, copyflag, & @\
          staggerloc, gridToFieldMap, & @\
          ungriddedLBound, ungriddedUBound, maxHaloLWidth, maxHaloUWidth, & @\
          rc=localrc) @\
 @\
      if (ESMF_LogMsgFoundError(localrc, & @\
          ESMF_ERR_PASSTHRU, & @\
          ESMF_CONTEXT, rc)) return @\
 @\
      call ESMF_FieldValidate(ESMF_FieldCreateFromDataPtr##mrank##D##mtypekind, & @\
          rc=localrc) @\
 @\
      if (ESMF_LogMsgFoundError(localrc, & @\
          ESMF_ERR_PASSTHRU, & @\
          ESMF_CONTEXT, rc)) return @\
 @\
      if (present(rc)) rc = ESMF_SUCCESS @\
    end function ESMF_FieldCreateFromDataPtr##mrank##D##mtypekind  @\
 @\
! < end macro - do not edit directly >  @\
!------------------------------------------------------------------------------ @\

