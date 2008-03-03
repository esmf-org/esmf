#if 0
! $Id: ESMF_FieldCreateMacros.h,v 1.25.2.4 2008/03/03 20:46:35 feiliu Exp $
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
!BOPI @\
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
! !ARGUMENTS: @\
!      type(ESMF_Field) :: field @\
!      type(ESMF_Grid) :: grid                  @\
!      <type> (ESMF_KIND_<kind>), dimension(<rank>), target :: farray @\
!      type(ESMF_CopyFlag), intent(in), optional   :: copyflag @\
!      type(ESMF_StaggerLoc), intent(in), optional ::staggerloc  @\
!      integer, intent(in), optional :: gridToFieldMap(:)     @\
!      integer, intent(in), optional :: ungriddedLBound(:) @\
!      integer, intent(in), optional :: ungriddedUBound(:) @\
!      integer, intent(in), optional :: maxHaloLWidth(:) @\
!      integer, intent(in), optional :: maxHaloUWidth(:) @\
!      integer, intent(out), optional :: rc                @\
! @\
! !DESCRIPTION: @\
!     An interface function to {\tt ESMF\_FieldSetCommit()}. @\
!     finishes the {\tt ESMF\_Field} started with FieldCreateEmpty. @\
! @\
!     The arguments are: @\
!     \begin{description} @\
!     \item [field]  @\
!           Points to an {\tt ESMF\_Field} object.  @\
!     \item [grid]  @\
!           Pointer to an {\tt ESMF\_Grid} object.  @\
!     \item [farray] @\
!           Native fortran data array to be copied/referenced in Field @\
!     \item [copyflag] @\
!           Whether to copy the existing data space or reference directly. Valid @\
!           values are {\tt ESMF\_DATA\_COPY} or {\tt ESMF\_DATA\_REF} (default). @\
!     \item [{[staggerloc]}] @\
!           Stagger location of data in grid cells.  For valid  @\
!           predefined values see Section \ref{sec:opt:staggerloc}. @\
!           To create a custom stagger location see Section @\
!           \ref{sec:usage:staggerloc:adv}. @\
!     \item [{[gridToFieldMap]}] @\
!           List that contains as many elements as is indicated by the {\tt grid}'s rank.  @\
!           The list elements map each dimension of the Grid object to a dimension in the @\
!           Field's Array by specifying the appropriate Array dimension index. The default is to @\
!           map all of the grid's dimensions against the lower dimensions of the Field's @\
!           Array in sequence, i.e. gridDimmap = (/1, 2, .../). Unmapped dimensions are @\
!           undistributed dimensions.  The total undistributed dimensions are the total  @\
!           Array dimensions - the distributed dimensions in the Grid (distRank).  All @\
!           gridToFieldMap entries must be greater than or equal to one and smaller than or equal @\
!           to the Array rank. It is erroneous to specify the same entry multiple times @\
!           unless it is zero.  If the Array rank is less than the Grid dimCount then @\
!           the default gridToFieldMap will contain zeros for the dimCount. @\
!           A zero entry in the dimmap indicates that the particular Grid dimension will @\
!           be replicating the Array across the DEs along this direction. @\
!     \item [{[ungriddedLBound]}] @\
!           Lower bounds of the ungridded dimensions of the Field. @\
!     \item [{[ungriddedUBound]}] @\
!           Upper bounds of the ungridded dimensions of the Field. @\
!     \item [{[maxHaloLWidth]}] @\
!           Lower bound of halo region.  Defaults to 0. ! NOT IMPLEMENTED @\
!     \item [{[maxHaloUWidth]}] @\
!           Upper bound of halo region.  Defaults to 0. ! NOT IMPLEMENTED @\
!     \item [{[rc]}]  @\
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. @\
!     \end{description} @\
! @\
!EOPI @\

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
       logical                        :: isGridded(ESMF_MAXDIM) @\
       logical                        :: flag @\
       type(ESMF_Array)               :: array, newarray @\
       type(ESMF_DistGrid)            :: distgrid @\
       integer                        :: compEUWidth(ESMF_MAXDIM), compELWidth(ESMF_MAXDIM) @\
       integer                        :: fieldUndistDimCount                         @\
@\
       if (present(rc)) then @\
         rc = ESMF_RC_NOT_IMPL @\
       endif @\
       localrc = ESMF_RC_NOT_IMPL @\
@\
       ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit,field,rc) @\
@\
       ! Check the size of the native array. @\
       fieldDimCount = size(shape(farray)) @\
@\
       ! Get number of grid dimensions, number @\
       ! of distributed grid dimensions, distgrid, @\
          ! number of ungridded Field dimensions, and number of undistributed Field Dimensions @\
       call ESMF_GridGet(grid, dimCount=gridDimCount, distDimCount=gridDistDimCount, & @\
                            distgrid=distgrid, rc=localrc) @\
       if (ESMF_LogMsgFoundError(localrc, & @\
           ESMF_ERR_PASSTHRU, & @\
           ESMF_CONTEXT, rc)) return @\
       fieldUngriddedDimCount = fieldDimCount-gridDimCount @\
       fieldUndistDimCount = fieldDimCount-gridDistDimCount @\
@\
       ! Error Check Input @\
          if (present(gridToFieldMap)) then  @\
               if (size(gridToFieldMap) < gridDimCount) then @\
          call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, &  @\
                "- gridToFieldMap must at least be of the same size & @\
                     as the Grid's dimension ", & @\
                  ESMF_CONTEXT, rc)  @\
          return @\
               endif @\
          endif @\
@\
          if (present(ungriddedLBound)) then  @\
               if (size(ungriddedLBound) < fieldUngriddedDimCount) then @\
          call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, &  @\
                "- ungriddedLBound must have size greater than or equal to & @\
                    size(farray) - the Grid's dimension ", & @\
                  ESMF_CONTEXT, rc)  @\
          return @\
               endif @\
          endif @\
@\
          if (present(ungriddedUBound)) then  @\
               if (size(ungriddedUBound) < fieldUngriddedDimCount) then @\
          call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, &  @\
                "- ungriddedUBound must have size greater than or equal to & @\
                    size(farray) - the Grid's dimension ", & @\
                  ESMF_CONTEXT, rc)  @\
          return @\
               endif @\
          endif @\
@\
          if (present(maxHaloLWidth)) then  @\
               if (size(maxHaloLWidth) < gridDistDimCount) then @\
          call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, &  @\
                "- maxHaloLWidth must have size greater than or equal to & @\
                    the Grid's distributed dimension ", & @\
                  ESMF_CONTEXT, rc)  @\
          return @\
               endif @\
          endif @\
@\
          if (present(maxHaloUWidth)) then  @\
               if (size(maxHaloUWidth) < gridDistDimCount) then @\
          call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, &  @\
                "- maxHaloUWidth must have size greater than or equal to & @\
                    the Grid's distributed dimension ", & @\
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
       if(present(maxHaloLWidth)) then @\
         localMaxHaloLWidth(1:gridDistDimCount) = maxHaloLWidth (1:gridDistDimCount) @\
       else @\
         do i = 1, gridDistDimCount @\
                       localMaxHaloLWidth(i) = 0 @\
         enddo @\
       endif @\
@\
       if(present(maxHaloUWidth)) then @\
         localMaxHaloUWidth(1:gridDistDimCount) = maxHaloUWidth (1:gridDistDimCount) @\
       else @\
         do i = 1, gridDistDimCount @\
           localMaxHaloUWidth(i) = 0 @\
         enddo @\
       endif @\
@\
       if (present(gridToFieldMap)) then @\
         localGridToFieldMap(1:gridDimCount) = gridToFieldMap (1:gridDimCount) @\
       else @\
         do i = 1, gridDimCount @\
           localGridToFieldMap(i) = i @\
         enddo @\
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
@\
       ! set Array ungridded bounds depending on what user provides@\
       if (present(ungriddedLBound)) then @\
          if(present(ungriddedUBound)) then @\
            ! Both present so copy @\
            localUngriddedLBound(1:fieldUngriddedDimCount) = ungriddedLBound(1:fieldUngriddedDimCount) @\
            localUngriddedUBound(1:fieldUngriddedDimCount) = ungriddedUBound(1:fieldUngriddedDimCount) @\
          else  @\
            ! Copy lower bound and make upper bound high enough to fit @\
            localUngriddedLBound(1:fieldUngriddedDimCount) = ungriddedLBound(1:fieldUngriddedDimCount) @\
            do i=1, fieldUngriddedDimCount        @\
              localUngriddedUBound(i) = ungriddedLBound(i)+size (farray,ungriddedIndex(i))-1 @\
            enddo   @\
          endif @\
        else  @\
          if(present(ungriddedUBound)) then @\
            ! Copy upper bound and make lower bound low enough to fit @\
            do i=1, fieldUngriddedDimCount        @\
              localUngriddedLBound(i) = ungriddedUBound(i)-size (farray,ungriddedIndex(i))+1 @\
            enddo   @\
            localUngriddedUBound(1:fieldUngriddedDimCount) = ungriddedUBound(1:fieldUngriddedDimCount) @\
          else  @\
            ! No user info copy array bounds @\
            ! Note: because of 'target' attribute bounds will be 1...size @\
            do i=1, fieldUngriddedDimCount        @\
               localUngriddedLBound(i) = lbound(farray,ungriddedIndex(i)) @\
               localUngriddedUBound(i) = ubound(farray,ungriddedIndex(i)) @\
           enddo   @\
         endif @\
       endif @\
@\
       ! Get computationalEdgeWidths                                        @\
       call ESMF_GridGet(grid, staggerloc=localStaggerloc, & @\
             computationalEdgeLWidth=compELWidth, computationalEdgeUWidth=compEUWidth, & @\
             rc=localrc)                                                   @\
          if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &              @\
             ESMF_CONTEXT, rcToReturn=rc)) return                          @\
@\
@\
       ! if there are undistributed dimensions then get that info @\
       if (fieldUndistDimCount>0) then @\
          ! The undistributed info from the Grid needs to be @\
          ! combined with the ungridded info from the Field in order @\
          ! to create the Array for the Field. @\
          call ESMF_GridGetArrayUndistInfo(grid, & @\
               staggerloc=localStaggerLoc, & @\
               gridToArrayMap=gridToFieldMap, & @\
               ungriddedLBound=localUngriddedLBound (1:fieldUngriddedDimCount), & @\
               ungriddedUBound=localUngriddedUBound (1:fieldUngriddedDimCount), & @\
               distgridToArrayMap=distgridToArrayMap, & @\
               undistLBound=undistLBound, undistUBound=undistUBound, & @\
               rc=localrc) @\
          if (ESMF_LogMsgFoundError(localrc, & @\
              ESMF_ERR_PASSTHRU, & @\
              ESMF_CONTEXT, rc)) return @\
@\
          ! Create Array with undistributed dimensions                                    @\
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
        else ! else just use distributed map from grid @\
          ! Get map between distributed dimensions                                   @\
          call ESMF_GridGet(grid, distgridToGridMap=distgridToArrayMap, rc=localrc) @\
                 if (ESMF_LogMsgFoundError(localrc, & @\
                     ESMF_ERR_PASSTHRU, & @\
                     ESMF_CONTEXT, rc)) return @\
@\
          ! Create Array with only distributed dimensions                                    @\
          array = ESMF_ArrayCreate(farray, distgrid=distgrid, & @\
                  distgridToArrayMap=distgridToArrayMap (1:gridDistDimCount), & @\
                  computationalEdgeLWidth=compELWidth(1:gridDistDimCount), & @\
                  computationalEdgeUWidth=compEUWidth(1:gridDistDimCount), & @\
                  totalLWidth=localMaxHaloLWidth(1:gridDistDimCount), & @\
                  totalUWidth=localMaxHaloUWidth(1:gridDistDimCount), & @\
                  staggerloc=localStaggerLoc%staggerloc, rc=localrc) @\
                 if (ESMF_LogMsgFoundError(localrc, & @\
                         ESMF_ERR_PASSTHRU, & @\
                         ESMF_CONTEXT, rc)) return @\
@\
     endif @\
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
     ! Should call a common FieldSetCommitConstructor here instead of just setting things up ourselves @\
     ! (The field Sets were all moved here in preparation for this) @\
     field%ftypep%staggerloc = localStaggerLoc @\
     field%ftypep%gridToFieldMap(1:gridDimCount) = localGridToFieldMap(1:gridDimCount) @\
     field%ftypep%maxHaloLWidth(1:gridDistDimCount) = localMaxHaloLWidth (1:gridDistDimCount) @\
     field%ftypep%maxHaloUWidth(1:gridDistDimCount) = localMaxHaloUWidth (1:gridDistDimCount) @\
     field%ftypep%ungriddedLBound(1:fieldUngriddedDimCount) =localUngriddedLBound(1:fieldUngriddedDimCount) @\
     field%ftypep%ungriddedUBound(1:fieldUngriddedDimCount) =localUngriddedUBound(1:fieldUngriddedDimCount) @\
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
!BOPI @\
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
!           Pointer to an {\tt ESMF\_Grid} object.  @\
!     \item [farray] @\
!           Native fortran data array to be copied/referenced in Field @\
!   \item [copyflag] @\
!           Whether to copy the existing data space or reference directly. Valid @\
!           values are {\tt ESMF\_DATA\_COPY} or {\tt ESMF\_DATA\_REF} (default). @\
!     \item [{[staggerloc]}] @\
!           Stagger location of data in grid cells.  For valid  @\
!           predefined values see Section \ref{sec:opt:staggerloc}. @\
!           To create a custom stagger location see Section @\
!           \ref{sec:usage:staggerloc:adv}. @\
!     \item [{[gridToFieldMap]}] @\
!           List that contains as many elements as is indicated by the {\tt grid}'s rank.  @\
!           The list elements map each dimension of the Grid object to a dimension in the @\
!           Field's Array by specifying the appropriate Array dimension index. The default is to @\
!           map all of the grid's dimensions against the lower dimensions of the Field's @\
!           Array in sequence, i.e. gridDimmap = (/1, 2, .../). Unmapped dimensions are @\
!           undistributed dimensions.  The total undistributed dimensions are the total  @\
!           Array dimensions - the distributed dimensions in the Grid (distRank).  All @\
!           gridToFieldMap entries must be greater than or equal to one and smaller than or equal @\
!           to the Array rank. It is erroneous to specify the same entry multiple times @\
!           unless it is zero.  If the Array rank is less than the Grid dimCount then @\
!           the default gridToFieldMap will contain zeros for the dimCount. @\
!           A zero entry in the dimmap indicates that the particular Grid dimension will @\
!           be replicating the Array across the DEs along this direction. @\
!     \item [{[ungriddedLBound]}] @\
!           Lower bounds of the ungridded dimensions of the Field. Defaults to 1. @\
!     \item [{[ungriddedUBound]}] @\
!           Upper bounds of the ungridded dimensions of the Field. @\
!     \item [{[maxHaloLWidth]}] @\
!           Lower bound of halo region.  Defaults to 0. ! NOT IMPLEMENTED @\
!     \item [{[maxHaloUWidth]}] @\
!           Upper bound of halo region.  Defaults to 0. ! NOT IMPLEMENTED @\
!     \item [{[name]}]  @\
!           {\tt Field} name.  @\
!     \item [{[iospec]}]  @\
!           I/O specification. ! NOT IMPLEMENTED @\
!     \item [{[rc]}]  @\
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. @\
!     \end{description} @\
! @\
!EOPI @\

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

