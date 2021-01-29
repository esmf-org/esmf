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
!
#define ESMF_FILENAME "ESMF_FieldSMM.F90"
!
!   ESMF Field Communications SMM module
module ESMF_FieldSMMMod
!
!==============================================================================
!
!
!------------------------------------------------------------------------------
! INCLUDES
#include "ESMF.h"
!------------------------------------------------------------------------------
!
!BOPI
! !MODULE: ESMF_FieldSMMMod - FieldSMM routines for Field objects
!
! !DESCRIPTION:
! The code in this file implements the {\tt ESMF\_FieldSMM} subroutine.
!
!EOPI
!------------------------------------------------------------------------------
! !USES:
    use ESMF_UtilTypesMod
    use ESMF_InitMacrosMod
    use ESMF_LogErrMod
    use ESMF_VMMod
    use ESMF_FieldMod
    use ESMF_FieldGetMod
    use ESMF_ArrayMod
    use ESMF_RHandleMod
    use ESMF_FactorReadMod ! Read weight factors from netCDF file.
    implicit none
    private
!------------------------------------------------------------------------------
! !PRIVATE TYPES:
!  <none>
!------------------------------------------------------------------------------
! !PUBLIC TYPES:
!  <none>
!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:
    public ESMF_FieldSMMStore
    public ESMF_FieldSMM
    public ESMF_FieldSMMRelease
!
!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
    character(*), parameter, private :: version = &
      '$Id$'

!------------------------------------------------------------------------------
    interface ESMF_FieldSMMStore
        module procedure ESMF_FieldSMMStoreI4
        module procedure ESMF_FieldSMMStoreI8
        module procedure ESMF_FieldSMMStoreR4
        module procedure ESMF_FieldSMMStoreR8
        module procedure ESMF_FieldSMMStoreI4TR
        module procedure ESMF_FieldSMMStoreI8TR
        module procedure ESMF_FieldSMMStoreR4TR
        module procedure ESMF_FieldSMMStoreR8TR
        module procedure ESMF_FieldSMMStoreNF
        module procedure ESMF_FieldSMMStoreNFTR
        module procedure ESMF_FieldSMMStoreFromFile
        module procedure ESMF_FieldSMMStoreFromFileTR
    end interface
!------------------------------------------------------------------------------
contains

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldSMM()"
!BOP
! !IROUTINE: ESMF_FieldSMM - Execute a Field sparse matrix multiplication
!
! !INTERFACE:
  subroutine ESMF_FieldSMM(srcField, dstField, routehandle, keywordEnforcer, &
             zeroregion, termorderflag, checkflag, rc)
!
! !ARGUMENTS:
        type(ESMF_Field),          intent(in),    optional  :: srcField
        type(ESMF_Field),          intent(inout), optional  :: dstField
        type(ESMF_RouteHandle),    intent(inout)            :: routehandle
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
        type(ESMF_Region_Flag),    intent(in),    optional  :: zeroregion
        type(ESMF_TermOrder_Flag), intent(in),    optional  :: termorderflag
        logical,                   intent(in),    optional  :: checkflag
        integer,                   intent(out),   optional  :: rc
!
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \begin{description}
! \item[6.1.0] Added argument {\tt termorderflag}.
!              The new argument gives the user control over the order in which
!              the src terms are summed up.
! \end{description}
! \end{itemize}
!
! !DESCRIPTION:
!   \begin{sloppypar}
!   Execute a precomputed Field sparse matrix multiplication from {\tt srcField} to
!   {\tt dstField}. 
!   Both {\tt srcField} and {\tt dstField} must match the respective Fields
!   used during {\tt ESMF\_FieldSMMStore()} in {\em type}, {\em kind}, and 
!   memory layout of the {\em gridded} dimensions. However, the size, number, 
!   and index order of {\em ungridded} dimensions may be different. See section
!   \ref{RH:Reusability} for a more detailed discussion of RouteHandle 
!   reusability.
!   \end{sloppypar}
!
!   The {\tt srcField} and {\tt dstField} arguments are optional in support of
!   the situation where {\tt srcField} and/or {\tt dstField} are not defined on
!   all PETs. The {\tt srcField} and {\tt dstField} must be specified on those
!   PETs that hold source or destination DEs, respectively, but may be omitted
!   on all other PETs. PETs that hold neither source nor destination DEs may
!   omit both arguments.
!
!   It is erroneous to specify the identical Field object for {\tt srcField} and
!   {\tt dstField} arguments.
!
!   See {\tt ESMF\_FieldSMMStore()} on how to precompute 
!   {\tt routehandle}.
!
!   This call is {\em collective} across the current VM.
!
!   For examples and associated documentation regarding this method see Section
!   \ref{sec:field:usage:smm_1dptr}. 
!
!   \begin{description}
!   \item [{[srcField]}]
!     {\tt ESMF\_Field} with source data.
!   \item [{[dstField]}]
!     {\tt ESMF\_Field} with destination data.
!   \item [routehandle]
!     Handle to the precomputed Route.
!   \item [{[zeroregion]}]
!     \begin{sloppypar}
!     If set to {\tt ESMF\_REGION\_TOTAL} {\em (default)} the total regions of
!     all DEs in {\tt dstField} will be initialized to zero before updating the 
!     elements with the results of the sparse matrix multiplication. If set to
!     {\tt ESMF\_REGION\_EMPTY} the elements in {\tt dstField} will not be
!     modified prior to the sparse matrix multiplication and results will be
!     added to the incoming element values. Setting {\tt zeroregion} to 
!     {\tt ESMF\_REGION\_SELECT} will only zero out those elements in the 
!     destination Field that will be updated by the sparse matrix
!     multiplication. See section \ref{const:region} for a complete list of
!     valid settings.
!     \end{sloppypar}
!   \item [{[termorderflag]}]
!     Specifies the order of the source side terms in all of the destination
!     sums. The {\tt termorderflag} only affects the order of terms during
!     the execution of the RouteHandle. See the \ref{RH:bfb} section for an
!     in-depth discussion of {\em all} bit-for-bit reproducibility
!     aspects related to route-based communication methods.
!     See \ref{const:termorderflag} for a full list of options.
!     The default is {\tt ESMF\_TERMORDER\_FREE}, allowing maximum flexibility
!     in the order of terms for optimum performance.
!   \item [{[checkflag]}]
!     If set to {\tt .TRUE.} the input Field pair will be checked for
!     consistency with the precomputed operation provided by {\tt routehandle}.
!     If set to {\tt .FALSE.} {\em (default)} only a very basic input check
!     will be performed, leaving many inconsistencies undetected. Set
!     {\tt checkflag} to {\tt .FALSE.} to achieve highest performance.
!   \item [{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
        integer                 :: localrc      ! local return code
        
        ! local variables to buffer optional arguments
        type(ESMF_Array)        :: l_srcArray ! helper variable
        type(ESMF_Array)        :: l_dstArray ! helper variable

        ! initialize return code; assume routine not implemented
        localrc = ESMF_RC_NOT_IMPL
        if (present(rc)) rc = ESMF_RC_NOT_IMPL

        ! Check init status of arguments, deal with optional Field args
        ESMF_INIT_CHECK_DEEP(ESMF_RouteHandleGetInit, routehandle, rc)
        ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit, srcField, rc)
        ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit, dstField, rc)

        if (present(srcField)) then
          call ESMF_FieldGet(srcField, array=l_srcArray, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        else
          call ESMF_ArraySetThisNull(l_srcArray, localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        endif

        if (present(dstField)) then
          call ESMF_FieldGet(dstField, array=l_dstArray, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        else
          call ESMF_ArraySetThisNull(l_dstArray, localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        endif
        
        ! perform Field sparse matrix multiplication through internal array
        call ESMF_ArraySMM(srcArray=l_srcArray, dstArray=l_dstArray, &
          routehandle=routehandle, zeroregion=zeroregion, &
          termorderflag=termorderflag, checkflag=checkflag, &
          rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
        
        ! return successfully
        if (present(rc)) rc = ESMF_SUCCESS

    end subroutine ESMF_FieldSMM

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldSMMRelease()"
!BOP
! !IROUTINE: ESMF_FieldSMMRelease - Release resources associated with Field 
! sparse matrix multiplication
!
! !INTERFACE:
  subroutine ESMF_FieldSMMRelease(routehandle, keywordEnforcer, noGarbage, rc)
!
! !ARGUMENTS:
        type(ESMF_RouteHandle), intent(inout)           :: routehandle
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
        logical,                intent(in),   optional  :: noGarbage
        integer,                intent(out),  optional  :: rc
!
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \item\apiStatusModifiedSinceVersion{5.2.0r}
! \begin{description}
! \item[8.0.0] Added argument {\tt noGarbage}.
!   The argument provides a mechanism to override the default garbage collection
!   mechanism when destroying an ESMF object.
! \end{description}
! \end{itemize}
!
! !DESCRIPTION:
!   Release resources associated with a Field sparse matrix multiplication. After this call
!   {\tt routehandle} becomes invalid.
!
!   \begin{description}
!   \item [routehandle]
!     Handle to the precomputed Route.
!   \item[{[noGarbage]}]
!     If set to {\tt .TRUE.} the object will be fully destroyed and removed
!     from the ESMF garbage collection system. Note however that under this 
!     condition ESMF cannot protect against accessing the destroyed object 
!     through dangling aliases -- a situation which may lead to hard to debug 
!     application crashes.
! 
!     It is generally recommended to leave the {\tt noGarbage} argument
!     set to {\tt .FALSE.} (the default), and to take advantage of the ESMF 
!     garbage collection system which will prevent problems with dangling
!     aliases or incorrect sequences of destroy calls. However this level of
!     support requires that a small remnant of the object is kept in memory
!     past the destroy call. This can lead to an unexpected increase in memory
!     consumption over the course of execution in applications that use 
!     temporary ESMF objects. For situations where the repeated creation and 
!     destruction of temporary objects leads to memory issues, it is 
!     recommended to call with {\tt noGarbage} set to {\tt .TRUE.}, fully 
!     removing the entire temporary object from memory.
!   \item [{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
        integer                 :: localrc      ! local return code

        ! initialize return code; assume routine not implemented
        localrc = ESMF_RC_NOT_IMPL
        if (present(rc)) rc = ESMF_RC_NOT_IMPL

        ! Check init status of arguments
        ESMF_INIT_CHECK_DEEP(ESMF_RouteHandleGetInit, routehandle, rc)
            
        ! Call into the RouteHandle code
        call ESMF_RouteHandleRelease(routehandle, noGarbage=noGarbage, &
          rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
        
        ! return successfully
        if (present(rc)) rc = ESMF_SUCCESS

    end subroutine ESMF_FieldSMMRelease

!---------------------------------------------------------------------------- 
!BOP 
! !IROUTINE: ESMF_FieldSMMStore - Precompute Field sparse matrix multiplication with local factors
! 
! !INTERFACE: 
! ! Private name; call using ESMF_FieldSMMStore() 
! subroutine ESMF_FieldSMMStore<type><kind>(srcField, dstField, & 
!        routehandle, factorList, factorIndexList, keywordEnforcer, &
!        ignoreUnmatchedIndices, srcTermProcessing, pipelineDepth, rc)
! 
! !ARGUMENTS: 
!   type(ESMF_Field),         intent(in)              :: srcField  
!   type(ESMF_Field),         intent(inout)           :: dstField  
!   type(ESMF_RouteHandle),   intent(inout)           :: routehandle
!   <type>(ESMF_KIND_<kind>), intent(in)              :: factorList(:) 
!   integer,                  intent(in),             :: factorIndexList(:,:) 
!   type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
!   logical,                  intent(in),    optional :: ignoreUnmatchedIndices
!   integer,                  intent(inout), optional :: srcTermProcessing
!   integer,                  intent(inout), optional :: pipeLineDepth
!   integer,                  intent(out),   optional :: rc
! 
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \item\apiStatusModifiedSinceVersion{5.2.0r}
! \begin{description}
! \item[6.1.0] Added arguments {\tt srcTermProcessing}, {\tt pipelineDepth}
!              The two arguments {\tt srcTermProcessing} and {\tt pipelineDepth}
!              provide access to the tuning parameters affecting the sparse matrix
!              execution. 
! \item[7.0.0] Added argument {\tt transposeRoutehandle} to allow a handle to
!              the transposed matrix operation to be returned.\newline
!              Added argument {\tt ignoreUnmatchedIndices} to support sparse 
!              matrices that contain elements with indices that do not have a
!              match within the source or destination Array.
! \item[7.1.0r] Removed argument {\tt transposeRoutehandle} and provide it
!              via interface overloading instead. This allows argument 
!              {\tt srcField} to stay strictly intent(in) for this entry point.
! \end{description}
! \end{itemize}
!
! !DESCRIPTION: 
! 
! \begin{sloppypar}
! Store a Field sparse matrix multiplication operation from {\tt srcField}
! to {\tt dstField}. PETs that specify non-zero matrix coefficients must use
! the <type><kind> overloaded interface and provide the {\tt factorList} and
! {\tt factorIndexList} arguments. Providing {\tt factorList} and
! {\tt factorIndexList} arguments with {\tt size(factorList) = (/0/)} and
! {\tt size(factorIndexList) = (/2,0/)} or {\tt (/4,0/)} indicates that a 
! PET does not provide matrix elements. Alternatively, PETs that do not 
! provide matrix elements may also call into the overloaded interface
! {\em without} {\tt factorList} and {\tt factorIndexList} arguments.
! \end{sloppypar}
!  
! Both {\tt srcField} and {\tt dstField} are interpreted as sequentialized 
! vectors. The 
! sequence is defined by the order of DistGrid dimensions and the order of 
! tiles within the DistGrid or by user-supplied arbitrary sequence indices. See 
! section \ref{Array:SparseMatMul} for details on the definition of {\em sequence indices}. 
! SMM corresponds to an identity mapping of the source Field vector to 
! the destination Field vector. 
!  
! Source and destination Fields may be of different <type><kind>. Further source 
! and destination Fields may differ in shape, however, the number of elements 
! must match. 
!  
! It is erroneous to specify the identical Field object for srcField and dstField 
! arguments.
!
!   The routine returns an {\tt ESMF\_RouteHandle} that can be used to call 
!   {\tt ESMF\_FieldSMM()} on any pair of Fields that matches 
!   {\tt srcField} and {\tt dstField} in {\em type}, {\em kind}, and 
!   memory layout of the {\em gridded} dimensions. However, the size, number, 
!   and index order of {\em ungridded} dimensions may be different. See section
!   \ref{RH:Reusability} for a more detailed discussion of RouteHandle 
!   reusability.
!  
! This method is overloaded for:\newline
! {\tt ESMF\_TYPEKIND\_I4}, {\tt ESMF\_TYPEKIND\_I8},\newline 
! {\tt ESMF\_TYPEKIND\_R4}, {\tt ESMF\_TYPEKIND\_R8}.
! \newline
!  
! This call is collective across the current VM.  
! 
! For examples and associated documentation regarding this method see Section
! \ref{sec:field:usage:smm_1dptr}. 
! 
! The arguments are:
!
! \begin{description}
!
! \item [srcField]
!       {\tt ESMF\_Field} with source data.
!
! \item [dstField]
!       {\tt ESMF\_Field} with destination data. The data in this Field may be
!     destroyed by this call.
!
! \item [routehandle]
!       Handle to the precomputed Route.
!
! \item [factorList]
!       List of non-zero coefficients.
!
! \item [factorIndexList]
!     Pairs of sequence indices for the factors stored in {\tt factorList}.
!
!     \begin{sloppypar}
!     The second dimension of {\tt factorIndexList} steps through the list of
!     pairs, i.e. {\tt size(factorIndexList,2) == size(factorList)}. The first
!     dimension of {\tt factorIndexList} is either of size 2 or size 4.
!     \end{sloppypar}
!     The second dimension of {\tt factorIndexList} steps through the list of
!
!     In the {\em size 2 format} {\tt factorIndexList(1,:)} specifies the
!     sequence index of the source element in the {\tt srcField} while
!     {\tt factorIndexList(2,:)} specifies the sequence index of the
!     destination element in {\tt dstField}. For this format to be a valid
!     option source and destination Fields must have matching number of
!     tensor elements (the product of the sizes of all Field tensor dimensions).
!     Under this condition an identity matrix can be applied within the space of
!     tensor elements for each sparse matrix factor.
!
!     \begin{sloppypar}
!     The {\em size 4 format} is more general and does not require a matching
!     tensor element count. Here the {\tt factorIndexList(1,:)} specifies the
!     sequence index while {\tt factorIndexList(2,:)} specifies the tensor
!     sequence index of the source element in the {\tt srcField}. Further
!     {\tt factorIndexList(3,:)} specifies the sequence index and
!     {\tt factorIndexList(4,:)} specifies the tensor sequence index of the 
!     destination element in the {\tt dstField}.
!     \end{sloppypar}
!
!     See section \ref{Array:SparseMatMul} for details on the definition of 
!     Field {\em sequence indices} and {\em tensor sequence indices}.
!
!   \item [{[ignoreUnmatchedIndices]}]
!     A logical flag that affects the behavior for when sequence indices 
!     in the sparse matrix are encountered that do not have a match on the 
!     {\tt srcField} or {\tt dstField} side. The default setting is 
!     {\tt .false.}, indicating that it is an error when such a situation is 
!     encountered. Setting {\tt ignoreUnmatchedIndices} to {\tt .true.} ignores
!     entries with unmatched indices.
!
!   \item [{[srcTermProcessing]}]
!     The {\tt srcTermProcessing} parameter controls how many source terms,
!     located on the same PET and summing into the same destination element,
!     are summed into partial sums on the source PET before being transferred
!     to the destination PET. A value of 0 indicates that the entire arithmetic
!     is done on the destination PET; source elements are neither multiplied
!     by their factors nor added into partial sums before being sent off by the
!     source PET. A value of 1 indicates that source elements are multiplied
!     by their factors on the source side before being sent to the destination
!     PET. Larger values of {\tt srcTermProcessing} indicate the maximum number
!     of terms in the partial sums on the source side.
!
!     Note that partial sums may lead to bit-for-bit differences in the results.
!     See section \ref{RH:bfb} for an in-depth discussion of {\em all}
!     bit-for-bit reproducibility aspects related to route-based communication
!     methods.
!
!     \begin{sloppypar}
!     The {\tt ESMF\_FieldSMMStore()} method implements an auto-tuning scheme
!     for the {\tt srcTermProcessing} parameter. The intent on the
!     {\tt srcTermProcessing} argument is "{\tt inout}" in order to
!     support both overriding and accessing the auto-tuning parameter.
!     If an argument $>= 0$ is specified, it is used for the
!     {\tt srcTermProcessing} parameter, and the auto-tuning phase is skipped.
!     In this case the {\tt srcTermProcessing} argument is not modified on
!     return. If the provided argument is $< 0$, the {\tt srcTermProcessing}
!     parameter is determined internally using the auto-tuning scheme. In this
!     case the {\tt srcTermProcessing} argument is re-set to the internally
!     determined value on return. Auto-tuning is also used if the optional
!     {\tt srcTermProcessing} argument is omitted.
!     \end{sloppypar}
!
!   \item [{[pipelineDepth]}]
!     The {\tt pipelineDepth} parameter controls how many messages a PET
!     may have outstanding during a sparse matrix exchange. Larger values
!     of {\tt pipelineDepth} typically lead to better performance. However,
!     on some systems too large a value may lead to performance degradation,
!     or runtime errors.
!
!     Note that the pipeline depth has no effect on the bit-for-bit
!     reproducibility of the results. However, it may affect the performance
!     reproducibility of the exchange.
!
!     The {\tt ESMF\_FieldSMMStore()} method implements an auto-tuning scheme
!     for the {\tt pipelineDepth} parameter. The intent on the
!     {\tt pipelineDepth} argument is "{\tt inout}" in order to
!     support both overriding and accessing the auto-tuning parameter.
!     If an argument $>= 0$ is specified, it is used for the
!     {\tt pipelineDepth} parameter, and the auto-tuning phase is skipped.
!     In this case the {\tt pipelineDepth} argument is not modified on
!     return. If the provided argument is $< 0$, the {\tt pipelineDepth}
!     parameter is determined internally using the auto-tuning scheme. In this
!     case the {\tt pipelineDepth} argument is re-set to the internally
!     determined value on return. Auto-tuning is also used if the optional
!     {\tt pipelineDepth} argument is omitted.
!
!   \item [{[rc]}]  
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!
! \end{description} 
! 
!EOP 
!---------------------------------------------------------------------------- 

#undef  ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldSMMStoreI4" 
!BOPI
! !IROUTINE: ESMF_FieldSMMStore - Precompute Field sparse matrix multiplication
!
! !INTERFACE:
  ! Private name; call using ESMF_FieldSMMStore()
    subroutine ESMF_FieldSMMStoreI4(srcField, dstField, & 
        routehandle, factorList, factorIndexList, keywordEnforcer, &
        ignoreUnmatchedIndices, srcTermProcessing, &
        pipeLineDepth, rc) 

        ! input arguments 
        type(ESMF_Field),       intent(in)              :: srcField  
        type(ESMF_Field),       intent(inout)           :: dstField  
        type(ESMF_RouteHandle), intent(inout)           :: routehandle
        integer(ESMF_KIND_I4),  intent(in)              :: factorList(:)
        integer,                intent(in)              :: factorIndexList(:,:) 
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
        logical,                intent(in),    optional :: ignoreUnmatchedIndices
        integer,                intent(inout), optional :: srcTermProcessing
        integer,                intent(inout), optional :: pipeLineDepth
        integer,                intent(out),   optional :: rc 

!EOPI
        ! local variables as temporary input/output arguments 

        ! internal local variables 
        integer                                     :: localrc 
        type(ESMF_Array)                            :: srcArray, dstArray   

        ! Initialize return code; assume routine not implemented 
        localrc = ESMF_RC_NOT_IMPL 
        if(present(rc)) rc = ESMF_RC_NOT_IMPL 

        ! check variable: focus on field and farray 
        ! rely on ArraySMM to check the sanity of other variables 
        ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit, srcField, rc) 
        ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit, dstField, rc) 

        ! Retrieve source and destination arrays. 
        call ESMF_FieldGet(srcField, array=srcArray, rc=localrc) 
        if (ESMF_LogFoundError(localrc, & 
            ESMF_ERR_PASSTHRU, & 
            ESMF_CONTEXT, rcToReturn=rc)) return 
        call ESMF_FieldGet(dstField, array=dstArray, rc=localrc) 
        if (ESMF_LogFoundError(localrc, & 
            ESMF_ERR_PASSTHRU, & 
            ESMF_CONTEXT, rcToReturn=rc)) return 

        ! perform sparse matrix multiplication through array 
        ! For performance consideration: 
        ! Rely on ArraySMM to perform sanity checking of the other parameters 
        call ESMF_ArraySMMStore(srcArray=srcArray, dstArray=dstArray, &
          routehandle=routehandle, factorList=factorList, &
          factorIndexList=factorIndexList, &
          ignoreUnmatchedIndices=ignoreUnmatchedIndices, &
          srcTermProcessing=srcTermProcessing, pipelineDepth=pipelineDepth, &
          rc=localrc)
        if (ESMF_LogFoundError(localrc, & 
            ESMF_ERR_PASSTHRU, & 
            ESMF_CONTEXT, rcToReturn=rc)) return 

        if (present(rc)) rc = ESMF_SUCCESS 
    end subroutine ESMF_FieldSMMStoreI4
!------------------------------------------------------------------------------ 

#undef  ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldSMMStoreI8" 
!BOPI
! !IROUTINE: ESMF_FieldSMMStore - Precompute Field sparse matrix multiplication
!
! !INTERFACE:
  ! Private name; call using ESMF_FieldSMMStore()
    subroutine ESMF_FieldSMMStoreI8(srcField, dstField, & 
        routehandle, factorList, factorIndexList, keywordEnforcer, &
        ignoreUnmatchedIndices, srcTermProcessing, &
        pipeLineDepth, rc)

        ! input arguments 
        type(ESMF_Field),       intent(in)              :: srcField  
        type(ESMF_Field),       intent(inout)           :: dstField  
        type(ESMF_RouteHandle), intent(inout)           :: routehandle
        integer(ESMF_KIND_I8),  intent(in)              :: factorList(:)
        integer,                intent(in)              :: factorIndexList(:,:) 
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
        logical,                intent(in),    optional :: ignoreUnmatchedIndices
        integer,                intent(inout), optional :: srcTermProcessing
        integer,                intent(inout), optional :: pipeLineDepth
        integer,                intent(out),   optional :: rc 

!EOPI
        ! local variables as temporary input/output arguments 

        ! internal local variables 
        integer                                     :: localrc 
        type(ESMF_Array)                            :: srcArray, dstArray   

        ! Initialize return code; assume routine not implemented 
        localrc = ESMF_RC_NOT_IMPL 
        if(present(rc)) rc = ESMF_RC_NOT_IMPL 

        ! check variable: focus on field and farray 
        ! rely on ArraySMM to check the sanity of other variables 
        ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit, srcField, rc) 
        ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit, dstField, rc) 

        ! Retrieve source and destination arrays. 
        call ESMF_FieldGet(srcField, array=srcArray, rc=localrc) 
        if (ESMF_LogFoundError(localrc, & 
            ESMF_ERR_PASSTHRU, & 
            ESMF_CONTEXT, rcToReturn=rc)) return 
        call ESMF_FieldGet(dstField, array=dstArray, rc=localrc) 
        if (ESMF_LogFoundError(localrc, & 
            ESMF_ERR_PASSTHRU, & 
            ESMF_CONTEXT, rcToReturn=rc)) return 

        ! perform sparse matrix multiplication through array 
        ! For performance consideration: 
        ! Rely on ArraySMM to perform sanity checking of the other parameters 
        call ESMF_ArraySMMStore(srcArray=srcArray, dstArray=dstArray, &
          routehandle=routehandle, factorList=factorList, & 
          factorIndexList=factorIndexList, &
          ignoreUnmatchedIndices=ignoreUnmatchedIndices, &
          srcTermProcessing=srcTermProcessing, pipelineDepth=pipelineDepth, &
          rc=localrc)
        if (ESMF_LogFoundError(localrc, & 
            ESMF_ERR_PASSTHRU, & 
            ESMF_CONTEXT, rcToReturn=rc)) return 

        if (present(rc)) rc = ESMF_SUCCESS 
    end subroutine ESMF_FieldSMMStoreI8
!------------------------------------------------------------------------------ 

#undef  ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldSMMStoreR4"
!BOPI
! !IROUTINE: ESMF_FieldSMMStore - Precompute Field sparse matrix multiplication
!
! !INTERFACE:
  ! Private name; call using ESMF_FieldSMMStore()
    subroutine ESMF_FieldSMMStoreR4(srcField, dstField, & 
        routehandle, factorList, factorIndexList, keywordEnforcer, &
        ignoreUnmatchedIndices, srcTermProcessing, &
        pipeLineDepth, rc)

        ! input arguments 
        type(ESMF_Field),       intent(in)              :: srcField  
        type(ESMF_Field),       intent(inout)           :: dstField  
        type(ESMF_RouteHandle), intent(inout)           :: routehandle
        real(ESMF_KIND_R4),     intent(in)              :: factorList(:)
        integer,                intent(in)              :: factorIndexList(:,:) 
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
        logical,                intent(in),    optional :: ignoreUnmatchedIndices
        integer,                intent(inout), optional :: srcTermProcessing
        integer,                intent(inout), optional :: pipeLineDepth
        integer,                intent(out),   optional :: rc 

!EOPI
        ! local variables as temporary input/output arguments 

        ! internal local variables 
        integer                                     :: localrc 
        type(ESMF_Array)                            :: srcArray, dstArray   

        ! Initialize return code; assume routine not implemented 
        localrc = ESMF_RC_NOT_IMPL 
        if(present(rc)) rc = ESMF_RC_NOT_IMPL 

        ! check variable: focus on field and farray 
        ! rely on ArraySMM to check the sanity of other variables 
        ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit, srcField, rc) 
        ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit, dstField, rc) 

        ! Retrieve source and destination arrays. 
        call ESMF_FieldGet(srcField, array=srcArray, rc=localrc) 
        if (ESMF_LogFoundError(localrc, & 
            ESMF_ERR_PASSTHRU, & 
            ESMF_CONTEXT, rcToReturn=rc)) return 
        call ESMF_FieldGet(dstField, array=dstArray, rc=localrc) 
        if (ESMF_LogFoundError(localrc, & 
            ESMF_ERR_PASSTHRU, & 
            ESMF_CONTEXT, rcToReturn=rc)) return 

        ! perform sparse matrix multiplication through array 
        ! For performance consideration: 
        ! Rely on ArraySMM to perform sanity checking of the other parameters 
        call ESMF_ArraySMMStore(srcArray=srcArray, dstArray=dstArray, &
          routehandle=routehandle, factorList=factorList, & 
          factorIndexList=factorIndexList, &
          ignoreUnmatchedIndices=ignoreUnmatchedIndices, &
          srcTermProcessing=srcTermProcessing, pipelineDepth=pipelineDepth, &
          rc=localrc)
        if (ESMF_LogFoundError(localrc, & 
            ESMF_ERR_PASSTHRU, & 
            ESMF_CONTEXT, rcToReturn=rc)) return 

        if (present(rc)) rc = ESMF_SUCCESS 
    end subroutine ESMF_FieldSMMStoreR4
!------------------------------------------------------------------------------ 

#undef  ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldSMMStoreR8"
!BOPI
! !IROUTINE: ESMF_FieldSMMStore - Precompute Field sparse matrix multiplication
!
! !INTERFACE:
  ! Private name; call using ESMF_FieldSMMStore()
    subroutine ESMF_FieldSMMStoreR8(srcField, dstField, & 
        routehandle, factorList, factorIndexList, keywordEnforcer, &
        ignoreUnmatchedIndices, srcTermProcessing, &
        pipeLineDepth, rc)

        ! input arguments 
        type(ESMF_Field),       intent(in)              :: srcField  
        type(ESMF_Field),       intent(inout)           :: dstField  
        type(ESMF_RouteHandle), intent(inout)           :: routehandle
        real(ESMF_KIND_R8),     intent(in)              :: factorList(:)
        integer,                intent(in)              :: factorIndexList(:,:) 
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
        logical,                intent(in),    optional :: ignoreUnmatchedIndices
        integer,                intent(inout), optional :: srcTermProcessing
        integer,                intent(inout), optional :: pipeLineDepth
        integer,                intent(out),   optional :: rc 

!EOPI
        ! local variables as temporary input/output arguments 

        ! internal local variables 
        integer                                     :: localrc 
        type(ESMF_Array)                            :: srcArray, dstArray   

        ! Initialize return code; assume routine not implemented 
        localrc = ESMF_RC_NOT_IMPL 
        if(present(rc)) rc = ESMF_RC_NOT_IMPL 

        ! check variable: focus on field and farray 
        ! rely on ArraySMM to check the sanity of other variables 
        ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit, srcField, rc) 
        ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit, dstField, rc) 

        ! Retrieve source and destination arrays. 
        call ESMF_FieldGet(srcField, array=srcArray, rc=localrc) 
        if (ESMF_LogFoundError(localrc, & 
            ESMF_ERR_PASSTHRU, & 
            ESMF_CONTEXT, rcToReturn=rc)) return 
        call ESMF_FieldGet(dstField, array=dstArray, rc=localrc) 
        if (ESMF_LogFoundError(localrc, & 
            ESMF_ERR_PASSTHRU, & 
            ESMF_CONTEXT, rcToReturn=rc)) return 

        ! perform sparse matrix multiplication through array 
        ! For performance consideration: 
        ! Rely on ArraySMM to perform sanity checking of the other parameters 
        call ESMF_ArraySMMStore(srcArray=srcArray, dstArray=dstArray, &
          routehandle=routehandle, factorList=factorList, & 
          factorIndexList=factorIndexList, &
          ignoreUnmatchedIndices=ignoreUnmatchedIndices, &
          srcTermProcessing=srcTermProcessing, pipelineDepth=pipelineDepth, &
          rc=localrc)
        if (ESMF_LogFoundError(localrc, & 
            ESMF_ERR_PASSTHRU, & 
            ESMF_CONTEXT, rcToReturn=rc)) return 

        if (present(rc)) rc = ESMF_SUCCESS 
    end subroutine ESMF_FieldSMMStoreR8
!------------------------------------------------------------------------------ 

!---------------------------------------------------------------------------- 
!BOP 
! !IROUTINE: ESMF_FieldSMMStore - Precompute Field sparse matrix multiplication and transpose with local factors
! 
! !INTERFACE: 
! ! Private name; call using ESMF_FieldSMMStore() 
! subroutine ESMF_FieldSMMStore<type><kind>TR(srcField, dstField, & 
!        routehandle, transposeRoutehandle, factorList, factorIndexList, &
!        keywordEnforcer, ignoreUnmatchedIndices, srcTermProcessing, &
!        pipelineDepth, rc)
! 
! !ARGUMENTS: 
!   type(ESMF_Field),         intent(inout)           :: srcField  
!   type(ESMF_Field),         intent(inout)           :: dstField  
!   type(ESMF_RouteHandle),   intent(inout)           :: routehandle
!   type(ESMF_RouteHandle),   intent(inout)           :: transposeRoutehandle
!   <type>(ESMF_KIND_<kind>), intent(in)              :: factorList(:) 
!   integer,                  intent(in),             :: factorIndexList(:,:) 
!   type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
!   logical,                  intent(in),    optional :: ignoreUnmatchedIndices
!   integer,                  intent(inout), optional :: srcTermProcessing
!   integer,                  intent(inout), optional :: pipeLineDepth
!   integer,                  intent(out),   optional :: rc
! 
! !DESCRIPTION: 
! 
! \begin{sloppypar}
! Store a Field sparse matrix multiplication operation from {\tt srcField}
! to {\tt dstField}. PETs that specify non-zero matrix coefficients must use
! the <type><kind> overloaded interface and provide the {\tt factorList} and
! {\tt factorIndexList} arguments. Providing {\tt factorList} and
! {\tt factorIndexList} arguments with {\tt size(factorList) = (/0/)} and
! {\tt size(factorIndexList) = (/2,0/)} or {\tt (/4,0/)} indicates that a 
! PET does not provide matrix elements. Alternatively, PETs that do not 
! provide matrix elements may also call into the overloaded interface
! {\em without} {\tt factorList} and {\tt factorIndexList} arguments.
! \end{sloppypar}
!  
! Both {\tt srcField} and {\tt dstField} are interpreted as sequentialized 
! vectors. The 
! sequence is defined by the order of DistGrid dimensions and the order of 
! tiles within the DistGrid or by user-supplied arbitrary sequence indices. See 
! section \ref{Array:SparseMatMul} for details on the definition of {\em sequence indices}. 
! SMM corresponds to an identity mapping of the source Field vector to 
! the destination Field vector. 
!  
! Source and destination Fields may be of different <type><kind>. Further source 
! and destination Fields may differ in shape, however, the number of elements 
! must match. 
!  
! It is erroneous to specify the identical Field object for srcField and dstField 
! arguments.
!
!   The routine returns an {\tt ESMF\_RouteHandle} that can be used to call 
!   {\tt ESMF\_FieldSMM()} on any pair of Fields that matches 
!   {\tt srcField} and {\tt dstField} in {\em type}, {\em kind}, and 
!   memory layout of the {\em gridded} dimensions. However, the size, number, 
!   and index order of {\em ungridded} dimensions may be different. See section
!   \ref{RH:Reusability} for a more detailed discussion of RouteHandle 
!   reusability.
!  
! This method is overloaded for:\newline
! {\tt ESMF\_TYPEKIND\_I4}, {\tt ESMF\_TYPEKIND\_I8},\newline 
! {\tt ESMF\_TYPEKIND\_R4}, {\tt ESMF\_TYPEKIND\_R8}.
! \newline
!  
! This call is collective across the current VM.  
! 
! For examples and associated documentation regarding this method see Section
! \ref{sec:field:usage:smm_1dptr}. 
! 
! The arguments are:
!
! \begin{description}
!
! \item [srcField]
!       {\tt ESMF\_Field} with source data. The data in this Array may be
!     destroyed by this call.
!
! \item [dstField]
!       {\tt ESMF\_Field} with destination data. The data in this Field may be
!     destroyed by this call.
!
! \item [routehandle]
!       Handle to the precomputed Route.
!
! \item [transposeRoutehandle]
!     A handle to the transposed matrix operation is returned. The
!     transposed operation goes from {\tt dstArray} to {\tt srcArray}.
!
! \item [factorList]
!       List of non-zero coefficients.
!
! \item [factorIndexList]
!     Pairs of sequence indices for the factors stored in {\tt factorList}.
!
!     \begin{sloppypar}
!     The second dimension of {\tt factorIndexList} steps through the list of
!     pairs, i.e. {\tt size(factorIndexList,2) == size(factorList)}. The first
!     dimension of {\tt factorIndexList} is either of size 2 or size 4.
!     \end{sloppypar}
!     The second dimension of {\tt factorIndexList} steps through the list of
!
!     In the {\em size 2 format} {\tt factorIndexList(1,:)} specifies the
!     sequence index of the source element in the {\tt srcField} while
!     {\tt factorIndexList(2,:)} specifies the sequence index of the
!     destination element in {\tt dstField}. For this format to be a valid
!     option source and destination Fields must have matching number of
!     tensor elements (the product of the sizes of all Field tensor dimensions).
!     Under this condition an identity matrix can be applied within the space of
!     tensor elements for each sparse matrix factor.
!
!     \begin{sloppypar}
!     The {\em size 4 format} is more general and does not require a matching
!     tensor element count. Here the {\tt factorIndexList(1,:)} specifies the
!     sequence index while {\tt factorIndexList(2,:)} specifies the tensor
!     sequence index of the source element in the {\tt srcField}. Further
!     {\tt factorIndexList(3,:)} specifies the sequence index and
!     {\tt factorIndexList(4,:)} specifies the tensor sequence index of the 
!     destination element in the {\tt dstField}.
!     \end{sloppypar}
!
!     See section \ref{Array:SparseMatMul} for details on the definition of 
!     Field {\em sequence indices} and {\em tensor sequence indices}.
!
!   \item [{[ignoreUnmatchedIndices]}]
!     A logical flag that affects the behavior for when sequence indices 
!     in the sparse matrix are encountered that do not have a match on the 
!     {\tt srcField} or {\tt dstField} side. The default setting is 
!     {\tt .false.}, indicating that it is an error when such a situation is 
!     encountered. Setting {\tt ignoreUnmatchedIndices} to {\tt .true.} ignores
!     entries with unmatched indices.
!
!   \item [{[srcTermProcessing]}]
!     The {\tt srcTermProcessing} parameter controls how many source terms,
!     located on the same PET and summing into the same destination element,
!     are summed into partial sums on the source PET before being transferred
!     to the destination PET. A value of 0 indicates that the entire arithmetic
!     is done on the destination PET; source elements are neither multiplied
!     by their factors nor added into partial sums before being sent off by the
!     source PET. A value of 1 indicates that source elements are multiplied
!     by their factors on the source side before being sent to the destination
!     PET. Larger values of {\tt srcTermProcessing} indicate the maximum number
!     of terms in the partial sums on the source side.
!
!     Note that partial sums may lead to bit-for-bit differences in the results.
!     See section \ref{RH:bfb} for an in-depth discussion of {\em all}
!     bit-for-bit reproducibility aspects related to route-based communication
!     methods.
!
!     \begin{sloppypar}
!     The {\tt ESMF\_FieldSMMStore()} method implements an auto-tuning scheme
!     for the {\tt srcTermProcessing} parameter. The intent on the
!     {\tt srcTermProcessing} argument is "{\tt inout}" in order to
!     support both overriding and accessing the auto-tuning parameter.
!     If an argument $>= 0$ is specified, it is used for the
!     {\tt srcTermProcessing} parameter, and the auto-tuning phase is skipped.
!     In this case the {\tt srcTermProcessing} argument is not modified on
!     return. If the provided argument is $< 0$, the {\tt srcTermProcessing}
!     parameter is determined internally using the auto-tuning scheme. In this
!     case the {\tt srcTermProcessing} argument is re-set to the internally
!     determined value on return. Auto-tuning is also used if the optional
!     {\tt srcTermProcessing} argument is omitted.
!     \end{sloppypar}
!
!   \item [{[pipelineDepth]}]
!     The {\tt pipelineDepth} parameter controls how many messages a PET
!     may have outstanding during a sparse matrix exchange. Larger values
!     of {\tt pipelineDepth} typically lead to better performance. However,
!     on some systems too large a value may lead to performance degradation,
!     or runtime errors.
!
!     Note that the pipeline depth has no effect on the bit-for-bit
!     reproducibility of the results. However, it may affect the performance
!     reproducibility of the exchange.
!
!     The {\tt ESMF\_FieldSMMStore()} method implements an auto-tuning scheme
!     for the {\tt pipelineDepth} parameter. The intent on the
!     {\tt pipelineDepth} argument is "{\tt inout}" in order to
!     support both overriding and accessing the auto-tuning parameter.
!     If an argument $>= 0$ is specified, it is used for the
!     {\tt pipelineDepth} parameter, and the auto-tuning phase is skipped.
!     In this case the {\tt pipelineDepth} argument is not modified on
!     return. If the provided argument is $< 0$, the {\tt pipelineDepth}
!     parameter is determined internally using the auto-tuning scheme. In this
!     case the {\tt pipelineDepth} argument is re-set to the internally
!     determined value on return. Auto-tuning is also used if the optional
!     {\tt pipelineDepth} argument is omitted.
!
!   \item [{[rc]}]  
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!
! \end{description} 
! 
!EOP 
!---------------------------------------------------------------------------- 


#undef  ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldSMMStoreI4TR" 
!BOPI
! !IROUTINE: ESMF_FieldSMMStore - Precompute Field sparse matrix multiplication
!
! !INTERFACE:
  ! Private name; call using ESMF_FieldSMMStore()
    subroutine ESMF_FieldSMMStoreI4TR(srcField, dstField, & 
        routehandle, transposeRoutehandle, factorList, factorIndexList, &
        keywordEnforcer, ignoreUnmatchedIndices, srcTermProcessing, &
        pipeLineDepth, rc) 

        ! input arguments 
        type(ESMF_Field),       intent(inout)           :: srcField  
        type(ESMF_Field),       intent(inout)           :: dstField  
        type(ESMF_RouteHandle), intent(inout)           :: routehandle
        type(ESMF_RouteHandle), intent(inout)           :: transposeRoutehandle
        integer(ESMF_KIND_I4),  intent(in)              :: factorList(:)
        integer,                intent(in)              :: factorIndexList(:,:) 
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
        logical,                intent(in),    optional :: ignoreUnmatchedIndices
        integer,                intent(inout), optional :: srcTermProcessing
        integer,                intent(inout), optional :: pipeLineDepth
        integer,                intent(out),   optional :: rc 

!EOPI
        ! local variables as temporary input/output arguments 

        ! internal local variables 
        integer                                     :: localrc 
        type(ESMF_Array)                            :: srcArray, dstArray   

        ! Initialize return code; assume routine not implemented 
        localrc = ESMF_RC_NOT_IMPL 
        if(present(rc)) rc = ESMF_RC_NOT_IMPL 

        ! check variable: focus on field and farray 
        ! rely on ArraySMM to check the sanity of other variables 
        ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit, srcField, rc) 
        ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit, dstField, rc) 

        ! Retrieve source and destination arrays. 
        call ESMF_FieldGet(srcField, array=srcArray, rc=localrc) 
        if (ESMF_LogFoundError(localrc, & 
            ESMF_ERR_PASSTHRU, & 
            ESMF_CONTEXT, rcToReturn=rc)) return 
        call ESMF_FieldGet(dstField, array=dstArray, rc=localrc) 
        if (ESMF_LogFoundError(localrc, & 
            ESMF_ERR_PASSTHRU, & 
            ESMF_CONTEXT, rcToReturn=rc)) return 

        ! perform sparse matrix multiplication through array 
        ! For performance consideration: 
        ! Rely on ArraySMM to perform sanity checking of the other parameters 
        call ESMF_ArraySMMStore(srcArray=srcArray, dstArray=dstArray, &
          routehandle=routehandle, factorList=factorList, &
          factorIndexList=factorIndexList, &
          ignoreUnmatchedIndices=ignoreUnmatchedIndices, &
          srcTermProcessing=srcTermProcessing, pipelineDepth=pipelineDepth, &
          transposeRoutehandle=transposeRoutehandle, rc=localrc)
        if (ESMF_LogFoundError(localrc, & 
            ESMF_ERR_PASSTHRU, & 
            ESMF_CONTEXT, rcToReturn=rc)) return 

        if (present(rc)) rc = ESMF_SUCCESS 
    end subroutine ESMF_FieldSMMStoreI4TR
!------------------------------------------------------------------------------ 

#undef  ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldSMMStoreI8TR" 
!BOPI
! !IROUTINE: ESMF_FieldSMMStore - Precompute Field sparse matrix multiplication
!
! !INTERFACE:
  ! Private name; call using ESMF_FieldSMMStore()
    subroutine ESMF_FieldSMMStoreI8TR(srcField, dstField, & 
        routehandle, transposeRoutehandle, factorList, factorIndexList, &
        keywordEnforcer, ignoreUnmatchedIndices, srcTermProcessing, &
        pipeLineDepth, rc)

        ! input arguments 
        type(ESMF_Field),       intent(inout)           :: srcField  
        type(ESMF_Field),       intent(inout)           :: dstField  
        type(ESMF_RouteHandle), intent(inout)           :: routehandle
        type(ESMF_RouteHandle), intent(inout)           :: transposeRoutehandle
        integer(ESMF_KIND_I8),  intent(in)              :: factorList(:)
        integer,                intent(in)              :: factorIndexList(:,:) 
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
        logical,                intent(in),    optional :: ignoreUnmatchedIndices
        integer,                intent(inout), optional :: srcTermProcessing
        integer,                intent(inout), optional :: pipeLineDepth
        integer,                intent(out),   optional :: rc 

!EOPI
        ! local variables as temporary input/output arguments 

        ! internal local variables 
        integer                                     :: localrc 
        type(ESMF_Array)                            :: srcArray, dstArray   

        ! Initialize return code; assume routine not implemented 
        localrc = ESMF_RC_NOT_IMPL 
        if(present(rc)) rc = ESMF_RC_NOT_IMPL 

        ! check variable: focus on field and farray 
        ! rely on ArraySMM to check the sanity of other variables 
        ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit, srcField, rc) 
        ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit, dstField, rc) 

        ! Retrieve source and destination arrays. 
        call ESMF_FieldGet(srcField, array=srcArray, rc=localrc) 
        if (ESMF_LogFoundError(localrc, & 
            ESMF_ERR_PASSTHRU, & 
            ESMF_CONTEXT, rcToReturn=rc)) return 
        call ESMF_FieldGet(dstField, array=dstArray, rc=localrc) 
        if (ESMF_LogFoundError(localrc, & 
            ESMF_ERR_PASSTHRU, & 
            ESMF_CONTEXT, rcToReturn=rc)) return 

        ! perform sparse matrix multiplication through array 
        ! For performance consideration: 
        ! Rely on ArraySMM to perform sanity checking of the other parameters 
        call ESMF_ArraySMMStore(srcArray=srcArray, dstArray=dstArray, &
          routehandle=routehandle, factorList=factorList, & 
          factorIndexList=factorIndexList, &
          ignoreUnmatchedIndices=ignoreUnmatchedIndices, &
          srcTermProcessing=srcTermProcessing, pipelineDepth=pipelineDepth, &
          transposeRoutehandle=transposeRoutehandle, rc=localrc)
        if (ESMF_LogFoundError(localrc, & 
            ESMF_ERR_PASSTHRU, & 
            ESMF_CONTEXT, rcToReturn=rc)) return 

        if (present(rc)) rc = ESMF_SUCCESS 
    end subroutine ESMF_FieldSMMStoreI8TR
!------------------------------------------------------------------------------ 

#undef  ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldSMMStoreR4TR"
!BOPI
! !IROUTINE: ESMF_FieldSMMStore - Precompute Field sparse matrix multiplication
!
! !INTERFACE:
  ! Private name; call using ESMF_FieldSMMStore()
    subroutine ESMF_FieldSMMStoreR4TR(srcField, dstField, & 
        routehandle, transposeRoutehandle, factorList, factorIndexList, &
        keywordEnforcer, ignoreUnmatchedIndices, srcTermProcessing, &
        pipeLineDepth, rc)

        ! input arguments 
        type(ESMF_Field),       intent(inout)           :: srcField  
        type(ESMF_Field),       intent(inout)           :: dstField  
        type(ESMF_RouteHandle), intent(inout)           :: routehandle
        type(ESMF_RouteHandle), intent(inout)           :: transposeRoutehandle
        real(ESMF_KIND_R4),     intent(in)              :: factorList(:)
        integer,                intent(in)              :: factorIndexList(:,:) 
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
        logical,                intent(in),    optional :: ignoreUnmatchedIndices
        integer,                intent(inout), optional :: srcTermProcessing
        integer,                intent(inout), optional :: pipeLineDepth
        integer,                intent(out),   optional :: rc 

!EOPI
        ! local variables as temporary input/output arguments 

        ! internal local variables 
        integer                                     :: localrc 
        type(ESMF_Array)                            :: srcArray, dstArray   

        ! Initialize return code; assume routine not implemented 
        localrc = ESMF_RC_NOT_IMPL 
        if(present(rc)) rc = ESMF_RC_NOT_IMPL 

        ! check variable: focus on field and farray 
        ! rely on ArraySMM to check the sanity of other variables 
        ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit, srcField, rc) 
        ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit, dstField, rc) 

        ! Retrieve source and destination arrays. 
        call ESMF_FieldGet(srcField, array=srcArray, rc=localrc) 
        if (ESMF_LogFoundError(localrc, & 
            ESMF_ERR_PASSTHRU, & 
            ESMF_CONTEXT, rcToReturn=rc)) return 
        call ESMF_FieldGet(dstField, array=dstArray, rc=localrc) 
        if (ESMF_LogFoundError(localrc, & 
            ESMF_ERR_PASSTHRU, & 
            ESMF_CONTEXT, rcToReturn=rc)) return 

        ! perform sparse matrix multiplication through array 
        ! For performance consideration: 
        ! Rely on ArraySMM to perform sanity checking of the other parameters 
        call ESMF_ArraySMMStore(srcArray=srcArray, dstArray=dstArray, &
          routehandle=routehandle, factorList=factorList, & 
          factorIndexList=factorIndexList, &
          ignoreUnmatchedIndices=ignoreUnmatchedIndices, &
          srcTermProcessing=srcTermProcessing, pipelineDepth=pipelineDepth, &
          transposeRoutehandle=transposeRoutehandle, rc=localrc)
        if (ESMF_LogFoundError(localrc, & 
            ESMF_ERR_PASSTHRU, & 
            ESMF_CONTEXT, rcToReturn=rc)) return 

        if (present(rc)) rc = ESMF_SUCCESS 
    end subroutine ESMF_FieldSMMStoreR4TR
!------------------------------------------------------------------------------ 

#undef  ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldSMMStoreR8TR"
!BOPI
! !IROUTINE: ESMF_FieldSMMStore - Precompute Field sparse matrix multiplication
!
! !INTERFACE:
  ! Private name; call using ESMF_FieldSMMStore()
    subroutine ESMF_FieldSMMStoreR8TR(srcField, dstField, & 
        routehandle, transposeRoutehandle, factorList, factorIndexList, &
        keywordEnforcer, ignoreUnmatchedIndices, srcTermProcessing, &
        pipeLineDepth, rc)

        ! input arguments 
        type(ESMF_Field),       intent(inout)           :: srcField  
        type(ESMF_Field),       intent(inout)           :: dstField  
        type(ESMF_RouteHandle), intent(inout)           :: routehandle
        type(ESMF_RouteHandle), intent(inout)           :: transposeRoutehandle
        real(ESMF_KIND_R8),     intent(in)              :: factorList(:)
        integer,                intent(in)              :: factorIndexList(:,:) 
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
        logical,                intent(in),    optional :: ignoreUnmatchedIndices
        integer,                intent(inout), optional :: srcTermProcessing
        integer,                intent(inout), optional :: pipeLineDepth
        integer,                intent(out),   optional :: rc 

!EOPI
        ! local variables as temporary input/output arguments 

        ! internal local variables 
        integer                                     :: localrc 
        type(ESMF_Array)                            :: srcArray, dstArray   

        ! Initialize return code; assume routine not implemented 
        localrc = ESMF_RC_NOT_IMPL 
        if(present(rc)) rc = ESMF_RC_NOT_IMPL 

        ! check variable: focus on field and farray 
        ! rely on ArraySMM to check the sanity of other variables 
        ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit, srcField, rc) 
        ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit, dstField, rc) 

        ! Retrieve source and destination arrays. 
        call ESMF_FieldGet(srcField, array=srcArray, rc=localrc) 
        if (ESMF_LogFoundError(localrc, & 
            ESMF_ERR_PASSTHRU, & 
            ESMF_CONTEXT, rcToReturn=rc)) return 
        call ESMF_FieldGet(dstField, array=dstArray, rc=localrc) 
        if (ESMF_LogFoundError(localrc, & 
            ESMF_ERR_PASSTHRU, & 
            ESMF_CONTEXT, rcToReturn=rc)) return 

        ! perform sparse matrix multiplication through array 
        ! For performance consideration: 
        ! Rely on ArraySMM to perform sanity checking of the other parameters 
        call ESMF_ArraySMMStore(srcArray=srcArray, dstArray=dstArray, &
          routehandle=routehandle, factorList=factorList, & 
          factorIndexList=factorIndexList, &
          ignoreUnmatchedIndices=ignoreUnmatchedIndices, &
          srcTermProcessing=srcTermProcessing, pipelineDepth=pipelineDepth, &
          transposeRoutehandle=transposeRoutehandle, rc=localrc)
        if (ESMF_LogFoundError(localrc, & 
            ESMF_ERR_PASSTHRU, & 
            ESMF_CONTEXT, rcToReturn=rc)) return 

        if (present(rc)) rc = ESMF_SUCCESS 
    end subroutine ESMF_FieldSMMStoreR8TR
!------------------------------------------------------------------------------ 

#undef  ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldSMMStoreNF" 
!---------------------------------------------------------------------------- 
!BOP 
! !IROUTINE: ESMF_FieldSMMStore - Precompute Field sparse matrix multiplication without local factors
! 
! !INTERFACE: 
! ! Private name; call using ESMF_FieldSMMStore() 
    subroutine ESMF_FieldSMMStoreNF(srcField, dstField, &
        routehandle, keywordEnforcer, ignoreUnmatchedIndices, &
        srcTermProcessing, pipelineDepth, rc)
!
! !ARGUMENTS:
        type(ESMF_Field),       intent(in)              :: srcField  
        type(ESMF_Field),       intent(inout)           :: dstField  
        type(ESMF_RouteHandle), intent(inout)           :: routehandle
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
        logical,                intent(in),    optional :: ignoreUnmatchedIndices
        integer,                intent(inout), optional :: srcTermProcessing
        integer,                intent(inout), optional :: pipeLineDepth
        integer,                intent(out),   optional :: rc 
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \item\apiStatusModifiedSinceVersion{5.2.0r}
! \begin{description}
! \item[6.1.0] Added arguments {\tt srcTermProcessing}, {\tt pipelineDepth}
!              The two arguments {\tt srcTermProcessing} and {\tt pipelineDepth}
!              provide access to the tuning parameters affecting the sparse matrix
!              execution. 
! \item[7.0.0] Added argument {\tt transposeRoutehandle} to allow a handle to
!              the transposed matrix operation to be returned.\newline
!              Added argument {\tt ignoreUnmatchedIndices} to support sparse 
!              matrices that contain elements with indices that do not have a
!              match within the source or destination Array.
! \item[7.1.0r] Removed argument {\tt transposeRoutehandle} and provide it
!              via interface overloading instead. This allows argument 
!              {\tt srcField} to stay strictly intent(in) for this entry point.
! \end{description}
! \end{itemize}
!
! !DESCRIPTION: 
!
! \begin{sloppypar}
! Store a Field sparse matrix multiplication operation from {\tt srcField}
! to {\tt dstField}. PETs that specify non-zero matrix coefficients must use
! the <type><kind> overloaded interface and provide the {\tt factorList} and
! {\tt factorIndexList} arguments. Providing {\tt factorList} and
! {\tt factorIndexList} arguments with {\tt size(factorList) = (/0/)} and
! {\tt size(factorIndexList) = (/2,0/)} or {\tt (/4,0/)} indicates that a 
! PET does not provide matrix elements. Alternatively, PETs that do not 
! provide matrix elements may also call into the overloaded interface
! {\em without} {\tt factorList} and {\tt factorIndexList} arguments.
! \end{sloppypar}
! 
! Both {\tt srcField} and {\tt dstField} are interpreted as sequentialized 
! vectors. The 
! sequence is defined by the order of DistGrid dimensions and the order of 
! tiles within the DistGrid or by user-supplied arbitrary sequence indices. See 
! section \ref{Array:SparseMatMul} for details on the definition of {\em sequence indices}. 
! SMM corresponds to an identity mapping of the source Field vector to 
! the destination Field vector. 
!  
! Source and destination Fields may be of different <type><kind>. Further source 
! and destination Fields may differ in shape, however, the number of elements 
! must match. 
!  
! It is erroneous to specify the identical Field object for srcField and dstField 
! arguments. 
!  
!   The routine returns an {\tt ESMF\_RouteHandle} that can be used to call 
!   {\tt ESMF\_FieldSMM()} on any pair of Fields that matches 
!   {\tt srcField} and {\tt dstField} in {\em type}, {\em kind}, and 
!   memory layout of the {\em gridded} dimensions. However, the size, number, 
!   and index order of {\em ungridded} dimensions may be different. See section
!   \ref{RH:Reusability} for a more detailed discussion of RouteHandle 
!   reusability.
!  
! This method is overloaded for:\newline
! {\tt ESMF\_TYPEKIND\_I4}, {\tt ESMF\_TYPEKIND\_I8},\newline 
! {\tt ESMF\_TYPEKIND\_R4}, {\tt ESMF\_TYPEKIND\_R8}.
! \newline
!
! This call is collective across the current VM.  
! 
! For examples and associated documentation regarding this method see Section
! \ref{sec:field:usage:smm_1dptr}. 
! 
! The arguments are:
!
! \begin{description}
!
! \item [srcField]
!       {\tt ESMF\_Field} with source data.
!
! \item [dstField]
!       {\tt ESMF\_Field} with destination data. The data in this Field may be
!     destroyed by this call.
!
! \item [routehandle]
!       Handle to the precomputed Route.
!
!   \item [{[ignoreUnmatchedIndices]}]
!     A logical flag that affects the behavior for when sequence indices 
!     in the sparse matrix are encountered that do not have a match on the 
!     {\tt srcField} or {\tt dstField} side. The default setting is 
!     {\tt .false.}, indicating that it is an error when such a situation is 
!     encountered. Setting {\tt ignoreUnmatchedIndices} to {\tt .true.} ignores
!     entries with unmatched indices.
!
!   \item [{[srcTermProcessing]}]
!     The {\tt srcTermProcessing} parameter controls how many source terms,
!     located on the same PET and summing into the same destination element,
!     are summed into partial sums on the source PET before being transferred
!     to the destination PET. A value of 0 indicates that the entire arithmetic
!     is done on the destination PET; source elements are neither multiplied
!     by their factors nor added into partial sums before being sent off by the
!     source PET. A value of 1 indicates that source elements are multiplied
!     by their factors on the source side before being sent to the destination
!     PET. Larger values of {\tt srcTermProcessing} indicate the maximum number
!     of terms in the partial sums on the source side.
!
!     Note that partial sums may lead to bit-for-bit differences in the results.
!     See section \ref{RH:bfb} for an in-depth discussion of {\em all}
!     bit-for-bit reproducibility aspects related to route-based communication
!     methods.
!
!     \begin{sloppypar}
!     The {\tt ESMF\_FieldSMMStore()} method implements an auto-tuning scheme
!     for the {\tt srcTermProcessing} parameter. The intent on the
!     {\tt srcTermProcessing} argument is "{\tt inout}" in order to
!     support both overriding and accessing the auto-tuning parameter.
!     If an argument $>= 0$ is specified, it is used for the
!     {\tt srcTermProcessing} parameter, and the auto-tuning phase is skipped.
!     In this case the {\tt srcTermProcessing} argument is not modified on
!     return. If the provided argument is $< 0$, the {\tt srcTermProcessing}
!     parameter is determined internally using the auto-tuning scheme. In this
!     case the {\tt srcTermProcessing} argument is re-set to the internally
!     determined value on return. Auto-tuning is also used if the optional
!     {\tt srcTermProcessing} argument is omitted.
!     \end{sloppypar}
!
!   \item [{[pipelineDepth]}]
!     The {\tt pipelineDepth} parameter controls how many messages a PET
!     may have outstanding during a sparse matrix exchange. Larger values
!     of {\tt pipelineDepth} typically lead to better performance. However,
!     on some systems too large a value may lead to performance degradation,
!     or runtime errors.
!
!     Note that the pipeline depth has no effect on the bit-for-bit
!     reproducibility of the results. However, it may affect the performance
!     reproducibility of the exchange.
!
!     The {\tt ESMF\_FieldSMMStore()} method implements an auto-tuning scheme
!     for the {\tt pipelineDepth} parameter. The intent on the
!     {\tt pipelineDepth} argument is "{\tt inout}" in order to
!     support both overriding and accessing the auto-tuning parameter.
!     If an argument $>= 0$ is specified, it is used for the
!     {\tt pipelineDepth} parameter, and the auto-tuning phase is skipped.
!     In this case the {\tt pipelineDepth} argument is not modified on
!     return. If the provided argument is $< 0$, the {\tt pipelineDepth}
!     parameter is determined internally using the auto-tuning scheme. In this
!     case the {\tt pipelineDepth} argument is re-set to the internally
!     determined value on return. Auto-tuning is also used if the optional
!     {\tt pipelineDepth} argument is omitted.
!
!   \item [{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!
! \end{description} 
! 
!EOP 
!---------------------------------------------------------------------------- 
        ! local variables as temporary input/output arguments 

        ! internal local variables 
        integer                                     :: localrc 
        type(ESMF_Array)                            :: srcArray, dstArray   

        ! Initialize return code; assume routine not implemented 
        localrc = ESMF_RC_NOT_IMPL 
        if(present(rc)) rc = ESMF_RC_NOT_IMPL 

        ! check variable: focus on field and farray 
        ! rely on ArraySMM to check the sanity of other variables 
        ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit, srcField, rc) 
        ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit, dstField, rc) 

        ! Retrieve source and destination arrays. 
        call ESMF_FieldGet(srcField, array=srcArray, rc=localrc) 
        if (ESMF_LogFoundError(localrc, & 
            ESMF_ERR_PASSTHRU, & 
            ESMF_CONTEXT, rcToReturn=rc)) return 
        call ESMF_FieldGet(dstField, array=dstArray, rc=localrc) 
        if (ESMF_LogFoundError(localrc, & 
            ESMF_ERR_PASSTHRU, & 
            ESMF_CONTEXT, rcToReturn=rc)) return 

        ! perform sparse matrix multiplication through array 
        ! For performance consideration: 
        ! Rely on ArraySMM to perform sanity checking of the other parameters 
        call ESMF_ArraySMMStore(srcArray=srcArray, dstArray=dstArray, &
          routehandle=routehandle, &
          ignoreUnmatchedIndices=ignoreUnmatchedIndices, &
          srcTermProcessing=srcTermProcessing, pipelineDepth=pipelineDepth, &
          rc=localrc)
        if (ESMF_LogFoundError(localrc, & 
            ESMF_ERR_PASSTHRU, & 
            ESMF_CONTEXT, rcToReturn=rc)) return 

        if (present(rc)) rc = ESMF_SUCCESS 
    end subroutine ESMF_FieldSMMStoreNF


#undef  ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldSMMStoreNFTR" 
!---------------------------------------------------------------------------- 
!BOP 
! !IROUTINE: ESMF_FieldSMMStore - Precompute Field sparse matrix multiplication and transpose without local factors
! 
! !INTERFACE: 
! ! Private name; call using ESMF_FieldSMMStore() 
    subroutine ESMF_FieldSMMStoreNFTR(srcField, dstField, &
        routehandle, transposeRoutehandle, keywordEnforcer, ignoreUnmatchedIndices, &
        srcTermProcessing, pipelineDepth, rc)
!
! !ARGUMENTS:
        type(ESMF_Field),       intent(inout)           :: srcField  
        type(ESMF_Field),       intent(inout)           :: dstField  
        type(ESMF_RouteHandle), intent(inout)           :: routehandle
        type(ESMF_RouteHandle), intent(inout)           :: transposeRoutehandle
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
        logical,                intent(in),    optional :: ignoreUnmatchedIndices
        integer,                intent(inout), optional :: srcTermProcessing
        integer,                intent(inout), optional :: pipeLineDepth
        integer,                intent(out),   optional :: rc 
!
! !DESCRIPTION: 
!
! \begin{sloppypar}
! Store a Field sparse matrix multiplication operation from {\tt srcField}
! to {\tt dstField}. PETs that specify non-zero matrix coefficients must use
! the <type><kind> overloaded interface and provide the {\tt factorList} and
! {\tt factorIndexList} arguments. Providing {\tt factorList} and
! {\tt factorIndexList} arguments with {\tt size(factorList) = (/0/)} and
! {\tt size(factorIndexList) = (/2,0/)} or {\tt (/4,0/)} indicates that a 
! PET does not provide matrix elements. Alternatively, PETs that do not 
! provide matrix elements may also call into the overloaded interface
! {\em without} {\tt factorList} and {\tt factorIndexList} arguments.
! \end{sloppypar}
! 
! Both {\tt srcField} and {\tt dstField} are interpreted as sequentialized 
! vectors. The 
! sequence is defined by the order of DistGrid dimensions and the order of 
! tiles within the DistGrid or by user-supplied arbitrary sequence indices. See 
! section \ref{Array:SparseMatMul} for details on the definition of {\em sequence indices}. 
! SMM corresponds to an identity mapping of the source Field vector to 
! the destination Field vector. 
!  
! Source and destination Fields may be of different <type><kind>. Further source 
! and destination Fields may differ in shape, however, the number of elements 
! must match. 
!  
! It is erroneous to specify the identical Field object for srcField and dstField 
! arguments. 
!  
!   The routine returns an {\tt ESMF\_RouteHandle} that can be used to call 
!   {\tt ESMF\_FieldSMM()} on any pair of Fields that matches 
!   {\tt srcField} and {\tt dstField} in {\em type}, {\em kind}, and 
!   memory layout of the {\em gridded} dimensions. However, the size, number, 
!   and index order of {\em ungridded} dimensions may be different. See section
!   \ref{RH:Reusability} for a more detailed discussion of RouteHandle 
!   reusability.
!  
! This method is overloaded for:\newline
! {\tt ESMF\_TYPEKIND\_I4}, {\tt ESMF\_TYPEKIND\_I8},\newline 
! {\tt ESMF\_TYPEKIND\_R4}, {\tt ESMF\_TYPEKIND\_R8}.
! \newline
!
! This call is collective across the current VM.  
! 
! For examples and associated documentation regarding this method see Section
! \ref{sec:field:usage:smm_1dptr}. 
! 
! The arguments are:
!
! \begin{description}
!
! \item [srcField]
!       {\tt ESMF\_Field} with source data. The data in this Field may be
!     destroyed by this call.
!
! \item [dstField]
!       {\tt ESMF\_Field} with destination data. The data in this Field may be
!     destroyed by this call.
!
! \item [routehandle]
!       Handle to the precomputed Route.
!
! \item [transposeRoutehandle]
!     A handle to the transposed matrix operation is returned. The
!     transposed operation goes from {\tt dstArray} to {\tt srcArray}.
!     
!   \item [{[ignoreUnmatchedIndices]}]
!     A logical flag that affects the behavior for when sequence indices 
!     in the sparse matrix are encountered that do not have a match on the 
!     {\tt srcField} or {\tt dstField} side. The default setting is 
!     {\tt .false.}, indicating that it is an error when such a situation is 
!     encountered. Setting {\tt ignoreUnmatchedIndices} to {\tt .true.} ignores
!     entries with unmatched indices.
!
!   \item [{[srcTermProcessing]}]
!     The {\tt srcTermProcessing} parameter controls how many source terms,
!     located on the same PET and summing into the same destination element,
!     are summed into partial sums on the source PET before being transferred
!     to the destination PET. A value of 0 indicates that the entire arithmetic
!     is done on the destination PET; source elements are neither multiplied
!     by their factors nor added into partial sums before being sent off by the
!     source PET. A value of 1 indicates that source elements are multiplied
!     by their factors on the source side before being sent to the destination
!     PET. Larger values of {\tt srcTermProcessing} indicate the maximum number
!     of terms in the partial sums on the source side.
!
!     Note that partial sums may lead to bit-for-bit differences in the results.
!     See section \ref{RH:bfb} for an in-depth discussion of {\em all}
!     bit-for-bit reproducibility aspects related to route-based communication
!     methods.
!
!     \begin{sloppypar}
!     The {\tt ESMF\_FieldSMMStore()} method implements an auto-tuning scheme
!     for the {\tt srcTermProcessing} parameter. The intent on the
!     {\tt srcTermProcessing} argument is "{\tt inout}" in order to
!     support both overriding and accessing the auto-tuning parameter.
!     If an argument $>= 0$ is specified, it is used for the
!     {\tt srcTermProcessing} parameter, and the auto-tuning phase is skipped.
!     In this case the {\tt srcTermProcessing} argument is not modified on
!     return. If the provided argument is $< 0$, the {\tt srcTermProcessing}
!     parameter is determined internally using the auto-tuning scheme. In this
!     case the {\tt srcTermProcessing} argument is re-set to the internally
!     determined value on return. Auto-tuning is also used if the optional
!     {\tt srcTermProcessing} argument is omitted.
!     \end{sloppypar}
!
!   \item [{[pipelineDepth]}]
!     The {\tt pipelineDepth} parameter controls how many messages a PET
!     may have outstanding during a sparse matrix exchange. Larger values
!     of {\tt pipelineDepth} typically lead to better performance. However,
!     on some systems too large a value may lead to performance degradation,
!     or runtime errors.
!
!     Note that the pipeline depth has no effect on the bit-for-bit
!     reproducibility of the results. However, it may affect the performance
!     reproducibility of the exchange.
!
!     The {\tt ESMF\_FieldSMMStore()} method implements an auto-tuning scheme
!     for the {\tt pipelineDepth} parameter. The intent on the
!     {\tt pipelineDepth} argument is "{\tt inout}" in order to
!     support both overriding and accessing the auto-tuning parameter.
!     If an argument $>= 0$ is specified, it is used for the
!     {\tt pipelineDepth} parameter, and the auto-tuning phase is skipped.
!     In this case the {\tt pipelineDepth} argument is not modified on
!     return. If the provided argument is $< 0$, the {\tt pipelineDepth}
!     parameter is determined internally using the auto-tuning scheme. In this
!     case the {\tt pipelineDepth} argument is re-set to the internally
!     determined value on return. Auto-tuning is also used if the optional
!     {\tt pipelineDepth} argument is omitted.
!
!   \item [{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!
! \end{description} 
! 
!EOP 
!---------------------------------------------------------------------------- 
        ! local variables as temporary input/output arguments 

        ! internal local variables 
        integer                                     :: localrc 
        type(ESMF_Array)                            :: srcArray, dstArray   

        ! Initialize return code; assume routine not implemented 
        localrc = ESMF_RC_NOT_IMPL 
        if(present(rc)) rc = ESMF_RC_NOT_IMPL 

        ! check variable: focus on field and farray 
        ! rely on ArraySMM to check the sanity of other variables 
        ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit, srcField, rc) 
        ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit, dstField, rc) 

        ! Retrieve source and destination arrays. 
        call ESMF_FieldGet(srcField, array=srcArray, rc=localrc) 
        if (ESMF_LogFoundError(localrc, & 
            ESMF_ERR_PASSTHRU, & 
            ESMF_CONTEXT, rcToReturn=rc)) return 
        call ESMF_FieldGet(dstField, array=dstArray, rc=localrc) 
        if (ESMF_LogFoundError(localrc, & 
            ESMF_ERR_PASSTHRU, & 
            ESMF_CONTEXT, rcToReturn=rc)) return 

        ! perform sparse matrix multiplication through array 
        ! For performance consideration: 
        ! Rely on ArraySMM to perform sanity checking of the other parameters 
        call ESMF_ArraySMMStore(srcArray=srcArray, dstArray=dstArray, &
          routehandle=routehandle, &
          ignoreUnmatchedIndices=ignoreUnmatchedIndices, &
          srcTermProcessing=srcTermProcessing, pipelineDepth=pipelineDepth, &
          transposeRoutehandle=transposeRoutehandle, rc=localrc)
        if (ESMF_LogFoundError(localrc, & 
            ESMF_ERR_PASSTHRU, & 
            ESMF_CONTEXT, rcToReturn=rc)) return 

        if (present(rc)) rc = ESMF_SUCCESS 
    end subroutine ESMF_FieldSMMStoreNFTR

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldSMMStoreFromFile"

!BOP
! !IROUTINE: ESMF_FieldSMMStore - Precompute sparse matrix multiplication using factors read from file
!
! !INTERFACE:
! ! Private name; call using ESMF_FieldSMMStore()
    subroutine ESMF_FieldSMMStoreFromFile(srcField, dstField, filename, &
      routehandle, keywordEnforcer, ignoreUnmatchedIndices, &
      srcTermProcessing, pipelineDepth, rc)

! ! ARGUMENTS:
      type(ESMF_Field),       intent(in)              :: srcField  
      type(ESMF_Field),       intent(inout)           :: dstField
      character(len=*),       intent(in)              :: filename
      type(ESMF_RouteHandle), intent(inout)           :: routehandle
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
      logical,                intent(in),    optional :: ignoreUnmatchedIndices
      integer,                intent(inout), optional :: srcTermProcessing
      integer,                intent(inout), optional :: pipeLineDepth
      integer,                intent(out),   optional :: rc
!
! !DESCRIPTION:
!
! Compute an {\tt ESMF\_RouteHandle} using factors read from file.
!
! The arguments are:
!
! \begin{description}
!
! \item [srcField]
!       {\tt ESMF\_Field} with source data.
!
! \item [dstField]
!       {\tt ESMF\_Field} with destination data. The data in this Field may be
!       destroyed by this call.
!
! \item [filename]
!       Path to the file containing weights for creating an {\tt ESMF\_RouteHandle}.
!       See ~(\ref{sec:weightfileformat}) for a description of the SCRIP weight
!       file format. Only "row", "col", and "S" variables are required. They
!       must be one-dimensionsal with dimension "n\_s".
!
! \item [routehandle]
!       Handle to the {\tt ESMF\_RouteHandle}.
!
!   \item [{[ignoreUnmatchedIndices]}]
!     A logical flag that affects the behavior for when sequence indices
!     in the sparse matrix are encountered that do not have a match on the
!     {\tt srcField} or {\tt dstField} side. The default setting is
!     {\tt .false.}, indicating that it is an error when such a situation is
!     encountered. Setting {\tt ignoreUnmatchedIndices} to {\tt .true.} ignores
!     entries with unmatched indices.
!
!   \item [{[srcTermProcessing]}]
!     The {\tt srcTermProcessing} parameter controls how many source terms,
!     located on the same PET and summing into the same destination element,
!     are summed into partial sums on the source PET before being transferred
!     to the destination PET. A value of 0 indicates that the entire arithmetic
!     is done on the destination PET; source elements are neither multiplied
!     by their factors nor added into partial sums before being sent off by the
!     source PET. A value of 1 indicates that source elements are multiplied
!     by their factors on the source side before being sent to the destination
!     PET. Larger values of {\tt srcTermProcessing} indicate the maximum number
!     of terms in the partial sums on the source side.
!
!     Note that partial sums may lead to bit-for-bit differences in the results.
!     See section \ref{RH:bfb} for an in-depth discussion of {\em all}
!     bit-for-bit reproducibility aspects related to route-based communication
!     methods.
!
!     \begin{sloppypar}
!     The {\tt ESMF\_FieldSMMStore()} method implements an auto-tuning scheme
!     for the {\tt srcTermProcessing} parameter. The intent on the
!     {\tt srcTermProcessing} argument is "{\tt inout}" in order to
!     support both overriding and accessing the auto-tuning parameter.
!     If an argument $>= 0$ is specified, it is used for the
!     {\tt srcTermProcessing} parameter, and the auto-tuning phase is skipped.
!     In this case the {\tt srcTermProcessing} argument is not modified on
!     return. If the provided argument is $< 0$, the {\tt srcTermProcessing}
!     parameter is determined internally using the auto-tuning scheme. In this
!     case the {\tt srcTermProcessing} argument is re-set to the internally
!     determined value on return. Auto-tuning is also used if the optional
!     {\tt srcTermProcessing} argument is omitted.
!     \end{sloppypar}
!
!   \item [{[pipelineDepth]}]
!     The {\tt pipelineDepth} parameter controls how many messages a PET
!     may have outstanding during a sparse matrix exchange. Larger values
!     of {\tt pipelineDepth} typically lead to better performance. However,
!     on some systems too large a value may lead to performance degradation,
!     or runtime errors.
!
!     Note that the pipeline depth has no effect on the bit-for-bit
!     reproducibility of the results. However, it may affect the performance
!     reproducibility of the exchange.
!     The {\tt ESMF\_FieldSMMStore()} method implements an auto-tuning scheme
!     for the {\tt pipelineDepth} parameter. The intent on the
!     {\tt pipelineDepth} argument is "{\tt inout}" in order to
!     support both overriding and accessing the auto-tuning parameter.
!     If an argument $>= 0$ is specified, it is used for the
!     {\tt pipelineDepth} parameter, and the auto-tuning phase is skipped.
!     In this case the {\tt pipelineDepth} argument is not modified on
!     return. If the provided argument is $< 0$, the {\tt pipelineDepth}
!     parameter is determined internally using the auto-tuning scheme. In this
!     case the {\tt pipelineDepth} argument is re-set to the internally
!     determined value on return. Auto-tuning is also used if the optional
!     {\tt pipelineDepth} argument is omitted.
!
!   \item [{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!
! \end{description}
!
!EOP
!-------------------------------------------------------------------------------

      ! LOCAL VARIABLES:
      real(ESMF_KIND_R8), dimension(:), allocatable :: factorList
      integer, dimension(:, :), allocatable :: factorIndexList
      integer :: localrc

      real(ESMF_KIND_R8), pointer :: src(:,:)

      ! Initialize return code; assume routine not implemented
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! Fill the factorList and factorIndexList.
      call ESMF_FactorRead(filename, &
                           factorList, &
                           factorIndexList, &
                           rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! Generate routeHandle from factorList and factorIndexList
      call ESMF_FieldSMMStore(srcField=srcField, &
                              dstField=dstField, &
                              routehandle=routeHandle, &
                              factorList=factorList, &
                              factorIndexList=factorIndexList,   &
                              ignoreUnmatchedIndices=ignoreUnmatchedIndices, &
                              srcTermProcessing=srcTermProcessing, &
                              pipeLineDepth=pipeLineDepth, &
                              rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      deallocate(factorList)
      deallocate(factorIndexList)

      if (present(rc)) rc = ESMF_SUCCESS

    end subroutine ESMF_FieldSMMStoreFromFile

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldSMMStoreFromFileTR"

!BOP
! !IROUTINE: ESMF_FieldSMMStore - Precompute sparse matrix multiplication and transpose using factors read from file
!
! !INTERFACE:
! ! Private name; call using ESMF_FieldSMMStore()
    subroutine ESMF_FieldSMMStoreFromFileTR(srcField, dstField, filename, &
      routehandle, transposeRoutehandle, keywordEnforcer, &
      ignoreUnmatchedIndices, srcTermProcessing, pipelineDepth, rc)

! ! ARGUMENTS:
      type(ESMF_Field),       intent(inout)           :: srcField  
      type(ESMF_Field),       intent(inout)           :: dstField
      character(len=*),       intent(in)              :: filename
      type(ESMF_RouteHandle), intent(inout)           :: routehandle
      type(ESMF_RouteHandle), intent(inout)           :: transposeRoutehandle
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
      logical,                intent(in),    optional :: ignoreUnmatchedIndices
      integer,                intent(inout), optional :: srcTermProcessing
      integer,                intent(inout), optional :: pipeLineDepth
      integer,                intent(out),   optional :: rc
!
! !DESCRIPTION:
!
! Compute an {\tt ESMF\_RouteHandle} using factors read from file.
!
! The arguments are:
!
! \begin{description}
!
! \item [srcField]
!       {\tt ESMF\_Field} with source data. The data in this Array may be
!       destroyed by this call.
!
! \item [dstField]
!       {\tt ESMF\_Field} with destination data. The data in this Field may be
!       destroyed by this call.
!
! \item [filename]
!       Path to the file containing weights for creating an {\tt ESMF\_RouteHandle}.
!       See ~(\ref{sec:weightfileformat}) for a description of the SCRIP weight
!       file format. Only "row", "col", and "S" variables are required. They
!       must be one-dimensionsal with dimension "n\_s".
!
! \item [routehandle]
!       Handle to the {\tt ESMF\_RouteHandle}.
!
! \item [transposeRoutehandle]
!     A handle to the transposed matrix operation is returned. The
!     transposed operation goes from {\tt dstArray} to {\tt srcArray}.
!
!   \item [{[ignoreUnmatchedIndices]}]
!     A logical flag that affects the behavior for when sequence indices
!     in the sparse matrix are encountered that do not have a match on the
!     {\tt srcField} or {\tt dstField} side. The default setting is
!     {\tt .false.}, indicating that it is an error when such a situation is
!     encountered. Setting {\tt ignoreUnmatchedIndices} to {\tt .true.} ignores
!     entries with unmatched indices.
!
!   \item [{[srcTermProcessing]}]
!     The {\tt srcTermProcessing} parameter controls how many source terms,
!     located on the same PET and summing into the same destination element,
!     are summed into partial sums on the source PET before being transferred
!     to the destination PET. A value of 0 indicates that the entire arithmetic
!     is done on the destination PET; source elements are neither multiplied
!     by their factors nor added into partial sums before being sent off by the
!     source PET. A value of 1 indicates that source elements are multiplied
!     by their factors on the source side before being sent to the destination
!     PET. Larger values of {\tt srcTermProcessing} indicate the maximum number
!     of terms in the partial sums on the source side.
!
!     Note that partial sums may lead to bit-for-bit differences in the results.
!     See section \ref{RH:bfb} for an in-depth discussion of {\em all}
!     bit-for-bit reproducibility aspects related to route-based communication
!     methods.
!
!     \begin{sloppypar}
!     The {\tt ESMF\_FieldSMMStore()} method implements an auto-tuning scheme
!     for the {\tt srcTermProcessing} parameter. The intent on the
!     {\tt srcTermProcessing} argument is "{\tt inout}" in order to
!     support both overriding and accessing the auto-tuning parameter.
!     If an argument $>= 0$ is specified, it is used for the
!     {\tt srcTermProcessing} parameter, and the auto-tuning phase is skipped.
!     In this case the {\tt srcTermProcessing} argument is not modified on
!     return. If the provided argument is $< 0$, the {\tt srcTermProcessing}
!     parameter is determined internally using the auto-tuning scheme. In this
!     case the {\tt srcTermProcessing} argument is re-set to the internally
!     determined value on return. Auto-tuning is also used if the optional
!     {\tt srcTermProcessing} argument is omitted.
!     \end{sloppypar}
!
!   \item [{[pipelineDepth]}]
!     The {\tt pipelineDepth} parameter controls how many messages a PET
!     may have outstanding during a sparse matrix exchange. Larger values
!     of {\tt pipelineDepth} typically lead to better performance. However,
!     on some systems too large a value may lead to performance degradation,
!     or runtime errors.
!
!     Note that the pipeline depth has no effect on the bit-for-bit
!     reproducibility of the results. However, it may affect the performance
!     reproducibility of the exchange.
!     The {\tt ESMF\_FieldSMMStore()} method implements an auto-tuning scheme
!     for the {\tt pipelineDepth} parameter. The intent on the
!     {\tt pipelineDepth} argument is "{\tt inout}" in order to
!     support both overriding and accessing the auto-tuning parameter.
!     If an argument $>= 0$ is specified, it is used for the
!     {\tt pipelineDepth} parameter, and the auto-tuning phase is skipped.
!     In this case the {\tt pipelineDepth} argument is not modified on
!     return. If the provided argument is $< 0$, the {\tt pipelineDepth}
!     parameter is determined internally using the auto-tuning scheme. In this
!     case the {\tt pipelineDepth} argument is re-set to the internally
!     determined value on return. Auto-tuning is also used if the optional
!     {\tt pipelineDepth} argument is omitted.
!
!   \item [{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!
! \end{description}
!
!EOP
!-------------------------------------------------------------------------------

      ! LOCAL VARIABLES:
      real(ESMF_KIND_R8), dimension(:), allocatable :: factorList
      integer, dimension(:, :), allocatable :: factorIndexList
      integer :: localrc

      real(ESMF_KIND_R8), pointer :: src(:,:)

      ! Initialize return code; assume routine not implemented
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! Fill the factorList and factorIndexList.
      call ESMF_FactorRead(filename, &
                           factorList, &
                           factorIndexList, &
                           rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! Generate routeHandle from factorList and factorIndexList
      call ESMF_FieldSMMStore(srcField=srcField, &
                              dstField=dstField, &
                              routehandle=routeHandle, &
                              factorList=factorList, &
                              factorIndexList=factorIndexList,   &
                              ignoreUnmatchedIndices=ignoreUnmatchedIndices, &
                              srcTermProcessing=srcTermProcessing, &
                              transposeRoutehandle=transposeRoutehandle, &
                              pipeLineDepth=pipeLineDepth, &
                              rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      deallocate(factorList)
      deallocate(factorIndexList)

      if (present(rc)) rc = ESMF_SUCCESS

    end subroutine ESMF_FieldSMMStoreFromFileTR

end module
