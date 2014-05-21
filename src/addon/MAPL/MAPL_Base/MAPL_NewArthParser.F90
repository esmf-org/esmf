! +-======-+ 
!  Copyright (c) 2003-2007 United States Government as represented by 
!  the Admistrator of the National Aeronautics and Space Administration.  
!  All Rights Reserved.
!  
!  THIS OPEN  SOURCE  AGREEMENT  ("AGREEMENT") DEFINES  THE  RIGHTS  OF USE,
!  REPRODUCTION,  DISTRIBUTION,  MODIFICATION AND REDISTRIBUTION OF CERTAIN 
!  COMPUTER SOFTWARE ORIGINALLY RELEASED BY THE UNITED STATES GOVERNMENT AS 
!  REPRESENTED BY THE GOVERNMENT AGENCY LISTED BELOW ("GOVERNMENT AGENCY").  
!  THE UNITED STATES GOVERNMENT, AS REPRESENTED BY GOVERNMENT AGENCY, IS AN 
!  INTENDED  THIRD-PARTY  BENEFICIARY  OF  ALL  SUBSEQUENT DISTRIBUTIONS OR 
!  REDISTRIBUTIONS  OF THE  SUBJECT  SOFTWARE.  ANYONE WHO USES, REPRODUCES, 
!  DISTRIBUTES, MODIFIES  OR REDISTRIBUTES THE SUBJECT SOFTWARE, AS DEFINED 
!  HEREIN, OR ANY PART THEREOF,  IS,  BY THAT ACTION, ACCEPTING IN FULL THE 
!  RESPONSIBILITIES AND OBLIGATIONS CONTAINED IN THIS AGREEMENT.
!  
!  Government Agency: National Aeronautics and Space Administration
!  Government Agency Original Software Designation: GSC-15354-1
!  Government Agency Original Software Title:  GEOS-5 GCM Modeling Software
!  User Registration Requested.  Please Visit http://opensource.gsfc.nasa.gov
!  Government Agency Point of Contact for Original Software:  
!  			Dale Hithon, SRA Assistant, (301) 286-2691
!  
! +-======-+ 
#include "MAPL_Generic.h"
!
! Part of this code is based on a fortran parser by Roland Schmehl:
!
!------- -------- --------- --------- --------- --------- --------- --------- -------
! Fortran 90 function parser v1.1
!------- -------- --------- --------- --------- --------- --------- --------- -------
!
! This function parser module is intended for applications where a set of mathematical
! fortran-style expressions is specified at runtime and is then evaluated for a large 
! number of variable values. This is done by compiling the set of function strings 
! into byte code, which is interpreted efficiently for the various variable values. 
!
! The source code is available from http://fparser.sourceforge.net
!
!Documentation in orginial reads
!--------------------------------------------------------------------
!Copyright (c) 2000-2008, Roland Schmehl.

!All rights reserved.

!* Redistribution and use in source and binary forms, with or without
!modification, are permitted provided that the following conditions are
!met:

!* Redistributions of source code must retain the above copyright notice,
!this list of conditions and the following disclaimer.

!* Redistributions in binary form must reproduce the above copyright
!notice, this list of conditions and the following disclaimer in the
!documentation and/or other materials provided with the distribution.

!* Neither the name of the copyright holder nor the names of its
!contributors may be used to endorse or promote products derived from
!this software without specific prior written permission.


!THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
!"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
!LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
!A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT
!OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
!SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
!LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
!DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
!THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
!(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
!OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
!----------------------------------------------------------------------
! End of original license

MODULE MAPL_NewArthParserMod

  use ESMF
  use MAPL_BaseMod
  use MAPL_CommsMod

  IMPLICIT NONE
  !------- -------- --------- --------- --------- --------- --------- --------- -------
  PRIVATE

  PUBLIC                     :: MAPL_StateEval
  PUBLIC                     :: CheckSyntax
  PUBLIC                     :: RealNum
  PUBLIC                     :: LowCase

  INTEGER,                                  PARAMETER :: cImmed   = 1,          &
                                                         cNeg     = 2,          &
                                                         cAdd     = 3,          & 
                                                         cSub     = 4,          & 
                                                         cMul     = 5,          & 
                                                         cDiv     = 6,          & 
                                                         cPow     = 7,          & 
                                                         cAbs     = 8,          &
                                                         cExp     = 9,          &
                                                         cLog10   = 10,         &
                                                         cLog     = 11,         &
                                                         cSqrt    = 12,         &
                                                         cSinh    = 13,         &
                                                         cCosh    = 14,         &
                                                         cTanh    = 15,         &
                                                         cSin     = 16,         &
                                                         cCos     = 17,         &
                                                         cTan     = 18,         &
                                                         cAsin    = 19,         &
                                                         cAcos    = 20,         &
                                                         cAtan    = 21,         &
                                                         cHeav    = 22,         &
                                                         VarBegin = 23

  CHARACTER (LEN=1), DIMENSION(cAdd:cPow),  PARAMETER :: Ops      = (/ '+',     &
                                                                       '-',     &
                                                                       '*',     &
                                                                       '/',     &
                                                                       '^' /)

  CHARACTER (LEN=5), DIMENSION(cAbs:cHeav), PARAMETER :: Funcs    = (/ 'abs  ', &
                                                                       'exp  ', &
                                                                       'log10', &
                                                                       'log  ', &
                                                                       'sqrt ', &
                                                                       'sinh ', &
                                                                       'cosh ', &
                                                                       'tanh ', &
                                                                       'sin  ', &
                                                                       'cos  ', &
                                                                       'tan  ', &
                                                                       'asin ', &
                                                                       'acos ', &
                                                                       'atan ', &
                                                                       'heav ' /)
  TYPE tComp
     INTEGER, DIMENSION(:), POINTER     :: ByteCode
     INTEGER                            :: ByteCodeSize
     REAL,    DIMENSION(:), POINTER     :: Immed
     INTEGER                            :: ImmedSize
     TYPE(Ptrs_Type), DIMENSION(:), POINTER :: Stack
     INTEGER                            :: StackSize, &
                                           StackPtr
  END TYPE tComp

  type Ptrs_Type
     integer:: rank
     integer, dimension(ESMF_MAXDIM):: lb,ub
     real, pointer:: Q2D(:,:  ) => null()
     real, pointer:: Q3D(:,:,:) => null()
  end type Ptrs_Type

CONTAINS
 
  subroutine bytecode_dealloc(comp,rc)
     type(tComp),       intent(inout) :: comp
     integer, optional, intent(out  ) :: rc

     integer      :: i
     character(len=ESMF_MAXSTR), parameter :: Iam = "bytecode_dealloc"
     integer      :: status

     do i=1,comp%StackSize
        if (associated(comp%stack(i)%Q2D)) deallocate(comp%Stack(i)%Q2D)
        if (associated(comp%stack(i)%Q3D)) deallocate(comp%Stack(i)%Q3D)
     end do
     deallocate(comp%Stack)
     deallocate(comp%ByteCode)
     deallocate(comp%Immed)
     RETURN_(ESMF_SUCCESS)

  end subroutine bytecode_dealloc

  subroutine MAPL_StateEval(state,expression,field,rc)
    type(ESMF_State),        intent(in   ) :: state
    character(len=*),        intent(in   ) :: expression
    type(ESMF_Field),        intent(inout) :: field
    integer, optional,       intent(out  ) :: rc
    
    character(len=ESMF_MAXSTR), allocatable :: fieldNames(:)
    integer                              :: varCount

    type(ESMF_Field)                     :: state_field

    integer                              :: i
    type(tComp)                          :: pcode
    logical, allocatable                 :: needed(:)
    logical                              :: isConformal
    character(len=ESMF_MAXSTR), parameter :: Iam="MAPL_StateEval"
    integer :: status
    
    call ESMF_StateGet(state,ITEMCOUNT=varCount,rc=status)
    VERIFY_(STATUS)   
    allocate(fieldnames(varCount),needed(varCount))   
    call ESMF_StateGet(state,itemnamelist=fieldNames,rc=status)
    VERIFY_(STATUS)

    ! confirm that each needed field is conformal
    call CheckSyntax(expression,fieldNames,needed,rc=status)
    VERIFY_(STATUS)
    do i=1,varCount
       if (needed(i)) then
          call ESMF_StateGet(state,fieldNames(i),field=state_field,rc=status)
          VERIFY_(STATUS)

          isConformal = CheckIfConformal(field,state_field,rc=status)
          VERIFY_(STATUS)
          if (.not.isConformal) then
             ASSERT_(.FALSE.)
          end if
       end if
    end do

    call parsef (pcode, expression, fieldNames, field, rc=status)
    VERIFY_(STATUS)
    call evalf(pcode,state,fieldNames,field,rc=status)
    VERIFY_(STATUS)
    call bytecode_dealloc(pcode,rc=status)
    VERIFY_(STATUS)

    deallocate(fieldNames,needed)
        

    end subroutine MAPL_StateEval
  !
  SUBROUTINE parsef (Comp, FuncStr, Var, field, rc)
    !----- -------- --------- --------- --------- --------- --------- --------- -------
    ! Parse ith function string FuncStr and compile it into bytecode
    !----- -------- --------- --------- --------- --------- --------- --------- -------
    IMPLICIT NONE
    TYPE (tComp)                   , INTENT(inout) :: Comp              ! Bytecode
    CHARACTER (LEN=*),               INTENT(in   ) :: FuncStr   ! Function string
    CHARACTER (LEN=*), DIMENSION(:), INTENT(in   ) :: Var       ! Array with variable names
    TYPE(ESMF_Field)               , INTENT(inout) :: Field     ! resultant field, use to get rank, etc . . .
    INTEGER, OPTIONAL              , INTENT(out  ) :: rc

    INTEGER,   DIMENSION(:),  ALLOCATABLE :: ipos              ! Associates function strings
    CHARACTER(len=LEN(FuncStr))           :: Func
    character(len=ESMF_MAXSTR), parameter :: Iam="parsef"
    integer :: status
    !----- -------- --------- --------- --------- --------- --------- --------- -------
    CALL CheckSyntax (FuncStr,Var,rc=status)
    VERIFY_(STATUS)
    Func = FuncStr                                           ! Local copy of function string
    CALL Replace ('**','^ ',Func)                            ! Exponent into 1-Char. format
    CALL RemoveSpaces (Func)                                 ! Condense function string
    CALL Compile (comp,Func,Var,field,rc=status)             ! Compile into bytecode
    VERIFY_(STATUS)
    RETURN_(ESMF_SUCCESS)
  END SUBROUTINE parsef
  !
  SUBROUTINE evalf (Comp, State, FieldNames, ResField, rc)
    !----- -------- --------- --------- --------- --------- --------- --------- -------
    ! Evaluate bytecode of ith function for the values passed in array Val(:)
    !----- -------- --------- --------- --------- --------- --------- --------- -------
    IMPLICIT NONE
    TYPE (tComp),                   INTENT(inout) :: Comp
    TYPE(ESMF_State),               INTENT(in   ) :: state
    CHARACTER(len=ESMF_MAXSTR)    , INTENT(in   ) :: FieldNames(:)
    TYPE(ESMF_Field),               INTENT(inout) :: ResField
    INTEGER, OPTIONAL,              INTENT(out  ) :: rc
    INTEGER                            :: IP,              & ! Instruction pointer
                                          DP,              & ! Data pointer
                                          SP                 ! Stack pointer
    INTEGER                            :: CurrByte,ValNumber
    TYPE(ESMF_Field)                   :: state_field
    character(len=ESMF_MAXSTR), parameter :: Iam="evalf"
    integer :: status
    !----- -------- --------- --------- --------- --------- --------- --------- -------
    DP = 1
    SP = 0
    DO IP=1,Comp%ByteCodeSize
       CurrByte = Comp%ByteCode(IP)
       if (CurrByte == cImmed) then
          SP=SP+1
          call CopyScalarToField(Comp%Stack(SP),Comp%Immed(DP),rc=status)
          VERIFY_(STATUS)
          DP=DP+1
       end if
       if (CurrByte == cNeg) then
          call UnaryFuncField(Comp%Stack(SP),CurrByte,rc=status)
          VERIFY_(STATUS)
       end if
       if (CurrByte >= cAdd .and. CurrByte <= cPow) then
          call ArthFieldToField(Comp%Stack(SP),Comp%Stack(SP-1),CurrByte,rc=status)
          VERIFY_(STATUS)
          SP=SP-1
       end if
       if (CurrByte >= cAbs .and. CurrByte <= cHeav) then
          call UnaryFuncField(Comp%Stack(SP),CurrByte,rc=status)
          VERIFY_(STATUS)
       end if
       if (CurrByte > cHeav) then
          SP=SP+1
          ValNumber = CurrByte-VarBegin+1
          call ESMF_StateGet(state,FieldNames(ValNumber),state_field,rc=status)
          VERIFY_(STATUS)
          call CopyFieldToPtr(state_field,Comp%Stack(SP),rc=status)
          VERIFY_(STATUS)
       end if
    END DO
    call CopyPtrToField(Comp%Stack(1),ResField,rc=status)
    VERIFY_(STATUS)

    RETURN_(ESMF_SUCCESS)
  END SUBROUTINE evalf

  FUNCTION CheckIfConformal(field_1,field_2,rc) result(res)
     TYPE(ESMF_Field),      intent(inout) :: field_1
     TYPE(ESMF_Field),      intent(inout) :: field_2
     integer, optional,     intent(out  ) :: rc

     logical                               :: res

     character(len=ESMF_MAXSTR), parameter :: Iam ="CheckIfConformal"
     integer                               :: status
     type(ESMF_Array)                      :: array_1,array_2
     type (ESMF_LocalArray), target        :: larrayList(1)
     type(ESMF_LocalArray), pointer        :: larray_1,larray_2
     integer                               :: rank_1, rank_2
     integer                               :: lbnds_1(ESMF_MAXDIM), ubnds_1(ESMF_MAXDIM)
     integer                               :: lbnds_2(ESMF_MAXDIM), ubnds_2(ESMF_MAXDIM)
     integer                               :: i

     call ESMF_FieldGet(field_1,array=array_1,rc=status)
     VERIFY_(STATUS)
     call ESMF_ArrayGet(array_1, localarrayList=larrayList, rc=status)
     VERIFY_(STATUS)
     larray_1 => lArrayList(1) ! alias
     call ESMF_LocalArrayGet(larray_1,rank=rank_1,totalLBound=lbnds_1,totalUBound=ubnds_1,rc=status)
     VERIFY_(STATUS)

     call ESMF_FieldGet(field_2,array=array_2,rc=status)
     VERIFY_(STATUS)
     call ESMF_ArrayGet(array_2, localarrayList=larrayList, rc=status)
     VERIFY_(STATUS)
     larray_2 => lArrayList(1) ! alias
     call ESMF_LocalArrayGet(larray_2,rank=rank_2,totalLBound=lbnds_2,totalUBound=ubnds_2,rc=status)
     VERIFY_(STATUS)

     if (rank_1 == 2 .and.  rank_2 == 2) then
        do i=1,2
           if (lbnds_1(i) /= lbnds_2(i)) then
              res = .false.
           else if (ubnds_1(i) /= ubnds_2(i)) then
              res = .false.
           else
              res = .true.
           end if
        end do
     end if
     if (rank_1 == 3 .and.  rank_2 == 3) then
        do i=1,3
           if (lbnds_1(i) /= lbnds_2(i)) then
              res = .false.
           else if (ubnds_1(i) /= ubnds_2(i)) then
              res = .false.
           else
              res = .true.
           end if
        end do
     end if
     if (rank_1 == 3 .and.  rank_2 == 2) then
        do i=1,2
           if (lbnds_1(i) /= lbnds_2(i)) then
              res = .false.
           else if (ubnds_1(i) /= ubnds_2(i)) then
              res = .false.
           else
              res = .true.
           end if
        end do
     end if
     if (rank_1 == 2 .and.  rank_2 == 3) then
        do i=1,2
           if (lbnds_1(i) /= lbnds_2(i)) then
              res = .false.
           else if (ubnds_1(i) /= ubnds_2(i)) then
              res = .false.
           else
              res = .true.
           end if
        end do
     end if

     RETURN_(ESMF_SUCCESS)

  END FUNCTION CheckIfConformal

  SUBROUTINE CopyFieldtoPtr(field,ptrs,rc)
     ! take data from input field and copy to output field
     ! if input is 2D and output is 3D replicate 2D on each slice of 3D
     TYPE(ESMF_Field),      intent(inout) :: field
     TYPE(Ptrs_Type),      intent(inout) :: ptrs
     integer, optional,     intent(out  ) :: rc

     real, pointer        :: var2d(:,:), var3d(:,:,:)

     type(ESMF_Array)      :: array
     type(ESMF_LocalArray) :: larray
     integer               :: rank
     integer               :: lbnds(ESMF_MAXDIM), ubnds(ESMF_MAXDIM)
     character(len=ESMF_MAXSTR), parameter :: Iam="CopyFieldtoField"
     integer :: status
     integer :: i

     call ESMF_FieldGet(field,array=array,rc=status)
     VERIFY_(STATUS)
     call ESMF_ArrayGet(array,rank=rank,rc=status)
     VERIFY_(STATUS)
     if (rank == 3 .and. ptrs%rank ==3) then
        call ESMF_FieldGet(field,0,var3d,rc=status)
        VERIFY_(STATUS)
        ptrs%Q3D=var3d
     else if (rank == 2 .and. ptrs%rank ==2) then
        call ESMF_FieldGet(field,0,var2d,rc=status)
        VERIFY_(STATUS)
        ptrs%Q2D=var2d
     else if (rank == 2 .and. ptrs%rank ==3) then
        call ESMF_FieldGet(field,0,var2d,rc=status)
        VERIFY_(STATUS)
        do i=ptrs%lb(3),ptrs%ub(3)
           ptrs%Q3D(:,:,i)=var2d
        end do
     end if
     RETURN_(ESMF_SUCCESS)

  END SUBROUTINE CopyFieldToPtr

  SUBROUTINE CopyPtrtoField(ptrs,field,rc)
     ! take data from input field and copy to output field
     ! if input is 2D and output is 3D replicate 2D on each slice of 3D
     TYPE(ESMF_Field),      intent(inout) :: field
     TYPE(Ptrs_Type),      intent(inout) :: ptrs
     integer, optional,     intent(out  ) :: rc

     real, pointer        :: var2d(:,:), var3d(:,:,:)

     type(ESMF_Array)      :: array
     type(ESMF_LocalArray) :: larray
     integer               :: rank
     integer               :: lbnds(ESMF_MAXDIM), ubnds(ESMF_MAXDIM)
     character(len=ESMF_MAXSTR), parameter :: Iam="CopyFieldtoField"
     integer :: status
     integer :: i

     call ESMF_FieldGet(field,array=array,rc=status)
     VERIFY_(STATUS)
     call ESMF_ArrayGet(array,rank=rank,rc=status)
     VERIFY_(STATUS)
     if (rank == 3 .and. ptrs%rank ==3) then
        call ESMF_FieldGet(field,0,var3d,rc=status)
        VERIFY_(STATUS)
        var3d=ptrs%Q3D
     else if (rank == 2 .and. ptrs%rank ==2) then
        call ESMF_FieldGet(field,0,var2d,rc=status)
        VERIFY_(STATUS)
        var2d=ptrs%Q2D
     end if
     RETURN_(ESMF_SUCCESS)

  END SUBROUTINE CopyPtrToField


  SUBROUTINE ArthFieldToField(ptrs_1,ptrs_2,arthcode,rc)
     ! perform arthimetic operation indicated by input code between field_1 and field_2
     ! result will overwrite data in field_2
     TYPE(Ptrs_Type),       intent(inout) :: ptrs_1
     TYPE(Ptrs_Type),       intent(inout) :: ptrs_2
     integer,               intent(in   ) :: arthcode
     integer, optional,     intent(out  ) :: rc
     Character(len=ESMF_MAXSTR), parameter    :: Iam="ArthFieldToField"
     integer                                  :: status

     if (ptrs_1%rank == 3 .and. ptrs_2%rank ==3) then
        select case(arthcode)
           case(cAdd)
              where(ptrs_1%Q3D /= MAPL_UNDEF .and. ptrs_2%Q3D /= MAPL_UNDEF)
                 ptrs_2%Q3D = ptrs_2%Q3D + ptrs_1%Q3D
              else where
                 ptrs_2%Q3D = MAPL_UNDEF
              end where
           case(cSub)
              where(ptrs_1%Q3D /= MAPL_UNDEF .and. ptrs_2%Q3D /= MAPL_UNDEF)
                 ptrs_2%Q3D = ptrs_2%Q3D - ptrs_1%Q3D
              else where
                 ptrs_2%Q3D = MAPL_UNDEF
              end where
           case(cMul)
              where(ptrs_1%Q3D /= MAPL_UNDEF .and. ptrs_2%Q3D /= MAPL_UNDEF)
                 ptrs_2%Q3D = ptrs_2%Q3D * ptrs_1%Q3D
              else where
                 ptrs_2%Q3D = MAPL_UNDEF
              end where
           case(cDiv)
              where(ptrs_1%Q3D /= MAPL_UNDEF .and. ptrs_2%Q3D /= MAPL_UNDEF)
                 ptrs_2%Q3D = ptrs_2%Q3D / ptrs_1%Q3D
              else where
                 ptrs_2%Q3D = MAPL_UNDEF
              end where
           case(cPow)
              where(ptrs_1%Q3D /= MAPL_UNDEF .and. ptrs_2%Q3D /= MAPL_UNDEF)
                 ptrs_2%Q3D = ptrs_2%Q3D ** ptrs_1%Q3D
              else where
                 ptrs_2%Q3D = MAPL_UNDEF
              end where
        end select
     else if (ptrs_1%rank == 2 .and. ptrs_2%rank ==2) then
        select case(arthcode)
           case(cAdd)
              where(ptrs_1%Q2D /= MAPL_UNDEF .and. ptrs_2%Q2D /= MAPL_UNDEF)
                 ptrs_2%Q2D = ptrs_2%Q2D + ptrs_1%Q2D
              else where
                 ptrs_2%Q2D = MAPL_UNDEF
              end where
           case(cSub)
              where(ptrs_1%Q2D /= MAPL_UNDEF .and. ptrs_2%Q2D /= MAPL_UNDEF)
                 ptrs_2%Q2D = ptrs_2%Q2D - ptrs_1%Q2D
              else where
                 ptrs_2%Q2D = MAPL_UNDEF
              end where
           case(cMul)
              where(ptrs_1%Q2D /= MAPL_UNDEF .and. ptrs_2%Q2D /= MAPL_UNDEF)
                 ptrs_2%Q2D = ptrs_2%Q2D * ptrs_1%Q2D
              else where
                 ptrs_2%Q2D = MAPL_UNDEF
              end where
           case(cDiv)
              where(ptrs_1%Q2D /= MAPL_UNDEF .and. ptrs_2%Q2D /= MAPL_UNDEF)
                 ptrs_2%Q2D = ptrs_2%Q2D / ptrs_1%Q2D
              else where
                 ptrs_2%Q2D = MAPL_UNDEF
              end where
           case(cPow)
              where(ptrs_1%Q2D /= MAPL_UNDEF .and. ptrs_2%Q2D /= MAPL_UNDEF)
                 ptrs_2%Q2D = ptrs_2%Q2D ** ptrs_1%Q2D
              else where
                 ptrs_2%Q2D = MAPL_UNDEF
              end where
        end select
!    maybe put in 2d + 3d, not needed for now
     end if
     RETURN_(ESMF_SUCCESS)
   
  END SUBROUTINE ArthFieldToField

  SUBROUTINE UnaryFuncField(ptrs,funcCode,rc)
     ! perform arthimetic operation indicated by input code between field_1 and field_2
     ! result will overwrite data in field_2
     TYPE(ptrs_type),       intent(inout) :: ptrs
     integer,               intent(in   ) :: funcCode
     integer, optional,     intent(out  ) :: rc

     character(len=ESMF_MAXSTR), parameter :: Iam="UnaryFuncField"
     integer :: status

     if (ptrs%rank == 3) then
        select case(funcCode)
           case(cNeg)
              where(ptrs%Q3D /= MAPL_UNDEF)
                 ptrs%Q3D = -ptrs%Q3D
              else where
                 ptrs%Q3D = MAPL_UNDEF
              end where
           case(cAbs)
              where(ptrs%Q3D /= MAPL_UNDEF)
                 ptrs%Q3D = abs(ptrs%Q3D)
              else where
                 ptrs%Q3D = MAPL_UNDEF
              end where
           case(cExp)
              where(ptrs%Q3D /= MAPL_UNDEF)
                 ptrs%Q3D = exp(ptrs%Q3D)
              else where
                 ptrs%Q3D = MAPL_UNDEF
              end where
           case(cLog10)
              where(ptrs%Q3D /= MAPL_UNDEF)
                 ptrs%Q3D = log10(ptrs%Q3D)
              else where
                 ptrs%Q3D = MAPL_UNDEF
              end where
           case(cLog)
              where(ptrs%Q3D /= MAPL_UNDEF)
                 ptrs%Q3D = log(ptrs%Q3D)
              else where
                 ptrs%Q3D = MAPL_UNDEF
              end where
           case(cSqrt)
              where(ptrs%Q3D /= MAPL_UNDEF)
                 ptrs%Q3D = sqrt(ptrs%Q3D)
              else where
                 ptrs%Q3D = MAPL_UNDEF
              end where
           case(cSinh)
              where(ptrs%Q3D /= MAPL_UNDEF)
                 ptrs%Q3D = sinh(ptrs%Q3D)
              else where
                 ptrs%Q3D = MAPL_UNDEF
              end where
           case(cCosh)
              where(ptrs%Q3D /= MAPL_UNDEF)
                 ptrs%Q3D = cosh(ptrs%Q3D)
              else where
                 ptrs%Q3D = MAPL_UNDEF
              end where
           case(cTanh)
              where(ptrs%Q3D /= MAPL_UNDEF)
                 ptrs%Q3D = tanh(ptrs%Q3D)
              else where
                 ptrs%Q3D = MAPL_UNDEF
              end where
           case(cSin)
              where(ptrs%Q3D /= MAPL_UNDEF)
                 ptrs%Q3D = sin(ptrs%Q3D)
              else where
                 ptrs%Q3D = MAPL_UNDEF
              end where
           case(cCos)
              where(ptrs%Q3D /= MAPL_UNDEF)
                 ptrs%Q3D = cos(ptrs%Q3D)
              else where
                 ptrs%Q3D = MAPL_UNDEF
              end where
           case(cTan)
              where(ptrs%Q3D /= MAPL_UNDEF)
                 ptrs%Q3D = tan(ptrs%Q3D)
              else where
                 ptrs%Q3D = MAPL_UNDEF
              end where
           case(cAsin)
              where(ptrs%Q3D /= MAPL_UNDEF)
                 ptrs%Q3D = asin(ptrs%Q3D)
              else where
                 ptrs%Q3D = MAPL_UNDEF
              end where
           case(cAcos)
              where(ptrs%Q3D /= MAPL_UNDEF)
                 ptrs%Q3D = acos(ptrs%Q3D)
              else where
                 ptrs%Q3D = MAPL_UNDEF
              end where
           case(cAtan)
              where(ptrs%Q3D /= MAPL_UNDEF)
                 ptrs%Q3D = atan(ptrs%Q3D)
              else where
                 ptrs%Q3D = MAPL_UNDEF
              end where
           case(cHeav)
              where(ptrs%Q3D /= MAPL_UNDEF)
                 ptrs%Q3D = Heav3D(ptrs%Q3D)
              else where
                 ptrs%Q3D = MAPL_UNDEF
              end where
        end select
     else if (ptrs%rank == 2) then
        select case(funcCode)
           case(cNeg)
              where(ptrs%Q2D /= MAPL_UNDEF)
                 ptrs%Q2D = -ptrs%Q2D
              else where
                 ptrs%Q2D = MAPL_UNDEF
              end where
           case(cAbs)
              where(ptrs%Q2D /= MAPL_UNDEF)
                 ptrs%Q2D = abs(ptrs%Q2D)
              else where
                 ptrs%Q2D = MAPL_UNDEF
              end where
           case(cExp)
              where(ptrs%Q2D /= MAPL_UNDEF)
                 ptrs%Q2D = exp(ptrs%Q2D)
              else where
                 ptrs%Q2D = MAPL_UNDEF
              end where
           case(cLog10)
              where(ptrs%Q2D /= MAPL_UNDEF)
                 ptrs%Q2D = log10(ptrs%Q2D)
              else where
                 ptrs%Q2D = MAPL_UNDEF
              end where
           case(cLog)
              where(ptrs%Q2D /= MAPL_UNDEF)
                 ptrs%Q2D = log(ptrs%Q2D)
              else where
                 ptrs%Q2D = MAPL_UNDEF
              end where
           case(cSqrt)
              where(ptrs%Q2D /= MAPL_UNDEF)
                 ptrs%Q2D = sqrt(ptrs%Q2D)
              else where
                 ptrs%Q2D = MAPL_UNDEF
              end where
           case(cSinh)
              where(ptrs%Q2D /= MAPL_UNDEF)
                 ptrs%Q2D = sinh(ptrs%Q2D)
              else where
                 ptrs%Q2D = MAPL_UNDEF
              end where
           case(cCosh)
              where(ptrs%Q2D /= MAPL_UNDEF)
                 ptrs%Q2D = cosh(ptrs%Q2D)
              else where
                 ptrs%Q2D = MAPL_UNDEF
              end where
           case(cTanh)
              where(ptrs%Q2D /= MAPL_UNDEF)
                 ptrs%Q2D = tanh(ptrs%Q2D)
              else where
                 ptrs%Q2D = MAPL_UNDEF
              end where
           case(cSin)
              where(ptrs%Q2D /= MAPL_UNDEF)
                 ptrs%Q2D = sin(ptrs%Q2D)
              else where
                 ptrs%Q2D = MAPL_UNDEF
              end where
           case(cCos)
              where(ptrs%Q2D /= MAPL_UNDEF)
                 ptrs%Q2D = cos(ptrs%Q2D)
              else where
                 ptrs%Q2D = MAPL_UNDEF
              end where
           case(cTan)
              where(ptrs%Q2D /= MAPL_UNDEF)
                 ptrs%Q2D = tan(ptrs%Q2D)
              else where
                 ptrs%Q2D = MAPL_UNDEF
              end where
           case(cAsin)
              where(ptrs%Q2D /= MAPL_UNDEF)
                 ptrs%Q2D = asin(ptrs%Q2D)
              else where
                 ptrs%Q2D = MAPL_UNDEF
              end where
           case(cAcos)
              where(ptrs%Q2D /= MAPL_UNDEF)
                 ptrs%Q2D = acos(ptrs%Q2D)
              else where
                 ptrs%Q2D = MAPL_UNDEF
              end where
           case(cAtan)
              where(ptrs%Q2D /= MAPL_UNDEF)
                 ptrs%Q2D = atan(ptrs%Q2D)
              else where
                 ptrs%Q2D = MAPL_UNDEF
              end where
           case(cHeav)
              where(ptrs%Q2D /= MAPL_UNDEF)
                 ptrs%Q2D = Heav2D(ptrs%Q2D)
              else where
                 ptrs%Q2D = MAPL_UNDEF
              end where
        end select
     end if
     RETURN_(ESMF_SUCCESS)
   
  END SUBROUTINE UnaryFuncField

  SUBROUTINE CopyScalarToField(ptrs,rn,rc)
     ! copy a scalar to ESMF field
     TYPE(Ptrs_Type),      intent(inout)  :: ptrs
     real,                  intent(in   ) :: rn
     integer, optional,     intent(out  ) :: rc

     character(len=ESMF_MAXSTR), parameter :: Iam="CopyScalarToField"
     integer :: status

     if (ptrs%rank == 2) then
        ptrs%Q2D=rn
     else if (ptrs%rank == 3) then
        ptrs%Q3D=rn
     end if
     RETURN_(ESMF_SUCCESS)

  END SUBROUTINE CopyScalarToField
  !
  SUBROUTINE CheckSyntax (FuncStr,Var,needed,ExtVar,rc)
    !----- -------- --------- --------- --------- --------- --------- --------- -------
    ! Check syntax of function string,  returns 0 if syntax is ok
    !----- -------- --------- --------- --------- --------- --------- --------- -------
    IMPLICIT NONE
    CHARACTER (LEN=*),               INTENT(in) :: FuncStr   ! Original function string
    CHARACTER (LEN=*), DIMENSION(:), INTENT(in) :: Var       ! Array with variable names
    CHARACTER (LEN=*), OPTIONAL,     INTENT(inout) :: ExtVar
    LOGICAL, OPTIONAL                           :: needed(:)
    INTEGER, OPTIONAL                           :: rc
    INTEGER                                     :: n
    CHARACTER (LEN=1)                           :: c
    REAL                                        :: r
    LOGICAL                                     :: err
    INTEGER                                     :: ParCnt, & ! Parenthesis counter
                                                   j,ib,in,lFunc
    LOGICAL                                     :: isUndef
    INTEGER                                     :: status
    character(len=ESMF_MAXSTR)                  :: func
    integer, allocatable                        :: ipos(:)
    character(len=ESMF_MAXSTR), parameter       :: IAm="CheckSyntax"
    !----- -------- --------- --------- --------- --------- --------- --------- -------
    Func = FuncStr                                           ! Local copy of function string
    ALLOCATE (ipos(LEN_TRIM(FuncStr)))
    CALL Replace ('**','^ ',Func)                            ! Exponent into 1-Char. format
    CALL RemoveSpaces (Func,ipos)
    j = 1
    ParCnt = 0
    lFunc = LEN_TRIM(Func)
    if (present(needed)) needed = .false.
    step: DO
       IF (j > lFunc) CALL ParseErrMsg (j, FuncStr, ipos)
       c = Func(j:j)
       !-- -------- --------- --------- --------- --------- --------- --------- -------
       ! Check for valid operand (must appear)
       !-- -------- --------- --------- --------- --------- --------- --------- -------
       IF (c == '-' .OR. c == '+') THEN                      ! Check for leading - or +
          j = j+1
          IF (j > lFunc) THEN 
             CALL ParseErrMsg (j, FuncStr, ipos, 'Missing operand')
             ASSERT_(.FALSE.)
          END IF
          c = Func(j:j)
          IF (ANY(c == Ops)) THEN
             CALL ParseErrMsg (j, FuncStr, ipos, 'Multiple operators')
             ASSERT_(.FALSE.)
          END IF
       END IF
       n = MathFunctionIndex (Func(j:))
       IF (n > 0) THEN                                       ! Check for math function
          j = j+LEN_TRIM(Funcs(n))
          IF (j > lFunc) THEN 
             CALL ParseErrMsg (j, FuncStr, ipos, 'Missing function argument')
             ASSERT_(.FALSE.)
          END IF
          c = Func(j:j)
          IF (c /= '(') THEN 
             CALL ParseErrMsg (j, FuncStr, ipos, 'Missing opening parenthesis')
             ASSERT_(.FALSE.)
          END IF
       END IF
       IF (c == '(') THEN                                    ! Check for opening parenthesis
          ParCnt = ParCnt+1
          j = j+1
          CYCLE step
       END IF
       IF (SCAN(c,'0123456789.') > 0) THEN                   ! Check for number
          r = RealNum (Func(j:),ib,in,err)
          IF (err) THEN
             CALL ParseErrMsg (j, FuncStr, ipos, 'Invalid number format:  '//Func(j+ib-1:j+in-2))
             ASSERT_(.FALSE.)
          END IF
          j = j+in-1
          IF (j > lFunc) EXIT
          c = Func(j:j)
       ELSE                                                  ! Check for variable
          isUndef = checkUndef(Func(j:),ib,in)
          if (isUndef) then
             j = j+in-1
             IF (j> lFunc) EXIT
             c = Func(j:j)
          else
             n = VariableIndex (Func(j:),Var,ib,in)
             if (present(needed).and.(n>0)) needed(n)=.true.
             IF (n == 0) THEN
                IF (present(ExtVar)) then
                   ExtVar = trim(ExtVar)//Func(j+ib-1:j+in-2)//","
                ELSE
                   CALL ParseErrMsg (j, FuncStr, ipos, 'Invalid element: '//Func(j+ib-1:j+in-2))
                   ASSERT_(.FALSE.)
                ENDIF
             END IF
             j = j+in-1
             IF (j > lFunc) EXIT
             c = Func(j:j)
          end if
       END IF
       DO WHILE (c == ')')                                   ! Check for closing parenthesis
          ParCnt = ParCnt-1
          IF (ParCnt < 0) THEN
             CALL ParseErrMsg (j, FuncStr, ipos, 'Mismatched parenthesis')
             ASSERT_(.FALSE.)
          END IF
          IF (Func(j-1:j-1) == '(') THEN
             CALL ParseErrMsg (j-1, FuncStr, ipos, 'Empty parentheses')
             ASSERT_(.FALSE.)
          END IF
          j = j+1
          IF (j > lFunc) EXIT
          c = Func(j:j)
       END DO
       !-- -------- --------- --------- --------- --------- --------- --------- -------
       ! Now, we have a legal operand: A legal operator or end of string must follow
       !-- -------- --------- --------- --------- --------- --------- --------- -------
       IF (j > lFunc) EXIT
       IF (ANY(c == Ops)) THEN                               ! Check for multiple operators
          IF (j+1 > lFunc) THEN
             CALL ParseErrMsg (j, FuncStr, ipos)
             ASSERT_(.FALSE.)
          END IF
          IF (ANY(Func(j+1:j+1) == Ops)) THEN
             CALL ParseErrMsg (j+1, FuncStr, ipos, 'Multiple operators')
             ASSERT_(.FALSE.)
          END IF
       ELSE                                                  ! Check for next operand
          CALL ParseErrMsg (j, FuncStr, ipos, 'Missing operator')
          ASSERT_(.FALSE.)
       END IF
       !-- -------- --------- --------- --------- --------- --------- --------- -------
       ! Now, we have an operand and an operator: the next loop will check for another 
       ! operand (must appear)
       !-- -------- --------- --------- --------- --------- --------- --------- -------
       j = j+1
    END DO step
    IF (ParCnt > 0) THEN
       CALL ParseErrMsg (j, FuncStr, ipos, 'Missing )')
       ASSERT_(.FALSE.)
    END IF
    DEALLOCATE(ipos)
    RETURN_(ESMF_SUCCESS)
  END SUBROUTINE CheckSyntax

  SUBROUTINE ParseErrMsg (j, FuncStr, ipos, Msg)
    !----- -------- --------- --------- --------- --------- --------- --------- -------
    ! Print error message and terminate program
    !----- -------- --------- --------- --------- --------- --------- --------- -------
    IMPLICIT NONE
    INTEGER,                     INTENT(in) :: j
    CHARACTER (LEN=*),           INTENT(in) :: FuncStr       ! Original function string
    INTEGER, DIMENSION(:),       INTENT(inout) :: ipos
    CHARACTER (LEN=*), OPTIONAL, INTENT(in) :: Msg
    INTEGER                                 :: k
    !----- -------- --------- --------- --------- --------- --------- --------- -------
    IF (PRESENT(Msg)) THEN
       WRITE(*,*) '*** Error in syntax of function string: '//Msg
    ELSE
       WRITE(*,*) '*** Error in syntax of function string:'
    ENDIF
    WRITE(*,*)
    WRITE(*,'(A)') ' '//FuncStr
    DO k=1,ipos(j)
       WRITE(*,'(A)',ADVANCE='NO') ' '                       ! Advance to the jth position
    END DO
    WRITE(*,'(A)') '?'
  END SUBROUTINE ParseErrMsg
  !
  FUNCTION OperatorIndex (c) RESULT (n)
    !----- -------- --------- --------- --------- --------- --------- --------- -------
    ! Return operator index
    !----- -------- --------- --------- --------- --------- --------- --------- -------
    IMPLICIT NONE
    CHARACTER (LEN=1), INTENT(in) :: c
    INTEGER                       :: n,j
    !----- -------- --------- --------- --------- --------- --------- --------- -------
    n = 0
    DO j=cAdd,cPow
       IF (c == Ops(j)) THEN
          n = j
          EXIT
       END IF
    END DO
  END FUNCTION OperatorIndex
  !
  FUNCTION MathFunctionIndex (str) RESULT (n)
    !----- -------- --------- --------- --------- --------- --------- --------- -------
    ! Return index of math function beginnig at 1st position of string str
    !----- -------- --------- --------- --------- --------- --------- --------- -------
    IMPLICIT NONE
    CHARACTER (LEN=*), INTENT(in) :: str
    INTEGER                       :: n,j
    INTEGER                       :: k
    CHARACTER (LEN=LEN(Funcs))    :: fun
    !----- -------- --------- --------- --------- --------- --------- --------- -------
    n = 0
    DO j=cAbs,cHeav                                          ! Check all math functions
       k = MIN(LEN_TRIM(Funcs(j)), LEN(str))   
       CALL LowCase (str(1:k), fun)
       IF (fun == Funcs(j)) THEN                             ! Compare lower case letters
          n = j                                              ! Found a matching function
          EXIT
       END IF
    END DO
  END FUNCTION MathFunctionIndex
  !
  FUNCTION VariableIndex (str, Var, ibegin, inext) RESULT (n)
    !----- -------- --------- --------- --------- --------- --------- --------- -------
    ! Return index of variable at begin of string str (returns 0 if no variable found)
    !----- -------- --------- --------- --------- --------- --------- --------- -------
    IMPLICIT NONE
    CHARACTER (LEN=*),               INTENT(in) :: str       ! String
    CHARACTER (LEN=*), DIMENSION(:), INTENT(in) :: Var       ! Array with variable names
    INTEGER                                     :: n         ! Index of variable
    INTEGER, OPTIONAL,              INTENT(out) :: ibegin, & ! Start position of variable name
                                                   inext     ! Position of character after name
    INTEGER                                     :: j,ib,in,lstr
    !----- -------- --------- --------- --------- --------- --------- --------- -------
    n = 0
    lstr = LEN_TRIM(str)
    IF (lstr > 0) THEN
       DO ib=1,lstr                                          ! Search for first character in str
          IF (str(ib:ib) /= ' ') EXIT                        ! When lstr>0 at least 1 char in str
       END DO                        
       DO in=ib,lstr                                         ! Search for name terminators
          IF (SCAN(str(in:in),'+-*/^) ') > 0) EXIT
       END DO
       DO j=1,SIZE(Var)
          IF (str(ib:in-1) == Var(j)) THEN                     
             n = j                                           ! Variable name found
             EXIT
          END IF
       END DO
    END IF
    IF (PRESENT(ibegin)) ibegin = ib
    IF (PRESENT(inext))  inext  = in
  END FUNCTION VariableIndex

  FUNCTION checkUndef (str, ibegin, inext) RESULT (isUndef)
    !----- -------- --------- --------- --------- --------- --------- --------- -------
    ! Return index of variable at begin of string str (returns 0 if no variable found)
    !----- -------- --------- --------- --------- --------- --------- --------- -------
    IMPLICIT NONE
    CHARACTER (LEN=*),               INTENT(in) :: str       ! String
    LOGICAL                                     :: isUndef   ! Index of variable
    INTEGER, OPTIONAL,              INTENT(out) :: ibegin, & ! Start position of variable name
                                                   inext     ! Position of character after name
    INTEGER                                     :: j,ib,in,lstr
    CHARACTER (LEN=ESMF_MAXSTR)                 :: fun
    !----- -------- --------- --------- --------- --------- --------- --------- -------
    isUndef = .false.
    lstr = LEN_TRIM(str)
    IF (lstr > 0) THEN
       DO ib=1,lstr                                          ! Search for first character in str
          IF (str(ib:ib) /= ' ') EXIT                        ! When lstr>0 at least 1 char in str
       END DO                        
       DO in=ib,lstr                                         ! Search for name terminators
          IF (SCAN(str(in:in),'+-*/^) ') > 0) EXIT
       END DO
       CALL LowCase (str(ib:in-1), fun)
       IF (trim(fun) == 'undef') THEN                     
             isUndef = .true.                           ! Variable name found
       END IF
    END IF
    IF (PRESENT(ibegin)) ibegin = ib
    IF (PRESENT(inext))  inext  = in
  END FUNCTION checkUndef
  !
  SUBROUTINE RemoveSpaces (str, ipos)
    !----- -------- --------- --------- --------- --------- --------- --------- -------
    ! Remove Spaces from string, remember positions of characters in old string
    !----- -------- --------- --------- --------- --------- --------- --------- -------
    IMPLICIT NONE
    CHARACTER (LEN=*), INTENT(inout) :: str
    INTEGER, OPTIONAL, INTENT(inout) :: ipos(:)
    INTEGER                          :: k,lstr
    !----- -------- --------- --------- --------- --------- --------- --------- -------
    lstr = LEN_TRIM(str)
    if (present(ipos)) ipos = (/ (k,k=1,lstr) /)
    k = 1
    DO WHILE (str(k:lstr) /= ' ')                             
       IF (str(k:k) == ' ') THEN
          str(k:lstr)  = str(k+1:lstr)//' '                  ! Move 1 character to left
          if (present(ipos)) ipos(k:lstr) = (/ ipos(k+1:lstr), 0 /)  ! Move 1 element to left
          k = k-1
       END IF
       k = k+1
    END DO
  END SUBROUTINE RemoveSpaces
  !
  SUBROUTINE Replace (ca,cb,str)
    !----- -------- --------- --------- --------- --------- --------- --------- -------
    ! Replace ALL appearances of character set ca in string str by character set cb
    !----- -------- --------- --------- --------- --------- --------- --------- -------
    IMPLICIT NONE
    CHARACTER (LEN=*),       INTENT(in) :: ca
    CHARACTER (LEN=LEN(ca)), INTENT(in) :: cb                ! LEN(ca) must be LEN(cb)
    CHARACTER (LEN=*),    INTENT(inout) :: str
    INTEGER                             :: j,lca
    !----- -------- --------- --------- --------- --------- --------- --------- -------
    lca = LEN(ca)
    DO j=1,LEN_TRIM(str)-lca+1
       IF (str(j:j+lca-1) == ca) str(j:j+lca-1) = cb
    END DO
  END SUBROUTINE Replace
  !
  SUBROUTINE Compile (Comp, F, Var, field, rc)
    !----- -------- --------- --------- --------- --------- --------- --------- -------
    ! Compile i-th function string F into bytecode
    !----- -------- --------- --------- --------- --------- --------- --------- -------
    IMPLICIT NONE
    TYPE (tComp) :: Comp              ! Bytecode
    CHARACTER (LEN=*),               INTENT(in   ) :: F         ! Function string
    CHARACTER (LEN=*), DIMENSION(:), INTENT(in   ) :: Var       ! Array with variable names
    TYPE(ESMF_Field)               , INTENT(inout) :: field     ! resultant field, use to get its rank, etc . . . 
    INTEGER                        , INTENT(out  ) :: rc
    INTEGER                                     :: istat, i
    TYPE(ESMF_Array)                            :: Array
    type (ESMF_LocalArray), target        :: larrayList(1)
    TYPE(ESMF_LocalArray) ,pointer        :: lArray
    INTEGER                          :: ResRank
    INTEGER                          :: lb(ESMF_MAXDIM)
    INTEGER                          :: ub(ESMF_MAXDIM)
    character(len=ESMF_MAXSTR), parameter       :: Iam = "Compile"
    integer                                     :: status
    !----- -------- --------- --------- --------- --------- --------- --------- -------
    IF (ASSOCIATED(Comp%ByteCode)) DEALLOCATE ( Comp%ByteCode, &
                                                   Comp%Immed,    &
                                                   Comp%Stack     )
    Comp%ByteCodeSize = 0
    Comp%ImmedSize    = 0
    Comp%StackSize    = 0
    Comp%StackPtr     = 0
    CALL CompileSubstr (Comp,F,1,LEN_TRIM(F),Var)               ! Compile string to determine size
    ALLOCATE ( Comp%ByteCode(Comp%ByteCodeSize), & 
               Comp%Immed(Comp%ImmedSize),       &
               Comp%Stack(Comp%StackSize),       &
               STAT = istat                      )

    call ESMF_FieldGet(field,array=array,rc=status)
    VERIFY_(STATUS)
    call ESMF_ArrayGet(array,localarrayList=larrayList,rc=status)
    VERIFY_(STATUS)
    lArray => lArrayList(1)
    call ESMF_LocalArrayGet(larray,rank=ResRank,totallbound=lb,totalubound=ub,rc=status)
    VERIFY_(STATUS)
    DO i=1,Comp%StackSize
       Comp%Stack(i)%rank = ResRank
       Comp%Stack(i)%lb = lb
       Comp%Stack(i)%ub = ub
       IF (ResRank == 2) then
          allocate(Comp%Stack(i)%Q2D(lb(1):ub(1),lb(2):ub(2)) )
       ELSE IF (ResRank == 3) then
          allocate(Comp%Stack(i)%Q3D(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3)) )
       END IF
    END DO

    Comp%ByteCodeSize = 0
    Comp%ImmedSize    = 0
    Comp%StackSize    = 0
    Comp%StackPtr     = 0
    CALL CompileSubstr (Comp,F,1,LEN_TRIM(F),Var)            ! Compile string into bytecode

    RETURN_(ESMF_SUCCESS)

  END SUBROUTINE Compile
  !
  SUBROUTINE AddCompiledByte (Comp, b)
    !----- -------- --------- --------- --------- --------- --------- --------- -------
    ! Add compiled byte to bytecode
    !----- -------- --------- --------- --------- --------- --------- --------- -------
    IMPLICIT NONE
    TYPE (tComp) :: Comp              ! Bytecode
    INTEGER, INTENT(in) :: b                             ! Value of byte to be added
    !----- -------- --------- --------- --------- --------- --------- --------- -------
    Comp%ByteCodeSize = Comp%ByteCodeSize + 1
    IF (ASSOCIATED(Comp%ByteCode)) Comp%ByteCode(Comp%ByteCodeSize) = b
  END SUBROUTINE AddCompiledByte
  !
  FUNCTION MathItemIndex (Comp, F, Var) RESULT (n)
    !----- -------- --------- --------- --------- --------- --------- --------- -------
    ! Return math item index, if item is real number, enter it into Comp-structure
    !----- -------- --------- --------- --------- --------- --------- --------- -------
    IMPLICIT NONE
    TYPE (tComp) :: Comp              ! Bytecode
    CHARACTER (LEN=*),               INTENT(in) :: F         ! Function substring
    CHARACTER (LEN=*), DIMENSION(:), INTENT(in) :: Var       ! Array with variable names
    INTEGER                                     :: n         ! Byte value of math item
    LOGICAL                                     :: isUndef   ! logical for undef
    !----- -------- --------- --------- --------- --------- --------- --------- -------
    n = 0
    IF (SCAN(F(1:1),'0123456789.') > 0) THEN                 ! Check for begin of a number
       Comp%ImmedSize = Comp%ImmedSize + 1
       IF (ASSOCIATED(Comp%Immed)) Comp%Immed(Comp%ImmedSize) = RealNum (F)
       n = cImmed
    ELSE
       isUndef = checkUndef(F)
       IF (isUndef) THEN
          Comp%ImmedSize = Comp%ImmedSize + 1
          IF (ASSOCIATED(Comp%Immed)) Comp%Immed(Comp%ImmedSize) = MAPL_UNDEF
          n = cImmed
       ELSE
          n = VariableIndex (F, Var)
          IF (n > 0) n = VarBegin+n-1
       END IF
    END IF
  END FUNCTION MathItemIndex
  !
  FUNCTION CompletelyEnclosed (F, b, e) RESULT (res)
    !----- -------- --------- --------- --------- --------- --------- --------- -------
    ! Check if function substring F(b:e) is completely enclosed by a pair of parenthesis
    !----- -------- --------- --------- --------- --------- --------- --------- -------
    IMPLICIT NONE
    CHARACTER (LEN=*), INTENT(in) :: F                       ! Function substring
    INTEGER,           INTENT(in) :: b,e                     ! First and last pos. of substring
    LOGICAL                       :: res
    INTEGER                       :: j,k
    !----- -------- --------- --------- --------- --------- --------- --------- -------
    res=.false.
    IF (F(b:b) == '(' .AND. F(e:e) == ')') THEN
       k = 0
       DO j=b+1,e-1
          IF     (F(j:j) == '(') THEN
             k = k+1
          ELSEIF (F(j:j) == ')') THEN
             k = k-1
          END IF
          IF (k < 0) EXIT
       END DO
       IF (k == 0) res=.true.                                ! All opened parenthesis closed
    END IF
  END FUNCTION CompletelyEnclosed
  !
  RECURSIVE SUBROUTINE CompileSubstr (Comp, F, b, e, Var)
    !----- -------- --------- --------- --------- --------- --------- --------- -------
    ! Compile i-th function string F into bytecode
    !----- -------- --------- --------- --------- --------- --------- --------- -------
    IMPLICIT NONE
    TYPE (tComp) :: Comp              ! Bytecode
    CHARACTER (LEN=*),               INTENT(in) :: F         ! Function substring
    INTEGER,                         INTENT(in) :: b,e       ! Begin and end position substring
    CHARACTER (LEN=*), DIMENSION(:), INTENT(in) :: Var       ! Array with variable names
    INTEGER                                     :: n
    INTEGER                                     :: b2,j,k,io
    CHARACTER (LEN=*),                PARAMETER :: calpha = 'abcdefghijklmnopqrstuvwxyz'// &
                                                            'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
    !----- -------- --------- --------- --------- --------- --------- --------- -------
    ! Check for special cases of substring
    !----- -------- --------- --------- --------- --------- --------- --------- -------
!    write(*,'(A25,I5,I5)')trim(F(b:e)),b,e !bma
    IF     (F(b:b) == '+') THEN                              ! Case 1: F(b:e) = '+...'
!      WRITE(*,*)'1. F(b:e) = "+..."'
       CALL CompileSubstr (Comp, F, b+1, e, Var)
       RETURN
    ELSEIF (CompletelyEnclosed (F, b, e)) THEN               ! Case 2: F(b:e) = '(...)'
!      WRITE(*,*)'2. F(b:e) = "(...)"'
       CALL CompileSubstr (Comp, F, b+1, e-1, Var)
       RETURN
    ELSEIF (SCAN(F(b:b),calpha) > 0) THEN        
       n = MathFunctionIndex (F(b:e))
       IF (n > 0) THEN
          b2 = b+INDEX(F(b:e),'(')-1
          IF (CompletelyEnclosed(F, b2, e)) THEN             ! Case 3: F(b:e) = 'fcn(...)'
!            WRITE(*,*)'3. F(b:e) = "fcn(...)"'
             CALL CompileSubstr(Comp, F, b2+1, e-1, Var)
             CALL AddCompiledByte (Comp, n)
             RETURN
          END IF
       END IF
    ELSEIF (F(b:b) == '-') THEN
       IF (CompletelyEnclosed (F, b+1, e)) THEN              ! Case 4: F(b:e) = '-(...)'
!         WRITE(*,*)'4. F(b:e) = "-(...)"'
          CALL CompileSubstr (Comp, F, b+2, e-1, Var)
          CALL AddCompiledByte (Comp, cNeg)
          RETURN
       ELSEIF (SCAN(F(b+1:b+1),calpha) > 0) THEN
          n = MathFunctionIndex (F(b+1:e))
          IF (n > 0) THEN
             b2 = b+INDEX(F(b+1:e),'(')
             IF (CompletelyEnclosed(F, b2, e)) THEN          ! Case 5: F(b:e) = '-fcn(...)'
!               WRITE(*,*)'5. F(b:e) = "-fcn(...)"'
                CALL CompileSubstr(Comp, F, b2+1, e-1, Var)
                CALL AddCompiledByte (Comp, n)
                CALL AddCompiledByte (Comp, cNeg)
                RETURN
             END IF
          END IF
       ENDIF
    END IF
    !----- -------- --------- --------- --------- --------- --------- --------- -------
    ! Check for operator in substring: check only base level (k=0), exclude expr. in ()
    !----- -------- --------- --------- --------- --------- --------- --------- -------    
    DO io=cAdd,cPow                                          ! Increasing priority +-*/^
       k = 0
       DO j=e,b,-1
          IF     (F(j:j) == ')') THEN
             k = k+1
          ELSEIF (F(j:j) == '(') THEN
             k = k-1
          END IF
          IF (k == 0 .AND. F(j:j) == Ops(io) .AND. IsBinaryOp (j, F)) THEN
             IF (ANY(F(j:j) == Ops(cMul:cPow)) .AND. F(b:b) == '-') THEN ! Case 6: F(b:e) = '-...Op...' with Op > -
!               WRITE(*,*)'6. F(b:e) = "-...Op..." with Op > -'
                CALL CompileSubstr (Comp, F, b+1, e, Var)
                CALL AddCompiledByte (Comp, cNeg)
                RETURN                 
             ELSE                                                        ! Case 7: F(b:e) = '...BinOp...'
!               WRITE(*,*)'7. Binary operator ',F(j:j)
                CALL CompileSubstr (Comp, F, b, j-1, Var)
                CALL CompileSubstr (Comp, F, j+1, e, Var)
                CALL AddCompiledByte (Comp, OperatorIndex(Ops(io)))
                Comp%StackPtr = Comp%StackPtr - 1
                RETURN
             END IF
          END IF
       END DO
    END DO
    !----- -------- --------- --------- --------- --------- --------- --------- -------
    ! Check for remaining items, i.e. variables or explicit numbers
    !----- -------- --------- --------- --------- --------- --------- --------- -------
    b2 = b
    IF (F(b:b) == '-') b2 = b2+1
    n = MathItemIndex(Comp, F(b2:e), Var)
!   WRITE(*,*)'8. AddCompiledByte ',n
    CALL AddCompiledByte (Comp, n)
    Comp%StackPtr = Comp%StackPtr + 1
    IF (Comp%StackPtr > Comp%StackSize) Comp%StackSize = Comp%StackSize + 1
    IF (b2 > b) CALL AddCompiledByte (Comp, cNeg)
  END SUBROUTINE CompileSubstr
  !
  FUNCTION IsBinaryOp (j, F) RESULT (res)
    !----- -------- --------- --------- --------- --------- --------- --------- -------
    ! Check if operator F(j:j) in string F is binary operator
    ! Special cases already covered elsewhere:              (that is corrected in v1.1)
    ! - operator character F(j:j) is first character of string (j=1)
    !----- -------- --------- --------- --------- --------- --------- --------- -------
    IMPLICIT NONE
    INTEGER,           INTENT(in) :: j                       ! Position of Operator
    CHARACTER (LEN=*), INTENT(in) :: F                       ! String
    LOGICAL                       :: res                     ! Result
    INTEGER                       :: k
    LOGICAL                       :: Dflag,Pflag
    !----- -------- --------- --------- --------- --------- --------- --------- -------
    res=.true.
    IF (F(j:j) == '+' .OR. F(j:j) == '-') THEN               ! Plus or minus sign:
       IF (j == 1) THEN                                      ! - leading unary operator ?
          res = .false.
       ELSEIF (SCAN(F(j-1:j-1),'+-*/^(') > 0) THEN           ! - other unary operator ?
          res = .false.
       ELSEIF (SCAN(F(j+1:j+1),'0123456789') > 0 .AND. &     ! - in exponent of real number ?
               SCAN(F(j-1:j-1),'eEdD')       > 0) THEN
          Dflag=.false.; Pflag=.false.
          k = j-1
          DO WHILE (k > 1)                                   !   step to the left in mantissa 
             k = k-1
             IF     (SCAN(F(k:k),'0123456789') > 0) THEN
                Dflag=.true.
             ELSEIF (F(k:k) == '.') THEN
                IF (Pflag) THEN
                   EXIT                                      !   * EXIT: 2nd appearance of '.'
                ELSE
                   Pflag=.true.                              !   * mark 1st appearance of '.'
                ENDIF
             ELSE
                EXIT                                         !   * all other characters
             END IF
          END DO
          IF (Dflag .AND. (k == 1 .OR. SCAN(F(k:k),'+-*/^(') > 0)) res = .false.
       END IF
    END IF
  END FUNCTION IsBinaryOp
  !
  FUNCTION RealNum (str, ibegin, inext, error) RESULT (res)
    !----- -------- --------- --------- --------- --------- --------- --------- -------
    ! Get real number from string - Format: [blanks][+|-][nnn][.nnn][e|E|d|D[+|-]nnn]
    !----- -------- --------- --------- --------- --------- --------- --------- -------
    IMPLICIT NONE
    CHARACTER (LEN=*),  INTENT(in) :: str                    ! String
    REAL                           :: res                    ! Real number
    INTEGER, OPTIONAL, INTENT(out) :: ibegin,              & ! Start position of real number
                                      inext                  ! 1st character after real number
    LOGICAL, OPTIONAL, INTENT(out) :: error                  ! Error flag
    INTEGER                        :: ib,in,istat
    LOGICAL                        :: Bflag,               & ! .T. at begin of number in str
                                      InMan,               & ! .T. in mantissa of number
                                      Pflag,               & ! .T. after 1st '.' encountered
                                      Eflag,               & ! .T. at exponent identifier 'eEdD'
                                      InExp,               & ! .T. in exponent of number
                                      DInMan,              & ! .T. if at least 1 digit in mant.
                                      DInExp,              & ! .T. if at least 1 digit in exp.
                                      err                    ! Local error flag
    !----- -------- --------- --------- --------- --------- --------- --------- -------
    Bflag=.true.; InMan=.false.; Pflag=.false.; Eflag=.false.; InExp=.false.
    DInMan=.false.; DInExp=.false.
    ib   = 1
    in   = 1
    DO WHILE (in <= LEN_TRIM(str))
       SELECT CASE (str(in:in))
       CASE (' ')                                            ! Only leading blanks permitted
          ib = ib+1
          IF (InMan .OR. Eflag .OR. InExp) EXIT
       CASE ('+','-')                                        ! Permitted only
          IF     (Bflag) THEN           
             InMan=.true.; Bflag=.false.                     ! - at beginning of mantissa
          ELSEIF (Eflag) THEN               
             InExp=.true.; Eflag=.false.                     ! - at beginning of exponent
          ELSE
             EXIT                                            ! - otherwise STOP
          ENDIF
       CASE ('0':'9')                                        ! Mark
          IF     (Bflag) THEN           
             InMan=.true.; Bflag=.false.                     ! - beginning of mantissa
          ELSEIF (Eflag) THEN               
             InExp=.true.; Eflag=.false.                     ! - beginning of exponent
          ENDIF
          IF (InMan) DInMan=.true.                           ! Mantissa contains digit
          IF (InExp) DInExp=.true.                           ! Exponent contains digit
       CASE ('.')
          IF     (Bflag) THEN
             Pflag=.true.                                    ! - mark 1st appearance of '.'
             InMan=.true.; Bflag=.false.                     !   mark beginning of mantissa
          ELSEIF (InMan .AND..NOT.Pflag) THEN
             Pflag=.true.                                    ! - mark 1st appearance of '.'
          ELSE
             EXIT                                            ! - otherwise STOP
          END IF
       CASE ('e','E','d','D')                                ! Permitted only
          IF (InMan) THEN
             Eflag=.true.; InMan=.false.                     ! - following mantissa
          ELSE
             EXIT                                            ! - otherwise STOP
          ENDIF
       CASE DEFAULT
          EXIT                                               ! STOP at all other characters
       END SELECT
       in = in+1
    END DO
    err = (ib > in-1) .OR. (.NOT.DInMan) .OR. ((Eflag.OR.InExp).AND..NOT.DInExp)
    IF (err) THEN
       res = 0.0
    ELSE
       READ(str(ib:in-1),*,IOSTAT=istat) res
       err = istat /= 0
    END IF
    IF (PRESENT(ibegin)) ibegin = ib
    IF (PRESENT(inext))  inext  = in
    IF (PRESENT(error))  error  = err
  END FUNCTION RealNum
  !  
  SUBROUTINE LowCase (str1, str2)
    !----- -------- --------- --------- --------- --------- --------- --------- -------
    ! Transform upper case letters in str1 into lower case letters, result is str2
    !----- -------- --------- --------- --------- --------- --------- --------- -------
    IMPLICIT NONE
    CHARACTER (LEN=*),  INTENT(in) :: str1
    CHARACTER (LEN=*), INTENT(out) :: str2
    INTEGER                        :: j,k
    CHARACTER (LEN=*),   PARAMETER :: lc = 'abcdefghijklmnopqrstuvwxyz'
    CHARACTER (LEN=*),   PARAMETER :: uc = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
    !----- -------- --------- --------- --------- --------- --------- --------- -------
    str2 = str1
    DO j=1,LEN_TRIM(str1)
       k = INDEX(uc,str1(j:j))
       IF (k > 0) str2(j:j) = lc(k:k)
    END DO
  END SUBROUTINE LowCase

  FUNCTION Heav2D(r) RESULT(res)
    IMPLICIT NONE
    real,   intent(in) :: r(:,:)
    real               :: res(size(r,1),size(r,2))

    where(r < 0.0)
       res = 0.0
    elsewhere
       res = 1.0
    endwhere
  END FUNCTION Heav2D

  FUNCTION Heav3D(r) RESULT(res)
    IMPLICIT NONE
    real,   intent(in) :: r(:,:,:)
    real               :: res(size(r,1),size(r,2),size(r,3))

    where(r < 0.0)
       res = 0.0
    elsewhere
       res = 1.0
    endwhere
  END FUNCTION Heav3D

END MODULE MAPL_NewArthParserMod
