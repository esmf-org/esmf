! $Id$

#ifdef NAME_
#undef NAME_
#endif

#define NAME_ READ_PARALLEL_

#include "overload.macro"

subroutine SUB_ ( layout, DATA, UNIT, FORMAT, arrdes, RC)

 type (ESMF_DELayout)                         :: layout
#if (VARTYPE_ > 0)
 TYPE_(kind=EKIND_), intent(  OUT)            :: data DIMENSIONS_
#else
 character(LEN=*),   intent(  OUT)            :: data DIMENSIONS_
#endif
 integer,            intent(in   ),  optional :: UNIT
 character(len=*),   intent(in   ),  optional :: FORMAT
 type(ArrDescr),     optional, intent(INOUT ) :: ARRDES
 integer         ,   intent(  out),  optional :: RC

 character(len=ESMF_MAXSTR) :: FORMATTED
 character(LEN=ESMF_MAXSTR) :: FILENAME
 logical                :: IS_NAMED
 integer                :: USABLE_UNIT
 integer                :: IOSTAT
 integer                :: status
 character(len=ESMF_MAXSTR) :: IAM='READ_PARALLEL'
#if (RANK_ == 1 && VARTYPE_ == 4)
 integer                               :: nretries
#endif

 if(present(arrdes)) then
  if (MAPL_AM_I_ROOT(layout)) then
   if(arrdes%offset>0) then 
      call MPI_FILE_SEEK(UNIT, arrdes%offset, MPI_SEEK_SET, STATUS)
      VERIFY_(STATUS)
   endif

#if (RANK_ == 0) 
#if (VARTYPE_ == 0)
   call MPI_FILE_READ(UNIT, data, len(data), MPITYPE_, MPI_STATUS_IGNORE, STATUS)
#else
   call MPI_FILE_READ(UNIT, data, 1, MPITYPE_, MPI_STATUS_IGNORE, STATUS)
#endif
#else
   call MPI_FILE_READ(UNIT, data, size(data), MPITYPE_, MPI_STATUS_IGNORE, STATUS)
#endif
   VERIFY_(STATUS)
  endif

#if (RANK_ == 0)
#if (VARTYPE_ == 0)
   call MAPL_CommsBcast(layout, data, len(data), MAPL_Root, status)
#else
   call MAPL_CommsBcast(layout, data, 1, MAPL_Root, status)
#endif
#else
   call MAPL_CommsBcast(layout, data, size(data), MAPL_Root, status)
#endif
   VERIFY_(status)

#if (RANK_ == 0) 
#if (VARTYPE_ == 0)
   arrdes%offset = arrdes%offset + len(data)*4 + 8
#else
   arrdes%offset = arrdes%offset + 1*EKIND_ + 8
#endif
#else
   arrdes%offset = arrdes%offset + size(data)*EKIND_ + 8
#endif

   RETURN_(ESMF_SUCCESS)
 endif



 if (present(unit)) then
    USABLE_UNIT = unit
 else
    USABLE_UNIT = 5 ! fortran stdin
 end if

  if(USABLE_UNIT < 0 ) then

#if (RANK_ > 2 || VARTYPE_ <= 0)

     ASSERT_(.FALSE.)

#else

     ASSERT_(-USABLE_UNIT<=LAST_UNIT)
     munit => MEM_units(-USABLE_UNIT)
     munit%prevrec = munit%prevrec + 1
#if (RANK_ > 0)
     ASSERT_(associated(munit%Records(munit%prevrec)%TKR_))
#endif
     data = munit%Records(munit%prevrec)%TKR_

#endif

  else

     if (MAPL_AM_I_ROOT(layout)) then
        if (present(UNIT)) then
           inquire(unit=USABLE_UNIT, formatted=FORMATTED)
#if (RANK_ == 1 && VARTYPE_ == 4)
           nretries = 0
10         continue
#endif
           if   (FORMATTED == "YES") then
              if (present(FORMAT)) then; read(USABLE_UNIT, FORMAT, IOSTAT=IOSTAT) DATA
              else;    read(USABLE_UNIT, *, IOSTAT=IOSTAT) DATA
              end if
           elseif(FORMATTED == "NO") then
	      read(USABLE_UNIT, IOSTAT=IOSTAT) DATA
           end if
#if (RANK_ == 1 && VARTYPE_ == 4)
           if (iostat /= 0) then
              nretries = nretries + 1
              print *,trim(IAM), ' read fails on attempt ',nretries
              if (nretries < 3) goto 10
           endif
#endif

           if (IOSTAT < 0) then
              inquire(unit=USABLE_UNIT, NAMED=IS_NAMED, NAME=FILENAME)
              if (.not. IS_NAMED) then
                 FILENAME = 'UNNAMED'
              end if
              print *, "Premature end of file ",FILENAME
              RETURN_(ESMF_FAILURE)
           end if
        else
           if (present(FORMAT)) then; read(*, FORMAT ) DATA
           else; read          *, DATA
           end if
        end if
     end if

#if (RANK_ == 0)
#if (VARTYPE_ == 0)
     call MAPL_CommsBcast(layout, data, len(data), MAPL_Root, status)
#else
     call MAPL_CommsBcast(layout, data, 1, MAPL_Root, status)
#endif
#else
     call MAPL_CommsBcast(layout, data, size(data), MAPL_Root, status)
#endif
     VERIFY_(status)

  end if

  RETURN_(ESMF_SUCCESS)
END SUBROUTINE SUB_

!---------------------------
#undef NAME_

#undef DIMENSIONS_
#undef RANK_
#undef VARTYPE_
