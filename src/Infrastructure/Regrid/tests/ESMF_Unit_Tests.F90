! $Id: ESMF_Unit_Tests.F90,v 1.1 2005/10/20 20:21:55 svasquez Exp $
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

module ESMF_Unit_Test
implicit none

PRIVATE
PUBLIC AssertEqual

!Assert that 2 scalars are identical
interface AssertEqual
 module procedure AssertEqual_char
 module procedure AssertEqual_int
 module procedure AssertEqual_log
end interface

contains
#undef  ESMF_METHOD
#define ESMF_METHOD "AssertEqual_char"


function AssertEqual_char( varA, varB,comment, line, file )
logical :: AssertEqual_char
character, intent(in) :: varA, varB
character(len=*), optional, intent(in) :: comment
integer, optional :: line
CHARACTER(Len=*), optional :: file

character(len=35) :: comento

comento='                        '
if (present(comment)) comento=comment

if (varA .eq. varB) then
  AssertEqual_char = .true.
 !print*, '     >>>>PASS<<<<  AssertEqual: ', trim(comento)
 if (present(line))  print*, '     >>>>PASS<<<< ', line, file, comento
else
  AssertEqual_char = .false.
  print*, '     >>>>FAIL<<<<  AssertEqual: Comparison of ', trim(comento )
 if (present(line))  print*, '     >>>>FAIL<<<<  ', line, file, comento
end if

return
end function AssertEqual_char

#undef  ESMF_METHOD
#define ESMF_METHOD "AssertEqual_int"

function AssertEqual_int( varA, varB, comment, line, file )
logical :: AssertEqual_int
integer, intent(in) :: varA, varB
character(len=*), optional, intent(in) :: comment
integer, optional :: line
character(len=*), optional :: file

character(len=35) :: comento
comento=' '
if (present(comment)) comento=comment

if (varA .eq. varB) then
  AssertEqual_int = .true.
 !print*, '     >>>>PASS<<<< ', trim(comento)
 if (present(line))  print*, '     >>>>PASS<<<<  ', line, file, trim(comento)
else
  AssertEqual_int = .false.
 !print*, '     >>>>FAIL<<<<  ', trim(comento)  
 if (present(line))  print*, '     >>>>FAIL<<<<  ', line, file, trim(comento)
end if

return
end function AssertEqual_int

#undef  ESMF_METHOD
#define ESMF_METHOD "AssertEqual_log"

function AssertEqual_log( varA, varB, comment, line, file)
logical :: AssertEqual_log
logical, intent(in) :: varA, varB
character(len=*), optional, intent(in) :: comment
integer, optional :: line
character(len=*), optional :: file

character(len=35) :: comento

comento=' '
if (present(comment)) comento=comment

AssertEqual_log = (varA .and. varB) .or..not. (varA .or. varB) 

if (AssertEqual_log ) then
  if (present(line)) print*, '     >>>>PASS<<<<  ', line, trim(file),  trim(comento) 
else
  if (present(line)) print*, '     >>>>FAIL<<<<  ', line, trim(file), trim(comento) 
end if

return
end function AssertEqual_log
#undef  ESMF_METHOD


end module ESMF_Unit_Test

