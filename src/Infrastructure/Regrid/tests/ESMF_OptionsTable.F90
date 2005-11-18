! $Id: ESMF_OptionsTable.F90,v 1.2 2005/11/18 21:38:02 svasquez Exp $
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

#define ESMF_FILENAME "ESMF_OptionsTable.F90"

#include <ESMF.h>

module ESMF_OptionsTable
  use ESMF_Mod

  public get_table, search_table


integer, parameter :: nOptions=9
integer, parameter, dimension(1:nOptions) :: nChoices=(/ 3,3,4,2,3,3,2,4,4 /)

character(len=25),parameter, dimension(1:nOptions) :: Option_name= &
       (/ 'SRCDELAYOUT'  &   ! (1)
        , 'DSTDELAYOUT'  &   ! (2)
        , 'FUNCTION   '  &   ! (3)
        , 'REGSCHEME  '  &   ! (4)
        , 'SRCGRID    '  &   ! (5)
        , 'DSTGRID    '  &   ! (6)
        , 'DOMAIN     '  &   ! (7)
        , 'SRCHALO    '  &   ! (8)
        , 'DSTHALO    '  &   ! (9)
       /)
character(len=25),  dimension(1:4,1:nOptions) :: Option_choice



contains

#undef  ESMF_METHOD
#define ESMF_METHOD "get_table"
subroutine get_table


!Option_name(1) = 'SRCDELAYOUT'
  Option_choice(1,1) = '1DX'
  Option_choice(2,1) = '1DY'
  Option_choice(3,1) = '2D'

!Option_name(2) = 'DSTDELAYOUT'
  Option_choice(1,2) = '1DX'
  Option_choice(2,2) = '1DY'
  Option_choice(3,2) = '2D'

!Option_name(3) = 'FUNCTION'
  Option_choice(1,3) = 'A'
  Option_choice(2,3) = 'B'
  Option_choice(3,3) = 'C'
  Option_choice(4,3) = 'D'

!Option_name(4) = 'REGSCHEME'
  Option_choice(1,4) = 'BILINEAR'
  Option_choice(2,4) = '1CONSERV'

!Option_name(5) = 'SRCGRID'
  Option_choice(1,5)='A'
  Option_choice(2,5)='D_NE'
  Option_choice(3,5)='C_NE'

!Option_name(6) = 'DSTGRID'
  Option_choice(1,6)='A'
  Option_choice(2,6)='D_NE'
  Option_choice(3,6)='C_NE'

!Option_name(7) = 'DOMAIN'
  Option_choice(1,7)='WHOLEGLOBE'
  Option_choice(2,7)='REGIONAL'

!Option_name(8) = 'SRCHALO'
   Option_choice(1,8)='0'
   Option_choice(2,8)='1'
   Option_choice(3,8)='2'
   Option_choice(4,8)='3'

!Option_name(9) = 'DSTHALO'
   Option_choice(1,9)='0'
   Option_choice(2,9)='1'
   Option_choice(3,9)='2'
   Option_choice(4,9)='3'

  return
end subroutine get_table

!==============================
#undef  ESMF_METHOD
#define ESMF_METHOD "search_table"
subroutine search_table(name_element, choice_element, i_name, i_choice, &
                       found_name, found_choice )
character(len=*), intent(in) :: name_element, choice_element
integer :: i_name, i_choice
logical :: found_name, found_choice

!initialize
found_name=.false.
found_choice =.false.
i_name   =-1
i_choice =-1

name_loop: do j=1,nOptions
              if (name_element .eq. Option_name(j) ) then
                i_name=j
                found_name=.true.
                do i=1,nChoices(j)
                  if (choice_element .eq. Option_choice(i,j) ) then
                    i_choice=i
                    found_choice=.true.
                    exit name_loop
                  end if
                end do
              end if
            end do name_loop

end subroutine search_table
#undef  ESMF_METHOD

end module ESMF_OptionsTable


