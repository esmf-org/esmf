!------------------------------------------------------------------------------
!BOP
!
! !DESCRIPTION:
!   This is the file that must be modified to set parameters for the
!   generic ESMF Application Driver.
!
!  In the text below, each line should be changed to the value of the
!   real Module name, SetServices subroutine name, and name of the
!   configuration file for the entire application.
!
!  The current settings match the empty Template and example files,
!   but will need to be changed if any of the modules or config files
!   are renamed.
!
!EOP


#define USER_APP_Mod            UserParentGridCompMod
#define USER_APP_SetServices    UserPComp_SetServices

#define USER_CONFIG_FILE       "sample.rc"

