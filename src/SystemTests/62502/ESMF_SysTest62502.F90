! $Id: ESMF_SysTest62502.F90,v 1.2 2003/03/10 03:23:14 cdeluca Exp $
!
! System test code #62502

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

!BOP
!
! !DESCRIPTION:
! System test number 62502.  2 components and 1 coupler, one-way coupling.
!
!
!\begin{verbatim}

    program ESMF_SysTest62502

#include "ESMF.h"

    ! Modules needed
    ! TODO: (these will be collapsed into a single ESMF_Mod soon)
    use ESMF_BaseMod
    use ESMF_IOMod
    use ESMF_DELayoutMod
    use ESMF_ArrayMod
    use ESMF_GridMod
    use ESMF_DataMapMod
    use ESMF_FieldMod
    use ESMF_StateMod
    use ESMF_CompMod
    
    use user_model1
    use user_model2
    use user_coupler

    implicit none
    
    ! Local variables
    integer, dimension(4) :: delist
    integer :: de_id, rc
    character(len=ESMF_MAXSTR) :: aname, cname1, cname2, cplname
    type(ESMF_DELayout) :: layout1 
    type(ESMF_State) :: c1exp, c2imp, cplstate(2)
    type(ESMF_Comp) :: app, comp1, comp2, cpl
        
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

    print *, "System Test #62502:"

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!    Create section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!

    ! TODO: move layout create down into comp after discover method ready.
    ! Create a DELayout for the Component
    delist = (/ 0, 1, 2, 3 /)
    layout1 = ESMF_DELayoutCreate(2, 2, delist, ESMF_XFAST, rc)

    ! Create the top level application component.
    aname = "System Test #62502"
    app = ESMF_CompCreate(aname, layout=layout1, ctype=ESMF_APPCOMP, rc=rc)
    print *, "Created component ", trim(aname), ",  rc =", rc

    ! Query application for layout.
    call ESMF_CompGet(app, layout=layout1, rc=rc)


    ! Create the 2 model components and coupler
    cname1 = "user model 1"
    comp1 = ESMF_CompCreate(cname1, layout=layout1, ctype=ESMF_GRIDCOMP, rc=rc)
    print *, "Created component ", trim(cname1), "rc =", rc

    cname2 = "user model 2"
    comp2 = ESMF_CompCreate(cname2, layout=layout1, ctype=ESMF_GRIDCOMP, rc=rc)
    print *, "Created component ", trim(cname2), "rc =", rc

    cplname = "user one-way coupler"
    cpl = ESMF_CompCreate(cplname, layout=layout1, ctype=ESMF_CPLCOMP, rc=rc)
    print *, "Created component ", trim(cplname), "rc =", rc


    print *, "Comp Creates finished"


!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!  Register section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
      call ESMF_CompRegister(comp1, userm1_register, rc)
      print *, "Comp Register finished, rc= ", rc

      call ESMF_CompRegister(comp2, userm2_register, rc)
      print *, "Comp Register finished, rc= ", rc

      call ESMF_CompRegister(cpl, usercpl_register, rc)
      print *, "Comp Register finished, rc= ", rc

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!  Init section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
 
      call ESMF_CompInitialize(comp1, rc)
      print *, "Comp 1 Initialize finished, rc =", rc
 
      call ESMF_CompInitialize(comp2, rc)
      print *, "Comp 2 Initialize finished, rc =", rc
 
      call ESMF_CompGet(comp1, export=c1exp, rc=rc)
      call ESMF_CompGet(comp2, import=c2imp, rc=rc)

      cplstate(1) = c1exp
      cplstate(2) = c2imp

      call ESMF_CompSet(cpl, statelist=cplstate, rc=rc)

      call ESMF_CompInitialize(cpl, rc)
      print *, "Coupler Initialize finished, rc =", rc
 
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!     Run section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

      call ESMF_CompRun(comp1, rc)
      print *, "Comp 1 Run returned, rc =", rc

      call ESMF_CompRun(cpl, rc)
      print *, "Coupler Run returned, rc =", rc

      call ESMF_CompRun(comp2, rc)
      print *, "Comp 2 Run returned, rc =", rc
 

      call ESMF_CompRun(comp1, rc)
      print *, "Comp 1 Run returned, rc =", rc

      call ESMF_CompRun(cpl, rc)
      print *, "Coupler Run returned, rc =", rc

      call ESMF_CompRun(comp2, rc)
      print *, "Comp 2 Run returned, rc =", rc
 
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!     Finalize section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!     Print result

      ! Figure out our local processor id for message below.
      call ESMF_DELayoutGetDEId(layout1, de_id, rc)

      call ESMF_CompFinalize(comp1, rc)
      print *, "Comp 1 Finalize finished, rc =", rc

      call ESMF_CompFinalize(comp2, rc)
      print *, "Comp 2 Finalize finished, rc =", rc

      call ESMF_CompFinalize(cpl, rc)
      print *, "Coupler Finalize finished, rc =", rc


      print *, "-----------------------------------------------------------------"
      print *, "-----------------------------------------------------------------"
      print *, "Test finished, de_id = ", de_id
      print *, "-----------------------------------------------------------------"
      print *, "-----------------------------------------------------------------"

      print *, "Comp Finalize returned"

!
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!     Destroy section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!     Clean up

      call ESMF_CompDestroy(comp1, rc)
      call ESMF_CompDestroy(comp2, rc)
      call ESMF_CompDestroy(cpl, rc)
      call ESMF_DELayoutDestroy(layout1, rc)
      print *, "All Destroy routines done"

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
      print *, "System Test #62502 complete!"

      end program ESMF_SysTest62502
    
!\end{verbatim}
    
