! $Id: user_model1.F90,v 1.4 2003/03/24 22:56:25 nscollins Exp $
!
! Example/test code which shows User Component calls.

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

!BOP
!
! !DESCRIPTION:
!  User-supplied Component, Version A (minimalist)
!
!
!\begin{verbatim}

    module user_model1
    
!   ! Some common definitions.  This requires the C preprocessor.
#include "ESMF.h"

!   ! ESMF modules
    use ESMF_BaseMod
    use ESMF_IOMod
    use ESMF_DELayoutMod
    use ESMF_ClockMod
    use ESMF_ArrayMod
    use ESMF_GridMod
    use ESMF_DataMapMod
    use ESMF_FieldMod
    use ESMF_StateMod
    use ESMF_CompMod
    
    implicit none
    
    public userm1_register
        
    contains

!-------------------------------------------------------------------------
!   !  The Register routine sets the subroutines to be called
!   !   as the init, run, and finalize routines.  Note that these are
!   !   private to the module.
 
    subroutine userm1_register(comp, rc)
        type(ESMF_Comp) :: comp
        integer :: rc

        print *, "in user register routine"

        ! Register the callback routines.

        call ESMF_CompSetRoutine(comp, ESMF_CALLINIT, 1, user_init, rc)
        call ESMF_CompSetRoutine(comp, ESMF_CALLRUN, 1, user_run, rc)
        call ESMF_CompSetRoutine(comp, ESMF_CALLFINAL, 1, user_final, rc)

        print *, "Registered Initialize, Run, and Finalize routines"

        ! If desired, this routine can register a private data block
        ! to be passed in to the routines above:
        ! call ESMF_CompSetData(comp, mydatablock, rc)

    end subroutine

!-------------------------------------------------------------------------
!   !  User Comp Component created by higher level calls, here is the
!   !   Initialization routine.
 
    
    subroutine user_init(comp, rc)
        type(ESMF_Comp) :: comp
        integer :: rc

!     ! Local variables
        type(ESMF_State) :: myexport
        type(ESMF_Field) :: humidity
        type(ESMF_DELayout) :: layout

        integer :: i, x, y
        type(ESMF_Grid) :: grid1
        type(ESMF_Array) :: array1
        type(ESMF_AxisIndex), dimension(ESMF_MAXGRIDDIM) :: index
        integer, dimension(:,:), pointer :: idata
        integer :: nDE_i, nDE_j
        real :: x_min, x_max, y_min, y_max
        integer :: i_max, j_max
        integer :: ni, nj, de_id
        integer :: horz_gridtype, vert_gridtype
        integer :: horz_stagger, vert_stagger
        integer :: horz_coord_system, vert_coord_system
        integer :: status, myde

        print *, "User Comp Init starting"

        ! This is where the model specific setup code goes.  

        ! Query component for information.
        call ESMF_CompGet(comp, export=myexport, layout=layout, rc=rc)

        ! Add a "humidity" field to the export state.
        i_max = 40
        j_max = 20
        horz_gridtype = ESMF_GridType_XY
        vert_gridtype = ESMF_GridType_Unknown
        horz_stagger = ESMF_GridStagger_A
        vert_stagger = ESMF_GridStagger_Unknown
        horz_coord_system = ESMF_CoordSystem_Cartesian
        vert_coord_system = ESMF_CoordSystem_Unknown
        x_min = 0.0
        x_max = 20.0
        y_min = 0.0
        y_max = 5.0

        grid1 = ESMF_GridCreate(i_max=i_max, j_max=j_max, &
                                nDE_i=4, nDE_j=1, &
                                horz_gridtype=horz_gridtype, &
                                vert_gridtype=vert_gridtype, &
                                horz_stagger=horz_stagger, &
                                vert_stagger=vert_stagger, &
                                horz_coord_system=horz_coord_system, &
                                vert_coord_system=vert_coord_system, &
                                x_min=x_min, x_max=x_max, &
                                y_min=y_min, y_max=y_max, &
                                name="source grid", rc=status)

        ! Figure out our local processor id
        call ESMF_DELayoutGetDEID(layout, de_id, rc)

        ! Set initial data values over exclusive domain to the de identifier
        call ESMF_GridGetDE(grid1, lcellexc_index=index, rc=rc)
        ni = index(1)%r - index(1)%l + 1
        nj = index(2)%r - index(2)%l + 1
        print *, "allocating", ni, " by ",nj," cells on DE", de_id
        allocate(idata(ni,nj))

        ! Set initial data values over whole array to our de id
        idata = de_id

        ! Create Array based on an existing, allocated F90 pointer.
        ! Data is type Integer, 1D.
        array1 = ESMF_ArrayCreate(idata, ESMF_NO_COPY, rc)
        print *, "Array Create returned"

        humidity = ESMF_FieldCreate(grid1, array1, relloc=ESMF_CELL_CENTER, &
                                         name="humidity", rc=rc)

        call ESMF_StateAddData(myexport, humidity, rc)
        call ESMF_StatePrint(myexport, rc=rc)

        print *, "User Comp Init returning"
   
    end subroutine user_init


!-------------------------------------------------------------------------
!   !  The Run routine where data is computed.
!   !
 
    subroutine user_run(comp, rc)
        type(ESMF_Comp) :: comp
        integer :: rc

!     ! Local variables
        type(ESMF_State) :: myexport
        type(ESMF_Field) :: humidity
        type(ESMF_Array) :: array1
        integer, dimension(:,:), pointer :: idata
        integer :: status

        print *, "User Comp Run starting"


        ! This is where the model specific computation goes.


        ! Here is where the output state is updated.
        call ESMF_CompGet(comp, export=myexport, rc=status)
        call ESMF_StateGetData(myexport, "humidity", humidity, rc=status)

        ! update field values here
        call ESMF_FieldGetData(humidity, array1, rc=rc) 
        ! Get a pointer to the start of the data
        call ESMF_ArrayGetData(array1, idata, ESMF_NO_COPY, rc)

        ! increment data values in place
        idata = idata + 10
     

        call ESMF_StatePrint(myexport, rc=status)
        call ESMF_FieldPrint(humidity, rc=status)
        call ESMF_ArrayPrint(array1, "", rc=status)
 
        print *, "User Comp Run returning"

        rc = status

    end subroutine user_run


!-------------------------------------------------------------------------
!   !  The Finalization routine where things are deleted and cleaned up.
!   !
 
    subroutine user_final(comp, rc)
        type(ESMF_Comp) :: comp
        integer :: rc

        print *, "User Comp Final starting"
    
        print *, "User Comp Final returning"
   
    end subroutine user_final


    end module user_model1
    
!\end{verbatim}
    
