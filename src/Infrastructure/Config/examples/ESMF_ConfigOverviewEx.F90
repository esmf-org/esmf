!
! Earth System Modeling Framework
! Copyright 2002-2009, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
!
      program ESMF_ConfigOverviewEx

!-------------------------------------------------------------------------
!ESMF_EXAMPLE        String used by test script to count examples.
!==============================================================================
!
! !DESCRIPTION:
! Example/test code which performs simple Configuration File routines.
!
      ! ESMF Framework module
      use ESMF_Mod
      implicit none

      ! Local variables
      integer             :: i, j
      type(ESMF_VM)       :: vm

!BOE
!\subsubsection{Common Code Arguments}

! Common Arguments used in the following code fragments:
!EOE

!BOC
      character(ESMF_MAXSTR) :: fname     ! file name
      character*20  :: fn1, fn2, fn3
      integer       :: rc                 ! error return code (0 is OK)
      integer       :: i_n 
      real          :: radius
      real          :: table(7,3)

      type(ESMF_Config)   :: cf
!EOC

      integer :: finalrc
      finalrc = ESMF_SUCCESS
  
      call ESMF_Initialize(rc=rc)

      if (rc.NE.ESMF_SUCCESS) then
        finalrc = ESMF_FAILURE
        print*, "'call ESMF_Initialize' failed"
      endif
  
      call ESMF_VMGetGlobal(vm, rc)

      if (rc.NE.ESMF_SUCCESS) then
        finalrc = ESMF_FAILURE
        print*, "' call ESMF_VMGetGlobal' failed"
      endif

!-------------------------------------------------------------------------
!   ! Example 1:
!   !

!BOE
!\subsubsection{Creation of a Config}

! The first step is to create the {\tt ESMF\_Config} and load the
! ASCII resource (rc) file into memory\footnote{See next section
! for a complete description of parameters for each routine/function}:
!EOE

!BOC
      cf = ESMF_ConfigCreate(rc)
!EOC

      if (rc.NE.ESMF_SUCCESS) then
        finalrc = ESMF_FAILURE
        print*, "' call ESMF_ConfigCreate' failed"
      endif

!BOC
      fname = "myResourceFile.rc"
      call ESMF_ConfigLoadFile(cf, fname, rc=rc)
!EOC

      if (rc.NE.ESMF_SUCCESS) then
        finalrc = ESMF_FAILURE
        print*, "' call ESMF_ConfigLoadFile' failed"
      endif

!BOE
!\subsubsection{Retrieval of constants}

! The next step is to select the label (record) of interest, say
!EOE

!BOC
      call ESMF_ConfigFindLabel(cf, 'constants:', rc=rc)
!EOC

      if (rc.NE.ESMF_SUCCESS) then
        finalrc = ESMF_FAILURE
        print*, "' call ESMF_ConfigFindLabel' failed"
      endif

!BOE
! Two constants, radius and i\_n, can be retrieved with the following code
! fragment:
!EOE

!BOC
      call ESMF_ConfigGetAttribute(cf, radius, rc=rc)      ! results in radius = 3.1415
      call ESMF_ConfigGetAttribute(cf, i_n, rc=rc)      ! results in i_n = 25
!EOC

      if (rc.NE.ESMF_SUCCESS) then
        finalrc = ESMF_FAILURE
        print*, "' call ESMF_ConfigGetAttribute' failed"
      endif

      if (radius.ne.3.1415 .OR. i_n.ne.25) then
        finalrc = ESMF_FAILURE
        print*, "bad results from ConfigGetAttribute"
      endif

!BOE
!\subsubsection{Retrieval of file names}

! File names can be retrieved with the following code fragment:
!EOE

!BOC
       call ESMF_ConfigFindLabel(cf, 'my_file_names:', rc=rc)
!EOC

      if (rc.NE.ESMF_SUCCESS) then
        finalrc = ESMF_FAILURE
        print*, "' call ESMF_ConfigFindLabel' failed"
      endif

!BOC
       call ESMF_ConfigGetAttribute(cf, fn1, rc=rc)   ! results in fn1 = 'jan87.dat'
       call ESMF_ConfigGetAttribute(cf, fn2, rc=rc)   ! results in fn2 = 'jan88.dat'
       call ESMF_ConfigGetAttribute(cf, fn3, rc=rc)   ! results in fn3 = 'jan89.dat'
!EOC

      if (rc.NE.ESMF_SUCCESS) then
        finalrc = ESMF_FAILURE
        print*, "' call ESMF_ConfigGetAttribute' failed"
      endif

      if (fn1.ne.'jan87.dat' .OR. fn2.ne.'jan88.dat' .OR. &
          fn3.ne.'jan89.dat') then
        finalrc = ESMF_FAILURE
        print*, "bad results from ConfigGetAttribute"
      endif

!BOE
!\subsubsection{Retrieval of tables}

! To access tabular data, the user first must use
! {\tt ESMF\_ConfigFindLabel()} to locate the beginning of the table, e.g.,
!EOE

!BOC
      call ESMF_ConfigFindLabel(cf, 'my_table_name::', rc=rc)
!EOC

      if (rc.NE.ESMF_SUCCESS) then
        finalrc = ESMF_FAILURE
        print*, "' call ESMF_ConfigFindLabel' failed"
      endif

!BOE
! Subsequently, {\tt call ESMF\_ConfigNextLine()} can be used to gain
! access to each row of the table. Here is a code fragment to read the above
! table (7 rows, 3 columns):
!EOE

!BOC
      do i = 1, 7
        call ESMF_ConfigNextLine(cf, rc=rc)
        do j = 1, 3
          call ESMF_ConfigGetAttribute(cf, table(i,j), rc=rc)
        enddo
      enddo
!EOC

!BOE
!\subsubsection{Destruction of a Config}

! The work with the configuration file {\tt cf} is finalized by call to
! {\tt ESMF\_ConfigDestroy()}:
!EOE

!BOC
      call ESMF_ConfigDestroy(cf, rc)
!EOC

      if (rc.NE.ESMF_SUCCESS) then
        finalrc = ESMF_FAILURE
        print*, "'call ESMF_ConfigDestroy' failed"
      end if

      if (finalrc.eq.ESMF_SUCCESS) then
        print *, "PASS: ESMF_ConfigOverviewEx.F90"
      else
        print *, "FAIL: ESMF_ConfigOverviewEx.F90"
      end if

      call ESMF_Finalize(rc=rc)

      end program ESMF_ConfigOverviewEx
