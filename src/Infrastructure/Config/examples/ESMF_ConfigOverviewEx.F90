! Id: ESMF_FieldWriteEx.F90,v 1.5 2004/06/16 14:03:13 nscollins Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2003, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the GPL.
!
!==============================================================================
!
      program ESMF_ConfigOverviewEx

!-------------------------------------------------------------------------
!EXAMPLE        String used by test script to count examples.
!==============================================================================
!BOC
!
! !DESCRIPTION:
! Example/test code which performs simple Configuration File routines.
!
      ! ESMF Framework module
      use ESMF_Mod
      implicit none

      ! Local variables
      integer             :: status, petcount, i, j
      type(ESMF_VM)       :: vm
!EOC

      ! {\em Common Arguments:}
!BOE
      character(ESMF_MAXSTR) :: fname              ! file name
      character*20  :: fn1, fn2, fn3
      integer       :: rc                 ! error return code (0 is OK)
      integer       :: n 
      real          :: r
      real          :: table(7,3)

      type(ESMF_Config)   :: cf
      type(ESMF_DELayout) :: layout
!EOE

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

!BOC
!\subsubsection{Creation of a Config}

! The first step is to create the {\tt ESMF\_Config} and load the
! ASCII resource (rc) file into memory\footnote{See next section
! for a complete description of parameters for each routine/function}:
!EOC

!BOE
      cf = ESMF_ConfigCreate(rc)
!EOE

      if (rc.NE.ESMF_SUCCESS) then
        finalrc = ESMF_FAILURE
        print*, "' call ESMF_ConfigCreate' failed"
      endif

!BOE
      fname = "myResourceFile.rc"
      call ESMF_ConfigLoadFile(cf, fname, rc=rc)
!EOE

      if (rc.NE.ESMF_SUCCESS) then
        finalrc = ESMF_FAILURE
        print*, "' call ESMF_ConfigLoadFile' failed"
      endif

!BOC
!\subsubsection{Retrieval of constants}

! The next step is to select the label (record) of interest, say
!EOC

!BOE
      call ESMF_ConfigFindLabel(cf, 'constants:', rc=rc)
!EOE

      if (rc.NE.ESMF_SUCCESS) then
        finalrc = ESMF_FAILURE
        print*, "' call ESMF_ConfigFindLabel' failed"
      endif

!BOC
! Two constants, r and n, can be retrieved with the following code
! fragment:
!EOC

!BOE
      call ESMF_ConfigGetAttribute(cf, r, rc=rc)      ! results in r = 3.1415
      call ESMF_ConfigGetAttribute(cf, n, rc=rc)      ! results in n = 25
!EOE

      if (rc.NE.ESMF_SUCCESS) then
        finalrc = ESMF_FAILURE
        print*, "' call ESMF_ConfigGetAttribute' failed"
      endif

      if (r.ne.3.1415 .OR. n.ne.25) then
        finalrc = ESMF_FAILURE
        print*, "bad results from ConfigGetAttribute"
      endif

!BOC
!\subsubsection{Retrieval of file names}

! File names can be retrieved with the following code fragment:
!EOC

!BOE
       call ESMF_ConfigFindLabel(cf, 'my_file_names:', rc=rc)
!EOE

      if (rc.NE.ESMF_SUCCESS) then
        finalrc = ESMF_FAILURE
        print*, "' call ESMF_ConfigFindLabel' failed"
      endif

!BOE
       call ESMF_ConfigGetAttribute(cf, fn1, rc=rc)   ! results in fn1 = 'jan87.dat'
       call ESMF_ConfigGetAttribute(cf, fn2, rc=rc)   ! results in fn2 = 'jan88.dat'
       call ESMF_ConfigGetAttribute(cf, fn3, rc=rc)   ! results in fn3 = 'jan89.dat'
!EOE

      if (rc.NE.ESMF_SUCCESS) then
        finalrc = ESMF_FAILURE
        print*, "' call ESMF_ConfigGetAttribute' failed"
      endif

      if (fn1.ne.'jan87.dat' .OR. fn2.ne.'jan88.dat' .OR. &
          fn3.ne.'jan89.dat') then
        finalrc = ESMF_FAILURE
        print*, "bad results from ConfigGetAttribute"
      endif

!BOC
!\subsubsection{Retrieval of tables}

! To access tabular data, the user first must use
! {\tt ESMF\_ConfigFindLabel()} to locate the beginning of the table, e.g.,
!EOC

!BOE
      call ESMF_ConfigFindLabel(cf, 'my_table_name::', rc=rc)
!EOE

      if (rc.NE.ESMF_SUCCESS) then
        finalrc = ESMF_FAILURE
        print*, "' call ESMF_ConfigFindLabel' failed"
      endif

!BOC
! Subsequently, {\tt call ESMF\_ConfigNextLine()} can be used to gain
! access to each row of the table. Here is a code fragment to read the above
! table (7 rows, 3 columns):
!EOC

!BOE
      do i = 1, 7
        call ESMF_ConfigNextLine(cf, rc=rc)
        do j = 1, 3
          call ESMF_ConfigGetAttribute(cf, table(i,j), rc=rc)
        enddo
      enddo
!EOE

!BOC
!\subsubsection{Destruction of a Config}

! The work with the configuration file {\tt cf} is finalized by call to
! {\tt ESMF\_ConfigDestroy()}:
!EOC

!BOE
      call ESMF_ConfigDestroy(cf, rc)
!EOE

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
