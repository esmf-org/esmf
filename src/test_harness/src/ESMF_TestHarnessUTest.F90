!
! Earth System Modeling Framework
! Copyright 2002-2009, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the GPL.
!
!==============================================================================
!
  program esmf_test_harness

!------------------------------------------------------------------------------
! INCLUDES
#include "ESMF.h"

!------------------------------------------------------------------------------
!USE_TEST_CASE
!==============================================================================
!BOP
! !PROGRAM: ESMF_TEST_HARNESS - Data redistribution and regridding tests
!
! !DESCRIPTION:
!
! The code in this file drives the testing harness for testing the redistribution
! and regridding methods. 
!
!-----------------------------------------------------------------------------
! !USES:
  use ESMF_TestMod  
  use ESMF_Mod
  use ESMF_TestHarnessMod
  implicit none

  ! cumulative result: count failures; no failure equals "all pass"
  integer :: result = 0

  ! individual test result code
  integer :: rc, finalrc

  ! local args needed to create/construct objects
  type(ESMF_VM)          :: vm

  ! global storage of test specification
  type (harness_descriptor) :: harness

!-----------superceeded remove
  ! global storage of test specification - used in report - need to update
  type (problem_descriptor_record), allocatable :: problem_descriptor(:)
!-----------superceeded remove

  ! local variables
  integer :: localPet, petCount
  integer :: test_report_flag
  integer :: problem_descriptor_count
  integer :: ncount
  ! top level test harness config file
  character(ESMF_MAXSTR) :: test_harness_name = "test_harness.rc"

  ! -------- beginning of executable code below here -------
  !   !Set finalrc to success
  finalrc = ESMF_SUCCESS

  call ESMF_TestStart(ESMF_SRCLINE, rc=rc)

  ! get global vm information
  call ESMF_VMGetGlobal(vm, rc=rc)
  if (rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE

  call ESMF_VMGet(vm, localPet, petCount=petCount, rc=rc)
  if (rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE

  !
  call read_testharness_config(rc)
  if (rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE
  call read_descriptor_files(rc)
  if (rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE
  call parse_descriptor_string(rc)
  if (rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE
  ! report results
! call TestHarnessReport(test_report_flag,rc)

  ! clean up and release memory
! deallocate( problem_descriptor )

  !---------------------------------------------------------------------------
999 continue
  !---------------------------------------------------------------------------

  call ESMF_TestEnd(result, ESMF_SRCLINE)


  ! -------- end of unit test code ------------------------

!============================================================================

contains

!============================================================================
!2345678901234567890123456789012345678901234567890123456789012345678901234567890
! !IROUTINE: read_testharness_config

! !INTERFACE:
  subroutine read_testharness_config(localrc)
!
! !ARGUMENTS:
  integer, intent(  out) :: localrc

!
! !DESCRIPTION:
! Routine opens the top level config file "test_harness.rc", which specifies the 
! test class, the reporting style, and depending on how the ESMF_TESTEXHAUSTIVE flag
! is set, extracts the list of files containing the problem descriptor strings.
!

  ! local ESMF types
  type(ESMF_Config)      :: localcf

  ! local parameters
  character(ESMF_MAXSTR), parameter :: test_class_name  = "test_class:"
  character(ESMF_MAXSTR), parameter :: test_report_name = "test_report:"


  ! local character strings
  character(ESMF_MAXSTR) :: ltag

  ! local integer variables
  integer :: file, ncolumns

  ! local  logical
  logical :: flag = .true.

  ! initialize return code
  localrc = ESMF_RC_NOT_IMPL

  !--------------------------------------------------------------------------
  ! create config handle and load the testing harness config file
  !--------------------------------------------------------------------------
  localcf = ESMF_ConfigCreate(localrc)
  if( ESMF_LogMsgFoundError(localrc, "cannot create config object",         &
                            rcToReturn=localrc) ) return
                            
  print*,'Opening ',trim( test_harness_name )
  call ESMF_ConfigLoadFile(localcf, trim(test_harness_name), rc=localrc )
  if( ESMF_LogMsgFoundError(localrc,                                        &
           "cannot load config file " // trim( test_harness_name ),         &
           rcToReturn=localrc) ) return
  
  !--------------------------------------------------------------------------
  ! find and load the test class 
  !--------------------------------------------------------------------------
  print*,'Reading ',trim( test_harness_name )
  call ESMF_ConfigFindLabel(localcf, trim(test_class_name), rc=localrc )
  if( ESMF_LogMsgFoundError(localrc,                                        &
           "cannot find config label " // trim( test_class_name ),          &
           rcToReturn=localrc) ) return

  call ESMF_ConfigGetAttribute(localcf, harness%testClass, rc=localrc )
  if( ESMF_LogMsgFoundError(localrc,                                        &
           "cannot get value for label " // trim( test_class_name ),        &
           rcToReturn=localrc) ) return

! debug

! print*,'-->',trim(harness%testClass),'<--', INDEX('FIELD',trim(harness%testClass))
  if ( trim(harness%testClass) /= 'ARRAY'  .and.                            &
       trim(harness%testClass) /= 'BUNDLE' .and.                            &
       trim(harness%testClass) /= 'FIELD'  .and.                            &
       trim(harness%testClass) /= 'GRID'   .and.                            &
       trim(harness%testClass) /= 'REGRID' ) then
     call ESMF_LogMsgSetError( ESMF_FAILURE,                                &
              "class name not of valid type " // trim(harness%testClass),   &
              rcToReturn=localrc)
     return
  endif

  print*,'Test class is ',trim( harness%testClass )

  !--------------------------------------------------------------------------
  ! determine type of test report to be requested
  !--------------------------------------------------------------------------
  call ESMF_ConfigFindLabel(localcf, trim(test_report_name), rc=localrc )
  if( ESMF_LogMsgFoundError(localrc,                                        &
           "cannot find config label " // trim( test_report_name ),         &
           rcToReturn=localrc) ) return

  !--------------------------------------------------------------------------
  ! read test report flag 
  !	0 = no output
  !	1 = report successful tests
  !    -1 = report test failures
  !	2 = report both successes and failures.
  !--------------------------------------------------------------------------
  call ESMF_ConfigGetAttribute(localcf, harness%reportType, rc=localrc )
  if( ESMF_LogMsgFoundError(localrc,                                        &
           "cannot get value for label " // trim( test_report_name ),       &
           rcToReturn=localrc) ) return

  if ( harness%reportType .lt.-2 .or. harness%reportType .gt.2) then
     call ESMF_LogMsgSetError( ESMF_FAILURE,                                &
               "report flag improperly set " // char(harness%reportType),   &
               rcToReturn=localrc)
     return
  endif
  print*,'Test report flag is ', harness%reportType

  !--------------------------------------------------------------------------
  ! based on whether exhaustive or nonexhaustive tests are to be run,  find 
  ! and load the problem descriptor file names
  !--------------------------------------------------------------------------
#ifdef ESMF_TESTEXHAUSTIVE
  ltag = 'exhaustive::'
  print *, "exhaustive"
#else
  ltag = 'nonexhaustive::'
  print *, "nonexhaustive"
#endif
  call ESMF_ConfigFindLabel(localcf, trim(ltag), rc=localrc )
  if( ESMF_LogMsgFoundError(localrc,                                        &
           "cannot find config label " // trim(ltag),                       &
           rcToReturn=localrc) ) return

  ! determine the number of entries
  call ESMF_ConfigGetDim(localcf, harness%numRecords, ncolumns, trim(ltag), &
                         rc=localrc)
  if( ESMF_LogMsgFoundError(localrc,                                        &
           "cannot find table size of " // trim(ltag),                      &
           rcToReturn=localrc) ) return

  print*,'The number of problem descriptor files is ',harness%numRecords
  if ( harness%numRecords .le. 0 ) then
     call ESMF_LogMsgSetError( ESMF_FAILURE,                                &
               "no problem descriptor files specified",                     &
               rcToReturn=localrc)
     return
  endif

  !--------------------------------------------------------------------------
  ! find the problem descriptor file names and read them
  !--------------------------------------------------------------------------
  call ESMF_ConfigFindLabel(localcf, trim(ltag), rc=localrc)
  if( ESMF_LogMsgFoundError(localrc,                                        &
           "cannot find table label of " // trim(ltag),                     &
           rcToReturn=localrc) ) return

  !--------------------------------------------------------------------------
  ! allocate space to hold problem descriptors and advance through file and 
  ! extract problem descriptor filenames
  !--------------------------------------------------------------------------
  allocate( harness%Record(harness%numRecords) )

  do file=1,harness%numRecords
     ! advance to new line in table
     call ESMF_ConfigNextLine(localcf, flag, rc=localrc)
     if( ESMF_LogMsgFoundError(localrc,                                     &
              "cannot advance to next line of table " // trim(ltag),        &
              rcToReturn=localrc) ) return
 
     ! retrieve the problem descriptor filenames 
     call ESMF_ConfigGetAttribute(localcf, harness%Record(file)%filename,   &
                                  rc=localrc)
     if( ESMF_LogMsgFoundError(localrc,                                     &
              "cannot get descriptor filename in " // trim(ltag),           &
              rcToReturn=localrc) ) return

     print*,'Descriptor file name:',trim(harness%Record(file)%filename)
  enddo   ! file

  !--------------------------------------------------------------------------
  ! clean up CF
  !--------------------------------------------------------------------------
  call ESMF_ConfigDestroy(localcf, localrc)
  if( ESMF_LogMsgFoundError(localrc,                                        &
           "cannot destroy config file " // trim(test_harness_name),        &
           rcToReturn=localrc) ) return

  ! if I've gotten this far without an error, then the routine has succeeded.
  localrc = ESMF_SUCCESS

  !--------------------------------------------------------------------------
  end subroutine read_testharness_config
  !--------------------------------------------------------------------------

!2345678901234567890123456789012345678901234567890123456789012345678901234567890

!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
! !IROUTINE: read_descriptor_files

! !INTERFACE:
  subroutine read_descriptor_files(localrc)
!
! !ARGUMENTS:
  integer, intent(  out) :: localrc

!
! !DESCRIPTION:
! This routine takes the problem descriptor file names specified in the top 
! level config file "test_harness.rc" and extracts from a config table the
! "problem descriptor string" and all the "problem specifier helper files."
! The helper files are divided into groups by flags preceeded by a dash. The
! "-c" flag indicates the file containing the CLASS specific settings. The
! "-d" flag indicates the file(s) containing the ensemble of distribution 
! configurations to be run with the specific "problem descriptor string."
! Likewise the "-g" flag indicates the file(s) containing the ensemble of
! grid configurations to be run with the specific "problem descriptor string."
! This routine only extracts the information from the configuration file. 
! additional processing occurs in a later routine.

  ! local ESMF types
  type(ESMF_Config)      :: localcf

  ! local parameters
  character(ESMF_MAXSTR), parameter :: descriptor_label      &
                                               = "problem_descriptor_string::"

  ! local character types
  type (sized_char_array), allocatable :: ltmpstring(:), lstring(:)

  ! local character strings
  character(ESMF_MAXSTR) :: lfilename, ltmp
  character(ESMF_MAXSTR) :: lchar, lchar1, lchar2
  
  logical :: flag = .true.

! local integer variables
  integer :: n, nn, k, pos, col, ncount, npds, ntmp
  integer :: file, nfiles,string, pstring
  integer :: cpos, dpos, gpos, csize, dsize, gsize
  integer, allocatable :: count(:), ncolumns(:), nstrings(:)
  integer, allocatable :: pds_loc(:), pds_flag(:)

! local logical variable
  logical :: endflag = .false.
  logical :: cflag, dflag, gflag
  ! initialize return flag
  localrc = ESMF_RC_NOT_IMPL

  !--------------------------------------------------------------------------
  ! open each problem descriptor, one at a time, and extract the table contents
  !--------------------------------------------------------------------------
  nfiles = harness%numRecords
  allocate( nstrings(nfiles) )

  do file=1,nfiles
    print*,'opening file number ',file
    !------------------------------------------------------------------------
    ! create a new config handle for reading problem descriptor strings
    !------------------------------------------------------------------------
    localcf = ESMF_ConfigCreate(localrc)
    if( ESMF_LogMsgFoundError(localrc, "cannot create config object",       &
                         rcToReturn=localrc) ) return

    !------------------------------------------------------------------------
    ! load file holding the problem descriptor strings
    !------------------------------------------------------------------------
    lfilename = trim( harness%Record(file)%filename )

    print*,'Opening ',trim( lfilename )
    call ESMF_ConfigLoadFile(localcf, trim( lfilename ), rc=localrc )
    if( ESMF_LogMsgFoundError(localrc,                                      &
           "cannot load config file " // trim( lfilename ),                 &
           rcToReturn=localrc) ) return

    !------------------------------------------------------------------------
    ! Search for the problem descriptor string table
    !------------------------------------------------------------------------
    call ESMF_ConfigFindLabel(localcf, trim(descriptor_label), rc=localrc )
    if( ESMF_LogMsgFoundError(localrc,                                      &
           "cannot find config label " // trim(descriptor_label),           &
           rcToReturn=localrc) ) return

    !------------------------------------------------------------------------
    ! determine the number of entries
    !------------------------------------------------------------------------
    call ESMF_ConfigGetDim(localcf, nstrings(file), ntmp,                   &
             trim(descriptor_label), rc=localrc)
    if( ESMF_LogMsgFoundError(localrc,                                      &
           "cannot get descriptor table size in file " // trim(lfilename),  &
           rcToReturn=localrc) ) return

    !------------------------------------------------------------------------
    ! determine that the table has entries before preceeding
    !------------------------------------------------------------------------
    if( nstrings(file) .le. 0 ) then
      call ESMF_LogMsgSetError( ESMF_FAILURE,                               &
               "problem descriptor table empty in file " //trim(lfilename), &
               rcToReturn=localrc)
      return
    endif

    write(lchar,"(i5)")  nstrings(file)
    print*,'problem descriptor table in file '//trim(lfilename)//' has '    &
            // trim(adjustl(lchar)) //' entries'

    !------------------------------------------------------------------------
    ! extract the table column lengths of this file
    !------------------------------------------------------------------------
    call ESMF_ConfigFindLabel(localcf, trim(descriptor_label), rc=localrc )
    if( ESMF_LogMsgFoundError(localrc,                                      &
           "cannot find config label " // trim(descriptor_label),           &
           rcToReturn=localrc) ) return

    allocate( ncolumns(nstrings(file)) )
    allocate ( ltmpstring(nstrings(file)) )

    do string=1,nstrings(file)
      call ESMF_ConfigNextLine(localcf, flag , rc=localrc)
      if( ESMF_LogMsgFoundError(localrc,                                    &
             "cannot advance to next line of table " //                     &
              trim(descriptor_label) // " in file " // trim(lfilename),     &
              rcToReturn=localrc) ) return

      ncolumns(string) = ESMF_ConfigGetLen(localcf, rc=localrc)
      if (localrc .ne. ESMF_SUCCESS .or. ncolumns(string) .lt. 2 ) then
        write(lchar,"(i5)")  string
        call ESMF_LogMsgSetError( ESMF_FAILURE,                             &
                 "problem reading line " // trim(adjustl(lchar)) //         &
                 " of table in file " // trim(lfilename), rcToReturn=localrc)
        return
      endif

      !----------------------------------------------------------------------
      ! allocate tempory storage so that the file needs to be read only once
      !----------------------------------------------------------------------
      allocate ( ltmpstring(string)%string( ncolumns(string) ) )
      ltmpstring(string)%size = ncolumns(string)
    enddo    ! end string

    !------------------------------------------------------------------------
    ! Starting again at the top of the table, extract the table contents into
    ! a local character array structure for later processing
    !------------------------------------------------------------------------
    call ESMF_ConfigFindLabel(localcf, trim(descriptor_label), rc=localrc )
    if( ESMF_LogMsgFoundError(localrc,                                      &
           "cannot find config label " // trim(descriptor_label),           &
           rcToReturn=localrc) ) return

    do string=1,nstrings(file)
    !------------------------------------------------------------------------
    ! copy the table into a character array
    !------------------------------------------------------------------------
      call ESMF_ConfigNextLine(localcf, flag , rc=localrc)
      if( ESMF_LogMsgFoundError(localrc,                                    &
             "cannot advance to next line of table " //                     &
              trim(descriptor_label) // " in file " // trim(lfilename),     &
              rcToReturn=localrc) ) return

      do col=1, ncolumns(string)
        call ESMF_ConfigGetAttribute(localcf, ltmp, rc=localrc)
        write(lchar,"(i5)") string
        if( ESMF_LogMsgFoundError(localrc,                                  &
              "cannot get table entry from line "//trim(adjustl(lchar)) //  &
              " column " // char(col)  // "of file " // trim(lfilename),    &
              rcToReturn=localrc) ) return
         ltmpstring(string)%string(col)%name = trim( ltmp )
      enddo     ! end col
    enddo       ! end string

    !------------------------------------------------------------------------
    ! count the number of actual problem descriptor strings & continuation lines
    !------------------------------------------------------------------------
    ncount = 0
    npds = 0
    allocate( pds_flag(nstrings(file)) )
    do string=1,nstrings(file)
       if( trim( ltmpstring(string)%string(1)%name ) /= "&") then
         pds_flag(string) = 1         
         npds = npds + 1
       else
         ncount = ncount + 1
         pds_flag(string) = 0         
       endif
    enddo     ! end string
    ! sanity check
    if( (npds + ncount) /= nstrings(file) ) then
      write(lchar,"(i5)")  nstrings(file)
      write(lchar1,"(i5)")  npds
      write(lchar2,"(i5)")  ncount
      call ESMF_LogMsgSetError( ESMF_FAILURE, "number of rows " //          &
             trim(adjustl(lchar)) // " in the table"  //                    &
             " does not match the sum of strings " // trim(adjustl(lchar1)) &
             // " and continuation lines " //  trim(adjustl(lchar2)) //     &
             " of file " // trim(lfilename), rcToReturn=localrc) 
    endif

    !------------------------------------------------------------------------
    ! save the addresses of the non-continuation lines
    !------------------------------------------------------------------------
    k = 0
    allocate( pds_loc(npds) )
    do string=1,nstrings(file)
       if( pds_flag(string) == 1 ) then
         k = k + 1
         pds_loc(k) =  string        
       endif
    enddo     ! end string
    ! sanity check
    if( npds .ne. k ) then 
      write(lchar,"(i5)")  nstrings(file)
      write(lchar1,"(i5)")  npds
      write(lchar2,"(i5)")  ncount
      call ESMF_LogMsgSetError( ESMF_FAILURE, "number of rows " //          &
             trim(adjustl(lchar)) // " in the table" //                     &
             " does not match the sum of strings "//trim(adjustl(lchar1))   &
             // " and continuation lines " // trim(adjustl(lchar2)) //      &
             " of file " // trim(lfilename), rcToReturn=localrc)
    endif

    !------------------------------------------------------------------------
    ! to simplify the later search algorithm, reshape the input table from a 
    ! series of lines with a PDS plus optional continuations lines, to a single
    ! line with everything on it. Count the total number of elements on both 
    ! type of lines to that we can allocate enough memory to store the whole 
    ! specification.
    !------------------------------------------------------------------------
    allocate( count(npds) )
    do k=1,npds
      if( trim( ltmpstring( pds_loc(k) )%string(1)%name ) == "&") then
        write(lchar,"(i5)")   pds_loc(k)
        call ESMF_LogMsgSetError( ESMF_FAILURE,                             &
                 "no problem descriptor string on line " //                 &
                  trim(adjustl(lchar)) // " of file " // trim(lfilename),   &
                 rcToReturn=localrc)
      else    ! at new PDS
        count(k) = ncolumns(pds_loc(k))
        pstring =  pds_loc(k)
 21     continue
        !--------------------------------------------------------------------
        ! if not end of table, look for additional continuation lines 
        !--------------------------------------------------------------------
        if(pstring < nstrings(file)) then
          pstring =  pstring + 1
          !------------------------------------------------------------------
          ! if find a continuation line add additional elements (minus the
          ! continuation symbol "&")
          !------------------------------------------------------------------
          if( trim( ltmpstring(pstring)%string(1)%name ) == "&" ) then 
            count(k) = count(k) + ncolumns(pstring) -1
            goto 21
          endif
        endif

      endif
    enddo     ! k
    !------------------------------------------------------------------------
    ! create reshaped workspace to hold the problem descriptor table contents
    !------------------------------------------------------------------------
    allocate ( lstring(npds) )    
    do k=1, npds
      allocate ( lstring(k)%string(count(k)) )    
      do n=1,ncolumns(pds_loc(k))
        lstring(k)%string(n)%name = &
                               trim( ltmpstring(pds_loc(k))%string(n)%name )
      enddo     ! n
        
      pstring =  pds_loc(k)
      nn = ncolumns(pds_loc(k))+1
 22   continue
      !----------------------------------------------------------------------
      ! if not end of table, look for additional continuation lines
      !----------------------------------------------------------------------
      if(pstring < nstrings(file)) then
        pstring =  pstring + 1
        !--------------------------------------------------------------------
        ! if find a continuation line, and add to the line length (minus the 
        ! continuation symbol)
        !--------------------------------------------------------------------
        if( trim( ltmpstring(pstring)%string(1)%name ) == "&" ) then
          do n=2,ncolumns(pstring)
            lstring(k)%string(nn)%name = &
                                   trim(ltmpstring(pstring)%string(n)%name )
            nn = nn + 1
          enddo     ! n
          goto 22
        endif
      endif
    enddo     ! k
    !------------------------------------------------------------------------
    ! mine the table entries for the problem descriptor strings
    !------------------------------------------------------------------------
    allocate( harness%Record(file)%string(npds) )
    do k=1,npds
       harness%Record(file)%string(k)%pds = trim( lstring(k)%string(1)%name )
    enddo     ! k
    !------------------------------------------------------------------------
    ! mine the table entries for the names of the specifier files
    !------------------------------------------------------------------------
    do k=1,npds
      pos = 2
      endflag = .true.
      cflag = .false.
      dflag = .false.
      gflag = .false.
      !----------------------------------------------------------------------
      ! loop through the specifiers for each of the problem desriptor strings
      !----------------------------------------------------------------------
      do while(endflag)
      ltmp = trim( lstring(k)%string(pos)%name )

        select case ( trim(ltmp) )

         !-------------------------------------------------------------------
         ! class descriptor file
         !-------------------------------------------------------------------
         case('-c')
         if( cflag ) then
           write(lchar,"(i5)") k
           call ESMF_LogMsgSetError( ESMF_FAILURE, "the -c specifier flag"  &
                    // " is used more than once on the " //                 &
                   trim(adjustl(lchar))//"th string of the problem " //     &
                   "descriptor table in file" // trim(lfilename),           &
                   rcToReturn=localrc)
           return
         endif
         ! starting position
         cpos = pos
 11      continue
         ! if not at the end of the row, then check next element
         if( pos < count(k) ) then
           pos = pos + 1
           ltmp =  trim( lstring(k)%string(pos)%name )
           ! if not a flag, repeat until a flag
           if( ltmp(1:1) /= '-' ) goto 11
           csize = pos-1-cpos
           endflag = .true.
         else  ! at end of row
           csize = pos-cpos
           endflag = .false.
         endif

         allocate( harness%Record(file)%string(k)%classfile%string(csize) )
         harness%Record(file)%string(k)%classfile%size = csize
         do n=1,csize
           harness%Record(file)%string(k)%classfile%string(n)%name =        &
                                      trim( lstring(k)%string(cpos+n)%name )
         enddo      ! n
         cflag = .true.

         !-------------------------------------------------------------------
         ! distribution descriptor file
         !-------------------------------------------------------------------
         case('-d')
         if( dflag ) then
           write(lchar,"(i5)") k
           call ESMF_LogMsgSetError( ESMF_FAILURE, "the -d specifier flag"  &
                    // " is used more than once on the " //                 &
                   trim(adjustl(lchar))//"th string of the problem " //     &
                   "descriptor table in file" // trim(lfilename),           &
                   rcToReturn=localrc)
           return
         endif
         ! starting position
         dpos = pos

 12      continue
         ! if not at the end of the row, then check next element
         if( pos < count(k) ) then
           pos = pos + 1
           ltmp =  trim( lstring(k)%string(pos)%name )
           ! if not a flag, repeat until a flag
           if( ltmp(1:1) /= '-' ) goto 12
           dsize = pos-1-dpos
           endflag =.true. 
         else  ! at end of row
           dsize = pos-dpos
           endflag =.false. 
         endif

         allocate( harness%Record(file)%string(k)%distfiles%string(dsize) )
         harness%Record(file)%string(k)%distfiles%size = dsize
         do n=1,dsize
           harness%Record(file)%string(k)%distfiles%string(n)%name =        & 
                                      trim( lstring(k)%string(dpos+n)%name )
         enddo      ! n
         dflag = .true.

         !-------------------------------------------------------------------
         ! grid descriptor file
         !-------------------------------------------------------------------
         case('-g')
         if( gflag ) then
           write(lchar,"(i5)") k
           call ESMF_LogMsgSetError( ESMF_FAILURE, "the -g specifier flag"  &
                    // " is used more than once on the " //                 &
                   trim(adjustl(lchar))//"th string of the problem " //     &
                   "descriptor table in file" // trim(lfilename),           &
                   rcToReturn=localrc)
           return
         endif
         ! starting position
         gpos = pos

 13      continue
         ! if not at the end of the row, then check next element
         if( pos < count(k) ) then
           pos = pos + 1
           ltmp =  trim( lstring(k)%string(pos)%name )
           ! if not a flag, repeat until a flag
           if( ltmp(1:1) /= '-' ) goto 13
           gsize = pos-1-gpos
           endflag = .true.
         else  ! at end of row
           gsize = pos-gpos
           endflag = .false.
         endif  

         allocate( harness%Record(file)%string(k)%gridfiles%string(gsize) )
         harness%Record(file)%string(k)%gridfiles%size = gsize
         do n=1,gsize
           harness%Record(file)%string(k)%gridfiles%string(n)%name =        &
                                     trim( lstring(k)%string(gpos+n)%name )
         enddo     ! n
         gflag = .true.

         !-------------------------------------------------------------------
         ! syntax error - entry after pds should be a flag
         !-------------------------------------------------------------------
         case default
         write(lchar,"(i5)")  pds_loc(k)
         call ESMF_LogMsgSetError( ESMF_FAILURE,                            &
               "no specifier flag on line " // trim(adjustl(lchar)) //      &
               " of file " //trim(lfilename), rcToReturn=localrc)

        end select  ! specifier flag type
      end do  ! while
    enddo      ! k

!  diagnostics
         !
         print*,'  Diagnositcs                     '
      do k=1,npds
         print*,'                                  '
         print*,k,trim( harness%Record(file)%string(k)%pds )
         print*,'Class Specifier files',   &
                                 harness%Record(file)%string(k)%classfile%size
         do n=1,harness%Record(file)%string(k)%classfile%size
           print*,n, &
               trim( harness%Record(file)%string(k)%classfile%string(n)%name )
         enddo   ! n
         print*,'Distribution Specifier files', &
                                 harness%Record(file)%string(k)%distfiles%size
         do n=1,harness%Record(file)%string(k)%distfiles%size
           print*,n,   &
               trim( harness%Record(file)%string(k)%distfiles%string(n)%name )
         enddo     ! n
         print*,'Grid Specifier files',   &
                                 harness%Record(file)%string(k)%gridfiles%size
         do n=1,harness%Record(file)%string(k)%gridfiles%size
           print*,n,   &
               trim( harness%Record(file)%string(k)%gridfiles%string(n)%name )
         enddo     ! n
      enddo      ! k
         !
!  diagnostics

    !------------------------------------------------------------------------
    ! finish cleaning up workspace before opening new file
    !------------------------------------------------------------------------
    deallocate( ncolumns, count )
    deallocate( ltmpstring, lstring )
    deallocate( pds_loc, pds_flag )

    !------------------------------------------------------------------------
    ! clean up CF
    !------------------------------------------------------------------------
    call ESMF_ConfigDestroy(localcf, localrc)
    if( ESMF_LogMsgFoundError(localrc,                                      &
            "cannot destroy config file " // trim(lfilename),               &
            rcToReturn=localrc) ) return

  enddo  ! file

  !--------------------------------------------------------------------------
  ! final deallocation
  !--------------------------------------------------------------------------
  deallocate( nstrings )

  !--------------------------------------------------------------------------
  ! if I've gotten this far without an error, then the routine has succeeded.
  !--------------------------------------------------------------------------
  localrc = ESMF_SUCCESS

  !--------------------------------------------------------------------------
  end subroutine read_descriptor_files
  !--------------------------------------------------------------------------

!2345678901234567890123456789012345678901234567890123456789012345678901234567890

!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
! !IROUTINE: parse_descriptor_string

! !INTERFACE:
  subroutine parse_descriptor_string(localrc)
!
! !ARGUMENTS:
  integer, intent(  out) :: localrc

!
! !DESCRIPTION:
! This routine parses a problem descriptor string for 
!	operation - redistribution or regrid plus method
!	rank of memory
!	rank of distribution
!	rank of grid association
!

  ! local character types

  ! local character strings
  character(ESMF_MAXSTR) :: lstring, lname
  character(ESMF_MAXSTR) :: src_string, dst_string

  logical :: flag = .true.

  ! local integer variables
  integer :: file, nfiles,string
  integer, allocatable :: nstrings(:)
  integer :: tag, location(2)
  integer :: dst_beg, dst_end, src_beg, src_end
  integer :: srcMulti, dstMulti
  integer :: srcBlock,dstBlock


  ! local logical variable
  logical :: endflag = .false.

  ! initialize return flag
  localrc = ESMF_RC_NOT_IMPL

  !----------------------------------------------------------------------------
  ! parse each problem descriptor string
  !----------------------------------------------------------------------------
  nfiles = harness%numRecords
  allocate( nstrings(nfiles) )

  !----------------------------------------------------------------------------
  !----------------------------------------------------------------------------
  do file=1,nfiles
     do string=1,nstrings(file)
        lstring = trim( harness%Record(file)%string(string)%pds )
  !----------------------------------------------------------------------------
  ! find the test operation (address of strings)
  ! 1. search for "->" or "=>" - tip of symbol provides ending address
  ! 2. back up until reach white space - provides begining address
  ! 3. src_beg = 1, src_end = operation_beg-1
  !    dst_beg = operation_end+1, dst_end = length(trim(string)
  !----------------------------------------------------------------------------
        call process_query(lstring, lname, tag, location, localrc)  
        harness%Record(file)%string(string)%process%name = lname
        harness%Record(file)%string(string)%process%tag = tag

        call memory_topology(lstring, location, srcMulti, srcBlock,            &
                             dstMulti, dstBlock, localrc)
        if( (srcMulti >= 1).or.(dstMulti >= 1) ) then
           ! multiple block memory structure
           ! TODO break into multiple single block strings


        elseif( (srcMulti == 0).and.(srcBlock == 1) ) then
           ! single block memory structure

        else
        endif



        src_beg = 1
        src_end = location(1)
        dst_beg = location(2)
        dst_end = len( trim(lstring) )
        !----------------------------------------------------------------------
        ! separate string into source and destination strings
        !----------------------------------------------------------------------
        src_string = adjustL( lstring(src_beg:src_end) )
        dst_string = adjustL( lstring(dst_beg:dst_end) )
        !----------------------------------------------------------------------
        !----------------------------------------------------------------------
      print*,' TEST '
      print*,src_beg, src_end, dst_beg,dst_end 
      print*,'123456789012345678901234567890'
      print*,trim(lstring)
      print*,'process ', harness%Record(file)%string(string)%process%name
      print*,'src',trim( src_string )
      print*,'dst',trim( dst_string ) 
  !--------------------------------------------------------------------------
  ! find rank of source and destination strings ( plus addresses of dividers ";")
  ! break into substrings, one for each dimension. Increment through, searching 
  ! for key flags, when found set flags.
  !--------------------------------------------------------------------------
    enddo    ! string
  enddo    ! file
  !--------------------------------------------------------------------------
  ! if I've gotten this far without an error, then the routine has succeeded.
  !--------------------------------------------------------------------------
  localrc = ESMF_SUCCESS

  !--------------------------------------------------------------------------
  end subroutine parse_descriptor_string
  !--------------------------------------------------------------------------

!2345678901234567890123456789012345678901234567890123456789012345678901234567890

!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
! !IROUTINE: TestHarnessReport

! !INTERFACE:
  subroutine TestHarnessReport(report_flag,localrc)
!
! !ARGUMENTS:
  integer, intent(in   ) :: report_flag
  integer, intent(  out) :: localrc

!
! !DESCRIPTION:
! This routine provides an additional reporting capability of the test results
! in a more human readable form.
!--------------------------------------------------------------------------
! Action associated to test report flag value.
!    -2 = debug report
!    -1 = report test failures
!     0 = no output
!     1 = report successful tests
!     2 = report both successes and failures.
!--------------------------------------------------------------------------
   ! local variables

   ! initialize local rc
   localrc = ESMF_SUCCESS

!--------------------------------------------------------------------------
! begin 
!--------------------------------------------------------------------------
  select case (report_flag)
     case(-2)
     ! debug report

     case(-1)
        ! only report failures
        call ReportFailure(localrc)

     case(0)
        ! no report
        return

     case(1)
        ! only report success
        call ReportSuccess(localrc)
  
     case(2)
        ! report both successes and failures
        call ReportFailure(localrc)
        call ReportSuccess(localrc)

     case default
     ! flag error
     print*,'error, report flag improperly set.'
     localrc = ESMF_FAILURE
     return

  end select  ! case of report flag

  end subroutine TestHarnessReport

!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
! !IROUTINE: ReportSuccess

! !INTERFACE:
  subroutine ReportSuccess(localrc)
!
! !ARGUMENTS: 
  integer, intent(out) :: localrc

!
! !DESCRIPTION:
! This routine produces a human readable report of the successful test results.
!--------------------------------------------------------------------------
  ! initialize local rc
  localrc = ESMF_SUCCESS

  print*,'Output human readable report of test successes'

  end subroutine ReportSuccess

!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
! !IROUTINE: ReportFailure

! !INTERFACE: 
  subroutine ReportFailure(localrc)
!
! !ARGUMENTS:
  integer, intent(out) :: localrc

!
! !DESCRIPTION:
! This routine produces a human readable report of the failed test results.
!--------------------------------------------------------------------------
  ! initialize local rc
  localrc = ESMF_SUCCESS

  print*,'Output human readable report of test failures'

  end subroutine ReportFailure

!--------------------------------------------------------------------------
!--------------------------------------------------------------------------


  end program ESMF_Test_Harness
