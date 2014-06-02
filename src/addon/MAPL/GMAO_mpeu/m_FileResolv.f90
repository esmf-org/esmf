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
!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !MODULE:  m_FileResolv --- Resolve file name templates and Executables
! 
! !INTERFACE:
!

   MODULE  m_FileResolv

! !USES:

   use  m_StrTemplate  ! grads style templates
    Use m_ioutil,   Only : luavail, opntext, clstext
   use  m_die
   Implicit NONE

!
! !PUBLIC MEMBER FUNCTIONS:
!
   PRIVATE
   PUBLIC  FileResolv
   PUBLIC  ExecResolv
   PUBLIC  remote_cp
   PUBLIC  gunzip
!
! !DESCRIPTION: This module provides routines for resolving GrADS like
!               file name templates. 
!
! !REVISION HISTORY: 
!
!  10Jan2000 da Silva  Initial code.
!  04Mar2001 da Silva  Changed from rcp to scp.
!  12Jun2002 da Silva  Added ExecResolv
!
!EOP
!-------------------------------------------------------------------------

  character(len=255) :: remote_cp = 'scp'
  character(len=255) ::    gunzip = 'gunzip'

CONTAINS

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE: FileResolv -- Resolve file name templates (single file)
! 
! !INTERFACE:
!
    subroutine FileResolv ( expid, nymd, nhms, templ, fname, &
                            stat, cache )  

! !USES:

    IMPLICIT NONE

!
! !INPUT PARAMETERS: 
!
    character(len=*), intent(in) :: expid          ! Experiment id
    integer,          intent(in) :: nymd           ! Year-month-day
    integer,          intent(in) :: nhms           ! Hour-min-sec
    character(len=*), intent(in) :: templ       ! file name template

!
! !OUTPUT PARAMETERS: 
!
    character(len=*),  intent(out) :: fname        ! resolved file name

    integer, OPTIONAL, intent(out) :: stat         ! Status
                                                   !  0 - file exists
                                                   !  1 - file does not exist

    logical, OPTIONAL, intent(in) :: cache         ! skips scp/gunzip if
                                                   ! file exists locally

! !DESCRIPTION: Resolve file name templates, scp'ing files from remote and
!               performing gunzip'ing as necessary.
!
! !TO DO:
!         1. Expand environment variables in templates           
!
! !REVISION HISTORY: 
!
!  10Jan2000  da Silva  Initial code,
!  18may2001  da Silva  Returned after setting stat=3 (file noy found)
!
!EOP
!--------------------------------------------------------------------------

   character(len=*), parameter	:: myname = 'FileResolv'

   integer, external  :: system
   character(len=255) :: path, host, dirn, basen, head, tail, cmd, filen

   integer i, rc
   logical :: fexists, caching


!  Default is cache = .true.
!  -------------------------
   if ( present(cache) ) then
        caching = cache
   else
        caching = .TRUE.
   end if

!  Start by expanding template
!  ---------------------------
   call strTemplate ( path, templ, 'GRADS', trim(expid), nymd, nhms, rc )
   if ( rc .ne. 0 ) then
        if ( present(stat) ) then 
             stat = 1
             return
        else
             call die ( myname, 'cannot expand template '//trim(templ) )
        end if
   end if


!  Parse file name
!  ---------------
   i = index ( trim(path), ':' )
   if ( i .gt. 0 ) then
        host  = path(1:i-1)
        fname = path(i+1:)
   else
        host = ''
        fname = path
   end if
   i = index ( trim(fname), '/', back=.true. )
   if ( i .gt. 1 ) then
        dirn  = fname(1:i-1)
        basen = fname(i+1:) 
   else if ( i .gt. 0 ) then
        dirn  = fname(1:i)
        basen = fname(i+1:) 
   else
        dirn  = ''
        basen = fname 
   end if
   i = index ( basen, '.', back=.true. )
   if ( i .gt. 0 ) then
      head = basen(1:i-1)
      tail = basen(i+1:)
   else
      head = basen
      tail = ''
   end if

!   print *, 'Template = |'//trim(templ)//'|'
!   print *, '   path  = |'//trim(path)//'|'
!   print *, '   host  = |'//trim(host)//'|'
!   print *, '   dirn  = |'//trim(dirn)//'|'
!   print *, '   basen = |'//trim(basen)//'|'
!   print *, '   head  = |'//trim(head)//'|'
!   print *, '   tail  = |'//trim(tail)//'|'
!   print *, '   fname = |'//trim(fname)//'|'


!  If file is remote, bring it here
!  --------------------------------
   if ( len_trim(host) .gt. 0 ) then
      if ( trim(tail) .eq. 'gz' ) then
           inquire ( file=trim(head),  exist=fexists ) 
           filen = head
      else
           inquire ( file=trim(basen), exist=fexists )
           filen = basen
      end if
      if ( .not. ( fexists .and. caching ) ) then
         cmd = trim(remote_cp) // ' ' // &
               trim(host) // ':' // trim(fname) // ' . '
         rc = system ( cmd ) 
         if ( rc .eq. 0 ) then
            fname = basen
         else
            if ( present(stat) ) then
               stat = 2
               return
               fname = basen
               call die ( myname, 'cannot execute: '//trim(cmd) )
            end if
         end if
       else 
         fname = filen
         call warn(myname,'using cached version of '//trim(filen) )
       end if


!  If not, make sure file exists locally
!  -------------------------------------
   else
      if ( trim(tail) .eq. 'gz' ) then
         inquire ( file=trim(basen), exist=fexists ) ! zipped copy local?
         if ( .not. fexists ) then 
            inquire ( file=trim(head), exist=fexists ) ! unzipped copy local?
         end if
      else
         inquire ( file=trim(fname), exist=fexists )
      end if
      if ( .not. fexists ) then
           if ( present(stat) ) then
              stat = 3
              return
           else
              call die(myname,'cannot find '//trim(fname) )
           end if
      end if
 
   end if 


!  If file is gzip'ed, leave original alone and create uncompressed
!  version in the local directory
!  ----------------------------------------------------------------
   if ( trim(tail) .eq. 'gz' ) then
      inquire ( file=trim(head), exist=fexists ) ! do we have a local copy?
      if ( .not. ( fexists .and. caching ) ) then
        if ( len_trim(host) .gt. 0 ) then             ! remove file.gz 
             cmd = trim(gunzip) // ' -f ' // trim(fname) 
        else                                          ! keep   file.gz
             cmd = trim(gunzip) // ' -c ' // trim(fname) // ' > ' // trim(head)
        end if
        rc = system ( cmd ) 
        if ( rc .eq. 0 ) then
           fname = head             
        else
           if ( present(stat) ) then
              stat = 4
              return
           else
              call die ( myname, 'cannot execute: '//trim(cmd) )
           end if
        end if
      else 
         fname = head             
         call warn(myname,'using cached version of '//trim(head) )
      end if
    end if


!   Once more, make sure file exists
!   --------------------------------
    inquire ( file=trim(fname), exist=fexists )
    if ( .not. fexists ) then
       if ( present(stat) ) then
          stat = 3
	  return
       else
          call die(myname,'cannot find '//trim(fname) )
       end if
    end if
 

!   All done
!   --------        
    if ( present(stat) ) stat = 0

  end subroutine FileResolv

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE: ExecResolv -- Resolves executable file names 
! 
! !INTERFACE:
!
    subroutine ExecResolv ( shortfn, longfn, &
                            stat )

! !USES:

    IMPLICIT NONE

!
! !INPUT PARAMETERS: 
!
    character(len=*), intent(in) :: shortfn       ! short executable file name

!
! !OUTPUT PARAMETERS: 
!
    character(len=*), intent(out)  :: longfn       ! executable full file name
    integer, OPTIONAL, intent(out) :: stat         ! Status
                                                   !  0 - file exists
                                                   !  1 - file does not exist


! !DESCRIPTION: Resolve executable file names. For example, given an executable
!               name (e.g., solve.x), it returns the first occurence of this
!  executable on your path (e.g., /home/dasilva/fvdas/bin/solve.x). It works
!  pretty much like the un*x "which" utility.
!
! !REVISION HISTORY: 
!
!  12Jun2002 da Silva  Initial code,
!
!EOP
!--------------------------------------------------------------------------

   character(len=*), parameter	:: myname = 'ExecResolv'

   integer rc, lu
   character(len=255) :: cmd
   integer, external  :: system
   logical :: fexists

   if ( present(stat) ) stat = 0

!  Create temp file with resolved name
!  -----------------------------------
   cmd = 'which ' // trim(shortfn) // ' | tail -1 > .ExecResolv'
   rc = system ( cmd )
   if ( rc .ne. 0 ) then
        if ( present(stat) ) then
             stat = 1
             return
        else
             call die ( myname, 'cannot run '//trim(cmd) )
        end if
  end if

! Retrieve it
! -----------
  lu =  luavail()
  call opntext ( lu, '.ExecResolv', 'old', rc )
  if ( rc .ne. 0 ) then
     if ( present(stat) ) then
        stat = 2
        return
     else
        call die ( myname, 'cannot open file .ExecResolv' )
     end if
  end if
  read(lu,'(a)',iostat=rc) cmd
  call clstext(lu,rc,'delete')

! Bottom line: does the file exist?
! ---------------------------------
  longfn = trim(cmd)
  inquire ( file=trim(longfn), exist=fexists )
  if ( .not. fexists ) then
     if ( present(stat) ) then
        stat = 3
     else
        call die(myname,'strange, cannot find resolved file '//trim(longfn) )
     end if
  end if

  end subroutine ExecResolv
  

  end MODULE m_FileResolv
