! Arlindo da Silva modifications (grep for "!ams"):
!
! - increased max num,ber of args from 64 to 128
! - disable creation of dependency file
! - commented out "splash screen"
! - disabled creation of "duplicate files"; now resulting files will
!   overwrite whatever was there earlier
!
module splitidnt
!  Identity of f90split utility
! ____________________________________________________________________
      character (len=*), parameter :: zsccs = &
"@(#)splitidnt.f90	1.5	98/10/24 Michel Olagnon, Phil Garnatz"
      character (len=*), parameter :: zvers = &
"@(#) splitidnt.f90	V-1.1 98/10/24 Michel Olagnon, Phil Garnatz"
      character (len=*), parameter :: zusg = &
"( usage: f90split < largefile [ > list_file ] )"
      character (len=*), parameter :: zhlp  = '( &
&"Fortran 90 utility to split free source form code into"/&
&"as many files as there are procedures. Contained procedures"/&
&"are stored within their container"/&
&"_____________________________________________________________________"/&
&"All rights to this code waived, so that it may be freely distributed"/&
&"as public domain software subject to the condition that these 6 lines"/&
&"are verbatim reproduced. Originally written by Michel Olagnon, from"/&
&"Ifremer, France, who would be pleased to receive your comments and"/&
&"corrections. M. Olagnon (Michel.Olagnon@ifremer.fr) Improved by"/&
&"Phil Garnatz, Cray Research Inc. for makefile generation"/&
&"_____________________________________________________________________"/&
&"                    version 1.1 of 24 Oct 1998"/&
&"  Split standard input stream, containing source of several fortran90"/&
&"  program units into individual files, each containing a single"/&
&"  program unit, named after it, or main0001.f90-main9999.f90, or"/&
&"  bdta0001.f90-bdta9999.f90. If a file with that name already exists,"/&
&"  it is put in dupl0001.f90-dupl9999.f90."/&
&"  Lists on stdout the use and include dependencies"/&
&"_____________________________________________________________________"/&
&"Note: If you do not like code to start in column 7, remember that,"/&
&"      had Diophantes left a 6 characters margin, then mathematicians"/&
&"      might have spared much efforts on A**N = B**N + C**N ..."/&
&"      My margin is wide to let you put your corrections there."/&
&"____________________________________________________________________")'
!
end module splitidnt
module splitprms
!  Parameters for f90split utility
! ____________________________________________________________________
      character (len=26), parameter :: zlwc="abcdefghijklmnopqrstuvwxyz"
      character (len=26), parameter :: zupc="ABCDEFGHIJKLMNOPQRSTUVWXYZ"
      character (len=10), parameter :: zdgt="1234567890"
      character  (len=1), parameter :: ztab=char(9)
      integer, parameter              :: lend = 3
      character (len=lend), parameter :: zend = "END"
      integer, parameter              :: lctn = 8
      character (len=lctn), parameter :: zctn = "CONTAINS"
      integer, parameter              :: lntf = 9
      character (len=lntf), parameter :: zntf = "INTERFACE"
      integer, parameter              :: lsub = 10
      character (len=lsub), parameter :: zsub = "SUBROUTINE"
      integer, parameter              :: lpgm = 7
      character (len=lpgm), parameter :: zpgm = "PROGRAM"
      integer, parameter              :: lmdl = 6
      character (len=lmdl), parameter :: zmdl = "MODULE"
      integer, parameter              :: lfun = 8
      character (len=lfun), parameter :: zfun = "FUNCTION"
      integer, parameter              :: lbdt = 9
      character (len=lbdt), parameter :: zbdt = "BLOCKDATA"
      integer, parameter               :: lbdt1 = 5
      character (len=lbdt1), parameter :: zbdt1 = "BLOCK"
      integer, parameter               :: lbdt2 = 4
      character (len=lbdt2), parameter :: zbdt2 = "DATA"
      integer, parameter              :: luse = 3
      character (len=luse), parameter :: zuse = "USE"
      integer, parameter              :: linc = 7
      character (len=linc), parameter :: zinc = "INCLUDE"
!
      character (len=*), parameter :: zbasp = "main0000"
      character (len=*), parameter :: zbasb = "bdta0000"
      character (len=*), parameter :: zbasm = "modl0000"
      character (len=*), parameter :: zbasd = "dupl0000"
      character (len=*), parameter :: zbask = "dpds0000"
      character (len=*), parameter :: zfmtn =    "(i4.4)"
      integer, parameter  :: ifmts = 5 ! start pos. in names
      integer, parameter  :: ifmte = 8 ! end  pos. in names
      integer, parameter  :: nnamm = 9999 ! number max in names
      integer, parameter  :: klwc = -1 ! case processing: to lower
      integer, parameter  :: kupc =  1 ! case processing: to upper
      integer, parameter  :: klve =  0 ! case processing: leave as is
      integer, parameter  :: kpgm =  0 ! code for main program
      integer, parameter  :: kbdt =  1 ! code for block data
      integer, parameter  :: ksub =  2 ! code for subroutine
      integer, parameter  :: kfun =  3 ! code for function
      integer, parameter  :: kmdl =  4 ! code for module
      integer, parameter  :: kdup =  5 ! code for duplicate
      integer, parameter  :: kdpd =  6 ! code for dependencies
      integer, parameter  :: kend = -1 ! code for end-of-input
      integer, parameter  :: ktabn = 0 ! assume no TABs
      integer, parameter  :: ktabi = 1 ! accept TABs, no expand
      integer, parameter  :: ktabe = 2 ! expand TABs
      integer, parameter  :: nplam = 3 ! # of plans to expand TABs
      integer, parameter  :: luerr = 0 ! logical unit for stderr
      integer, parameter  :: lutmp = 2 ! logical unit for temp. file
      integer, parameter  :: lufil = 3 ! logical unit for final file
      integer, parameter  :: luinp = 5 ! logical unit for stdin
      integer, parameter  :: ludpd = 7 ! logical unit for depend file
      integer, parameter  :: lnamm = 31 ! max. variable name length
      integer, parameter  :: lfilm = 64 ! max. file name length
      integer, parameter  :: ncntm = 39 ! max. # cont. lines
      integer, parameter  :: linem = 132 ! max. line length
      integer, parameter  :: lsttm = (linem-1)*ncntm+linem
                             ! max. sttmt. length
      integer, parameter  :: ndepm =  100 ! max use/include deps
! The following declaration is technically non-standard in Fortran90:
! (the "max" function is not required to be accepted in a parameter
! statement)  to fix this, I added a contained routine, called at the
! beginning of the main program.
!     integer, parameter, dimension (linem, nplam) :: nxttab =  &
!     reshape (                                                 &
!              (/ max( (/ (6+3*((i-6+3)/3), i= 1,linem),        &
!                         (6+2*((i-6+2)/2), i= 1,linem) /),     &
!                      (/ (6, i= 1, 2*linem) /)            ),   &
!                 (/                      (i, i= 1,linem) /) /),&
!               (/ linem, nplam /) )
      integer,            dimension (linem, nplam) :: nxttab =  &
      reshape (                                                 &
               (/  (/ (6+3*((i-6+3)/3), i= 1,linem) /),         &
                   (/ (6+2*((i-6+2)/2), i= 1,linem) /),         &
                   (/               (i, i= 1,linem) /) /),      &
                (/ linem, nplam /) )
contains
 subroutine maxnxt
  nxttab(:,1:2) = max(6,nxttab(:,1:2))
 end subroutine maxnxt

end module splitprms
module splitdefs
!  Default settings for f90split utility
use splitprms
! ____________________________________________________________________
!ams  character (len=*), parameter :: zsuff = ".f90"
      character (len=*), parameter :: zsuff = "___.F90"
      character (len=*), parameter :: zsufk = ".mk"
      character (len=*), parameter :: zsufm = ".mod"
      character (len=*), parameter :: zsufo = ".o"
      integer  :: ktab =  ktabe
      integer  :: kcas =  klve ! code for case processing
!ams  integer  :: kmkd =  1    ! code for making dependencies
      integer  :: kmkd =  0    ! code for making dependencies
end module splitdefs
module splitcurs
use splitprms
!  Current status variables in f90split utility
! ____________________________________________________________________
      integer, save  :: nlini =  0 ! Lines input
      integer, save  :: nlins =  0 ! in current sub-unit
      integer, save  :: iplac =  1 ! plan for TAB expansion
      integer, save  :: mlins =  0 ! max line length
      integer, save  :: ndep  =  0 ! number of use/includes deps
      integer, save  :: iflina = 0   ! advance line is multiple
      integer, save  :: llina =  -1  ! length of advance stored line
      character (len=linem) :: zlina ! line in advance
      character (len=lfilm), dimension (ndepm) :: zdept
                                   ! current dependencies
end module splitcurs
program f90split
!  Split standard input stream, containing source of several fortran90
!  program units into individual files, each containing a single
!  program unit, named after it, or main0001.f90-main9999.f90, or
!  bdta0001.f90-bdta9999.f90. If a file with that name already exists,
!  it is put in dupl0001.f90-dupl9999.f90.
! ____________________________________________________________________
      use splitidnt
      use splitdefs
      use splitcurs
! ____________________________________________________________________
!
      character (len=lfilm) :: zfil, zdpd
      character (len=lnamm) :: znam
!
!ams      write (luerr, "(a)") "This is f90split: " // zvers
!ams      write (luerr, "(a)")  zusg
      call maxnxt
body: do
!
!  Open temporary output file
!
         open (lutmp, status='scratch', iostat=kerr)
         if (kerr /= 0) then
            write (luerr,*) "Unable to open scratch file"
            exit body
         endif
!
!  Open dependencies file
!
         if (kmkd == 1) then
            call nxtnam (kdpd, zdpd, kerr)
            if (kerr /= 0) then
               write (luerr,*) "Name space exhausted"
               exit body
            endif
            open (ludpd, file=trim(zdpd)//zsufk, iostat=kerr)
            if (kerr /= 0) then
               write (luerr,*) "Unable to open dependencies file"
               exit body
            endif
            write (luerr, "(a, a)") "Writing dependencies to ",   &
                                    trim (zdpd) // zsufk
         endif
!
         do
            nlins = 0
            if (ktab == ktabe) iplac = 1
!
!  Find type and name of unit
!
            call fndfst (kunt, znam, kerr)
            if (kunt == kend) then
!ams           write (luerr, *) "Trailing comments removed"
               exit body
            endif
            if (kerr /= 0) then
               exit body
            endif
!
!  Find name for corresponding file
!
            call getnam (kunt, znam, zfil, kerr)
            lfil = len_trim (zfil)
            if (kerr /= 0) then
               write (luerr,*) "Name space exhausted"
               exit body
            else
               if (kunt == kdup) then
                  write (luerr, *) trim (znam), "->", zfil (1:lfil)
               endif
            endif
!
!  Find end of current program unit
!
            if (kmkd == 1) then
               call fndiue (kerr)
            else
               call fndend (kerr)
            endif
            if (kerr /= 0) then
               write (luerr,*) zfil (1:lfil), " : Missing END statement"
            endif
            rewind lutmp
!
!  Copy scratch file to destination
!
            call cpyfil (zfil, kerr)
            if (kerr /= 0) then
               write (luerr,*) zfil (1:lfil), " : Unable to write"
               exit body
            else
               if (kmkd == 1) then
                  lsuf = len (zsuff)
                  write (ludpd,*) zfil (1:lfil-lsuf) // zsufo // " : ",&
                                  zfil (1:lfil),                       &
                                  (" ", trim (zdept (i)), i=1, ndep)
                  if (zsufm /= zsufo .and. kunt == kmdl) then
                  write (ludpd,*) zfil (1:lfil-lsuf) // zsufm // " : ",&
                                  zfil (1:lfil),                       &
                                  (" ", trim (zdept (i)), i=1, ndep)
                  endif
                  ndep = 0
               endif
               write (*,*) zfil (1:lfil)
            endif
!
!  Loop to next program unit
!
            rewind lutmp
         enddo
!
      enddo body
      close (lutmp)
end program f90split
subroutine fndfst (kunt, znam, kerr)
!  Read input file, copying it to the scratch file, until the first
!  non-comment statement is found.
!  Analyse this statement, and decide of the type and name of the
!  program unit that starts there.
use splitprms
use splitcurs
integer, intent (out) :: kunt           ! type of program unit
character (len=*), intent (out) :: znam ! name chosen
integer, intent (out) :: kerr           ! error code
! ____________________________________________________________________
      character (len=lsttm) :: zstt
!
      call nxtstt (zstt, ksta)
      if (ksta == 0) then
         call nlzfst (zstt, kunt, znam)
      elseif (ksta < 0) then
         if (nlins > 0) then
            kunt = kend
         else
            kunt = kpgm
         endif
         kerr = -1
      else
         kunt = kpgm
         znam = ' '
         kerr = ksta
      endif
end subroutine fndfst
subroutine fndiue (kerr)
!  Read input file, copying it to the scratch file, and
!  looking for dependencies, until an END statement is found.
use splitdefs
use splitcurs
integer, intent (out) :: kerr           ! error code
! ____________________________________________________________________
      character (len=lsttm) :: zstt
      character (len=lnamm) :: znam
      integer, save         :: jlvl = 0
      integer, save         :: jntf = 0
!
      ifnew = 0
      do
!
!  Get next statement
!
         call nxtstt (zstt, ksta)
         if (ksta /= 0) then
            kerr = 1
            exit
         endif
!
!  Look for USE, INCLUDE, or END of sub-unit or of INTERFACE
!
         call nlziue (zstt, klst)
         select case (klst)
         case (-1)! problem
            kerr = 1
            exit
         case (0) ! not end of anything
            continue
         case (1) ! end of sub-unit
            if (jlvl <= 0 .and. jntf == 0) then
               kerr = 0
               exit
            endif
            if (jlvl > 0) jlvl = jlvl - 1
            ifnew = 1
            cycle
         case (2) ! end of interface
            if (jntf <= 0) then
               write (luerr, *) "END INTERFACE out of place"
            else
               jntf = jntf - 1
            endif
            ifnew = 0
            cycle
         end select
!
!  Look for INTERFACE statement
!
         call fndntf (zstt, ifntf)
         if (ifntf /= 0) then
            jntf = jntf + 1
            ifnew = 1
            cycle
         endif
!
!  Look for CONTAINS statement
!
         call fndctn (zstt, ifctn)
         if (ifctn /= 0) then
            ifnew = 1
            cycle
         endif
!
!  Look for start of new unit
!
         if (ifnew /= 0) then
            call nlzfst (zstt, kunt, znam)
            if (kunt == ksub .or. kunt == kfun) then
               jlvl = jlvl + 1
               ifnew = 0
               cycle
            endif
         endif
      enddo
end subroutine fndiue
subroutine fndend (kerr)
!  Read input file, copying it to the scratch file, until an
!  END statement is found.
use splitdefs
integer, intent (out) :: kerr           ! error code
! ____________________________________________________________________
      character (len=lsttm) :: zstt
      character (len=lnamm) :: znam
      integer, save         :: jlvl = 0
      integer, save         :: jntf = 0
!
      ifnew = 0
      do
!
!  Get next statement
!
         call nxtstt (zstt, ksta)
         if (ksta /= 0) then
            kerr = 1
            exit
         endif
!
!  Look for END of sub-unit or of INTERFACE
!
         call nlzlst (zstt, klst)
         select case (klst)
         case (-1)! problem
            kerr = 1
            exit
         case (0) ! not end of anything
            continue
         case (1) ! end of sub-unit
            if (jlvl <= 0 .and. jntf == 0) then
               kerr = 0
               exit
            endif
            if (jlvl > 0) jlvl = jlvl - 1
            ifnew = 1
            cycle
         case (2) ! end of interface
            if (jntf <= 0) then
               write (luerr, *) "END INTERFACE out of place"
            else
               jntf = jntf - 1
            endif
            ifnew = 0
            cycle
         end select
!
!  Look for INTERFACE statement
!
         call fndntf (zstt, ifntf)
         if (ifntf /= 0) then
            jntf = jntf + 1
            ifnew = 1
            cycle
         endif
!
!  Look for CONTAINS statement
!
         call fndctn (zstt, ifctn)
         if (ifctn /= 0) then
            ifnew = 1
            cycle
         endif
!
!  Look for start of new unit
!
         if (ifnew /= 0) then
            call nlzfst (zstt, kunt, znam)
            if (kunt == ksub .or. kunt == kfun) then
               jlvl = jlvl + 1
               ifnew = 0
               cycle
            endif
         endif
      enddo
end subroutine fndend
subroutine cpyfil (zfil, kerr)
!  Copy scratch file to the final file
use splitdefs
use splitcurs
character (len=*), intent (in) :: zfil    ! file name
integer, intent (out) :: kerr             ! error code
! ____________________________________________________________________
      character (len=linem) :: zlin
!
      kerr = 0
body: do
!
!  Open final output file
!
         open (lufil, file=zfil, iostat=kerr)
         if (kerr /= 0) then
            write (luerr,*) "Unable to open final file"
            exit body
         endif
         do
            read (lutmp, "(a)", iostat=krea) zlin
            select case (krea)
            case (1:)
               write (luerr,*) "Problem reading scratch file"
               exit body
            case (:-1)
               exit
            case (0)
               llin = len_trim (zlin)
               if (ktab == ktabe) then
                  call xpdtab (zlin, llin)
               endif
               write (lufil, "(a)", iostat=kwri) zlin (1:llin)
               if (kwri /= 0) then
                  write (luerr,*) "Problem writing file ", zfil
                  exit body
               endif
            end select
         enddo
         close (lufil)
         exit body
      enddo body
end subroutine cpyfil
subroutine nxtstt (zstt, ksta)
!  Get (possibly multiple) non-comment statement and extract
!  single statement out of it
use splitcurs
character (len=lsttm), intent (out) :: zstt
integer, intent (out)               :: ksta ! status code
! ____________________________________________________________________
      character (len=1)           :: zdlm
      character (len=lsttm), save :: zmul
      integer, save               :: istt = 0
      integer, save               :: istts
      integer, save               :: lmul
!
      ksta = 0
body: do
         if (istt == 0) then
!
!  Get a (possibly multiple) non-comment statement
!
            call reastt (zmul, lmul, kget)
!
            if (kget /= 0) then
               ksta = kget
               istt = 0
               exit body
            else
               istt = 1
            endif
         endif
!
!  Look for character context
!
         ifchc1 = 0
         iloo   = istt
         lstt   = lmul
         do
!
!  Outside of character context, truncate at ; if any
!
            if (ifchc1 == 0) then
               ichc0 = scan (zmul (iloo:lstt), "'"//'"')
               if (ichc0 == 0) then
                  ismc = index (zmul (iloo:lstt), ';')
               else
                  ismc = index (zmul (iloo:ichc0), ';')
               endif
               if (ismc > 0) then
                  lstt = iloo + ismc - 2
                  exit
               elseif (ichc0 > 0) then
                  ifchc1 = 1
                  iloo = iloo + ichc0
                  zdlm = zmul (iloo-1:iloo-1)
               else
                  exit
               endif
            else
!
!  Within character context, look for its termination
!
               ichc1 = scan (zmul (iloo:lstt), zdlm)
               if (ichc1 == 0) then
                  exit
               else
                  ifchc1 = 0
                  iloo  = iloo + ichc1
               endif
            endif
         enddo
!
!  Copy current statement into zstt
!
         if (istts > 0) then
            zstt = repeat (" ", istts) // zmul (istt:lstt)
         else
            zstt = zmul (istt:lstt)
         endif
         if (istt == 1 .and. lstt == lmul) then
            iflina = 0
         else
            iflina = 1
         endif
         if (istts == 0 .and. lstt < lmul) then
            istts = verify (zmul (1:lmul), ' ') - 1
         elseif (lstt == lmul) then
            istts = 0
         endif
         if (lstt+1 < lmul) then
            istt = lstt + verify (zmul (lstt+2:lmul), ' ') + 1
         else
            istt = 0
         endif
         if (len_trim (zstt) > 0) then
            exit body
         endif
      enddo body
end subroutine nxtstt
subroutine reastt (zmul, lstt, ksta)
!  Read input file, copying it to the scratch file, until a
!  (possibly multiple) non-comment statement is found.
use splitdefs
use splitcurs
character (len=lsttm), intent (out) :: zmul
integer, intent (out)               :: lstt ! istt. length
integer, intent (out)               :: ksta ! status code
! ____________________________________________________________________
      character (len=linem) :: zlin
      character (len=1)     :: zdlm
      character (len=1), parameter :: zcr = achar (13)
!
      lstt  = 0
      ifchc0 = 0
      ifcnt0 = 0
      do
!
!  Something to write ?
!  Write advance line to scratch file
!
            if (llina > 0) then
               write (lutmp, "(a)", iostat=kwri) zlina (1:llina)
               if (kwri /= 0) then
                  write (luerr,*) "Problem writing scratch file"
                  ksta = 2
                  exit
               endif
            elseif (llina == 0) then
               write (lutmp, "()", iostat=kwri)
               if (kwri /= 0) then
                  write (luerr,*) "Problem writing scratch file"
                  ksta = 2
                  exit
               endif
            endif
!
!  Read a line
!
         read (luinp, "(a)", iostat=krea) zlin
!
         select case (krea)
         case (1:)
            ksta = 1
            llina = -1
            write (luerr,*) "Problem reading input"
            exit
         case (:-1)
            ksta = -1
            llina = -1
            exit
         case (0)
            ksta = 0
            nlini = nlini + 1
            nlins = nlins + 1
            llin  = len_trim (zlin)
!
!  remove trailing <CR> if any
!
            llina = llin
            if (llin <= 0) cycle
            if (zlin (llin:llin) == zcr) Then
               llin = llin - 1
               llina = llin
               if (llin <= 0) cycle
            endif
            zlina (1:llina) = zlin (1:llin)
!
!  process TABs
!
            select case (ktab)
            case (ktabi)
               call rmvtab (zlin, llin)
               mlins = max (mlins, llin)
            case (ktabn)
               mlins = max (mlins, llin)
            case (ktabe)
               call chktab (zlin, llin)
               call rmvtab (zlin, llin)
            endselect
!
!  Recognize and skip comments
!
            ifst = verify (zlin (1:llin), ' ')
            if (ifst == 0) cycle
            if (zlin (ifst:ifst) == '!') cycle
!
!  Recognize and skip pre-processing commands
!
            if (zlin (ifst:ifst) == '$') cycle
            if (zlin (ifst:ifst) == '#') cycle
!
!  Do not explore trailing comments if any
!
!  Look for character context
!
            ifchc1 = ifchc0
            iloo = ifst
            lxpl = llin
            do
!
!  Outside of character context, truncate at ! if any
!
               if (ifchc1 == 0) then
                  ichc0 = scan (zlin (iloo:llin), "'"//'"')
                  if (ichc0 == 0) then
                     icmt = index (zlin (iloo:llin), '!')
                  else
                     icmt = index (zlin (iloo:ichc0), '!')
                  endif
                  if (icmt > 0) then
                     ltmp = iloo + icmt - 2
                     lxpl = len_trim (zlin (1:ltmp))
                     exit
                  elseif (ichc0 > 0) then
                     ifchc1 = 1
                     iloo = iloo + ichc0
                     zdlm = zlin (iloo-1:iloo-1)
                  else
                     exit
                  endif
               else
!
!  Within character context, look for its termination
!
                  ichc1 = scan (zlin (iloo:llin), zdlm)
                  if (ichc1 == 0) then
                     exit
                  else
                     ifchc1 = 0
                     iloo  = iloo + ichc1
                  endif
               endif
            enddo
!
!  Look for continuation mark
!
            if (zlin (lxpl:lxpl) == '&') then
               ifcnt1 = 1
               llin   = len_trim (zlin (1:lxpl-1))
            else
               ifcnt1 = 0
            endif
!
!  Copy current statement fragment into zmul
!
!  Look for continued mark
!
            if (zlin (ifst:ifst) == '&') then
               ifst = ifst + verify (zlin (ifst+1:llin), ' ')
               if (ifchc0 == 0) then
                  lstt = lstt + 1
                  zmul (lstt:lstt) = ' '
               endif
            endif
!
!  Copy
!
            if (ifst > 1) then
               zmul (lstt+1:lstt+ifst-1) = " "
               lstt = lstt + ifst - 1
            endif
            lfrg = llin - ifst + 1
            zmul (lstt+1:lstt+lfrg) = zlin (ifst:llin)
            lstt = lstt + lfrg
            if (ifcnt1 == 0) exit
         end select
!
!  Loop until end of statement
!
         ifcnt0 = ifcnt1
         ifchc0 = ifchc1
      enddo
end subroutine reastt
subroutine nlzfst (zstt, kunt, znam)
!  Analyse a statement, and decide of the type (and name) of the
!  program unit that starts there.
use splitcurs
character (len=lsttm), intent (in) :: zstt ! the statement
integer, intent (out) :: kunt              ! type of program unit
character (len=*), intent (out) :: znam    ! name chosen
! ____________________________________________________________________
      character (len=lsttm) :: zsttw, zsttw1
      logical               :: ifwrk
!
body: do
!
!  Raise to upper case (No label to be removed)
!
         zsttw = adjustl (zstt)
         call raicas (zsttw)
!
!  Look for PROGRAM
!
         lstt = len_trim (zsttw)
         ipgm = index (zsttw (1:lstt), zpgm)
         if (ipgm == 1) then
            kunt = kpgm
            ikwdf = lpgm
         else
!
!  Look for MODULE
!
            imdl = index (zsttw (1:lstt), zmdl)
            if (imdl == 1) then
               kunt = kmdl
               ikwdf = lmdl
            else
!
!  Look for FUNCTION
!
               ifun = index (zsttw (1:lstt), zfun)
               if (ifun <= 1) then
                  ifwrk = (ifun == 1)
               else
                  ifwrk = (zsttw (ifun-1:ifun-1) == ' ')
               endif
               if (ifwrk) then
                  kunt = kfun
                  ikwdf = lfun + ifun - 1
               else
!
!  Look for SUBROUTINE
!
                  isub = index (zsttw (1:lstt), zsub)
                  if (isub <= 1) then
                     ifwrk = (isub == 1)
                  else
                     ifwrk = (zsttw (isub-1:isub-1) == ' ')
                  endif
                  if (ifwrk) then
                     kunt = ksub
                     ikwdf = lsub + isub - 1
                  else
!
!  Look for BLOCK DATA
!
                     ibdt1 = index (zsttw (1:lstt), zbdt1)
                     if (ibdt1 == 1) then
                        ikwdf = lbdt1 &
                              + verify (zsttw (lbdt1+1:lstt), ' ') &
                              - 1
                        if (ikwdf >= lbdt1) then
                           ibdt2 = index (zsttw (ikwdf+1:lstt), zbdt2)
                           if (ibdt2 == 1) then
                              kunt = kbdt
                              ikwdf = ikwdf + lbdt2
                           else
                              kunt = kpgm
                              znam = ' '
                              exit body
                           endif
                        else
                           kunt = kpgm
                           znam = ' '
                           exit body
                        endif
                     else
                        kunt = kpgm
                        znam = ' '
                        exit body
                     endif
                  endif
               endif
            endif
         endif
!
!  Find name
!
         inams = ikwdf + verify (zsttw (ikwdf+1:lstt), ' ')
         if (inams < ikwdf+2) then
            if (kunt /= kbdt) kunt = kpgm
            znam = ' '
            exit body
         endif
         iname = inams   &
               + verify (zsttw (inams+1:lstt+1), zupc//zdgt//"_") &
               - 1
         if (iname < inams) then
            if (kunt /= kbdt) kunt = kpgm
            znam = ' '
            exit body
         endif
         zsttw1 = adjustl (zstt)
         znam = zsttw1 (inams:iname)
         exit body
      enddo body
      kwri = 0
      if (iflina /= 0 .and. llina < 0) then
         write (lutmp, "(a)", iostat=kwri) trim (zstt)
      elseif (llina >= 0) then
         if (iflina == 0) then
            write (lutmp, "(a)", iostat=kwri) zlina (1:llina)
         else
            write (lutmp, "(a)", iostat=kwri) trim (zstt)
         endif
         llina = -1
      endif
      if (kwri /= 0) then
         write (luerr,*) "Problem writing scratch file"
      endif
end subroutine nlzfst
subroutine nlziue (zstt, klst)
!  Analyse a statement, and decide if it is use, include, or if current
!  program unit ends there.
use splitcurs
use splitdefs
character (len=lsttm), intent (in) :: zstt ! The statement
integer, intent (out) :: klst              ! result
! ____________________________________________________________________
      character (len=lsttm) :: zsttw
      character (len=1)     :: zdlm
!
body: do
         zsttw = adjustl (zstt)
!
!  Remove label and raise to upper case
!
         call rmvlbl (zsttw)
         call raicas (zsttw)
!
!  Look for first token, to be INCLUDE, USE, or END
!
         itokf = verify (zsttw, zupc) - 1
         lstt = len_trim (zsttw)
         klst = 0
         if (itokf == luse) then
             if (zsttw(1:luse) == zuse) then
!
!  Look for [space] use name
!
                 itoks = luse + verify (zsttw (luse+1:lstt), ' ')
                 itoke = itoks + verify (zsttw (itoks+1:lstt+1),        &
                                         zupc//zdgt//"_") - 1
                 if (ndep < ndepm) then
                    ndep = ndep + 1
                    zdept (ndep) = zsttw (itoks:itoke) // zsufm
                    call lwrcas (zdept (ndep))
                    do idep = 1, ndep - 1
                      if(zdept (idep) == zdept (ndep)) then
                        ndep = ndep - 1
                        exit body
                      end if
                    end do
                 endif
                 exit body
             endif
         endif

         if (itokf == linc) then
             if (zsttw(1:linc) == zinc) then
!
!  Look for [space] 'include_string' or "include_string"
!
                 itoks = linc + verify (zsttw (linc+1:lstt), ' ')
                 zdlm  = zsttw (itoks:itoks)
                 if(zdlm /= '"' .and. zdlm /= "'") then
                    exit body
                 end if
                 itoks = itoks + 1
                 itoke = itoks + index (zsttw (itoks+1:lstt), zdlm) - 1
                 if(itoke == itoks-1) then
                     exit body    ! no trailing delim found
                 endif

                 if (ndep < ndepm) then
                    ndep = ndep + 1
                    zdept (ndep) = zsttw (itoks:itoke)
                    do idep = 1, ndep - 1
                       if (zdept (idep) == zdept (ndep)) then
                          ndep = ndep - 1
                          exit body
                       end if
                    end do
                 endif
                 exit body
             endif
         endif

         if (itokf < lend) then
            klst = 0
            exit body
         endif
         if (zsttw (1:lend) /= zend) then
            klst = 0
            exit body
         endif
!
!  Nothing after END
!
         if (lstt == lend) then
            klst = 1
            exit body
         endif
!
!  Look for [space] unit name
!
         itoks = lend + verify (zsttw (lend+1:lstt), ' ')
         itoke = itoks + index (zsttw (itoks+1:lstt+1), ' ') - 1
         if (itoke < itoks+2) then
            klst = 0
            exit body
         endif
         if (    (zsttw (itoks:itoke) == zpgm)     &
             .or.(zsttw (itoks:itoke) == zsub)     &
             .or.(zsttw (itoks:itoke) == zfun)     &
             .or.(zsttw (itoks:itoke) == zbdt)     & ! Be laxist
             .or.(zsttw (itoks:itoke) == zmdl)     ) then
            klst = 1
            exit body
         elseif (zsttw (itoks:itoke) == zntf) then
            klst = 2
            exit body
         elseif (zsttw (itoks:itoke) == zbdt1) then
            itoks = itoke + verify (zsttw (itoke+1:lstt), ' ')
            if (itoks < itoke+2) then
               klst = 0
               exit body
            endif
            itoke = itoks + index (zsttw (itoks+1:lstt+1), ' ') - 1
            if (itoke < itoks+2) then
               klst = 0
               exit body
            endif
            if (zsttw (itoks:itoke) == zbdt2) then
               klst = 1
               exit body
            else
               klst = 0
               exit body
            endif
         else
            klst = 0
            exit body
         endif
      enddo body
      kwri = 0
      if (iflina /= 0 .and. llina < 0) then
         write (lutmp, "(a)", iostat=kwri) trim (zstt)
      else
         if (iflina == 0) then
            write (lutmp, "(a)", iostat=kwri) zlina (1:llina)
         else
            write (lutmp, "(a)", iostat=kwri) trim (zstt)
         endif
         llina = -1
      endif
      if (kwri /= 0) then
         write (luerr,*) "Problem writing scratch file"
         klst = -1
      endif
end subroutine nlziue
subroutine nlzlst (zstt, klst)
!  Analyse a statement, and decide if the current
!  program unit ends there.
use splitcurs
character (len=lsttm), intent (in) :: zstt ! The statement
integer, intent (out) :: klst              ! result
! ____________________________________________________________________
      character (len=lsttm) :: zsttw
!
body: do
         zsttw = adjustl (zstt)
!
!  Remove label and raise to upper case
!
         call rmvlbl (zsttw)
         call raicas (zsttw)
!
!  Look for first token, to be END
!
         itokf = verify (zsttw, zupc) - 1
         if (itokf < lend) then
            klst = 0
            exit body
         endif
         if (zsttw (1:lend) /= zend) then
            klst = 0
            exit body
         endif
!
!  Nothing after END
!
         lstt = len_trim (zsttw)
         if (lstt == lend) then
            klst = 1
            exit body
         endif
!
!  Look for [space] unit name
!
         itoks = lend + verify (zsttw (lend+1:lstt), ' ')
         itoke = itoks + index (zsttw (itoks+1:lstt+1), ' ') - 1
         if (itoke < itoks+2) then
            klst = 0
            exit body
         endif
         if (    (zsttw (itoks:itoke) == zpgm)     &
             .or.(zsttw (itoks:itoke) == zsub)     &
             .or.(zsttw (itoks:itoke) == zfun)     &
             .or.(zsttw (itoks:itoke) == zbdt)     & ! Be laxist
             .or.(zsttw (itoks:itoke) == zmdl)     ) then
            klst = 1
            exit body
         elseif (zsttw (itoks:itoke) == zntf) then
            klst = 2
            exit body
         elseif (zsttw (itoks:itoke) == zbdt1) then
            itoks = itoke + verify (zsttw (itoke+1:lstt), ' ')
            if (itoks < itoke+2) then
               klst = 0
               exit body
            endif
            itoke = itoks + index (zsttw (itoks+1:lstt+1), ' ') - 1
            if (itoke < itoks+2) then
               klst = 0
               exit body
            endif
            if (zsttw (itoks:itoke) == zbdt2) then
               klst = 1
               exit body
            else
               klst = 0
               exit body
            endif
         else
            klst = 0
            exit body
         endif
      enddo body
      kwri = 0
      if (iflina /= 0 .and. llina < 0) then
         write (lutmp, "(a)", iostat=kwri) trim (zstt)
      elseif (llina >= 0) then
         if (iflina == 0) then
            write (lutmp, "(a)", iostat=kwri) zlina (1:llina)
         else
            write (lutmp, "(a)", iostat=kwri) trim (zstt)
         endif
         llina = -1
      endif
      if (kwri /= 0) then
         write (luerr,*) "Problem writing scratch file"
         klst = -1
      endif
end subroutine nlzlst
subroutine fndctn (zstt, ifctn)
use splitprms
!  Look for CONTAINS statement
character (len=lsttm), intent (in) :: zstt ! The statement
integer, intent (out) :: ifctn
! ____________________________________________________________________
!
      character (len=lsttm) :: zsttw
!
body: do
         zsttw = adjustl (zstt)
!
!  Remove label and raise to upper case
!
         call rmvlbl (zsttw)
         call raicas (zsttw)
!
!  Look for first token, to be CONTAINS
!
         itokf = verify (zsttw, zupc) - 1
         if (itokf /= lctn) then
            ifctn = 0
            exit body
         endif
         if (zsttw (1:lctn) /= zctn) then
            ifctn = 0
            exit body
         endif
!
!  Nothing after CONTAINS
!
         lstt = len_trim (zsttw)
         if (lstt == lctn .and. zsttw (1:lctn) == zctn) then
            ifctn = 1
            exit body
         else
            ifctn = 0
            exit body
         endif
      enddo body
end subroutine fndctn
subroutine fndntf (zstt, ifntf)
use splitprms
!  Look for INTERFACE statement
character (len=lsttm), intent (in) :: zstt ! The statement
integer, intent (out) :: ifntf
! ____________________________________________________________________
!
      character (len=lsttm) :: zsttw
!
body: do
         zsttw = adjustl (zstt)
!
!  Remove label and raise to upper case
!
         call rmvlbl (zsttw)
         call raicas (zsttw)
!
!  Look for first token, to be INTERFACE
!
         itokf = verify (zsttw, zupc) - 1
         if (itokf /= lntf) then
            ifntf = 0
            exit body
         endif
         if (zsttw (1:lntf) /= zntf) then
            ifntf = 0
            exit body
         endif
!
!  Nothing after INTERFACE
!
         lstt = len_trim (zsttw)
         if (lstt == lntf .and. zsttw (1:lntf) == zntf) then
            ifntf = 1
            exit body
         elseif (lstt > lntf .and. zsttw (1:lntf) == zntf) then
            if (zsttw (lntf+1:lntf+1) == ' ') then
               ifntf = 1
               exit body
            else
               ifntf = 0
               exit body
            endif
         else
            ifntf = 0
            exit body
         endif
      enddo body
end subroutine fndntf
subroutine getnam (kunt, znam, zfil, kerr)
!  Return a file name from the type (and name) of the
!  program unit that is processed.
use splitdefs
integer, intent (inout) :: kunt            ! type of program unit
character (len=*), intent (in) :: znam     ! name if any
character (len=*), intent (out) :: zfil    ! file name
integer, intent (out) :: kerr              ! error code
! ____________________________________________________________________
      logical :: ifxst
      character (len=lnamm) :: znamw
!
!  Change according to desired case
!
      znamw = znam
      lnam = len_trim (znamw)
      select case (kcas)
      case (-1)
         call lwrcas (znamw (1:lnam))
      case (+1)
         call raicas (znamw (1:lnam))
      case default
         continue
      end select
      if (lnam > 0) then
!
!  Check that name is valid
!
         zfil = znamw (1:lnam) // zsuff
         inquire (file=zfil, exist=ifxst)
!ams     if (ifxst) then
!ams        kunt = kdup
!ams        call nxtnam (kunt, znamw, kerr)
!ams        lnam = len_trim (znamw)
!ams        zfil = znamw (1:lnam) // zsuff
!ams     else
            kerr = 0
!ams     endif
      else
         call nxtnam (kunt, znamw, kerr)
         lnam = len_trim (znamw)
         zfil = znamw (1:lnam) // zsuff
      endif
end subroutine getnam
subroutine nxtnam (kunt, znam, kerr)
!  Return the next name for the type of the
!  program unit that is processed.
use splitdefs
integer, intent (in) :: kunt               ! type of program unit
character (len=*), intent (out) :: znam    ! name
integer, intent (out) :: kerr              ! error code
! ____________________________________________________________________
      logical       :: ifxst
      integer, save :: idpd = 0
      integer, save :: ipgm = 0
      integer, save :: ibdt = 0
      integer, save :: idup = 0
      integer, save :: imdl = 0
      character (len=lfilm) :: zsuf
!
body: do
         select case (kunt)
         case (kdpd)
            idpd = idpd + 1
            inum = idpd
            znam = zbask
            zsuf = zsufk
         case (kpgm)
            ipgm = ipgm + 1
            inum = ipgm
            znam = zbasp
            zsuf = zsuff
         case (kmdl)
            imdl = imdl + 1
            inum = imdl
            znam = zbasm
            zsuf = zsuff
         case (kbdt)
            ibdt = ibdt + 1
            inum = ibdt
            znam = zbasb
            zsuf = zsuff
         case (ksub,kfun,kdup)
            idup = idup + 1
            inum = idup
            znam = zbasd
            zsuf = zsuff
         end select
         do
            if (inum > nnamm) then
               kerr = 1
               exit body
            endif
            write (znam (ifmts:ifmte), zfmtn) inum
            inquire (file=trim(znam)//trim(zsuf), exist=ifxst)
            if (ifxst) then
               inum = inum + 1
               cycle
            else
               exit
            endif
         enddo
         kerr = 0
         exit body
      enddo body
end subroutine nxtnam
subroutine raicas (zstr)
!  Raise a string to upper case
use splitprms
character (len=*), intent (inout) :: zstr  ! The string
logical :: toggle
character(len=1) :: togglechar
! ____________________________________________________________________
!
!   Modified to not do upper case to embedded strings (pgg 21.11.94)
      toggle = .TRUE.
      lstr = len_trim (zstr)
      do istr = 1, lstr
         if (toggle) then
            if(zstr (istr:istr) == '"' .or. zstr (istr:istr) == "'") then
               toggle = .not. toggle
               togglechar = zstr (istr:istr)
            end if
            irnk = index (zlwc, zstr (istr:istr))
            if (irnk > 0) then
               zstr (istr:istr) = zupc (irnk:irnk)
            endif
         else
            if(zstr (istr:istr) == togglechar) toggle = .not. toggle
         endif
      enddo
end subroutine raicas
subroutine lwrcas (zstr)
!  Lower a string to lower case
use splitprms
character (len=*), intent (inout) :: zstr  ! The string
! ____________________________________________________________________
!
      lstr = len_trim (zstr)
      do istr = 1, lstr
         irnk = index (zupc, zstr (istr:istr))
         if (irnk > 0) then
            zstr (istr:istr) = zlwc (irnk:irnk)
         endif
      enddo
end subroutine lwrcas
subroutine rmvlbl (zstt)
!  Remove statement label (Note: Label /= Construct name)
use splitprms
character (len=lsttm), intent (inout) :: zstt  ! The statement
! ____________________________________________________________________
!
      if (index (zdgt, zstt (1:1)) > 0) then
         istt = verify (zstt, zdgt//' ')
         zstt = zstt (istt:lsttm)
      endif
end subroutine rmvlbl
subroutine rmvtab (zstr, lstr)
!  Remove TABs and replace with spaces
use splitprms
character (len=lstr), intent (inout) :: zstr  ! The string
integer, intent (inout)              :: lstr  ! its trimmed length
! ____________________________________________________________________
!
      lsrc = lstr
      do
!
!  Search backwards so that trailing TABs eliminated first
!
         lsrc = index (zstr (1:lsrc), ztab, back=.true.)
         if (lsrc == 0) then
            exit
         endif
         zstr (lsrc:lsrc) = ' '
         lsrc = lsrc - 1
      enddo
      lstr = len_trim (zstr)
end subroutine rmvtab
subroutine chktab (zstr, lstr)
!  Verify and possibly update current TAB expansion plan
use splitdefs
use splitcurs
character (len=*), intent (inout) :: zstr  ! The string
integer, intent (inout)           :: lstr  ! its trimmed length
! ____________________________________________________________________
!
      lexp = lstr
!
!  Quick return when possible
!
body: do
      if (iplac == nplam) exit body
      if (index (zstr (1:lstr), ztab) == 0) exit body
      if (verify (zstr (1:lstr), ztab//' ') == 0) then
         lexp = 0
         lstr = 0
         exit body
      endif
!
!  Loop on expansion plans
!
      do
         istr = 1
         lexp = 0
         call expand
!
!  Check if line fits with current plan
!
         if (lexp > linem) then
            iplac = iplac + 1
            if (iplac < nplam) cycle
            lexp = lstr
         endif
         exit body
      enddo
      enddo body
      mlins = max (lexp, mlins)
contains
   subroutine expand
!
!  Expand each TAB on to next tab mark
!
      do
         if (lstr >= istr) then
            iwrk = index (zstr (istr:lstr), ztab)
         else
            exit
         endif
         if (iwrk /= 0) then
            lexp = lexp + iwrk - 1
            istr = istr + iwrk
!
!  Expand TAB on to next tab mark
!
            iexp = lexp + 1
            lfil = min (iexp, linem)
            lexp = nxttab (lfil, iplac)
!
!  Fill-up with spaces
!
         else
            exit
         endif
      enddo
      lexp = lexp + lstr - istr + 1
   end subroutine expand
end subroutine chktab
subroutine xpdtab (zstr, lstr)
!  Expand line using current TAB expansion plan
use splitdefs
use splitcurs
character (len=*), intent (inout) :: zstr  ! The line
integer, intent (inout)           :: lstr  ! its trimmed length
! ____________________________________________________________________
!
      character (len=linem) :: zlinw  ! work string
!
!  Quick return when possible
!
      if (iplac == nplam) then
         call rmvtab (zstr, lstr)
         return
      endif
      iwrk = index (zstr (1:lstr), ztab)
      if (iwrk == 0) return
      if (verify (zstr (1:lstr), ztab//' ') == 0) then
         lstr = 0
         return
      endif
!
      istr = 1
      lexp = 0
      zlinw = zstr
!
!  Removing TABs
!
      do
         if (iwrk /= 0) then
            lexp = lexp + iwrk - 1
            istr = istr + iwrk
!
!  Expand TAB on to next tab mark
!
            iexp = lexp + 1
            lfil = min (iexp, linem)
            lexp = nxttab (lfil, iplac)
!
!  Fill-up with spaces
!
            if (iexp <= lexp) then
               zlinw (iexp:lexp) = repeat (" ", lexp - iexp + 1)
            endif
            zlinw (lexp+1:linem) = zstr (istr:lstr)
            if (lstr >= istr) then
               iwrk = index (zstr (istr:lstr), ztab)
            else
               iwrk = 0
            endif
         else
            exit
         endif
      enddo
!
      lstr = len_trim (zlinw)
      zstr (1:lstr) = zlinw (1:lstr)
!
end subroutine xpdtab
