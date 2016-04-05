module faibidnt
!  Identity of f90aib utility
! ____________________________________________________________________
      character (len=*), parameter :: zsccs = &
"@(#)faibidnt.f90	1.3  00/12/15 Michel Olagnon"
      character (len=*), parameter :: zvers = &
"@(#) faibidnt.f90	V-0.3 00/12/15 Michel Olagnon"
      character (len=*), parameter :: zusg = &
"( usage: f90aib < file.f90  > file.f90 )"
      character (len=*), parameter :: zhlp  = '( &
&"Fortran 90 utility to process free source form code and"/&
&"automatically try to build interface blocks"/&
&"____________________________________________________________________"/&
&"Copyright (C) 1997-2000 M. Olagnon"/&
&"This program is free software; you can redistribute it and/or modify"/&
&"it under the terms of the GNU General Public License as published by"/&
&"the Free Software Foundation; either version 2 of the License, or"/&
&"(at your option) any later version."//&
&"This program is distributed in the hope that it will be useful,"/&
&"but WITHOUT ANY WARRANTY; without even the implied warranty of"/&
&"MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the"/&
&"GNU General Public License for more details."//&
&"You should have received a copy of the GNU General Public License"/&
&"along with this program; if not, write to the Free Software"/&
&"Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA."//&
&"Originally written by Michel Olagnon, from Ifremer, France,"/&
&"who would be pleased to receive your comments and corrections."/&
&" M. Olagnon (Michel.Olagnon@ifremer.fr)"/&
&"____________________________________________________________________"/&
&"                    version 0.3 of 15 Dec. 2000"/&
&"____________________________________________________________________"/&
&"Note: If you do not like code to start in column 7, remember that,"/&
&"      had Diophantes left a 6 characters margin, then mathematicians"/&
&"      might have spared much efforts on A**N = B**N + C**N ..."/&
&"      My margin is wide to let you put your corrections there."/&
&"____________________________________________________________________")'
!
end module faibidnt
module flexprms
!  Parameters for f90lex utility
! ____________________________________________________________________
      character  (len=1), parameter   :: ztab = achar(9)
      character  (len=1), parameter   :: zbks = achar(92)
!
!  A few source characteristics
!
      integer, parameter  :: lnamm = 31 ! max. variable name length
      integer, parameter  :: lfilm = 64 ! max. file name length
      integer, parameter  :: ncntm = 39 ! max. # cont. lines
      integer, parameter  :: linem = 132 ! max. line length
      integer, parameter  :: lsttm = (linem-1)*ncntm+linem
                             ! max. sttmt. length
!
!  Line codes
!
      integer, parameter  :: klunv = -1 ! Unavailable
      integer, parameter  :: klnrm =  0 ! Not continued, non-comment
      integer, parameter  :: kllst =  1 ! Last line
      integer, parameter  :: klctd =  2 ! Continued line
      integer, parameter  :: klcmt =  3 ! Comment line
      integer, parameter  :: klfcm =  4 ! False comment
      integer, parameter  :: kltcm =  5 ! Trailing comment line
!
!  Token codes
!
      integer, parameter  :: kkndf =  0 ! Undefined
      integer, parameter  :: kkcmt =  1 ! Comment string
      integer, parameter  :: kkebc =  2 ! Embedded comment in continued instr.
      integer, parameter  :: kkstr =  3 ! Character string
      integer, parameter  :: kkidf =  4 ! Identifier
      integer, parameter  :: kknui =  5 ! Integer Numerical value
      integer, parameter  :: kkknd =  6 ! _Kind (underscore)
      integer, parameter  :: kkdpt =  7 ! :
      integer, parameter  :: kkpvg =  8 ! ;
      integer, parameter  :: kkpou =  9 ! (
      integer, parameter  :: kkpfr = 10 ! )
      integer, parameter  :: kkslh = 11 ! /
      integer, parameter  :: kkcou = 12 ! (/
      integer, parameter  :: kkcfr = 13 ! /)
      integer, parameter  :: kkcmd = 14 ! $command (preprocessor)
      integer, parameter  :: kkqst = 15 ! ?
      integer, parameter  :: kkprc = 16 ! %
      integer, parameter  :: kkpms = 17 ! + or -
      integer, parameter  :: kkcct = 18 ! //
      integer, parameter  :: kkaff = 19 ! =
      integer, parameter  :: kkneq = 20 ! /=
      integer, parameter  :: kkleq = 21 ! <=
      integer, parameter  :: kkequ = 22 ! ==
      integer, parameter  :: kkgeq = 23 ! >=
      integer, parameter  :: kkpts = 24 ! =>
      integer, parameter  :: kksup = 25 ! >
      integer, parameter  :: kkinf = 26 ! <
      integer, parameter  :: kksta = 27 ! *
      integer, parameter  :: kkpow = 28 ! **
      integer, parameter  :: kkdot = 29 ! .
      integer, parameter  :: kksep = 30 ! ,
      integer, parameter  :: kklog = 31 ! .xxx.
      integer, parameter  :: kkeos = 32 ! End of Statement (no token)
      integer, parameter  :: kkamp = 33 ! & (not continuation)
      integer, parameter  :: kkfcm = 34 ! False comment (i.e. !$HPF)
      integer, parameter  :: kkbnm = 35 ! Block name
      integer, parameter  :: kkdcl = 36 ! ::
      integer, parameter  :: kklab = 37 ! label
      integer, parameter  :: kknuf = 38 ! Real Numerical value
      integer, parameter  :: kkpnb = 39 ! ( within defined type
      integer, parameter  :: kkukn = 40 ! Other
!   Macros
!ams  integer, parameter  :: nargm = 64 ! Max # of arguments
      integer, parameter  :: nargm = 128 ! Max # of arguments
      integer, parameter  :: kkar0 = 50 ! Base for macro arguments
      integer, parameter, dimension (0:nargm)  :: kkargt = &
          (/ (kkar0+i, i = 0, nargm) /) ! Macro arguments
!
end module flexprms
module flexvars
!  variables to hold token stream
use flexprms
! ____________________________________________________________________
      character (len=lsttm), save        :: ztoki  ! to hold identified
      character (len=linem), dimension (:), pointer, save ::&
                                            zbufc  ! comments buffer
      integer, dimension (1:lsttm), save :: kktokt ! codes
      integer, dimension (1:lsttm), save :: inamwt ! names
      integer, dimension (1:lsttm), save :: itokdt ! starting indexes
      integer, dimension (1:lsttm), save :: itokft ! termination indexes
      integer, parameter                 :: nrepm = lsttm
      integer, parameter                 :: nrepgm = 8*nrepm
      character (len=nrepgm), save       :: zrepg  ! to hold replacements
      integer, dimension (1:nrepm), save :: kkrept ! codes
      integer, dimension (1:nrepm), save :: irepwt ! names
      integer, dimension (1:nrepm), save :: irepdt ! starting indexes
      integer, dimension (1:nrepm), save :: irepft ! termination indexes
      integer, dimension (1:nrepm), save :: irepnt ! next in chain
      integer, save                      :: irepg = 0
      integer, save                      :: irep  = 0
contains
      subroutine inizbu (nsiz)
         integer, intent (in) :: nsiz
!
         allocate (zbufc (nsiz))
         return
      end subroutine inizbu
      subroutine xpdzbu (nsiz)
         integer, intent (inout) :: nsiz
         character (len=linem), dimension (:), allocatable :: zbufw
!
         allocate (zbufw (nsiz))
         zbufw (1:nsiz) = zbufc (1:nsiz)
         deallocate (zbufc)
         allocate (zbufc (2*nsiz))
         zbufc (1:nsiz) = zbufw (1:nsiz)
         deallocate (zbufw)
         nsiz = 2 * nsiz
         return
      end subroutine xpdzbu
end module flexvars
module fprsprms
!  Parsing parameters for f90ppr utility
use flexprms
! ____________________________________________________________________
!
!   Pre-processing commands
!
      character (len=*), parameter  :: zadol = "$"
      character (len=*), parameter  :: zadef = "$DEFINE"
      character (len=*), parameter  :: zaeli = "$ELIF"
      character (len=*), parameter  :: zaels = "$ELSE"
      character (len=*), parameter  :: zaend = "$ENDIF"
      character (len=*), parameter  :: zaevl = "$EVAL"
      character (len=*), parameter  :: zaifx = "$IF"
      character (len=*), parameter  :: zaifd = "$IFDEF"
      character (len=*), parameter  :: zaifn = "$IFNDEF"
      character (len=*), parameter  :: zainc = "$INCLUDE"
      character (len=*), parameter  :: zamcr = "$MACRO"
      character (len=*), parameter  :: zaund = "$UNDEF"
!
!  Codes for current statement parsing left context
!
      integer, parameter :: kcbeg =   0 ! begin statement, nothing known
      integer, parameter :: kcbex =   1 ! begin executable stt
      integer, parameter :: kcblb =   2 ! begin labelled stt
      integer, parameter :: kcbnb =   3 ! begin named block stt
      integer, parameter :: kccmd =   4 ! within fppr command
      integer, parameter :: kcntf =   5 ! within interface stt
      integer, parameter :: kcwtx =   6 ! within executable stt
      integer, parameter :: kcwtf =   7 ! within format stt
      integer, parameter :: kcwti =   8 ! within I/O stt
      integer, parameter :: kcdcl =   9 ! within declaration attributes
      integer, parameter :: kccas =  10 ! after CASE
      integer, parameter :: kcntt =  11 ! within INTENT
      integer, parameter :: kcipl =  12 ! after IMPLICIT
      integer, parameter :: kcuse =  13 ! after USE
      integer, parameter :: kcprc =  14 ! after ENTRY or FUNCTION
      integer, parameter :: kcall =  15 ! within allocation
      integer, parameter :: kcife =  16 ! after IF (, ELSEIF (
      integer, parameter :: kcass =  17 ! after ASSIGN
      integer, parameter :: kcbcl =  18 ! after DO
      integer, parameter :: kcdcp =  19 ! within proc. attributes
      integer, parameter :: kcukn =  39 ! nothing known, but not keyword
      integer, parameter :: kcany =  40 ! nothing known, may be keyword
!
!
!  Codes for current statement parsing right context
!
      integer, parameter :: krukn =   0 ! nothing known, but not keyword
      integer, parameter :: krlst =   1 ! last token
      integer, parameter :: krstr =   2 ! string
      integer, parameter :: krpou =   3 ! (
      integer, parameter :: krany =   4 ! nothing known, may be keyword
!
!  Type used for identifiers
!
      type namtyp
         integer  :: ihshf  ! points to next identifier in chain
         integer  :: irepc  ! points to replacement tokens chain
         integer  :: kwnam  ! is it a keyword, a common name, a variable
         integer  :: inamd  ! starting position in global chain
         integer  :: inamf  ! ending position in global chain
         integer  :: inamod ! starting position in output chain
         integer  :: inamof ! ending position in output chain
      end type namtyp
!
!  identifiers characteristics
!
      integer, parameter :: nnamm = 8192 ! max # of identifiers
      integer, parameter :: lnama = 6    ! average length of names
!
!  Possible types
!
      integer, parameter :: kwnul =  0 ! empty
!
!  Fortran 90 keywords
!
      integer, parameter :: kwcmd =   1 ! pre-processor command
      integer, parameter :: kwlop =   2 ! logical operator (> 3rd token )
      integer, parameter :: kwlct =   3 ! logical constant (> 3rd token )
      integer, parameter :: kwfmb =   4 ! format item (> format (       )
      integer, parameter :: kwiok =   5 ! I/O keywrd  (> read (         )
      integer, parameter :: kwatt =   6 ! type attribute   (< name      )
      integer, parameter :: kwaca =   7 ! allocation action (< (name)   )
      integer, parameter :: kwgnn =   8 ! generic name (> interface     )
      integer, parameter :: kwprc =   9 ! procedure
      integer, parameter :: kwctn =  10 ! CONTAINS
      integer, parameter :: kwdef =  11 ! DEFAULT
      integer, parameter :: kwnta =  12 ! Intent attribute
      integer, parameter :: kwass =  13 ! ASSIGN
      integer, parameter :: kwac2 =  14 ! action   (< [(]name[)]        )
      integer, parameter :: kwfmt =  15 ! FORMAT
      integer, parameter :: kwsts =  16 ! string spec. (< '...'         )
      integer, parameter :: kwtoa =  17 ! TO ( > assign)
      integer, parameter :: kwac3 =  18 ! action   (< name              )
      integer, parameter :: kwac4 =  19 ! action   ( alone              )
      integer, parameter :: kwsel =  20 ! SELECT CASE
      integer, parameter :: kwaio =  21 ! i/o action (< (iolist)        )
      integer, parameter :: kwac5 =  22 ! action (< [name]              )
      integer, parameter :: kwacd =  23 ! declaration action (COMMON, ..)
      integer, parameter :: kweli =  24 ! ELSEIF
      integer, parameter :: kwenp =  25 ! END procedure
      integer, parameter :: kwenf =  26 ! END INTERFACE
      integer, parameter :: kwent =  27 ! END TYPE
      integer, parameter :: kwfct =  28 ! FUNCTION
      integer, parameter :: kwhol =  29 ! H
      integer, parameter :: kwifp =  30 ! IF (
      integer, parameter :: kwipl =  31 ! IMPLICIT
      integer, parameter :: kwntt =  32 ! INTENT
      integer, parameter :: kwntf =  33 ! INTERFACE
      integer, parameter :: kwnon =  34 ! NONE
      integer, parameter :: kwac6 =  35 ! action (< (name)              )
      integer, parameter :: kwuse =  36 ! USE
      integer, parameter :: kwnly =  37 ! ONLY
      integer, parameter :: kwpps =  38 ! PRIVATE,PUBLIC,SEQUENCE
      integer, parameter :: kwrsl =  39 ! RESULT
      integer, parameter :: kwstt =  40 ! STAT
      integer, parameter :: kwthn =  41 ! THEN
      integer, parameter :: kwbcl =  42 ! DO
      integer, parameter :: kwwhl =  43 ! WHILE
      integer, parameter :: kwels =  44 ! ELSE
      integer, parameter :: kweni =  45 ! END IF
      integer, parameter :: kwend =  46 ! END DO
      integer, parameter :: kwens =  47 ! END SELECT
      integer, parameter :: kwenw =  48 ! END WHERE
      integer, parameter :: kwwhe =  49 ! WHERE
      integer, parameter :: kwelw =  50 ! ELSEWHERE
      integer, parameter :: kwcas =  51 ! CASE
      integer, parameter :: kwtyp =  52 ! TYPE
      integer, parameter :: kwfra =  53 ! FORALL
      integer, parameter :: kwena =  54 ! END FORALL
      integer, parameter :: kwgto =  55 ! GOTO
      integer, parameter :: kwpat =  56 ! proc attribute   (< name      )
      integer, parameter :: kwdta =  57 ! DATA
      integer, parameter :: kwsys = 255 ! last possible keyword
!
!  User-defined identifiers
!
      integer, parameter :: kwvar = 256 ! variable name
      integer, parameter :: kwntr = 257 ! known intrinsic
      integer, parameter :: kwlab = 258 ! label ( ^num              )
      integer, parameter :: kwblk = 259 ! block name ( < :          )
      integer, parameter :: kwdfd = 260 ! defined name
      integer, parameter :: kwext = 261 ! external name (> procedure)
      integer, parameter :: kwpdn = 262 ! pre-defined numerical
      integer, parameter :: kwpds = 263 ! pre-defined string
!
!   Macros
!
      integer, parameter :: kwmc0 = 280 ! macro name base
      integer, parameter, dimension (0:nargm)  :: kwmcrt = &
          (/ (kwmc0+i, i = 0, nargm) /) ! Macro with i arguments
!
!  Fortran 90 statement types
!
      integer, parameter :: ksukn =   0 ! nothing known
      integer, parameter :: ksprs =   1 ! procedure start
      integer, parameter :: ksprm =   2 ! procedure middle (contains)
      integer, parameter :: kspre =   3 ! procedure end
      integer, parameter :: ksifs =   4 ! start block if
      integer, parameter :: ksifm =   5 ! middle block if (else)
      integer, parameter :: ksife =   6 ! end block if
      integer, parameter :: kswhs =   7 ! start where
      integer, parameter :: kswhm =   8 ! middle where
      integer, parameter :: kswhe =   9 ! end where
      integer, parameter :: ksdos =  10 ! start do
      integer, parameter :: ksdoe =  11 ! end do
      integer, parameter :: ksnts =  12 ! start interface
      integer, parameter :: ksnte =  13 ! end interface
      integer, parameter :: kssls =  14 ! start select
      integer, parameter :: ksslm =  15 ! middle select (case)
      integer, parameter :: kssle =  16 ! end select
      integer, parameter :: kstys =  17 ! start type
      integer, parameter :: kstye =  18 ! end type
      integer, parameter :: ksifp =  19 ! possible if
      integer, parameter :: ksppr =  20 ! pre-processor command
      integer, parameter :: ksfrs =  21 ! start forall
      integer, parameter :: ksfre =  22 ! end forall
      integer, parameter :: ksuse =  23 ! use
      integer, parameter :: ksexe =  24 ! otherwise undefined executable
      integer, parameter :: ksdcl =  25 ! declarative statement
      integer, parameter :: ksany =  26 ! data or format, appear anywhere
      integer, parameter :: ksipl =  27 ! IMPLICIT declaration
!
end module fprsprms
module fprsvars
!  Parsing variables for f90ppr utility
use fprsprms
! ____________________________________________________________________
!
!   Pre-processing commands
!
      integer, save                 :: iwdol
      integer, save                 :: iwdef
      integer, save                 :: iweli
      integer, save                 :: iwels
      integer, save                 :: iwend
      integer, save                 :: iwevl
      integer, save                 :: iwifx
      integer, save                 :: iwifd
      integer, save                 :: iwifn
      integer, save                 :: iwinc
      integer, save                 :: iwmcr
      integer, save                 :: iwund
!
!  Global chain for identifiers
!
      character (len=nnamm*lnama), save :: znamg
      integer, save                     :: inamg = 0
!
!  Global chain for output names of identifiers
!
      character (len=nnamm*lnama*2), save :: znamo
      integer, save                       :: inamo = 0
!
!  Identifiers tables
!
      type (namtyp), dimension (1:nnamm), save :: tnamt = &
           (/ (namtyp (0, 0, 0, 0, 0, 0, 0), i = 1, nnamm) /)
      integer, save                            :: ialt = nnamm + 1
!
end module fprsvars
module fxprprms
!  Parameters for f90 expressions evaluation
use flexprms
! ____________________________________________________________________
      type opropd
         double precision dval
         integer koprd
         integer iprv
         integer inxt
      end type opropd
!
!  Operator & operand names
!
      character (len=*), parameter  :: zoand = ".AND."
      integer, save                 :: iwand
      character (len=*), parameter  :: zoequ = ".EQ."
      integer, save                 :: iwequ
      character (len=*), parameter  :: zogeq = ".GE."
      integer, save                 :: iwgeq
      character (len=*), parameter  :: zogth = ".GT."
      integer, save                 :: iwgth
      character (len=*), parameter  :: zoleq = ".LE."
      integer, save                 :: iwleq
      character (len=*), parameter  :: zolth = ".LT."
      integer, save                 :: iwlth
      character (len=*), parameter  :: zoneq = ".NE."
      integer, save                 :: iwneq
      character (len=*), parameter  :: zonot = ".NOT."
      integer, save                 :: iwnot
      character (len=*), parameter  :: zoori = ".OR."
      integer, save                 :: iwori
!
      character (len=*), parameter  :: zotru = ".TRUE."
      integer, save                 :: iwtru
      character (len=*), parameter  :: zofls = ".FALSE."
      integer, save                 :: iwfls
!
      character (len=*), parameter  :: zoint = "INT"
      integer, save                 :: iwint
      character (len=*), parameter  :: zonin = "NINT"
      integer, save                 :: iwnin
      character (len=*), parameter  :: zosin = "SIN"
      integer, save                 :: iwsin
      character (len=*), parameter  :: zocos = "COS"
      integer, save                 :: iwcos
      character (len=*), parameter  :: zotan = "TAN"
      integer, save                 :: iwtan
      character (len=*), parameter  :: zoatn = "ATAN"
      integer, save                 :: iwatn
      character (len=*), parameter  :: zolog = "LOG"
      integer, save                 :: iwlog
      character (len=*), parameter  :: zoexp = "EXP"
      integer, save                 :: iwexp
      character (len=*), parameter  :: zol10 = "LOG10"
      integer, save                 :: iwl10
      character (len=*), parameter  :: zosqr = "SQRT"
      integer, save                 :: iwsqr
      character (len=*), parameter  :: zomod = "MOD"
      integer, save                 :: iwmod
      character (len=*), parameter  :: zomax = "MAX"
      integer, save                 :: iwmax
      character (len=*), parameter  :: zomin = "MIN"
      integer, save                 :: iwmin
      character (len=*), parameter  :: zoat2 = "ATAN2"
      integer, save                 :: iwat2
      character (len=*), parameter  :: zoasn = "ASIN"
      integer, save                 :: iwasn
      character (len=*), parameter  :: zoacs = "ACOS"
      integer, save                 :: iwacs
      character (len=*), parameter  :: zosnh = "SINH"
      integer, save                 :: iwsnh
      character (len=*), parameter  :: zocsh = "COSH"
      integer, save                 :: iwcsh
      character (len=*), parameter  :: zotnh = "TANH"
      integer, save                 :: iwtnh
      character (len=*), parameter  :: zoabs = "ABS"
      integer, save                 :: iwabs
      character (len=*), parameter  :: zoknd = "KIND"
      integer, save                 :: iwknd
      character (len=*), parameter  :: zosik = "SELECTED_INT_KIND"
      integer, save                 :: iwsik
      character (len=*), parameter  :: zosrk = "SELECTED_REAL_KIND"
      integer, save                 :: iwsrk
!
!  pre-defined parameters
!
      character (len=*), parameter  :: zofcm = "FPPR_FALSE_CMT"
      integer, save                 :: iwfcm
      character (len=*), parameter  :: zocsk = "FPPR_KWD_CASE"
      integer, save                 :: iwcsk
      character (len=*), parameter  :: zocsu = "FPPR_USR_CASE"
      integer, save                 :: iwcsu
      character (len=*), parameter  :: zofxi = "FPPR_FXD_IN"
      integer, save                 :: iwfxi
      character (len=*), parameter  :: zofxo = "FPPR_FXD_OUT"
      integer, save                 :: iwfxo
      character (len=*), parameter  :: zosed = "FPPR_USE_SHARP"
      integer, save                 :: iwsed
      character (len=*), parameter  :: zomll = "FPPR_MAX_LINE"
      integer, save                 :: iwmll
      character (len=*), parameter  :: zoids = "FPPR_STP_INDENT"
      integer, save                 :: iwids
      character (len=*), parameter  :: zolnb = "FPPR_NMBR_LINES"
      integer, save                 :: iwlnb
!
      character (len=*), parameter  :: zalve = "FPPR_LEAVE"
      integer, save                 :: iwlve
      character (len=*), parameter  :: zalwr = "FPPR_LOWER"
      integer, save                 :: iwlwr
      character (len=*), parameter  :: zaupr = "FPPR_UPPER"
      integer, save                 :: iwupr
!
!  Operator & operand codes
!  ** Beware, operands must be in increasing priority order
!
      integer, parameter  :: kondf =  0 ! Undefined
      integer, parameter  :: konul = 13 ! Logical Numerical value
      integer, parameter  :: konui = 19 ! Integer Numerical value
      integer, parameter  :: konuf = 25 ! Real Numerical value
      integer, parameter  :: konot = 33 ! Not
      integer, parameter  :: koori = 34 ! Or
      integer, parameter  :: koand = 35 ! And
      integer, parameter  :: kogth = 36 ! >
      integer, parameter  :: kogeq = 37 ! >=
      integer, parameter  :: koequ = 38 ! ==
      integer, parameter  :: kolth = 39 ! <
      integer, parameter  :: koleq = 40 ! <=
      integer, parameter  :: koneq = 41 ! /=
      integer, parameter  :: komns = 42 ! -
      integer, parameter  :: kopls = 43 ! +
      integer, parameter  :: komlt = 44 ! *
      integer, parameter  :: kodiv = 45 ! /
      integer, parameter  :: kopow = 46 ! **
      integer, parameter  :: koint = 47 ! Int
      integer, parameter  :: konin = 48 ! Nint
      integer, parameter  :: kosin = 49 ! Sin
      integer, parameter  :: kocos = 50 ! Cos
      integer, parameter  :: kotan = 51 ! Tan
      integer, parameter  :: koatn = 52 ! Atan
      integer, parameter  :: kolog = 53 ! Log
      integer, parameter  :: koexp = 54 ! Exp
      integer, parameter  :: kol10 = 55 ! Log10
      integer, parameter  :: kosqr = 56 ! Sqrt
      integer, parameter  :: komod = 57 ! Mod
      integer, parameter  :: komax = 58 ! Max
      integer, parameter  :: komin = 59 ! Min
      integer, parameter  :: koat2 = 60 ! Atan2
      integer, parameter  :: koasn = 61 ! Asin
      integer, parameter  :: koacs = 62 ! Acos
      integer, parameter  :: kosnh = 63 ! Sinh
      integer, parameter  :: kocsh = 64 ! Cosh
      integer, parameter  :: kotnh = 65 ! Tanh
      integer, parameter  :: koabs = 66 ! Abs
      integer, parameter  :: koknd = 67 ! Kind
      integer, parameter  :: kosik = 68 ! Selected_Int_Kind
      integer, parameter  :: kosrk = 69 ! Selected_Real_Kind
      integer, parameter  :: kouds = 90 ! _   (of kind)
      integer, parameter  :: kosep = 91 ! ,
      integer, parameter  :: kopou = 92 ! (
      integer, parameter  :: kopfr = 93 ! )
      integer, parameter  :: komodi = 94 ! .Mod.
      integer, parameter  :: komaxi = 95 ! .Max.
      integer, parameter  :: komini = 96 ! .Min.
      integer, parameter  :: koat2i = 97 ! .Atan2.
      integer, parameter  :: kosrki = 98 ! .S_R_K.
      integer, parameter  :: koukn = 99 ! Other
!
end module fxprprms
module fxprvars
!  Variables for f90 expression analysis
use flexprms
use fxprprms
! ____________________________________________________________________
!
      integer, parameter                 :: nxptm = lsttm
      integer, parameter                 :: nxptgm = 8*nxptm
      type (opropd), dimension (1:nxptm), save :: oxptt ! oper[and|ator]
      integer, save                      :: ixpt  = 0
!
end module fxprvars
module fpprprms
!  Parameters for f90ppr utility
! ____________________________________________________________________
!
!  Case processing
!
      integer, parameter  :: kclwr = -1 ! case processing: to lower
      integer, parameter  :: kcupr =  1 ! case processing: to upper
      integer, parameter  :: kclve =  0 ! case processing: leave as is
      character (len=26), parameter :: zlwr="abcdefghijklmnopqrstuvwxyz"
      character (len=26), parameter :: zupr="ABCDEFGHIJKLMNOPQRSTUVWXYZ"
!
!  Logical units
!
      integer, parameter  :: luerr  = 0 ! logical unit for stderr
      integer, parameter  :: lufil  = 6 ! logical unit for final file
      integer, parameter  :: lustdi = 5 ! logical unit for stdin
      integer, parameter  :: lufic0 = 7 ! base logical unit for include
!
!  False comments
!
      integer, parameter  :: ncmtim = 16 ! max # of "False comments"
      integer, parameter  :: lcmtim =  8 ! max length of "False comments"
!
!  Defines
!
      integer, parameter  :: nnstdm = 64 ! max nesting for DEFINEs
!
!  Tests
!
      integer, parameter  :: nnsttm = 64 ! max nesting for IFs, IFDEFs
!
!  Include files
!
      integer, parameter  :: nnstim = 16 ! maximum nesting
      integer, parameter  :: lzficm = 96 ! maximum name length
!
!  Loop labels
!
      integer, parameter  :: nlabdm = 16 ! maximum nesting
      integer, parameter  :: llabdm =  5 ! maximum label length
!
end module fpprprms
module fpprcurs
use fpprprms
use flexprms
!  Current status variables in f90ppr utility
! ____________________________________________________________________
      character (len=1) :: zblk = '!' ! current blank line printing
      character (len=2*linem+1), save :: zlinb ! advance line in buffer
      character (len=2*linem+1), dimension (nnstim), save :: zlinbh
                                  ! advance lines heap for include
      integer, save   :: nhav = 0 ! How many lines do we have in advance
      integer, dimension (nnstim), save :: nhavh  ! and the heap
      integer, save   :: klrea = klunv   ! Type of current line
      integer, save   :: klnxt = klunv   ! Type of next line
      integer, dimension (nnstim), save :: klnxth ! and the heap
      integer, save     :: ifskp = 0  ! are we skipping code ?
      integer, dimension (0:nnstim), save  :: nlinit = &
            (/ (0, i = 0, nnstim) /)  ! number of lines input
      integer, save     :: linel = 72 ! current, desirable, linelength
      integer, save     :: nndt  = 0  ! current indentation
      integer, save     :: nndtp =  3 ! current step for indentation
      integer, save     :: lprc  =  0 ! current procedure nesting
      integer, save     :: luinp = lustdi ! current input unit
      integer, save     :: lufic = lufic0 ! current include unit
      integer, save     :: iclev = 0  ! current include level
      character (len=lzficm), dimension (0:nnstim), save  :: zficit = &
            (/ "standard input", &
              ("              ", i = 1, nnstim) /)
                                      ! table of names
!
!  Format
!
      integer, save     :: iffxd = 0  ! are we reading fixed format ?
      integer, save     :: iffxf = 0  ! are we outputing fixed format ?
      integer, save     :: iflnb = 0  ! are we numbering lines ?
!
!  Interpret # as $
!
      integer, save     :: ifsed = 0  ! is # same as $ ?
!
!  Case processing
!
      integer, save  :: kccask = kclve ! Case for keywords
      integer, save  :: kccasu = kclve ! Case for user identifiers
!
!  False comments
!
      integer, save  :: ncmti = 0 ! number of "False comments"
      character (len=lcmtim), dimension (ncmtim), save :: zcmtit
                                  ! the corresponding strings
!
!  Loop labels
!
      integer, save  :: nlabd = 0 ! number of labels
      integer, save  :: ndoe  = 0 ! number of loops ending on label
      character (len=llabdm), dimension (nlabdm), save :: zlabdt
                                  ! the corresponding strings
end module fpprcurs
module faibprms
!  Parameters for f90aib utility
! ____________________________________________________________________
!
      character (len=*), parameter   :: zsub = "Subroutine"
      character (len=*), parameter   :: zfun = "Function"
      character (len=*), parameter   :: zitf = "Interface"
      character (len=*), parameter   :: zeif = "End Interface"
      character (len=*), parameter   :: zend = "End"
      character (len=*), parameter   :: zdim = "Dimension"
      character (len=*), parameter   :: zlen = "Len"
      character (len=*), parameter   :: zxtn = "External"
      integer, parameter             :: nattm = 64 ! max # of attributes
!
end module faibprms
module faibvars
!  variables to hold interface
use faibprms
use flexprms
      character (len=lnamm), save        :: zprci  ! to hold proc. id
      character (len=2*nattm*lnamm*nargm), save :: zargi ! attr. tokens
      integer, dimension (1:nargm), save :: nattt  ! # of attr. tokens
      integer, dimension (1:nargm), save :: kargit ! arguments hashes
      integer, dimension (1:2*nattm,1:nargm), save :: kkattt ! attr. code
      integer, dimension (1:2*nattm,1:nargm), save :: iattdt ! attr. start
      integer, dimension (1:2*nattm,1:nargm), save :: iattft ! attr. end
      integer, dimension (1:2*nattm,1:nargm), save :: iattnt ! attr. hash
      integer, save                      :: nargi  ! # of args
      integer, save                      :: iargi  ! position in zargi
! ____________________________________________________________________
end module faibvars
module faibcurs
use faibprms
use fpprcurs
!  Current status variables in f90aib utility
! ____________________________________________________________________
      integer, save     :: ifexe = 0  ! are we in executable statements ?
      integer, save     :: lctn = -1  ! level of ``contained'' procs.
      integer, save     :: kfct = 0   ! type of current proc.
!
end module faibcurs
program f90aib
!  Process standard input, trying automatically to build interface
!  blocks for F90 source code, and output result on standard output.
! ____________________________________________________________________
      use faibidnt
      use fpprcurs
      use fxprprms
      interface
         subroutine aibtok (ztok, ltok, kktok)
!  add token to current stream, and reduce if end of statement
            use flexvars
            use faibcurs
            integer, intent (in)              :: ltok, kktok
            character (len=ltok), intent (in) :: ztok
         end subroutine aibtok
      end interface
! ____________________________________________________________________
!
!ams      write (luerr, "(a)") "This is f90aib: " // zvers
!ams      write (luerr, "(a)")  zusg
!
!  Initialize names, directives, expression evaluation
!
      call ininam
      call inicmd
      call inixpr
!
!  Loop on (possibly multiple instructions) input lines
!
      ifstp = 1 ! strip-out embedded comments
      ksta = 0
      do
         if (iffxd == 0) then
            call lexlin (aibtok, ifstp, ksta)
         else
            call lexfxd (aibtok, ifstp, ksta)
         endif
         if (ksta /= 0) exit
      enddo
!
end program f90aib
subroutine lexlin (trttok, ifstp, ksta)
!  Read input file, lexing free-form into token stream, until a
!  simultaneous end-of-line end-of-statement is found.
use flexprms
use fpprcurs
      interface
         subroutine trttok (ztok, ltok, kktok)
!  add token to current stream, and reduce if end of statement
            use flexvars
            use fpprcurs
            integer, intent (in)              :: ltok, kktok
            character (len=ltok), intent (in) :: ztok
         end subroutine trttok
      end interface
integer, intent (in)                :: ifstp ! strip-out comments ?
integer, intent (out)               :: ksta  ! status code
! ____________________________________________________________________
      character (len=2*linem) :: zlin
      character (len=lsttm) :: ztok
      character (len=1)     :: zdlm, zchr
!
      ksta  = 0
      ifcnt = 0
      ifchc = 0
      ntok  = 0
      kktok = kkndf
!
body: do
         do
            if (klrea == kllst .or. klrea == kltcm) then
               if (ifcnt /= 0) then
                  ksta = 2
                  call fpperr ("Unexpected end of input")
                  exit body
               else
                  if (iclev > 0) then
                     zlinb = zlinbh (iclev)
                     nhav  = nhavh  (iclev)
                     klnxt = klnxth (iclev)
                     iclev = iclev - 1
                     close (lufic)
                     lufic = lufic - 1
                     if (iclev == 0) then
                        luinp = lustdi
                     else
                        luinp = lufic
                     endif
                  else
                     ksta = -1
                     exit body
                  endif
               endif
            endif
!
!  Read a line
!
            call realin (luinp, zlin, klrea)
            if (klrea == klunv .and. iclev /= 0) then
               klrea = kllst
               cycle
            endif
            exit
         enddo
!
         select case (klrea)
         case (klunv)
            ksta = 1
            call fpperr ("Problem reading input")
            exit body
         case default
            ksta = 0
         end select
!
!  Recognize and skip full comments
!
         llin = len_trim (zlin)
         if (llin == 0) then
            call trttok ("!", 1, kkcmt)
            if (ifcnt == 0) then
               exit body
            else
               cycle body
            endif
         endif
         ilin = verify (zlin (1:llin), ztab // " ")
cmtl:    do
            if (ilin /= 0) then
               if (zlin (ilin:ilin) /= "!") then
                  exit cmtl
               endif
!
!  Do not skip "False comments"
!
               do icmti = 1, ncmti
                  lcmti = len_trim (zcmtit (icmti))
                  ilini = ilin + lcmti - 1
                  if (llin > ilini .and. &
                      zlin (ilin:ilini) == zcmtit (icmti)(1:lcmti)) then
                     call trttok (zlin (ilin:ilini), lcmti, kkfcm)
                     ilin = ilini + 1
                     exit cmtl
                  endif
               enddo
            endif
            if (ifcnt == 0) then
               call trttok (zlin, llin, kkcmt)
               exit body
            else
               call trttok (zlin, llin, kkebc)
               cycle body
            endif
         enddo cmtl
!
!  Check for continued mark
!
         if (ifcnt /= 0) then
            if (zlin (ilin:ilin) == "&") then
               ilin = ilin + 1
            else
               if (ifchc /= 0) then
                  ksta = 3
                  call fpperr ("Illegal continuation for string")
                  exit body
               else
                  if (kktok /= kkndf) then
                     call trttok (ztok, ltok, kktok)
                  endif
                  kktok = kkndf
               endif
            endif
         endif

         ifcnt = 0
         ichr = ilin - 1
!
!  Scan line
!
         do
               do
                  if (ichr >= llin) then
                     if (ifcnt == 0) then
                        if (kktok /= kkndf) then
                           call trttok (ztok, ltok, kktok)
                        endif
                        exit body
                     else
                        cycle body
                     endif
                  endif
                  ichr = ichr + 1
                  zchr = zlin (ichr:ichr)
                  if (ifchc == 0) then
                     select case (zchr)
!
!  Spaces
!
                     case (ztab,' ')
                        if (kktok /= kkndf) then
                           call trttok (ztok, ltok, kktok)
                        endif
                        kktok = kkndf
!
!  Letters
!
                     case ('A':'Z','a':'z')
                        if (kktok == kkidf .or. kktok == kkcmd) then
                           ltok = ltok + 1
                           ztok (ltok:ltok) = zchr
                        else
                           if (kktok /= kkndf) then
                              call trttok (ztok, ltok, kktok)
                           endif
                           ntok = ntok + 1
                           ltok = 1
                           ztok (ltok:ltok) = zchr
                           kktok = kkidf
                        endif
!
!  Digits
!
                     case ('0':'9')
                        if (kktok == kkidf .or. kktok == kknui) then
                           ltok = ltok + 1
                           ztok (ltok:ltok) = zchr
                        else
                           if (kktok /= kkndf) then
                              call trttok (ztok, ltok, kktok)
                           endif
                           ntok = ntok + 1
                           ltok = 1
                           ztok (ltok:ltok) = zchr
                           kktok = kknui
                        endif
!
!  Underscore (may be in identifier, or as a kind specifier)
!
                     case ('_')
                        select case (kktok)
                        case (kkidf)
                           ltok = ltok + 1
                           ztok (ltok:ltok) = zchr
                        case (kknui, kkstr)
                           call trttok (ztok, ltok, kktok)
                           ntok = ntok + 1
                           call trttok (zchr, 1, kkknd)
                           kktok = kkndf
                        case default
                           if (kktok /= kkndf) then
                              call trttok (ztok, ltok, kktok)
                           endif
                           ntok = ntok + 1
                           ltok = 1
                           ztok (ltok:ltok) = zchr
                           kktok = kkidf
                        end select
!
!  Colon
!
                     case (':')
                        if (kktok /= kkndf) then
                           call trttok (ztok, ltok, kktok)
                        endif
                        ntok = ntok + 1
                        call trttok (zchr, 1, kkdpt)
                        kktok = kkndf
!
!  Semi-colon
!
                     case (';')
                        if (kktok /= kkndf) then
                           call trttok (ztok, ltok, kktok)
                        endif
                        ntok = ntok + 1
                        call trttok (zchr, 1, kkpvg)
                        kktok = kkndf
!
!  Opening parenthesis
!
                     case ('(')
                        if (kktok /= kkndf) then
                           call trttok (ztok, ltok, kktok)
                        endif
                        ntok = ntok + 1
                        ltok = 1
                        ztok (ltok:ltok) = zchr
                        kktok = kkpou
!
!  Closing parenthesis
!
                     case (')')
                        if (kktok == kkslh) then
                           ltok = ltok + 1
                           ztok (ltok:ltok) = zchr
                           call trttok (ztok, ltok, kkcfr)
                           kktok = kkndf
                        else
                           if (kktok /= kkndf) then
                              call trttok (ztok, ltok, kktok)
                           endif
                           ntok = ntok + 1
                           call trttok (zchr, 1, kkpfr)
                           kktok = kkndf
                        endif
!
!  Exclamation mark (start of comment)
!
                     case ('!')
                        if (kktok /= kkndf .and. ifcnt == 0) then
                           call trttok (ztok, ltok, kktok)
                        endif
                        if (ifcnt == 0) then
                           ntok = ntok + 1
                           call trttok (zlin (ichr:llin), (llin-ichr+1),&
                                        kkcmt)
                           exit body
                        else
                           if (ifstp == 0) &
                           call wrtstt (zlin, 0, zlin, 0, &
                                        zlin (ichr:llin), &
                                        llin - ichr + 1, ichr-1)
                           cycle body
                        endif
!
!  Dollar (used as preprocessor command introduction)
!
                     case ('$')
                        if (kktok /= kkndf) then
                           call trttok (ztok, ltok, kktok)
                        endif
                        ntok = ntok + 1
                        ltok = 1
                        ztok (ltok:ltok) = '$'
                        kktok = kkcmd
!
!  Sharp (same as $ or !, depending on current status)
!
                     case ('#')
                        if (ifsed /= 0) then
                           if (kktok /= kkndf) then
                              call trttok (ztok, ltok, kktok)
                           endif
                           ntok = ntok + 1
                           ltok = 1
                           ztok (ltok:ltok) = '$'
                           kktok = kkcmd
                        else
                           if (kktok /= kkndf .and. ifcnt == 0) then
                              call trttok (ztok, ltok, kktok)
                           endif
                           if (ifcnt == 0) then
                              ntok = ntok + 1
                              call trttok (zlin (ichr:llin),&
                                           (llin-ichr+1), kkcmt)
                              exit body
                           else
                              if (ifstp == 0) &
                              call wrtstt (zlin, 0, zlin, 0, &
                                           zlin (ichr:llin), &
                                           llin - ichr + 1, ichr-1)
                              cycle body
                           endif
                        endif
!
!  Question mark
!
                     case ('?')
                        if (kktok /= kkndf) then
                           call trttok (ztok, ltok, kktok)
                        endif
                        ntok = ntok + 1
                        call trttok (zchr, 1, kkqst)
                        kktok = kkndf
!
!  Continuation mark
!
                     case ('&')
                        ifcnt = 1
                        if (ichr < llin) then
                           inxt = verify (zlin (ichr+1:llin),        &
                                          ztab // " ")
                           if (inxt /= 0) then
                              if (zlin (ichr+inxt:ichr+inxt) /= "!") then
                                 if (kktok /= kkndf) then
                                    call trttok (ztok, ltok, kktok)
                                 endif
                                 ntok = ntok + 1
                                 call trttok (zchr, 1, kkamp)
                                 kktok = kkndf
                                 ifcnt = 0
                              endif
                           endif
                        endif
!
!  Percent
!
                     case ('%')
                        if (kktok /= kkndf) then
                           call trttok (ztok, ltok, kktok)
                        endif
                        ntok = ntok + 1
                        call trttok (zchr, 1, kkprc)
                        kktok = kkndf
!
!  Plus and Minus
!
                     case ('+','-')
                        if (kktok /= kkndf) then
                           call trttok (ztok, ltok, kktok)
                        endif
                        ntok = ntok + 1
                        call trttok (zchr, 1, kkpms)
                        kktok = kkndf
!
!  Slash
!
                     case ('/')
                        select case (kktok)
                        case (kkslh)
                           ltok = ltok + 1
                           ztok (ltok:ltok) = zchr
                           call trttok (ztok, ltok, kkcct)
                           kktok = kkndf
                        case (kkpou)
                           ltok = ltok + 1
                           ztok (ltok:ltok) = zchr
                           call trttok (ztok, ltok, kkcou)
                           kktok = kkndf
                        case default
                           if (kktok /= kkndf) then
                              call trttok (ztok, ltok, kktok)
                           endif
                           ntok = ntok + 1
                           ltok = 1
                           ztok (ltok:ltok) = zchr
                           kktok = kkslh
                        end select
!
!  Star
!
                     case ('*')
                        if (kktok == kksta) then
                           ltok = ltok + 1
                           ztok (ltok:ltok) = zchr
                           call trttok (ztok, ltok, kkpow)
                           kktok = kkndf
                        else
                           if (kktok /= kkndf) then
                              call trttok (ztok, ltok, kktok)
                           endif
                           ntok = ntok + 1
                           ltok = 1
                           ztok (ltok:ltok) = zchr
                           kktok = kksta
                        endif
!
!  Superior
!
                     case ('>')
                        if (kktok == kkaff) then
                           ltok = ltok + 1
                           ztok (ltok:ltok) = zchr
                           call trttok (ztok, ltok, kkpts)
                           kktok = kkndf
                        else
                           if (kktok /= kkndf) then
                              call trttok (ztok, ltok, kktok)
                           endif
                           ntok = ntok + 1
                           ltok = 1
                           ztok (ltok:ltok) = zchr
                           kktok = kksup
                        endif
!
!  Inferior
!
                     case ('<')
                        if (kktok /= kkndf) then
                           call trttok (ztok, ltok, kktok)
                        endif
                        ntok = ntok + 1
                        ltok = 1
                        ztok (ltok:ltok) = zchr
                        kktok = kkinf
!
!  Equal
!
                     case ('=')
                        select case (kktok)
                        case (kkslh)
                           ltok = ltok + 1
                           ztok (ltok:ltok) = zchr
                           call trttok (ztok, ltok, kkneq)
                           kktok = kkndf
                        case (kkinf)
                           ltok = ltok + 1
                           ztok (ltok:ltok) = zchr
                           call trttok (ztok, ltok, kkleq)
                           kktok = kkndf
                        case (kkaff)
                           ltok = ltok + 1
                           ztok (ltok:ltok) = zchr
                           call trttok (ztok, ltok, kkequ)
                           kktok = kkndf
                        case (kksup)
                           ltok = ltok + 1
                           ztok (ltok:ltok) = zchr
                           call trttok (ztok, ltok, kkgeq)
                           kktok = kkndf
                        case default
                           if (kktok /= kkndf) then
                              call trttok (ztok, ltok, kktok)
                           endif
                           ntok = ntok + 1
                           ltok = 1
                           ztok (ltok:ltok) = zchr
                           kktok = kkaff
                        end select
!
!  Dot
!
                     case ('.')
                        if (kktok /= kkndf) then
                           call trttok (ztok, ltok, kktok)
                        endif
                        ntok = ntok + 1
                        call trttok (zchr, 1, kkdot)
                        kktok = kkndf
!
!  Separator
!
                     case (',')
                        if (kktok /= kkndf) then
                           call trttok (ztok, ltok, kktok)
                        endif
                        ntok = ntok + 1
                        call trttok (zchr, 1, kksep)
                        kktok = kkndf
!
!  String delimiter
!
                     case ('"',"'")
                        if (kktok == kkstr) then
                           if (zchr == zdlm) then
                              ltok = ltok + 1
                              ztok (ltok:ltok) = zchr
                           else
                              zdlm  = zchr
                              ltok  = 1
                              ztok (ltok:ltok) = zchr
                              ntok  = ntok + 1
                           endif
                           ifchc = 1
                        else
                           if (kktok /= kkndf) then
                              call trttok (ztok, ltok, kktok)
                           endif
                           if (ichr == llin) then
                              call fpperr ("Unmatched " // zchr)
                           else
                              zdlm  = zchr
                              ltok  = 1
                              ztok (ltok:ltok) = zchr
                              ntok  = ntok + 1
                              kktok = kkstr
                              ifchc = 1
                           endif
                        endif
!
!  Other character
!
                     case default
                        if (kktok /= kkndf) then
                           call trttok (ztok, ltok, kktok)
                        endif
                        ntok = ntok + 1
                        call trttok (zchr, 1, kkukn)
                        kktok = kkndf
                     end select
!
!  We are inside a char string
!
                  else
!
!  Test for end of current string
!
                     if (zchr == zdlm) then
                        ltok  = ltok + 1
                        ztok (ltok:ltok) = zchr
                        ifchc = 0
                     else
!
!  Test for end of line
!
                        if (ichr == llin) then
                           if (zchr == '&') then
                              ifcnt = 1
                              cycle body
                           else
                              call fpperr ("Unmatched " // zdlm)
                           endif
                        else
                           ltok = ltok + 1
                           ztok (ltok:ltok) = zchr
                        endif
                     endif
                  endif
               enddo
            enddo
      enddo body
      call trttok (ztok, 0, kkeos)
      return
end subroutine lexlin
subroutine aibtok (ztok, ltok, kktok)
!  add token to current stream, and reduce if end of statement
use flexvars
use fpprcurs
character (len=ltok), intent (in) :: ztok
integer, intent (in)              :: ltok, kktok
! ____________________________________________________________________
      integer, save  :: ntok = 0
      integer, save  :: itokf = 0
      integer, save  :: itokd
!
!  Skip if embedded comment
!
      if (kktok == kkebc) then
         return
      endif
!
!  Add to current stream
!
      if (ltok > 0) then
         ntok  = ntok + 1
         itokd = itokf + 1
         itokf = itokf + ltok
         kktokt (ntok) = kktok
         ztoki (itokd:itokf) = ztok (1:ltok)
         itokdt (ntok) = itokd
         itokft (ntok) = itokf
      endif
!
!  Reduce if end of statement
!
      if (kktok == kkpvg) then
         ntok = ntok - 1
      endif
      if ((kktok == kkpvg .or. kktok == kkeos) .and.  &
          (ntok > 0)                                ) then
         call trtstt (ntok, ksstt)
         ntok = 0
         itokf = 0
      endif
      return
end subroutine aibtok
subroutine rdcstt (ntok)
!  reduce lexed statement, to recognize constants, logical ops, ...
use flexvars
use fprsvars
use fpprprms
integer, intent (inout) :: ntok
! ____________________________________________________________________
!
      character (len=1) :: zchr
      interface
         logical function ifsame (zstr1, zstr2)
!           Case insensitive compare
         character (len=*), intent (in) :: zstr1, zstr2
         end function ifsame
      end interface
!
!  Note that we always skip the first token, since it may not be any
!  of the clusters considered. This trick solves the problem of
!  label E01 [ = ..., for instance] which is not a floating point
!  constant, and saves time...
!
!  Look for logical operators and variables .xxxx.
!
      if (index (ztoki (1:itokft(ntok)), '.') /= 0) then
         itok = 3
         do
            itok = itok + 1
            if (itok > ntok) exit
            if (kktokt (itok) == kkdot) then
               if (kktokt (itok-2) == kkdot .and. &
                   kktokt (itok-1) == kkidf .and. &
                   verify (ztoki (itokdt(itok-1):itokft(itok-1)), &
                           zlwr//zupr) == 0) then
                   kktokt (itok-2) = kkidf
                   itokft (itok-2) = itokft (itok)
                   itok2 = itok - 2
                   do itok1 = itok + 1, ntok
                      itok2 = itok2 + 1
                      kktokt (itok2) = kktokt (itok1)
                      itokdt (itok2) = itokdt (itok1)
                      itokft (itok2) = itokft (itok1)
                   enddo
                   ntok = itok2
               endif
            endif
         enddo
!
!  Look for floating point constants #.#
!
         itok = 3
         do
            itok = itok + 1
            if (itok > ntok) exit
            if (kktokt (itok) == kknui) then
               if (kktokt (itok-2) == kknui .and. &
                   kktokt (itok-1) == kkdot) then
                   itokft (itok-2) = itokft (itok)
                   itok2 = itok - 2
                   kktokt (itok2) = kknuf
                   do itok1 = itok + 1, ntok
                      itok2 = itok2 + 1
                      kktokt (itok2) = kktokt (itok1)
                      itokdt (itok2) = itokdt (itok1)
                      itokft (itok2) = itokft (itok1)
                   enddo
                   ntok = itok2
               endif
            endif
         enddo
!
!  Look for floating point constants #.
!
         itok = 2
         do
            itok = itok + 1
            if (itok > ntok) exit
            if (kktokt (itok) == kkdot) then
               if (kktokt (itok-1) == kknui) then
                   itokft (itok-1) = itokft (itok)
                   itok2 = itok - 1
                   kktokt (itok2) = kknuf
                   do itok1 = itok + 1, ntok
                      itok2 = itok2 + 1
                      kktokt (itok2) = kktokt (itok1)
                      itokdt (itok2) = itokdt (itok1)
                      itokft (itok2) = itokft (itok1)
                   enddo
                   ntok = itok2
               endif
            endif
         enddo
!
!  Look for floating point constants .#
!
         itok = 2
         do
            itok = itok + 1
            if (itok > ntok) exit
            if (kktokt (itok) == kknui) then
               if (kktokt (itok-1) == kkdot) then
                   itokft (itok-1) = itokft (itok)
                   kktokt (itok-1) = kknuf
                   itok2 = itok - 1
                   do itok1 = itok + 1, ntok
                      itok2 = itok2 + 1
                      kktokt (itok2) = kktokt (itok1)
                      itokdt (itok2) = itokdt (itok1)
                      itokft (itok2) = itokft (itok1)
                   enddo
                   ntok = itok2
               endif
            endif
         enddo
      endif
!
!  Look for exponent notation
!
      itok = 2
      do
         itok = itok + 1
         if (itok > ntok) exit
         if (kktokt (itok)   == kkidf        .and. &
             (kktokt (itok-1) == kknui) .or. &
             (kktokt (itok-1) == kknuf)           ) then
!
!  The following forms are possible: [EeDd][+-]#
!                                    [EeDd]#
!                                    [Ee]#_x
!                                    _x (comming from ._x)
!
            zchr = ztoki (itokdt(itok):itokdt(itok))
            if (index ("EeDd", zchr) /= 0) then
               itok1 = itok + 1
               if (itok1 < ntok) then
                  if (  itokdt(itok) == itokft(itok) .and. &
                      kktokt (itok1) == kkpms       ) then
                     itok1 = itok1 + 1
                     if (kktokt (itok1) == kknui) then
!  [EeDd][+-]#
                        itokft (itok-1) = itokft (itok1)
                        kktokt (itok-1) = kknuf
                        itok2 = itok-1
                        do itok1 = itok1 + 1, ntok
                           itok2 = itok2 + 1
                           kktokt (itok2) = kktokt (itok1)
                           itokdt (itok2) = itokdt (itok1)
                           itokft (itok2) = itokft (itok1)
                        enddo
                        ntok = itok2
                        cycle
                     endif
                  endif
               endif
               if (itok1 <= ntok) then
                  if (itokdt (itok)  == itokft (itok) .and. &
                      kktokt (itok1) == kknui         ) then
!  [EeDd]# (as 2 tokens)
                     itokft (itok-1) = itokft (itok1)
                     kktokt (itok-1) = kknuf
                     itok2 = itok-1
                     do itok1 = itok1 + 1, ntok
                        itok2 = itok2 + 1
                        kktokt (itok2) = kktokt (itok1)
                        itokdt (itok2) = itokdt (itok1)
                        itokft (itok2) = itokft (itok1)
                     enddo
                     ntok = itok2
                     cycle
                  endif
               endif
               if (itokdt (itok) < itokft (itok)) then
                     inumf = verify (ztoki(itokdt(itok)+1:itokft(itok)),&
                                     "0123456789")
                     if (inumf == 0) then
!  [EeDd]# (as a single identifier)
                        itokft (itok-1) = itokft (itok)
                        kktokt (itok-1) = kknuf
                        itok2 = itok-1
                        do itok1 = itok + 1, ntok
                           itok2 = itok2 + 1
                           kktokt (itok2) = kktokt (itok1)
                           itokdt (itok2) = itokdt (itok1)
                           itokft (itok2) = itokft (itok1)
                        enddo
                        ntok = itok2
                     else
                        inumf1 = itokdt(itok) + inumf
                        if (inumf1 > itokdt(itok) + 1 .and. &
                            ztoki (inumf1:inumf1) == '_' .and. &
                            inumf1 < itokft(itok) ) then
!  [EeDd]#_x (Dd are not standard)
                           itokft (itok-1) = inumf1 - 1
                           kktokt (itok-1) = kknuf
                           itok2 = ntok + 2
                           do itok1 = ntok, itok, -1
                              itok2 = itok2 - 1
                              kktokt (itok2) = kktokt (itok1)
                              itokdt (itok2) = itokdt (itok1)
                              itokft (itok2) = itokft (itok1)
                           enddo
                           itokdt (itok) = inumf1
                           itokft (itok) = inumf1
                           kktokt (itok) = kkknd
                           itok = itok + 1
                           itokdt (itok) = inumf1 + 1
                           inumf2 = verify ( &
                                     ztoki (itokdt(itok):itokft(itok)), &
                                     "0123456789")
                           if (inumf2 == 0) then
                              kktokt (itok) = kknui
                           else
                              kktokt (itok) = kkidf
                           endif
                           ntok = ntok + 1
                           itok = itok + 1
                        endif
                     endif
               endif
            elseif (zchr == '_' .and. &
                    itokdt (itok) < itokft (itok)) then
!  _x
               itok2 = ntok + 2
               do itok1 = ntok, itok, -1
                  itok2 = itok2 - 1
                  kktokt (itok2) = kktokt (itok1)
                  itokdt (itok2) = itokdt (itok1)
                  itokft (itok2) = itokft (itok1)
               enddo
               itokft (itok) = itokdt (itok)
               kktokt (itok) = kkknd
               itokdt (itok+1) = itokft (itok) + 1
               itok = itok + 1
               inumf2 = verify (ztoki (itokdt(itok):itokft(itok)), &
                                "0123456789")
               if (inumf2 == 0) then
                  kktokt (itok) = kknui
               else
                  kktokt (itok) = kkidf
               endif
               ntok = ntok + 1
               itok = itok + 1
            endif
         endif
      enddo
!
!  Remove embedded blanks in numerical constants
!
      itok = 2
      itok1 = 2
      do
         itok = itok + 1
         if (itok > ntok) exit
         if ((kktokt (itok) == kknui .and. kktokt (itok1) == kknui) .or.&
             (kktokt (itok) == kknuf .and. kktokt (itok1) == kknui) .or.&
             (kktokt (itok) == kknui .and. kktokt (itok1) == kknuf)) then
            itokft (itok1) = itokft (itok)
         else
            itok1 = itok1 + 1
            if (itok1 < itok) then
               kktokt (itok1) = kktokt (itok)
               itokdt (itok1) = itokdt (itok)
               itokft (itok1) = itokft (itok)
            endif
         endif
      enddo
      if (itok1 < ntok) ntok = itok1
!
!  find :: (must be outside of parentheses)
!
      itok = 2
      nparl = 0
      do
         itok = itok + 1
         if (itok > ntok) exit
         if (kktokt (itok) == kkpou) then
            nparl = nparl + 1
            cycle
         elseif (kktokt (itok) == kkpfr) then
            nparl = nparl - 1
            cycle
         elseif (nparl > 0) then
            cycle
         endif
         if (kktokt (itok) == kkdpt .and. &
             kktokt (itok-1) == kkdpt) then
            itokft (itok-1) = itokft (itok)
            kktokt (itok-1) = kkdcl
            do itok1 = itok + 1, ntok
               kktokt (itok1-1) = kktokt (itok1)
               itokdt (itok1-1) = itokdt (itok1)
               itokft (itok1-1) = itokft (itok1)
            enddo
            ntok = ntok - 1
            exit  ! (There should only be one :: per instruction)
         endif
      enddo
!
!  Now, look for double word keywords
!  (Here, we start again from the first token)
!
      itok  = 1
      itok1 = 1
expl: do
         itok = itok + 1
         if (itok > ntok) exit expl
         if (kktokt (itok)  == kkidf .and. &
             kktokt (itok1) == kkidf) then
            itokd = itokdt (itok1)
            itokf = itokft (itok)
            ihsh = khshstr (ztoki (itokd:itokf))
            if (tnamt(ihsh)%kwnam /= 0 .and. &
                tnamt(ihsh)%kwnam <= kwsys) then
               do
                  inamd = tnamt(ihsh)%inamd
                  inamf = tnamt(ihsh)%inamf
                  if (ifsame (ztoki (itokd:itokf), &
                              znamg (inamd:inamf))) then
                     itokft (itok1) = itokft (itok)
                     do itok2 = itok+1, ntok
                         kktokt (itok2-1) = kktokt (itok2)
                         itokdt (itok2-1) = itokdt (itok2)
                         itokft (itok2-1) = itokft (itok2)
                     enddo
                     itok = itok - 1
                     ntok = ntok - 1
                     cycle expl
                  endif
                  if (tnamt(ihsh)%ihshf /= 0) then
                     ihsh = tnamt(ihsh)%ihshf
                  else
                     exit
                  endif
               enddo
            endif
         endif
         itok1 = itok1 + 1
      enddo expl
!
!  Look for parentheses within defined types
!
         itok = 4
         do
            itok = itok + 1
            if (itok > ntok) exit
            if (kktokt (itok) == kkprc) then
               if (kktokt (itok-1) == kkpfr) then
                  npar = 1
                  do itok1 = itok - 3, 2, -1
                    if (kktokt (itok1) == kkpfr)  npar = npar + 1
                    if (kktokt (itok1) == kkpou .or. &
                        kktokt (itok1) == kkpnb) then
                       npar = npar - 1
                       if (npar == 0) then
                          kktokt (itok1) = kkpnb
                          exit
                       endif
                    endif
                  enddo
               endif
               if ((itok+2) < ntok) then
                  if (kktokt (itok+2) == kkpou) kktokt (itok+2) = kkpnb
               endif
               itok = itok + 1
            endif
         enddo
      return
end subroutine rdcstt
subroutine trtstt (ntok, ksstt)
!  Parse statement (partially)
use flexvars
use fprsvars
use faibcurs
use faibvars
integer, intent (inout) :: ntok
integer, intent (out)   :: ksstt
! ____________________________________________________________________
!
      integer, save   :: kctxc = kcbeg
!
      itoks = 1
      kctok = kctxc
      ksstt = ksukn
!
!  Apply pre-processor command
!
      if (kktokt (itoks) == kkcmd) then
         itokd = itokdt (itoks)
         itokf = itokft (itoks)
         call tstidf (ztoki (itokd:itokf), kcbeg, krukn, kwidf, &
                      inamwt (itoks))
         call trtcmd (inamwt (itoks), ntok)
         return
      endif
!
!  blank lines
!
      if (ntok == 0) then
         return
      elseif (ntok == 1 .and. kktokt (1) == kkcmt) then
         return
      elseif (ntok == 1 .and. kktokt (1) == kkebc) then
         return
      elseif (ntok == 1 .and. kktokt (1) == kkfcm) then
         return
      endif
!
!  Are-we still in executable code ?
!
      if (ifexe > 0) then
         kwnam = kwnul
         if (kktokt (itoks)  == kkidf) then
            itokd = itokdt (itoks)
            itokf = itokft (itoks)
            call tstidf (ztoki (itokd:itokf), kcany, krany, kwnam, &
                         inamwt (itoks))
            if (kwnam /= kwenp .and. &
                kwnam /= kwctn .and. &
                kwnam /= kwenf .and. &
                kwnam /= kwntf)  return
         else
            return
         endif
!
!        Reduce "token clusters"
!
         if (ntok > itoks) call rdcstt (ntok)
         if (kktokt (itoks)  == kkidf) then
            itokd = itokdt (itoks)
            itokf = itokft (itoks)
            ihsh = khshstr (ztoki (itokd:itokf))
            kwnam = tnamt(ihsh)%kwnam
            if (kwnam /= kwenp .and. &
                kwnam /= kwctn .and. &
                kwnam /= kwenf .and. &
                kwnam /= kwntf)  return
!
!   Look for end of procedure or contains
!
            if (kwnam == kwenp) then
                if (lctn == 0)   call outitf
                if (lctn >= 0 )  lctn = lctn - 1
                if (lctn < -1 )  lctn = lctn + 1
                ifexe = 0
            elseif (kwnam == kwctn) then
                ifexe = 0
            endif
         endif
!
!   Not in executable code
!
      elseif (ifexe == 0) then   !___________ ( if (ifexe == 0) )
!
!   Look for start/end of procedure, interface, type
!
         call rghprs (kswrk)
!
!   Choice according to level of containment
!
         select case (lctn)
         case (:-2)
!
            select case (kswrk)
            case (ksprs, kstys, ksnts)
               lctn = lctn - 1
            case (kspre, kstye, ksnte)
               lctn = lctn + 1
            case (ksprm)
               ifexe = 0
            case default
               continue
            end select
!
         case (-1)
!
            select case (kswrk)
            case (ksprs)
!
!   Start of a possible new interface
!
                  call subfct (ifnew)
                  if (ifnew == 0) then
                     lctn = lctn - 1
                  else
                     lctn = 0
                     kfct = ifnew
                     call newitf
                  endif
            case (kstys, ksnts)
               lctn = lctn - 1
            case (kspre, kstye, ksnte)
               lctn = lctn + 1
            case (ksprm)
               ifexe = 0
            case (ksexe)
               ifexe = 1
            case default
               continue
            end select
!
         case (0)
!
            select case (kswrk)
            case (ksprs)
               lctn = 1
            case (kstys)
!
!  One needs to keep the type, and to write it
!  when it is used to define an argument
!
               lctn = 1
            case (ksnts)
!
!  One needs to keep the interface, and to write it
!  when it is used to define an argument
!
               if (nargi > 0) then
                  ifexe = -1
                  call chkitf (ifarg)
                  if (ifarg /= 0) then
                     call bdyitf (ksnts)
                     lctn = -1
                  endif
               else
                  lctn = 1
               endif
            case (kstye, ksnte)
               lctn = -1
            case (kspre)
               call outitf
               lctn = -1
            case (ksprm)
               ifexe = 0
            case (ksexe)
               ifexe = 1
!
!   Look for argument info
!
            case (ksipl)
               if (nargi > 0) call bdyitf (kswrk)
            case (ksuse)
               if (nargi > 0) then
                  call chkuse (ifarg)
                  if (ifarg /= 0) call bdyitf (kswrk)
               endif
            case default
               if (nargi > 0) call chkdcl
            end select
!
!
         case (1:)
!
!   Look for level change
!
            select case (kswrk)
            case (ksprs, kstys, ksnts)
               lctn = lctn + 1
            case (kspre, kstye, ksnte)
               lctn = lctn - 1
            case (ksprm)
               ifexe = 0
!            case (ksexe)
!               ifexe = 1
            case default
               continue
            end select
         end select
      else                          !___________ ( if (ifexe = -1) )
!
!   Look for start/end of procedure, interface, type
!
         call rghprs (kswrk)
!
!   Choice according to level of containment
!
         select case (lctn)
         case (:-1)
!
            call bdyitf (kswrk)
            select case (kswrk)
            case (ksprs, ksnts)
               lctn = lctn - 1
            case (kspre)
               lctn = lctn + 1
               if (lctn == 0) call enditf
            case (ksnte)
               lctn = lctn + 1
               if (lctn == 0) ifexe = 0
            case default
               continue
            end select
!
         case (0)
!
            select case (kswrk)
            case (ksprs)
               call itfprc (ifarg)
               if (ifarg > 0) then
                  call heaitf
                  call bdyitf (ksprs)
                  lctn = -1
               else
                  lctn = +1
               endif
            case (ksnte)
               ifexe = 0
            case default
               continue
            end select

         case (1:)
!
            select case (kswrk)
            case (ksprs)
               lctn = lctn + 1
            case (kspre)
               lctn = lctn - 1
            case default
               continue
            end select
         end select
!
      endif
!
      return
contains
      subroutine rghprs (kode)
!
!   Rough parsing of current token stream
!   Recognize: - start/end proc
!              - use
!              - declarative instruction
!              - start/end interface
!              - start/end type
!              - contains
!              - executable instruction
! ___________________________________________________________________
!
!   Look for start of procedure
!
      itok = 1
      if (ntok > itok) call rdcstt (ntok)
!
!   Label ?
!
      if (kktokt (itok) == kknui) then
         kctok = kcblb
         itok = itok + 1
      endif
!
!   Block name ?
!
      if (itok + 1 < ntok) then
         if (kktokt (itok+1) == kkdpt .and. &
             kktokt (itok)   == kkidf) then
            kode = ksexe
            return
         endif
      endif
!
!  Identify first keyword
!
      kode = ksukn
      if (kktokt (itok) == kkidf) then
            itokd = itokdt (itok)
            itokf = itokft (itok)
            krtok = krukn
            if (itok == ntok) then
               krtok = krlst
            else
               if (kktokt (itok+1) == kkpou .or. &
                   kktokt (itok+1) == kkpnb)  then
                  krtok = krpou
               endif
            endif
            call tstidf (ztoki (itokd:itokf), kctok, krtok, kwidf, &
                         inamwt (itok))
            select case (kwidf)
            case (kwpat, kwprc, kwfct)
                  kode = ksprs
                  return
            case (kwenp)
                  kode = kspre
                  return
            case (kwuse)
                  kode = ksuse
                  return
            case (kwipl)
                  kode = ksipl
                  return
            case (kwatt)
                  kctok = kcdcl
            case (kwntf)
                  kode = ksnts
                  return
            case (kwenf)
                  kode = ksnte
                  return
            case (kwtyp)
                  if (kktokt (itok+1) == kkpou .or. &
                      kktokt (itok+1) == kkpnb)  then
                     kctok = kcdcl
                  else
                     kode = kstys
                     return
                  endif
            case (kwent)
                  kode = kstye
                  return
            case (kwctn)
                  kode = ksprm
                  return
            case (kwfmt, kwdta, kwpps, kwac5)
                  kode = ksany
                  return
            case default
                  kode = ksexe
                  return
            end select
      else
            kode = ksexe
            return
      endif
!
!
      lpar = 0
      do itok = itok+1, ntok
         if (kktokt (itok) == kkidf .and. lpar == 0) then
            itokd = itokdt (itok)
            itokf = itokft (itok)
            krtok = krukn
            if (itok == ntok) then
               krtok = krlst
            else
               if (kktokt (itok+1) == kkpou .or. &
                   kktokt (itok+1) == kkpnb)  then
                  krtok = krpou
               endif
            endif
            call tstidf (ztoki (itokd:itokf), kctok, krtok, kwidf, &
                         inamwt (itok))
            select case (kwidf)
            case (kwatt)
                  kctok = kcdcl
            case (kwpat)
                  kctok = kcdcp
            case (kwprc, kwfct)
                  kode = ksprs
                  return
            case default
                  kode = ksdcl
                  return
            end select
         elseif (kktokt (itok) == kkpou .or. &
                 kktokt (itok) == kkpnb) then
            lpar = lpar + 1
         elseif (kktokt (itok) == kkpfr) then
            lpar = lpar - 1
         elseif (kktokt (itok) == kkdcl) then
            kode = ksdcl
            return
         elseif (kktokt (itok) == kksep .and. lpar == 0) then
            kode = ksdcl
            return
         endif
      enddo
      end subroutine rghprs
      subroutine subfct (ifprc)
      integer, intent (out) :: ifprc
!  Find out if start of procedure is subroutine or function or else
! ___________________________________________________________________
!
      interface ifsame
         logical function ifsame (zstr1, zstr2)
!        Case insensitive compare
         character (len=*), intent (in) :: zstr1, zstr2
         end function ifsame
      end interface
      itok = 1
      ifprc = 0
      kctok = kctxc
find: do
         if (itok >= ntok) exit
         if (kktokt (itok) == kkidf) then
            itokd = itokdt (itok)
            itokf = itokft (itok)
            ihsh  = khshstr (ztoki (itokd:itokf))
            if (tnamt(ihsh)%kwnam == 0) then
                ifprc = 0
                exit find
            else
               do
                  inamd = tnamt(ihsh)%inamd
                  inamf = tnamt(ihsh)%inamf
                  if (ifsame (ztoki (itokd:itokf), znamg (inamd:inamf)))&
                      exit
                  if (tnamt(ihsh)%ihshf == 0) then
                     ifprc = 0
                     exit find
                  else
                     ihsh = tnamt(ihsh)%ihshf
                  endif
               enddo
               if (tnamt(ihsh)%kwnam == kwprc) then
                  if     (ifsame (ztoki (itokd:itokf), zsub)) then
                     ifprc = 2
                  else
                     ifprc = 0
                  endif
                  exit find
               elseif (tnamt(ihsh)%kwnam == kwfct) then
                  ifprc = 1
                  exit find
               else
                  itok = itok + 1
                  if    (tnamt(ihsh)%kwnam == kwtyp &
                    .or. tnamt(ihsh)%kwnam == kwatt) then
                     if (kktokt (itok) == kksta) then
                        itok = itok + 1
                        if (itok >= ntok) then
                           ifprc = 0
                           exit find
                        endif
                        if (kktokt (itok) == kknui) then
                           itok = itok + 1
                           cycle find
                        endif
                     endif
                     lpar = 0
                     do
                        if (kktokt (itok) == kkpou) then
                           lpar = lpar + 1
                        elseif (kktokt (itok) == kkpfr) then
                           lpar = lpar - 1
                        else
                           if (lpar == 0) cycle find
                        endif
                        itok = itok + 1
                        if (lpar == 0) cycle find
                        if (itok >= ntok) then
                           ifprc = 0
                           exit find
                        endif
                     enddo
                  endif
                  cycle find
               endif
            endif
         else
            ifprc = 0
            exit find
         endif
      enddo find
!
     if (ifprc /= 1 .and. ifprc /= 2) return
     call tstidf (ztoki (itokd:itokf), kcany, krany, kwwrk, inamwt (itok))
         itok = itok + 1
         if (itok > ntok) then
            ifprc = 0
            return
         endif
         if (kktokt (itok) /= kkidf) then
            ifprc = 0
            return
         endif
         itokd = itokdt (itok)
         itokf = itokft (itok)
         zprci = ztoki (itokd:itokf)
         call tstidf (zprci, kcany, krany, kwwrk, inamwt (itok))
         if (ifprc == 1) then
           nargi = 1
           call iniarg (nargi, itok)
         else
           nargi = 0
         endif
!
!   Look for argument list
!
         itok = itok + 1
         if (itok > ntok) return
         if (kktokt (itok) /= kkpou) then
            ifprc = 0
            return
         endif
         do
            itok = itok + 1
            if (itok > ntok) then
               ifprc = 0
               return
            endif
            if (kktokt (itok) == kkpfr) exit
            if (kktokt (itok) == kkidf) then
               itokd = itokdt (itok)
               itokf = itokft (itok)
               nargi = nargi + 1
               if (nargi > nargm) then
                  call fpperr ("Too many arguments")
                  call fpperr ("raise nargm and try again")
               endif
               call tstidf (ztoki (itokd:itokf), kcany, krany, kwwrk, &
                            inamwt (itok))
               call iniarg (nargi, itok)
            elseif (kktokt (itok) /= kksta) then ! possible * arg.
               ifprc = 0
               return
            endif
            itok = itok + 1
            if (itok > ntok) then
               ifprc = 0
               return
            endif
            if (kktokt (itok) == kkpfr) exit
            if (kktokt (itok) /= kksep) then
               ifprc = 0
               return
            endif
         enddo
       if (ifprc == 1 .and. itok < ntok) then
         itok = itok + 1
         if (kktokt (itok) == kkidf) then        ! result clause
               itokd = itokdt (itok)
               itokf = itokft (itok)
               call tstidf (ztoki (itokd:itokf), kcany, krany, kwwrk, &
                            inamwt (itok))
         else
               ifprc = 0
               return
         endif
         itok = itok + 1
         if (itok > ntok) then
            ifprc = 0
            return
         endif
         if (kktokt (itok) /= kkpou) then
            ifprc = 0
            return
         endif
         itok = itok + 1
         if (kktokt (itok) == kkidf) then        ! result name
               itokd = itokdt (itok)
               itokf = itokft (itok)
               call tstidf (ztoki (itokd:itokf), kcany, krany, kwwrk, &
                            inamwt (itok))
               nargi = nargi + 1
               if (nargi > nargm) then
                  call fpperr ("Too many arguments")
                  call fpperr ("raise nargm and try again")
               endif
               call iniarg (nargi, itok)
         else
               ifprc = 0
               return
         endif
         itok = itok + 1
         if (itok > ntok) then
            ifprc = 0
            return
         endif
         if (kktokt (itok) /= kkpfr) then
            ifprc = 0
            return
         endif
       endif
!
      end subroutine subfct
      subroutine chkitf (ifarg)
      integer, intent (out) :: ifarg
!  Find out if procedure in interface statement is argument
! ___________________________________________________________________
!
      ifarg = 0
!
      itok = 2
      if (itok > ntok) return
      if (kktokt (itok) == kkidf) then
            itokd = itokdt (itok)
            itokf = itokft (itok)
            ihsh  = indhsh (ztoki (itokd:itokf))
            do iarg = 1, nargi
               if (ihsh == kargit (iarg)) then
                  ifarg = iarg
                  exit
               endif
            enddo
      endif
!
      return
!
      end subroutine chkitf
      subroutine chkuse (ifarg)
      integer, intent (out) :: ifarg
!  Find out if module with ONLY clause refers to argument
! ___________________________________________________________________
!
!      ifarg = 0
!
!      itok = 3
!      if (itok > ntok) then
         ifarg = 1
         return
!      end if
!      used: do
!         itok = itok + 1
!         if (itok > ntok) exit
!         if (kktokt (itok) == kkidf) then
!            itokd = itokdt (itok)
!            itokf = itokft (itok)
!            ihsh  = indhsh (ztoki (itokd:itokf))
!            do iarg = 1, nargi
!               if (ihsh == kargit (iarg)) then
!                  ifarg = iarg
!                  exit used
!               endif
!            enddo
!         endif
!      enddo used
!
!      return
!
      end subroutine chkuse
      subroutine itfprc (ifarg)
      integer, intent (out) :: ifarg
!  Find out if procedure in interface is argument
! ___________________________________________________________________
!
      interface
         logical function ifsame (zstr1, zstr2)
!        Case insensitive compare
         character (len=*), intent (in) :: zstr1, zstr2
         end function ifsame
      end interface
!
      ifarg = 0
!
      itok = 1
      ifprc = 0
      kctok = kctxc
find: do
         if (itok >= ntok) exit
         if (kktokt (itok) == kkidf) then
            itokd = itokdt (itok)
            itokf = itokft (itok)
            ihsh  = khshstr (ztoki (itokd:itokf))
            if (tnamt(ihsh)%kwnam == 0) then
                ifprc = 0
                exit find
            else
               do
                  inamd = tnamt(ihsh)%inamd
                  inamf = tnamt(ihsh)%inamf
                  if (ifsame (ztoki (itokd:itokf), znamg (inamd:inamf)))&
                      exit
                  if (tnamt(ihsh)%ihshf == 0) then
                     ifprc = 0
                     exit find
                  else
                     ihsh = tnamt(ihsh)%ihshf
                  endif
               enddo
               if (tnamt(ihsh)%kwnam == kwprc) then
                  if     (ifsame (ztoki (itokd:itokf), zsub)) then
                     ifprc = 2
                  else
                     ifprc = 0
                  endif
                  exit find
               elseif (tnamt(ihsh)%kwnam == kwfct) then
                  ifprc = 1
                  exit find
               else
                  itok = itok + 1
                  if (tnamt(ihsh)%kwnam == kwtyp) then
                     lpar = 0
                     do
                        if (kktokt (itok) == kkpou) then
                           lpar = lpar + 1
                        elseif (kktokt (itok) == kkpfr) then
                           lpar = lpar - 1
                        endif
                        itok = itok + 1
                        if (lpar == 0) cycle find
                        if (itok >= ntok) then
                           ifprc = 0
                           exit find
                        endif
                     enddo
                  endif
                  cycle find
               endif
            endif
         else
            ifprc = 0
            exit find
         endif
      enddo find
!
     if (ifprc /= 1 .and. ifprc /= 2) return
         itok = itok + 1
         if (itok > ntok) then
            ifprc = 0
            return
         endif
         if (kktokt (itok) /= kkidf) then
            ifprc = 0
            return
         endif
         itokd = itokdt (itok)
         itokf = itokft (itok)
         ihsh  = indhsh (ztoki (itokd:itokf))
         do iarg = 1, nargi
            if (ihsh == kargit (iarg)) then
               ifarg = iarg
               exit
            endif
         enddo
!
      end subroutine itfprc
      subroutine iniarg (iarg, itok)
!  Initialise a new argument from list
! ___________________________________________________________________
!
      integer, intent (in) :: iarg, itok
!
      nattt (iarg) = 0
      kargit (iarg) = inamwt (itok)
      iattnt (:,iarg) = 0
      end subroutine iniarg
      subroutine newitf
!  Output start of interface block
! ___________________________________________________________________
!
      call heaitf
      call outstt (ntok, ksprs)
!
      iargi = 0
!
      end subroutine newitf
      subroutine heaitf
!  Output "interface"
! ___________________________________________________________________
!
      character (len=len(zitf)) :: zwrk
!
      zwrk = zitf
      if (kccask /= kclve) then
          call chgcas (zwrk, kccask)
      endif
!
      call wrtstt (zwrk, 0, zwrk, len_trim (zwrk), zwrk, 0, nndt)
      lprc = lprc + 1
      nndt = nndt + nndtp
!
      end subroutine heaitf
      subroutine enditf
!  Output "end interface"
! ___________________________________________________________________
!
      character (len=len(zeif)) :: zwrk
!
      zwrk = zeif
      if (kccask /= kclve) then
          call chgcas (zwrk, kccask)
      endif
!
      nndt = nndt - nndtp
      call wrtstt (zwrk, 0, zwrk, len_trim (zwrk), zwrk, 0, nndt)
      lprc = lprc - 1
!
      end subroutine enditf
      subroutine outitf
!  Output end of interface block
! ___________________________________________________________________
!
      integer, dimension (1:nargm) :: iargit ! arguments order
      interface
subroutine outtks (kktknt, itkndt, itknft, itknnt, ztkni, ksstt)
!  build and output statement
use flexvars
use fprsvars
use fpprcurs
character (len=*), intent (in)       :: ztkni
integer, intent (in), dimension (:)  :: kktknt, itkndt, itknft, itknnt
integer, intent (in)                 :: ksstt
end subroutine outtks
      end interface
      interface ifsame
         logical function ifsame (zstr1, zstr2)
!        Case insensitive compare
         character (len=*), intent (in) :: zstr1, zstr2
         end function ifsame
      end interface
!
!  Find out a no-backstep order
!
      iargit (1:nargi) = (/ (i, i=1,nargi) /)
      iargw = 1
      iswp  = 0
args: do
        if (iargw > nargi) exit
        iarg = iargit (iargw)
        natt = nattt (iarg)
        do iatt = 1, natt
           if (kkattt (iatt, iarg) == kkidf) then
               iattn = iattnt (iatt, iarg)
               kwnam = tnamt(iattn)%kwnam
!               if (kwnam > kwsys) then
                  do iarg1 = iargw + 1, nargi
                     if (iattn == kargit (iargit (iarg1)) .and. &
                         iswp /= iarg1) then
                        iargit (iargw) = iargit (iarg1)
                        iargit (iarg1) = iarg
                        iswp = iarg1
                        cycle args
                     endif
                  enddo
!               endif
           endif
        enddo
        iargw = iargw + 1
        iswp  = 0
      enddo args
!
!  Output arguments declarations
!
      do iargw = 1, nargi
        iarg = iargit (iargw)
        natt = nattt (iarg)
        if (natt > 0) then
!
!  Add ":: name" to id. attribute list
!
           if (natt /= 1 .or. &
               (.not.ifsame (zargi (iattdt(1,iarg):iattft(1,iarg)), zxtn))) then
              if (zargi(iargi:iargi) /= ',') then
                natt = natt + 1
                iargi = iargi + 1
                iattdt (natt, iarg) = iargi
              endif
              iargf = iargi + 1
              iattft (natt, iarg) = iargf
              kkattt (natt, iarg) = kkdcl
              zargi (iargi:iargf) = "::"
              iargi = iargf
           endif
           natt = natt + 1
           inamd = tnamt(kargit(iarg))%inamod
           inamf = tnamt(kargit(iarg))%inamof
           iargi = iargi + 1
           iattdt (natt, iarg) = iargi
           iargf = iargi + inamf - inamd
           iattft (natt, iarg) = iargf
           kkattt (natt, iarg) = kkidf
           zargi (iargi:iargf) = znamo (inamd:inamf)
           iattnt (natt, iarg) = kargit(iarg)
           iargi = iargf
           call outtks (kkattt (1:natt, iarg), iattdt (1:natt, iarg), &
                        iattft (1:natt, iarg), iattnt (1:natt, iarg), &
                        zargi, ksdcl)
        endif
      enddo
!
!  Output end procedure / end interface
!
      do itok = 1, ntok
         itokd = itokdt (itok)
         itokf = itokft (itok)
         call tstidf (ztoki (itokd:itokf), kcany, krany, kwwrk, &
                      inamwt (itok))
      enddo
      if (kwwrk > kwsys) then
         ifnmd = 1
      else
         ifnmd = 0
      endif
      if (ntok == 1 .and.         &
          (itokft(1)-itokdt(1)) == (len_trim (zend) - 1)) then
         itokd = itokdt (ntok)
         itokf = itokd + len_trim (zend) - 1
         itokft (ntok) = itokf
         ztoki (itokd:itokf) = zend
         kktokt (ntok) = kkidf
         call tstidf (zend, kcany, krany, kwwrk, inamwt (ntok))
         ntok = 2
         itokd = itokf + 1
         itokdt (ntok) = itokd
         kktokt (ntok) = kkidf
         if (kfct == 1) then
           itokf = itokd + len_trim (zfun) - 1
           itokft (ntok) = itokf
           ztoki (itokd:itokf) = zfun
           call tstidf (zfun, kcany, krany, kwwrk, inamwt (ntok))
         else ! if (kfct == 2) then
           itokf = itokd + len_trim (zsub) - 1
           itokft (ntok) = itokf
           ztoki (itokd:itokf) = zsub
           call tstidf (zsub, kcany, krany, kwwrk, inamwt (ntok))
         endif
      endif
!
!  Put procedure name if it is not already there
!
      if (ifnmd == 0) then
         ntok = ntok + 1
         itokd = itokf + 1
         itokf = itokd + len_trim (zprci) - 1
         itokdt (ntok) = itokd
         itokft (ntok) = itokf
         kktokt (ntok) = kkidf
         ztoki (itokd:itokf) = zprci
         call tstidf (zprci, kcany, krany, kwwrk, inamwt (ntok))
      endif
      call outstt (ntok, kspre)
      call enditf
      end subroutine outitf
      subroutine bdyitf (ksstti)
!  Output current line in interface block
! ___________________________________________________________________
!
      integer, intent (in) :: ksstti
!
      do itok = 1, ntok
         if (kktokt (itok) == kkidf) then
            itokd = itokdt (itok)
            itokf = itokft (itok)
            call tstidf (ztoki (itokd:itokf), kcany, krany, kwwrk, &
                         inamwt (itok))
         endif
      enddo
      call outstt (ntok, ksstti)
      end subroutine bdyitf
      subroutine chkdcl
!  Check for arguments declarations
! ___________________________________________________________________
!
!
      integer, dimension (1:2*nattm) :: jwrk1, jwrk2 ! work arrays
      natt  = 0
      ifidf = 0
      ntoka = 0
      itok  = 0
      do 
         itok = itok + 1
         if (itok > ntok) exit
!
!
!
         if (kktokt (itok) == kkidf) then
            itokd = itokdt (itok)
            itokf = itokft (itok)
            inamw = indhsh (ztoki (itokd:itokf))
            if (inamw == 0) then
               if (natt > 0) then
                  ifidf = 1
                  cycle
               else
                  return
               endif
            endif
            kwwrk = tnamt(inamw)%kwnam
            select case (kwwrk)
            case (kwatt, kwntt, kwtyp)
               natt = natt + 1
               ntoka = itok
               cycle
            case default ! kwvar, kwext only should be allowed, but keywords
                         ! can be used ...
               ifidf = 1
               do iarg = 1, nargi
                  if (inamw == kargit (iarg)) then
                     natt = nattt (iarg)
                     iargi0 = iargi
                     natt0 = natt
                     if (natt /= 0) then
                        natt = natt + 1
                        iargi = iargi + 1
                        iattdt (natt, iarg) = iargi
                        iattft (natt, iarg) = iargi
                        kkattt (natt, iarg) = kksep
                        zargi (iargi:iargi) = ","
                     endif
                     do iatt = 1, ntoka
                        natt = natt + 1
                        iargi = iargi + 1
                        iattdt (natt, iarg) = iargi
                        if (kktokt (iatt) /= kkidf) then
                           itokd = itokdt (iatt)
                           itokf = itokft (iatt)
                           iargf = iargi + itokf - itokd
                           iattft (natt, iarg) = iargf
                           kkattt (natt, iarg) = kktokt (iatt)
                           zargi (iargi:iargf) = ztoki (itokd:itokf)
                           iargi = iargf
                        else
                           itokd = itokdt (iatt)
                           itokf = itokft (iatt)
                           call tstidf (ztoki (itokd:itokf),  kcany, krany, &
                                        kwwrk, inamw)
                           if (inamw == indhsh (zdim) .and. ntoka == 1) then
                               iargi = iargi0
                               natt = natt0
                           elseif (inamw > 0) then
                             if (tnamt(inamw)%kwnam <= kwsys) then
                                inamd = tnamt(inamw)%inamod
                                inamf = tnamt(inamw)%inamof
                                iargf = iargi + inamf - inamd
                                iattft (natt, iarg) = iargf
                                kkattt (natt, iarg) = kktokt (iatt)
                                zargi (iargi:iargf) = znamo (inamd:inamf)
                             else
                                iargf = iargi + itokf - itokd
                                iattft (natt, iarg) = iargf
                                kkattt (natt, iarg) = kktokt (iatt)
                                zargi (iargi:iargf) = ztoki (itokd:itokf)
                             endif
                             iargi = iargf
                           else
                              iargf = iargi + itokf - itokd
                              iattft (natt, iarg) = iargf
                              kkattt (natt, iarg) = kktokt (iatt)
                              zargi (iargi:iargf) = ztoki (itokd:itokf)
                             iargi = iargf
                           endif
                           iattnt (natt, iarg) = inamw
                        endif
                     enddo
                     if (itok < ntok - 2) then
                        if (kktokt (itok+1) == kkpou) then
!
!  Add ", dimension (xxxx)" to id. attribute list
!
                           natt = natt + 1
                           iargi = iargi + 1
                           iattdt (natt, iarg) = iargi
                           iattft (natt, iarg) = iargi
                           kkattt (natt, iarg) = kksep
                           zargi (iargi:iargi) = ","
                           natt = natt + 1
                           iargi = iargi + 1
                           iattdt (natt, iarg) = iargi
                           iargf = iargi + len_trim (zdim) - 1
                           iattft (natt, iarg) = iargf
                           kkattt (natt, iarg) = kkidf
                           zargi (iargi:iargf) = zdim
                           iattnt (natt, iarg) = indhsh (zdim)
                           iargi = iargf
                           lpar = 0
                           do itok = itok+1, ntok
                              natt = natt + 1
                              iargi = iargi + 1
                              iattdt (natt, iarg) = iargi
                              itokd = itokdt (itok)
                              itokf = itokft (itok)
                              iargf = iargi + itokf - itokd
                              iattft (natt, iarg) = iargf
                              kkattt (natt, iarg) = kktokt (itok)
                              zargi (iargi:iargf) = ztoki (itokd:itokf)
                              iargi = iargf
                              if (kktokt (itok) == kkidf) then
                                 call tstidf (ztoki (itokd:itokf), &
                                              kcany, krany, kwwrk, &
                                              iattnt (natt, iarg))
                              elseif (kktokt (itok) == kkpou) then
                                 lpar = lpar + 1
                              elseif (kktokt (itok) == kkpfr) then
                                 lpar = lpar - 1
                                 if (lpar == 0) exit
                              endif
                           enddo
                        elseif (kktokt (itok+1) == kksta) then
!
!  add "(len=xxxx)" to the "character" definition
!
                           natt = natt + 1
                           nattw = natt
                           iargi = iargi + 1
                           iattdt (natt, iarg) = iargi
                           iattft (natt, iarg) = iargi
                           if (nattw > 2 .and.                &
                               kkattt (2, iarg) == kkpou) then
                              kkattt (nattw, iarg) = kksep
                              zargi (iargi:iargi) = ","
                              ifchq = 1
                           else
                              kkattt (natt, iarg) = kkpou
                              zargi (iargi:iargi) = "("
                              ifchq = 0
                           endif
                           natt = natt + 1
                           iargi = iargi + 1
                           iattdt (natt, iarg) = iargi
                           iargf = iargi + len_trim (zlen) - 1
                           iattft (natt, iarg) = iargf
                           kkattt (natt, iarg) = kkidf
                           zargi (iargi:iargf) = zlen
                           iattnt (natt, iarg) = indhsh (zlen)
                           natt = natt + 1
                           iargi = iargf + 1
                           iattdt (natt, iarg) = iargi
                           iattft (natt, iarg) = iargi
                           kkattt (natt, iarg) = kkaff
                           zargi (iargi:iargi) = "="
                           ifudl = 0 
                           if (ntok > itok+3) then
                              if (kktokt (itok+2) == kkpou .and. &
                                  kktokt (itok+3) == kksta .and. &
                                  kktokt (itok+4) == kkpfr) ifudl = 1
                           endif
                           if (ifudl == 0) then
                            lpar = 0
                            do itok = itok+2, ntok
                              natt = natt + 1
                              iargi = iargi + 1
                              iattdt (natt, iarg) = iargi
                              itokd = itokdt (itok)
                              itokf = itokft (itok)
                              iargf = iargi + itokf - itokd
                              iattft (natt, iarg) = iargf
                              kkattt (natt, iarg) = kktokt (itok)
                              zargi (iargi:iargf) = ztoki (itokd:itokf)
                              iattnt (natt, iarg) = indhsh (ztoki (itokd:itokf))
                              iargi = iargf
                              if (kktokt (itok) == kkpou) then
                                 lpar = lpar + 1
                              elseif (kktokt (itok) == kkpfr) then
                                 lpar = lpar - 1
                              endif
                              if (lpar == 0) exit
                            enddo
                           else
!
!  remove brackets in (*)
!
                            natt = natt + 1
                            iargi = iargi + 1
                            iattdt (natt, iarg) = iargi
                            iattft (natt, iarg) = iargi
                            kkattt (natt, iarg) = kksta
                            zargi (iargi:iargi) = "*"
                           endif
                           if (ifchq == 0) then
                              natt = natt + 1
                              iargi = iargi + 1
                              iattdt (natt, iarg) = iargi
                              iattft (natt, iarg) = iargi
                              kkattt (natt, iarg) = kkpfr
                              zargi (iargi:iargi) = ")"
                           endif
!
!  move it to the right place
!
                           if (nattw > 2) then
                              if (ifchq == 0) then
                                 iattw = 2
                              else
                                 lpar = 0
                                 do iattw = 2, nattw
                                    if (kkattt (iattw, iarg) == kkpou) then
                                       lpar = lpar + 1
                                     elseif (kkattt (iattw, iarg) == kkpfr) then
                                       lpar = lpar - 1
                                     endif
                                     if (lpar == 0) exit
                                 enddo
                              endif
                              natts = natt-nattw
                              jwrk1 (1:natts+1) = kkattt (nattw:natt, iarg)
                              jwrk2 (1:nattw-iattw) = kkattt (iattw:nattw-1, iarg)
                              kkattt (iattw+natts+1:natt, iarg) = jwrk2 (1:nattw-iattw)
                              kkattt (iattw:iattw+natts, iarg) = jwrk1 (1:natts+1)
                              jwrk1 (1:natts+1) = iattdt (nattw:natt, iarg)
                              jwrk2 (1:nattw-iattw) = iattdt (iattw:nattw-1, iarg)
                              iattdt (iattw+natts+1:natt, iarg) = jwrk2 (1:nattw-iattw)
                              iattdt (iattw:iattw+natts, iarg) = jwrk1 (1:natts+1)
                              jwrk1 (1:natts+1) = iattft (nattw:natt, iarg)
                              jwrk2 (1:nattw-iattw) = iattft (iattw:nattw-1, iarg)
                              iattft (iattw+natts+1:natt, iarg) = jwrk2 (1:nattw-iattw)
                              iattft (iattw:iattw+natts, iarg) = jwrk1 (1:natts+1)
                              jwrk1 (1:natts+1) = iattnt (nattw:natt, iarg)
                              jwrk2 (1:nattw-iattw) = iattnt (iattw:nattw-1, iarg)
                              iattnt (iattw+natts+1:natt, iarg) = jwrk2 (1:nattw-iattw)
                              iattnt (iattw:iattw+natts, iarg) = jwrk1 (1:natts+1)
                           endif
                        endif
                     endif
                     nattt (iarg) = natt
                     exit
                  endif
               enddo
            end select
         elseif (kktokt (itok) == kkpou) then
!
!  Add (xxxx) to attribute list
!
            lpar = 0
            do itok = itok, ntok
                 if (kktokt (itok) == kkpou) then
                    lpar = lpar + 1
                 elseif (kktokt (itok) == kkpfr) then
                    lpar = lpar - 1
                    if (lpar == 0) then
                       if (ifidf == 0) ntoka = itok
                       exit
                    endif
                 endif
            enddo
         elseif (kktokt (itok) == kkdcl) then
            ifidf = 1
         endif
      enddo
      end subroutine chkdcl
end subroutine trtstt
integer function khshstr (zstr)
!  A hash function for use in f90ppr
character (len=*), intent (in) :: zstr
! ____________________________________________________________________
      character (len=26), parameter :: zlwc="abcdefghijklmnopqrstuvwxyz"
      character (len=26), parameter :: zupc="ABCDEFGHIJKLMNOPQRSTUVWXYZ"
!
      jhsh = 0
      do istr = 1, len (zstr)
         irnk = index (zlwc, zstr (istr:istr))
         if (irnk > 0) then
            ichr = iachar (zupc (irnk:irnk))
         else
            ichr = iachar (zstr (istr:istr))
         endif
         jhsh = jhsh*17 + ichr - 45
         jhsh = modulo (jhsh, 4091)
      enddo
      khshstr = jhsh + 1
      return
end function khshstr
subroutine ininam
!  Initialise the names for f90ppr
use fprsvars
! _____________________________________________________________
!
      znamg (     1:  1230) = "&
&$$DEFINE$ELIF$ELSE$ENDIF$EVAL$IF$IFDEF$IFNDEF$INCLUDE$MACRO$UNDEF.AND..EQ..EQV..FALSE..GE..GT..LE..LT..NE..NEQV..NOT..OR..TRUE.ABS&
&ACOSASINATANATAN2ACCESSACTIONADVANCEALLOCATABLEALLOCATEASSIGNASSIGNMENTBACKSPACEBLANKBLOCKDATACALLCASECHARACTERCLOSECOMMONCOMPLEXC&
&ONTAINSCONTINUECOSCOSHCYCLEDATADEALLOCATEDEFAULTDELIMDIMENSIONDIRECTDODOUBLEPRECISIONELEMENTALELSEELSEIFELSEWHEREENDENDBLOCKDATAEN&
&DDOENDFILEENDFORALLENDFUNCTIONENDIFENDINTERFACEENDMODULEENDPROGRAMENDSELECTENDSUBROUTINEENDTYPEENDWHEREENTRYEOREQUIVALENCEERREXIST&
&EXITEXPEXTERNALFILEFMTFORALLFORMFORMATFORMATTEDFUNCTIONGOTOHIFIMPLICITININCLUDEINOUTINQUIREINTINTEGERINTENTINTERFACEINTRINSICIOLEN&
&GTHIOSTATKINDLENLOGLOG10LOGICALMAXMINMODMODULEMODULEPROCEDURENAMENAMEDNAMELISTNEXTRECNINTNMLNONENULLIFYNUMBEROONLYOPENOPENEDOPERAT&
&OROPTIONALOUTPADPARAMETERPAUSEPOINTERPOSITIONPRINTPRIVATEPROCEDUREPROGRAMPUBLICPUREREADREADWRITEREALRECRECLRECURSIVERESULTRETURNRE&
&WINDSAVESINSINHSELECTCASESELECTED_INT_KINDSELECTED_REAL_KINDSEQUENCESEQUENTIALSIZESQRTSTATSTATUSSTOPSUBROUTINETANTANHTARGETTHENTOT&
&YPETYPEUNFORMATTEDUNITUSEWHEREWHILEWRITEFPPR_LEAVEFPPR_LOWERFPPR_UPPERFPPR_FALSE_CMTFPPR_KWD_CASEFPPR_USR_CASEFPPR_FXD_INFPPR_FXD_&
&OUTFPPR_USE_SHARPFPPR_MAX_LINEFPPR_STP_INDENTFPPR_NMBR_LINES"
      inamg =   1230
!
      znamo (     1:  1249) = "&
&$$Define$ElIf$Else$EndIf$Eval$If$IfDef$IfnDef$Include$Macro$UnDef.And..Eq..Eqv..False..Ge..Gt..Le..Lt..Ne..Neqv..Not..Or..True.Abs&
&AcosAsinAtanAtan2AccessActionAdvanceAllocatableAllocateAssignAssignmentBackspaceBlankBlock DataCallCaseCharacterCloseCommonComplex&
&ContainsContinueCosCoshCycleDataDeallocateDefaultDelimDimensionDirectDoDouble PrecisionElementalElseElse IfElsewhereEndEnd Block D&
&ataEnd DoEndfileEnd ForallEnd FunctionEnd IfEnd InterfaceEnd ModuleEnd ProgramEnd SelectEnd SubroutineEnd TypeEnd WhereEntryEorEqu&
&ivalenceErrExistExitExpExternalFileFmtForallFormFormatFormattedFunctionGo ToHIfImplicitInIncludeInoutInquireIntIntegerIntentInterf&
&aceIntrinsicIoLengthIoStatKindLenLogLog10LogicalMaxMinModModuleModule ProcedureNameNamedNamelistNextRecNintNmlNoneNullifyNumberOOn&
&lyOpenOpenedOperatorOptionalOutPadParameterPausePointerPositionPrintPrivateProcedureProgramPublicPureReadReadWriteRealRecReclRecur&
&siveResultReturnRewindSaveSinSinhSelect CaseSelected_Int_KindSelected_Real_KindSequenceSequentialSizeSqrtStatStatusStopSubroutineT&
&anTanhTargetThenToTypeTypeUnformattedUnitUseWhereWhileWriteFPPR_leaveFPPR_lowerFPPR_upperFPPR_false_cmtFPPR_kwd_caseFPPR_usr_caseF&
&PPR_fxd_inFPPR_fxd_outFPPR_use_sharpFPPR_max_lineFPPR_stp_indentFPPR_nmbr_lines"
      inamo =   1249
!
      tnamt (    5) = namtyp (    0,    0,   42,  1071,  1075,  1090,  1094)
      tnamt (   28) = namtyp (    0,    0,   29,   580,   580,   597,   597)
      tnamt (   35) = namtyp (    0,    0,   16,   760,   760,   778,   778)
      tnamt (   48) = namtyp (    0,    0,    6,   637,   645,   654,   662)
      tnamt (   74) = namtyp (    0,    0,   56,   860,   863,   878,   881)
      tnamt (   92) = namtyp (    0,    0,   50,   365,   373,   368,   376)
      tnamt (  164) = namtyp (    0,    0,  263,  1162,  1173,  1181,  1192)
      tnamt (  179) = namtyp (    0,    0,   21,  1076,  1080,  1095,  1099)
      tnamt (  190) = namtyp (    0,    0,    2,    71,    74,    71,    74)
      tnamt (  228) = namtyp (    0,    0,   14,   202,   210,   202,   210)
      tnamt (  274) = namtyp (    0,    0,    5,   818,   825,   836,   843)
      tnamt (  276) = namtyp (    0,    0,    1,    25,    29,    25,    29)
      tnamt (  292) = namtyp (    0,    0,   14,   909,   914,   927,   932)
      tnamt (  325) = namtyp (    0,    0,   20,   926,   935,   944,   954)
      tnamt (  393) = namtyp ( 8191,    0,   52,  1040,  1043,  1059,  1062)
      tnamt (  426) = namtyp (    0,    0,   42,   329,   330,   330,   331)
      tnamt (  433) = namtyp (    0,    0,    5,   716,   720,   734,   738)
      tnamt (  454) = namtyp (    0,    0,   39,   897,   902,   915,   920)
      tnamt (  458) = namtyp (    0,    0,   22,   826,   830,   844,   848)
      tnamt (  467) = namtyp (    0,    0,    6,   797,   805,   815,   823)
      tnamt (  495) = namtyp (    0,    0,    1,    46,    53,    46,    53)
      tnamt (  502) = namtyp (    0,    0,   30,   581,   582,   598,   599)
      tnamt (  510) = namtyp (    0,    0,   12,   591,   592,   608,   609)
      tnamt (  511) = namtyp (    0,    0,  257,   612,   614,   629,   631)
      tnamt (  557) = namtyp (    0,    0,    5,   536,   539,   552,   555)
      tnamt (  561) = namtyp (    0,    0,    5,   646,   653,   663,   670)
      tnamt (  564) = namtyp (    0,    0,    2,    87,    90,    87,    90)
      tnamt (  568) = namtyp (    0,    0,    5,   769,   774,   787,   792)
      tnamt (  577) = namtyp (    0,    0,   22,   593,   599,   610,   616)
      tnamt (  597) = namtyp (    0,    0,    5,   979,   988,   998,  1007)
      tnamt (  609) = namtyp (    0,    0,    6,   660,   663,   677,   680)
      tnamt (  651) = namtyp (    0,    0,  263,  1216,  1230,  1235,  1249)
      tnamt (  698) = namtyp (    0,    0,   17,  1038,  1039,  1057,  1058)
      tnamt (  719) = namtyp (    0,    0,   34,   743,   746,   761,   764)
      tnamt (  721) = namtyp (    0,    0,   44,   355,   358,   357,   360)
      tnamt (  743) = namtyp (    0,    0,    6,   314,   322,   315,   323)
      tnamt (  747) = namtyp (    0,    0,    5,   712,   715,   730,   733)
      tnamt (  751) = namtyp (    0,    0,   25,   466,   478,   479,   492)
      tnamt (  758) = namtyp (    0,    0,    6,   233,   241,   234,   242)
      tnamt (  767) = namtyp (    0,    0,    6,   811,   817,   829,   835)
      tnamt (  797) = namtyp (    0,    0,    8,   775,   782,   793,   800)
      tnamt (  819) = namtyp (    0,    0,    2,    91,    94,    91,    94)
      tnamt (  822) = namtyp (    0,    0,    1,    39,    45,    39,    45)
      tnamt (  824) = namtyp (    0,    0,   57,   288,   291,   289,   292)
      tnamt (  834) = namtyp (    0,    0,    3,    80,    86,    80,    86)
      tnamt (  852) = namtyp (    0,    0,  262,  1091,  1100,  1110,  1119)
      tnamt (  895) = namtyp (    0,    0,   21,   864,   867,   882,   885)
      tnamt (  903) = namtyp (    0,    0,    6,   877,   880,   895,   898)
      tnamt (  919) = namtyp (    0,    0,   38,   915,   918,   933,   936)
      tnamt (  924) = namtyp (    0,    0,    1,     2,     8,     2,     8)
      tnamt (  937) = namtyp (    0,    0,    5,   884,   887,   902,   905)
      tnamt (  948) = namtyp (    0,    0,    5,   868,   876,   886,   894)
      tnamt (  966) = namtyp (    0,    0,   25,   410,   420,   417,   428)
      tnamt ( 1050) = namtyp (    0,    0,    6,   528,   535,   544,   551)
      tnamt ( 1128) = namtyp ( 8190,    0,    5,   559,   567,   575,   583)
      tnamt ( 1219) = namtyp (    0,    0,    6,   664,   666,   681,   683)
      tnamt ( 1238) = namtyp (    0,    0,   37,   761,   764,   779,   782)
      tnamt ( 1240) = namtyp (    0,    0,    8,   192,   201,   192,   201)
      tnamt ( 1293) = namtyp (    0,    0,   32,   622,   627,   639,   644)
      tnamt ( 1317) = namtyp (    0,    0,    9,   847,   853,   865,   871)
      tnamt ( 1326) = namtyp (    0,    0,   19,   268,   275,   269,   276)
      tnamt ( 1333) = namtyp (    0,    0,    2,   107,   112,   107,   112)
      tnamt ( 1372) = namtyp (    0,    0,   22,   903,   908,   921,   926)
      tnamt ( 1382) = namtyp ( 8192,    0,   10,   260,   267,   261,   268)
      tnamt ( 1385) = namtyp (    0,    0,   46,   389,   393,   394,   399)
      tnamt ( 1389) = namtyp (    0,    0,    5,   160,   166,   160,   166)
      tnamt ( 1399) = namtyp (    0,    0,  257,   993,   996,  1012,  1015)
      tnamt ( 1413) = namtyp (    0,    0,   35,   747,   753,   765,   771)
      tnamt ( 1426) = namtyp (    0,    0,   47,   457,   465,   469,   478)
      tnamt ( 1450) = namtyp (    0,    0,  257,   682,   684,   699,   701)
      tnamt ( 1461) = namtyp (    0,    0,   45,   421,   425,   429,   434)
      tnamt ( 1496) = namtyp (    0,    0,    6,  1028,  1033,  1047,  1052)
      tnamt ( 1543) = namtyp (    0,    0,    1,    60,    65,    60,    65)
      tnamt ( 1566) = namtyp (    0,    0,    9,   216,   224,   216,   225)
      tnamt ( 1576) = namtyp (    0,    0,  257,   685,   687,   702,   704)
      tnamt ( 1607) = namtyp (    0,    0,    1,     9,    13,     9,    13)
      tnamt ( 1608) = namtyp (    0,    0,  257,  1024,  1027,  1043,  1046)
      tnamt ( 1667) = namtyp (    0,    0,   25,   377,   388,   380,   393)
      tnamt ( 1668) = namtyp (    0,    0,  257,   688,   690,   705,   707)
      tnamt ( 1682) = namtyp (    0,    0,   28,   568,   575,   584,   591)
      tnamt ( 1686) = namtyp (    0,    0,   21,   765,   768,   783,   786)
      tnamt ( 1736) = namtyp (    0,    0,    1,    54,    59,    54,    59)
      tnamt ( 1750) = namtyp (    0,    0,    6,   697,   711,   714,   729)
      tnamt ( 1776) = namtyp (    0,    0,    1,    14,    18,    14,    18)
      tnamt ( 1777) = namtyp (    0,    0,   48,   486,   493,   501,   509)
      tnamt ( 1830) = namtyp (    0,    0,    5,   323,   328,   324,   329)
      tnamt ( 1931) = namtyp (    0,    0,    5,   740,   742,   758,   760)
      tnamt ( 1937) = namtyp (    0,    0,    5,   729,   735,   747,   753)
      tnamt ( 1956) = namtyp (    0,    0,    6,   675,   681,   692,   698)
      tnamt ( 1964) = namtyp (    0,    0,    5,   154,   159,   154,   159)
      tnamt ( 1974) = namtyp (    0,    0,   23,   247,   252,   248,   253)
      tnamt ( 1977) = namtyp (    0,    0,   40,   997,  1000,  1016,  1019)
      tnamt ( 1992) = namtyp (    0,    0,    1,    30,    32,    30,    32)
      tnamt ( 2009) = namtyp (    0,    0,    2,    95,    98,    95,    98)
      tnamt ( 2023) = namtyp (    0,    0,    5,  1059,  1062,  1078,  1081)
      tnamt ( 2031) = namtyp (    0,    0,    6,   331,   345,   332,   347)
      tnamt ( 2085) = namtyp (    0,    0,  257,   128,   130,   128,   130)
      tnamt ( 2119) = namtyp (    0,    0,    5,   211,   215,   211,   215)
      tnamt ( 2166) = namtyp (    0,    0,   18,   494,   498,   510,   514)
      tnamt ( 2195) = namtyp (    0,    0,    5,   654,   659,   671,   676)
      tnamt ( 2211) = namtyp (    0,    0,   22,  1007,  1010,  1026,  1029)
      tnamt ( 2254) = namtyp (    0,    0,  263,  1174,  1187,  1193,  1206)
      tnamt ( 2264) = namtyp (    0,    0,    2,    99,   102,    99,   102)
      tnamt ( 2268) = namtyp (    0,    0,   23,   721,   728,   739,   746)
      tnamt ( 2285) = namtyp (    0,    0,   38,   854,   859,   872,   877)
      tnamt ( 2287) = namtyp (    0,    0,  257,   936,   952,   955,   971)
      tnamt ( 2295) = namtyp (    0,    0,   22,   806,   810,   824,   828)
      tnamt ( 2297) = namtyp (    0,    0,    5,   794,   796,   812,   814)
      tnamt ( 2303) = namtyp (    0,    0,    6,   253,   259,   254,   260)
      tnamt ( 2311) = namtyp (    0,    0,   23,   502,   512,   518,   528)
      tnamt ( 2353) = namtyp (    0,    0,   54,   401,   409,   407,   416)
      tnamt ( 2354) = namtyp (    0,    0,  257,   670,   674,   687,   691)
      tnamt ( 2364) = namtyp (    0,    0,   12,   791,   793,   809,   811)
      tnamt ( 2401) = namtyp (    0,    0,    5,   549,   552,   565,   568)
      tnamt ( 2421) = namtyp (    0,    0,  263,  1125,  1137,  1144,  1156)
      tnamt ( 2499) = namtyp (    0,    0,    2,   113,   117,   113,   117)
      tnamt ( 2515) = namtyp (    0,    0,    8,   838,   846,   856,   864)
      tnamt ( 2587) = namtyp (    0,    0,    2,   103,   106,   103,   106)
      tnamt ( 2601) = namtyp (    0,    0,   15,   553,   558,   569,   574)
      tnamt ( 2637) = namtyp (    0,    0,    3,   122,   127,   122,   127)
      tnamt ( 2669) = namtyp (    0,    0,   25,   438,   446,   448,   457)
      tnamt ( 2687) = namtyp (    0,    0,  262,  1101,  1110,  1120,  1129)
      tnamt ( 2718) = namtyp (    0,    0,   31,   583,   590,   600,   607)
      tnamt ( 2771) = namtyp (    0,    0,   22,   283,   287,   284,   288)
      tnamt ( 2847) = namtyp (    0,    0,  263,  1138,  1150,  1157,  1169)
      tnamt ( 2863) = namtyp (    0,    0,    5,   309,   313,   310,   314)
      tnamt ( 2878) = namtyp (    0,    0,  257,   143,   147,   143,   147)
      tnamt ( 2884) = namtyp (    0,    0,  257,   276,   278,   277,   279)
      tnamt ( 2914) = namtyp (    0,    0,  262,  1081,  1090,  1100,  1109)
      tnamt ( 2934) = namtyp (    0,    0,   38,   831,   837,   849,   855)
      tnamt ( 2942) = namtyp (    0,    0,    5,   881,   883,   899,   901)
      tnamt ( 2960) = namtyp (    0,    0,  257,   131,   134,   131,   134)
      tnamt ( 3032) = namtyp (    0,    0,    1,    19,    24,    19,    24)
      tnamt ( 3040) = namtyp (    0,    0,    5,  1048,  1058,  1067,  1077)
      tnamt ( 3042) = namtyp (    0,    0,   49,  1066,  1070,  1085,  1089)
      tnamt ( 3053) = namtyp (    0,    0,    6,   167,   177,   167,   177)
      tnamt ( 3091) = namtyp (    0,    0,  257,   736,   739,   754,   757)
      tnamt ( 3097) = namtyp (    0,    0,    2,   118,   121,   118,   121)
      tnamt ( 3098) = namtyp (    0,    0,  257,   922,   925,   940,   943)
      tnamt ( 3125) = namtyp (    0,    0,    9,  1011,  1020,  1030,  1039)
      tnamt ( 3128) = namtyp (    0,    0,    5,   516,   520,   532,   536)
      tnamt ( 3134) = namtyp (    0,    0,    5,  1001,  1006,  1020,  1025)
      tnamt ( 3192) = namtyp (    0,    0,    1,    33,    38,    33,    38)
      tnamt ( 3197) = namtyp (    0,    0,   11,   302,   308,   303,   309)
      tnamt ( 3259) = namtyp (    0,    0,   55,   576,   579,   592,   596)
      tnamt ( 3271) = namtyp (    0,    0,    5,   148,   153,   148,   153)
      tnamt ( 3299) = namtyp (    0,    0,    5,   989,   992,  1008,  1011)
      tnamt ( 3309) = namtyp (    0,    0,    6,   783,   790,   801,   808)
      tnamt ( 3310) = namtyp (    0,    0,  257,   919,   921,   937,   939)
      tnamt ( 3360) = namtyp (    0,    0,    6,   615,   621,   632,   638)
      tnamt ( 3373) = namtyp (    0,    0,  257,   953,   970,   972,   989)
      tnamt ( 3386) = namtyp (    0,    0,  257,   135,   138,   135,   138)
      tnamt ( 3403) = namtyp (    0,    0,   53,   543,   548,   559,   564)
      tnamt ( 3430) = namtyp (    0,    0,   25,   374,   376,   377,   379)
      tnamt ( 3447) = namtyp (    0,    0,   27,   479,   485,   493,   500)
      tnamt ( 3461) = namtyp (    0,    0,    5,   499,   501,   515,   517)
      tnamt ( 3463) = namtyp (    0,    0,  257,  1021,  1023,  1040,  1042)
      tnamt ( 3480) = namtyp (    0,    0,   12,   600,   604,   617,   621)
      tnamt ( 3484) = namtyp (    0,    0,   41,  1034,  1037,  1053,  1056)
      tnamt ( 3488) = namtyp (    0,    0,   56,   888,   896,   906,   914)
      tnamt ( 3497) = namtyp (    0,    0,  263,  1111,  1124,  1130,  1143)
      tnamt ( 3501) = namtyp (    0,    0,  263,  1201,  1215,  1220,  1234)
      tnamt ( 3509) = namtyp (    0,    0,  263,  1151,  1161,  1170,  1180)
      tnamt ( 3512) = namtyp (    0,    0,    5,   513,   515,   529,   531)
      tnamt ( 3525) = namtyp (    0,    0,    2,    66,    70,    66,    70)
      tnamt ( 3535) = namtyp (    0,    0,    7,   292,   301,   293,   302)
      tnamt ( 3539) = namtyp (    0,    0,  257,   139,   142,   139,   142)
      tnamt ( 3611) = namtyp (    0,    0,   14,   394,   400,   400,   406)
      tnamt ( 3612) = namtyp (    0,    0,  257,   525,   527,   541,   543)
      tnamt ( 3676) = namtyp (    0,    0,   13,   186,   191,   186,   191)
      tnamt ( 3718) = namtyp (    0,    0,    5,   540,   542,   556,   558)
      tnamt ( 3719) = namtyp (    0,    0,    9,   691,   696,   708,   713)
      tnamt ( 3729) = namtyp (    0,    0,   38,   971,   978,   990,   997)
      tnamt ( 3733) = namtyp (    0,    0,    5,   754,   759,   772,   777)
      tnamt ( 3803) = namtyp (    0,    0,    7,   178,   185,   178,   185)
      tnamt ( 3854) = namtyp (    0,    0,   21,   242,   246,   243,   247)
      tnamt ( 3895) = namtyp (    0,    0,    2,    75,    79,    75,    79)
      tnamt ( 3922) = namtyp (    0,    0,   25,   447,   456,   458,   468)
      tnamt ( 3930) = namtyp (    0,    0,   33,   628,   636,   645,   653)
      tnamt ( 3968) = namtyp (    0,    0,   18,   225,   228,   226,   229)
      tnamt ( 4013) = namtyp (    0,    0,   21,   605,   611,   622,   628)
      tnamt ( 4031) = namtyp (    0,    0,   26,   426,   437,   435,   447)
      tnamt ( 4032) = namtyp (    0,    0,   24,   359,   364,   361,   367)
      tnamt ( 4034) = namtyp (    0,    0,   22,   521,   524,   537,   540)
      tnamt ( 4038) = namtyp (    0,    0,  257,   279,   282,   280,   283)
      tnamt ( 4049) = namtyp (    0,    0,   36,  1063,  1065,  1082,  1084)
      tnamt ( 4061) = namtyp (    0,    0,   56,   346,   354,   348,   356)
      tnamt ( 4080) = namtyp (    0,    0,   51,   229,   232,   230,   233)
      tnamt ( 4083) = namtyp (    0,    0,   16,     1,     1,     1,     1)
      tnamt ( 8190) = namtyp (    0, 1128,  263,  1188,  1200,  1207,  1219)
      tnamt ( 8191) = namtyp (    0,  393,    6,  1044,  1047,  1063,  1066)
      tnamt ( 8192) = namtyp (    0, 1382,  257,   667,   669,   684,   686)
      ialt =   8190
!
!
end subroutine ininam
subroutine tstidf (znam, kctok, krtok, kwnam, inam)
!  Try to make out what this identifier is
use fprsvars
use flexprms
use fpprcurs
character (len=*), intent (in) :: znam
integer, intent (in)           :: kctok, krtok
integer, intent (out)          :: kwnam, inam
! ____________________________________________________________________
!
    interface
      logical function ifsame (zstr1, zstr2)
!        Case insensitive compare
      character (len=*), intent (in) :: zstr1, zstr2
      end function ifsame
!
      logical function ifposs (kwnam, kctxt, krctx)
!  Test if keyword kwnam possible in context kctxt kwnam krctx
      use flexvars
      use fprsvars
      integer, intent (in) :: kwnam, kctxt, krctx
      end function ifposs
    end interface
!
      lnam = len_trim (znam)
      ihsh = khshstr (znam (1:lnam))
      ifput = 0
      inam  = 0
      if (tnamt(ihsh)%kwnam == 0) then
         kwnam = kwvar
         ifput = 1
         inam  = ihsh
         ibck = 0
      else
         do
            inamd = tnamt(ihsh)%inamd
            inamf = tnamt(ihsh)%inamf
            if (ifsame (znam (1:lnam), znamg (inamd:inamf))) then
               kwnam = tnamt(ihsh)%kwnam
               if (ifposs (kwnam, kctok, krtok)) then
                  inam = ihsh
                  exit
               endif
            endif
            if (tnamt(ihsh)%ihshf == 0) then
               ialt = ialt - 1
               if (tnamt(ialt)%kwnam /= 0) then
                     call fpperr ("insufficient name space, " // &
                                  "raise max # of names and try again")
                     stop
               endif
               kwnam = kwvar
               ifput = 1
               inam = ialt
               ibck = ihsh
               tnamt(ihsh)%ihshf = inam
               exit
            else
               ihsh = tnamt(ihsh)%ihshf
            endif
         enddo
      endif
!
      if (ifput /= 0) then
         inamd = inamg + 1
         inamg = inamg + lnam
         if (inamg > nnamm*lnama) then
            call fpperr ("insufficient name space (identifiers), " // &
                         "raise length of global chain and try again")
            stop
         endif
         inams = inamo + 1
         inamo = inamo + lnam
         if (inamo > nnamm*lnama*2) then
            call fpperr ("insufficient name space (out. idf. names), "//&
                         "raise length of global chain and try again")
            stop
         endif
         tnamt (inam) = namtyp (0, ibck, kwnam, inamd, inamg, &
                                inams, inamo)
         znamg (inamd:inamg) = znam (1:lnam)
         znamo (inams:inamo) = znam (1:lnam)
      endif
      return
end subroutine tstidf
logical function ifsame (zstr1, zstr2)
!  Case insensitive compare
use fpprprms
character (len=*), intent (in) :: zstr1, zstr2
! ____________________________________________________________________
      character (len=1) :: zchr1, zchr2
!
      lstr1 = len_trim (zstr1)
      lstr2 = len_trim (zstr2)
      if (lstr1 == lstr2) then
         ifsame = .true.
         do istr = 1, lstr1
            zchr1 = zstr1 (istr:istr)
            irnk  = index (zlwr, zchr1)
            if (irnk > 0) zchr1 = zupr (irnk:irnk)
            zchr2 = zstr2 (istr:istr)
            irnk  = index (zlwr, zchr2)
            if (irnk > 0) zchr2 = zupr (irnk:irnk)
            ifsame = (zchr1 == zchr2)
            if (.not. ifsame) exit
         enddo
      else
         ifsame = .false.
      endif
end function ifsame
logical function ifposs (kwnam, kctxt, krctx)
!  Test if keyword kwnam possible in context kctxt kwnam krctx
use fprsvars
integer, intent (in) :: kwnam, kctxt, krctx
! ____________________________________________________________________
!
      if (kctxt == kcany .and. krctx == krany) then
         ifposs = .true.
         return
      endif
      select case (kwnam)
      case default
         ifposs = .true.
      case (kwcmd)
         select case (kctxt)
         case default
            ifposs = .false.
         case (kcbeg, kccmd)
            ifposs = .true.
         end select
      case (kwlop, kwlct)
         select case (kctxt)
         case default
            ifposs = .true.
         case (kcukn, kcbeg, kcbex, kcblb, kcbnb)
            ifposs = .false.
         end select
      case (kwfmb)
         select case (kctxt)
         case default
            ifposs = .false.
         case (kcwtf)
            ifposs = .true.
         end select
      case (kwiok)
         select case (kctxt)
         case default
            ifposs = .false.
         case (kcwti)
            ifposs = .true.
         end select
      case (kwatt)
         select case (kctxt)
         case default
            ifposs = .false.
         case (kcbeg, kcdcl, kcany)
            ifposs = .true.
         end select
      case (kwpat)
         select case (kctxt)
         case default
            ifposs = .false.
         case (kcbeg, kcdcl, kcany)
            ifposs = .true.
         end select
      case (kwaca, kwaio, kwac6)
         select case (kctxt)
         case default
            ifposs = .false.
         case (kcbeg, kcbex, kcblb, kcany)
            ifposs = (krctx == krpou)
         end select
      case (kwgnn)
         select case (kctxt)
         case default
            ifposs = .false.
         case (kcntf, kcany)
            ifposs = .true.
         end select
      case (kwprc)
         select case (kctxt)
         case default
            ifposs = .false.
         case (kcbeg, kcdcp, kcany)
            ifposs = .true.
         end select
      case (kwctn)
         select case (kctxt)
         case default
            ifposs = .false.
         case (kcbeg, kcbex, kcany)
            ifposs = (krctx == krlst)
         end select
      case (kwdef)
         select case (kctxt)
         case default
            ifposs = .false.
         case (kccas, kcany)
            ifposs = .true.
         end select
      case (kwnta)
         select case (kctxt)
         case default
            ifposs = .false.
         case (kcntt, kcany)
            ifposs = .true.
         end select
      case (kwsts)
         select case (krctx)
         case default
            ifposs = .false.
         case (krstr)
            ifposs = .true.
         end select
      case (kwtoa)
         select case (kctxt)
         case default
            ifposs = .false.
         case (kcass)
            ifposs = .true.
         end select
      case (kwass)
         select case (kctxt)
         case default
            ifposs = .false.
         case (kcbeg, kcbex, kcblb, kcany)
            ifposs = .true.
         end select
      case (kwac2, kwac3, kwcas, kwgto)
         select case (kctxt)
         case default
            ifposs = .false.
         case (kcbeg, kcbex, kcblb, kcife)
            ifposs = (krctx /= krlst)
         case (kcany)
            ifposs = .true.
         end select
      case (kwac4, kwelw)
         select case (kctxt)
         case default
            ifposs = .false.
         case (kcbeg, kcbex, kcblb, kcife)
            ifposs = (krctx == krlst)
         case (kcany)
            ifposs = .true.
         end select
      case (kwfmt)
         select case (kctxt)
         case default
            ifposs = .false.
         case (kcblb, kcany)
            ifposs = .true.
         end select
      case (kwsel, kwwhe)
         select case (kctxt)
         case default
            ifposs = .false.
         case (kcbeg, kcbex, kcblb, kcbnb, kcany)
            ifposs = .true.
         end select
      case (kwfra)
         select case (kctxt)
         case default
            ifposs = .false.
         case (kcbeg, kcbex, kcblb, kcany)
            ifposs = .true.
         end select
      case (kwac5)
         select case (kctxt)
         case default
            ifposs = .false.
         case (kcbeg, kcbex, kcblb, kcife, kcany)
            ifposs = .true.
         end select
      case (kwacd)
         select case (kctxt)
         case default
            ifposs = .false.
         case (kcbeg)
            ifposs = (krctx /= krlst)
         case (kcany)
            ifposs = .true.
         end select
      case (kwdta)
         select case (kctxt)
         case default
            ifposs = .false.
         case (kcbeg)
            ifposs = (krctx /= krlst)
         case (kcany)
            ifposs = .true.
         end select
      case (kweli)
         select case (kctxt)
         case default
            ifposs = .false.
         case (kcbeg)
            ifposs = (krctx == krpou)
         case (kcany)
            ifposs = .true.
         end select
      case (kwenp)
         select case (kctxt)
         case default
            ifposs = .false.
         case (kcbeg, kcbex, kcblb, kcany)
            ifposs = .true.
         end select
      case (kwent)
         select case (kctxt)
         case default
            ifposs = .false.
         case (kcbeg, kcany)
            ifposs = .true.
         end select
      case (kwfct)
         select case (kctxt)
         case default
            ifposs = (krctx == krpou)
         case (kcbeg, kcdcl, kcdcp, kcany)
            ifposs = .true.
         end select
      case (kwhol)
! No support for Hollerith
            ifposs = .false.
      case (kwifp)
         select case (kctxt)
         case default
            ifposs = .false.
         case (kcbeg, kcbex, kcblb, kcbnb)
            ifposs = (krctx == krpou)
         case (kcany)
            ifposs = .true.
         end select
      case (kwipl)
         select case (kctxt)
         case default
            ifposs = .false.
         case (kcbeg, kcany)
            ifposs = .true.
         end select
      case (kwntt)
         select case (kctxt)
         case default
            ifposs = .false.
         case (kcbeg, kcdcl)
            ifposs = (krctx == krpou)
         case (kcany)
            ifposs = .true.
         end select
      case (kwntf)
         select case (kctxt)
         case default
            ifposs = .false.
         case (kcbeg, kcany)
            ifposs = .true.
         end select
      case (kwnon)
         select case (kctxt)
         case default
            ifposs = .false.
         case (kcipl, kcany)
            ifposs = .true.
         end select
      case (kwuse)
         select case (kctxt)
         case default
            ifposs = .false.
         case (kcbeg)
            ifposs = (krctx /= krlst)
         case (kcany)
            ifposs = .true.
         end select
      case (kwnly)
         select case (kctxt)
         case default
            ifposs = .false.
         case (kcuse, kcany)
            ifposs = .true.
         end select
      case (kwpps)
         select case (kctxt)
         case default
            ifposs = .false.
         case (kcbeg, kcdcl, kcany)
            ifposs = .true.
         end select
      case (kwrsl)
         select case (kctxt)
         case default
            ifposs = .false.
         case (kcprc, kcany)
            ifposs = .true.
         end select
      case (kwstt)
         select case (kctxt)
         case default
            ifposs = .false.
         case (kcall, kcany)
            ifposs = .true.
         end select
      case (kwthn)
         select case (kctxt)
         case default
            ifposs = .false.
         case (kcife, kcany)
            ifposs = .true.
         end select
      case (kwbcl)
         select case (kctxt)
         case default
            ifposs = .false.
         case (kcbeg, kcbex, kcblb, kcbnb, kcany)
            ifposs = .true.
         end select
      case (kwwhl)
         select case (kctxt)
         case default
            ifposs = .false.
         case (kcbcl, kcany)
            ifposs = .true.
         end select
      case (kwels, kweni, kwend, kwens, kwenw, kwenf, kwena)
         select case (kctxt)
         case default
            ifposs = .false.
         case (kcbeg, kcbex, kcany)
            ifposs = .true.
         end select
      case (kwtyp)
         select case (kctxt)
         case default
            ifposs = .false.
         case (kcbeg)
            ifposs = (krctx /= krpou)
         case (kcany)
            ifposs = .true.
         end select
      end select
end function ifposs
subroutine wrtstt (zlab, llab, zstt, lstt, zcmt, lcmt, nndti)
!  write a label, a statement, and a trailing comment
use flexprms
use fpprcurs
character (len=*), intent (in) :: zlab, zstt, zcmt
integer, intent (in)           :: llab, lstt, lcmt, nndti
! ____________________________________________________________________
!
      character (len=*), parameter :: zfmts  = "(/)"
      character (len=*), parameter :: zfmt1n = "(a)"
      character (len=*), parameter :: zfmt1x = "(a,'&')"
      character (len=*), parameter :: zfmtl = "('# line ',i8,a)"
      character (len=1)            :: zdlm
      character (len=linem)        :: zheaw
      character (len=10)           :: zset
!
      integer, save :: nlino = 0
      integer, parameter :: lspltm = 5 ! decisions for splitting
      integer            :: lsplt
!
      lcmtw = lcmt
!
!  Null strings
!
      if (lstt <= 0) then
         if (lcmt /= 0) then
            nndtw = nndti
            lcmtw = lcmt
            if (lcmtw + nndtw > linem) then
               nndtw = 0
               lcmtw = min (lcmt, linem)
            endif
            if (llab == 0) then
               write (lufil, zfmt1n) repeat (" ", nndtw) &
                                     // zcmt (1:lcmtw)
            else
               nndtw = max (1, nndtw - llab)
               write (lufil, zfmt1n)    zlab (1:llab) &
                                     // repeat (" ", nndtw) &
                                     // zcmt (1:lcmtw)
            endif
         else
            if (llab == 0) then
               write (lufil, zfmts)
            else
               write (lufil, zfmt1n) zlab (1:llab)
            endif
         endif
         nlino = nlino + 1
         return
      endif
!
!  Write line number
!
      if (iflnb /= 0) then
         write (lufil, zfmtl) nlinit (iclev), &
                              ' "' // trim(zficit(iclev)) // '"'
      endif
!
!  Find a reasonnable step
!
      if (iffxf == 0) then
         nndtw = nndti
         linew = linel
      else
         nndtw = max (nndti, 6)   ! fixed form code starts after column 6
         linew = 72         ! fixed form code ends on column 72
      endif
      lsttw = lstt + max (llab + 1 - nndtw, 0)
      if (lsttw > linew-2-nndtw) then
         call chxspl
      else
         lsplt = 0
      endif
      if (lsplt >= lspltm) then
         if (iffxf == 0) then
            linew = linem
            call chxspl
         endif
         if (lsplt >= lspltm) then
            if (iffxf == 0) then
               nndtw = 0
            else
               nndtw = 6   ! fixed form code starts columns 6
            endif
            lsttw = lstt + max (llab + 1 - nndtw, 0)
            call chxspl
            if (lsplt >= lspltm) then
               ncnt = (lsttw+linew-nndtw-3) / (linew-2-nndtw)
               write (luerr, *) "More than max # of continuation lines"
               write (luerr, *) "output lines", nlino + 1,             &
                                         " - ", nlino + ncnt
            endif
         endif
      endif
!
      ifin = 0
      ifchc = 0
      nampw = 2
      do
         ideb = ifin + 1
         ifin = ifin + linew - nampw - nndtw
         if (ideb == 1) then
            ifin = ifin + 1 ! no need for & at line start
            if (llab /= 0) then
               nndtw1 = max (nndtw, llab + 1)
               ifin = ifin + nndtw - nndtw1
            endif
         else
            if (nndtw > 1) then
               ifin = ifin + 1 ! leading & will not use character
            endif
         endif
         ifin = min (ifin, lstt)
         ifchp = ifchc
         if (ifin < lstt .and. lsplt < lspltm) then
            ilst = ideb
            do ichr = ideb, ifin
               if (ifchc == 0 .and. &
                   (zstt (ichr:ichr) == "'" .or. &
                    zstt (ichr:ichr) == '"' )    ) then
                  ifchc = 1
                  zdlm = zstt (ichr:ichr)
               elseif (ifchc == 1 .and. &
                       zstt (ichr:ichr) == zdlm) then
                  ifchc = 0
                  ilst  = ichr
               endif
            enddo
            if (ifchc == 0) then
               select case (lsplt)
               case default
                  zset = " "
               case (1)
                  zset = " +-"
               case (2)
                  zset = " +-,:="
               case (3)
                  zset = " +-,:=/*"
               case (4)
                  zset = " +-,:=/*><"
               end select
               ibck = scan (zstt (ilst+1:ifin+1), zset, &
                             back=.true.)
               if (ibck > 0) then
                  ifin = ilst + ibck
               endif
               nampw = 3
            else
               nampw = 2
            endif
         endif
!
!  First line of instruction
!
         if (ideb == 1) then
            if (ifin == lstt) then
               if (lcmt == 0) then
                  if (llab == 0) then
                     write (lufil, zfmt1n) repeat (" ", nndtw) &
                                           // zstt (ideb:ifin)
                  else
                     nndtw2 = nndtw1 - llab
                     write (lufil, zfmt1n)    zlab (1:llab)        &
                                           // repeat (" ", nndtw2) &
                                           // zstt (ideb:ifin)
                  endif
               else
                  if (llab == 0) then
                     llin = ifin - ideb + 1 + nndtw
                     if (llin <= linew) then
                        write (lufil, zfmt1n) repeat (" ", nndtw) &
                                              // zstt (ideb:ifin) &
                                              // zcmt (1:lcmtw)
                     else
                        write (lufil, zfmt1n) repeat (" ", nndtw) &
                                              // zstt (ideb:ifin)
                        nlino = nlino + 1
                        write (lufil, zfmt1n) adjustl (zcmt (1:lcmtw))
                     endif
                  else
                     nndtw2 = nndtw1 - llab
                     llin = ifin - ideb + 1 + nndtw1
                     if (llin <= linew) then
                        write (lufil, zfmt1n)    zlab (1:llab)        &
                                              // repeat (" ", nndtw2) &
                                              // zstt (ideb:ifin)     &
                                              // zcmt (1:lcmtw)
                     else
                        write (lufil, zfmt1n)    zlab (1:llab)        &
                                              // repeat (" ", nndtw2) &
                                              // zstt (ideb:ifin)
                        nlino = nlino + 1
                        write (lufil, zfmt1n) adjustl (zcmt (1:lcmtw))
                     endif
                  endif
               endif
               nlino = nlino + 1
               exit
            else
               if (iffxf == 0) then
                  if (llab == 0) then
                     write (lufil, zfmt1x) repeat (" ", nndtw) &
                                           // zstt (ideb:ifin)
                  else
                     nndtw2 = nndtw1 - llab
                     write (lufil, zfmt1x)    zlab (1:llab)        &
                                           // repeat (" ", nndtw2) &
                                           // zstt (ideb:ifin)
                  endif
               else
                  nfilw = 72 - nndtw - (ifin - ideb + 1)
                  if (llab == 0) then
                     write (lufil, zfmt1x) repeat (" ", nndtw) &
                                        // zstt (ideb:ifin) &
                                        // repeat (" ", nfilw)
                  else
                     nndtw2 = nndtw1 - llab
                     write (lufil, zfmt1x)    zlab (1:llab)        &
                                           // repeat (" ", nndtw2) &
                                           // zstt (ideb:ifin)     &
                                           // repeat (" ", nfilw)
                  endif
               endif
            endif
!
!  Other line of instruction
!
         else
            nndtw1 = max (nndtw, 1)
            if (iffxf == 0) then
               zheaw = repeat (" ", nndtw1 - 1) // "& "
               if (ifchp == 0) then
                  nndtw1 = nndtw1 + 1
               endif
            elseif (ifchp /= 0) then
               zheaw = repeat (" ", 5) // "&"
               nndtw1 = 6
            else
               zheaw = repeat (" ", 5) // "&"
            endif
            if (ifin == lstt) then
               if (lcmt == 0) then
                  write (lufil, zfmt1n) zheaw (1:nndtw1) &
                                     // zstt (ideb:ifin)
               else
                  llin = ifin - ideb + 1 + nndtw1
                  if (llin <= linew) then
                     write (lufil, zfmt1n) zheaw (1:nndtw1) &
                                        // zstt (ideb:ifin) &
                                        // zcmt (1:lcmtw)
                  else
                     write (lufil, zfmt1n) zheaw (1:nndtw1) &
                                        // zstt (ideb:ifin)
                     nlino = nlino + 1
                     write (lufil, zfmt1n) adjustl (zcmt (1:lcmtw))
                  endif
               endif
               nlino = nlino + 1
               exit
            else
               if (iffxf == 0) then
                  write (lufil, zfmt1x) zheaw (1:nndtw1) &
                                     // zstt (ideb:ifin)
               else
                  nfilw = 72 - nndtw1 + 1 - (ifin - ideb + 2)
                  write (lufil, zfmt1x) zheaw (1:nndtw1) &
                                        // zstt (ideb:ifin) &
                                        // repeat (" ", nfilw)
               endif
            endif
         endif
         nlino = nlino + 1
      enddo
      return
contains
   subroutine chxspl
! Choose assumptions governing splitting at end of lines
! ____________________________________________________________________
!
   lsplt = 0
   levels: do
      if (lsplt >= lspltm) exit levels
      ifinw = 0
      ifchc = 0
      nlinw = 0
      nampw = 2
      do
         idebw = ifinw + 1
         ifinw = ifinw + linew - nampw - nndtw
         if (idebw == 1) then
            ifinw = ifinw + 1 ! no need for & at line start
            if (llab /= 0) then
               nndtw1 = max (nndtw, llab + 1)
               ifinw = ifinw + nndtw - nndtw1
            endif
         else
            if (nndtw > 1) then
               ifinw = ifinw + 1 ! leading & will not use character
            endif
         endif
         ifinw = min (ifinw, lstt)
!
!  Need to split - Use current criteria
!
         if (ifinw < lstt) then
            ilst = idebw
            do ichr = idebw, ifinw
               if (ifchc == 0 .and. &
                   (zstt (ichr:ichr) == "'" .or. &
                    zstt (ichr:ichr) == '"' )    ) then
                  ifchc = 1
                  zdlm = zstt (ichr:ichr)
               elseif (ifchc == 1 .and. &
                       zstt (ichr:ichr) == zdlm) then
                  ifchc = 0
                  ilst  = ichr
               endif
            enddo
            if (ifchc == 0) then
               select case (lsplt)
               case default
                  zset = " "
               case (1)
                  zset = " +-"
               case (2)
                  zset = " +-,:="
               case (3)
                  zset = " +-,:=/*"
               case (4)
                  zset = " +-,:=/*><"
               end select
               ibck = scan (zstt (ilst+1:ifinw+1), zset, &
                             back=.true.)
               if (ibck > 0) then
                  ifinw = ilst + ibck
                  nampw = 3
               else
                  lsplt = lsplt + 1
                  cycle levels
               endif
            else
               ibck = index (zstt (ilst+1:ifinw+1), zdlm, &
                             back=.true.)
               if (ibck > 0) then
                  ifinw = ilst + ibck - 1
                  nampw = 3
                  ifchc = 0
               else
                  nampw = 2
               endif
            endif
         endif
!
!  First line of instruction
!
         if (idebw == 1) then
            if (ifinw == lstt) then
               nlinw = nlinw + 1
               exit
            endif
!
!  Other line of instruction
!
         else
            if (ifinw == lstt) then
               nlinw = nlinw + 1
               exit
            endif
         endif
         nlinw = nlinw + 1
         if (nlinw > ncntm) exit
      enddo
      if (nlinw <= ncntm) then
         return
      else
         lsplt = lsplt + 1
      endif
   enddo levels
   end subroutine chxspl
end subroutine wrtstt
subroutine fpperr (zstr)
!  Output error message
use fpprcurs
character (len=*), intent (in) :: zstr
! ____________________________________________________________________
!
      character (len=*), parameter :: zfmt = &
                            "(a, ', line ', a, ': Error:', ' ', a)"
      character (len=11) :: znum
!
      write (znum, "(i11)") nlinit (iclev)
      write (luerr, zfmt) trim (zficit (iclev)), &
                          trim (adjustl (znum)), &
                          trim (zstr)
      return
end subroutine fpperr
subroutine lexfxd (trttok, ifstp, ksta)
!  Read input file, lexing fixed-form into token stream, until a
!  simultaneous end-of-line end-of-statement is found.
use flexprms
use fpprcurs
interface
         subroutine trttok (ztok, ltok, kktok)
!  add token to current stream, and reduce if end of statement
            use flexvars
            use fpprcurs
            integer, intent (in)              :: ltok, kktok
            character (len=ltok), intent (in) :: ztok
         end subroutine trttok
end interface
integer, intent (in)                :: ifstp ! strip-out comments ?
integer, intent (out)               :: ksta  ! status code
! ____________________________________________________________________
      character (len=2*linem) :: zlinw
      character (len=lsttm) :: zlin
      character (len=lsttm) :: ztok
      character (len=1)     :: zdlm, zchr
      integer, save         :: ifctn = 0
!
      ksta  = 0
      llin  = 0
      ifchc = 0
      ifcnt = 0
      ntok  = 0
      kktok = kkndf
!
body: do
         llin = 0
         ibeg = 1
!
!  Read a line
!
rdlin:   do
            do
               ifctn = 0
               if (klrea == kllst .or. klrea == kltcm) then
                  if (iclev > 0) then
                     zlinb = zlinbh (iclev)
                     nhav  = nhavh  (iclev)
                     klnxt = klnxth (iclev)
                     iclev = iclev - 1
                     close (lufic)
                     lufic = lufic - 1
                     if (iclev == 0) then
                        luinp = lustdi
                     else
                        luinp = lufic
                     endif
                     if (klnxt /= kllst .or. nhav /= 0) exit
                  else
                     ksta = -1
                     exit body
                  endif
               elseif (klrea == klctd) then
                  ifctn = 1
               endif
!
               call realin (luinp, zlinw, klrea)
               if (klrea == klunv .and. iclev /= 0) then
                  klrea = kllst
                  cycle
               endif
               exit
            enddo
!
            select case (klrea)
!
!  Unavailable
!
            case (klunv)
               ksta = 1
               call fpperr ("Problem reading input")
               exit body
!
!  "False comments"
!
            case (klfcm)
               llinw = len_trim (zlinw)
               do icmti = 1, ncmti
                  lcmti = len_trim (zcmtit (icmti))
                  if (llinw > lcmti .and. &
                      zlinw (1:lcmti) == zcmtit (icmti)(1:lcmti)) then
                     call trttok (zlinw (1:lcmti), lcmti, kkfcm)
                     ibeg = lcmti + 1
                     exit rdlin
                  endif
               enddo
!
!  True comments
!
            case (klcmt, kltcm)
               llinw = len_trim (zlinw)
               call trttok (zlinw, llinw, kkcmt)
               call trttok (ztok, 0, kkeos)
               cycle rdlin
!
!  Non-comment, not continued
!
            case (klnrm, kllst)
               llinw = len_trim (zlinw)
               ilinw = verify (zlinw (1:llinw), ztab // " ")
               ilin  = llin + 1
               llin  = llinw - ilinw + ilin 
               zlin (ilin:llin) = zlinw (ilinw:llinw)
               exit rdlin
!
!  Continued
!
            case (klctd)
!
!  Check for trailing comment
!
               if (ifctn == 0) then
                  llinw = 72
               else
                  llinw = 72 - 6
               endif
               ichr = ibeg - 1
               ifctc = 0
check:         do
                  if (ichr >= llinw) then
                     exit check
                  endif
                  ichr = ichr + 1
                  zchr = zlinw (ichr:ichr)
                  if (ifctc == 0) then
                     if (zchr == "!") then
                        if (ifstp == 0) &
                           call wrtstt (zlinw, 0, zlinw, 0, &
                                        zlinw (ichr:llinw), &
                                        llinw - ichr + 1, ichr-1)
                        llinw = ichr - 1
                        exit check
                     elseif (zchr == '"' .or. zchr == "'") then
                        zdlm = zchr
                        ifctc = 1
                     endif
                  else
                     if (zchr == zdlm) then
                        ifctc = 0
                     endif
                  endif
               enddo check
!
               ilinw = verify (zlinw (1:llinw), ztab // " ")
               ilin  = llin + 1
               llin  = llinw - ilinw + ilin 
               zlin (ilin:llin) = zlinw (ilinw:llinw)
               cycle rdlin
            end select
!
         enddo rdlin
!
         ichr = ibeg - 1
!
!  Scan line
!
         do
               do
                  if (ichr >= llin) then
                     if (ifcnt == 0) then
                        if (kktok /= kkndf) then
                           call trttok (ztok, ltok, kktok)
                        endif
                        exit body
                     else
                        cycle body
                     endif
                  endif
                  ichr = ichr + 1
                  zchr = zlin (ichr:ichr)
                  if (ifchc == 0) then
                     select case (zchr)
!
!  Tabs
!
                     case (ztab)
                        if (kktok /= kkndf) then
                           call trttok (ztok, ltok, kktok)
                        endif
                        kktok = kkndf
!
!  Spaces (are taken as significant, too complex to handle otherwise)
!
                     case (' ')
                        if (kktok /= kkndf) then
                           call trttok (ztok, ltok, kktok)
                        endif
                        kktok = kkndf
!
!  Letters
!
                     case ('A':'Z','a':'z')
                        if (kktok == kkidf .or. kktok == kkcmd) then
                           ltok = ltok + 1
                           ztok (ltok:ltok) = zchr
                        else
                           if (kktok /= kkndf) then
                              call trttok (ztok, ltok, kktok)
                           endif
                           ntok = ntok + 1
                           ltok = 1
                           ztok (ltok:ltok) = zchr
                           kktok = kkidf
                        endif
!
!  Digits
!
                     case ('0':'9')
                        if (kktok == kkidf .or. kktok == kknui) then
                           ltok = ltok + 1
                           ztok (ltok:ltok) = zchr
                        else
                           if (kktok /= kkndf) then
                              call trttok (ztok, ltok, kktok)
                           endif
                           ntok = ntok + 1
                           ltok = 1
                           ztok (ltok:ltok) = zchr
                           kktok = kknui
                        endif
!
!  Underscore (may be in identifier, or as a kind specifier)
!
                     case ('_')
                        select case (kktok)
                        case (kkidf)
                           ltok = ltok + 1
                           ztok (ltok:ltok) = zchr
                        case (kknui, kkstr)
                           call trttok (ztok, ltok, kktok)
                           ntok = ntok + 1
                           call trttok (zchr, 1, kkknd)
                           kktok = kkndf
                        case default
                           if (kktok /= kkndf) then
                              call trttok (ztok, ltok, kktok)
                           endif
                           ntok = ntok + 1
                           ltok = 1
                           ztok (ltok:ltok) = zchr
                           kktok = kkidf
                        end select
!
!  Colon
!
                     case (':')
                        if (kktok /= kkndf) then
                           call trttok (ztok, ltok, kktok)
                        endif
                        ntok = ntok + 1
                        call trttok (zchr, 1, kkdpt)
                        kktok = kkndf
!
!  Semi-colon
!
                     case (';')
                        if (kktok /= kkndf) then
                           call trttok (ztok, ltok, kktok)
                        endif
                        ntok = ntok + 1
                        call trttok (zchr, 1, kkpvg)
                        kktok = kkndf
!
!  Opening parenthesis
!
                     case ('(')
                        if (kktok /= kkndf) then
                           call trttok (ztok, ltok, kktok)
                        endif
                        ntok = ntok + 1
                        ltok = 1
                        ztok (ltok:ltok) = zchr
                        kktok = kkpou
!
!  Closing parenthesis
!
                     case (')')
                        if (kktok == kkslh) then
                           ltok = ltok + 1
                           ztok (ltok:ltok) = zchr
                           call trttok (ztok, ltok, kkcfr)
                           kktok = kkndf
                        else
                           if (kktok /= kkndf) then
                              call trttok (ztok, ltok, kktok)
                           endif
                           ntok = ntok + 1
                           call trttok (zchr, 1, kkpfr)
                           kktok = kkndf
                        endif
!
!  Exclamation mark (start of comment)
!
                     case ('!')
                        if (kktok /= kkndf .and. ifcnt == 0) then
                           call trttok (ztok, ltok, kktok)
                        endif
                        if (ifcnt == 0) then
                           ntok = ntok + 1
                           call trttok (zlin (ichr:llin), (llin-ichr+1),&
                                        kkcmt)
                           exit body
                        else
                           if (ifstp == 0) &
                           call wrtstt (zlin, 0, zlin, 0, &
                                        zlin (ichr:llin), &
                                        llin - ichr + 1, ichr-1)
                           cycle body
                        endif
!
!  Dollar (used as preprocessor command introduction)
!
                     case ('$')
                        if (kktok /= kkndf) then
                           call trttok (ztok, ltok, kktok)
                        endif
                        ntok = ntok + 1
                        ltok = 1
                        ztok (ltok:ltok) = '$'
                        kktok = kkcmd
!
!  Sharp (same as $ or !, depending on current status)
!
                     case ('#')
                        if (ifsed /= 0) then
                           if (kktok /= kkndf) then
                              call trttok (ztok, ltok, kktok)
                           endif
                           ntok = ntok + 1
                           ltok = 1
                           ztok (ltok:ltok) = '$'
                           kktok = kkcmd
                        else
                           if (kktok /= kkndf .and. ifcnt == 0) then
                              call trttok (ztok, ltok, kktok)
                           endif
                           if (ifcnt == 0) then
                              ntok = ntok + 1
                              call trttok (zlin (ichr:llin),&
                                           (llin-ichr+1), kkcmt)
                              exit body
                           else
                              if (ifstp == 0) &
                              call wrtstt (zlin, 0, zlin, 0, &
                                           zlin (ichr:llin), &
                                           llin - ichr + 1, ichr-1)
                              cycle body
                           endif
                        endif
!
!  Question mark
!
                     case ('?')
                        if (kktok /= kkndf) then
                           call trttok (ztok, ltok, kktok)
                        endif
                        ntok = ntok + 1
                        call trttok (zchr, 1, kkqst)
                        kktok = kkndf
!
!  Continuation mark
!
                     case ('&')
                        ifcnt = 1
                        if (ichr < llin) then
                           inxt = verify (zlin (ichr+1:llin),        &
                                          ztab // " ")
                           if (inxt /= 0) then
                              if (zlin (ichr+inxt:ichr+inxt) /= "!") then
                                 if (kktok /= kkndf) then
                                    call trttok (ztok, ltok, kktok)
                                 endif
                                 ntok = ntok + 1
                                 call trttok (zchr, 1, kkamp)
                                 kktok = kkndf
                                 ifcnt = 0
                              endif
                           endif
                        endif
!
!  Percent
!
                     case ('%')
                        if (kktok /= kkndf) then
                           call trttok (ztok, ltok, kktok)
                        endif
                        ntok = ntok + 1
                        call trttok (zchr, 1, kkprc)
                        kktok = kkndf
!
!  Plus and Minus
!
                     case ('+','-')
                        if (kktok /= kkndf) then
                           call trttok (ztok, ltok, kktok)
                        endif
                        ntok = ntok + 1
                        call trttok (zchr, 1, kkpms)
                        kktok = kkndf
!
!  Slash
!
                     case ('/')
                        select case (kktok)
                        case (kkslh)
                           ltok = ltok + 1
                           ztok (ltok:ltok) = zchr
                           call trttok (ztok, ltok, kkcct)
                           kktok = kkndf
                        case (kkpou)
                           ltok = ltok + 1
                           ztok (ltok:ltok) = zchr
                           call trttok (ztok, ltok, kkcou)
                           kktok = kkndf
                        case default
                           if (kktok /= kkndf) then
                              call trttok (ztok, ltok, kktok)
                           endif
                           ntok = ntok + 1
                           ltok = 1
                           ztok (ltok:ltok) = zchr
                           kktok = kkslh
                        end select
!
!  Star
!
                     case ('*')
                        if (kktok == kksta) then
                           ltok = ltok + 1
                           ztok (ltok:ltok) = zchr
                           call trttok (ztok, ltok, kkpow)
                           kktok = kkndf
                        else
                           if (kktok /= kkndf) then
                              call trttok (ztok, ltok, kktok)
                           endif
                           ntok = ntok + 1
                           ltok = 1
                           ztok (ltok:ltok) = zchr
                           kktok = kksta
                        endif
!
!  Superior
!
                     case ('>')
                        if (kktok == kkaff) then
                           ltok = ltok + 1
                           ztok (ltok:ltok) = zchr
                           call trttok (ztok, ltok, kkpts)
                           kktok = kkndf
                        else
                           if (kktok /= kkndf) then
                              call trttok (ztok, ltok, kktok)
                           endif
                           ntok = ntok + 1
                           ltok = 1
                           ztok (ltok:ltok) = zchr
                           kktok = kksup
                        endif
!
!  Inferior
!
                     case ('<')
                        if (kktok /= kkndf) then
                           call trttok (ztok, ltok, kktok)
                        endif
                        ntok = ntok + 1
                        ltok = 1
                        ztok (ltok:ltok) = zchr
                        kktok = kkinf
!
!  Equal
!
                     case ('=')
                        select case (kktok)
                        case (kkslh)
                           ltok = ltok + 1
                           ztok (ltok:ltok) = zchr
                           call trttok (ztok, ltok, kkneq)
                           kktok = kkndf
                        case (kkinf)
                           ltok = ltok + 1
                           ztok (ltok:ltok) = zchr
                           call trttok (ztok, ltok, kkleq)
                           kktok = kkndf
                        case (kkaff)
                           ltok = ltok + 1
                           ztok (ltok:ltok) = zchr
                           call trttok (ztok, ltok, kkequ)
                           kktok = kkndf
                        case (kksup)
                           ltok = ltok + 1
                           ztok (ltok:ltok) = zchr
                           call trttok (ztok, ltok, kkgeq)
                           kktok = kkndf
                        case default
                           if (kktok /= kkndf) then
                              call trttok (ztok, ltok, kktok)
                           endif
                           ntok = ntok + 1
                           ltok = 1
                           ztok (ltok:ltok) = zchr
                           kktok = kkaff
                        end select
!
!  Dot
!
                     case ('.')
                        if (kktok /= kkndf) then
                           call trttok (ztok, ltok, kktok)
                        endif
                        ntok = ntok + 1
                        call trttok (zchr, 1, kkdot)
                        kktok = kkndf
!
!  Separator
!
                     case (',')
                        if (kktok /= kkndf) then
                           call trttok (ztok, ltok, kktok)
                        endif
                        ntok = ntok + 1
                        call trttok (zchr, 1, kksep)
                        kktok = kkndf
!
!  String delimiter
!
                     case ('"',"'")
                        if (kktok == kkstr) then
                           if (zchr == zdlm) then
                              ltok = ltok + 1
                              ztok (ltok:ltok) = zchr
                           else
                              zdlm  = zchr
                              ltok  = 1
                              ztok (ltok:ltok) = zchr
                              ntok  = ntok + 1
                           endif
                           ifchc = 1
                        else
                           if (kktok /= kkndf) then
                              call trttok (ztok, ltok, kktok)
                           endif
                           zdlm  = zchr
                           ltok  = 1
                           ztok (ltok:ltok) = zchr
                           ntok  = ntok + 1
                           kktok = kkstr
                           ifchc = 1
                        endif
!
!  Other character
!
                     case default
                        if (kktok /= kkndf) then
                           call trttok (ztok, ltok, kktok)
                        endif
                        ntok = ntok + 1
                        call trttok (zchr, 1, kkukn)
                        kktok = kkndf
                     end select
!
!  We are inside a char string
!
                  else
!
!  Test for end of current string
!
                     if (zchr == zdlm) then
                        ltok  = ltok + 1
                        ztok (ltok:ltok) = zchr
                        ifchc = 0
                     else
!
!  Test for end of line
!
                        if (ichr == llin) then
                           if (zchr == '&') then
                              ifcnt = 1
                              cycle body
                           else
                              call fpperr ("Unmatched " // zdlm)
                           endif
                        else
                           ltok = ltok + 1
                           ztok (ltok:ltok) = zchr
                        endif
                     endif
                  endif
               enddo
            enddo
      enddo body
      call trttok (ztok, 0, kkeos)
      return
end subroutine lexfxd
subroutine outstt (ntok, ksstt)
!  rebuild current statement
use flexvars
use fprsvars
use fpprcurs
integer, intent (in)       :: ntok
integer, intent (in)       :: ksstt
! ____________________________________________________________________
!
      integer, dimension (nnstdm) :: irepst
      integer, dimension (nargm)  :: irepat
      character (len=2*lsttm) :: zstt
      character (len=linem)   :: zcmt, zlab
      character (len=lsttm)   :: ztokw
!
!  Return if pre-processor command
!
      if (ksstt == ksppr) return
!
!  Need to change indent value ?
!
      select case (ksstt)
      case (ksprm)
         lprc = lprc - 1
         if (lprc == 0) nndt = nndt - nndtp
         nndt = nndt - nndtp
      case (kspre)
         lprc = lprc - 1
         if (lprc == 0) nndt = nndt - nndtp
         nndt = nndt - nndtp
      case (ksifm)
         nndt = nndt - nndtp
      case (ksife)
         nndt = nndt - nndtp
      case (ksfre)
         nndt = nndt - nndtp
      case (kswhm)
         nndt = nndt - nndtp
      case (kswhe)
         nndt = nndt - nndtp
      case (ksdoe)
         do idoe = 1, ndoe
            nndt = nndt - nndtp
         enddo
      case (ksnte)
         nndt = nndt - nndtp
      case (ksslm)
         nndt = nndt - nndtp
      case (kssle)
         nndt = nndt - nndtp
      case (kstye)
         nndt = nndt - nndtp
      end select
      nndt = max (nndt, 0)
      lstt = 0
      llab = 0
      lpar = 0
      irep1 = irep
      irepg1 = irepg
      if (ntok == 0) then
!
!  blank lines
!
         call wrtstt (zlab, llab, zblk, 0, zblk, len_trim (zblk), 0)
      elseif (ntok == 1 .and. kktokt (1) == kkcmt) then
         itokd = itokdt (1)
         itokf = itokft (1)
         ltok  = itokf - itokd + 1
         call wrtstt (zlab, llab, zblk, 0, ztoki (itokd:itokf), ltok, 0)
      elseif (ntok == 1 .and. kktokt (1) == kkebc) then
         itokd = itokdt (1)
         itokf = itokft (1)
         ltok  = itokf - itokd + 1
         call wrtstt (zlab, llab, zblk, 0, ztoki (itokd:itokf), ltok, 0)
      elseif (ntok == 1 .and. kktokt (1) == kkfcm) then
         itokd = itokdt (1)
         itokf = itokft (1)
         ltok  = itokf - itokd + 1
         call wrtstt (ztoki (itokd:itokf), ltok, zblk, 0, zblk, 0, 0)
      else
         kkprv = kkukn
         ifblk = 0
         lcmt  = 0
         instr = 0
         itok  = 0
         do
            if (instr == 0) then
               itok = itok + 1
               if (itok > ntok) exit
               itokd = itokdt (itok)
               itokf = itokft (itok)
               ltokw = itokf - itokd + 1
               ztokw (1:ltokw) = ztoki (itokd:itokf)
               kktok = kktokt (itok)
               if (kktok == kkidf) then
                  inam = inamwt (itok)
                  kwnam = tnamt(inam)%kwnam
                  if (kwnam == kwdfd .and. tnamt(inam)%irepc /= 0) then
                     instr = 1
                     irepd = tnamt(inam)%irepc
                     cycle
                  elseif (kwnam >= kwmc0 .and. tnamt(inam)%irepc /= 0) &
                         then
!
!  Analyse macro arguments
!
                     narge = kwnam - kwmc0
                     narg  = 1
                     irepat (narg) = irep1 + 1
                     itok1 = itok + 1
maca:                do
                        if (ntok < itok1 + 2*narge) then
                           ifvld = 0
                           exit maca
                        endif
                        if (kktokt (itok1) /= kkpou .and. &
                            kktokt (itok1) /= kkpnb) then
                           ifvld = 0
                           exit maca
                        endif
                        lpar  = 1
                        do
                           itok1 = itok1 + 1
                           if (itok1 > ntok) then
                              ifvld = 0
                              exit maca
                           endif
!
!  Add token to current arg list
!
                           itokd = itokdt (itok1)
                           itokf = itokft (itok1)
                           ltokw = itokf - itokd + 1
                           ireps  = irepg1 + 1
                           irepg1 = irepg1 + ltokw
                           if (irepg1 > nrepgm) then
                              write (luerr, *) "insufficient name space&
                                               & (out. repl. names)"
                              write (luerr, *) "raise length of global&
                                               & chain and try again"
                              stop
                           endif
                           irep1 = irep1 + 1
                           if (irep1 > nrepm) then
                              write (luerr, *) "insufficient name space&
                                               & (out. repl. names)"
                              write (luerr, *) "raise # of repl. names&
                                               & and try again"
                              stop
                           endif
                           kkrept (irep1) = kktokt (itok1)
                           irepdt (irep1) = ireps
                           irepft (irep1) = irepg1
                           irepnt (irep1) = irep1 + 1
                           zrepg (ireps:irepg1) = ztoki (itokd:itokf)
!
! Check for next argument or last one
! Find out nature of names
!
                           select case (kktokt (itok1))
                           case (kkpfr)
                              lpar = lpar - 1
                              if (lpar == 0) then
                                 if (narg == narge) then
                                    irep1 = irep1 - 1
                                    irepnt (irep1) = 0
                                    itok = itok1
                                    ifvld = 1
                                 else
                                    ifvld = 0
                                 endif
                                 exit maca
                              endif
                           case (kkpou, kkpnb)
                              lpar = lpar + 1
                           case (kksep)
                              if (lpar == 1) then
                                 irep1 = irep1 - 1
                                 irepnt (irep1) = 0
                                 narg = narg + 1
                                 irepat (narg) = irep1 + 1
                              endif
                           case (kkidf)
                              call tstidf (zrepg (ireps:irepg1), kcukn, &
                                           krukn, kwork, irepwt (irep1))
                           case default
                              continue
                           end select
                        enddo
                     enddo maca
!
! If it was legal macro, apply it
!
                     if (ifvld /= 0) then
                        instr = 1
                        irepd = tnamt(inam)%irepc
                        cycle
                     endif
                  else
                     itokd = tnamt(inam)%inamod
                     itokf = tnamt(inam)%inamof
                     ltokw = itokf - itokd + 1
                     ztokw (1:ltokw) = znamo (itokd:itokf)
                  endif
               endif
            else
               if (irepd == 0) then
                  instr = instr - 1
                  if (instr /= 0) then
                     irepd = irepst (instr)
                  endif
                  cycle
               endif
               itokd = irepdt (irepd)
               itokf = irepft (irepd)
               ltokw = itokf - itokd + 1
               ztokw (1:ltokw) = zrepg (itokd:itokf)
               kktok = kkrept (irepd)
               if (kktok == kkidf) then
                  inam  = irepwt (irepd)
                  kwnam = tnamt(inam)%kwnam
                  if (kwnam == kwdfd .and. tnamt(inam)%irepc /= 0) then
                     irepst (instr) = irepnt (irepd)
                     instr = instr + 1
                     if (instr > nnstdm) then
                        write (luerr, *) "defines should not be nested",&
                                         "deeper than", nnstdm
                        exit
                     endif
                     irepd = tnamt(inam)%irepc
                     cycle
                  endif
               elseif (kktok > kkar0) then
                  irepst (instr) = irepnt (irepd)
                  instr = instr + 1
                  if (instr > nnstdm) then
                     write (luerr, *) "defines should not be nested",&
                                      "deeper than", nnstdm
                     exit
                  endif
                  iarg  = kktok - kkar0
                  irepd = irepat (iarg)
                  cycle
               endif
               irepd = irepnt (irepd)
            endif
            if (ltokw + lstt + 2 > 2*lsttm) then
               call fpperr ("Substitution leads to line length overflow")
               exit
            endif
!
!  Construct line inserting spaces where necessary
!
            select case (kktok)
            case (kkcmt, kkebc)   ! Comment string
               if (ifblk == 0) then
                  ltok = ltokw + 1
                  zcmt (1:ltok) = " " // ztokw (1:ltokw)
               else
                  ltok = ltokw
                  zcmt (1:ltok) = ztokw (1:ltokw)
               endif
               lcmt = ltok
               exit
            case (kkfcm)   ! False comment
               llab = ltokw
               zlab (1:llab) = ztokw (1:ltokw)
               ltok = 0
            case (kklab)   ! Numerical label
               if (lstt == 0) then
                  llab = ltokw
                  zlab (1:llab) = ztokw (1:ltokw)
                  ltok = 0
               else
                  ltok = ltokw + 2
                  zstt (lstt+1:lstt+ltok) = " " // ztokw (1:ltokw) // " "
                  ifblk = 1
               endif
            case (kkstr, & ! Character string
            &     kkknd, & ! _Kind (underscore)
            &     kkbnm, & ! Block name
            &     kknui, & ! Integer Numerical value
            &     kknuf)   ! Real Numerical value
               ltok = ltokw
               zstt (lstt+1:lstt+ltok) = ztokw (1:ltokw)
               ifblk = 0
            case (kkprc)   ! %
               ltok = ltokw
               if (ifblk /= 0) lstt = lstt - 1
               zstt (lstt+1:lstt+ltok) = ztokw (1:ltokw)
               ifblk = 0
            case (kksep)   ! ,
               ltok = ltokw + 1
               if (ifblk /= 0) lstt = lstt - 1
               zstt (lstt+1:lstt+ltok) = ztokw (1:ltokw) // " "
               ifblk = 1
            case (kkidf)   ! Identifier
!
               if (kwnam <= kwsys .and. kccask /= kclve) then
                  call chgcas (ztokw (1:ltokw), kccask)
               elseif (kwnam > kwsys .and. kccasu /= kclve) then
                  call chgcas (ztokw (1:ltokw), kccasu)
               endif
!
               if (kwnam == kwlop .and. ifblk == 0) then
                  ltok = ltokw + 2
                  zstt (lstt+1:lstt+ltok) = " " // ztokw (1:ltokw) // " "
                  ifblk = 1
               elseif (kwnam == kwlop .and. ifblk /= 0) then
                  ltok = ltokw + 1
                  zstt (lstt+1:lstt+ltok) = ztokw (1:ltokw) // " "
                  ifblk = 1
               elseif (kwnam == kwac5) then
                  if (ifblk /= 0 .or. lstt == 0) then
                     ltok = ltokw + 1
                     zstt (lstt+1:lstt+ltok) = ztokw (1:ltokw) // " "
                  else
                     ltok = ltokw + 2
                     zstt (lstt+1:lstt+ltok) = " " // ztokw (1:ltokw)   &
                                                   // " "
                  endif
                  ifblk = 1
               elseif ((kkprv == kkidf .or. &
                        kkprv == kknui .or. &
                        kkprv == kknuf ) .and. ifblk == 0) then
                  ltok = ltokw + 1
                  zstt (lstt+1:lstt+ltok) = " " // ztokw (1:ltokw)
                  ifblk = 0
               else
                  ltok = ltokw
                  zstt (lstt+1:lstt+ltok) = ztokw (1:ltokw)
                  ifblk = 0
               endif
            case (kkdpt)   ! :
               if (lpar <= 0) then
                  ltok = ltokw + 1
                  zstt (lstt+1:lstt+ltok) = ztokw (1:ltokw) // " "
                  ifblk = 1
               else
                  ltok = ltokw
                  zstt (lstt+1:lstt+ltok) = ztokw (1:ltokw)
                  ifblk = 0
               endif
!
            case (kkpvg, & ! ;
            &     kkcmd, & ! $command (preprocessor)
            &     kkdot)   ! )
               ltok = ltokw + 1
               zstt (lstt+1:lstt+ltok) = ztokw (1:ltokw) // " "
               ifblk = 1
            case (kkqst)   ! ?
               ltok = 0
               if (ifblk /= 0) lstt = lstt - 1
               ifblk = 0
            case (kkpfr)   ! )
               lpar = lpar - 1
               ltok = ltokw + 1
               if (ifblk /= 0) lstt = lstt - 1
               zstt (lstt+1:lstt+ltok) = ztokw (1:ltokw) // " "
               ifblk = 1
            case (kkpou)   ! (
               lpar = lpar + 1
               if (lpar <= 1 .and. ifblk == 0) then
                  ltok = ltokw + 1
                  zstt (lstt+1:lstt+ltok) = " " // ztokw (1:ltokw)
               else
                  ltok = ltokw
                  zstt (lstt+1:lstt+ltok) = ztokw (1:ltokw)
               endif
               ifblk = 0
            case (kkpnb)   ! ( within defined type
               lpar = lpar + 1
               ltok = ltokw
               zstt (lstt+1:lstt+ltok) = ztokw (1:ltokw)
               ifblk = 0
            case (kkslh, & ! /
            &     kkpms, & ! + or -
            &     kkcct, & ! //
            &     kksta, & ! *
            &     kkpow, & ! **
            &     kkaff)   ! =
               if (lpar <= 0 .and. ifblk == 0) then
                  ltok = ltokw + 2
                  zstt (lstt+1:lstt+ltok) = " " // ztokw (1:ltokw) // " "
                  ifblk = 1
               elseif (lpar <= 0) then
                  ltok = ltokw + 1
                  zstt (lstt+1:lstt+ltok) = ztokw (1:ltokw) // " "
                  ifblk = 1
               else
                  if (ifblk /= 0) lstt = lstt - 1
                  ltok = ltokw
                  zstt (lstt+1:lstt+ltok) = ztokw (1:ltokw)
                  ifblk = 0
               endif
            case (kkcou, & ! (/
            &     kkneq, & ! /=
            &     kkleq, & ! <=
            &     kkequ, & ! ==
            &     kkgeq, & ! >=
            &     kkpts, & ! =>
            &     kksup, & ! >
            &     kkinf, & ! <
            &     kkdcl, & ! ::
            &     kklog, & ! .xxx.
            &     kkcfr)   ! /)
               if (ifblk == 0) then
                  ltok = ltokw + 2
                  zstt (lstt+1:lstt+ltok) = " " // ztokw (1:ltokw) // " "
               else
                  ltok = ltokw + 1
                  zstt (lstt+1:lstt+ltok) = ztokw (1:ltokw) // " "
               endif
               ifblk = 1
            case default
               ltok = ltokw
               zstt (lstt+1:lstt+ltok) = ztokw (1:ltokw)
               ifblk = 0
            endselect
            lstt = lstt + ltok
            kkprv = kktok
         enddo
         do
            if (zstt (lstt:lstt) /= " " .or. lstt <= 1) exit
            lstt = lstt - 1
         enddo
         call wrtstt (zlab, llab, zstt, lstt, zcmt, lcmt, nndt)
      endif
!
!  Need to change indent value ?
!
      select case (ksstt)
      case (ksprs)
         if (lprc == 0) nndt = nndt + nndtp
         lprc = lprc + 1
         nndt = nndt + nndtp
      case (ksprm)
         if (lprc == 0) nndt = nndt + nndtp
         lprc = lprc + 1
         nndt = nndt + nndtp
      case (ksifs)
         nndt = nndt + nndtp
      case (ksifm)
         nndt = nndt + nndtp
      case (ksfrs)
         nndt = nndt + nndtp
      case (kswhs)
         nndt = nndt + nndtp
      case (kswhm)
         nndt = nndt + nndtp
      case (ksdos)
         nndt = nndt + nndtp
      case (ksnts)
         nndt = nndt + nndtp
      case (kssls)
         nndt = nndt + nndtp
      case (ksslm)
         nndt = nndt + nndtp
      case (kstys)
         nndt = nndt + nndtp
      end select
      return
end subroutine outstt
subroutine chgcas (zstr, kccas)
!  Case change
use fpprprms
character (len=*), intent (inout) :: zstr
! ____________________________________________________________________
!
      lstr = len_trim (zstr)
      select case (kccas)
      case (kcupr)
         do istr = 1, lstr
            irnk  = index (zlwr, zstr (istr:istr))
            if (irnk > 0) zstr (istr:istr) = zupr (irnk:irnk)
         enddo
      case (kclwr)
         do istr = 1, lstr
            irnk  = index (zupr, zstr (istr:istr))
            if (irnk > 0) zstr (istr:istr) = zlwr (irnk:irnk)
         enddo
      end select
end subroutine chgcas
subroutine realin (lurea, zlin, kllin)
!  Read a line, with advance buffering, for fixed form, in order
!  to detect continued lines
use flexvars
use fpprcurs
integer, intent (in)                :: lurea
character (len=*), intent (out)     :: zlin  ! The line
integer, intent (out)               :: kllin ! line type code
! ____________________________________________________________________
      character (len=*), parameter  :: zcmt1 = "Cc!Xx*"
      character (len=1), parameter  :: zcr = achar (13)
      integer, parameter            :: icntf = 6
      integer, save                 :: nhavm = 0
!
!  If don't have, read line
!
      if (nhav == 0) then
         read (lurea, "(a)", iostat=krea) zlinb
!
         if (krea /= 0) then
            kllin = klunv
            return
         else
            nhav = nhav + 1
            klnxt = klnrm
         endif
         llinw = len_trim (zlinb)
!
!  Remove trailing <CR> if any
!
         if (llinw > 0) then
            if (zlinb (llinw:llinw) == zcr) then
               zlinb (llinw:llinw) = ' '
               llinw = llinw - 1
            endif
         endif
!
!  If fixed form, find out if line is comment
!
         if (iffxd /= 0) then
!
!  Recognize blank comments
!
            if (llinw == 0) then
               zlinb (1:1) = '!'
               llinw = 1
               klnxt = klcmt
!
!  And other comments
!
            else
               if (index (zcmt1, zlinb (1:1)) /= 0) then
                  zlinb (1:1) = '!'
!
!  Do not skip "False comments"
!
                  klnxt = klcmt
                  do icmti = 1, ncmti
                     lcmti = len_trim (zcmtit (icmti))
                     if (llinw > lcmti .and. &
                         zlinb (1:lcmti) == zcmtit (icmti)(1:lcmti)) then
                        klnxt = klfcm
                        exit
                     endif
                  enddo
               endif
            endif
!
!  Handle past column 72 parts
!
            if (klnxt /= klcmt .and. llinw > 72) then
               zlinb = zlinb (1:72)
            endif
         endif
      endif
!
!  Provide requested line
!
      if (nhav == 1) then
         if (iffxd == 0) then
            zlin = trim (zlinb)
         else
            zlin = zlinb
         endif
         kllin = klnxt
      elseif (nhav > 1) then
         zlin = trim (zbufc (1))
         kllin = klcmt
         do ihav = 1, nhav - 2
            zbufc (ihav) = zbufc (ihav+1)
         enddo
      else
         kllin = klunv
         return
      endif
      nhav = nhav - 1
      nlinit (iclev) = nlinit (iclev) + 1
!
!  If necessary, read lines in advance
!
      if (nhav == 0) then
rdlin:   do
            read (lurea, "(a)", iostat=krea) zlinb
!
            select case (krea)
            case (1:)
               if (nhav == 0) then
                  if (klnxt == klcmt) then
                     kllin = kltcm
                  else
                     kllin = kllst
                  endif
               endif
               call fpperr ("Problem reading next line")
               return
            case (:-1)
               if (nhav == 0) then
                  if (klnxt == klcmt) then
                     kllin = kltcm
                  else
                     kllin = kllst
                  endif
               endif
               return
            case (0)
               nhav = nhav + 1
               llinw = len_trim (zlinb)
!
!  Remove trailing <CR> if any
!
               if (llinw > 0) then
                  if (zlinb (llinw:llinw) == zcr) then
                     zlinb (llinw:llinw) = ' '
                     llinw = llinw - 1
                  endif
               endif
!
!  If fixed form,
!
               if (iffxd /= 0) then
!
!                find out if next line is comment
!
                  klnxt = klnrm
!
!  Recognize blank comments
!
                  if (llinw == 0) then
                      klnxt = klcmt
                      zlinb (1:1) = '!'
                      llinw = 1
                  elseif (verify (zlinb (1:llinw), ztab // " ") == 0) &
                         then
                      klnxt = klcmt
                      zlinb (1:1) = '!'
                      llinw = 1
!
!  And other comments
!
                  else
                     if (index (zcmt1, zlinb (1:1)) /= 0) then
                        zlinb (1:1) = '!'
!
!  Exclude "False comments"
!
                        klnxt = klcmt
                        do icmti = 1, ncmti
                           lcmti = len_trim (zcmtit (icmti))
                           if (llinw > lcmti .and. &
                               zlinb (1:lcmti) ==  &
                               zcmtit (icmti)(1:lcmti)) then
                              klnxt = klfcm
                              exit rdlin
                           endif
                        enddo
                     endif
                  endif
!
!  Store comments for future use
!
                  if (klnxt == klcmt) then
                     if (nhav > nhavm) then
                        if (nhavm == 0) then
                           nhavm = 16
                           call inizbu (nhavm)
                        else
                           call xpdzbu (nhavm)
                        endif 
                     endif
                     zbufc (nhav) (1:linem) = zlinb (1:llinw)
                     cycle rdlin
                  endif
!
!  Check for continuation mark
!
!
!  Handle past column 72 parts
!
                  if (llinw > 72) then
                     zlinb = zlinb (1:72)
                  endif
                  klnxt = klnrm
                  ilinw = verify (zlinb (1:llinw), ztab // " ")
!
!  Exclude Pre-processing commands, which may start column 1
!
                  if (ilinw /= 6 .and. &
                      (zlinb (ilinw:ilinw) == "$" .or. &
                       zlinb (ilinw:ilinw) == "#")) then
                     exit rdlin
                  endif
!
!  Handle TABs as ^<TAB>non-zero-digit = continuation
!
                  if (llinw > 2) then
                     if (zlinb (1:1) == ztab) then

                        if (index ("123456789", zlinb (2:2)) /= 0) then
                           kllin = klctd
                           zlinb = zlinb (3:llinw)
                           exit rdlin
                        endif   
                     elseif (llinw > icntf) then
                        ilinw = verify (zlinb (1:llinw), &
                                        ztab // " 123456789")
                        if (ilinw >= icntf              .and. &
                            zlinb (icntf:icntf) /= ztab .and. &
                            zlinb (icntf:icntf) /= ' ') then
                           kllin = klctd
                           zlinb = zlinb (icntf+1:llinw)
                           exit rdlin
                        endif
                     else
                        cycle rdlin
                     endif
                  endif
               endif
               exit rdlin
            end select
         enddo rdlin
      endif
      return
end subroutine realin
subroutine inixpr
!  Initialize pointers to expression operators
use fprsvars
use fxprprms
! ____________________________________________________________________
!
      iwand = iwnam (zoand)
      iwequ = iwnam (zoequ)
      iwgeq = iwnam (zogeq)
      iwgth = iwnam (zogth)
      iwleq = iwnam (zoleq)
      iwlth = iwnam (zolth)
      iwneq = iwnam (zoneq)
      iwnot = iwnam (zonot)
      iwori = iwnam (zoori)
!
      iwtru = iwnam (zotru)
      iwfls = iwnam (zofls)
!
      iwint = iwnam (zoint)
      iwnin = iwnam (zonin)
      iwsin = iwnam (zosin)
      iwcos = iwnam (zocos)
      iwtan = iwnam (zotan)
      iwatn = iwnam (zoatn)
      iwlog = iwnam (zolog)
      iwexp = iwnam (zoexp)
      iwl10 = iwnam (zol10)
      iwsqr = iwnam (zosqr)
      iwmod = iwnam (zomod)
      iwmax = iwnam (zomax)
      iwmin = iwnam (zomin)
      iwat2 = iwnam (zoat2)
      iwasn = iwnam (zoasn)
      iwacs = iwnam (zoacs)
      iwsnh = iwnam (zosnh)
      iwcsh = iwnam (zocsh)
      iwtnh = iwnam (zotnh)
      iwabs = iwnam (zoabs)
      iwknd = iwnam (zoknd)
      iwsik = iwnam (zosik)
      iwsrk = iwnam (zosrk)
!
      iwfcm = iwnam (zofcm)
      iwcsk = iwnam (zocsk)
      iwcsu = iwnam (zocsu)
      iwfxi = iwnam (zofxi)
      iwfxo = iwnam (zofxo)
      iwsed = iwnam (zosed)
      iwmll = iwnam (zomll)
      iwids = iwnam (zoids)
      iwlnb = iwnam (zolnb)
!
      iwlve = iwnam (zalve)
      iwlwr = iwnam (zalwr)
      iwupr = iwnam (zaupr)
!
      return
contains
      integer function iwnam (znam)
      character (len=*), intent (in) :: znam
! ____________________________________________________________________
      interface
         logical function ifsame (zstr1, zstr2)
!        Case insensitive compare
         character (len=*), intent (in) :: zstr1, zstr2
         end function ifsame
      end interface
      lnam = len_trim (znam)
      ihsh = khshstr (znam (1:lnam))
      if (tnamt(ihsh)%kwnam == 0) then
         iwnam = 0
      else
         do
            inamd = tnamt(ihsh)%inamd
            inamf = tnamt(ihsh)%inamf
            if (ifsame (znam (1:lnam), znamg (inamd:inamf))) then
               iwnam = ihsh
               exit
            endif
            if (tnamt(ihsh)%ihshf == 0) then
               iwnam = 0
               exit
            else
               ihsh = tnamt(ihsh)%ihshf
            endif
         enddo
      endif
      end function iwnam
end subroutine inixpr
subroutine inicmd
!  Initialize pointers to pre-processor commands
use fprsvars
! ____________________________________________________________________
!
      iwdol = iwnam (zadol)
      iwdef = iwnam (zadef)
      iweli = iwnam (zaeli)
      iwels = iwnam (zaels)
      iwend = iwnam (zaend)
      iwevl = iwnam (zaevl)
      iwifx = iwnam (zaifx)
      iwifd = iwnam (zaifd)
      iwifn = iwnam (zaifn)
      iwinc = iwnam (zainc)
      iwmcr = iwnam (zamcr)
      iwund = iwnam (zaund)
!
      return
contains
      integer function iwnam (znam)
      character (len=*), intent (in) :: znam
! ____________________________________________________________________
      interface
         logical function ifsame (zstr1, zstr2)
!        Case insensitive compare
         character (len=*), intent (in) :: zstr1, zstr2
         end function ifsame
      end interface
      lnam = len_trim (znam)
      ihsh = khshstr (znam (1:lnam))
      if (tnamt(ihsh)%kwnam == 0) then
         iwnam = 0
      else
         do
            inamd = tnamt(ihsh)%inamd
            inamf = tnamt(ihsh)%inamf
            if (ifsame (znam (1:lnam), znamg (inamd:inamf))) then
               iwnam = ihsh
               exit
            endif
            if (tnamt(ihsh)%ihshf == 0) then
               iwnam = 0
               exit
            else
               ihsh = tnamt(ihsh)%ihshf
            endif
         enddo
      endif
      end function iwnam
end subroutine inicmd
subroutine trtcmd (iwnam, ntok)
!  Process pre-processor command
use fprsvars ! uses fprsprms
use flexvars ! uses flexprms
use fxprvars ! uses fxprprms
use fpprcurs ! uses fpprprms
integer, intent (in) :: iwnam
integer, intent (in) :: ntok
! ____________________________________________________________________
!
      integer, save :: iflev  = 0
      integer, save :: iflevr = 0
      integer       :: lzfic
      integer       :: inamod
      integer       :: inamof
      integer, save, dimension (nnsttm) :: ifdont = 0
      integer, dimension (nargm) :: inamat
      character (len=1), parameter :: zdlm = achar (1)
      character (len=2*lsttm) :: zsttw
      character (len=lsttm)   :: ztokw
      character (len=linem)   :: zrepw
      character (len=2)       :: znum
      character (len=lzficm)  :: zfic
      logical                 :: ifevl
!
body: do
!
!   $DEFINE
!
         if (iwnam == iwdef) then
            if (ifskp /= 0) exit body
            itok = 2
            if (itok > ntok) exit body
            itokd = itokdt (itok)
            itokf = itokft (itok)
            if (kktokt (itok) == kkidf) then
               call tstidf (ztoki (itokd:itokf), kcukn, krukn, kwtok, &
                            inam)
!
!   pre-defined keyword
!
               if (tnamt(inam)%kwnam == kwpdn .or. &
                   tnamt(inam)%kwnam == kwpds) then
!
!   False comments
!
                  if (inam == iwfcm) then
                     if (ncmti == ncmtim) then
                        call fpperr ("Too many false comments")
                        exit body
                     endif
                     itok = 3
                     if (ntok < itok) then
                        call fpperr ("Missing comment symbol")
                        exit body
                     endif
                     if (ntok == 4 .and. kktokt (itok) == kkcmd  &
                     .and. itokdt (itok) == itokft (itok)        &
                     .and. ztoki(itokdt(ntok)+1:itokdt(ntok)+1) == "!" &
                     .and. kktokt (ntok) == kkstr                ) then
                        itokd = itokdt (ntok) + 1
                        itokf = itokft (ntok) - 1
                     elseif (kktokt (itok) == kkcmt .or. &
                             kktokt (itok) == kkebc) then
                        itokd = itokdt (itok)
                        itokf = itokft (itok)
                     else
                        call fpperr ("Symbol is not comment")
                        exit body
                     endif
                     ncmti = ncmti + 1
                     zcmtit (ncmti) = ztoki (itokd:itokf)
!
!   Case processing
!
                  elseif (inam == iwcsk .or. inam == iwcsu) then
                     itok = 3
                     if (ntok < itok) then
                        kccur = kclve
                        exit body
                     endif
                     if (kktokt (itok) == kkidf) then
                        itokd = itokdt (itok)
                        itokf = itokft (itok)
                        call tstidf (ztoki (itokd:itokf), kcukn, krukn, &
                                     kwtok, inam1)
                        if (inam1 == iwlve) then
                           kccur = kclve
                        elseif (inam1 == iwlwr) then
                           kccur = kclwr
                        elseif (inam1 == iwupr) then
                           kccur = kcupr
                        else
                           call lexxpr (itok, ntok, kerr)
                           if (kerr /= 0) exit body
                           call evlxpr (ifevl, kerr)
                           if (kerr /= 0) exit body
                           kccur = nint (oxptt(1)%dval)
                           if (kccur /= kclve .and. &
                               kccur /= kclwr .and. &
                               kccur /= kcupr ) then
                              call fpperr ("Not a case definition")
                              exit body
                           endif
                        endif
                     else
                        call lexxpr (itok, ntok, kerr)
                        if (kerr /= 0) exit body
                        call evlxpr (ifevl, kerr)
                        if (kerr /= 0) exit body
                        kccur = nint (oxptt(1)%dval)
                        if (kccur /= kclve .and. &
                            kccur /= kclwr .and. &
                            kccur /= kcupr ) then
                           call fpperr ("Not a case definition")
                           exit body
                        endif
                     endif
                     if (inam == iwcsk) then
                        kccask = kccur
                     elseif (inam == iwcsu) then
                        kccasu = kccur
                     endif
!
!   Line length for splitting
!
                  elseif (inam == iwmll) then
                     itok = 3
                     if (ntok < itok) then
                        exit body
                     endif
                     call lexxpr (itok, ntok, kerr)
                     if (kerr /= 0) exit body
                     call evlxpr (ifevl, kerr)
                     if (kerr /= 0) exit body
                     jval = nint (oxptt(1)%dval)
                     if (jval < 2 .or. jval > linem ) then
                        call fpperr ("Not an allowed line length")
                        exit body
                     endif
                     linel = jval
!
!   step for indentation
!
                  elseif (inam == iwids) then
                     itok = 3
                     if (ntok < itok) then
                        exit body
                     endif
                     call lexxpr (itok, ntok, kerr)
                     if (kerr /= 0) exit body
                     call evlxpr (ifevl, kerr)
                     if (kerr /= 0) exit body
                     jval = nint (oxptt(1)%dval)
                     if (jval < 0 .or. jval > linem/2 ) then
                        call fpperr ("Not an allowed indentation step")
                        exit body
                     endif
                     nndtp = jval
!
!   Output line numbering information
!
                  elseif (inam == iwlnb) then
                     itok = 3
                     if (ntok < itok) then
                        iflnb = 1
                        exit body
                     else
                        call lexxpr (itok, ntok, kerr)
                        if (kerr /= 0) exit body
                        call evlxpr (ifevl, kerr)
                        if (kerr /= 0) exit body
                        if (ifevl) then
                           iflnb = 1
                        else
                           iflnb = 0
                        endif
                     endif
!
!   Input in Free or Fixed form
!
                  elseif (inam == iwfxi) then
                     itok = 3
                     if (ntok < itok) then
                        iffxd = 1
                        exit body
                     else
                        call lexxpr (itok, ntok, kerr)
                        if (kerr /= 0) exit body
                        call evlxpr (ifevl, kerr)
                        if (kerr /= 0) exit body
                        if (ifevl) then
                           iffxd = 1
                        else
                           iffxd = 0
                        endif
                     endif
!
!   Output in Free or Fixed form
!
                  elseif (inam == iwfxo) then
                     itok = 3
                     if (ntok < itok) then
                        iffxf = 1
                        exit body
                     else
                        call lexxpr (itok, ntok, kerr)
                        if (kerr /= 0) exit body
                        call evlxpr (ifevl, kerr)
                        if (kerr /= 0) exit body
                        if (ifevl) then
                           iffxf = 1
                        else
                           iffxf = 0
                        endif
                     endif
!
!   Treat Sharp as dollar
!
                  elseif (inam == iwsed) then
                     itok = 3
                     if (ntok < itok) then
                        ifsed = 1
                        exit body
                     else
                        call lexxpr (itok, ntok, kerr)
                        if (kerr /= 0) exit body
                        call evlxpr (ifevl, kerr)
                        if (kerr /= 0) exit body
                        if (ifevl) then
                           ifsed = 1
                        else
                           ifsed = 0
                        endif
                     endif
                  endif
                  exit body
               endif
               tnamt(inam)%kwnam = kwdfd
!
!   $"xxxxx" (not to be analysed), possibly multiple instruction
!
               itok = 3
               if (ntok == 4 .and. kktokt (itok) == kkcmd .and. &
                   itokdt (itok) == itokft (itok)         .and. &
                   kktokt (ntok) == kkstr) then
                  itokd = itokdt (ntok) + 1
                  itokf = itokft (ntok) - 1
                  lnam  = itokf - itokd + 1
                  inams = inamo + 1
                  inamo = inamo + lnam
                  if (inamo > nnamm*lnama*2) then
                     call fpperr ("insufficient name space&
                                      & (out. idf. names)")
                     call fpperr ("raise length of global&
                                      & chain and try again")
                     stop
                  endif
                  tnamt(inam)%inamod = inams
                  tnamt(inam)%inamof = inamo
                  znamo (inams:inamo) = ztoki (itokd:itokf)
!
!   expression (to be analysed)
!
               elseif (itok <= ntok) then
                  itokd = itokdt (itok)
                  itokf = itokft (ntok)
                  lnam  = itokf - itokd + 1
                  inams = inamo + 1
                  inamo = inamo + lnam
                  if (inamo > nnamm*lnama*2) then
                     call fpperr ("insufficient name space&
                                      & (out. idf. names)")
                     call fpperr ("raise length of global&
                                      & chain and try again")
                     stop
                  endif
                  ireps = irepg + 1
                  irepg = irepg + lnam
                  if (irepg > nrepgm) then
                     call fpperr ("insufficient name space&
                                      & (out. repl. names)")
                     call fpperr ("raise length of global&
                                      & chain and try again")
                     stop
                  endif
                  irepd = irep + 1
                  irep  = irep + (ntok - itok + 1)
                  if (irep > nrepm) then
                     call fpperr ("insufficient name space&
                                      & (out. repl. names)")
                     call fpperr ("raise # of repl. names&
                                      & and try again")
                     stop
                  endif
                  tnamt(inam)%irepc  = irepd
                  tnamt(inam)%inamod = inams
                  tnamt(inam)%inamof = inamo
                  znamo (inams:inamo) = ztoki (itokd:itokf)
                  zrepg (ireps:irepg) = ztoki (itokd:itokf)
                  itokn = itokft (ntok)
                  do itok1 = itok, ntok
                     itokd = itokdt (itok1)
                     itokf = itokft (itok1)
                     kkrept (irepd) = kktokt (itok1)
                     if (kkrept (irepd) == kkidf) then
                        if (itok1 == ntok) then
                           krtok = krany
                        else
                           select case (kktokt (itok1+1))
                           case (kkstr)
                              krtok = krstr
                           case (kkpou)
                              krtok = krpou
                           case default
                              krtok = krany
                           end select
                        endif
                        call tstidf (ztoki (itokd:itokf), kcany, krtok, &
                                     kwtok, irepwt (irepd))
                     endif
                     irepdt (irepd) = itokd + irepg - itokn
                     irepft (irepd) = itokf + irepg - itokn
                     irepnt (irepd) = irepd + 1
                     irepd = irepd + 1
                  enddo
                  irepnt (irep) = 0
               endif
               exit body
            else
               call fpperr ("Cannot redefine keyword" // &
                                ztoki (itokd:itokf))
               exit body
            endif
!
!   $UNDEF
!
         elseif (iwnam == iwund) then
            if (ifskp /= 0) exit body
            itok = 2
            if (itok > ntok) exit body
            itokd = itokdt (itok)
            itokf = itokft (itok)
            if (kktokt (itok) == kkidf) then
               call tstidf (ztoki (itokd:itokf), kcukn, krukn, kwtok, &
                            inam)
               if (tnamt(inam)%kwnam == kwdfd) then
                  lnam  = itokf - itokd + 1
                  inams = inamo + 1
                  inamo = inamo + lnam
                  if (inamo > nnamm*lnama*2) then
                     call fpperr ("insufficient name space&
                                      & (out. idf. names)")
                     call fpperr ("raise length of global&
                                      & chain and try again")
                     stop
                  endif
                  tnamt(inam)%kwnam  = kwvar
                  tnamt(inam)%inamod = inams
                  tnamt(inam)%inamof = inamo
                  znamo (inams:inamo) = ztoki (itokd:itokf)
               endif
               exit body
            else
               call fpperr ("Cannot undefine keyword" // &
                                ztoki (itokd:itokf))
               exit body
            endif
!
!   $EVAL
!
         elseif (iwnam == iwevl) then
            if (ifskp /= 0) exit body
            itok = 2
            if (itok > ntok) exit body
            itokd = itokdt (itok)
            itokf = itokft (itok)
            if (kktokt (itok) == kkidf) then
               call tstidf (ztoki (itokd:itokf), kcukn, krukn, kwtok, &
                            inam)
               itok = 3
               if (ntok < itok) then
                   zrepw = ".True."
               else
                   call lexxpr (itok, ntok, kerr)
                   if (kerr /= 0) exit body
                   call strxpr (zrepw, krep, kerr)
                   if (kerr /= 0) exit body
               endif
               tnamt(inam)%kwnam = kwdfd
!
               lnam  = len_trim (zrepw)
               inams = inamo + 1
               inamo = inamo + lnam
               if (inamo > nnamm*lnama*2) then
                     call fpperr ("insufficient name space&
                                      & (out. idf. names)")
                     call fpperr ("raise length of global&
                                      & chain and try again")
                     stop
               endif
               ireps = irepg + 1
               irepg = irepg + lnam
               if (irepg > nrepgm) then
                     call fpperr ("insufficient name space&
                                      & (out. repl. names)")
                     call fpperr ("raise length of global&
                                      & chain and try again")
                     stop
               endif
               irepd = irep + 1
               irep  = irep + 1
               if (irep > nrepm) then
                     call fpperr ("insufficient name space&
                                      & (out. repl. names)")
                     call fpperr ("raise # of repl. names&
                                      & and try again")
                     stop
               endif
               tnamt(inam)%irepc  = irepd
               tnamt(inam)%inamod = inams
               tnamt(inam)%inamof = inamo
               znamo (inams:inamo) = trim (zrepw)
               zrepg (ireps:irepg) = trim (zrepw)
               itokn = itokft (ntok)
               select case (krep)
               case (konul)
                  kkrept (irepd) = kklog
               case (konui)
                  kkrept (irepd) = kknui
               case (konuf)
                  kkrept (irepd) = kknuf
               case default
                  kkrept (irepd) = kkndf
               end select
               irepdt (irepd) = irepg + 1 - len_trim (zrepw)
               irepft (irepd) = irepg
               irepnt (irepd) = irepd + 1
               irepd = irepd + 1
               irepnt (irep) = 0
               exit body
            else
               call fpperr ("Cannot redefine keyword" // &
                                ztoki (itokd:itokf))
               exit body
            endif
!
!   $IF
!
         elseif (iwnam == iwifx) then
            itok = 2
            if (itok > ntok) exit body
            if (iflev >= nnsttm) then
               call fpperr ("Tests are too deeply nested")
               exit body
            endif
            iflev = iflev + 1
            ifdont (iflev) = -1
            if (ifskp == 0) then
               call lexxpr (itok, ntok, kerr)
               if (kerr /= 0) exit body
               call evlxpr (ifevl, kerr)
               if (kerr /= 0) exit body
               if (ifevl) then
                  ifskp = 0
                  ifdont (iflev) = 1
                  iflevr = iflev
                  exit body
               else
                  ifskp = 1
                  exit body
               endif
            endif
!
!   $IFDEF
!
         elseif (iwnam == iwifd) then
            itok = 2
            if (itok > ntok) exit body
            if (iflev >= nnsttm) then
               call fpperr ("Tests are too deeply nested")
               exit body
            endif
            iflev = iflev + 1
            ifdont (iflev) = -2
            if (ifskp == 0) then
               itokd = itokdt (itok)
               itokf = itokft (itok)
               if (kktokt (itok) == kkidf) then
                  call tstidf (ztoki (itokd:itokf), kcukn, krukn, &
                               kwtok, inam)
                  if (tnamt(inam)%kwnam == kwdfd) then
                     ifskp = 0
                     ifdont (iflev) = 2
                     iflevr = iflev
                  else
                     ifskp = 1
                  endif
                  exit body
               else
                  call fpperr ("Cannot test word " // ztoki(itokd:itokf))
                  exit body
               endif
            endif
!
!   $IFNDEF
!
         elseif (iwnam == iwifn) then
            itok = 2
            if (itok > ntok) exit body
            if (iflev >= nnsttm) then
               call fpperr ("Tests are too deeply nested")
               exit body
            endif
            iflev = iflev + 1
            ifdont (iflev) = -2
            if (ifskp == 0) then
               itokd = itokdt (itok)
               itokf = itokft (itok)
               if (kktokt (itok) == kkidf) then
                  call tstidf (ztoki (itokd:itokf), kcukn, krukn, &
                               kwtok, inam)
                  if (tnamt(inam)%kwnam /= kwdfd) then
                     ifskp = 0
                     ifdont (iflev) = 2
                     iflevr = iflev
                  else
                     ifskp = 1
                  endif
               else
                  call fpperr ("Cannot test word " // ztoki(itokd:itokf))
               endif
            endif
!
!   $ELSE
!
         elseif (iwnam == iwels) then
            if (iflevr == iflev - 1) then
               iflevr = iflevr + 1
               if (ifdont (iflevr) <= 0) then
                  ifskp = 0
                  ifdont (iflevr) = abs (ifdont (iflev))
               else
                  ifskp = 1
               endif
            elseif (iflevr == iflev) then
               iflevr = iflevr - 1
               ifskp  = 1
            endif
!
!   $ELIF
!
         elseif (iwnam == iweli) then
            itok = 2
            if (itok > ntok) exit body
            if (iflevr == iflev - 1) then
               iflevr = iflevr + 1
               if (ifdont (iflevr) == -1) then
                  call lexxpr (itok, ntok, kerr)
                  if (kerr /= 0) exit body
                  call evlxpr (ifevl, kerr)
                  if (kerr /= 0) exit body
                  if (ifevl) then
                     ifskp = 0
                     ifdont (iflevr) = 1
                  else
                     ifskp = 1
                     iflevr = iflevr -1
                  endif
               elseif (ifdont (iflevr) == -2) then
                  itokd = itokdt (itok)
                  itokf = itokft (itok)
                  if (kktokt (itok) == kkidf) then
                     call tstidf (ztoki (itokd:itokf), kcukn, krukn, &
                                  kwtok, inam)
                     if (tnamt(inam)%kwnam == kwdfd) then
                        ifskp = 0
                        ifdont (iflevr) = 2
                     else
                        ifskp = 1
                        iflevr = iflevr -1
                     endif
                  else
                     call fpperr ("Cannot test word " // ztoki(itokd:itokf))
                     exit body
                  endif
               else
                  ifskp = 1
               endif
            elseif (iflevr == iflev) then
               iflevr = iflevr - 1
               ifskp  = 1
            endif
!
!   $ENDIF
!
         elseif (iwnam == iwend) then
            iflev = iflev - 1
            if (iflevr >= iflev) then
               iflevr = iflev
               ifskp  = 0
            endif
!
!   $INCLUDE
!
         elseif (iwnam == iwinc) then
            if (ifskp /= 0) exit body
            itok = 2
            if (itok > ntok) exit body
            itokd = itokdt (itok)
            itokf = itokft (itok)
            select case (kktokt (itok))
               case (kkstr)
                  itokd = itokd + 1
                  itokf = itokf - 1
                  zfic = ztoki (itokd:itokf)
                  lzfic = itokf - itokd + 1
               case (kkidf)
                  call tstidf (ztoki(itokd:itokf), kcukn, krukn,  &
                               kwtok, inam)
                  if (tnamt(inam)%irepc /= 0) then
                     if (kkrept (tnamt(inam)%irepc) == kkstr) then
                        inamod = tnamt (inam)%inamod + 1
                        inamof = tnamt (inam)%inamof - 1
                        zfic = znamo (inamod:inamof)
                        lzfic = inamof - inamod + 1
                     else
                        call fpperr ("Include file name " // &
                                     ztoki (itokd:itokf) // &
                                     " must evaluate to string")
                        exit body
                     end if
                  else
                     call fpperr ("Include file name " // &
                                  ztoki (itokd:itokf) // &
                                  " is undefined")
                     exit body
                  end if
               case default
                  call fpperr ("Include file name must be string " // &
                               "or evaluate to string")
                  exit body
            end select
!
            lufic = lufic + 1
            open (lufic, file=zfic(1:lzfic), action="read", &
                  iostat=kerr)
            if (kerr /= 0) then
               call fpperr ("Unable to open include file: " // &
                                zfic (1:lzfic))
               close (lufic, iostat=kerr)
               lufic = lufic - 1
               exit body
            endif
            if (iclev < nnstim) then
               iclev = iclev + 1
            else
               call fpperr ("Include files too deeply nested")
               exit body
            endif
            islh = index (zfic(1:lzfic), '/', back=.true.)
            if (islh /= 0) then
               zfic = zfic (islh:lzfic)
               lzfic = lzfic + islh - 1
            endif
            zficit (iclev) = zfic (1:lzfic)
            nlinit (iclev) = 0
            zlinbh (iclev) = zlinb
            nhavh  (iclev) = nhav
            nhav = 0
            klnxth (iclev) = klnxt
            klrea = klnrm
            luinp = lufic
!
!   $MACRO
!
         elseif (iwnam == iwmcr) then
            if (ifskp /= 0) exit body
            if (ntok < 6) exit body
!
!   Analyse Macro to check that it is valid
!
            itok0 = 2
maca:       do
               if (kktokt (itok0) /= kkidf .or. &
                   kktokt (itok0+1) /= kkpou) then
                  ifvld = 0
                  exit maca
               endif
               narg = 1
               do itok = itok0 + 3, ntok, 2
                  if (kktokt (itok-1) /= kkidf) then
                     ifvld = 0
                     exit maca
                  else
                     itokd = itokdt (itok-1)
                     itokf = itokft (itok-1)
                     call tstidf (ztoki (itokd:itokf), kcukn, krukn, &
                                  kwtok, inam)
                     inamat (narg) = inam
                  endif
                  if (kktokt (itok) == kksep) then
                     narg = narg + 1
                     cycle
                  elseif (kktokt (itok) == kkpfr) then
                     ifvld = 1
                     exit maca
                  else
                     ifvld = 0
                     exit maca
                  endif
               enddo
               ifvld = 0
               exit maca
            enddo maca
            itokd = itokdt (itok0)
            itokf = itokft (itok0)
            if (ifvld /= 0) then
               call tstidf (ztoki (itokd:itokf), kcukn, krukn, kwtok, &
                            inam)
               tnamt(inam)%kwnam = kwmcrt (narg)
               tnamt(inam)%irepc = irep + 1
               if (kktokt (ntok) == kkcmt .or. &
                   kktokt (ntok) == kkebc) then
                  ntok1 = ntok - 1
               else
                  ntok1 = ntok
               endif
               itok = itok + 1
!
!   $"xxxxx" (not to be analysed), possibly multiple instruction
!
               if (ntok1 == itok+1 .and. kktokt (itok) == kkcmd .and. &
                   itokdt (itok) == itokft (itok)         .and. &
                   kktokt (ntok1) == kkstr) then
                  itokd = itokdt (ntok1) + 1
                  itokf = itokft (ntok1) - 1
                  lsttw = itokf - itokd + 4
!
!   Use non-printable char as delim
!
                  zsttw (1:lsttw) = zdlm // " " // &
                                    ztoki (itokd:itokf) // zdlm
                  do iarg = 1, narg
                     inam = inamat (iarg)
                     inamd = tnamt(inam)%inamd
                     inamf = tnamt(inam)%inamf
                     lnam  = inamf - inamd + 1
                     write (znum, "(i2)") iarg
                     do
                        iind = index (zsttw(1:lsttw), znamg(inamd:inamf))
                        if (iind == 0) then
                           exit
                        else
                           lsttw1 = lsttw - lnam + 4
                           zsttw (1:lsttw1) = zsttw (1:iind-1) //     &
                                              zdlm // znum // zdlm // &
                                              zsttw (iind+lnam:lsttw)
                           lsttw = lsttw1
                        endif
                     enddo
                  enddo
!
!   zsttw is now "xxx"nn"xxxxx"nn"xxxxx". Build token chain
!
                  kktok = kkukn
                  ltokw = 0
                  istt  = 2
                  do
                     if (istt > lsttw) then
                        exit
                     endif
                     if (zsttw (istt:istt) == zdlm .and. &
                         kktok == kkukn) then
                        if (ltokw /= 0) then
                           ireps = irepg + 1
                           irepg = irepg + ltokw
                           if (irepg > nrepgm) then
                              call fpperr ("insufficient name space&
                                               & (out. repl. names)")
                              call fpperr ("raise length of global&
                                               & chain and try again")
                              stop
                           endif
                           irep  = irep + 1
                           if (irep > nrepm) then
                              call fpperr ("insufficient name space&
                                               & (out. repl. names)")
                              call fpperr ("raise # of repl. names&
                                               & and try again")
                              stop
                           endif
                           zrepg (ireps:irepg) = ztokw (1:ltokw)
                           kkrept (irep) = kktok
                           irepdt (irep) = ireps
                           irepft (irep) = irepg
                           irepnt (irep) = irep + 1
                        endif
                        kktok = kkar0
                        ltokw = 0
                     elseif (zsttw (istt:istt) == zdlm .and. &
                             kktok == kkar0) then
                        irep  = irep + 1
                        if (irep > nrepm) then
                           call fpperr ("insufficient name space&
                                            & (out. repl. names)")
                           call fpperr ("raise # of repl. names&
                                            & and try again")
                           stop
                        endif
                        read (ztokw (1:ltokw), "(i2)") iarg
                        kkrept (irep) = kkargt (iarg)
                        irepdt (irep) = 0
                        irepft (irep) = 0
                        irepnt (irep) = irep + 1
                        kktok = kkukn
                        ltokw = 0
                     else
                        ltokw = ltokw + 1
                        ztokw (ltokw:ltokw) = zsttw (istt:istt)
                     endif
                     istt = istt + 1
                  enddo
                  irepnt (irep) = 0
!
!   expression (to be analysed)
!
               elseif (itok <= ntok1) then
                  itokd = itokdt (itok)
                  itokf = itokft (ntok1)
                  lnam  = itokf - itokd + 1
                  inams = inamo + 1
                  inamo = inamo + lnam
                  if (inamo > nnamm*lnama*2) then
                     call fpperr ("insufficient name space&
                                      & (out. idf. names)")
                     call fpperr ("raise length of global&
                                      & chain and try again")
                     stop
                  endif
                  ireps = irepg + 1
                  irepg = irepg + lnam
                  if (irepg > nrepgm) then
                     call fpperr ("insufficient name space&
                                      & (out. repl. names)")
                     call fpperr ("raise length of global&
                                      & chain and try again")
                     stop
                  endif
                  irepd = irep + 1
                  irep  = irep + (ntok1 - itok + 1)
                  if (irep > nrepm) then
                     call fpperr ("insufficient name space&
                                      & (out. repl. names)")
                     call fpperr ("raise # of repl. names&
                                      & and try again")
                     stop
                  endif
                  tnamt(inam)%irepc  = irepd
                  tnamt(inam)%inamod = inams
                  tnamt(inam)%inamof = inamo
                  znamo (inams:inamo) = ztoki (itokd:itokf)
                  zrepg (ireps:irepg) = ztoki (itokd:itokf)
                  itokn = itokft (ntok1)
                  do itok1 = itok, ntok1
                     itokd = itokdt (itok1)
                     itokf = itokft (itok1)
                     kkrept (irepd) = kktokt (itok1)
                     if (kkrept (irepd) == kkidf) then
                        call tstidf (ztoki (itokd:itokf), kcukn, krukn, &
                                     kwtok, irepwt (irepd))
                        do iarg = 1, narg
                           if (irepwt (irepd) == inamat (iarg)) then
                              kkrept (irepd) = kkargt (iarg)
                              exit
                           endif
                        enddo
                     endif
                     irepdt (irepd) = itokd + irepg - itokn
                     irepft (irepd) = itokf + irepg - itokn
                     irepnt (irepd) = irepd + 1
                     irepd = irepd + 1
                  enddo
                  irepnt (irep) = 0
               endif
               exit body
            else
               call fpperr ("Illegal macro expression for " // &
                                ztoki (itokd:itokf))
               exit body
            endif
!
         endif
         exit body
      enddo body
      return
end subroutine trtcmd
subroutine lexxpr (itok0, ntok, kerr)
!  prepare lexed statement for analysis as an expression
use flexvars
use fxprvars
use fprsvars
use fpprcurs
integer, intent (in) :: itok0
integer, intent (in) :: ntok
! ____________________________________________________________________
!
      double precision            :: dxpt
      integer, dimension (nnstdm) :: irepst
      character (len=lsttm)       :: ztokw
!
      kerr = 0
      instr = 0
      itok  = itok0 - 1
      ixptg = 0
      ixpt  = 0
      do
         if (instr == 0) then
            itok = itok + 1
            if (itok > ntok) exit
            itokd = itokdt (itok)
            itokf = itokft (itok)
            kktok = kktokt (itok)
            if (kktok == kkidf) then
               call tstidf (ztoki (itokd:itokf), kcany, krany, kwtok, &
                            inam)
               kwnam = tnamt(inam)%kwnam
               if (kwnam == kwdfd .and. tnamt(inam)%irepc /= 0) then
                  instr = 1
                  irepd = tnamt(inam)%irepc
                  cycle
               elseif (kwnam >= kwmc0 .and. tnamt(inam)%irepc /= 0) &
                      then
!
!  Analyse macro arguments
!
                  narge = kwnam - kwmc0
                  narg  = 1
                  itok1 = itok + 1
maca:             do
                     if (ntok < itok1 + 2*narge) then
                        ifvld = 0
                        exit maca
                     endif
                     if (kktokt (itok1) /= kkpou) then
                        ifvld = 0
                        exit maca
                     endif
                     lpar  = 1
                     do
                        itok1 = itok1 + 1
                        if (itok1 > ntok) then
                           ifvld = 0
                           exit maca
                        endif
!
!  Add token to current arg list
!
                        itokd = itokdt (itok1)
                        itokf = itokft (itok1)
                        ltokw = itokf - itokd + 1
                        ireps = irepg + 1
                        irepg = irepg + ltokw
                        if (irepg > nrepgm) then
                           call fpperr ("insufficient name space&
                                            & (out. repl. names)")
                           call fpperr ("raise length of global&
                                            & chain and try again")
                           stop
                        endif
                        irep = irep + 1
                        if (irep > nrepm) then
                           call fpperr ("insufficient name space&
                                            & (out. repl. names)")
                           call fpperr ("raise # of repl. names&
                                            & and try again")
                           stop
                        endif
                        kkrept (irep) = kktokt (itok1)
                        irepdt (irep) = ireps
                        irepft (irep) = irepg
                        irepnt (irep) = irep + 1
                        zrepg (ireps:irepg) = ztoki (itokd:itokf)
!
! Check for next argument or last one
!
                        select case (kktokt (itok1))
                        case (kkpfr)
                           lpar = lpar - 1
                           if (lpar == 0) then
                              if (narg == narge) then
                                 irep = irep - 1
                                 irepnt (irep) = 0
                                 itok = itok1
                                 ifvld = 1
                              else
                                 ifvld = 0
                              endif
                              exit maca
                           endif
                        case (kkpou)
                           lpar = lpar + 1
                        case (kksep)
                           if (lpar == 1) then
                              irep = irep - 1
                              irepnt (irep) = 0
                              narg = narg + 1
                           endif
                        case default
                           continue
                        end select
                     enddo
                  enddo maca
!
! If it was legal macro, apply it
!
                  if (ifvld /= 0) then
                     instr = 1
                     irepd = tnamt(inam)%irepc
                     cycle
                  endif
               else
                  itokd = tnamt(inam)%inamod
                  itokf = tnamt(inam)%inamof
                  ltokw = itokf - itokd + 1
                  ztokw (1:ltokw) = znamo (itokd:itokf)
               endif
            else
               ltokw = itokf - itokd + 1
               ztokw (1:ltokw) = ztoki (itokd:itokf)
            endif
         else
            if (irepd == 0) then
               instr = instr - 1
               if (instr /= 0) then
                  irepd = irepst (instr)
               endif
               cycle
            endif
            itokd = irepdt (irepd)
            itokf = irepft (irepd)
            ltokw = itokf - itokd + 1
            ztokw (1:ltokw) = zrepg (itokd:itokf)
            kktok = kkrept (irepd)
            if (kktok == kkidf) then
               inam  = irepwt (irepd)
               kwnam = tnamt(inam)%kwnam
               if (kwnam == kwdfd .and. tnamt(inam)%irepc /= 0) then
                  irepst (instr) = irepnt (irepd)
                  instr = instr + 1
                  if (instr > nnstdm) then
                  call fpperr ("defines nesting overflow")
                  exit
                  endif
                  irepd = tnamt(inam)%irepc
                  cycle
               else
                  itokd = tnamt(inam)%inamod
                  itokf = tnamt(inam)%inamof
                  ltokw = itokf - itokd + 1
                  ztokw (1:ltokw) = znamo (itokd:itokf)
               endif
            endif
            irepd = irepnt (irepd)
         endif
         if (ltokw + ixptg > nxptgm) then
            call fpperr ("Substitution leads to expression length&
                        & overflow")
            exit
         endif
         select case (kktok)
         case default
            kerr = 1
            call fpperr ("term not allowed in expression: " // &
                         ztokw (1:ltokw))
            exit
         case (kkcmt, kkebc)   ! Comment string
            exit
!
!  Constants
!
         case (kknui)   ! Integer Numerical value
            ixpt = ixpt + 1
            read (ztokw (1:ltokw), *, iostat=kerr) dxpt
            if (kerr /= 0) then
               call fpperr ("term not allowed in expression: " // &
                            ztokw (1:ltokw))
               exit
            endif
            oxptt (ixpt) = opropd (dxpt, konui, ixpt-1, ixpt+1)
         case (kknuf)   ! Real Numerical value
            ixpt = ixpt + 1
            read (ztokw (1:ltokw), *, iostat=kerr) dxpt
            if (kerr /= 0) then
               call fpperr ("term not allowed in expression: " // &
                            ztokw (1:ltokw))
               exit
            endif
            oxptt (ixpt) = opropd (dxpt, konuf, ixpt-1, ixpt+1)
         case (kkidf)   ! Identifier
!
!  Old form comparisons
!
            if (kwnam == kwlop) then
               if (inam == iwand) then
                  ixpt = ixpt + 1
                  oxptt (ixpt) = opropd (0.0d0, koand, ixpt-1, ixpt+1)
               elseif (inam == iwequ) then
                  ixpt = ixpt + 1
                  oxptt (ixpt) = opropd (0.0d0, koequ, ixpt-1, ixpt+1)
               elseif (inam == iwgeq) then
                  ixpt = ixpt + 1
                  oxptt (ixpt) = opropd (0.0d0, kogeq, ixpt-1, ixpt+1)
               elseif (inam == iwgth) then
                  ixpt = ixpt + 1
                  oxptt (ixpt) = opropd (0.0d0, kogth, ixpt-1, ixpt+1)
               elseif (inam == iwleq) then
                  ixpt = ixpt + 1
                  oxptt (ixpt) = opropd (0.0d0, koleq, ixpt-1, ixpt+1)
               elseif (inam == iwlth) then
                  ixpt = ixpt + 1
                  oxptt (ixpt) = opropd (0.0d0, kolth, ixpt-1, ixpt+1)
               elseif (inam == iwneq) then
                  ixpt = ixpt + 1
                  oxptt (ixpt) = opropd (0.0d0, koneq, ixpt-1, ixpt+1)
               elseif (inam == iwnot) then
                  ixpt = ixpt + 1
                  oxptt (ixpt) = opropd (0.0d0, konot, ixpt-1, ixpt+1)
               elseif (inam == iwori) then
                  ixpt = ixpt + 1
                  oxptt (ixpt) = opropd (0.0d0, koori, ixpt-1, ixpt+1)
               else
                  kerr = 1
                  call fpperr ("term not allowed in expression: " // &
                               ztokw (1:ltokw))
                  exit
               endif
!
!  Logical constants
!
            elseif (kwnam == kwlct) then
               if (inam == iwtru) then
                  ixpt = ixpt + 1
                  dxpt = 1.0D0
                  oxptt (ixpt) = opropd (dxpt, konui, ixpt-1, ixpt+1)
               elseif (inam == iwfls) then
                  ixpt = ixpt + 1
                  dxpt = 0.0D0
                  oxptt (ixpt) = opropd (dxpt, konui, ixpt-1, ixpt+1)
               else
                  kerr = 1
                  call fpperr ("term not allowed in expression: " // &
                               ztokw (1:ltokw))
                  exit
               endif
!
!  Known intrinsics
!
            elseif (kwnam == kwntr) then
               if (inam == iwint) then
                  ixpt = ixpt + 1
                  oxptt (ixpt) = opropd (0.0d0, koint, ixpt-1, ixpt+1)
               elseif (inam == iwnin) then
                  ixpt = ixpt + 1
                  oxptt (ixpt) = opropd (0.0d0, konin, ixpt-1, ixpt+1)
               elseif (inam == iwsin) then
                  ixpt = ixpt + 1
                  oxptt (ixpt) = opropd (0.0d0, kosin, ixpt-1, ixpt+1)
               elseif (inam == iwcos) then
                  ixpt = ixpt + 1
                  oxptt (ixpt) = opropd (0.0d0, kocos, ixpt-1, ixpt+1)
               elseif (inam == iwtan) then
                  ixpt = ixpt + 1
                  oxptt (ixpt) = opropd (0.0d0, kotan, ixpt-1, ixpt+1)
               elseif (inam == iwatn) then
                  ixpt = ixpt + 1
                  oxptt (ixpt) = opropd (0.0d0, koatn, ixpt-1, ixpt+1)
               elseif (inam == iwlog) then
                  ixpt = ixpt + 1
                  oxptt (ixpt) = opropd (0.0d0, kolog, ixpt-1, ixpt+1)
               elseif (inam == iwexp) then
                  ixpt = ixpt + 1
                  oxptt (ixpt) = opropd (0.0d0, koexp, ixpt-1, ixpt+1)
               elseif (inam == iwl10) then
                  ixpt = ixpt + 1
                  oxptt (ixpt) = opropd (0.0d0, kol10, ixpt-1, ixpt+1)
               elseif (inam == iwsqr) then
                  ixpt = ixpt + 1
                  oxptt (ixpt) = opropd (0.0d0, kosqr, ixpt-1, ixpt+1)
               elseif (inam == iwmod) then
                  ixpt = ixpt + 1
                  oxptt (ixpt) = opropd (0.0d0, komod, ixpt-1, ixpt+1)
               elseif (inam == iwmax) then
                  ixpt = ixpt + 1
                  oxptt (ixpt) = opropd (0.0d0, komax, ixpt-1, ixpt+1)
               elseif (inam == iwmin) then
                  ixpt = ixpt + 1
                  oxptt (ixpt) = opropd (0.0d0, komin, ixpt-1, ixpt+1)
               elseif (inam == iwat2) then
                  ixpt = ixpt + 1
                  oxptt (ixpt) = opropd (0.0d0, koat2, ixpt-1, ixpt+1)
               elseif (inam == iwasn) then
                  ixpt = ixpt + 1
                  oxptt (ixpt) = opropd (0.0d0, koasn, ixpt-1, ixpt+1)
               elseif (inam == iwacs) then
                  ixpt = ixpt + 1
                  oxptt (ixpt) = opropd (0.0d0, koacs, ixpt-1, ixpt+1)
               elseif (inam == iwsnh) then
                  ixpt = ixpt + 1
                  oxptt (ixpt) = opropd (0.0d0, kosnh, ixpt-1, ixpt+1)
               elseif (inam == iwcsh) then
                  ixpt = ixpt + 1
                  oxptt (ixpt) = opropd (0.0d0, kocsh, ixpt-1, ixpt+1)
               elseif (inam == iwtnh) then
                  ixpt = ixpt + 1
                  oxptt (ixpt) = opropd (0.0d0, kotnh, ixpt-1, ixpt+1)
               elseif (inam == iwabs) then
                  ixpt = ixpt + 1
                  oxptt (ixpt) = opropd (0.0d0, koabs, ixpt-1, ixpt+1)
               elseif (inam == iwknd) then
                  ixpt = ixpt + 1
                  oxptt (ixpt) = opropd (0.0d0, koknd, ixpt-1, ixpt+1)
               elseif (inam == iwsik) then
                  ixpt = ixpt + 1
                  oxptt (ixpt) = opropd (0.0d0, kosik, ixpt-1, ixpt+1)
               elseif (inam == iwsrk) then
                  ixpt = ixpt + 1
                  oxptt (ixpt) = opropd (0.0d0, kosrk, ixpt-1, ixpt+1)
               else
                  kerr = 1
                  call fpperr ("term not allowed in expression: " // &
                               ztokw (1:ltokw))
                  exit
               endif
            else
               kerr = 1
               call fpperr ("term not allowed in expression: " // &
                            ztokw (1:ltokw))
               exit
            endif
!
!  "Separators"
!
         case (kkpfr)   ! )
            ixpt = ixpt + 1
            oxptt (ixpt) = opropd (0.0d0, kopfr, ixpt-1, ixpt+1)
         case (kkpou)   ! (
            ixpt = ixpt + 1
            oxptt (ixpt) = opropd (0.0d0, kopou, ixpt-1, ixpt+1)
         case (kksep)   ! ,
            ixpt = ixpt + 1
            oxptt (ixpt) = opropd (0.0d0, kosep, ixpt-1, ixpt+1)
!
!  Comparisons
!
         case (kksup)   ! >
            ixpt = ixpt + 1
            oxptt (ixpt) = opropd (0.0d0, kogth, ixpt-1, ixpt+1)
         case (kkgeq)   ! >=
            ixpt = ixpt + 1
            oxptt (ixpt) = opropd (0.0d0, kogeq, ixpt-1, ixpt+1)
         case (kkneq)   ! /=
            ixpt = ixpt + 1
            oxptt (ixpt) = opropd (0.0d0, koneq, ixpt-1, ixpt+1)
         case (kkleq)   ! <=
            ixpt = ixpt + 1
            oxptt (ixpt) = opropd (0.0d0, koleq, ixpt-1, ixpt+1)
         case (kkinf)   ! <
            ixpt = ixpt + 1
            oxptt (ixpt) = opropd (0.0d0, kolth, ixpt-1, ixpt+1)
         case (kkequ)   ! ==
            ixpt = ixpt + 1
            oxptt (ixpt) = opropd (0.0d0, koequ, ixpt-1, ixpt+1)
!
!  Operators
!
         case (kkslh)   ! /
            ixpt = ixpt + 1
            oxptt (ixpt) = opropd (0.0d0, kodiv, ixpt-1, ixpt+1)
         case (kkpms)   ! + or -
            ixpt = ixpt + 1
            if (ztokw (1:1) == "+") then
               oxptt (ixpt) = opropd (0.0d0, kopls, ixpt-1, ixpt+1)
            else
               oxptt (ixpt) = opropd (0.0d0, komns, ixpt-1, ixpt+1)
            endif
         case (kksta)   ! *
            ixpt = ixpt + 1
            oxptt (ixpt) = opropd (0.0d0, komlt, ixpt-1, ixpt+1)
         case (kkpow)   ! **
            ixpt = ixpt + 1
            oxptt (ixpt) = opropd (0.0d0, kopow, ixpt-1, ixpt+1)
         endselect
      enddo
!
      oxptt (ixpt)%inxt = 0
      return
end subroutine lexxpr
subroutine evlxpr (ifres, kerr)
!  analyse lexed expression and evaluate result as logical
logical, intent (out) :: ifres
integer, intent (out) :: kerr
! ____________________________________________________________________
!
      double precision :: dres
      kerr = 0
      ifres = .true.
      call valxpr (dres, kres, kerr)
      ifres = (dres /= 0.0d0)
      return
end subroutine evlxpr
subroutine valxpr (dresw, kresw, kerr)
!  analyse lexed expression and value result in a character string
use fxprvars
use fpprcurs
double precision, intent (out) :: dresw
integer, intent (out) :: kresw, kerr
! ____________________________________________________________________
!
      kerr = 0
      kresw = konuf
      dresw = 1.0d0
      ixpte = ixpt
body: do
         if (ixpte < 1) then
            call fpperr ("Empty expression")
            kerr = 1
            exit body
         elseif (ixpte == 1) then
            if (oxptt(ixpte)%koprd == konui .or. &
                oxptt(ixpte)%koprd == konuf) then
               kresw = oxptt(ixpte)%koprd
               dresw = oxptt(ixpte)%dval
            else
               call fpperr ("Illegal expression")
               kerr = 2
            endif
            exit body
         else
!
!  Change Fun (A, B) into (A) .Fun. (B) where it appears
!
            ixptw = 1
            ixpta = ixpte
            do
               select case (oxptt(ixptw)%koprd)
               case (komod, komax, komin, koat2, kosrk)
!
!  Find corresponding separator
!
                  lpar = 0
                  ixpt1 = oxptt (ixptw)%inxt
                  do
                     select case (oxptt(ixpt1)%koprd)
                     case (kopfr)
                        lpar = lpar - 1
                        if (lpar <= 0) then
                           if (oxptt(ixptw)%koprd == kosrk) then
                              ixptw = oxptt (ixptw)%inxt
                              exit
                           endif
                           call fpperr ("Missing argument")
                           kerr = 3
                           exit body
                        endif
                     case (kopou)
                        lpar = lpar + 1
                     case (kosep)
                        koprdw = oxptt(ixptw)%koprd
                        iprv = oxptt (ixptw)%iprv
                        oxptt (ixptw) = oxptt (oxptt (ixptw)%inxt)
                        oxptt (ixptw)%iprv = iprv
                        oxptt (oxptt (ixptw)%inxt)%iprv = ixptw
                        if (ixpta + 2 > nxptm) then
                           call fpperr ("Operation stack too small" // &
                           " increase # of operators/ands and try again")
                           kerr = 4
                           exit body
                        endif
                        select case (koprdw)
                        case (komod)
                           koopr = komodi
                        case (komax)
                           koopr = komaxi
                        case (komin)
                           koopr = komini
                        case (koat2)
                           koopr = koat2i
                        case (kosrk)
                           koopr = kosrki
                        case default
                           koopr = koukn
                        end select
                        ixpt2 = oxptt (ixpt1)%iprv
                        ixpt4 = oxptt (ixpt1)%inxt
                        oxptt (ixpt1) = &
                                    opropd (0.0D0, kopfr, ixpt2, ixpta+1)
                        oxptt (ixpta+1) = &
                                    opropd (0.0D0, koopr, ixpt1, ixpta+2)
                        oxptt (ixpta+2) = &
                                    opropd (0.0D0, kopou, ixpta+1, ixpt4)
                        oxptt (ixpt4)%iprv = ixpta + 2
                        ixpta = ixpta + 2
                        exit
                     end select
                     ixpt1 = oxptt (ixpt1)%inxt
                     if (ixpt1 <= 0) then
                        call fpperr ("Illegal expression")
                        kerr = 5
                        exit body
                     endif
                  enddo
               case default
                  ixptw = oxptt (ixptw)%inxt
               end select
               if (ixptw <= 0) exit
            enddo
!
!  Reduce expressions
!
            ixptw = 1
            ixptd = 1
            do
               select case (oxptt(ixptw)%koprd)
               case (kopfr)
                  ixptf = ixptw
                  if (oxptt (ixptw)%iprv == 0) then
                     call fpperr ("Unexpected )")
                     kerr = 6
                     exit body
                  endif
                  call rdcxpr
                  if (oxptt (ixptd)%inxt == 0 .and.                     &
                      oxptt (ixptd)%iprv == 0) exit
                  ixptd = 1
                  ixptw = 1
                  cycle
               case (kopou)
                  ixptd = ixptw
               case (kosep)
                  call fpperr ("Illegal expression")
                  kerr = 5
                  exit body
               case default
               end select
               ixpt1 = oxptt (ixptw)%inxt
               if (ixpt1 <= 0) then
                  ixptf = ixptw
                  call rdcxpr
                  exit
               else
                  ixptw = ixpt1
               endif
            enddo
            if (kerr /= 0) then
               call fpperr ("Illegal expression")
               kerr = 5
               exit body
            else
               kresw = oxptt(ixptd)%koprd
               dresw = oxptt(ixptd)%dval
            endif
         endif
         exit body
      enddo body
      return
contains
      subroutine rdcxpr
!   Reduce an expression with no inner parentheses
         integer, dimension (nxptm) :: koprt, ixptt
         double precision dopd1, dopd2, dres
!
!   Remove enclosing parentheses if any
!
         ixpt4 = ixptf
         do
            kopr4 = oxptt(ixpt4)%koprd
            ixpt3 = oxptt(ixpt4)%iprv
            if (ixpt3 == 0) then
               if (kopr4 /= konui .and. kopr4 /= konuf) then
                  kerr = 7
               endif
               return
            endif
            kopr2 = oxptt(ixptd)%koprd
            ixpt2 = oxptt(ixptd)%inxt
            if (kopr4 == kopfr .and. kopr2 == kopou) then
               if (oxptt(ixpt4)%inxt /= 0) then
                  oxptt (oxptt(ixpt4)%inxt)%iprv = ixpt3
               endif
               oxptt(ixpt3)%inxt = oxptt(ixpt4)%inxt
               iprvw = oxptt(ixptd)%iprv
               oxptt(ixptd) = oxptt(ixpt2)
               oxptt(ixptd)%iprv = iprvw
               oxptt (oxptt(ixptd)%inxt)%iprv = ixptd
               ixptf = ixpt3
            else
               exit
            endif
         enddo
!
!   Find and apply unary operators, and store binary ones
!
         ixpt4 = ixptf
         ixpt3 = oxptt(ixpt4)%iprv
         kopr4 = oxptt(ixpt4)%koprd
         ixpt2 = oxptt(ixptd)%inxt
!
         nopr  = 0
         do
            if (ixptf == ixptd) exit
            if (ixpt3 /= ixptd) then
               ixpt2 = oxptt(ixpt3)%iprv
               kopr2 = oxptt(ixpt2)%koprd
            else
               ixpt2 = 0
               kopr2 = koukn
            endif
            kopr3 = oxptt(ixpt3)%koprd
            kopr4 = oxptt(ixpt4)%koprd
            if (kopr4 == konui .or. kopr4 == konuf) then
               if (kopr3 /= konui .and. kopr3 /= konuf .and. &
                   kopr2 /= konui .and. kopr2 /= konuf) then
!
!   Apply unary operator at 3 to value at 4
!
                   dopd1 = oxptt(ixpt4)%dval
                   call xqtunr (kopr3, kopr4, dopd1, kres, dres)
                   if (kerr /= 0) then
                      return
                   endif
                   oxptt(ixpt3)%dval  = dres
                   oxptt(ixpt3)%koprd = kres
                   oxptt(ixpt3)%inxt  = oxptt(ixpt4)%inxt
                   if (oxptt(ixpt3)%inxt /= 0) then
                      oxptt (oxptt(ixpt3)%inxt)%iprv = ixpt3
                   endif
               else
                   select case (kopr3)
                   case default
                      continue
                   case (koori, koand, kogth, kogeq, kolth, koleq, &
                         koneq, komns, kopls, komlt, kodiv, kopow, &
                         koequ, komodi, komaxi, komini, koat2i, kosrki)
                      nopr = nopr + 1
                      koprt (nopr) = kopr3
                      ixptt (nopr) = ixpt3
                   end select
               endif
            endif
            if (ixpt2 == 0) exit
            ixpt4 = ixpt3
            ixpt3 = ixpt2
         enddo
!
!   Apply binary operators in priority order
!
         if (nopr > 0) then
            do
               iopr = element (maxloc (koprt (1:nopr)), 1)
               kopr = koprt (iopr)
               if (kopr == 0) exit
               ixpt3 = ixptt (iopr)
               ixpt2 = oxptt(ixpt3)%iprv
               ixpt4 = oxptt(ixpt3)%inxt
               dopd1 = oxptt(ixpt2)%dval
               dopd2 = oxptt(ixpt4)%dval
               kopd1 = oxptt(ixpt2)%koprd
               kopd2 = oxptt(ixpt4)%koprd
               call xqtbin (kopr, kopd1, kopd2, dopd1, dopd2, &
                            kres, dres)
               if (kerr /= 0) then
                  return
               endif
               oxptt(ixpt2)%dval  = dres
               oxptt(ixpt2)%koprd = kres
               oxptt(ixpt2)%inxt  = oxptt(ixpt4)%inxt
               if (oxptt(ixpt2)%inxt /= 0) then
                  oxptt (oxptt(ixpt2)%inxt)%iprv = ixpt2
               endif
               koprt (iopr) = 0
            enddo
         endif
!
         return
      end subroutine rdcxpr
      subroutine xqtunr (kopr, kopd, dopd, kres, dres)
!   Apply unary operator kopr to operand dopd of type kopd
         integer, intent (in)           :: kopr, kopd
         double precision, intent (in)  :: dopd
         integer, intent (out)          :: kres
         double precision, intent (out) :: dres
!
         select case (kopr)
         case (konot) ! Not
            if (dopd /= 0.0D0) then
               dres = 0.0D0
            else
               dres = 1.0D0
            endif
            kres = konui
         case (komns) ! -
            dres = - dopd
            kres = kopd
         case (kopls) ! +
            dres = dopd
            kres = kopd
         case (koint) ! Int
            dres = int (dopd)
            kres = konui
         case (konin) ! Nint
            dres = nint (dopd)
            kres = konui
         case (kosin) ! Sin
            dres = sin (dopd)
            kres = konuf
         case (kocos) ! Cos
            dres = cos (dopd)
            kres = konuf
         case (kotan) ! Tan
            dres = tan (dopd)
            kres = konuf
         case (koatn) ! Atan
            dres = atan (dopd)
            kres = konuf
         case (kolog) ! Log
            dres = log (dopd)
            kres = konuf
         case (koexp) ! Exp
            dres = exp (dopd)
            kres = konuf
         case (kol10) ! Log10
            dres = log10 (dopd)
            kres = konuf
         case (kosqr) ! Sqrt
            dres = sqrt (dopd)
            kres = konuf
         case (koasn) ! Asin
            dres = asin (dopd)
            kres = konuf
         case (koacs) ! Acos
            dres = acos (dopd)
            kres = konuf
         case (kosnh) ! Sinh
            dres = sinh (dopd)
            kres = konuf
         case (kocsh) ! Cosh
            dres = cosh (dopd)
            kres = konuf
         case (kotnh) ! Tanh
            dres = tanh (dopd)
            kres = konuf
         case (koabs) ! Abs
            dres = abs (dopd)
            kres = kopd
         case (kosik) ! Selected_int_kind
            dres = selected_int_kind (nint (dopd))
            kres = konui
         case (kosrk) ! Selected_real_kind
            dres = selected_real_kind (nint (dopd))
            kres = kopd
         case (kopou) ! Nothing
            dres = dopd
            kres = kopd
         case default ! Other
            kerr = 9
            return
         end select
      end subroutine xqtunr
      subroutine xqtbin (kopr, kopd1, kopd2, dopd1, dopd2, kres, dres)
!   Apply binary operator kopr to operands dopdi of type kopdi
         integer, intent (in)           :: kopr, kopd1, kopd2
         double precision, intent (in)  :: dopd1, dopd2
         integer, intent (out)          :: kres
         double precision, intent (out) :: dres
!
         select case (kopr)
         case (koori) ! Or
            if (dopd1 /= 0.0D0 .or. dopd2 /= 0.0D0) then
               dres = 1.0D0
            else
               dres = 0.0D0
            endif
            kres = konui
         case (koand) ! And
            if (dopd1 /= 0.0D0 .and. dopd2 /= 0.0D0) then
               dres = 1.0D0
            else
               dres = 0.0D0
            endif
            kres = konui
         case (kogth) ! >
            if (dopd1 > dopd2) then
               dres = 1.0D0
            else
               dres = 0.0D0
            endif
            kres = konui
         case (kogeq) ! >=
            if (dopd1 >= dopd2) then
               dres = 1.0D0
            else
               dres = 0.0D0
            endif
            kres = konui
         case (koequ) ! ==
            if (dopd1 == dopd2) then
               dres = 1.0D0
            else
               dres = 0.0D0
            endif
            kres = konui
         case (kolth) ! <
            if (dopd1 < dopd2) then
               dres = 1.0D0
            else
               dres = 0.0D0
            endif
            kres = konui
         case (koleq) ! <=
            if (dopd1 <= dopd2) then
               dres = 1.0D0
            else
               dres = 0.0D0
            endif
            kres = konui
         case (koneq) ! /=
            if (dopd1 /= dopd2) then
               dres = 1.0D0
            else
               dres = 0.0D0
            endif
            kres = konui
         case (komns) ! -
            dres = dopd1 - dopd2
            if (kopd1 == konui .and. kopd2 == konui) then
               kres = konui
            else
               kres = konuf
            endif
         case (kopls) ! +
            dres = dopd1 + dopd2
            if (kopd1 == konui .and. kopd2 == konui) then
               kres = konui
            else
               kres = konuf
            endif
         case (komlt) ! *
            dres = dopd1 * dopd2
            if (kopd1 == konui .and. kopd2 == konui) then
               kres = konui
            else
               kres = konuf
            endif
         case (kodiv) ! /
            if (kopd1 == konui .and. kopd2 == konui) then
               dres = nint (dopd1) / nint (dopd2)
               kres = konui
            else
               dres = dopd1 / dopd2
               kres = konuf
            endif
         case (kopow) ! **
            if (kopd1 == konui .and. kopd2 == konui) then
               dres = nint (dopd1) ** nint (dopd2)
               kres = konui
            elseif (kopd1 == konuf .and. kopd2 == konui) then
               dres = dopd1 ** nint (dopd2)
               kres = konuf
            else
               dres = dopd1 ** dopd2
               kres = konuf
            endif
         case (komodi) ! .Mod.
            if (kopd1 == konui .and. kopd2 == konui) then
               dres = mod (nint (dopd1), nint (dopd2))
               kres = konui
            else
               dres = mod (dopd1, dopd2)
               kres = konuf
            endif
         case (komaxi) ! .Max.
            dres = max (dopd1, dopd2)
            if (kopd1 == konui .and. kopd2 == konui) then
               kres = konui
            else
               kres = konuf
            endif
         case (komini) ! .Min.
            dres = min (dopd1, dopd2)
            if (kopd1 == konui .and. kopd2 == konui) then
               kres = konui
            else
               kres = konuf
            endif
         case (koat2i) ! .Atan2.
            dres = atan2 (dopd1, dopd2)
            kres = konuf
         case (kosrki) ! .S_R_K.
            dres = selected_real_kind (nint (dopd1), nint (dopd2))
            kres = konui
         case default ! Other
            kerr = 11
            return
         end select
      end subroutine xqtbin
      integer function element (j, i)
         integer, dimension (:), intent (in) :: j
         integer, intent (in)                :: i
         element = j (i)
      end function element
end subroutine valxpr
subroutine strxpr (zres, kres, kerr)
!  analyse lexed expression and evaluate result as string
use fxprprms
character (len=*), intent (out) :: zres
integer, intent (out)           :: kres, kerr
! ____________________________________________________________________
!
      double precision :: dres
      kerr = 0
      call valxpr (dres, kres, kerr)
      select case (kres)
      case (konul)
         if (dres /= 0.0d0) then
            zres = ".True."
         else
            zres = ".False."
         endif
      case (konui)
         write (zres, *) nint (dres)
         zres = adjustl (zres)
      case (konuf)
         write (zres, *) dres
         zres = adjustl (zres)
      case default
         zres = ".True."
      end select
      return
end subroutine strxpr
subroutine outtks (kktknt, itkndt, itknft, itknnt, ztkni, ksstt)
!  build and output statement
use flexvars
use fprsvars
use fpprcurs
character (len=*), intent (in)       :: ztkni
integer, intent (in), dimension (:)  :: kktknt, itkndt, itknft, itknnt
integer, intent (in)                 :: ksstt
! ____________________________________________________________________
!
      character (len=2*lsttm) :: zstt
      character (len=linem)   :: zcmt, zlab
      character (len=lsttm)   :: ztknw
!
      ntkn = size (kktknt)
!
!  Need to change indent value ?
!
      select case (ksstt)
      case (ksprm)
         lprc = lprc - 1
         if (lprc == 0) nndt = nndt - nndtp
         nndt = nndt - nndtp
      case (kspre)
         lprc = lprc - 1
         if (lprc == 0) nndt = nndt - nndtp
         nndt = nndt - nndtp
      case (ksifm)
         nndt = nndt - nndtp
      case (ksife)
         nndt = nndt - nndtp
      case (ksfre)
         nndt = nndt - nndtp
      case (kswhm)
         nndt = nndt - nndtp
      case (kswhe)
         nndt = nndt - nndtp
      case (ksdoe)
         do idoe = 1, ndoe
            nndt = nndt - nndtp
         enddo
      case (ksnte)
         nndt = nndt - nndtp
      case (ksslm)
         nndt = nndt - nndtp
      case (kssle)
         nndt = nndt - nndtp
      case (kstye)
         nndt = nndt - nndtp
      end select
      nndt = max (nndt, 0)
      lstt = 0
      llab = 0
      lpar = 0
      irep1 = irep
      irepg1 = irepg
      if (ntkn == 0) then
!
!  blank lines
!
         call wrtstt (zlab, llab, zblk, 0, zblk, len_trim (zblk), 0)
      elseif (ntkn == 1 .and. kktknt (1) == kkcmt) then
         itknd = itkndt (1)
         itknf = itknft (1)
         ltkn  = itknf - itknd + 1
         call wrtstt (zlab, llab, zblk, 0, ztkni (itknd:itknf), ltkn, 0)
      elseif (ntkn == 1 .and. kktknt (1) == kkebc) then
         itknd = itkndt (1)
         itknf = itknft (1)
         ltkn  = itknf - itknd + 1
         call wrtstt (zlab, llab, zblk, 0, ztkni (itknd:itknf), ltkn, 0)
      else
         kkprv = kkukn
         ifblk = 0
         lcmt  = 0
         itkn  = 0
         do
            itkn = itkn + 1
            if (itkn > ntkn) exit
            itknd = itkndt (itkn)
            itknf = itknft (itkn)
            ltknw = itknf - itknd + 1
            ztknw (1:ltknw) = ztkni (itknd:itknf)
            kktkn = kktknt (itkn)
!
!  Construct line inserting spaces where necessary
!
            select case (kktkn)
            case (kkcmt, kkebc)   ! Comment string
               if (ifblk == 0) then
                  ltkn = ltknw + 1
                  zcmt (1:ltkn) = " " // ztknw (1:ltknw)
               else
                  ltkn = ltknw
                  zcmt (1:ltkn) = ztknw (1:ltknw)
               endif
               lcmt = ltkn
               exit
            case (kkfcm)   ! False comment
               llab = ltknw
               zlab (1:llab) = ztknw (1:ltknw)
               ltkn = 0
            case (kklab)   ! Numerical label
               if (lstt == 0) then
                  llab = ltknw
                  zlab (1:llab) = ztknw (1:ltknw)
                  ltkn = 0
               else
                  ltkn = ltknw + 2
                  zstt (lstt+1:lstt+ltkn) = " " // ztknw (1:ltknw) // " "
                  ifblk = 1
               endif
            case (kkstr, & ! Character string
            &     kkknd, & ! _Kind (underscore)
            &     kkbnm, & ! Block name
            &     kknui, & ! Integer Numerical value
            &     kknuf)   ! Real Numerical value
               ltkn = ltknw
               zstt (lstt+1:lstt+ltkn) = ztknw (1:ltknw)
               ifblk = 0
            case (kkprc)   ! %
               ltkn = ltknw
               if (ifblk /= 0) lstt = lstt - 1
               zstt (lstt+1:lstt+ltkn) = ztknw (1:ltknw)
               ifblk = 0
            case (kksep)   ! ,
               ltkn = ltknw + 1
               if (ifblk /= 0) lstt = lstt - 1
               zstt (lstt+1:lstt+ltkn) = ztknw (1:ltknw) // " "
               ifblk = 1
            case (kkidf)   ! Identifier
!
               kwnam = tnamt(itknnt(itkn))%kwnam
               if (kwnam <= kwsys .and. kccask /= kclve) then
                  call chgcas (ztknw (1:ltknw), kccask)
               elseif (kwnam > kwsys .and. kccasu /= kclve) then
                  call chgcas (ztknw (1:ltknw), kccasu)
               endif
!
               if (kwnam == kwlop .and. ifblk == 0) then
                  ltkn = ltknw + 2
                  zstt (lstt+1:lstt+ltkn) = " " // ztknw (1:ltknw) // " "
                  ifblk = 1
               elseif (kwnam == kwlop .and. ifblk /= 0) then
                  ltkn = ltknw + 1
                  zstt (lstt+1:lstt+ltkn) = ztknw (1:ltknw) // " "
                  ifblk = 1
               elseif (kwnam == kwac5) then
                  if (ifblk /= 0 .or. lstt == 0) then
                     ltkn = ltknw + 1
                     zstt (lstt+1:lstt+ltkn) = ztknw (1:ltknw) // " "
                  else
                     ltkn = ltknw + 2
                     zstt (lstt+1:lstt+ltkn) = " " // ztknw (1:ltknw)   &
                                                   // " "
                  endif
                  ifblk = 1
               elseif ((kkprv == kkidf .or. &
                        kkprv == kknui .or. &
                        kkprv == kknuf ) .and. ifblk == 0) then
                  ltkn = ltknw + 1
                  zstt (lstt+1:lstt+ltkn) = " " // ztknw (1:ltknw)
                  ifblk = 0
               else
                  ltkn = ltknw
                  zstt (lstt+1:lstt+ltkn) = ztknw (1:ltknw)
                  ifblk = 0
               endif
            case (kkdpt)   ! :
               if (lpar <= 0) then
                  ltkn = ltknw + 1
                  zstt (lstt+1:lstt+ltkn) = ztknw (1:ltknw) // " "
                  ifblk = 1
               else
                  ltkn = ltknw
                  zstt (lstt+1:lstt+ltkn) = ztknw (1:ltknw)
                  ifblk = 0
               endif
!
            case (kkpvg, & ! ;
            &     kkcmd, & ! $command (preprocessor)
            &     kkdot)   ! )
               ltkn = ltknw + 1
               zstt (lstt+1:lstt+ltkn) = ztknw (1:ltknw) // " "
               ifblk = 1
            case (kkqst)   ! ?
               ltkn = 0
               if (ifblk /= 0) lstt = lstt - 1
               ifblk = 0
            case (kkpfr)   ! )
               lpar = lpar - 1
               ltkn = ltknw + 1
               if (ifblk /= 0) lstt = lstt - 1
               zstt (lstt+1:lstt+ltkn) = ztknw (1:ltknw) // " "
               ifblk = 1
            case (kkpou)   ! (
               lpar = lpar + 1
               if (lpar <= 1 .and. ifblk == 0) then
                  ltkn = ltknw + 1
                  zstt (lstt+1:lstt+ltkn) = " " // ztknw (1:ltknw)
               else
                  ltkn = ltknw
                  zstt (lstt+1:lstt+ltkn) = ztknw (1:ltknw)
               endif
               ifblk = 0
            case (kkpnb)   ! ( within defined type
               lpar = lpar + 1
               ltkn = ltknw
               zstt (lstt+1:lstt+ltkn) = ztknw (1:ltknw)
               ifblk = 0
            case (kkslh, & ! /
            &     kkpms, & ! + or -
            &     kkcct, & ! //
            &     kksta, & ! *
            &     kkpow, & ! **
            &     kkaff)   ! =
               if (lpar <= 0 .and. ifblk == 0) then
                  ltkn = ltknw + 2
                  zstt (lstt+1:lstt+ltkn) = " " // ztknw (1:ltknw) // " "
                  ifblk = 1
               elseif (lpar <= 0) then
                  ltkn = ltknw + 1
                  zstt (lstt+1:lstt+ltkn) = ztknw (1:ltknw) // " "
                  ifblk = 1
               else
                  if (ifblk /= 0) lstt = lstt - 1
                  ltkn = ltknw
                  zstt (lstt+1:lstt+ltkn) = ztknw (1:ltknw)
                  ifblk = 0
               endif
            case (kkcou, & ! (/
            &     kkneq, & ! /=
            &     kkleq, & ! <=
            &     kkequ, & ! ==
            &     kkgeq, & ! >=
            &     kkpts, & ! =>
            &     kksup, & ! >
            &     kkinf, & ! <
            &     kkdcl, & ! ::
            &     kklog, & ! .xxx.
            &     kkcfr)   ! /)
               if (ifblk == 0) then
                  ltkn = ltknw + 2
                  zstt (lstt+1:lstt+ltkn) = " " // ztknw (1:ltknw) // " "
               else
                  ltkn = ltknw + 1
                  zstt (lstt+1:lstt+ltkn) = ztknw (1:ltknw) // " "
               endif
               ifblk = 1
            case default
               ltkn = ltknw
               zstt (lstt+1:lstt+ltkn) = ztknw (1:ltknw)
               ifblk = 0
            endselect
            lstt = lstt + ltkn
            kkprv = kktkn
         enddo
         do
            if (zstt (lstt:lstt) /= " " .or. lstt <= 1) exit
            lstt = lstt - 1
         enddo
         call wrtstt (zlab, llab, zstt, lstt, zcmt, lcmt, nndt)
      endif
!
!  Need to change indent value ?
!
      select case (ksstt)
      case (ksprs)
         if (lprc == 0) nndt = nndt + nndtp
         lprc = lprc + 1
         nndt = nndt + nndtp
      case (ksprm)
         if (lprc == 0) nndt = nndt + nndtp
         lprc = lprc + 1
         nndt = nndt + nndtp
      case (ksifs)
         nndt = nndt + nndtp
      case (ksifm)
         nndt = nndt + nndtp
      case (ksfrs)
         nndt = nndt + nndtp
      case (kswhs)
         nndt = nndt + nndtp
      case (kswhm)
         nndt = nndt + nndtp
      case (ksdos)
         nndt = nndt + nndtp
      case (ksnts)
         nndt = nndt + nndtp
      case (kssls)
         nndt = nndt + nndtp
      case (ksslm)
         nndt = nndt + nndtp
      case (kstys)
         nndt = nndt + nndtp
      end select
      return
end subroutine outtks
integer function indhsh (znam)
!  Return hash index for identifier
use fprsvars
use flexprms
use fpprcurs
character (len=*), intent (in) :: znam
! ____________________________________________________________________
!
    interface
      logical function ifsame (zstr1, zstr2)
!        Case insensitive compare
      character (len=*), intent (in) :: zstr1, zstr2
      end function ifsame
    end interface
!
      lnam = len_trim (znam)
      ihsh = khshstr (znam (1:lnam))
      inam  = 0
      if (tnamt(ihsh)%kwnam == 0) then
         indhsh = 0
      else
         do
            inamd = tnamt(ihsh)%inamd
            inamf = tnamt(ihsh)%inamf
            if (ifsame (znam (1:lnam), znamg (inamd:inamf))) then
                  indhsh = ihsh
                  exit
            endif
            if (tnamt(ihsh)%ihshf == 0) then
                  indhsh = 0
                  exit
            else
                  ihsh = tnamt(ihsh)%ihshf
                  cycle
            endif
         enddo
      endif
!
      return
end function indhsh
