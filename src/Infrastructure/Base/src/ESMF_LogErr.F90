! $Id: ESMF_LogErr.F90,v 1.47 2004/12/03 22:03:52 cpboulder Exp $
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
!     ESMF LogErr Module
      module ESMF_LogErrMod
!
!==============================================================================
!
! This file contains the LogErr class definition and all LogErr class
! methods.
!
!------------------------------------------------------------------------------
! INCLUDES
!! this should be including ESMF.h, but since it also includes the cover
!! log macros, it can't be included here.  so just include each file 
!! individually.  If we add files to ESMF.h they also need to be added here.
#include "ESMF_Macros.inc"
#include "ESMF_Conf.inc"
#include "ESMF_Version.inc"
#include "ESMF_LogConstants.inc"
#include "ESMF_ErrReturnCodes.inc"


!BOPI
!============================================================================
! !MODULE: Fortran Interface to Log class. 
!
! !DESCRIPTION:
!
! The Fortran interface to the {\tt ESMF\_Log} class is written in both
! Fortran and C/C++.
! This file contains the interface code written in Fortran.  It also contains
! some utility functions used by the {\tt ESMF\_Log} class.
!
!------------------------------------------------------------------------------
! !USES:
    ! inherit from ESMF base class
    use ESMF_BaseTypesMod
    use ESMF_VMTypesMod
    use ESMF_VMBaseMod

implicit none
!
!------------------------------------------------------------------------------
! !PRIVATE TYPES:
!------------------------------------------------------------------------------

!     ! ESMF_MsgType
type ESMF_MsgType
    sequence
    integer      :: mtype
end type

!     ! Msg Types
type(ESMF_MsgType), parameter           :: &
    ESMF_LOG_INFO  =   ESMF_MsgType(1), &
    ESMF_LOG_WARNING = ESMF_MsgType(2), &
    ESMF_LOG_ERROR =   ESMF_MsgType(3)

!     ! ESMF_Halt
type ESMF_HaltType
    sequence
    integer      :: htype
end type

!     ! Halt Types
type(ESMF_HaltType), parameter           :: &
    ESMF_LOG_HALTNEVER  =  ESMF_HaltType(1), &
    ESMF_LOG_HALTWARNING = ESMF_HaltType(2), &
    ESMF_LOG_HALTERROR =   ESMF_HaltType(3)
    
!     ! ESMF_LogType
type ESMF_LogType
    sequence
    integer      :: ftype
end type
    
!     ! Log Types
type(ESMF_LogType), parameter		:: &
    ESMF_LOG_SINGLE = ESMF_LogType(1), &
    ESMF_LOG_MULTI = ESMF_LogType(2)
    
     
!     ! Log Entry                            
type ESMF_LOGENTRY
    private
    sequence  	
    character(len=64)				msg	
    character(len=32) 				file,method	
    character(len=8) 				d		                                     
    character(len=8)				lt  		
    integer                                     petNumber	   				
    integer					        h,m,s,ms				        
    integer					        line   
    logical					        methodflag,lineflag,fileflag,stopprogram					
end type ESMF_LOGENTRY

!     ! ESMF_LOGARRAY
type ESMF_LOGARRAY
    private
    sequence
    character(len=64)                                   nameLogErrFile   
    integer                              	    ::  findex
    integer                                         ::  unitNumber
    type(ESMF_Logical)			            ::  flushed 
    type(ESMF_Logical)			            ::  dirty
    type(ESMF_Logical)                              ::  FileIsOpen
end type ESMF_LOGARRAY

!     ! Log  
type ESMF_Log
    private
    sequence        
#ifndef ESMF_NO_INITIALIZERS
    type(ESMF_LOGENTRY), dimension(:,:),pointer       ::  LOG_ENTRY=>Null()
    type(ESMF_LOGARRAY), dimension(:),pointer       ::  LOG_ARRAY =>Null()
#else
    type(ESMF_LOGENTRY), dimension(:),pointer       ::  LOG_ENTRY
    type(ESMF_LOGARRAY), dimension(:),pointer       ::  LOG_ARRAY
#endif                                        
    type(ESMF_VM)                                   ::  vm  
    type(ESMF_HaltType)                             ::  halt
    type(ESMF_LogType)			            ::  logtype       
    integer                                             maxElements
    integer                                             stream 
    integer                                             petCount 
    type(ESMF_Logical)                              ::  flushImmediately    
    type(ESMF_Logical)                              ::  rootOnly    
    type(ESMF_Logical)                              ::  verbose  
    type(ESMF_Logical)                              ::  logNone
    
end type ESMF_Log

!------------------------------------------------------------------------------
! !PUBLIC TYPES:
    public ESMF_LOG_INFO
    public ESMF_LOG_WARNING
    public ESMF_LOG_ERROR
    public ESMF_LOG_SINGLE
    public ESMF_LOG_MULTI
    public ESMF_LOG_HALTNEVER
    public ESMF_LOG_HALTWARNING
    public ESMF_LOG_HALTERROR
!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:
   public ESMF_Log
   public ESMF_LogClose
   public ESMF_LogFinalize
   public ESMF_LogFlush
   public ESMF_LogFoundAllocError
   public ESMF_LogFoundError
   public ESMF_LogGet
   public ESMF_LogInitialize
   public ESMF_LogMsgFoundAllocError
   public ESMF_LogMsgFoundError
   public ESMF_LogMsgSetError
   public ESMF_LogOpen
   public ESMF_LogSet
   public ESMF_LogWrite
   public ESMF_HaltType
   public ESMF_MsgType

!  Overloaded = operator functions
   public operator(.eq.),operator(.gt.)
   
! overload .eq. and .gt. with additional derived types so you can compare 
!  them as if they were simple integers.
 

interface operator (.eq.)
   module procedure ESMF_lmteq
   module procedure ESMF_lhteq
   module procedure ESMF_llteq
end interface

interface operator (.gt.)
   module procedure ESMF_lmtgt
end interface
!EOPI  

type(ESMF_Log),SAVE::ESMF_LogDefault	
!----------------------------------------------------------------------------

contains

!------------------------------------------------------------------------------
! functions to compare two ESMF_DataTypes to see if they're the same or not

function ESMF_lhteq(ht1, ht2)
logical ESMF_lhteq
type(ESMF_HaltType), intent(in) :: ht1,ht2
    
    ESMF_lhteq = (ht1%htype .eq. ht2%htype)
end function

function ESMF_lmteq(mt1, mt2)
logical ESMF_lmteq
type(ESMF_MsgType), intent(in) :: mt1,mt2

    ESMF_lmteq = (mt1%mtype .eq. mt2%mtype)
end function

function ESMF_llteq(lt1, lt2)
logical ESMF_llteq
type(ESMF_LogType), intent(in) :: lt1,lt2

    ESMF_llteq = (lt1%ftype .eq. lt2%ftype)
end function

function ESMF_lmtgt(mt1, mt2)
logical ESMF_lmtgt
type(ESMF_MsgType), intent(in) :: mt1,mt2

    ESMF_lmtgt = (mt1%mtype .gt. mt2%mtype)
end function

!--------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_LogClose - Close Log file(s)

! !INTERFACE: 
    subroutine ESMF_LogClose(log, rc)
!
! !ARGUMENTS:
    type(ESMF_Log)	            :: log
    integer, intent(out),optional   :: rc

! !DESCRIPTION:
!      This routine closes the file(s) associated with the {\tt log}.
!
!      The arguments are:
!      \begin{description}
! 
!      \item [log]
!            An {\tt ESMF\_Log} object.
!      \item [{[rc]}]
!            Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!      \end{description}
! 
!EOP

	!character(len=10) 					:: t
	!character(len=8) 					:: d
	integer  :: i
	
	if (present(rc)) then
	  	rc=ESMF_FAILURE
	endif
	do i=1, ESMF_LogDefault%petCount
	    if (log%LOG_ARRAY(i)%FileIsOpen .eq. ESMF_TRUE) then
		!call DATE_AND_TIME(d,t)
		!WRITE(log%unitnumber,100) d,t,"INFO     Log Close"
		!100 FORMAT(a8,2x,a10,2x,a)
		!if (log%verbose .eq. ESMF_TRUE) print *,d,"  ",t,"  INFO       Log Close"
		!CLOSE(UNIT=log%stdOutUnitNumber)
		log%LOG_ARRAY(i)%FileIsOpen=ESMF_FALSE
		if (present(rc)) rc=ESMF_SUCCESS
	    endif	
	enddo
	
end subroutine ESMF_LogClose

!--------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_LogFinalize - Finalize Log file(s)

! !INTERFACE: 
	subroutine ESMF_LogFinalize(rc)
!
! !ARGUMENTS:
    integer, intent(out),optional	:: rc

! !DESCRIPTION:
!      This routine finalizes the global log.  The default log will be flushed
!      and the default log will be closed.
!
!      The arguments are:
!      \begin{description}
! 
!      \item [{[rc]}]
!            Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!      \end{description}
! 
!EOPI
	integer::rc2,status,i
	
	if (present(rc)) rc=ESMF_FAILURE
	do i=1, ESMF_LogDefault%petCount
	    if (ESMF_LogDefault%LOG_ARRAY(i)%FileIsOpen .eq. ESMF_TRUE) then
	        if (ESMF_LogDefault%LOG_ARRAY(i)%flushed .eq. ESMF_FALSE) then
		    call ESMF_LogFlush(ESMF_LogDefault,petnum=i,rc=rc2)	
		endif  
		ESMF_LogDefault%LOG_ARRAY(i)%FileIsOpen=ESMF_FALSE
	    endif
	enddo	
	if (present(rc)) rc=ESMF_SUCCESS
	deallocate(ESMF_LogDefault%LOG_ENTRY,stat=status)
	deallocate(ESMF_LogDefault%LOG_ARRAY,stat=status)
	
	
end subroutine ESMF_LogFinalize

!--------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_LogFlush - Flushes the Log file(s)

! !INTERFACE: 
	subroutine ESMF_LogFlush(log,petnum,rc)
!
!
! !ARGUMENTS:
	type(ESMF_LOG)				:: log
	integer, intent(in),optional		:: petnum
	integer, intent(out),optional		:: rc

! !DESCRIPTION:
!      This subroutine flushes th {\tt log}.
!
!      The arguments are:
!      \begin{description}
! 
!      \item [{[log]}]
!            An optional {\tt ESMF\_Log} object that can be used instead
!	     of the default log.
!      \item [{[rc]}]
!            Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!      \end{description}
! 
!EOP
    integer 			    :: i,j,ok,status,pet

    if (present(rc)) rc=ESMF_FAILURE
    if (present(petnum)) then 
        pet=petnum+1
    else
        pet=1
    endif	
    
    if ((log%LOG_ARRAY(pet)%FileIsOpen .eq. ESMF_TRUE) .AND. &
        (log%LOG_ARRAY(pet)%flushed .eq. ESMF_FALSE) .AND. &
	(log%LOG_ARRAY(pet)%dirty .eq. ESMF_TRUE))  then	
    	ok=0
    	do i=1, ESMF_LOG_MAXTRYOPEN
    	    OPEN(UNIT=log%LOG_ARRAY(pet)%unitnumber,File=log%LOG_ARRAY(pet)%nameLogErrFile,& 
	    POSITION="APPEND", ACTION="WRITE", STATUS="UNKNOWN", IOSTAT=status)
            if (status.eq.0) then
	        do j=1, log%LOG_ARRAY(pet)%findex
    		    if (log%LOG_ENTRY(pet,j)%lineflag) then								
    		        if (log%LOG_ENTRY(pet,j)%methodflag) then
    			    WRITE(log%LOG_ARRAY(pet)%unitnumber,122) &
                                  log%LOG_ENTRY(pet,j)%d     , " ", log%LOG_ENTRY(pet,j)%h   , &
                                  log%LOG_ENTRY(pet,j)%m     ,      log%LOG_ENTRY(pet,j)%s   , ".", &
                                  log%LOG_ENTRY(pet,j)%ms    , " ", log%LOG_ENTRY(pet,j)%lt  , "  pet", &
				  pet-1, " ", &
                                  log%LOG_ENTRY(pet,j)%file  , " ", log%LOG_ENTRY(pet,j)%line, " ", &
                                  log%LOG_ENTRY(pet,j)%method, " ", log%LOG_ENTRY(pet,j)%msg
    		        else
    			    WRITE(log%LOG_ARRAY(pet)%unitnumber,123) &
                                  log%LOG_ENTRY(pet,j)%d   , " ", log%LOG_ENTRY(pet,j)%h   , &
                                  log%LOG_ENTRY(pet,j)%m   ,      log%LOG_ENTRY(pet,j)%s   , ".", &
                                  log%LOG_ENTRY(pet,j)%ms  , " ", log%LOG_ENTRY(pet,j)%lt  , "  pet", &
				  pet-1, " ", &
                                  log%LOG_ENTRY(pet,j)%file, " ", log%LOG_ENTRY(pet,j)%line, " ", &
                                  log%LOG_ENTRY(pet,j)%msg
    		        endif	
                    else
    		        if (log%LOG_ENTRY(pet,j)%methodflag) then
    		            WRITE(log%LOG_ARRAY(pet)%unitnumber,132) &
                                  log%LOG_ENTRY(pet,j)%d     , " ", log%LOG_ENTRY(pet,j)%h  , &
                                  log%LOG_ENTRY(pet,j)%m     ,      log%LOG_ENTRY(pet,j)%s  , ".", &
                                  log%LOG_ENTRY(pet,j)%ms    , " ", log%LOG_ENTRY(pet,j)%lt , "  pet", &
				  pet-1, " ", &
    			          log%LOG_ENTRY(pet,j)%method, " ", log%LOG_ENTRY(pet,j)%msg
    		        else
    		            WRITE(log%LOG_ARRAY(pet)%unitnumber,133) &
                                  log%LOG_ENTRY(pet,j)%d  , " ", log%LOG_ENTRY(pet,j)%h , &
                                  log%LOG_ENTRY(pet,j)%m  ,      log%LOG_ENTRY(pet,j)%s , ".", &
                                  log%LOG_ENTRY(pet,j)%ms , " ", log%LOG_ENTRY(pet,j)%lt, "  pet", &
				  pet-1, " ", &
                                  log%LOG_ENTRY(pet,j)%msg
    		        endif	
    	            endif
		enddo    
    	        CLOSE(UNIT=log%LOG_ARRAY(pet)%unitnumber)
    	        ok=1
    	    endif	
    	    if (ok.eq.1) exit    
       enddo
   endif
   
   log%LOG_ARRAY(pet)%findex = 1 
   122  FORMAT(a8,a,i2.2,i2.2,i2.2,a,i6.6,a,i0,a,a,a,a,a,i0,a,a,a,a)
   123  FORMAT(a8,a,i2.2,i2.2,i2.2,a,i6.6,a,i0,a,a,a,a,a,i0,a,a)
   132  FORMAT(a8,a,i2.2,i2.2,i2.2,a,i6.6,a,i0,a,a,a,a,a,a,a)
   133  FORMAT(a8,a,i2.2,i2.2,i2.2,a,i6.6,a,i0,a,a,a,a,a)
   
   log%LOG_ARRAY(pet)%flushed = ESMF_TRUE
   log%LOG_ARRAY(pet)%dirty = ESMF_FALSE
   rc=ESMF_SUCCESS	
      
end subroutine ESMF_LogFlush
!--------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_LogFoundAllocError - Check Fortran status for allocation error

! !INTERFACE: 
	function ESMF_LogFoundAllocError(statusToCheck, line, file, & 
                                         method, rcToReturn,log)
!
! !RETURN VALUE:
	logical                                 ::ESMF_LogFoundAllocError
! !ARGUMENTS:
!	
	integer, intent(in)                     :: statusToCheck
	integer, intent(in), optional           :: line
	character(len=*), intent(in), optional  :: file
	character(len=*), intent(in), optional  :: method
	integer, intent(out),optional           :: rcToReturn
	type(ESMF_LOG),intent(in),optional	:: log

! !DESCRIPTION:
!      This function returns a logical true when a Fortran status code
!      returned from a memory allocation indicates an allocation error.  
!      An ESMF predefined memory allocation error 
!      message will be added to the {\tt ESMF\_Log} along with {\tt line}, 
!      {\tt file} and {\tt method}.  Additionally, the 
!      {\tt statusToCheck} will be converted to a {\tt rcToReturn}.
!
!      The arguments are:
!      \begin{description}
! 	
!      \item [statusToCheck]
!            Fortran allocation status to check.
!      \item [{[line]}]
!            Integer source line number.  Expected to be set by
!            using the preprocessor macro {\tt \_\_LINE\_\_} macro.
!      \item [{[file]}]
!            User-provided source file name. 
!      \item [{[method]}]
!            User-provided method string.
!      \item [{[rcToReturn]}]
!            If specified, set the {\tt rcToReturn} value to 
!            {\tt ESMF\_RC\_MEM} which is the error code for a memory 
!            allocation eror.
!      \item [{[log]}]
!            An optional {\tt ESMF\_Log} object that can be used instead
!	         of the default log.
!      
!      \end{description}
! 
!EOP
    character(len=ESMF_MAXSTR)::tempmsg
    character(len=ESMF_MAXSTR)::allocmsg
	integer::msglen=0
	
    ESMF_LogFoundAllocError=.FALSE.
    if (statusToCheck .NE. 0) then
        if (present(rcToReturn)) rcToReturn=ESMF_RC_MEM
        call c_esmc_loggeterrormsg(ESMF_RC_MEM,tempmsg,msglen)
        allocmsg=tempmsg(1:msglen)
	call ESMF_LogWrite(trim(allocmsg),ESMF_LOG_ERROR,line,file,method,log)
	ESMF_LogFoundAllocError=.TRUE.
    else
        if (present(rcToReturn)) rcToReturn=ESMF_SUCCESS
    endif	
       
end function ESMF_LogFoundAllocError

!--------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_LogFoundError - Check ESMF return code for error

! !INTERFACE: 
	function ESMF_LogFoundError(rcToCheck, line, file, method,& 
	         rcToReturn, log)
!
! !RETURN VALUE:
	logical                                         ::ESMF_LogFoundError
! !ARGUMENTS:
!	
	integer, intent(in)                             :: rcToCheck
	integer, intent(in), optional                   :: line
	character(len=*), intent(in), optional          :: file
	character(len=*), intent(in), optional	        :: method
	integer, intent(out), optional                  :: rcToReturn
	type(ESMF_LOG),intent(in),optional		:: log
	
! !DESCRIPTION:
!      This function returns a logical true for ESMF return codes that indicate 
!      an error.  A predefined error message will added to the {\tt ESMF\_Log} 
!      along with {\tt line}, {\tt file} and {\tt method}.  Additionally, 
!      {\tt rcToReturn} will be set to {\tt rcToCheck}.
!
!      The arguments are:
!      \begin{description}
! 	
!      \item [rcToCheck]
!            Return code to check.
!      \item [{[line]}]
!            Integer source line number.  Expected to be set by
!            using the preprocessor macro {\tt \_\_LINE\_\_} macro.
!      \item [{[file]}]
!            User-provided source file name. 
!      \item [{[method]}]
!            User-provided method string.
!      \item [{[rcToReturn]}]
!            If specified, copy the {\tt rcToCheck} value to {\tt rc}.
!            This is not the return code for this function; it allows
!            the calling code to do an assignment of the error code
!            at the same time it is testing the value.
!      \item [{[log]}]
!            An optional {\tt ESMF\_Log} object that can be used instead
!	     of the default log.
!      
!      \end{description}
! 
!EOP
	
    ESMF_LogFoundError = .FALSE.
    if (present(rcToReturn)) rcToReturn = rcToCheck
    if (rcToCheck .NE. ESMF_SUCCESS) then
        call ESMF_LogWrite("StandardError",ESMF_LOG_ERROR,line,file,method,&
	log)
        ESMF_LogFoundError = .TRUE.
    endif	
       
end function ESMF_LogFoundError

!--------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_LogGet - Return information about a log object

! !INTERFACE: 
	subroutine ESMF_LogGet(log,verbose,flush,rootOnly,halt,logtype,stream,&
	                       maxElements,rc)
!
! !ARGUMENTS:
!	
	type(ESMF_Log), intent(in) 		                :: log
	type(ESMF_Logical), intent(out),optional		:: verbose
	type(ESMF_Logical), intent(out),optional		:: flush
	type(ESMF_Logical), intent(out),optional		:: rootOnly
	type(ESMF_HaltType), intent(out),optional               :: halt
	type(ESMF_LogType), intent(out),optional	        :: logtype
	integer, intent(out),optional			        :: stream  
	integer, intent(out),optional			        :: maxElements
	integer, intent(out),optional			        :: rc
	

! !DESCRIPTION:
!      This subroutine returns properties about a log object.
!
!      The arguments are:
!      \begin{description}
!
!      \item [{[log]}]
!            Log object.
!      \item [{[verbose]}]
!            Verbose flag.
!      \item [{[flush]}]
!            Flush flag.
!      \item [{[rootOnly]}]
!	     Root only flag
!      \item [{[halt]}]
!	     Halt definitions 
!      \item [{[logtype]}]
!            Defines either single or multilog
!      \item [{[stream]}]
!            The type of stream (free(0), preordered(1))
!      \item [{[maxElements]}]
!            Maximum number of elements in the log.
!      \item [{[rc]}]
!            Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!      \end{description}
! 
!EOPI
	if (present(rc)) rc=ESMF_FAILURE
	if (present(verbose)) verbose=log%verbose
	if (present(flush)) flush=log%flushImmediately
	if (present(rootOnly)) rootOnly=log%rootOnly
	if (present(halt)) halt=log%halt
	if (present(logtype)) logtype=log%logtype
	if (present(stream)) stream=log%stream
	if (present(maxElements)) maxElements=log%maxElements	
	if (present(rc)) rc=ESMF_SUCCESS 
end subroutine ESMF_LogGet

!--------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_LogInitialize - Initialize Log file(s)

! !INTERFACE: 
      subroutine ESMF_LogInitialize(filename, lognone, logtype, rc)
!
! !ARGUMENTS:
      character(len=*)                          :: filename
      integer, intent(in),optional		:: lognone  
      type(ESMF_LogType), intent(in),optional  :: logtype  
      integer, intent(out),optional	        :: rc

! !DESCRIPTION:
!      This routine initializes the global default {\tt ESMF\_Log}.  
!      The default {\tt ESMF\_Log} is assigned the {\tt filename} and 
!      is associated with an open Fortran unit number.
!
!      The arguments are:
!      \begin{description}
! 
!      \item [{[filename]}]
!            Name of file.
!      \item [{[lognone]}]
!            Turns off logging if equal to {\tt ESMF\_LOG\_NONE}
!      \item [{[logtype]}]
!            Specifies ESMF\_LOG\_SINGLE or ESMF\_LOG\_MULTI
!      \item [{[rc]}]
!            Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!      \end{description}
! 
!EOPI
	
    integer 				       :: status, i, j, rc2,rc3,un	
    type(ESMF_LOGENTRY), dimension(:,:), pointer :: localbuf
    type(ESMF_LOGARRAY), dimension(:), pointer  :: localbuf2
	
    if (present(rc)) rc=ESMF_FAILURE
    ESMF_LogDefault%logNone = ESMF_FALSE !default is to log
    ESMF_LogDefault%maxElements = 10
    ESMF_LogDefault%halt=ESMF_LOG_HALTNEVER
    ESMF_LogDefault%flushImmediately = ESMF_TRUE
    call ESMF_VMGetGlobal(ESMF_LogDefault%vm,rc3)
    call ESMF_VMGet(ESMF_LogDefault%vm,petCount=ESMF_LogDefault%petCount)
    if (present(lognone)) then
      if (lognone .eq. ESMF_LOG_NONE) ESMF_LogDefault%logNone = ESMF_TRUE 
      ESMF_LogDefault%petCount = 1
    endif
    if (present(logtype)) then
        ESMF_LogDefault%logtype=logtype
        if (ESMF_LogDefault%logtype .eq. ESMF_LOG_SINGLE) then
	    ESMF_LogDefault%petCount=1
	endif
    else
        ESMF_LogDefault%logtype=ESMF_LOG_SINGLE
	ESMF_LogDefault%petCount=1
    endif 
    allocate(localbuf2(ESMF_LogDefault%petCount),stat=status)
    if (status .ne. 0) then
      print *, "allocation of buffer failed"
      if (present(rc)) rc = ESMF_FAILURE
      return
    endif
    ESMF_LogDefault%LOG_ARRAY => localbuf2
    un = ESMF_LOG_FORT_STDOUT
    do i=1, ESMF_LogDefault%petCount
        ESMF_LogDefault%LOG_ARRAY(i)%FileIsOpen=ESMF_FALSE
        ESMF_LogDefault%LOG_ARRAY(i)%flushed = ESMF_FALSE
	ESMF_LogDefault%LOG_ARRAY(i)%dirty = ESMF_FALSE
	ESMF_LogDefault%LOG_ARRAY(i)%fIndex = 1
	if (ESMF_LogDefault%petCount .eq. 1) then
	    ESMF_LogDefault%LOG_ARRAY(1)%nameLogErrFile=trim(filename)
        else
	    filename = "pet" // char(i) // "." // trim(filename)
	    ESMF_LogDefault%LOG_ARRAY(i)%nameLogErrFile=filename
	endif
    if (len(ESMF_LogDefault%LOG_ARRAY(i)%nameLogErrFile) .gt. 32) then
        print *, "filename exceeded 32 characters"
        if (present(rc)) rc = ESMF_FAILURE
        return
    endif
	ESMF_LogDefault%LOG_ARRAY(i)%unitnumber=un   
        do j=un, ESMF_LOG_UPPER
            inquire(unit=j,iostat=status)
            if (status .eq. 0) then
              ESMF_LogDefault%LOG_ARRAY(i)%FileIsOpen = ESMF_TRUE
	      ESMF_LogDefault%LOG_ARRAY(i)%unitNumber= j
	      un = j+1
              exit
	    else
	      if (present(rc)) rc=ESMF_FAILURE
	      return
     	    endif
        enddo          
    enddo
    
    ! BEWARE:  absoft 8.0 compiler bug - if you try to allocate directly
    ! you get an error.  if you allocate a local buffer and then point the
    ! derived type buffer at it, it works.  go figure.
    
    allocate(localbuf(ESMF_LogDefault%petCount, ESMF_LogDefault%maxElements), &
    stat=status)
    if (status .ne. 0) then
      print *, "allocation of buffer failed"
      if (present(rc)) rc = ESMF_FAILURE
      return
    endif
    ESMF_LogDefault%LOG_ENTRY => localbuf
    
    call c_ESMC_LogInitialize(filename,rc2)
    if (present(rc)) rc=ESMF_SUCCESS
	
end subroutine ESMF_LogInitialize

!--------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_LogMsgFoundAllocError - Check Fortran status for allocation error and write message

! !INTERFACE: 
	function ESMF_LogMsgFoundAllocError(statusToCheck,msg,line,file, &
                                            method,rcToReturn,log)
!
! !RETURN VALUE:
	logical                                 ::ESMF_LogMsgFoundAllocError
! !ARGUMENTS:
!	
	integer, intent(in)                     :: statusToCheck
	character(len=*), intent(in)            :: msg
	integer, intent(in), optional           :: line
	character(len=*), intent(in), optional  :: file
	character(len=*), intent(in), optional	:: method
        integer, intent(out),optional           :: rcToReturn	
        type(ESMF_LOG), intent(in), optional	:: log

! !DESCRIPTION:
!      This function returns a logical true when a Fortran status code
!      returned from a memory allocation indicates an allocation error.
!      An ESMF predefined memory allocation error message 
!      will be added to the {\tt ESMF\_Log} along with a user added {\tt msg}, 
!      {\tt line}, {\tt file} and 
!      {\tt method}.  Additionally, statusToCheck will be converted to 
!      {\tt rcToReturn}.
!
!      The arguments are:
!      \begin{description}
! 	
!      \item [{[statusToCheck]}]
!            Fortran allocation status to check.
!      \item [{[msg}]]
!            User-provided message string.
!      \item [{[line]}]
!            Integer source line number.  Expected to be set by
!            using the preprocessor macro {\tt \_\_LINE\_\_} macro.
!      \item [{[file]}]
!            User-provided source file name. 
!      \item [{[method]}]
!            User-provided method string.
!      \item [{[rcToReturn]}]
!            If specified, set the {\tt rcToReturn} value to 
!            {\tt ESMF\_RC\_MEM} which is the error code for a memory 
!            allocation eror.
!      \item [{[log]}]
!            An optional {\tt ESMF\_Log} object that can be used instead
!	         of the default log.
!      
!      \end{description}
! 
!EOP
    character(len=ESMF_MAXSTR)::tempmsg
    character(len=ESMF_MAXSTR)::allocmsg
    integer::msglen=0
    
    ESMF_LogMsgFoundAllocError=.FALSE.
    if (statusToCheck .NE. 0) then
        call c_esmc_loggeterrormsg(ESMF_RC_MEM,tempmsg,msglen)
	if (present(rcToReturn)) rcToReturn=ESMF_RC_MEM
        allocmsg=tempmsg(1:msglen)
	call ESMF_LogWrite(trim(allocmsg)//msg,ESMF_LOG_ERROR,line,file,method,log)	
	ESMF_LogMsgFoundAllocError=.TRUE.
    else
        if (present(rcToReturn)) rcToReturn=ESMF_SUCCESS
    endif	
       
end function ESMF_LogMsgFoundAllocError

!--------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_LogMsgFoundError - Check ESMF return code for error and write message

! !INTERFACE: 
	function ESMF_LogMsgFoundError(rcToCheck, msg, line, file, method, &
                                       rcToReturn, log)
!
! !RETURN VALUE:
	logical                                         ::ESMF_LogMsgFoundError
! !ARGUMENTS:
!	
	integer, intent(in)                             :: rcToCheck
	character(len=*), intent(in)                    :: msg
	integer, intent(in), optional                   :: line
	character(len=*), intent(in), optional          :: file
	character(len=*), intent(in), optional	        :: method
	integer, intent(out),optional                   :: rcToReturn
	type(ESMF_LOG), intent(in), optional		:: log
	

! !DESCRIPTION:
!      This function returns a logical true for ESMF return codes that indicate
!      an error.  A predefined error message will added to the {\tt ESMF\_Log} 
!      along with
!      a user added {\tt msg}, {\tt line}, {\tt file} and {\tt method}.  
!      Additionally, {\tt rcToReturn} will be set to {\tt rcToCheck}.
!
!      The arguments are:
!      \begin{description}
! 	
!      \item [rcToCheck]
!            Return code to check.
!      \item [msg]
!            User-provided message string.
!      \item [{[line]}]
!            Integer source line number.  Expected to be set by
!            using the preprocessor macro {\tt \_\_LINE\_\_} macro.
!      \item [{[file]}]
!            User-provided source file name. 
!      \item [{[method]}]
!            User-provided method string.
!      \item [{[rcToReturn]}]
!            If specified, copy the {\tt rcToCheck} value to {\tt rc}.
!            This is not the return code for this function; it allows
!            the calling code to do an assignment of the error code
!            at the same time it is testing the value.
!      \item [{[log]}]
!            An optional {\tt ESMF\_Log} object that can be used instead
!	         of the default log.
!      
!      \end{description}
! 
!EOP
	
    ESMF_LogMsgFoundError=.FALSE.
	if (present(rcToReturn)) rcToReturn = rcToCheck
	if (rcToCheck .NE. ESMF_SUCCESS) then
	    call ESMF_LogWrite(msg,ESMF_LOG_ERROR,line,file,method,log)
	    ESMF_LogMsgFoundError=.TRUE.
	endif	
       
end function ESMF_LogMsgFoundError

!--------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_LogMsgSetError - Set ESMF return code for error and write msg

! !INTERFACE: 
	subroutine ESMF_LogMsgSetError(rcValue, msg, line, file, method, &
                                       rcToReturn, log)

! !ARGUMENTS:
!	
	integer, intent(in)                             :: rcValue
	character(len=*), intent(in)                    :: msg
	integer, intent(in), optional                   :: line
	character(len=*), intent(in), optional          :: file
	character(len=*), intent(in), optional	        :: method
	integer, intent(out),optional                   :: rcToReturn
	type(ESMF_LOG), intent(in), optional		:: log
	

! !DESCRIPTION:
!      This subroutine sets the {\tt rcToReturn} value to {\tt rcValue} if
!      {\tt rcToReturn} is present and writes this error code to the {\tt ESMF\_Log}
!      if an error is generated.  A predefined error message will added to the 
!      {\tt ESMF\_Log} along with a user added {\tt msg}, {\tt line}, {\tt file}
!      and {\tt method}.  
!
!      The arguments are:
!      \begin{description}
! 	
!      \item [rcValue]
!            rc value for set
!      \item [msg]
!            User-provided message string.
!      \item [{[line]}]
!            Integer source line number.  Expected to be set by
!            using the preprocessor macro {\tt \_\_LINE\_\_} macro.
!      \item [{[file]}]
!            User-provided source file name. 
!      \item [{[method]}]
!            User-provided method string.
!      \item [{[rcToReturn]}]
!            If specified, copy the {\tt rcValue} value to {\tt rcToreturn}.
!            This is not the return code for this function; it allows
!            the calling code to do an assignment of the error code
!            at the same time it is testing the value.
!      \item [{[log]}]
!            An optional {\tt ESMF\_Log} object that can be used instead
!	         of the default log.
!      
!      \end{description}
! 
!EOP
	
    if (present(rcToReturn)) rcToReturn = rcValue
    if (rcValue .NE. ESMF_SUCCESS) then
        call ESMF_LogWrite(msg,ESMF_LOG_ERROR,line,file,method,log)
    endif	
       
end subroutine ESMF_LogMsgSetError

!--------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_LogOpen - Open Log file(s)

! !INTERFACE: 
    subroutine ESMF_LogOpen(log, filename, lognone, logtype, rc)
!
! !ARGUMENTS:
    type(ESMF_Log)			    :: log
    character(len=32)			    :: filename
    integer, intent(in),optional            :: lognone  
    type(ESMF_LogType), intent(in),optional :: logtype  
    integer, intent(out),optional	    :: rc

! !DESCRIPTION:
!      This routine opens a file with {\tt filename} and associates
!      it with the {\tt log}.  This is only
!      used when the user does not want to use the default {\tt ESMF\_Log}.
!
!      The arguments are:
!      \begin{description}
! 
!      \item [{[log]}]
!            An {\tt ESMF\_Log} object.
!      \item [{[filename]}]
!            Name of file.
!      \item [{[lognone]}]
!            Turns off logging if equal to {\tt ESMF\_LOG\_NONE}
!      \item [{[logtype]}]
!            Specifies ESMF\_LOG\_SINGLE or ESMF\_LOG\_MULTI
!      \item [{[rc]}]
!            Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!      \end{description}
! 
!EOP
    integer 				         :: status, i, j, rc2,rc3,un	
    type(ESMF_LOGENTRY), dimension(:,:), pointer :: localbuf
    type(ESMF_LOGARRAY), dimension(:), pointer   :: localbuf2

    if (present(rc)) rc=ESMF_FAILURE
    log%logNone = ESMF_FALSE !default is to log
    log%maxElements = 10
    log%halt=ESMF_LOG_HALTNEVER
    log%flushImmediately = ESMF_TRUE
    call ESMF_VMGetGlobal(log%vm,rc3)
    call ESMF_VMGet(log%vm,petCount=log%petCount)
    if (present(lognone)) then
      if (lognone .eq. ESMF_LOG_NONE) log%logNone = ESMF_TRUE 
      log%petCount = 1
    endif
    if (present(logtype)) then
        log%logtype=logtype
        if (log%logtype .eq. ESMF_LOG_SINGLE) then
	    log%petCount=1
	endif
    else
        log%logtype=ESMF_LOG_SINGLE
	log%petCount=1
    endif 
    allocate(localbuf2(log%petCount),stat=status)
    if (status .ne. 0) then
      print *, "allocation of buffer failed"
      if (present(rc)) rc = ESMF_FAILURE
      return
    endif
    log%LOG_ARRAY => localbuf2
    un = ESMF_LOG_FORT_STDOUT
    do i=1, log%petCount
        log%LOG_ARRAY(i)%FileIsOpen=ESMF_FALSE
        log%LOG_ARRAY(i)%flushed = ESMF_FALSE
	log%LOG_ARRAY(i)%dirty = ESMF_FALSE
	log%LOG_ARRAY(i)%fIndex = 1
	if (log%petCount .eq. 1) then
	    log%LOG_ARRAY(1)%nameLogErrFile=filename
        else
	    filename = "pet" // char(i) // "." // filename
	    log%LOG_ARRAY(i)%nameLogErrFile=filename
	endif
	log%LOG_ARRAY(i)%unitnumber=un   
        do j=un, ESMF_LOG_UPPER
            inquire(unit=j,iostat=status)
            if (status .eq. 0) then
              log%LOG_ARRAY(i)%FileIsOpen = ESMF_TRUE
	      log%LOG_ARRAY(i)%unitNumber= j
	      un = j+1
              exit
	    else
	      if (present(rc)) rc=ESMF_FAILURE
	      return
     	    endif
        enddo          
    enddo
    
    ! BEWARE:  absoft 8.0 compiler bug - if you try to allocate directly
    ! you get an error.  if you allocate a local buffer and then point the
    ! derived type buffer at it, it works.  go figure.
    
    allocate(localbuf(log%petCount, log%maxElements), &
    stat=status)
    if (status .ne. 0) then
      print *, "allocation of buffer failed"
      if (present(rc)) rc = ESMF_FAILURE
      return
    endif
    log%LOG_ENTRY => localbuf
    
    call c_ESMC_LogInitialize(filename,rc2)
    if (present(rc)) rc=ESMF_SUCCESS    
    
end subroutine ESMF_LogOpen	

!--------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_LogSet - Set Log parameters

! !INTERFACE: 
	subroutine ESMF_LogSet(log,verbose,flush,rootOnly,halt, &
                               stream,maxElements,rc)
!
! !ARGUMENTS:
!	
	type(ESMF_Log) 				   		            :: log
	type(ESMF_Logical), intent(in),optional			:: verbose
	type(ESMF_Logical), intent(in),optional			:: flush
	type(ESMF_Logical), intent(in),optional			:: rootOnly
	type(ESMF_HaltType), intent(in),optional        :: halt
	integer, intent(in),optional			        :: stream  
	integer, intent(in),optional			        :: maxElements
	integer, intent(out),optional			        :: rc
	
! !DESCRIPTION:
!      This subroutine sets the properties for the log object.
!
!      The arguments are:
!      \begin{description}
!
!      \item [{[log]}]
!            Log object.
!      \item [{[verbose]}]
!            Verbose flag.
!      \item [{[rootOnly]}]
!	         Root only flag
!      \item [{[halt]}]
!	         Halt definitions {\tt ESMF\_HALTWARNING}, {\tt ESMF\_HALTERROR},{\tt ESMF\_HALTNEVER}
!      \item [{[stream]}]
!            The type of stream (free(0), preordered(1))
!      \item [{[maxElements]}]
!            Maximum number of elements in the log.
!      \item [{[rc]}]
!            Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!      \end{description}
! 
!EOPI 
    integer :: status
    type(ESMF_LOGENTRY), dimension(:,:), pointer :: localbuf
	if (present(rc)) rc=ESMF_FAILURE
	if (present(verbose)) log%verbose=verbose
	if (present(flush)) log%flushImmediately=flush
	if (present(rootOnly)) log%rootOnly=rootOnly
	if (present(halt)) log%halt=halt
	if (present(stream)) log%stream=stream
	if (present(maxElements) .and. maxElements .gt. 0) then
	    if (log%maxElements .ne. maxElements) then
	        allocate(localbuf(ESMF_LogDefault%petCount, maxElements), &
                     stat=status)
                ! TODO: copy old contents over, or flush first!!
	        deallocate(log%LOG_ENTRY,stat=status)
                log%LOG_ENTRY => localbuf
		log%maxElements=maxElements
	    endif
	endif    
	if (present(rc)) rc=ESMF_SUCCESS 
end subroutine ESMF_LogSet

!--------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_LogWrite - Write to Log file(s)

! !INTERFACE: 
	subroutine ESMF_LogWrite(msg,MsgType,line,file,method,log,rc)
!
!
! !ARGUMENTS:
	character(len=*), intent(in)            :: msg
	type(ESMF_MsgType), intent(in)          :: msgtype
	integer, intent(in), optional           :: line
	character(len=*), intent(in), optional  :: file
	character(len=*), intent(in), optional	:: method
	type(ESMF_LOG), intent(in), optional	:: log
	integer, intent(out),optional		    :: rc

! !DESCRIPTION:
!      This subroutine writes to the file associated with an {\tt ESMF\_Log}.
!      A message is passed in along with the {\tt msgtype}, {\tt line}, 
!      {\tt file} and {\tt method}.  If the write to the {\tt ESMF\_Log}
!      is successful, the function will return a logical {\tt true}.  This 
!      function is the base function used by all the other {\tt ESMF\_Log} 
!      writing methods.
!
!      The arguments are:
!      \begin{description}
! 
!      \item [msg]
!            User-provided message string.
!      \item [msgtype]
!            The type of message.  See Section~\ref{opt:msgtype} for
!            possible values.
!      \item [{[line]}]
!            Integer source line number.  Expected to be set by
!            using the preprocessor macro {\tt \_\_LINE\_\_} macro.
!      \item [{[file]}]
!            User-provided source file name. 
!      \item [{[method]}]
!            User-provided method string.
!      \item [{[log]}]
!            An optional {\tt ESMF\_Log} object that can be used instead
!	         of the default log.
!      \item [{[rc]}]
!            Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!      \end{description}
! 
!EOP
    
    character(len=10)               :: t
    character(len=8)                :: d
    !character(len=7)                :: lt
    character(len=32)               ::tmethod,tfile
    integer			    ::tline
    integer                         ::h,m,s,ms,y,mn,dy
    integer			    ::rc2
    integer                         ::petid
    
    if (ESMF_LogDefault%LogType .eq. ESMF_LOG_SINGLE) then
        petid=1
    else
        call ESMF_VMGet(ESMF_LogDefault%vm,localpet=petid)
	petid=petid+1
    endif	
    
    if (ESMF_LogDefault%logNone .ne. ESMF_TRUE) then
    ESMF_LogDefault%LOG_ARRAY(petid)%dirty = ESMF_TRUE
    call c_esmc_timestamp(y,mn,dy,h,m,s,ms)
    call DATE_AND_TIME(d,t)	
    if (present(rc)) rc=ESMF_FAILURE
    ESMF_LogDefault%LOG_ENTRY(petid,ESMF_LogDefault%LOG_ARRAY(petid)%findex)%methodflag = .FALSE.
    ESMF_LogDefault%LOG_ENTRY(petid,ESMF_LogDefault%LOG_ARRAY(petid)%findex)%lineflag = .FALSE.
    ESMF_LogDefault%LOG_ENTRY(petid,ESMF_LogDefault%LOG_ARRAY(petid)%findex)%fileflag = .FALSE.
    if (present(method)) then
        tmethod=adjustl(method)
	ESMF_LogDefault%LOG_ENTRY(petid,ESMF_LogDefault%LOG_ARRAY(petid)%findex)%methodflag=.TRUE.
	ESMF_LogDefault%LOG_ENTRY(petid,ESMF_LogDefault%LOG_ARRAY(petid)%findex)%method = tmethod
    endif	
    if (present(line)) then
        tline=line 
	ESMF_LogDefault%LOG_ENTRY(petid,ESMF_LogDefault%LOG_ARRAY(petid)%findex)%lineflag = .TRUE.
	ESMF_LogDefault%LOG_ENTRY(petid,ESMF_LogDefault%LOG_ARRAY(petid)%findex)%line = tline
    endif	
    if (present(file)) then
        tfile=adjustl(file)
	ESMF_LogDefault%LOG_ENTRY(petid,ESMF_LogDefault%LOG_ARRAY(petid)%findex)%fileflag = .TRUE.
	ESMF_LogDefault%LOG_ENTRY(petid,ESMF_LogDefault%LOG_ARRAY(petid)%findex)%file = tfile
    endif
    select case (msgtype%mtype)
        case (1)
    	    ESMF_LogDefault%LOG_ENTRY(petid,ESMF_LogDefault%LOG_ARRAY(petid)%findex)%lt="INFO"
        case (2)
    	    ESMF_LogDefault%LOG_ENTRY(petid,ESMF_LogDefault%LOG_ARRAY(petid)%findex)%lt="WARNING"
   	case default
    	    ESMF_LogDefault%LOG_ENTRY(petid,ESMF_LogDefault%LOG_ARRAY(petid)%findex)%lt="ERROR"
    end select	
    ESMF_LogDefault%LOG_ENTRY(petid,ESMF_LogDefault%LOG_ARRAY(petid)%findex)%d = d
    ESMF_LogDefault%LOG_ENTRY(petid,ESMF_LogDefault%LOG_ARRAY(petid)%findex)%h = h
    ESMF_LogDefault%LOG_ENTRY(petid,ESMF_LogDefault%LOG_ARRAY(petid)%findex)%m = m
    ESMF_LogDefault%LOG_ENTRY(petid,ESMF_LogDefault%LOG_ARRAY(petid)%findex)%s = s
    ESMF_LogDefault%LOG_ENTRY(petid,ESMF_LogDefault%LOG_ARRAY(petid)%findex)%ms = ms	
    ESMF_LogDefault%LOG_ENTRY(petid,ESMF_LogDefault%LOG_ARRAY(petid)%findex)%msg = msg
    if ((ESMF_LogDefault%halt .eq. ESMF_LOG_HALTERROR).and. (msgtype .eq. ESMF_LOG_ERROR)) then
        ESMF_LogDefault%LOG_ENTRY(petid,ESMF_LogDefault%LOG_ARRAY(petid)%findex)%stopprogram=.TRUE.
        call ESMF_LogFlush(ESMF_LogDefault,petnum=petid,rc=rc2)
    endif    	 
    if ((ESMF_LogDefault%halt .eq. ESMF_LOG_HALTWARNING).and. (msgtype .gt. ESMF_LOG_WARNING)) then
        ESMF_LogDefault%LOG_ENTRY(petid,ESMF_LogDefault%LOG_ARRAY(petid)%findex)%stopprogram=.TRUE.
	call ESMF_LogFlush(ESMF_LogDefault, petnum=petid,rc=rc2)
    endif
    if (ESMF_LogDefault%LOG_ARRAY(petid)%findex .eq. ESMF_LogDefault%maxElements) then
        call ESMF_LogFlush(ESMF_LogDefault, petnum=petid,rc=rc2) 
	ESMF_LogDefault%LOG_ARRAY(petid)%findex = 1
    else
        ESMF_LogDefault%LOG_ARRAY(petid)%findex = ESMF_LogDefault%LOG_ARRAY(petid)%findex + 1	
    endif	
    ESMF_LogDefault%LOG_ARRAY(petid)%flushed = ESMF_FALSE
    endif
    if (present(rc)) rc=ESMF_SUCCESS
end subroutine ESMF_LogWrite

end module ESMF_LogErrMod

