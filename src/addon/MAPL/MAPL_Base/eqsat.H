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


if    (TX<=TMIN(PHASE)) then
   TI = TMIN(PHASE)
elseif(TX>=TMAX(PHASE)) then
   TI = TMAX(PHASE)
else
   TI = TX
end if

if(MXRT) then
   EF = 1.0
else
   EF = 1.0-ESFAC
endif
 
if(UTBL) then ! Use table

! Initialize table, if necessary

   if(.not.TableReady) call MAPL_EQsatSET

! Get saturation vapor pressure from table

   TI  = (TI - TMINTBL)*DEGSUBS
   IT  = ceiling(TI)
   DDQ = TABLE(IT+1) - TABLE(max(IT,1))
   EX  = TABLE(IT+1) - DDQ*(IT-TI)

! Compute saturation specific humidity

   if(present(PL)) then
      if(PX > MINPFAC*EX) then
         DD  = 1.0/(PX - EF*EX)
         EX  = ESFAC*EX*DD
         DDQ = ESFAC*PX*DD*DD*DDQ
      else
         if(MXRT) then
           EX  = MAX_RS
         else
           EX  = MAX_QS
         endif
         DDQ = 0.0
      end if
   end if

else  ! Use exact formulation

! Saturation vapor pressure

#include FILE_

! Derivative of esat from finite difference

   if(present(DQ)) then
      if    (TI==TMIN(PHASE)) then
         DDQ = 0.0
      elseif(TI==TMAX(PHASE)) then
         DDQ = 0.0
      else
         DD  = EX
         TI  = TX + DELTA_T
#include FILE_
         DDQ = EX-DD
         EX  = DD
      end if
   end if

! Compute saturation specific humidity

   if(present(PL)) then
      if(PX > MINPFAC*EX) then
         DD  = 1.0/(PX - EF*EX)
         EX  = ESFAC*EX*DD
         DDQ = ESFAC*PX*DD*DD*DDQ
      else
         if(MXRT) then
           EX  = MAX_RS
         else
           EX  = MAX_QS
         endif
         DDQ = 0.0
      end if
   end if

endif ! not table

if(present(DQ)) DX = DDQ*DEGSUBS

