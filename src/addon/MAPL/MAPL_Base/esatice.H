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

select case (TYPE)

case (Starr)
   TT = TI - ZEROC
   if    (TT < TSTARR1                   ) then
      EX = (TT*(TT*(TT*(TT*(TT*(TT*S16+S15)+S14)+S13)+S12)+S11)+S10)
   elseif(TT >= TSTARR1 .and. TT < TSTARR2) then
      W = (TSTARR2 - TT)/(TSTARR2-TSTARR1)
      EX =       W *(TT*(TT*(TT*(TT*(TT*(TT*S16+S15)+S14)+S13)+S12)+S11)+S10) &
           + (1.-W)*(TT*(TT*(TT*(TT*(TT*(TT*S26+S25)+S24)+S23)+S22)+S21)+S20)
   elseif(TT >= TSTARR2 .and. TT < TSTARR3) then
      EX = (TT*(TT*(TT*(TT*(TT*(TT*S26+S25)+S24)+S23)+S22)+S21)+S20)
   elseif(TT >= TSTARR3 .and. TT < TSTARR4) then
      W = (TSTARR4 - TT)/(TSTARR4-TSTARR3)
      EX =       W *(TT*(TT*(TT*(TT*(TT*(TT*S26+S25)+S24)+S23)+S22)+S21)+S20) &
           + (1.-W)*(TT*(TT*(TT*(TT*(TT*(TT*BI6+BI5)+BI4)+BI3)+BI2)+BI1)+BI0)
   else
      EX = (TT*(TT*(TT*(TT*(TT*(TT*BI6+BI5)+BI4)+BI3)+BI2)+BI1)+BI0)
   endif

case (GoffGratch)
   TT = ZEROC/TI
   EX = DI(0) * exp( -(DI(1)/TT + DI(2)*log(TT) + DI(3)*TT) )

case (MurphyKoop)
   EX = exp( CI(0)+ CI(1)/TI + CI(2)*log(TI) + CI(3)*TI )

end select
