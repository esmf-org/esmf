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
   TT = TI-ZEROC
   EX = (TT*(TT*(TT*(TT*(TT*(TT*B6+B5)+B4)+B3)+B2)+B1)+B0)

case (GoffGratch)
   TT = TS/TI
   EX = 10.0**(  DL(1)*(TT - 1.0) + DL(2)*log10(TT)  + &
        DL(3)*(10.0**(DL(4)*(1.0 - (1.0/TT))) - 1.0) + &
        DL(5)*(10.0**(DL(6)*(TT  -  1.0    )) - 1.0) + &
        LOGPS + 2.0                                           )

case (MurphyKoop)

   EX = exp(                   (CL(0)+CL(1)/TI+CL(2)*log(TI)+CL(3)*TI) + &
        tanh(CL(4)*(TI-CL(5)))*(CL(6)+CL(7)/TI+CL(8)*log(TI)+CL(9)*TI)   )

end select
