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
! $Id: MAPL_Mod.F90,v 1.13 2013-03-16 00:09:04 atrayano Exp $


module MAPL_Mod

  use ESMFL_Mod         !  Stopgap
  use MAPL_BaseMod
  use MAPL_IOMod
  use MAPL_CFIOMod
  use MAPL_CommsMod
  use MAPL_LocStreamMod
  use MAPL_GenericMod
  use MAPL_VarSpecMod
  use MAPL_ConstantsMod
  use MAPL_SortMod
  use MAPL_ProfMod
  use MAPL_SunMod
  use MAPL_LocStreamMod
  use MAPL_InterpMod
  use MAPL_HeapMod
  use MAPL_SatVaporMod
  use MAPL_HorzTransformMod
  use MAPL_CapMod
  use MAPL_MemUtilsMod
  use MAPL_HashMod
  use MAPL_LoadBalanceMod

end module MAPL_Mod
