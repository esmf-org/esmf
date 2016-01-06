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
!  $Id: MAPL_NominalOrbitsMod.F90,v 1.1 2011-02-25 21:48:02 adasilva Exp $

  MODULE MAPL_NominalOrbitsMod 

      IMPLICIT NONE

!BOP

! !MODULE: Orbits_Mod --- Orbital tracks and masks for Polar Orbiters
      
      PRIVATE

! !PUBLIC MEMBER FUNCTIONS:
      PUBLIC Orbits_Track   ! Generate satellite ground tracks
      PUBLIC Orbits_Swath    ! Generate satellite ground tracks' mask
      PUBLIC Orbits_Track0  ! Information provider for track
!     PUBLIC Orbits_Mask0   ! Information provider for mask
 
#ifdef __PROTEX__
  !DESCRIPTION:  This module provides necessary functions for generating
                 ground tracks, and corresponding masks for polar orbiter 
                 satellites.

  REVISION HISTORY:

  07Jul2009   Albayrak  Initial implementation. \\
  30Jul2009   Albayrak  Beta version 2 implemantation

#endif

!EOP     

!     $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
!     Internal constants (not public!)
!     $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$


       INTEGER, PARAMETER :: dp=SELECTED_REAL_KIND(15,307)



!     1- Satellite names
       INTEGER, PARAMETER :: Aqua     = 1
       INTEGER, PARAMETER :: Calipso  = 2
       INTEGER, PARAMETER :: CloudSat = 3
       INTEGER, PARAMETER :: Aura     = 4
       INTEGER, PARAMETER :: Terra    = 5


!     2- Extrapolation Coef
       INTEGER, PARAMETER :: Num_sat  = 5    ! Number of satellites 
       INTEGER, PARAMETER :: Num_coef = 9    ! extrapolation coef  
       REAL(dp), DIMENSION (Num_sat*Num_coef), PARAMETER :: Coefs = (/ &   !Aqua
         91.499324355103511,  -3.203889864874015,   0.000273159230172, &
         91.499293811827172,  -0.377148321262191,   0.000091480174841, &
         91.499384350660918,   1.280726447439133,  -0.001159920580549, &

         91.499573289741733,  -3.297445861946248,   0.000239609802876, &  !Calipso
         91.499584024805969,  -0.460273580159880,   0.000125365994414, &
         91.499608453633982,   1.205360260436871,  -0.001192777696086, &

         91.499480902088152,  -3.285668060865482,   0.000203038327945, &  !CloudSat
         91.499455508570492,  -0.448712225445062,   0.000158569625164, &
         91.499464053621409,   1.216826349594128,  -0.001182646395663, &

         91.499391954502713,   2.573431689964800,   0.000258633680290, &  !Aura 
         91.499394689118887,  -0.873061358923764,   0.000110177145433, &
         91.499402404413175,   0.792159351805744,  -0.001228074893676, &

         91.499598756142376,  -1.925320207499946,  -0.000582299168678, &   !Terra
         91.499618148202927,  -1.413146107480887,   0.000030982760855, &
         91.499493908174912,  -3.023526958545179,  -0.001275012087734  &


         /)

!      Earth/Time related constants 
       REAL(dp), PARAMETER  :: g0 = 0.000072921158553     !rad per seconds
       REAL(dp), PARAMETER  :: g1p = 0.0172027912;  
                           ! increase in the lon of GR per day (rad/day)
!      g2p need to be calculated (optimized) version in matlab values are copied here  
!      Rotational rate of the earth rad per day; this is calculated for
!      each sat. Think of a parameter ....
       REAL(dp), DIMENSION(Num_sat), PARAMETER :: g2p = (/                 &
          6.283200, &  !Aqua 
          6.283295, &  !Calipso
          6.283200, &  !CloudSat
          6.283185, &  !Aura
          6.283190  &  !Terra
        /) 
       INTEGER, PARAMETER :: Num_normcoef = 3    ! extrapolation coef  
!      Learning parameters
       REAL(dp), DIMENSION(1:Num_sat*3), PARAMETER :: Norm_factor = (/           &
          3.426270004720844e+06, 5.456731200281643e+06, 6.292978192496602e+06, &   !Aqua
          3.627175963236969e+06, 5.325345682183397e+06, 6.292978192496602e+06, &   !Calipso
          3.622642340324375e+06, 5.328452165105113e+06, 6.292978192496602e+06, &   !CloudSat
          3.615314416474894e+06, 5.333592634678283e+06, 6.292978192496602e+06, &   !Aura
          1.922132316029709e+06, 6.149792370758657e+06, 6.292978192496602e+06  &   !Terra
        /) 




!        INTEGER, PARAMETER   :: Day2year = 81  ! First day of the year for learning  
!        INTEGER, PARAMETER   :: RefDate  = 20090323 ! this is date learning started
!        INTEGER, PARAMETER   :: RefTime  = 000000
       INTEGER, DIMENSION(1:Num_sat), PARAMETER   :: Day2year = (/ 281, 281, 281, 281, 281/)  ! First day of the year for learning  
       INTEGER, DIMENSION(1:Num_sat), PARAMETER   :: RefDate  = (/20091009, 20091009,20091009,20091009,20091009/) ! this is date learning started
       INTEGER, DIMENSION(1:Num_sat), PARAMETER   :: RefTime  = (/000000,000000,000000,000000,210000/)


!     3-Other Coef
       REAL(dp), PARAMETER :: pi=3.141592653589793
       REAL(dp), PARAMETER :: earth_radius=6371.0  !sphere
       REAL(dp), PARAMETER :: myeps = 0.0000001    !for zero checks
       REAL(dp), PARAMETER :: cdeg2km = earth_radius * (pi / 180.0) ! constant for deg2km 
       REAL(dp), PARAMETER :: ckm2deg = (1.0/earth_radius) * (180.0/pi)



!     $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
!     END OF INTERNAL CONSTANTS 
!     $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

!    Definition of overloaded functions ...........
       interface linspace
             module procedure linspace1
             module procedure linspace2
       end interface

       interface Orbits_Track0
             module procedure Orbits_Track1
             module procedure Orbits_Track2
       end interface             



!     ERROR codes
!      0 - OK
!      3 - ALLOCATE (memory) problem
!     90 - Swath width is not given correctly
!     99 - Satellite name is not correct




CONTAINS
!-------------------------------------------------------------------------
!BOP
 
! !IROUTINE: Orbits_Track --- Calculates satellite ground tracks
!
! !IIROUTINE: Orbits_Track --- Ground tracks for built-in satellites

!
! !INTERFACE:
!

  SUBROUTINE Orbits_Track(lons, lats, Sat_name, nymd, nhms, deltat, rc)

       IMPLICIT NONE
! !INPUT PARAMETERS:
      character(len=*), intent(in) :: Sat_name ! Satellite name
      integer, intent(in) :: nymd(2)  ! Beginning/ending date: YYYYMMDD
      integer, intent(in) :: nhms(2)  ! Beginning/ending time: HHMMSS
      integer, intent(in) :: deltat   ! Time step [secs]

! !OUTPUT PARAMETERS:
      real(dp), pointer       :: lons(:)  ! Ground track longitudes [degrees]  
      real(dp), pointer       :: lats(:)  ! Ground track latitudes  [degrees]  
      integer, intent(out):: rc       ! Error code = 0 all is well
                                      !            = 3 memory allocation error
      REAL(dp)    :: time_day


#ifdef ___PROTEX___
!
   !DESCRIPTION:  This routine  calculates the satellite ground track during 
   a given time interval. These calculations are performed using pre-calculated 
   coefficients using combination of coordinate transformations, optimization 
   techniques and mathematical modeling techniques.

#endif
!EOP


!     Other parameters!
      REAL(dp), DIMENSION(:), ALLOCATABLE  :: t1_l, lat_l, lon_l, obstime
      real(dp)    :: sim_start, sim_end
      real(dp)    :: s_fraction2day, e_fraction2day
      integer     :: i, flagauto
      INTEGER     :: ts, las, los, time, sattemp
      integer     :: Sat
      integer     :: ierr1, ierr2, ierr3
      REAL(dp)    :: distlat  !lat is equal dis 
      INTEGER     :: say                   !()()()()()()
!     ............................................................................

      rc = 0 ! Initiate error code to 0
      CALL satname2int(Sat_name, Sat, rc)
      if ( rc /= 0 ) return
      CALL get_time(Sat, nymd(1), nhms(1), s_fraction2day, sim_start) ! handle time 
      CALL get_time(Sat, nymd(2), nhms(2), e_fraction2day, sim_end)

      time_day = deltat/(24.0*60.0*60.0)  !()()()()
      CALL built_vecsize(sim_start, sim_end, time_day, say) ! ()()()()()

      ALLOCATE(lons(1:say),  stat=ierr1) ! deallocate in the driver
      ALLOCATE(lats(1:say),  stat=ierr2) ! deallocate in the driver
      ALLOCATE(obstime(1:say),stat=ierr3) ! deallocate in the driver
      if ( ierr1 /= 0 .or. ierr2 /= 0 .or.  ierr3 /= 0 ) then
         rc = 3 !
         print*, rc
         return
      endif

      distlat = 1 !for flagauto 0 option it is not used. later change to optional 
      CALL find_waypointslat(Sat, deltat,sim_start,sim_end,distlat, &
                             obstime, lats, lons, ts, las, los, rc )
      if (rc>0) return

END SUBROUTINE Orbits_Track 


! SUBROUTINE Orbits_Track0(Sat_name)
!     IMPLICIT NONE
! 
!       character(len=*), intent(in) :: Sat_name ! Satellite name
!       integer :: Sat
! 
!       Sat = satname2int(Sat_name)   ! Get satellite coef   
!       print*, "       "
!       print*, "----------------------------------------------------------"
!       print*, "satellite name: ", Sat_name, " and carresponding Sat number: ", Sat
!       print*, "Learning is performed on:", RefDate, " at:", 000000, " hour/min/sec"
!       print*, "This is the ", Day2year, " th day of the year"   
! 
!       print*, "----------------------------------------------------------"
!       print*, "      "
! 
! END SUBROUTINE Orbits_Track0


!       subroutine orb_getsat(sat,sat_name,rc)
!        integer, intent(in)  :: sat
!        integer, intent(out) :: rc
!        character(len=*), intent(out) :: sat_name
!           
!              if ( sat == AQUA     ) then
!                                          sat_name = "Aqua"
!        else  if ( sat == CALIPSO  ) then
!                                          sat_name = "Calipso"
!        else  if ( sat == CLOUDSAT ) then
!                                          sat_name = "Cloudsat"
!        else  if ( sat == AURA     ) then
!                                          sat_name = "Aura"
!        else
!              rc = 99
!              return
!        end if
!        rc = 0
!        end


Subroutine satname2int(name, satnum, rc)
       IMPLICIT NONE
       character(len=*), intent(in) :: name ! Satellite name
       integer, intent(out) :: satnum       ! Satellite number
       integer, intent(out) :: rc

       rc = 0
       if ((name.EQ."AQUA").or.(name.EQ."aqua").or.(name.EQ."Aqua")) then         
           satnum = 1
       elseif ((name.EQ." calipso").or.(name.EQ."CALIPSO").or.(name.EQ."Calipso")) then         
           satnum = 2
       elseif ((name.EQ." cloudsat").or.(name.EQ."CLOUDSAT").or.(name.EQ."CloudSat")) then         
           satnum = 3
       elseif ((name.EQ." aura").or.(name.EQ."AURA").or.(name.EQ."Aura")) then         
           satnum = 4
       elseif ((name.EQ." terra").or.(name.EQ."TERRA").or.(name.EQ."Terra")) then         
           satnum = 5
       else 
          rc =99
          return
       endif
end Subroutine satname2int

SUBROUTINE Orbits_Track1(name, Satcoef_vec1, Normcoef_vec1)
    ! See interface
    IMPLICIT NONE

      character(len=*), intent(in) :: name ! Satellite name
      real(dp), intent(out), dimension(1:Num_coef) :: Satcoef_vec1
      real(dp), intent(out), dimension(1:Num_normcoef) :: Normcoef_vec1
      integer                               :: start_ind, end_ind
      integer                               :: Normcoef_ind
      integer                               :: iSat
      integer                               :: rc
!     ...................................
      ! Num_sat, Num_coef needs to be defined at the begining of the module 
      ! Num_coef is 9 for each sat...
      ! Also Norm factor coef. need to be retrived. 
      ! Here locations of these vectors are calculated
!     ...................................
        CALL satname2int(name, iSat, rc)
        if ( rc /= 0 ) return
        start_ind = (iSat - 1) * Num_coef + 1 !CALL calc_ind
        end_ind  = start_ind +  (Num_coef-1) 
        Satcoef_vec1(1:9) = Coefs(start_ind:end_ind) ! x,y,z


!       % ________________Mult by Norm fator_________________________
        !Normcoef_ind = (iSat-1)*Num_sat+1
        Normcoef_ind = iSat*Num_normcoef-(Num_normcoef-1)
!         print*, Normcoef_ind
!         print*, start_ind, end_ind
        Normcoef_vec1 = (/Norm_factor(Normcoef_ind),       &
                         Norm_factor(Normcoef_ind+1),     &
                         Norm_factor(Normcoef_ind+2)/)

END SUBROUTINE Orbits_Track1

SUBROUTINE Orbits_Track2(iSat, Satcoef_vec, Normcoef_vec)
    ! See interface
    IMPLICIT NONE

      integer, intent(in) :: iSat ! this is the sat number ex: Aqua=1
      real(dp), intent(out), dimension(1:Num_coef) :: Satcoef_vec
      real(dp), intent(out), dimension(1:Num_normcoef) :: Normcoef_vec
      integer                               :: start_ind, end_ind
      integer                               :: Normcoef_ind
!     ...................................
      ! Num_sat, Num_coef needs to be defined at the begining of the module 
      ! Num_coef is 9 for each sat...
      ! Also Norm factor coef. need to be retrived. 
      ! Here locations of these vectors are calculated
!     ...................................

        start_ind = (iSat - 1) * Num_coef + 1 !CALL calc_ind
        end_ind  = start_ind +  (Num_coef-1) 
        Satcoef_vec(1:9) = Coefs(start_ind:end_ind) ! x,y,z

!       % ________________Mult by Norm fator_________________________
        !Normcoef_ind = (iSat-1)*Num_sat+1
        Normcoef_ind = iSat*Num_normcoef-(Num_normcoef-1)
!         print*, Normcoef_ind
!         print*, start_ind, end_ind
        Normcoef_vec = (/Norm_factor(Normcoef_ind),       &
                         Norm_factor(Normcoef_ind+1),     &
                         Norm_factor(Normcoef_ind+2)/)

END SUBROUTINE Orbits_Track2


! &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
! &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&


    SUBROUTINE Orbits_Swath (slons, slats, Sat_name, nymd, nhms, deltat, &
                           SwathWidth, rc, wrap)
! SUBROUTINE Orbits_Swath (slons, slats, Sat_name, nymd, nhms, deltat, SwathWidth, wrapon, rc)

      IMPLICIT NONE
! !INPUT PARAMETERS:
      real(dp),    pointer         :: slons(:,:)      !Track longitude [degree]
      real(dp),    pointer         :: slats(:,:)      !Track latitude  [degree]
      character(len=*),intent(in)  :: Sat_name        !Satellite name Aqua etc ...
      integer,  intent(in)         :: nymd(2)         !Beginning/ending date: YYYYMMDD
      integer,  intent(in)         :: nhms(2)         !Beginning/ending time: HHMMSS
      REAL(dp), INTENT(IN)         :: SwathWidth(:)   !Right, left swath [km]
      integer, intent(in)          :: deltat          ! Time step [secs]
      LOGICAL, intent(in), optional:: wrap       ! if true then wrap to -180 to 180

      !LOGICAL                      :: wrapon       ! if true then wrap to -180 to 180


! !OUTPUT PARAMETERS:
      integer, intent(out)        :: rc

!     Other parameters!

      real(dp),    pointer  :: lons(:)      !Ground Track longitude [degree]
      real(dp),    pointer  :: lats(:)      !Ground Track latitude  [degree]
      REAL(dp),    DIMENSION(:,:,:), ALLOCATABLE  :: latlonshift
      real(dp)              :: dist2creat, temp1
      integer, parameter    :: iii = 3      !this is used for the number of swaths
                                            !here is 3 indicating mid left right
      integer               :: Sat          ! Satellite number
      integer               :: ierr1

      logical               :: wrapon

      if ( present(wrap) ) then
           wrapon = wrap
      else                 
           wrapon = .true.
      end if

      rc = 0 ! Initiate error code to 0
      CALL Orbits_Track(lons, lats, Sat_name, nymd, nhms, deltat, rc) !rc will return 3
      if ( rc /= 0 ) return
      CALL satname2int(Sat_name, Sat, rc)  ! name to sat number rc will return 99
      if ( rc /= 0 ) return

!     Check if the Swath width is given correctly
      if ( SwathWidth(1)<0.AND.SwathWidth(2) < 0 ) then
         rc = 90
         return
      else if (SwathWidth(1)<0.AND.SwathWidth(2) >= 0) then
         temp1 = SwathWidth(2) + SwathWidth(1)
         if (temp1<=0) then
            rc = 90
            return
         end if
      else if (SwathWidth(2)<0.AND.SwathWidth(1) >= 0) then
         temp1 = SwathWidth(1) + SwathWidth(2)
         if (temp1<=0) then
            rc = 90
            return
         end if
      end if

!     2- Find sweep points
      if (allocated(latlonshift)) deallocate(latlonshift) ! this is main
      ALLOCATE(latlonshift(iii, SIZE(lats) ,2), stat = ierr1 ) ! either is ok size of lat or lon
      if ( ierr1 /= 0 ) then
          rc = 3 !
          return
      endif
      latlonshift = 0
      !CALL find_sweeppoints(iii,lat_l(1:las),lon_l(1:los), real(swath(1),dp),  &
      !                  real(swath(2),dp), latlonshift)
      CALL find_sweeppoints(iii, lats, lons, -SwathWidth(1), SwathWidth(2), wrapon, latlonshift)
      ALLOCATE(slats(iii, SIZE(lats)), stat = ierr1 ) ! either is ok size of lat or lon
      ALLOCATE(slons(iii, SIZE(lons)), stat = ierr1 ) ! either is ok size of lat or lon

      slats(1:iii, 1:SIZE(lats)) = latlonshift(1:iii, 1:SIZE(lats) ,1)
      slons(1:iii, 1:SIZE(lons)) = latlonshift(1:iii, 1:SIZE(lons) ,2)
      slats(2,1:SIZE(lats))  = lats(1:SIZE(lats))  ! slats(1:3, :) 1- right 2- original ground track 3-left
      slons(2, 1:SIZE(lons)) = lons(1:SIZE(lons))

      DEALLOCATE(latlonshift, stat = ierr1)
      if (ierr1 /= 0) then
        rc = 3 !
        return
      endif       

  END SUBROUTINE Orbits_Swath


! &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
! &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&



! &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
! &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

!-------------------------------------------------------------------------


!                         -----------------
!                         INTERNAL ROUTINES
!                         -----------------
!-------------------------------------------------------------------------

Subroutine do_gridcorrections(lon_vec_centers1, lat_vec_centers1, &
                              lon_vec_centers, lat_vec_centers, rc)
       IMPLICIT NONE
       REAL(dp), DIMENSION(:), INTENT(IN) :: lon_vec_centers1, lat_vec_centers1
       REAL(dp), DIMENSION(:), INTENT(INOUT) :: lon_vec_centers, lat_vec_centers
       integer, intent(inout) :: rc
!      one need to have data center analysis example lat has to be in between
!      90 to -90, lon has to be in between -180 to 180

       if (size(lon_vec_centers1) < 30) then
             print*, " you need to have minimum of 30 members in lon centers"
             stop
       end if
       if (size(lat_vec_centers1) < 30) then
             print*, " you need to have minimum of 30 members in lat centers"
             stop
       end if

       lon_vec_centers = lon_vec_centers1

       if ( (lon_vec_centers1(1)>180) .OR. (lon_vec_centers1(size(lon_vec_centers1))>180) ) then
              lon_vec_centers(1:size(lon_vec_centers1)) = lon_vec_centers1(1:size(lon_vec_centers1)) - 180.00
       end if
       if (lon_vec_centers1(1) > 0) then
          CALL updownlon(lon_vec_centers1, lon_vec_centers, rc)
          if(rc>0) return
       end if
       if (lat_vec_centers1(1) < 0) then
          CALL updownlon(lat_vec_centers1, lat_vec_centers, rc)
          if (rc>0) return
       else
          lat_vec_centers = lat_vec_centers1
       end if
end subroutine do_gridcorrections

SUBROUTINE updownlon(vec_centers, upsidedown, rc)
     IMPLICIT NONE
     REAL(dp), DIMENSION(:), INTENT(IN)  :: vec_centers
     REAL(dp), DIMENSION(:), INTENT(OUT) :: upsidedown
     integer, intent(inout) :: rc
     REAL(dp), DIMENSION(:), allocatable :: tempcenters 
     INTEGER :: i, ierr1

     allocate( tempcenters(1:size(vec_centers)), stat=ierr1)
     if (ierr1 /= 0) then
         rc = 3 !
       return
     endif     

     tempcenters = vec_centers
     DO i=1, size(vec_centers)
       upsidedown(size(vec_centers)+1-i) = tempcenters(i)
     END DO
     deallocate(tempcenters, stat=ierr1)
     if (ierr1 /= 0) then
         rc = 3 !
       return
     endif 
END SUBROUTINE updownlon


SUBROUTINE find_waypointslat(iSat, time_sec, sim_start,sim_end, distlat,   &
                               t1_l, lat_l, lon_l, ts, las, los, rc)
      ! sat name - time flag - interval second
      IMPLICIT NONE
! !ARGUMENTS:
!
      INTEGER, INTENT(IN) :: iSat, time_sec 
      REAL(dp),    INTENT(IN) :: sim_start, sim_end
      REAL(dp),    OPTIONAL, INTENT(IN) :: distlat
      REAL(dp), DIMENSION(:), INTENT(INOUT):: t1_l, lat_l, lon_l ! data matrices (dm) 
      INTEGER, INTENT(OUT)             :: ts, las, los       ! size of dm
!!!      INTEGER, INTENT(OUT):: time
      integer, intent(inout):: rc
      REAL(dp), DIMENSION(:),  ALLOCATABLE :: t1
      REAL(dp), DIMENSION(:,:),ALLOCATABLE :: lat_estwayp, long_estwayp ! temp arrays
      REAL(dp), DIMENSION(3,1)  :: ECI_est, ECEF_est ! estimated Earth centered Innertia
      REAL(dp), DIMENSION(:), ALLOCATABLE  :: dist_wpoints ! distance between way points
      REAL(dp), DIMENSION(:,:), ALLOCATABLE:: latestwp, longestwp ! estimated lat long
      REAL(dp), DIMENSION(:), ALLOCATABLE :: t1m                  !temp obs time matrix
      REAL(dp)    :: max_wp, latest, lonest
      REAL(dp)    :: time_day, fraction2day, lat, lon, alt
      INTEGER :: say, i
      INTEGER :: ierr1, ierr2, ierr3, ierr4
!
!DESCRIPTION:
!   This subroutine finds waypoints for the possible max distance on the equator
!   line. If flagauto = 0 then equal deltat data is created. Otherwise optimum
!   numbeer of minimum data is created (nonlinear).  



          IF (time_sec.LE.0.0) THEN
              print*, "Error: time is negative. For now it is not allowed"
              STOP
          ENDIF
          time_day = time_sec/(24.0*60.0*60.0) 
          CALL built_vecsize(sim_start, sim_end, time_day, say)



          ALLOCATE (t1(1:say),             stat = ierr1)
          ALLOCATE (lat_estwayp(1:say,1),  stat = ierr2)
          ALLOCATE (long_estwayp(1:say,1), stat = ierr3)
          if ( ierr1 /= 0 .or. ierr2 /= 0 .or.  ierr3 /= 0 ) then
              rc = 3 !
            return
          endif


          CALL built_vec(sim_start, sim_end, time_day, t1)
          DO i=1,size(t1)
             CALL get_latlon(iSat, t1(i), latest, lonest)
             lat_estwayp(i,1)  = latest
             long_estwayp(i,1) = lonest
          END DO ! i



          ! % shift long (-pi)
          long_estwayp = (long_estwayp-180.0);
          ts = size(t1)
          las = size(lat_estwayp,1)
          los = size(long_estwayp,1)
          t1_l(1:ts)  = t1 
          lat_l(1:las) = lat_estwayp(:,1)
          lon_l(1:los) = long_estwayp(:,1)
          DEALLOCATE (t1, stat=ierr1)
          DEALLOCATE (lat_estwayp, stat=ierr2)
          DEALLOCATE (long_estwayp, stat=ierr3)
          if ( ierr1 /= 0 .or. ierr2 /= 0 .or.  ierr3 /= 0 ) then
             rc = 3 !
           return
          endif        

END SUBROUTINE find_waypointslat



SUBROUTINE find_sweeppoints(iii, latwayp, longwayp, l, r, wrapon, latlonshift)
       IMPLICIT NONE
! !ARGUMENTS:
!
       REAL(dp), DIMENSION(:),     INTENT(IN)   :: latwayp, longwayp
       REAL(dp), INTENT(IN)     :: l, r  ! left right swap space
       INTEGER, INTENT(IN)      :: iii
       REAL(dp), DIMENSION(:,:,:), INTENT(INOUT):: latlonshift
       LOGICAL, intent(in)      :: wrapon       ! if true then wrap to -180 to 180
 
       REAL(dp), DIMENSION(:), ALLOCATABLE :: myvec, myvec_deg
       REAL(dp), DIMENSION(:), ALLOCATABLE :: latshift1, lonshift1 
       REAL(dp)                            :: az, latout1, lonout1
       INTEGER                         :: say, i, count1
!DESCRIPTION:
!      Swap points are the points that gives the satellite instrument view 
!      points that is different than the original track. For now calculation
!      is basic. r, and l gives how far sweep points will go vertically from the orbit points. 
!
! !SEE ALSO:
!  get_recon, get_azimuth
!
       ALLOCATE(myvec(iii), myvec_deg(iii))
       if (r>l) then 
               CALL linspace(l,r,iii,myvec)
       else
               CALL linspace(r,l,iii,myvec)
       end if
       do i=1, size(myvec)
          myvec_deg(i) =  ckm2deg * myvec(i)
       end do
      DO say = 1,size(myvec)
          ALLOCATE(latshift1(1:size(longwayp)),lonshift1(1:size(longwayp)))
          count1 = 1
          DO i=1,size(longwayp)-1
           az = get_azimuth(latwayp(i),longwayp(i),latwayp(i+1),longwayp(i+1), wrapon)
           CALL get_reckon(latwayp(i),longwayp(i), az+90.0,      &
                          myvec_deg(say), latout1, lonout1, wrapon)
           latshift1(count1) = latout1
           lonshift1(count1) = lonout1
           count1 = count1 + 1
          END DO
          ! i increased by 1
          CALL get_reckon(latwayp(i),longwayp(i), az+90.0,  &
                  myvec_deg(say), latout1, lonout1, wrapon)
          latshift1(count1) = latout1
          lonshift1(count1) = lonout1
          latlonshift(say,:,1) = latshift1(:)
          latlonshift(say,:,2) = lonshift1(:)
          DEALLOCATE(latshift1, lonshift1)
      END DO 
      DEALLOCATE(myvec, myvec_deg)
END SUBROUTINE find_sweeppoints



SUBROUTINE get_latlon(iSat, t1, lat_estwayp, long_estwayp)
       IMPLICIT NONE
       REAL(dp),    INTENT(IN)    :: t1
       INTEGER, INTENT(IN)    :: iSat
       REAL(dp),    INTENT(INOUT) :: lat_estwayp, long_estwayp
       REAL(dp), DIMENSION(3,1) :: ECI_est, ECEF_est
       REAL(dp)                 :: lat, lon, alt, fraction2day 
       INTEGER              :: i
!DESCRIPTION:
!   getlatlon calls three important functions to calculate lat lon values of 
!   a satellite for a g   iven time. Those functions are described in the 
!   following subsections.
       CALL get_estimateECI(iSat, t1, ECI_est)
       fraction2day = get_fraction(t1)
       CALL ECI2ECEF(iSat,fraction2day, ECI_est, ECEF_est)
       CALL ECEF2LLA(ECEF_est(1,1), ECEF_est(2,1),   &
                     ECEF_est(3,1), lat, lon, alt)
       lat_estwayp  = rad2deg(lat)
       long_estwayp = rad2deg(lon)
   
END SUBROUTINE

SUBROUTINE get_estimateECI(iSat, ntime_day, ECI_est)
        IMPLICIT NONE
        INTEGER, INTENT(IN) :: iSat
        REAL(dp), INTENT(IN)    :: ntime_day 
        REAL(dp), DIMENSION(:,:), INTENT(OUT) :: ECI_est
        REAL(dp), DIMENSION(Num_coef) :: myvec
        REAL(dp), DIMENSION(Num_normcoef) :: normvec
        REAL(dp)                      :: xdata_est, ydata_est, zdata_est
        INTEGER                   :: start, ends, temp1 



       CALL Orbits_Track0(iSat, myvec, normvec) ! myvec has 9 sat coef
                                                ! normvec has normalization coef
!       % _________________________Estimates_________________________
        xdata_est = sin(myvec(1) * ntime_day + myvec(2)) + myvec(3);
        ydata_est = sin(myvec(4) * ntime_day + myvec(5)) + myvec(6);
        zdata_est = sin(myvec(7) * ntime_day + myvec(8)) + myvec(9);

!       % ________________Mult by Norm fator_________________________
        xdata_est = xdata_est * normvec(1)   ! max(xECI);
        ydata_est = ydata_est * normvec(2) ! max(yECI);
        zdata_est = zdata_est * normvec(3) ! max(zECI);
        ECI_est(1,1) = xdata_est
        ECI_est(2,1) = ydata_est
        ECI_est(3,1) = zdata_est

END SUBROUTINE get_estimateECI

SUBROUTINE  ECI2ECEF(iSat,fraction2day,  ECI_est, ECEF_est)
        IMPLICIT NONE
        INTEGER, INTENT(IN)  :: iSat
        REAL(dp), DIMENSION(:,:), INTENT(IN)  :: ECI_est
        REAL(dp), DIMENSION(:,:), INTENT(OUT) :: ECEF_est
        REAL(dp), DIMENSION(3,3) :: RotM        ! rotation matrix from In2ECEF         
        INTEGER   :: t1 
        REAL(dp)      :: Goft, a1, a2
        REAL(dp)      :: fraction2day 

        t1 =  Day2year(iSat)
        Goft = g0 + g1p*t1 + g2p(iSat) * fraction2day
        a1 = cos(Goft);
        a2 = sin(Goft);
        RotM = 0.0
        RotM(1,1) = a1
        RotM(1,2) = a2
        RotM(2,1) = -a2
        RotM(2,2) = a1
        RotM(3,3) = 1
        ECEF_est = MATMUL(RotM, ECI_est)
END SUBROUTINE ECI2ECEF  

SUBROUTINE get_time(iSat, nymd, nhms, fraction2day, ntime_day)
        IMPLICIT  NONE
        INTEGER, INTENT(IN) :: iSat, nymd, nhms
        REAL(dp), INTENT(OUT)   :: fraction2day, ntime_day
        INTEGER :: Year, Month, Day, Hour, Minute, Seconds 

        Year   = int(nymd / 10000 )
        Month  = mod ( nymd,  10000 ) / 100
        Day    = mod ( nymd,    100 )
        Hour = int(nhms / 10000 )
        Minute = mod ( nhms,  10000 ) / 100
        Seconds = mod ( nhms,    100 )
        fraction2day = (Hour*60*60+Minute*60+Seconds)/(24.0*60.0*60.0)  !fraction
                                         !calculated from seconds  
        ntime_day=(-ODS_Julian(RefDate(iSat))+ODS_Julian(nymd))+fraction2day
END SUBROUTINE get_time

SUBROUTINE ECEF2LLA(x, y, z, lat, lon, alt)
!       % ECEF3LLA - convert earth-centered earth-fixed (ECEF)
!       %            cartesian coordinates to latitude, longitude,
!       %            and altitude
!       %
!       % USAGE:
!       % [lat,lon,alt] = ecef2lla(x,y,z)
!       %
!       % lat = geodetic latitude (radians)
!       % lon = longitude (radians)
!       % alt = height above WGS84 ellipsoid (m)
!       % x = ECEF X-coordinate (m)
!       % y = ECEF Y-coordinate (m)
!       % z = ECEF Z-coordinate (m)
!       %
!       % Notes: (1) This function assumes the WGS84 model.
!       %        (2) Latitude is customary geodetic (not geocentric).
!       %        (3) Inputs may be scalars, vectors, or matrices of the same
!       %            size and shape. Outputs will have that same size and shape.
!       %        (4) Tested but no warranty; use at your own risk.
!       %        (5) Michael Kleder, April 2006
       REAL(dp), INTENT(IN)  :: x,y,z
       REAL(dp), INTENT(OUT) :: lat,lon,alt
!      % WGS84 ellipsoid constants:
       REAL(dp), PARAMETER  :: a = 6378137 ! those should be defined outside
       REAL(dp), PARAMETER  :: e = 8.1819190842622e-2
       REAL(dp), PARAMETER  :: pi = 3.14159265358979323846 
       REAL(dp)             :: b, ep, p, th, N
       LOGICAL k

       b   = sqrt(a**2 * (1-e**2))
       ep  = sqrt((a**2-b**2)/b**2)
       p   = sqrt(x**2+y**2)
       th  = atan2(a*z,b*p)
       lon = atan2(y,x)
       lat = atan2((z+ep**2*b*sin(th)**3),(p-e**2*a*cos(th)**3))
       N   = a/sqrt(1-e**2*sin(lat)**2)
       alt = p/cos(lat)-N
!%     !return lon in range [0,2*pi)
       !lon = mod(lon,2*pi) ! does not the same as matlab
       lon = lon-floor(lon/(2*pi))*(2*pi)
!      % correct for numerical instability in altitude near exact poles:
!      % (after this correction, error is about 2 millimeters, which is about
!      % the same as the numerical precision of the overall function)
       !k= (abs(x)<1.AND.abs(y)<1 )
       !print *, k
       !!alt(k) = abs(z(k))-b
END SUBROUTINE ECEF2LLA

integer function ODS_Julian ( CalDate )
      implicit NONE
      integer  CalDate  ! Calendar date in the format YYYYMMDD
                        !   where YYYY is the year, MM is the
                        !   month and DD is the day.  A negative
                        !   number implies that the year is B.C.
!     Other variables
!     ---------------
      integer     Year
      integer     Month
      integer     Day
      integer     iGreg  ! Gregorian Calendar adopted Oct 12, 1582
      parameter ( iGreg = 15 + 31 * ( 10 + 12 * 1582 ) )
      integer     JulDay
      integer     jy, jm, ja

      Year   =       CalDate / 10000
      Month  = mod ( CalDate,  10000 ) / 100
      Day    = mod ( CalDate,    100 )
!     Change year 0 to year 1
      if ( Year  .eq. 0 ) Year = 1
!     Account for the nonexisting year 0
      if ( Year  .lt. 0 ) Year = Year + 1
      if ( Month .gt. 2 ) then
         jy = Year
         jm = Month + 1
      else
         jy = Year  - 1
         jm = Month + 13
      endif
      JulDay = int ( 365.25  * jy )       &
             + int ( 30.6001 * jm )       &
             + Day + 1720995
!     Test whether to change to Gregorian Celendar
      if ( Day + 31 * ( Month + 12 * Year ) .ge. iGreg) then
        ja     = int ( 0.01 * jy )
        Julday = JulDay + 2 - ja + int ( 0.25 * ja )
      endif
      ODS_Julian = JulDay
      return
end function ODS_Julian

!      &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

SUBROUTINE linspace1(d1,d2,n,y)
       ! from d1 to d2, n by n -> y
         IMPLICIT   NONE
        REAL(dp), INTENT(IN)   :: d1, d2, n
        INTEGER            :: n2, i
        REAL(dp), DIMENSION(:), INTENT(INOUT) :: y
        n2 = n
        DO i = 0, n2-2
          y(i+1) = i 
        END DO
        y(1:n2-1) = (d1+y(1:n2-1)*(d2-d1)/(floor(n)-1))   ! d2);
        y(n2) = d2
END SUBROUTINE linspace1

SUBROUTINE linspace2(d1,d2,n,y)
        IMPLICIT   NONE
        REAL(dp), INTENT(IN)   :: d1, d2
        INTEGER, INTENT(IN)   :: n
        INTEGER            :: n2, i
        REAL(dp), DIMENSION(:), INTENT(INOUT) :: y

        n2 = n
        DO i = 0, n2-2
          y(i+1) = i 
        END DO
        y(1:n2-1) = (d1+y(1:n2-1)*(d2-d1)/(n-1))   ! d2);
        y(n2) = d2
END SUBROUTINE linspace2

SUBROUTINE build_array(s,e,dp, mat)
       IMPLICIT NONE
       REAL, DIMENSION(:) :: mat
       REAL               :: s,dp ! start, end, delta increase, matrix
       INTEGER            :: k, e 

       mat(1) =  s
       do k=2,e 
         mat(k) = mat(k-1) + dp
       end do
END SUBROUTINE build_array

REAL(dp) FUNCTION deg2km(angle)
       IMPLICIT NONE    
       REAL(dp) angle

       deg2km = earth_radius * deg2rad(angle) 
END FUNCTION deg2km

REAL(dp) FUNCTION km2deg(r)
       IMPLICIT NONE    
       REAL(dp) :: r, rad

       rad = r/earth_radius
       km2deg = rad2deg(rad)
END FUNCTION km2deg

REAL(dp) FUNCTION deg2rad(angle)
       IMPLICIT NONE

       REAL(dp), INTENT(IN) :: angle
       deg2rad = (pi/180.0) * angle
END FUNCTION deg2rad

REAL(dp) FUNCTION rad2deg(rad)
       IMPLICIT NONE
       REAL(dp), INTENT(IN) :: rad

       rad2deg = (180.0/pi) * rad
END FUNCTION rad2deg

REAL(dp) FUNCTION get_distance(lt1, ln1, lt2, ln2)
       ! Computes great circle distance and azimuth
       ! Lat - lon in degrees
         IMPLICIT NONE
       REAL(dp) :: lat1, lat2, lon1, lon2
       REAL(dp) :: lt1, lt2, ln1, ln2
       REAL(dp) :: a, r

       lat1 = deg2rad(lt1)
       lon1 = deg2rad(ln1)
       lat2 = deg2rad(lt2)
       lon2 = deg2rad(ln2)
       r=1.0
       a = sin((lat2-lat1)/2.0)**2.0 + cos(lat1) * cos(lat2) * &
           sin((lon2-lon1)/2.0)**2.0;
       get_distance = r * 2.0 * atan2(sqrt(a),sqrt(1.0 - a));
       get_distance = rad2deg(get_distance)
END FUNCTION get_distance

REAL(dp) FUNCTION get_azimuth(lt1, ln1, lt2, ln2, wrapon) ! need to be
       ! Computes great circle distance and azimuth
       ! Lat - lon in degrees
       IMPLICIT NONE
       REAL(dp) :: lat1, lat2, lon1, lon2
       REAL(dp) :: lt1, lt2, ln1, ln2
       REAL(dp) :: az, epsilone, saz
       LOGICAL, intent(in) :: wrapon       ! if true then wrap to -180 to 180

       lat1 = deg2rad(lt1)
       lon1 = deg2rad(ln1)
       lat2 = deg2rad(lt2)
       lon2 = deg2rad(ln2)
       ! Inputs LAT1, LON1, LAT2, LON2 are in units of radians.
        az = atan2(cos(lat2) * sin(lon2-lon1), cos(lat1) * sin(lat2)-  &
             sin(lat1) * cos(lat2) * cos(lon2-lon1))
       ! Azimuths are undefined at the poles, so we choose a convention: zero at
       ! the north pole and pi at the south pole.
       if (lat1 <= -pi/2.0) az = 0.0
       if (lat2 >=  pi/2.0) az = 0.0
       if (lat2 <=  -pi/2.0) az = pi
       if (lat1 >=   pi/2.0) az = pi
       epsilone = 1.74E-8


       if (wrapon) then
           if ( az>-epsilone .AND. az < epsilone) then
               saz = 0
           else
               saz = az/abs(az)
           end if
           az = pi*((abs(az)/pi) - 2*ceiling(((abs(az)/pi)-1)/2)) * saz
       end if

       if ( az < -epsilone) then ! change to 0 to 2pi
           az = az + 2*pi
       end if
       !  Reset near-zero points
       if (az<0.0) az=0
       get_azimuth = rad2deg(az)
END FUNCTION get_azimuth

SUBROUTINE get_reckon(phi1, lambda1, az1, rng1, phi, lambda, wrapon)
       ! phi0 - > lat, lambda0 -> lon
       ! calculates a position (LATOUT, LONOUT) at a given range RNG and azimuth
       ! AZ along a great circle from a starting point defined by LAT and LON.
       ! LAT and LON are in degrees.  The range is in degrees of arc length on a
       ! sphere.  The input azimuth is in degrees, measured clockwise from due
       ! north. Translated from MatLab 
       IMPLICIT NONE

       REAL(dp), INTENT(IN) :: phi1, lambda1, az1, rng1
       REAL(dp), INTENT(INOUT) :: phi, lambda
       REAL(dp)                :: phi0, lambda0 
       REAL(dp) :: epsilone, az, saz, rng
       LOGICAL, intent(in) :: wrapon       ! if true then wrap to -180 to 180

       epsilone = 10*1.74E-8
       phi0 = deg2rad(phi1)
       lambda0 = deg2rad(lambda1)
       az = deg2rad(az1)
       rng = deg2rad(rng1)
       if (phi0 >= pi/2-epsilone) az = pi   ! starting at north pole
       if (phi0 <= epsilone-pi/2) az = 0    ! starting at south pole
       ! Calculate coordinates of great circle end point using spherical trig.
       phi = asin(sin(phi0)*cos(rng) + cos(phi0)*sin(rng)*cos(az))
       lambda = lambda0 + atan2( sin(rng)*sin(az),            &
                cos(phi0)*cos(rng) - sin(phi0)*sin(rng)*cos(az) )


       if (wrapon) then 
           if ( lambda>-epsilone .AND. lambda < epsilone) then
               saz = 0
           else
               saz = lambda/abs(lambda)
           end if
           lambda = pi*((abs(lambda)/pi) - 2*ceiling(((abs(lambda)/pi)-1)/2)) * saz
       end if

       lambda = rad2deg(lambda)
       phi    = rad2deg(phi)
END SUBROUTINE get_reckon

subroutine built_vecsize(sim_start, sim_end, time_day, say)
       implicit none
       real(dp), intent(in) :: sim_start, sim_end, time_day
       integer          :: say
       real(dp)             :: temp1

       say = 1
       temp1 = sim_start

       !print*, "inside simend-----", sim_end, temp1, time_day 
       !WRITE(*,"(1F10.2)")  sim_end

       do while (sim_end.GE.temp1)
         temp1 = temp1 + time_day
         if (temp1.LE.sim_end) then
          say = say + 1 
           end if
            enddo
end subroutine built_vecsize

subroutine built_vec(sim_start, sim_end, time_day, t1)
       implicit none
       real(dp), intent(in) :: sim_start, sim_end, time_day
       integer          :: say
       real(dp)             :: temp1
       real(dp),  dimension(:) :: t1 

       say = 1         
       temp1 = sim_start ! garante that it lesim start !time
       t1(1) = sim_start
       do while (sim_end.GE.temp1)
         temp1 = temp1 + time_day
          say = say + 1 
          if (say>size(t1)) then
             exit 
          end if
          t1(say) = temp1
       enddo
end subroutine built_vec

SUBROUTINE find_LTGE(distlon, lat_vec_centers, vec_lat, long_limits)
       !This functions finds minimum possible long value for carrespondig
       !lat interval
       IMPLICIT NONE

       INTEGER :: mys, i, k
       REAL(dp), POINTER, DIMENSION(:) :: long_limits  !INTENT(INOUT)
       REAL(dp), DIMENSION(:),INTENT(IN)::lat_vec_centers, distlon, vec_lat 
       REAL(dp), DIMENSION(:), ALLOCATABLE :: long_limitstemp

       mys = size(lat_vec_centers)
       ALLOCATE(long_limitstemp(1:mys))
       do i=1,size(vec_lat) - 1
          do k=1,mys
             if ( (lat_vec_centers(k).LT.vec_lat(i)).AND.  & 
                  (lat_vec_centers(k).GE.vec_lat(i+1)) ) then
                long_limitstemp(i) = cdeg2km * distlon(k)
                exit
             end if 
         end do 
       end do
       long_limitstemp(i)=long_limitstemp(i-1)       
       ALLOCATE(long_limits(1:i))
       long_limits = long_limitstemp(1:i)
       DEALLOCATE(long_limitstemp)
END SUBROUTINE find_LTGE

SUBROUTINE redifine_timeint(kk, longind, ti, iSat,      &
                 lat_new, lon_new, t1_new, carylat, carylon, caryt)

       IMPLICIT NONE
       REAL(dp), DIMENSION(:), INTENT(INOUT) :: lat_new, lon_new, t1_new
       INTEGER, INTENT(INOUT) :: carylat, carylon, caryt
       REAL(dp), DIMENSION(:), INTENT(INOUT) :: longind
       INTEGER, INTENT(IN)               :: kk 
       INTEGER, INTENT(IN)               :: ti
       REAL(dp), DIMENSION(:), ALLOCATABLE   :: t1 
       REAL(dp), DIMENSION(:), ALLOCATABLE   :: t1_f, lat_final, lon_final, ppr 
       REAL(dp), DIMENSION(:,:), ALLOCATABLE :: latestwp, longestwp
       REAL(dp), DIMENSION(:), ALLOCATABLE  :: t_tempo
       REAL(dp), DIMENSION(:,:), ALLOCATABLE:: lat_wayptemp, long_wayptemp
       INTEGER             :: temp1, temp2, temp4, iSat, say, ii, temp_size
       REAL(dp) :: simtimestart_temp, simtimeend_temp, ti_day, time_day
       REAL(dp) :: lat_estwayp, lon_estwayp
       
       ALLOCATE(latestwp(1:carylat,1), longestwp(1:carylon,1), t1(1:caryt))
       
       latestwp(:,1)   =  lat_new(1:carylat)
       longestwp(:,1)  =  lon_new(1:carylon)
       t1(:)           =  t1_new(1:caryt)
       ti_day = REAL(ti)/(24.0*60.0*60.0)

       temp1 = longind(kk)
       temp2  = longind(kk)+1

       if (temp2 <= size(latestwp,1) ) then   
           simtimestart_temp = t1(temp1)
           simtimeend_temp = t1(temp2)
           CALL built_vecsize(simtimestart_temp, simtimeend_temp, &
                              ti_day, say)
           ALLOCATE(t_tempo(1:say))
           CALL built_vec(simtimestart_temp,simtimeend_temp,      &
                              ti_day, t_tempo)
           ALLOCATE(lat_wayptemp(1:size(t_tempo),1) ,             &
                              long_wayptemp(1:size(t_tempo),1) )



           do ii=1,size(t_tempo) ! Calculate carresponding lat lon points
             CALL get_latlon(iSat, t_tempo(ii), lat_estwayp, lon_estwayp)
              lat_wayptemp(ii,1)  = lat_estwayp
                long_wayptemp(ii,1) = lon_estwayp
                  end do
                  
           long_wayptemp = (long_wayptemp-180)
           temp_size = size(t1)+size(t_tempo)-2        

           ALLOCATE(t1_f(1:temp_size))
           ALLOCATE(lat_final(1:temp_size)) 
           ALLOCATE(lon_final(1:temp_size))

           CALL add_newdata(t1, temp1, t_tempo, t1_f)
           CALL add_newdata(latestwp(:,1), temp1, lat_wayptemp(:,1), lat_final)
           CALL add_newdata(longestwp(:,1), temp1, long_wayptemp(:,1), lon_final)
           temp4 = size(lat_wayptemp,1)-2;
           longind(kk+1:size(longind)) = longind(kk+1:size(longind)) + temp4;
           carylat =  size(lat_final)
           carylon =  size(lon_final)
           caryt   =  size(t1_f)
           lat_new(1:carylat) =  lat_final
           lon_new(1:carylon) =  lon_final
           t1_new(1:caryt)    =  t1_f
           DEALLOCATE(t1_f, lat_final, lon_final )
           DEALLOCATE(lat_wayptemp,long_wayptemp)  
           DEALLOCATE (t_tempo)
       end if
      DEALLOCATE(latestwp, longestwp, t1)
END SUBROUTINE redifine_timeint

SUBROUTINE add_newdata(t1, temp1, t_tempo, rtime)
       IMPLICIT NONE
       REAL(dp), DIMENSION(:), INTENT(IN)     :: t1, t_tempo 
       REAL(dp), DIMENSION(:), INTENT(INOUT)  :: rtime 
       INTEGER, INTENT(IN) :: temp1
       INTEGER :: i, ii, k 

       ii = 1
       do i=1, temp1-1
         rtime(ii) = t1(i)
         ii=ii+1 
       end do
       do k=1, size(t_tempo)
         rtime(ii) = t_tempo(k)
         ii=ii+1
       end do
       do i=temp1+2, size(t1)
         rtime(ii) = t1(i)
         ii=ii+1
       end do
END SUBROUTINE add_newdata


REAL(dp) FUNCTION find_delta_t(iSat, kk, long_limits, longind,t1,time_day)
       IMPLICIT NONE
       REAL(dp)    :: ti, ti_day
       INTEGER, INTENT(IN) :: kk 
       REAL(dp), INTENT(IN)    :: time_day
       REAL(dp), DIMENSION(:), INTENT(IN)  :: longind 
       REAL(dp), INTENT(IN)  :: long_limits 
       REAL(dp), DIMENSION(:), INTENT(IN)  :: t1 
       INTEGER, INTENT(IN)             :: iSat
       REAL(dp), DIMENSION(:), ALLOCATABLE :: t_tempo
       REAL(dp), DIMENSION(:,:), ALLOCATABLE :: lat_wayptemp, long_wayptemp
       INTEGER             :: flag_add, temp1, temp2, say, ii
       REAL(dp) :: simtimestart_temp, simtimeend_temp
       REAL(dp) :: lat_estwayp, lon_estwayp, dista
       flag_add = 1
       ti = time_day
       do while (flag_add.EQ.1)
          ti = ceiling(ti/2.0); !create new way points
          ti_day = ti/(24*60*60)
          temp1 = longind(kk)
          temp2  = longind(kk)+1
          simtimestart_temp = t1(temp1)
          simtimeend_temp = t1(temp2)
          CALL built_vecsize(simtimestart_temp, simtimeend_temp, &
               ti_day, say)
          ALLOCATE(t_tempo(1:say))
          CALL built_vec(simtimestart_temp,simtimeend_temp,      &
               ti_day, t_tempo)
          ALLOCATE(lat_wayptemp(1:size(t_tempo),1) ,             &
                   long_wayptemp(1:size(t_tempo),1) )
          DO ii=1,size(t_tempo) ! Cal caresponding lat lon

             CALL get_latlon(iSat, t_tempo(ii), lat_estwayp, lon_estwayp)
             lat_wayptemp(ii,1)  = lat_estwayp
             long_wayptemp(ii,1) = lon_estwayp             
          END DO
         long_wayptemp = (long_wayptemp-180)
         flag_add = 0
         dista = cdeg2km * get_distance( lat_wayptemp(1,1),long_wayptemp(1,1), &
         lat_wayptemp(2,1),long_wayptemp(2,1))
         if  (dista.GT.long_limits) then
             flag_add = 1
         end if
         DEALLOCATE( lat_wayptemp,long_wayptemp )  
         DEALLOCATE (t_tempo)
       end do
       find_delta_t = ti
END FUNCTION find_delta_t

REAL(dp) FUNCTION get_fraction(t1)
       IMPLICIT NONE
       REAL(dp), INTENT(IN) :: t1
       REAL(dp) :: fraction2day, temp3, temp4 

       if (t1.EQ.0.0) then
           fraction2day = 0.0
       else if (t1.lt.1.0 .AND. t1.gt.0) then
           fraction2day = t1
       else
           temp3 = floor(t1)
           temp4 = t1
           if (temp3 == temp4) then 
             fraction2day = temp3 ! either is ok 
           else
             fraction2day = MOD(temp4,temp3)
           end if
       end if
       get_fraction = fraction2day
END FUNCTION get_fraction

SUBROUTINE find_LTGE_sc(myarray, num1, num2, myindex)
       IMPLICIT NONE
       INTEGER :: k, say
       INTEGER, INTENT(IN) :: num1, num2 
       REAL(dp), POINTER, DIMENSION(:) :: myindex  !INTENT(INOUT)
       REAL(dp), DIMENSION(:), INTENT(IN) :: myarray

       !REAL(dp), DIMENSION(:), ALLOCATABLE :: myindextemp 
       REAL(dp), DIMENSION(:), ALLOCATABLE :: myindextemp 

       say = 0
       ALLOCATE(myindextemp(1:size(myarray)))
       myindextemp = 0.0
       do k=1, size(myarray)
           if ( (myarray(k).LT.num1).AND.  & 
                   (myarray(k).GE.num2) )  then
              say = say + 1
              myindextemp(say) = k
            end if 
       end do

       if (say.EQ.0) then
           ALLOCATE(myindex(1:1))    
           myindex(1) = -999999
       else
           ALLOCATE(myindex(1:say))
           myindex(1:say) = myindextemp(1:say)
       end if
       DEALLOCATE(myindextemp)
END SUBROUTINE find_LTGE_sc


SUBROUTINE find_LEGT_sc(myarray, num1, num2, myindex)
       IMPLICIT NONE
       INTEGER :: k, say
       INTEGER, INTENT(IN) :: num1, num2 
       REAL(dp), POINTER, DIMENSION(:) :: myindex  !INTENT(INOUT)
       REAL(dp), DIMENSION(:), INTENT(IN) :: myarray
       REAL(dp), DIMENSION(:), ALLOCATABLE :: myindextemp 
       say = 0
       ALLOCATE(myindextemp(1:size(myarray)))
       myindextemp = 0.0
       do k=1, size(myarray)
           if ( (myarray(k).LE.num1).AND.  & 
                   (myarray(k).GT.num2) )  then
              say = say + 1
              myindextemp(say) = k
            end if 
       end do
       if (say.EQ.0) then
           ALLOCATE(myindex(1:1))    
           myindex(1) = -999999.0
       else
           ALLOCATE(myindex(1:say))
           myindex = myindextemp(1:say)
       end if
       DEALLOCATE(myindextemp)
END SUBROUTINE find_LEGT_sc

SUBROUTINE ss(ar)
       real(dp),  pointer, dimension(:) :: ar
       allocate(ar(1:2))
       ar = (/ 3, 4 /)
       return
END SUBROUTINE ss
      
SUBROUTINE orbits(lat, lon, iSat, nymd, nhms) ! for now time has to be after
       IMPLICIT  NONE
       INTEGER, INTENT(IN)   :: iSat
       INTEGER, INTENT(IN)   :: nymd, nhms 
       REAL(dp), INTENT(OUT)     :: lat,lon
       REAL(dp), DIMENSION(Num_coef)    :: myvec ! 3 for each x,y,z
       REAL(dp), DIMENSION(3,1)  :: ECI_est, ECEF_est
       REAL(dp)                  :: alt
       REAL(dp)      :: fraction2day, ntime_day 

       CALL get_time(iSat, nymd, nhms, fraction2day, ntime_day)
       CALL get_estimateECI(iSat, ntime_day, ECI_est)
       CALL ECI2ECEF(iSat,fraction2day, ECI_est, ECEF_est)
       CALL ECEF2LLA(ECEF_est(1,1), ECEF_est(2,1),   &
                     ECEF_est(3,1), lat, lon, alt)
END SUBROUTINE orbits
      
END MODULE MAPL_NominalOrbitsMod


