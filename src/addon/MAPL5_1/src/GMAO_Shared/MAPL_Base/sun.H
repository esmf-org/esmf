! $Id$


      type(MAPL_SunOrbit),               intent(IN ) :: ORBIT
      real,                              intent(IN ) :: LONS DIMENSIONS
      real,                              intent(IN ) :: LATS DIMENSIONS
      real,                              intent(OUT) :: ZTH  DIMENSIONS
      real,                              intent(OUT) :: SLR  DIMENSIONS
      type(ESMF_TimeInterval), optional, intent(INout) :: INTV
      type(ESMF_Clock),        optional, intent(IN ) :: CLOCK
      integer,                 optional, intent(IN ) :: TIME
      type(ESMF_Time),         optional, intent(IN ) :: currTime
      real,                    optional, intent(OUT) :: DIST
      real,                    optional, intent(OUT) :: ZTHB DIMENSIONS
      real,                    optional, intent(OUT) :: ZTHD DIMENSIONS
      real,                    optional, intent(OUT) :: ZTH1 DIMENSIONS
      real,                    optional, intent(OUT) :: ZTHN DIMENSIONS
      integer,                 optional, intent(OUT) :: RC

!     Locals

      character(len=ESMF_MAXSTR), parameter :: IAm = "MAPL_SunGetInsolation"
      integer                               :: STATUS

      integer           :: IDAY, IDAYP1, TIME_
      integer           :: NT
      real              :: FAC, ZS, ZC, ANG, AA, DD
      real*8            :: SECS

      integer           :: YEAR
      integer           :: SEC_OF_DAY
      integer           :: DAY_OF_YEAR
      integer           :: L
      type (ESMF_Time)  :: CURRENTTIME
      type (ESMF_Time)  :: firstofyear
      type (ESMF_Clock) :: MYCLOCK
      type (ESMF_TimeInterval)  :: ts
      real,    dimension THE_SIZE :: ZTT, SLT, Y, ZTB, ZTD, NCC

!     Begin
      
      if (present(TIME)) then
         TIME_ = TIME
      else
         TIME_ = 0
      endif


      if (present(currTime)) then
         CURRENTTIME = CURRTIME
      else
         if (present(CLOCK)) then
            call ESMF_ClockGet(      CLOCK, currTime=CURRENTTIME, RC=STATUS)
         else
            call ESMF_ClockGet(ORBIT%CLOCK, currTime=CURRENTTIME, RC=STATUS)
         end if
         VERIFY_(STATUS)
      end if

!#define DP_SUN
#ifdef DP_SUN
! WMP- Hack for Doubly Period Constant Solar Insolation
            call WRITE_PARALLEL('--- WARNING --- sun.H --- Doubly Periodic Using Daily Mean Solar Insolation')
            TIME_=MAPL_SunDailyMean
! WMP- Hack for Doubly Period Constant Solar Insolation
#endif

      MEAN_OR_INST: if(.not.present(INTV) .or. TIME_==MAPL_SunDailyMean &
                       .or.  TIME_==MAPL_SunAnnualMean) then

         call ESMF_TimeGet(CURRENTTIME, YY=YEAR, S=SEC_OF_DAY, &
              dayOfYear=DAY_OF_YEAR, RC=STATUS)
         VERIFY_(STATUS)

#ifdef DP_SUN
! WMP- Hack for Doubly Period Constant Solar Insolation
         TIME_=10
         YEAR=2005
         SEC_OF_DAY=86400/2
         DAY_OF_YEAR=31*7
! WMP- Hack for Doubly Period Constant Solar Insolation
#endif

         YEAR = mod(YEAR-1,ORBIT%YEARS_PER_CYCLE)
         IDAY = YEAR*int(ORBIT%YEARLEN)+DAY_OF_YEAR

         if(present(DIST)) DIST = ORBIT%PP(IDAY)

         select case (TIME_)

         case(MAPL_SunDailyMean)

            SLR  = sin(LATS)*ORBIT%ZS(IDAY)
            ZTH  = cos(LATS)*ORBIT%ZC(IDAY)
            Y    =  max(-1.0,-SLR/ZTH)

            where (Y < 1.0)
               Y   = ACOS(Y)
               SLR = (4.0*ORBIT%PP(IDAY)/MAPL_PI) * (Y *SLR + SIN(Y)*ZTH)
               ZTH = SLR*MAPL_PI / (4.0*Y)
            elsewhere
               SLR = 0.0
               ZTH = 0.0
            endwhere

         case(MAPL_SunAnnualMean)

            SLR  = 0.0
            ZTH  = 0.0

            do IDAY = 1,ORBIT%DAYS_PER_CYCLE
               SLT  = sin(LATS)*ORBIT%ZS(IDAY)
               ZTT  = cos(LATS)*ORBIT%ZC(IDAY)
               Y    =  max(-1.0,-SLT/ZTT)
               where (Y < 1.0)
                  Y   = ACOS(Y)
                  SLT = (4.0*ORBIT%PP(IDAY)/MAPL_PI) * (Y *SLT + SIN(Y)*ZTT)
                  ZTT = SLR*MAPL_PI / (4.0*Y)
               elsewhere
                  SLT = 0.0
                  ZTT = 0.0
               endwhere
               SLR = SLR + SLT
               ZTH = ZTH + ZTT
            end do

            SLR = SLR / float(ORBIT%DAYS_PER_CYCLE)
            ZTH = ZTH / float(ORBIT%DAYS_PER_CYCLE)

            if(present(DIST)) DIST = 1.0

         case (0,                       &
               MAPL_SunAutumnalEquinox, & 
               MAPL_SunWinterSolstice , & 
               MAPL_SunVernalEquinox  , & 
               MAPL_SunSummerSolstice   ) 

            FAC = real(SEC_OF_DAY)/86400.
            ANG = 2.0*MAPL_PI*FAC

            if(TIME_==0) then
               IDAYP1 = mod(IDAY,ORBIT%DAYS_PER_CYCLE) + 1

               ZS  = ORBIT%ZS(IDAYP1)*FAC + ORBIT%ZS(IDAY)*(1.-FAC)
               ZC  = ORBIT%ZC(IDAYP1)*FAC + ORBIT%ZC(IDAY)*(1.-FAC)
               AA  = ORBIT%PP(IDAYP1)*FAC + ORBIT%PP(IDAY)*(1.-FAC)
            else
               call GETIDAY(IDAY,TIME_,ORBIT,RC=STATUS)
               VERIFY_(STATUS)

               ZS  = ORBIT%ZS(IDAY)
               ZC  = ORBIT%ZC(IDAY)
               AA  = ORBIT%PP(IDAY)
            endif

            ZTH =         ZS*SIN(LATS) +       ZC*COS(LATS) &
                 * (sin(ANG)*SIN(LONS) - cos(ANG)*COS(LONS))
            ZTH = max(ZTH, 0.0)
            SLR = ZTH*AA

            if(present(DIST)) DIST = AA

         case(10)
            SLR = 0.3278
            ZTH = 0.6087247

         case(11)
            SLR = 0.3449
            ZTH = 0.5962

         case(12)
            SLR = 0.3461
            ZTH = 0.5884

         end select

         if(present(ZTHB)) ZTHB = ZTH
         if(present(ZTHD)) ZTHD = ZTH
         if(present(ZTH1)) ZTH1 = ZTH
         if(present(ZTHN)) ZTHN = ZTH

       else ! Return Time-Interval-Mean present(INTV)

         call ESMF_TimeIntervalGet (INTV, S_R8=SECS, rc=STATUS)
         VERIFY_(STATUS)

         if(SECS>1500.) then
            NT = nint(SECS/300.0)
         else
            NT = 5
         end if

         TS = INTV/dble(NT)

         MYCLOCK =  ESMF_ClockCreate(name="SUN_CLOCK", &
              TimeStep  = ts                   , &
              StartTime = CURRENTTIME          , &
              StopTime  = CURRENTTIME + INTV   , &
              rc        = STATUS                 )
         VERIFY_(STATUS)

         SLR = 0.0
         ZTH = 0.0

         if(present(ZTHD)) then
            ZTHD = 0.0
            NCC  = 0.0
         end if

         if(present(ZTHB)) ZTHB = 0.0
         if(present(DIST)) DIST = 0.0

         call MAPL_SunGetInsolation( LONS, LATS, ORBIT, ZTT, SLT, &
              CLOCK=MYCLOCK, TIME=TIME, DIST=DD, RC=STATUS)
         VERIFY_(STATUS)

         if(present(ZTH1)) ZTH1 = max(ZTT,0.0)

         do L=1,NT
            SLR = SLR +     SLT*0.5
            ZTH = ZTH + ZTT*SLT*0.5

            if(present(ZTHB)) ZTHB = ZTHB + 0.5*ZTT
            if(present(ZTHD)) then
               where(ZTT>0.0)
                  ZTHD = ZTHD + 0.5*ZTT
                  NCC  = NCC  + 0.5
               end where
            end if

            if(present(DIST)) DIST = DIST + 0.5*DD

            call ESMF_ClockAdvance    ( MYCLOCK, RC=STATUS )
            VERIFY_(STATUS)

            call MAPL_SunGetInsolation( LONS, LATS, ORBIT, ZTT, SLT, &
                 CLOCK=MYCLOCK, TIME=TIME, ZTHB=ZTB, ZTHD=ZTD, DIST=DD, RC=STATUS)
            VERIFY_(STATUS)

            SLR = SLR +     SLT*0.5
            ZTH = ZTH + ZTT*SLT*0.5

            if(present(ZTHB)) ZTHB = ZTHB + 0.5*ZTT
            if(present(ZTHD)) then
               where(ZTT>0.0)
                  ZTHD = ZTHD + 0.5*ZTT
                  NCC  = NCC  + 0.5
               end where
            end if

            if(present(DIST)) DIST = DIST + 0.5*DD
         enddo

         if(present(ZTHN)) ZTHN = max(ZTT,0.0)

         where(SLR > 0.0)
            SLR = SLR/dble(NT)
            ZTH = ZTH/dble(NT)
            ZTH = ZTH / SLR
         elsewhere
            ZTH = 0.0
            SLR = 0.0
         end where

         if(present(ZTHB)) ZTHB = ZTHB/dble(NT)

         if(present(ZTHD)) then
            where(NCC>0.0)
               ZTHD = ZTHD/dble(NCC)
            end where
         end if

         if(present(DIST)) DIST = DIST/dble(NT)

         call ESMF_ClockDestroy(myclock, rc=STATUS)
         VERIFY_(STATUS)

      endif MEAN_OR_INST


      RETURN_(ESMF_SUCCESS)

