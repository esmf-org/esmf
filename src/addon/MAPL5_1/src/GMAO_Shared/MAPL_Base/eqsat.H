

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

