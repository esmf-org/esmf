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
