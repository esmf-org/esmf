

use MAPL_Mod


integer i,l
integer i1, i2, rate

real*4, dimension(140:340) :: q1, q2, q3, q4, q5, q6, t, p
real*8, dimension(140:340) :: qd1, qd2, qd3, qd4, qd5, qd6, td, pd


open(20,file='eqsat_verification.dat',form='formatted',status='unknown')

call MAPL_EQsatSet(Formulation=MAPL_UseMurphyKoopQsat)

do i=140,340
   p(i)=100000.
   t(i)=i
   pd(i)=100000.
   td(i)=i
   q3(i)=MAPL_Eqsat(t(i))
   q4(i)=MAPL_Eqsat(t(i),OverIce=.true.)
   q5(i)=MAPL_Eqsat(t(i),P(i))
   q6(i)=MAPL_Eqsat(t(i),P(i),OverIce=.true.)
enddo

call mapl_eqsatset(usetable=.false.)

do i=140,340
   q1(i)=mapl_EQSAT(t(i))
   q2(i)=mapl_EQSAT(t(i),OverIce=.true.)
   write(20,*)  t(i),q1(i),q3(i),q2(i),q4(i),q5(i),q6(i)
enddo


do i=140,340
t(i)=i
enddo

call MAPL_EqsatSet(Formulation=MAPL_UseStarrQsat)
q1=MAPL_Eqsat(T)
q4=MAPL_Eqsat(T,OverIce=.true.)
call MAPL_EqsatSet(Formulation=MAPL_UseCAMQsat)
q2=MAPL_Eqsat(T)
q5=MAPL_Eqsat(T,OverIce=.true.)
call MAPL_EqsatSet(Formulation=MAPL_UseMurphyKoopQsat)
q3=MAPL_Eqsat(T)
q6=MAPL_Eqsat(T,OverIce=.true.)


do i=140,340
   write(20,*)  t(i),q1(i),q2(i),q3(i),q4(i),q5(i),q6(i)
enddo


call MAPL_EqsatSet(Formulation=1)
q1=MAPL_Eqsat(T,p)
q4=MAPL_Eqsat(T,p,OverIce=.true.)
call MAPL_EqsatSet(Formulation=2)
q2=MAPL_Eqsat(T,p)
q5=MAPL_Eqsat(T,p,OverIce=.true.)
call MAPL_EqsatSet(Formulation=3)
q3=MAPL_Eqsat(T,p)
q6=MAPL_Eqsat(T,p,OverIce=.true.)


do i=140,340
   write(20,*)  t(i),q1(i),q2(i),q3(i),q4(i),q5(i),q6(i)
enddo


call mapl_eqsatset(usetable=.true.)


call MAPL_EqsatSet(Formulation=MAPL_UseStarrQsat)
q1=MAPL_Eqsat(T)
q4=MAPL_Eqsat(T,OverIce=.true.)
call MAPL_EqsatSet(Formulation=MAPL_UseCAMQsat)
q2=MAPL_Eqsat(T)
q5=MAPL_Eqsat(T,OverIce=.true.)
call MAPL_EqsatSet(Formulation=MAPL_UseMurphyKoopQsat)
q3=MAPL_Eqsat(T)
q6=MAPL_Eqsat(T,OverIce=.true.)


do i=140,340
   write(20,*)  t(i),q1(i),q2(i),q3(i),q4(i),q5(i),q6(i)
enddo


call MAPL_EqsatSet(Formulation=MAPL_UseStarrQsat)
q1=MAPL_Eqsat(T)
q4=MAPL_Eqsat(T,OverIce=.true.)
qd1=MAPL_Eqsat(Td)
qd4=MAPL_Eqsat(Td,OverIce=.true.)

do i=140,340
   write(20,*)  t(i),q1(i),qd1(i),q4(i),qd4(i)
enddo

call MAPL_EqsatSet(Formulation=MAPL_UseCAMQsat)
q2=MAPL_Eqsat(T)
q5=MAPL_Eqsat(T,OverIce=.true.)
qd2=MAPL_Eqsat(Td)
qd5=MAPL_Eqsat(Td,OverIce=.true.)

do i=140,340
   write(20,*)  t(i),q2(i),qd2(i),q5(i),qd5(i)
enddo

call MAPL_EqsatSet(Formulation=MAPL_UseMurphyKoopQsat)
q3=MAPL_Eqsat(T)
q6=MAPL_Eqsat(T,OverIce=.true.)
qd3=MAPL_Eqsat(Td)
qd6=MAPL_Eqsat(Td,OverIce=.true.)

do i=140,340
   write(20,*)  t(i),q3(i),qd3(i),q6(i),qd6(i)
enddo

q6=MAPL_Eqsat(T,p)

call MAPL_EqsatSet(MixingRatio=.true.)

q2=MAPL_Eqsat(T)
q5=MAPL_Eqsat(T,p)

do i=140,340
   write(20,*)  t(i),q3(i),q2(i),q6(i),q5(i)
enddo


stop

end

