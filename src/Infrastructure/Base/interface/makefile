
# longs
DO_F = lf95
DO_CC = g++

# halem
#DO_F = f90
#DO_CC = cxx

# any/all
DO_C = cc

all: ESMF_Base.o

ESMF_Base.o: ESMF_Base.F90
	${DO_F} -c ESMF_Base.F90

clean:
	rm -f  *.o *.mod
