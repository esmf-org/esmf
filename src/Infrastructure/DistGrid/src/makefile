
# longs
DO_F = lf95
DO_CC = g++

# halem
#DO_F = f90
#DO_CC = cxx

# any/all
DO_C = cc

CLASSES = ESMF_DistGrid.o 
IFLAGS = -I../../Base/interface -I../../IO/interface 

all: ${CLASSES}


ESMF_DistGrid.o : ESMF_DistGrid.F90
	${DO_F} -c ${IFLAGS} ESMF_DistGrid.F90


clean:
	rm -f *.o *.mod
