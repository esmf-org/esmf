
# longs
DO_F = lf95
DO_CC = g++

# halem
#DO_F = f90
#DO_CC = cxx

# any/all
DO_C = cc

CLASSES = ESMF_Grid.o
IFLAGS = -I../../Base/interface -I../../IO/interface -I../../DistGrid/src \
         -I../../PhysGrid/src

all: ${CLASSES}


ESMF_Grid.o : ESMF_Grid.F90
	${DO_F} -c ${IFLAGS} ESMF_Grid.F90


clean:
	rm -f *.o *.mod
