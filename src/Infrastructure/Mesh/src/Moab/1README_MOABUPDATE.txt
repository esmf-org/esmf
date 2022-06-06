# MOAB version update README

# This procedure can be used to update the internal MOAB library in ESMF to a new version

# build external moab:

cd $MOAB_DIR
make clean
autoreconf -fi

CC=/usr/local/bin/mpicc CXX=/usr/local/bin/mpicxx F77=/usr/local/bin/mpif77 FC=/usr/local/bin/mpif90 ./configure --enable-shared --enable-debug --with-mpi=/usr/local/lib --with-lapack="-L/usr/local/lib/openblas -lopenblas" --prefix=~/MOAB531 > configure.out 2>&1

make all > make.out 2>&1 &
make check
make install


# esmf with external moab
export ESMF_MOAB=external
export ESMF_MOAB_INCLUDE=~/MOAB531/include
export ESMF_MOAB_LIBPATH=~/MOAB531/lib
export ESMF_MOAB_LIBS=-lMOAB

# build and test esmf with external moab first
cd $ESMF_DIR; make distclean; make; make all_tests

# copy Matrix3.hpp file
cp src/Infrastructure/Mesh/src/Moab/src/Matrix3.hpp /tmp

# copy all files from moab to esmf, clean up from build first
cd $MOAB_DIR
make clean
\cp -R src/* ../esmf/src/Infrastructure/Mesh/src/Moab/

# edit new Matrix3.hpp to use ESMF LAPACK symbols (as with old copy in /tmp)

# reset to internal MOAB
export ESMF_MOAB=internal
unset ESMF_MOAB_INCLUDE
unset ESMF_MOAB_LIBPATH
unset ESMF_MOAB_LIBS

# build esmf and run all tests
cd $ESMF_DIR; make distclean; make; make all_tests
