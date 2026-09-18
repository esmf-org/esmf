module load mpi/mpich-x86_64
export ESMF_DIR=/home/ilcentro/Work/NASA/ALI/ESMF
export ESMF_COMM=mpich
export ESMF_PIO=internal
export ESMF_NETCDF=nc-config
export ESMF_NETCDF_INCLUDE=/usr/lib64/gfortran/modules
export ESMF_NETCDFF_INCLUDE=/usr/lib64/gfortran/modules
export ESMF_GDAL=true
#export ESMF_GDAL_INCLUDE=/usr/include/gdal
export ESMF_GDAL_INCLUDE=/home/ilcentro/tools/gdal-debug/include
#export ESMF_SHAPEFILE_INCLUDE=/usr/include
#export ESMF_GDAL_LIBPATH=/usr/lib64
export ESMF_GDAL_LIBPATH=/home/ilcentro/tools/gdal-debug/lib64
#export ESMF_SHAPEFILE_LIBS=-lshp
export ESMF_GDAL_LIBS=-lgdal
export ESMF_BOPT=g
export ESMF_CXXCOMPILEOPTS="-DESMFIO_DEBUG -DESMF_REGRID_DEBUG_WRITE_MESH"
