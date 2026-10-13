# ParallelIO

The Parallel IO libraries (PIO) are high-level parallel I/O C and
Fortran libraries for applications that need to do netCDF I/O from
large numbers of processors on a HPC system.

PIO provides a netCDF-like API, and allows users to designate some
subset of processors to perform IO. Computational code calls
netCDF-like functions to read and write data, and PIO uses the IO
processors to perform all necessary IO.

## Intracomm Mode

In Intracomm mode, PIO allows the user to designate some subset of
processors to do all I/O. The I/O processors also participate in
computational work.

![I/O on Many Processors with Async
 Mode](./doc/images/I_O_on_Many_Intracomm.png)

## Async Mode

PIO also supports the creation of multiple computation components,
each containing many processors, and one shared set of IO
processors. The computational components can perform write operation
asynchronously, and the IO processors will take care of all storage
interaction.

![I/O on Many Processors with Async
 Mode](./doc/images/I_O_on_Many_Async.png)

## Website

For complete documentation, see our website at
[http://ncar.github.io/ParallelIO/](http://ncar.github.io/ParallelIO/).

## Mailing List

The (low-traffic) PIO mailing list is at
https://groups.google.com/forum/#!forum/parallelio, send email to the
list at parallelio@googlegroups.com.

## Testing

The results of our continuous integration testing with GitHub actions
can be found on any of the Pull Requests on the GitHub site:
https://github.com/NCAR/ParallelIO.

The results of our nightly tests on multiple platforms can be found on
our cdash site at
[http://my.cdash.org/index.php?project=PIO](http://my.cdash.org/index.php?project=PIO).

## Dependencies

PIO can use NetCDF (version 4.6.1+) and/or PnetCDF (version 1.9.0+)
for I/O. NetCDF may be built with or without netCDF-4 features. NetCDF
is required for PIO, PnetCDF is optional.

The NetCDF C library must be built with MPI, which requires that it be
linked with an MPI-enabled version of HDF5. Optionally, NetCDF can be
built with DAP support, which introduces a dependency on CURL.  HDF5,
itself, introduces dependencies on LIBZ and (optionally) SZIP.

## Building PIO

To build PIO, unpack the distribution tarball and do:

```
CC=mpicc FC=mpif90 ./configure --enable-fortran && make check install
```

For a full description of the available options and flags, try:
```
./configure --help
```

Note that environment variables CC and FC may need to be set to the
MPI versions of the C and Fortran compiler. Also CPPFLAGS and LDFLAGS
may need to be set to indicate the locations of one or more of the
dependent libraries. (If using MPI compilers, the entire set of
dependent libraries should be built with the same compilers.) For
example:

```
export CC=mpicc
export FC=mpifort
export CPPFLAGS='-I/usr/local/netcdf-fortran-4.4.5_c_4.6.3_mpich-3.2/include -I/usr/local/netcdf-c-4.6.3_hdf5-1.10.5/include -I/usr/local/pnetcdf-1.11.0_shared/include'
export LDFLAGS='-L/usr/local/netcdf-c-4.6.3_hdf5-1.10.5/lib -L/usr/local/pnetcdf-1.11.0_shared/lib'
./configure --prefix=/usr/local/pio-2.4.2 --enable-fortran
make check
make install
```

## Building with CMake

The typical configuration with CMake can be done as follows:

```
CC=mpicc FC=mpif90 cmake [-DOPTION1=value1 -DOPTION2=value2 ...] /path/to/pio/source
```

Full instructions for the cmake build can be found in the installation
documentation.

# References

Hartnett, E., Edwards, J., "THE PARALLELIO (PIO) C/FORTRAN LIBRARIES
FOR SCALABLE HPC PERFORMANCE", 37th Conference on Environmental
Information Processing Technologies, American Meteorological Society
Annual Meeting, January, 2021. Retrieved on Feb 3, 2021, from
[https://www.researchgate.net/publication/348169990_THE_PARALLELIO_PIO_CFORTRAN_LIBRARIES_FOR_SCALABLE_HPC_PERFORMANCE].

Hartnett, E., Edwards, J., "POSTER: THE PARALLELIO (PIO) C/FORTRAN LIBRARIES
FOR SCALABLE HPC PERFORMANCE", 37th Conference on Environmental
Information Processing Technologies, American Meteorological Society
Annual Meeting, January, 2021. Retrieved on Feb 3, 2021, from
[https://www.researchgate.net/publication/348170136_THE_PARALLELIO_PIO_CFORTRAN_LIBRARIES_FOR_SCALABLE_HPC_PERFORMANCE].
