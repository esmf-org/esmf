#!/usr/bin/env python
#PBS  -r n 
#PBS  -j oe 
#PBS  -S /bin/bash  
#PBS  -l select=1:ncpus=128:mpiprocs=128:ompthreads=1:mem=230GB
#PBS  -N parallelioTest
#PBS  -A P93300606
#PBS  -q main
#PBS  -l walltime=08:00:00

import os, sys, shutil
import subprocess
lmod_root = os.environ["LMOD_ROOT"]
sys.path.append(lmod_root+"/lmod/init/")
from env_modules_python import module


compilers = ["cce/15.0.1", "intel/2023.0.0", "intel-oneapi/2023.0.0", "nvhpc/23.5", "gcc/12.2.0"]
#compilers = ["intel/2023.0.0"]
#mpilibs = ["cray-mpich/8.1.25", "intel-mpi/2021.8.0", "mpi-serial/2.3.0"]
mpilibs = ["mpi-serial/2.3.0","cray-mpich/8.1.25"]
netcdf = ["netcdf/4.9.2","netcdf-mpi/4.9.2"]
pnetcdf = ["parallel-netcdf/1.12.3"]

cmakeopts = [None,["-DPIO_ENABLE_FORTRAN=OFF"],["-DPIO_ENABLE_TIMING=OFF","-DPIO_ENABLE_NETCDF_INTEGRATION=ON"],["-DWITH_PNETCDF=OFF"]]

piodir = "/glade/work/jedwards/sandboxes/ParallelIO"
bldcnt=0
module("purge")
module("load", "ncarenv/23.06")
module("load", "cesmdev/1.0")
module("load", "cmake/3.26.3")

for compiler in compilers:
    cmd = " load "+compiler
    module("load", compiler)
    for mpilib in mpilibs:
        module("load", mpilib)
        cmakeflags = ["-DPIO_ENABLE_EXAMPLES=OFF"]
        for netlib in netcdf:
            module("unload", "netcdf")
            module("unload", "hdf5")
            module("unload", "netcdf-mpi")
            module("unload", "hdf5-mpi")
            if "mpi-serial" in mpilib:
                cc = os.getenv("CC")
                ftn = os.getenv("FC")
                if not cc:
                    cc = "cc"
                if not ftn:
                    ftn = "ftn"
                mpi_serial = os.environ["NCAR_ROOT_MPI_SERIAL"]
                cmakeflags.extend(["-DPIO_USE_MPISERIAL=ON","-DMPISERIAL_PATH="+mpi_serial,"-DPIO_ENABLE_TESTS=OFF","-DPIO_ENABLE_TIMING=OFF"])
                if "mpi" in netlib:
                    continue

            module("load", netlib)
            if not "mpi-serial" in mpilib:
                cc = "cc"
                ftn = "ftn"
                for plib in pnetcdf:
                    module("load", plib)
            cflags = " -g "
            fflags = " -g " 
            if "gcc" in compiler:
                fflags += " -ffree-line-length-none -ffixed-line-length-none  -fallow-argument-mismatch "
            elif "intel" in compiler:
                cflags += " -std=gnu99 "
            if "mpi-serial" in mpilib:
                fflags += " -I$NCAR_INC_MPI_SERIAL "
            module("list")
            for cmakeopt in cmakeopts:
                if cmakeopt and "mpi-serial" in mpilib and "PNETCDF" in cmakeopt:
                    continue
                bldcnt = bldcnt+1
                bld = f"/glade/derecho/scratch/jedwards/piotest/bld{bldcnt:02d}"
                os.chdir(piodir)
                if os.path.exists(bld):
                    shutil.rmtree(bld)
                os.mkdir(bld)
                print(" ")
                print(f"bld is {bld}", flush=True)
                os.environ["CC"] = cc
                os.environ["FC"] = ftn
                os.environ["CFLAGS"] = cflags
                os.environ["FFLAGS"] = fflags
                cmake = ["cmake"]

                if cmakeflags:
                    cmake.extend(cmakeflags)
                if cmakeopt:
                    cmake.extend(cmakeopt)
                cmake.extend([piodir])
                print("Running cmake")
                print(f"compiler = {compiler} netcdf={netlib} mpilib={mpilib} cmake = {cmake}", flush=True)
                cmakeout = None
                try:
                    cmakeout = subprocess.run(cmake, check=True, cwd=bld, capture_output=True, text=True)
#                    cmakeout = subprocess.run(cmake, check=True, cwd=bld)
                except:
                    #print(f"cmake process failed {cmakeout.stdout}")
                    continue


                printline = False
                for line in cmakeout.stdout.split("\n"):
                    if "PIO Configuration Summary" in line:
                        printline = True
                    if printline:
                        print(line, flush=True)
                #                print(f"cmakeout is {cmakeout.stdout}")
                ctestout = None
                print("Running make", flush=True)
                if "mpi-serial" in mpilib:
                    try:
                        makeout = subprocess.run(["make"], check=True, cwd=bld, capture_output=True, text=True)
                    except:
                        print("mpi-serial make process failed")
                else:
                    try:
                        makeout = subprocess.run(["make", "tests"], check=True, cwd=bld, capture_output=True, text=True)
                    except:
                        print("make tests failed")
                        continue
                    try:
                        print("Running ctest", flush=True)
                        ctestout = subprocess.run(["ctest"], capture_output=True, cwd=bld, text=True)
                    except:
                        print("ctest process failed")
                #print(f"makeout is {makeout.stdout}")
                if ctestout:
                    for line in ctestout.stdout.split("\n"):
                        if "Fail" in line or "tests fail" in line:                            
                            print(line, flush=True)

