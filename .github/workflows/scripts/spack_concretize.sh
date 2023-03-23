#!/bin/bash

# get arguments
while getopts a:c:d:i:r: flag
do
  case "${flag}" in
    a) arch=${OPTARG};;
    c) comp=${OPTARG};;
    d) deps=${OPTARG};;
    i) install_dir=${OPTARG};;
    r) run_dir=${OPTARG};;
  esac
done

echo $deps

# check for default values
if [ -z "$deps" ]; then
  echo "Dependencies are not given! Exiting ..."
  exit
fi

if [ ! -z `echo $deps | grep '^-'` ]; then
  echo "argument -d is given but dependencies are not listed!"
  exit
fi
if [[ -z "$install_dir" || ! -z `echo $install_dir | grep '^-'` ]]; then
  install_dir="$HOME/.spack-ci"
fi

if [[ -z "$run_dir" || ! -z `echo $run_dir | grep '^-'` ]]; then
  run_dir=`pwd`
fi

if [[ -z "$arch" || ! -z `echo $arch | grep '^-'` ]]; then
  arch="x86_64_v4"
fi

if [[ -z "$comp" || ! -z `echo $comp | grep '^-'` ]]; then
  comp="gcc@11.3.0"
fi

# print out arguments
echo "Target Architecture: $arch"
echo "Compiler: $comp"
echo "Dependencies: $deps"
echo "Install Directory: $install_dir"
echo "Run Directory: $run_dir"

# go to directory
cd $run_dir

# checkout spack
echo "::group::Checkout Spack"
#git clone https://github.com/spack/spack.git
git clone -b esmf-update https://github.com/theurich/spack.git
echo "::endgroup::"

# find available compilers
if [[ "$comp" == *"oneapi"* ]]; then
  echo "::group::Find Available Compilers and MPIs"
  # find hpckit version (same with basekit)
  hpckit_pkg_version=`echo "$comp" | awk -F\@ '{print $2}'`
  echo "hpckit_pkg_version = $hpckit_pkg_version"

  # find compiler package and version from hpckit specification
  comp_pkg=`apt-cache depends intel-hpckit-$hpckit_pkg_version | grep "intel-oneapi-compiler-fortran-" | awk '{print $2}'`
  comp_version=`echo "$comp_pkg" | awk -F\- '{print $5}'`
  echo "comp_pkg           = $comp_pkg"
  echo "comp_version       = $comp_version"

  # find MPI package and version from hpckit specification
  mpi_devel_pkg=`apt-cache depends intel-hpckit-$hpckit_pkg_version | grep "intel-oneapi-mpi-devel-" | awk '{print $2}'`
  mpi_pkg=`apt-cache depends $mpi_devel_pkg | grep "intel-oneapi-mpi-[1-9]" | awk '{print $2}'`
  mpi_version=`echo "$mpi_pkg" | awk -F\- '{print $4}'`
  echo "mpi_devel_pkg      = $mpi_devel_pkg"
  echo "mpi_pkg            = $mpi_pkg"
  echo "mpi_version        = $mpi_version"
  echo "::endgroup::"

  # create config file
  echo "::group::Create Config file to Load Compiler"
  echo "compiler=$comp_version" > config.txt
  echo "mpi=$mpi_version" >> config.txt
  cat config.txt
  echo "::endgroup::"

  # load compiler
  echo "::group::Load Intel One API Compiler" 
  . /opt/intel/oneapi/setvars.sh --config=config.txt --force
  comp="oneapi@$comp_version"
  mpi="oneapi-mpi@$mpi_version"
  echo "::endgroup::"

  # test installation by building and running simple MPI job
  echo "::group::Test Installation"
  echo "program test_mpi" > test_mpi.F90
  echo "  use mpi" >> test_mpi.F90
  echo "  integer :: err, p, id" >> test_mpi.F90
  echo "  call MPI_Init(err)" >> test_mpi.F90
  echo "  call MPI_Comm_size(MPI_COMM_WORLD, p, err)" >> test_mpi.F90
  echo "  call MPI_Comm_rank(MPI_COMM_WORLD, id, err)" >> test_mpi.F90
  echo "  print*, 'Hello from ', id, ' of ', p" >> test_mpi.F90
  echo "  call MPI_Finalize(err)" >> test_mpi.F90
  echo "end program test_mpi" >> test_mpi.F90

  mpiifort -o test_mpi.x test_mpi.F90
  mpirun -np 2 ./test_mpi.x
  echo "::endgroup::"
fi

exit

# find compilers
. spack/share/spack/setup-env.sh
spack compiler find
cat ~/.spack/linux/compilers.yaml
echo "::endgroup::"

# find external tools
echo "::group::Find Externals"
spack external find
echo "::endgroup::"

# add Intel MPI to spack
if [[ "$comp" == *"oneapi"* ]]; then
  echo "::group::Create packages.yaml"
  echo "packages:" > ~/.spack/packages.yaml 
  echo "  mpi:" >> ~/.spack/packages.yaml
  echo "    buildable: false" >> ~/.spack/packages.yaml
  echo "    require:" >> ~/.spack/packages.yaml
  echo "    - one_of: [oneapi-mpi%${comp}]" >> ~/.spack/packages.yaml
  echo "  oneapi-mpi:" >> ~/.spack/packages.yaml
  echo "    externals:" >> ~/.spack/packages.yaml
  echo "    - spec: ${mpi}%${comp}" >> ~/.spack/packages.yaml
  echo "      prefix: /opt/intel/oneapi/mpi/$mpi_version" >> ~/.spack/packages.yaml
  echo "    buildable: false" >> ~/.spack/packages.yaml
  cat ~/.spack/packages.yaml
  echo "::endgroup::"
fi

# create spack.yaml
echo "::group::Create spack.yaml"
echo "spack:" > spack.yaml
echo "  concretizer:" >> spack.yaml
echo "    targets:" >> spack.yaml
echo "      host_compatible: false" >> spack.yaml
echo "    unify: true" >> spack.yaml
echo "  specs:" >> spack.yaml
IFS=', ' read -r -a array <<< "$deps"
for d in "${array[@]}"
do
  echo "  - $d %$comp target=$arch" >> spack.yaml
done
echo "  packages:" >> spack.yaml
echo "    all:" >> spack.yaml
echo "      target: ['$arch']" >> spack.yaml
echo "      compiler: [$comp]" >> spack.yaml
echo "      providers:" >> spack.yaml
if [[ "$comp" == *"oneapi"* ]]; then
echo "        mpi: [$mpi]" >> spack.yaml
else
echo "        mpi: [openmpi]" >> spack.yaml
fi
echo "  view: $install_dir/view" >> spack.yaml
echo "  config:" >> spack.yaml
echo "    source_cache: $install_dir/source_cache" >> spack.yaml
echo "    misc_cache: $install_dir/misc_cache" >> spack.yaml
echo "    test_cache: $install_dir/test_cache" >> spack.yaml
echo "    install_tree:" >> spack.yaml
echo "      root: $install_dir/opt" >> spack.yaml
#echo "    install_missing_compilers: true" >> spack.yaml
cat spack.yaml
echo "::endgroup::"

# concretize spack environment
echo "::group::Concretize Spack Environment Using YAML Specification"
spack --color always -e $run_dir/. concretize -f
echo "::endgroup::"
