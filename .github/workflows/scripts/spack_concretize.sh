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
git clone https://github.com/spack/spack.git
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
fi

# find compilers
. spack/share/spack/setup-env.sh
spack compiler find
cat ~/.spack/packages.yaml
echo "::endgroup::"

# find external tools, excluding cmake since runner uses 4.0 which is not compatible with some libraries
echo "::group::Find Externals"
spack external find --exclude cmake
echo "::endgroup::"

# create config file (to fix FetchError issue)
echo "::group::Create config.yaml"
echo "config:" > ~/.spack/config.yaml
echo "  url_fetch_method: curl" >> ~/.spack/config.yaml
echo "  connect_timeout: 60" >> ~/.spack/config.yaml
cat ~/.spack/config.yaml
echo "::endgroup::"

# add Intel MPI to spack
if [[ "$comp" == *"oneapi"* ]]; then
  echo "::group::Create packages.yaml"
  #echo "packages:" > ~/.spack/packages.yaml 
  #echo "  mpi:" >> ~/.spack/packages.yaml
  #echo "    buildable: false" >> ~/.spack/packages.yaml
  #echo "    require:" >> ~/.spack/packages.yaml
  #echo "    - one_of: [${mpi}%${comp}]" >> ~/.spack/packages.yaml
  #echo "  oneapi-mpi:" >> ~/.spack/packages.yaml
  #echo "    externals:" >> ~/.spack/packages.yaml
  #echo "    - spec: ${mpi}%${comp}" >> ~/.spack/packages.yaml
  #echo "      prefix: /opt/intel/oneapi/mpi/$mpi_version" >> ~/.spack/packages.yaml
  #echo "    buildable: false" >> ~/.spack/packages.yaml
  echo "packages:" >> ~/.spack/packages.yaml
  echo "  intel-oneapi-mpi:" >> ~/.spack/packages.yaml
  echo "    externals:" >> ~/.spack/packages.yaml
  echo "    - spec: intel-${mpi}" >> ~/.spack/packages.yaml
  echo "      prefix: /opt/intel/oneapi/" >> ~/.spack/packages.yaml
  echo "    buildable: false" >> ~/.spack/packages.yaml
  cat ~/.spack/packages.yaml
  echo "::endgroup::"
fi

# check given gcc compiler is found or not? If not, use newer version
if [[ "$comp" == *"gcc"* ]]; then
  echo "::group::Check gcc compiler"
  str=`echo $comp | awk -F\@ '{print $1}'`
  comp_ver=`spack compiler list | grep "${str}@" | tr -d "${str}@" | sort -n | tail -n 1`

  use_latest=0
  if [[ "$comp" == *"gcc@latest"* ]]; then
     echo "The gcc@latest is set. Trying to find latest available gcc compiler ..."
     use_latest=1
  elif [ -z "$(cat ~/.spack/packages.yaml | grep $comp)" ]; then
     echo "Given compiler ($comp) is not found! Exiting ..."
     exit 1
  fi

  if [[ $use_latest == 1 ]]; then
     comp="${str}@$comp_ver"
  fi
  echo "Using $comp gnu compiler."
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
echo "        mpi: [intel-${mpi}]" >> spack.yaml
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
exc=$?
if [ $exc -ne 0 ]; then
  echo "Error in concretizing dependencies! exit code is $exc ..."
  exit $exc
fi
echo "::endgroup::"
