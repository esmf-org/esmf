#!/bin/bash

# get arguments
while getopts c: flag
do
  case "${flag}" in
    c) comp=${OPTARG};;
  esac
done

# print out arguments
echo "Compiler: $comp"

# exit if it is already installed
if [ -d "/opt/intel/oneapi" ]; then
  echo "/opt/intel/oneapi directory exists. Skip installing Intel oneAPI Compiler ..."
  exit
fi

# download the key to system keyring
echo "::group::Setup Intel oneAPI Repository"
wget -O- https://apt.repos.intel.com/intel-gpg-keys/GPG-PUB-KEY-INTEL-SW-PRODUCTS.PUB | \
  gpg --dearmor | sudo tee /usr/share/keyrings/oneapi-archive-keyring.gpg > /dev/null

# add signed entry to apt sources and configure the APT client to use Intel repository
echo "deb [signed-by=/usr/share/keyrings/oneapi-archive-keyring.gpg] \
  https://apt.repos.intel.com/oneapi all main" | sudo tee /etc/apt/sources.list.d/oneAPI.list

# update packages list and repository index
sudo apt-get -qq update
echo "::endgroup::"

# get compiler version
echo "::group::Check Specified Versiion"
compiler_version=`echo "$comp" | awk -F\@ '{print $2}'`
echo "Compiler Version: $compiler_version"

# check the version is available or not
if [ -z "$compiler_version" ]; then
  echo "Compiler version needs to be specified! Exiting ..."
  exit 1
else
  list_compiler_versions=`apt list 2>&1 | grep "intel-basekit-[1-9]" | awk -F\- '{print $3}' | awk -F\/ '{printf $1" "}'`
  echo "List of available Intel oneAPI basekit, hpckit versions: $list_compiler_versions"
  in_the_list=0
  for i in $list_compiler_versions
  do
    if [[ "$i" == "$compiler_version" ]]; then
      in_the_list=1
      break
    fi
  done
  if [ $in_the_list == 0 ]; then
    echo "Specified compiler version ($compiler_version) is not available! Exiting ..."
    exit 1
  fi
fi
echo "::endgroup::"

# install
echo "::group::Install Intel oneAPI Base and HPC Kits"
sudo apt-get install -qq intel-basekit-$compiler_version
sudo apt-get install -qq intel-hpckit-$compiler_version
sudo setfacl -R -m u:`whoami`:rwx /opt/intel
echo "::endgroup::"

# list content of the installation folders (for debugging)
echo "::group::List Files in Installation Folder"
echo "> ls -al /opt/intel/oneapi/compiler/"
ls -al /opt/intel/oneapi/compiler/
echo "> ls -al /opt/intel/oneapi/mpi/"
ls -al /opt/intel/oneapi/mpi/
echo "::endgroup::"
