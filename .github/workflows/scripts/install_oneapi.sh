#!/bin/bash

# download the key to system keyring
wget -O- https://apt.repos.intel.com/intel-gpg-keys/GPG-PUB-KEY-INTEL-SW-PRODUCTS.PUB | \
  gpg --dearmor | sudo tee /usr/share/keyrings/oneapi-archive-keyring.gpg > /dev/null

# add signed entry to apt sources and configure the APT client to use Intel repository
echo "deb [signed-by=/usr/share/keyrings/oneapi-archive-keyring.gpg] \
  https://apt.repos.intel.com/oneapi all main" | sudo tee /etc/apt/sources.list.d/oneAPI.list

# update packages list and repository index
sudo apt-get update
sudo apt-get install apt-show-versions

# get list of available mpi versions
apt list | grep "intel-oneapi-mpi-" | grep -v devel | awk -F\/ '{print $1}' >& mpi_versions.txt

# get list of available fortran compilers
apt list | grep "intel-oneapi-compiler-fortran-" | grep -v devel | grep -v common | grep -v runtime | awk -F\/ '{print $1}' >& fc_versions.txt

# install
#sudo apt-get install intel-basekit
#sudo apt-get install intel-hpckit
#sudo apt-get 
#sudo apt-get intel-oneapi-mpi-2021.8.0
sudo apt-get install `tail -n 1 fc_versions.txt`
sudo apt-get install `tail -n 1 mpi_versions.txt`
