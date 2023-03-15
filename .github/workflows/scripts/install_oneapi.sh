#!/bin/bash

# get arguments
#while getopts i: flag
#do
#  case "${flag}" in
#    i) install_dir=${OPTARG};;
#  esac
#done

# check argument
#if [[ -z "$install_dir" || ! -z `echo $install_dir | grep '^-'` ]]; then
#  install_dir="/opt/intel/oneapi"
#fi

# print out arguments
#echo "Install Directory: $install_dir";
#
#

# install core packages
#apt-get update
#apt-get install -y python3-dev python3-pip

# download installation script

#wget https://registrationcenter-download.intel.com/akdlm/irc_nas/19084/l_HPCKit_p_2023.0.0.25400.sh
#chmod 755 l_HPCKit_p_2023.0.0.25400.sh

# install components
#./l_HPCKit_p_2023.0.0.25400.sh -a --silent --eula accept \
#  --install-dir $install_dir \
#  --components intel.oneapi.lin.dpcpp-cpp-compiler-pro:intel.oneapi.lin.ifort-compiler:intel.oneapi.lin.mpi.devel

# download the key to system keyring
wget -O- https://apt.repos.intel.com/intel-gpg-keys/GPG-PUB-KEY-INTEL-SW-PRODUCTS.PUB | \
  gpg --dearmor | sudo tee /usr/share/keyrings/oneapi-archive-keyring.gpg > /dev/null

# add signed entry to apt sources and configure the APT client to use Intel repository
echo "deb [signed-by=/usr/share/keyrings/oneapi-archive-keyring.gpg] \
  https://apt.repos.intel.com/oneapi all main" | sudo tee /etc/apt/sources.list.d/oneAPI.list

# update packages list and repository index
sudo apt-get update
sudo apt-get install apt-show-versions

# install
sudo apt-get install intel-basekit
sudo apt-get install intel-hpckit
apt-show-versions -b -a -p intel-basekit
apt-show-versions -b -a -p intel-hpckit
