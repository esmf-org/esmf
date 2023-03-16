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

# download the key to system keyring
wget -O- https://apt.repos.intel.com/intel-gpg-keys/GPG-PUB-KEY-INTEL-SW-PRODUCTS.PUB | \
  gpg --dearmor | sudo tee /usr/share/keyrings/oneapi-archive-keyring.gpg > /dev/null

# add signed entry to apt sources and configure the APT client to use Intel repository
echo "deb [signed-by=/usr/share/keyrings/oneapi-archive-keyring.gpg] \
  https://apt.repos.intel.com/oneapi all main" | sudo tee /etc/apt/sources.list.d/oneAPI.list

# update packages list and repository index
sudo apt-get update

# get compiler version
compiler_version=`echo "$comp" | awk -F\@ '{print $2}'`

# check the version is available or not
suffix=""
if [ ! -z "$compiler_version" ]; then
  list_compiler_versions=`apt list 2>&1 | grep "intel-basekit-[1-9]" | awk -F\- '{print $3}' | awk -F\/ '{printf $1" "}'`
  in_the_list=0
  for i in $list_compiler_versions
  do
    if [[ "$i" == "$compiler_version" ]]; then
      in_the_list=1
      break
    fi
  done
  if [ $in_the_list == 1 ]; then
    suffix="-$compiler_version"
  fi
fi

# install
sudo apt-get install intel-basekit${suffix}
sudo apt-get install intel-hpckit${suffix}
