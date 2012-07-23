#! /bin/bash

##
## General syntax help function
## Usage: help <exit status>
##
help () {
  echo -e "Usage: `basename $0` [ --tag <tagnum> ] [ --branch <branchnum> ]"
  echo -e "                 [ --branch-tag <tagnum> ]"
  echo -e "\nNB: If multiple tag options are given only the last is used"
  exit $1
}

##
## Error output function (should be handed a string)
##
perr() {
  echo -e "\nERROR: ${@}\n"
  help 1
}

## Force us to work in the correct directory
cd `dirname $0`

## By default, check out the PIO trunk
BRANCH="trunk"

## Process our input arguments
while [ $# -gt 0 ]; do
  case $1 in
    --branch)
      if [ $# -lt 2 ]; then
        perr "${1} requires a branch name"
      fi
      BRANCH="branches/$2"
      shift 2
      ;;
    --branch-tag)
      if [ $# -lt 2 ]; then
        perr "${1} requires a branch name"
      fi
      BRANCH="branch_tags/$2"
      shift 2
      ;;
    -h | --help | -help)
      help 0
      ;;
    --tag)
      if [ $# -lt 2 ]; then
        perr "${1} requires a tag name"
      fi
      BRANCH="trunk_tags/$2"
      shift 2
      ;;
    *)
      perr "Unknown argument; $1"
      ;;
  esac
done

if [ "${BRANCH}" == "trunk" ]; then
  echo "Checkout of PIO trunk"
else
  echo "Checkout of PIO tag ${BRANCH}"
fi

## Check out the pio subdir of PIO
svn co -q http://parallelio.googlecode.com/svn/${BRANCH}/pio pio
res=$?
if [ $res -ne 0 ]; then
  echo "ERROR: Checkout of PIO repository failed"
  exit $res
fi

## Check for file changes
stars="**************************************************************"
changealert="\n${stars}\nFiles have been"
changealert="${changealert} added to PIO, add these to makefile"
changealert="${changealert}\n${stars}\n"

piofiles="`(cd pio; ls *.F90 *.cc *.c)`"
for file in $piofiles; do
  sfile=`echo ${file} | sed -e 's/cc$/C/'`
  lfile="`ls $sfile 2> /dev/null`"
  if [ -z "${lfile}" ]; then
    if [ -n "${changealert}" ]; then
      echo -e $changealert
      changealert=""
    fi
    echo $file
  fi
done

changealert="\n${stars}\nFiles have been"
changealert="${changealert} removed from PIO, remove these from makefile"
changealert="${changealert}\n${stars}\n"

currfiles="`(ls *.F90 *.C *.c)`"
for file in $currfiles; do
  sfile=`echo ${file} | sed -e 's/C$/cc/'`
  lfile="`(cd pio; ls $sfile 2> /dev/null)`"
  if [ -z "${lfile}" ]; then
    if [ -n "${changealert}" ]; then
      echo -e $changealert
      changealert=""
    fi
    echo $file
  fi
done

## Remove all current F90, C, and C++ files as well as header files
rm -f *.F90 *.h *.c *.cc *.C
res=$?
if [ $res -ne 0 ]; then
  echo "ERROR: unable to remove old PIO installation"
  rm -rf pio
  exit $res
fi

## Create ESMF redefinition files
esmfincname="ESMFPIO.h"
esmcincname="ESMCPIO.h"
esmfinchead="! WARNING: Auto-generated file, do not edit"
esmcinchead="// WARNING: Auto-generated file, do not edit"

if [ -f "../include/${esmfincname}" ]; then
  rm "../include/${esmfincname}"
fi
echo -e ${esmfinchead} > "../include/${esmfincname}"

if [ -f "../include/${esmcincname}" ]; then
  rm "../include/${esmcincname}"
fi
echo -e ${esmcinchead} > "../include/${esmcincname}"

## Move the files we need into the current directory
## Add include of ESMFPIO.H along the way
esmfpioinc="#include \"${esmfincname}\""
esmcpioinc="#include \"${esmcincname}\""
awkinsert="BEGIN { print LINE1 } { print }"
#mv pio/*.F90 .
for file in pio/*.F90; do
  # Add public symbols to header file
  cat ${file} | awk -f f90symbols.awk >> "../include/${esmfincname}"
  # Rewrite the file by adding the include line at the top
  fname="`basename ${file}`"
  cat ${file} | awk -v LINE1="${esmfpioinc}" "${awkinsert}" > ${fname}
  res=$?
  if [ $res -ne 0 ]; then
    echo "ERROR: unable to create ${fname}"
    rm -rf pio
    exit $res
  fi
  # Don't need the original file anymore
  rm ${file}
done

#mv pio/*.F90.in .

# Now, take care of pio.h
file="pio/pio.h"
if [ -f "${file}" ]; then
  # Add public symbols to header file
  cat ${file} | awk -f cppsymbols.awk >> "../include/${esmcincname}"
  if [ $res -ne 0 ]; then
      echo "ERROR: unable to create ../include/${esmcincname}"
      rm -rf pio
      exit $res
  fi
  # Rewrite the file by adding the include line at the top
  fname="../include/pio.h"
  cat ${file} | awk -v LINE1="${esmfpioinc}" "${awkinsert}" > ${fname}
  res=$?
  if [ $res -ne 0 ]; then
      echo "ERROR: unable to create ${fname}"
      rm -rf pio
      exit $res
  fi
  # Don't need the original file anymore
  rm ${file}
else
  echo "ERROR: pio.h not found in repository"
  rm -rf pio
  exit 1
fi

# Move the rest of the .h files
mv pio/pio*.h ../include
res=$?
if [ $res -ne 0 ]; then
  echo "ERROR: unable to install c++ binding header (.h) files"
  rm -rf pio
  exit $res
fi

mv pio/*.h .
res=$?
if [ $res -ne 0 ]; then
  echo "ERROR: unable to install PIO c header (.h) files"
  rm -rf pio
  exit $res
fi

mv pio/*.c .
res=$?
if [ $res -ne 0 ]; then
  echo "ERROR: unable to install c files"
  rm -rf pio
  exit $res
fi
find pio -name \*.cc |                                             \
sed -e 's/pio\/\([A-Za-z0-9_]*\).cc/mv pio\/\1.cc \1.C/' | bash
res=$?
if [ $res -ne 0 ]; then
  echo "ERROR: unable to install C++ files"
  rm -rf pio
  exit $res
fi

## Blow away the rest
rm -rf pio
res=$?
if [ $res -ne 0 ]; then
  echo "ERROR: unable to remove unnecessary files"
  rm -rf pio
  exit $res
else
  echo "PIO successfully installed from ${BRANCH}"
fi
