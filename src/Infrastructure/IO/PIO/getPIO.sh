#! /bin/bash

# getPio.sh -- Bring in a new version of PIO, warning about possible changes.
# $\Id$

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

##
## fileInList is a helper function test that returns true iff
## the test file (arg $1) is in a colon-separated list of files (arg $2)
## The function "returns" (i.e., echos) "yes" or "no" as the result
##
fileInList() {
  ans="no"
  if [ $# -ne 2 ]; then
    echo "INTERNAL ERROR: fileInList requires two arguments"
    exit 3
  fi
  for file in ${2}; do
    if [ "${file}" == "${1}" ]; then
      ans="yes"
      break
    fi
  done
  echo ${ans}
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

piofiles="`(cd pio; ls *.F90 *.cc *.c 2> /dev/null)`"
for file in $piofiles; do
  sfile=`echo ${file} | sed -e 's/cc$/C/'`
  lfile="`ls $sfile 2> /dev/null`"
  if [ -z "${lfile}" ]; then
    if [ "${file}" != "pio_cpp_sizes.F90" ]; then
      if [ -n "${changealert}" ]; then
        echo -e $changealert
        changealert=""
      fi
      echo $file
    fi
  fi
done

changealert="\n${stars}\nFiles have been"
changealert="${changealert} removed from PIO, remove these from makefile"
changealert="${changealert}\n${stars}\n"

currfiles="`(ls *.F90 *.C *.c 2> /dev/null)`"
for file in $currfiles; do
  sfile=`echo ${file} | sed -e 's/C$/cc/'`
  lfile="`(cd pio; ls $sfile 2> /dev/null)`"
  if [ -z "${lfile}" ]; then
    if [ "${file}" != "pnetcdfversion.c" ]; then
      if [ -n "${changealert}" ]; then
        echo -e $changealert
        changealert=""
      fi
      echo $file
    fi
  fi
done

## Remove all current F90, C, and C++ files as well as header files
## Exception, keep pnetcdfversion.c
keepfiles="pnetcdfversion.c"
remlist=`ls *.F90 *.h *.cc *.C *.c 2> /dev/null`
for file in ${remlist}; do
  if [ "`fileInList ${file} \"${keepfiles}\"`" != "yes" ]; then
    rm -f ${file}
    res=$?
    if [ $res -ne 0 ]; then
      echo "ERROR: unable to remove old PIO file, \"${file}\""
      rm -rf pio
      exit $res
    fi
  fi
done

## Create ESMF redefinition files
esmcincname="ESMFPIO.h"
esmcincfile="${esmcincname}"
esmfpioinc="#include \"${esmcincname}\""
esmcpioinc="#include \"../PIO/${esmcincname}\""

# ESMCPIO.h needs to be in the PIO directory so Fortran files can find it
if [ -f "${esmcincfile}" ]; then
  rm -f "${esmcincfile}"
fi
touch "${esmcincfile}"

## Move the files we need into the current directory

# Some files need special handling (or should be ignored)
cppintfiles="pio_cpp_binding.F90"
cppintfiles="${cppintfiles} darray_cpp_binding.F90"
cppintfiles="${cppintfiles} nf_cpp_binding.F90"
ignorefiles="pio_cpp_sizes.F90"
nosymfiles="${cppintfiles} ${ignorefiles}"

# Gather up the public symbols for later substitution
pubsyms=""
for file in pio/*.F90; do
  fname="`basename ${file}`"
  if [ "`fileInList ${fname} \"${nosymfiles}\"`" != "yes" ]; then
    # Add public symbols to list
    newsyms="`cat ${file} | awk -f f90symbols.awk`"
    if [ -n "${newsyms}" ]; then
      if [ -n "${pubsyms}" ]; then
        pubsyms="${pubsyms}:${newsyms}"
      else
        pubsyms="${newsyms}"
      fi
    fi
  fi
done

# Now, bring in the F90 files, translating as necessary along the way
awkinsert="BEGIN { print LINE1 } { print }"

for file in pio/*.F90; do
  fname="`basename ${file}`"
  ifile=${file}
  if [ "`fileInList ${fname} \"${ignorefiles}\"`" == "yes" ]; then
    continue
  fi
  if [ "`fileInList ${fname} \"${cppintfiles}\"`" == "yes" ]; then
    # For the C++ interface files, we have to insert the #include line
    ftemp="`basename ${file} F90`tmp"
    cat ${ifile} | awk -v LINE1="${esmfpioinc}" "${awkinsert}" > ${ftemp}
  # Don't need the original file anymore
    rm ${ifile}
    ifile=${ftemp}
    res=$?
    if [ $res -ne 0 ]; then
      echo "ERROR: unable to create ${fname}"
      rm -rf pio
      exit $res
    fi
  fi
  # Since this takes a while, amuse the spectator with a blow by blow
  echo "Translating \"${fname}\""
  cat ${ifile} | awk -v PUBSYMS="${pubsyms}" -f f90files.awk > ${fname}
  res=$?
  if [ $res -ne 0 ]; then
    echo "ERROR: unable to create ${fname}"
#    rm -rf pio
    exit $res
  fi
  # Don't need the original file anymore
  rm ${ifile}
done

# Now, take care of pio.h
file="pio/pio.h"
if [ -f "${file}" ]; then
  # Add public symbols to header file
  cat ${file} | awk -f cppsymbols.awk >> "${esmcincfile}"
  if [ $res -ne 0 ]; then
      echo "ERROR: unable to create ${esmcincfile}"
      rm -rf pio
      exit $res
  fi
  # Rewrite the file by adding the include line at the top
  fname="../include/pio.h"
  # Make sure it is writable
  chmod 644 $fname
  awkinsert="BEGIN { DIDINSERT = 0 }
            ((DIDINSERT == 0) && (NF == 0)) {
              printf \"%s\\n\\n\", LINE3
              DIDINSERT = 1
            }
            { print }"
  cat ${file} | awk -v LINE3="${esmcpioinc}" "${awkinsert}" > ${fname}
  res=$?
  if [ $res -ne 0 ]; then
      echo "ERROR: unable to create ${fname}"
      rm -rf pio
      exit $res
  fi
  # Don't need the original file anymore
  rm -f ${file}
else
  echo "ERROR: pio.h not found in repository"
  rm -rf pio
  exit 1
fi

# Move the pio_* include files
# First, make sure we can overwrite them (CVS permissions and all that)
rm -f `ls pio/pio_*.h 2> /dev/null | sed -e 's/pio/..\/include/'`
mv pio/pio*.h ../include
res=$?
if [ $res -ne 0 ]; then
  echo "ERROR: unable to install c++ binding header (.h) files"
  rm -rf pio
  exit $res
fi

# Move remaining include files to PIO. This should work since we cleaned up
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
