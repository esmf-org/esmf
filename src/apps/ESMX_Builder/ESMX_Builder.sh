#!/bin/bash

# usage instructions
usage () {
  printf "\n"
  printf "Usage: $0 [OPTIONS ...] [ESMX_BUILD_FILE]\n"
  printf "\n"
  printf "ESMX_BUILD_FILE\n"
  printf "  ESMX build configuration file\n"
  printf "  (default: esmxBuild.yaml)\n"
  printf "\n"
  printf "OPTIONS\n"
  printf "  --esmx-dir=ESMF_ESMXDIR\n"
  printf "      ESMX source directory\n"
  printf "      (default: use ESMF_ESMXDIR in ESMFMKFILE)\n"
  printf "  --esmfmkfile=ESMFMKFILE\n"
  printf "      ESMF makefile fragment\n"
  printf "      (default: use environment)\n"
  printf "  --build-dir=BUILD_DIR\n"
  printf "      build directory\n"
  printf "      (default: build)\n"
  printf "  --prefix=INSTALL_PREFIX\n"
  printf "      installation prefix\n"
  printf "      (default: install)\n"
  printf "  --build-type=BUILD_TYPE\n"
  printf "      build type; valid options are 'debug', 'release',\n"
  printf "      'relWithDebInfo'\n"
  printf "      (default: release)\n"
  printf "  -g\n"
  printf "      set --build-type=debug\n"
  printf "  --build-args=BUILD_ARGS\n"
  printf "      cmake arguments (e.g. -DARG=VAL)\n"
  printf "  --disable-comps=DISABLE_COMPS\n"
  printf "      disable components\n"
  printf "  --build-jobs=BUILD_JOBS\n"
  printf "      number of jobs used for building esmx and components\n"
  printf "  --load-module=MODULEFILE\n"
  printf "      load modulefile before building\n"
  printf "  --load-bashenv=BASHENV\n"
  printf "      load bash environment file before building\n"
  printf "  --test[=TEST_ARGS], -t[=TEST_ARGS]\n"
  printf "      (beta) enable testing, add TEST_ARGS as needed.\n"
  printf "  --verbose, -v\n"
  printf "      build with verbose output\n"
  printf "\n"
}

# usage error
usage_error () {
  printf "ERROR: $1 $2\n"
  usage
  exit 1
}

# print settings
settings () {
  printf "Settings:\n"
  printf "\n"
  printf "  ESMX_BUILD_FILE=${BUILD_FILE}\n"
  printf "  ESMF_ESMXDIR=${ESMF_ESMXDIR}\n"
  printf "  ESMFMKFILE=${ESMFMKFILE}\n"
  printf "  BUILD_DIR=${BUILD_DIR}\n"
  printf "  INSTALL_PREFIX=${INSTALL_PREFIX}\n"
  printf "  BUILD_TYPE=${BUILD_TYPE}\n"
  printf "  BUILD_ARGS=${BUILD_ARGS}\n"
  printf "  DISABLE_COMPS=${DISABLE_COMPS}\n"
  printf "  BUILD_JOBS=${BUILD_JOBS}\n"
  printf "  MODULEFILE=${MODULEFILE}\n"
  printf "  BASHENV=${BASHENV}\n"
  printf "  TEST=${TEST}\n"
  printf "  TEST_ARGS=${TEST_ARGS}\n"
  printf "  VERBOSE=${VERBOSE}\n"
  printf "\n"
}

# default settings
CWD="${PWD}"
ESMFMKFILE="${ESMFMKFILE:-esmf.mk}"
BUILD_FILE="esmxBuild.yaml"
ESMF_ESMXDIR=""
BUILD_TYPE="release"
BUILD_ARGS=""
DISABLE_COMPS=""
BUILD_JOBS=""
BUILD_DIR="${CWD}/build"
INSTALL_PREFIX="${CWD}/install"
MODULEFILE=""
BASHENV=""
TEST=false
TEST_ARGS=""
VERBOSE=false

# required arguments
if [ "$1" = "--help" ] || [ "$1" = "-h" ]; then
  usage
  exit 0
fi

# process arguments
POSITIONAL_ARGS=()
while [[ $# -gt 0 ]]; do
  case "$1" in
    --help|-h) usage; exit 0 ;;
    --esmx-dir=?*) ESMF_ESMXDIR=${1#*=} ;;
    --esmx-dir)  usage_error "$1" "requires an argument" ;;
    --esmx-dir=) usage_error "$1" "requires an argument" ;;
    --esmfmkfile=?*) ESMFMKFILE=${1#*=} ;;
    --esmfmkfile)  usage_error "$1" "requires an argument" ;;
    --esmfmkfile=) usage_error "$1" "requires an argument" ;;
    --build-dir=?*) BUILD_DIR=${1#*=} ;;
    --build-dir)  usage_error "$1" "requires an argument" ;;
    --build-dir=) usage_error "$1" "requires an argument" ;;
    --prefix=?*) INSTALL_PREFIX=${1#*=} ;;
    --prefix)  usage_error "$1" "requires an argument" ;;
    --prefix=) usage_error "$1" "requires an argument" ;;
    --build-type=?*) BUILD_TYPE=${1#*=} ;;
    --build-type)  usage_error "$1" "requires an argument" ;;
    --build-type=) usage_error "$1" "requires an argument" ;;
    -g) BUILD_TYPE="Debug" ;;
    -g=?*) usage_error "$1" "argument ignored" ;;
    -g=)   usage_error "$1" "argument ignored" ;;
    --build-args=?*) BUILD_ARGS=${1#*=} ;;
    --build-args)  usage_error "$1" "requires an argument" ;;
    --build-args=) usage_error "$1" "requires an argument" ;;
    --build-jobs=?*) BUILD_JOBS=${1#*=} ;;
    --build-jobs)  usage_error "$1" "requires an argument" ;;
    --build-jobs=) usage_error "$1" "requires an argument" ;;
    --disable-comps=?*) DISABLE_COMPS=${1#*=} 
      DISABLE_COMPS=${DISABLE_COMPS/' '/','} 
      DISABLE_COMPS=${DISABLE_COMPS/';'/','} ;;
    --disable-comps)  usage_error  "$1" "requires an argument" ;;
    --disable-comps=) usage_error "$1" "requires an argument" ;;
    --load-module=?*) MODULEFILE=${1#*=} ;;
    --load-module)  usage_error "$1" "requires an argument" ;;
    --load-module=) usage_error "$1" "requires an argument" ;;
    --load-bashenv=?*) BASHENV=${1#*=} ;;
    --load-bashenv)  usage_error "$1" "requires an argument" ;;
    --load-bashenv=) usage_error "$1" "requires an argument" ;;
    --test|-t) TEST=true ;;
    --test=?*|-t=?*) TEST=true; TEST_ARGS=${1#*=} ;;
    --test=)   usage_error "$1" "argument ignored" ;;
    --verbose|-v) VERBOSE=true ;;
    --verbose=?*) usage_error "$1" "argument ignored" ;;
    --verbose=)   usage_error "$1" "argument ignored" ;;
    -?*) printf "ERROR: Unknown option $1\n"; usage; exit 1 ;;
    *) POSITIONAL_ARGS+=("${1}") ;;
  esac
  shift
done
set -- "${POSITIONAL_ARGS[@]}"

set -eu

if [ ! -z "${MODULEFILE}" ] ; then
  if [ ! -f "${MODULEFILE}" ]; then
    printf "ERROR: MODULEFILE does not exist.\n"
    usage; exit 1 ;
  fi
  module use $(cd $(dirname ${MODULEFILE}) && pwd)
  module load $(basename ${MODULEFILE})
fi

if [ ! -z "${BASHENV}" ] ; then
  if [ ! -f "${BASHENV}" ]; then
    printf "ERROR: BASHENV does not exist.\n"
    usage; exit 1 ;
  fi
  source ${BASHENV}
fi

# read ESMX build file from positional arguments
if [[ $# -ge 1 ]]; then
  BUILD_FILE="${1}"
else
  BUILD_FILE="esmxBuild.yaml"
fi

# set ESMF_ESMXDIR using ESMFMKFILE
if [ -z ${ESMF_ESMXDIR} ]; then
  if [ ! -f "${ESMFMKFILE}" ]; then
    echo "ERROR: Cannot locate ESMX directory."
    usage; exit 1
  fi
  ESMF_ESMXDIR=`grep "ESMF_ESMXDIR" ${ESMFMKFILE} || true;`
  if [ -z ${ESMF_ESMXDIR} ]; then
    echo "ERROR: ESMF_ESMXDIR is not listed in ESMFMKFILE."
    echo "       Please check ESMF version"
    exit 1
  fi
  ESMF_ESMXDIR=${ESMF_ESMXDIR#*=}
fi

# check ESMF_ESMXDIR
if [ ! -d "${ESMF_ESMXDIR}" ]; then
  echo "ERROR: ESMF_ESMXDIR directory is missing: ${ESMF_ESMXDIR}"
  usage; exit 1
fi

# check BUILD_FILE
if [ ! -f "${BUILD_FILE}" ]; then
  echo "ERROR: ESMX_BUILD_FILE is missing: ${BUILD_FILE}"
  usage; exit 1
fi

# print settings
if [ "${VERBOSE}" = true ] ; then
  settings
fi

# cmake settings
CMAKE_SETTINGS=("")
if [ ! -z "${BUILD_TYPE}" ]; then
  CMAKE_SETTINGS+=("-DCMAKE_BUILD_TYPE=${BUILD_TYPE}")
fi
if [ ! -z "${INSTALL_PREFIX}" ]; then
  CMAKE_SETTINGS+=("-DCMAKE_INSTALL_PREFIX=${INSTALL_PREFIX}")
fi
if [ ! -z "${BUILD_FILE}" ]; then
  CMAKE_SETTINGS+=("-DESMX_BUILD_FILE=${BUILD_FILE}")
fi
if [ ! -z "${DISABLE_COMPS}" ]; then
  CMAKE_SETTINGS+=("-DESMX_DISABLE_COMPS=${DISABLE_COMPS}")
fi
if [ "${TEST}" = true ]; then
  CMAKE_SETTINGS+=("-DESMX_TEST=ON")
fi
if [ "${VERBOSE}" = true ]; then
  CMAKE_SETTINGS+=("-DESMX_BUILD_VERBOSE=ON")
fi
if [ ! -z "${BUILD_JOBS}" ]; then
  CMAKE_SETTINGS+=("-DESMX_BUILD_JOBS=${BUILD_JOBS}")
fi
if [ ! -z "${BUILD_ARGS}" ]; then
  CMAKE_SETTINGS+=("-DESMX_BUILD_ARGS=${BUILD_ARGS}")
fi

# make settings
BUILD_SETTINGS=("")
if [ "${VERBOSE}" = true ]; then
  BUILD_SETTINGS+=("-v")
fi
if [ ! -z "${BUILD_JOBS}" ]; then
  BUILD_SETTINGS+=("-j ${BUILD_JOBS}")
fi

# test settings
TEST_SETTINGS=("")
if [ ! -z "${TEST_ARGS}" ]; then
  TEST_SETTINGS+=("${TEST_ARGS}")
fi

# install settings
INSTALL_SETTINGS=("")

# build and install
set +e
cmake -S${ESMF_ESMXDIR} -B${BUILD_DIR} ${CMAKE_SETTINGS[@]}
if [ "$?" !=  "0" ]; then
  echo "ESMX_Builder Failed: (cmake)"
  exit -1
fi
cmake --build ${BUILD_DIR} ${BUILD_SETTINGS[@]}
if [ "$?" !=  "0" ]; then
  echo "ESMX_Builder Failed: (cmake --build)"
  exit -2
fi
cmake --install ${BUILD_DIR} ${INSTALL_SETTINGS[@]}
if [ "$?" !=  "0" ]; then
  echo "ESMX_Builder Failed: (cmake --install)"
  exit -3
fi
if [ "${TEST}" = true ]; then
  (cd ${BUILD_DIR}/Driver; ctest ${TEST_SETTINGS[@]})
  if [ "$?" !=  "0" ]; then
    echo "ESMX_Builder Failed: (ctest)"
    LASTTEST="${BUILD_DIR}/Driver/Testing/Temporary/LastTest.log"
    if [ -f "${LASTTEST}" ]; then
      exit $(grep -c '^Test Failed\.$' ${LASTTEST})
    else
      exit -4
    fi
  fi
fi
