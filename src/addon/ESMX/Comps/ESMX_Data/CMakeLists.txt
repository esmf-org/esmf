cmake_minimum_required(VERSION 3.22)

list(APPEND CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/cmake)
find_package(ESMF 8.6.0 MODULE REQUIRED)

if(NOT DEFINED CMAKE_Fortran_COMPILER)
  set(CMAKE_Fortran_COMPILER "${ESMF_F90COMPILER}")
endif()
if(NOT DEFINED CMAKE_Fortran_FLAGS)
  set(CMAKE_Fortran_FLAGS "${ESMF_F90COMPILEOPTS}")
endif()

project(ESMX_Data
        VERSION 1.0
        LANGUAGES Fortran)

if(CMAKE_Fortran_COMPILER_ID MATCHES "GNU")
  set(CMAKE_Fortran_FLAGS -ffree-line-length-none)
  set(CMAKE_Fortran_FLAGS_RELEASE "-O2")
  set(CMAKE_Fortran_FLAGS_RELWITHDEBINFO "-g -fbacktrace -O2")
  set(CMAKE_Fortran_FLAGS_DEBUG "-g -fbacktrace -O0 -fcheck=all -ffpe-trap=invalid,zero,overflow,underflow")
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "Intel")
  set(CMAKE_Fortran_FLAGS_RELEASE "-O2")
  set(CMAKE_Fortran_FLAGS_RELWITHDEBINFO "-g -traceback -O2")
  set(CMAKE_Fortran_FLAGS_DEBUG "-g -traceback -O0 -check all -fpe0 -ftrapuv -init=snan,arrays")
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "NVHPC")
  set(CMAKE_Fortran_FLAGS_RELEASE "-O2 -fast")
  set(CMAKE_Fortran_FLAGS_RELWITHDEBINFO "-g -traceback -O2 -fast")
  set(CMAKE_Fortran_FLAGS_DEBUG "-g -traceback -O0")
else()
  message(WARNING "${CMAKE_Fortran_COMPILER_ID} Fortran compiler will be used with default options")
endif()

add_library(ESMX_Data ESMX_Data.F90)
target_link_libraries(ESMX_Data ESMF)

install(TARGETS ESMX_Data
        EXPORT ESMX_Data
        LIBRARY DESTINATION lib
        ARCHIVE DESTINATION lib
        INCLUDES DESTINATION mod
)
install(DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/
        DESTINATION mod
        FILES_MATCHING
        PATTERN "*.mod"
        PATTERN "CMakeFiles" EXCLUDE
)
install(EXPORT ESMX_Data DESTINATION cmake)
