cmake_minimum_required(VERSION 3.22)

project(ESMX_Data
        VERSION 9.0.0
        LANGUAGES Fortran)

find_package(ESMF 9.0.0 REQUIRED)

include(GNUInstallDirs)

# custom compiler options
if(CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")
  set(CMAKE_Fortran_FLAGS -ffree-line-length-none)
  set(CMAKE_Fortran_FLAGS_RELWITHDEBINFO "-g -fbacktrace -O2")
  set(CMAKE_Fortran_FLAGS_DEBUG "-g -fbacktrace -O0 -fcheck=all -ffpe-trap=invalid,zero,overflow,underflow")
elseif(CMAKE_Fortran_COMPILER_ID STREQUAL "Cray")
  set(CMAKE_Fortran_FLAGS_RELWITHDEBINFO "-g -O2")
  set(CMAKE_Fortran_FLAGS_DEBUG "-g")
elseif(CMAKE_Fortran_COMPILER_ID STREQUAL "Flang")
  set(CMAKE_Fortran_FLAGS_RELWITHDEBINFO "-g -O2")
  set(CMAKE_Fortran_FLAGS_DEBUG "-g")
elseif(CMAKE_Fortran_COMPILER_ID STREQUAL "LLVMFlang")
  set(CMAKE_Fortran_FLAGS_RELWITHDEBINFO "-g -O2")
  set(CMAKE_Fortran_FLAGS_DEBUG "-g")
elseif(CMAKE_Fortran_COMPILER_ID STREQUAL "Intel")
  set(CMAKE_Fortran_FLAGS_RELWITHDEBINFO "-g -traceback -O2")
  set(CMAKE_Fortran_FLAGS_DEBUG "-g -traceback -O0 -check all -fpe0 -ftrapuv -init=snan,arrays")
elseif(CMAKE_Fortran_COMPILER_ID STREQUAL "IntelLLVM")
  set(CMAKE_Fortran_FLAGS_RELWITHDEBINFO "-g -traceback -O2")
  set(CMAKE_Fortran_FLAGS_DEBUG "-g -traceback -O0 -check all -fpe0 -ftrapuv -init=snan,arrays")
elseif(CMAKE_Fortran_COMPILER_ID STREQUAL "NAG")
  set(CMAKE_Fortran_FLAGS_RELWITHDEBINFO "-g -gline")
  set(CMAKE_Fortran_FLAGS_DEBUG "-g -gline -C=all")
elseif(CMAKE_Fortran_COMPILER_ID STREQUAL "NVHPC")
  set(CMAKE_Fortran_FLAGS_RELWITHDEBINFO "-g -traceback -O2 -fast")
  set(CMAKE_Fortran_FLAGS_DEBUG "-g -traceback -O0")
endif()

add_library(ESMX_Data ESMX_Data.F90 ESMX_DataProcess.F90)
target_include_directories(ESMX_Data
  INTERFACE $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/${CMAKE_INSTALL_INCLUDEDIR}>
  $<INSTALL_INTERFACE:${CMAKE_INSTALL_INCLUDEDIR}>
)
set_target_properties(ESMX_Data
  PROPERTIES Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/${CMAKE_INSTALL_INCLUDEDIR}
)
target_link_libraries(ESMX_Data ESMF)
