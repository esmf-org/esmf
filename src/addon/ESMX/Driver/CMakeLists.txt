# Where to look for Find<Package>.cmake files
list(APPEND CMAKE_MODULE_PATH ${CMAKE_CURRENT_LIST_DIR}/cmake)

# Find ESMF
if (NOT ESMF_FOUND)
  find_package(ESMF 8.4.0 MODULE REQUIRED)
elseif(ESMF_VERSION VERSION_LESS 8.4.0)
  message(FATAL_ERROR "ESMF version required is at least \"8.4.0\" (using ${ESMF_VERSION})")
endif()

# Set compilers per ESMFMKFILE
set(CMAKE_CXX_COMPILER ${ESMF_CXXCOMPILER})
set(CMAKE_Fortran_COMPILER ${ESMF_F90COMPILER})
set(CMAKE_CXX_COMPILER ${ESMF_CXXCOMPILER} PARENT_SCOPE)
set(CMAKE_Fortran_COMPILER ${ESMF_F90COMPILER} PARENT_SCOPE)

# Specify driver dependency
add_library(esmx_driver ESMX_Driver.F90)
target_include_directories(esmx_driver PUBLIC ${CMAKE_CURRENT_BINARY_DIR})
target_link_libraries(esmx_driver PUBLIC ESMF)

# Generate comp*.* files from esmxBuild.yaml
find_package(PythonInterp 3.5 REQUIRED)
execute_process(COMMAND ${PYTHON_EXECUTABLE}
  ${CMAKE_CURRENT_LIST_DIR}/esmxGen.py --ifile ${ESMX_BUILD_FILE} --odir ${CMAKE_CURRENT_BINARY_DIR}
  RESULT_VARIABLE ret)
if(ret EQUAL "1")
  message( FATAL_ERROR "esmxGen.py failed processing esmxBuild.yaml")
endif()

# Include compList.txt which in turn includes component libraries
include(${CMAKE_CURRENT_BINARY_DIR}/compList.txt)
