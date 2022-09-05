cmake_minimum_required(VERSION 3.5.2)
enable_language(Fortran)

# Where to look for Find<Package>.cmake files
list(APPEND CMAKE_MODULE_PATH ${CMAKE_CURRENT_LIST_DIR}/cmake)

# Find ESMF
find_package(ESMF 8.4.0 REQUIRED)

# Set compilers per ESMFMKFILE
set(CMAKE_CXX_COMPILER ${ESMF_CXXCOMPILER})
set(CMAKE_Fortran_COMPILER ${ESMF_F90COMPILER})

# Specific project settings
project(ESMX VERSION 0.1.0)
add_executable(esmx esmx.F90)
target_link_libraries(esmx PRIVATE ESMF)
install(TARGETS esmx)
target_include_directories(esmx PUBLIC ${PROJECT_BINARY_DIR})

# Generate 
find_package(Python 3 COMPONENTS Interpreter REQUIRED)
execute_process(COMMAND ${Python_EXECUTABLE}
  ${CMAKE_CURRENT_LIST_DIR}/esmxGen.py --ifile ./esmxBuild.yaml --odir ${CMAKE_CURRENT_BINARY_DIR}
  RESULT_VARIABLE ret)
if(ret EQUAL "1")
  message( FATAL_ERROR "esmxGen.py failed processing esmxBuild.yaml")
endif()

include(${CMAKE_CURRENT_BINARY_DIR}/compList.txt)
