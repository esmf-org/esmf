cmake_minimum_required(VERSION 3.5.2)
enable_language(Fortran)

# Add ESMX driver
add_subdirectory(Driver)

# Specific project settings
project(ESMX VERSION 0.1.0)

if(CMAKE_Fortran_COMPILER_ID MATCHES "GNU")
  set(CMAKE_Fortran_FLAGS_RELEASE "-O2")
  set(CMAKE_Fortran_FLAGS_RELWITHDEBINFO "-g -fbacktrace -02")
  set(CMAKE_Fortran_FLAGS_DEBUG "-g -fbacktrace -O0 -fcheck=all -ffpe-trap=invalid,zero,overflow,underflow")
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "Intel")
  set(CMAKE_Fortran_FLAGS_RELEASE "-O2")
  set(CMAKE_Fortran_FLAGS_RELWITHDEBINFO "-g -traceback -O2")
  set(CMAKE_Fortran_FLAGS_DEBUG "-g -traceback -O0 -check all -fpe0 -ftrapuv -init=snan,arrays")
else()
  message(WARNING "${CMAKE_Fortran_COMPILER_ID} Fortran compiler will be used with default options")
endif()

# define ESMX_EXE_NAME
if(NOT DEFINED ESMX_EXE_NAME)
  set(ESMX_EXE_NAME "esmx_app")
endif()

# Create ESMX executable
add_executable(${ESMX_EXE_NAME} ESMX_App.F90)
target_include_directories(${ESMX_EXE_NAME} PUBLIC ${PROJECT_BINARY_DIR})
target_link_libraries(${ESMX_EXE_NAME} PUBLIC esmx_driver)

# Install executable
install(
  TARGETS ${ESMX_EXE_NAME}
  EXPORT esmx
  RUNTIME DESTINATION bin
  LIBRARY DESTINATION lib
  ARCHIVE DESTINATION lib)
