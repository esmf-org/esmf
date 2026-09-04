cmake_minimum_required(VERSION 3.22)

# Project
project(ESMX
        VERSION 9.0.0
        LANGUAGES Fortran CXX C
        )

if(PROJECT_IS_TOP_LEVEL)
  # Find ESMF
  find_package(ESMF 9.0.0 REQUIRED)

  # Set global defaults
  if(CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")
    set(CMAKE_Fortran_FLAGS_RELWITHDEBINFO "-g -fbacktrace -O2" CACHE STRING "" FORCE)
    set(CMAKE_Fortran_FLAGS_DEBUG "-g -fbacktrace -O0 -fcheck=all -ffpe-trap=invalid,zero,overflow,underflow" CACHE STRING "" FORCE)

  elseif(CMAKE_Fortran_COMPILER_ID STREQUAL "Cray")
    set(CMAKE_Fortran_FLAGS_RELWITHDEBINFO "-g -O2" CACHE STRING "" FORCE)
    set(CMAKE_Fortran_FLAGS_DEBUG "-g" CACHE STRING "" FORCE)

  elseif(CMAKE_Fortran_COMPILER_ID STREQUAL "Flang")
    set(CMAKE_Fortran_FLAGS_RELWITHDEBINFO "-g -O2" CACHE STRING "" FORCE)
    set(CMAKE_Fortran_FLAGS_DEBUG "-g" CACHE STRING "" FORCE)

  elseif(CMAKE_Fortran_COMPILER_ID STREQUAL "LLVMFlang")
    set(CMAKE_Fortran_FLAGS_RELWITHDEBINFO "-g -O2" CACHE STRING "" FORCE)
    set(CMAKE_Fortran_FLAGS_DEBUG "-g" CACHE STRING "" FORCE)

  elseif(CMAKE_Fortran_COMPILER_ID STREQUAL "Intel")
    set(CMAKE_Fortran_FLAGS_RELWITHDEBINFO "-g -traceback -O2" CACHE STRING "" FORCE)
    set(CMAKE_Fortran_FLAGS_DEBUG "-g -traceback -O0 -check all -fpe0 -ftrapuv -init=snan,arrays" CACHE STRING "" FORCE)

  elseif(CMAKE_Fortran_COMPILER_ID STREQUAL "IntelLLVM")
    set(CMAKE_Fortran_FLAGS_RELWITHDEBINFO "-g -traceback -O2" CACHE STRING "" FORCE)
    set(CMAKE_Fortran_FLAGS_DEBUG "-g -traceback -O0 -check all -fpe0 -ftrapuv -init=snan,arrays" CACHE STRING "" FORCE)
    # Fix for ifx 2023.x: Pass -check all to the linker in Debug mode to pull in MSan runtime symbols
    add_link_options($<$<AND:$<CONFIG:Debug>,$<STREQUAL:$<TARGET_PROPERTY:LINKER_LANGUAGE>,Fortran>>:-check all>)

  elseif(CMAKE_Fortran_COMPILER_ID STREQUAL "NAG")
    set(CMAKE_Fortran_FLAGS_RELWITHDEBINFO "-g -gline" CACHE STRING "" FORCE)
    set(CMAKE_Fortran_FLAGS_DEBUG "-g -gline -C=all" CACHE STRING "" FORCE)
    # Ensure NAG runtime check and line-tracing libraries link properly across targets
    add_link_options(
      $<$<AND:$<CONFIG:RelWithDebInfo>,$<STREQUAL:$<TARGET_PROPERTY:LINKER_LANGUAGE>,Fortran>>:-gline>
      $<$<AND:$<CONFIG:Debug>,$<STREQUAL:$<TARGET_PROPERTY:LINKER_LANGUAGE>,Fortran>>:-gline;-C=all>
    )

  elseif(CMAKE_Fortran_COMPILER_ID STREQUAL "NVHPC")
    set(CMAKE_Fortran_FLAGS_RELWITHDEBINFO "-g -traceback -O2 -fast" CACHE STRING "" FORCE)
    set(CMAKE_Fortran_FLAGS_DEBUG "-g -traceback -O0" CACHE STRING "" FORCE)
  endif()
endif()

# Define standard installation directories
include(GNUInstallDirs)

# Add ESMX driver
add_subdirectory(Driver)

# Define ESMX_EXE_NAME defaults if not supplied by parent scope/CLI
if(NOT DEFINED ESMX_EXE_NAME)
  set(ESMX_EXE_NAME "esmx_app" CACHE STRING "Name of the ESMX executable")
endif()

# Create ESMX executable
add_executable(${ESMX_EXE_NAME} ESMX_App.F90)
target_include_directories(${ESMX_EXE_NAME} PUBLIC ${CMAKE_CURRENT_BINARY_DIR})
target_link_libraries(${ESMX_EXE_NAME} PRIVATE esmx_driver)
set_target_properties(${ESMX_EXE_NAME} PROPERTIES
  INSTALL_RPATH     "${CMAKE_INSTALL_PREFIX}/${CMAKE_INSTALL_LIBDIR};${CMAKE_INSTALL_PREFIX}/lib"
  LINKER_LANGUAGE   Fortran
)

# Install executable
install(TARGETS ${ESMX_EXE_NAME} DESTINATION ${CMAKE_INSTALL_BINDIR})
