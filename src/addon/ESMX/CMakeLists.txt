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
    set(CMAKE_Fortran_FLAGS_DEBUG "-g -traceback -O0 -check bounds,pointers,stack -fpe0 -ftrapuv -init=snan,arrays" CACHE STRING "" FORCE)

  elseif(CMAKE_Fortran_COMPILER_ID STREQUAL "NAG")
    set(CMAKE_Fortran_FLAGS_RELWITHDEBINFO "-g -gline" CACHE STRING "" FORCE)
    set(CMAKE_Fortran_FLAGS_DEBUG "-g -gline -C=array -C=bits -C=dangling -C=pointer -C=recursion" CACHE STRING "" FORCE)

  elseif(CMAKE_Fortran_COMPILER_ID STREQUAL "NVHPC")
    set(CMAKE_Fortran_FLAGS_RELWITHDEBINFO "-g -traceback -O2 -fast" CACHE STRING "" FORCE)
    set(CMAKE_Fortran_FLAGS_DEBUG "-g -traceback -O0" CACHE STRING "" FORCE)
  endif()
endif()

# Sanitizer options

include(CheckFortranCompilerFlag)

set(USE_SANITIZERS "" CACHE STRING "List of sanitizers for Fortran: Address, Leak, Undefined, Thread, Memory")

if(USE_SANITIZERS)
  # End-to-end compile AND link check for general sanitizer support
  set(CMAKE_REQUIRED_LINK_OPTIONS "-fsanitize=address")
  check_fortran_compiler_flag("-fsanitize=address" Fortran_SUPPORTS_SANITIZERS)
  unset(CMAKE_REQUIRED_LINK_OPTIONS)

  if(NOT Fortran_SUPPORTS_SANITIZERS)
    message(WARNING "Sanitizers requested (-DUSE_SANITIZERS='${USE_SANITIZERS}'), but Fortran compiler '${CMAKE_Fortran_COMPILER_ID}' failed sanitizer link checks. Sanitizers will be ignored.")
    set(USE_SANITIZERS "" CACHE STRING "List of sanitizers" FORCE)

  else()
    # 3. Normalize options to uppercase
    set(SANITIZERS_UPPER "")
    foreach(ITEM IN LISTS USE_SANITIZERS)
      string(TOUPPER "${ITEM}" ITEM_UPPER)
      list(APPEND SANITIZERS_UPPER "${ITEM_UPPER}")
    endforeach()

    # 4. Enforce mutual exclusion rules
    if("THREAD" IN_LIST SANITIZERS_UPPER)
      if("ADDRESS" IN_LIST SANITIZERS_UPPER OR "LEAK" IN_LIST SANITIZERS_UPPER OR "MEMORY" IN_LIST SANITIZERS_UPPER)
        message(FATAL_ERROR "ThreadSanitizer (TSan) cannot be combined with Address, Leak, or Memory sanitizers in Fortran.")
      endif()
    endif()

    if("MEMORY" IN_LIST SANITIZERS_UPPER)
      if("ADDRESS" IN_LIST SANITIZERS_UPPER OR "LEAK" IN_LIST SANITIZERS_UPPER)
        message(FATAL_ERROR "MemorySanitizer (MSan) cannot be combined with Address or Leak sanitizers in Fortran.")
      endif()
    endif()

    # 5. Assemble flags based on validated selections
    set(SANITIZER_FLAGS "")

    foreach(SANITIZER IN LISTS SANITIZERS_UPPER)
      if(SANITIZER STREQUAL "ADDRESS")
        list(APPEND SANITIZER_FLAGS "address")
      elseif(SANITIZER STREQUAL "LEAK")
        list(APPEND SANITIZER_FLAGS "leak")
      elseif(SANITIZER STREQUAL "UNDEFINED")
        list(APPEND SANITIZER_FLAGS "undefined")
      elseif(SANITIZER STREQUAL "THREAD")
        list(APPEND SANITIZER_FLAGS "thread")
      elseif(SANITIZER STREQUAL "MEMORY")
        if(CMAKE_Fortran_COMPILER_ID MATCHES "LLVMFlang|Flang|Clang")
          list(APPEND SANITIZER_FLAGS "memory")
          add_compile_options($<$<COMPILE_LANGUAGE:Fortran>:-fsanitize-memory-track-origins>)
        else()
          message(FATAL_ERROR "MemorySanitizer (MSan) for Fortran requires LLVM Flang.")
        endif()
      else()
        message(FATAL_ERROR "Unknown Fortran sanitizer option: '${SANITIZER}'")
      endif()
    endforeach()

    # 6. Apply flags strictly to Fortran targets and linker
    if(SANITIZER_FLAGS)
      list(JOIN SANITIZER_FLAGS "," SANITIZER_ARG)

      add_compile_options(
        $<$<COMPILE_LANGUAGE:Fortran>:-fsanitize=${SANITIZER_ARG}>
        $<$<COMPILE_LANGUAGE:Fortran>:-fno-omit-frame-pointer>
      )

      add_link_options(-fsanitize=${SANITIZER_ARG})
    endif()

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
