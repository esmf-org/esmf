cmake_minimum_required(VERSION 3.21)
# Where to look for Find<Package>.cmake files
list(APPEND CMAKE_MODULE_PATH ${CMAKE_CURRENT_LIST_DIR}/cmake)

# Find ESMF
find_package(ESMF 8.4.0 MODULE REQUIRED)

# Set compilers per ESMFMKFILE
find_program(CMAKE_CXX_COMPILER ${ESMF_CXXCOMPILER})
find_program(CMAKE_Fortran_COMPILER ${ESMF_F90COMPILER})
set(CMAKE_CXX_COMPILER ${ESMF_CXXCOMPILER} PARENT_SCOPE)
set(CMAKE_Fortran_COMPILER ${ESMF_F90COMPILER} PARENT_SCOPE)

if(CMAKE_Fortran_COMPILER_ID MATCHES "GNU")
  set(CMAKE_Fortran_FLAGS_RELEASE "-O2" PARENT_SCOPE)
  set(CMAKE_Fortran_FLAGS_RELWITHDEBINFO "-g -fbacktrace -02" PARENT_SCOPE)
  set(CMAKE_Fortran_FLAGS_DEBUG "-g -fbacktrace -O0 -fcheck=all -ffpe-trap=invalid,zero,overflow,underflow" PARENT_SCOPE)
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "Intel")
  set(CMAKE_Fortran_FLAGS_RELEASE "-O2" PARENT_SCOPE)
  set(CMAKE_Fortran_FLAGS_RELWITHDEBINFO "-g -traceback -O2" PARENT_SCOPE)
  set(CMAKE_Fortran_FLAGS_DEBUG "-g -traceback -O0 -check all -fpe0 -ftrapuv -init=snan,arrays" PARENT_SCOPE)
endif()

# Specify driver dependency
add_library(esmx_driver ESMX_Driver.F90)
target_include_directories(esmx_driver PUBLIC ${CMAKE_CURRENT_BINARY_DIR})
target_link_libraries(esmx_driver PUBLIC ESMF)

if(DEFINED DISABLE_ESMX_COMPS)
  set(DISABLE_ESMX_COMPS --disable_esmx_comps ${DISABLE_ESMX_COMPS})
endif()
unset(DISABLE_ESMX_COMPS CACHE)

# Generate comp*.* files from esmxBuild.yaml
find_package(Python 3.5 REQUIRED COMPONENTS Interpreter)
execute_process(COMMAND ${Python_EXECUTABLE}
  ${CMAKE_CURRENT_LIST_DIR}/esmxGen.py --ifile ${ESMX_BUILD_FILE} --odir ${CMAKE_CURRENT_BINARY_DIR} ${DISABLE_ESMX_COMPS}
  RESULT_VARIABLE ret)
if(NOT ret EQUAL "0")
  message(FATAL_ERROR "esmxGen.py failed processing esmxBuild.yaml")
endif()

# Include compList.txt which in turn includes component libraries
include(${CMAKE_CURRENT_BINARY_DIR}/compList.txt)
include(FetchContent)

if(TEST)
  set(CMAKE_TEST TRUE)
  enable_testing()
  if(NOT DEFINED TEST_DIR)
    set(CMAKE_TEST_DIR "tests")
  else()
    set(CMAKE_TEST_DIR ${TEST_DIR})
  endif()
  string(REPLACE "\"" "" ESMF_INTERNAL_MPIRUN ${ESMF_INTERNAL_MPIRUN})
  string(STRIP ${ESMF_INTERNAL_MPIRUN} ESMF_INTERNAL_MPIRUN)
  find_program(CMAKE_TEST_EXE
    NAMES ${ESMF_INTERNAL_MPIRUN}
    NO_CACHE
  )
  file(MAKE_DIRECTORY ${CMAKE_TEST_DIR})
  set(CMAKE_TEST_TASKS -np 4)
endif()
unset(TEST CACHE)
unset(TEST_DIR CACHE)

if(VERBOSE)
  set(CMAKE_VERBOSE "-v")
else()
  set(CMAKE_VERBOSE "")
endif()
unset(VERBOSE CACHE)

if(DEFINED JOBS)
  set(MAKE_JOBS "--jobs=${JOBS}")
  set(CMAKE_JOBS "-j ${JOBS}")
else()
  set(MAKE_JOBS "")
  set(CMAKE_JOBS "")
endif()
unset(JOBS CACHE)

# Add components using build type
foreach(CMP IN ITEMS ${COMPS})
  # get component options
  set(CMP_BUILD_TYPE     "${${CMP}-BUILD_TYPE}")
  string(TOLOWER         "${CMP_BUILD_TYPE}" CMP_BUILD_TYPE)
  set(CMP_SOURCE_DIR     "${${CMP}-SOURCE_DIR}")
  set(CMP_GIT_REPOSITORY "${${CMP}-GIT_REPOSITORY}")
  set(CMP_GIT_TAG        "${${CMP}-GIT_TAG}")
  set(CMP_GIT_DIR        "${${CMP}-GIT_DIR}")
  set(CMP_CMAKE_CONFIG   "${${CMP}-CMAKE_CONFIG}")
  set(CMP_INSTALL_PREFIX "${${CMP}-INSTALL_PREFIX}")
  set(CMP_LIBRARY_DIR    "${${CMP}-LIBRARY_DIR}")
  set(CMP_CONFIG_DIR     "${${CMP}-CONFIG_DIR}")
  set(CMP_INCLUDE_DIR    "${${CMP}-INCLUDE_DIR}")
  set(CMP_FORT_MODULE    "${${CMP}-FORT_MODULE}")
  set(CMP_LIBRARIES      "${${CMP}-LIBRARIES}")
  string(REPLACE "," ";" "${${CMP}-LIBRARIES}" CMP_LIBRARIES)
  set(CMP_LINK_LIBRARIES "${${CMP}-LINK_LIBRARIES}")
  string(REPLACE "," ";" "${${CMP}-LINK_LIBRARIES}" CMP_LINK_LIBRARIES)
  set(CMP_LINK_PATHS     "${${CMP}-LINK_PATHS}")
  string(REPLACE "," ";" "${${CMP}-LINK_PATHS}" CMP_LINK_PATHS)
  set(CMP_BUILD_SCRIPT   "${${CMP}-BUILD_SCRIPT}")
  set(CMP_BUILD_ARGS     "${${CMP}-BUILD_ARGS}")

  # download git repository
  if(NOT CMP_GIT_REPOSITORY STREQUAL "")
    message(STATUS "Fetching content ... ${CMP}")
    if(EXISTS ${CMP_SOURCE_DIR})
      message(FATAL_ERROR "Source directory already exists for ${CMP}")
    endif()
    if(CMP_GIT_TAG STREQUAL "")
      message(FATAL_ERROR "Git tag required for ${CMP}")
    endif()
    FetchContent_Declare(${CMP}
      GIT_REPOSITORY ${CMP_GIT_REPOSITORY}
      GIT_TAG ${CMP_GIT_TAG}
      SOURCE_DIR ${CMP_GIT_DIR}
    )
    FetchContent_Populate(${CMP})
  endif()

  # ESMX_Data component is built internally
  if("${CMP}" STREQUAL "ESMX_Data")
    set(CMP_BUILD_TYPE "none")
  endif()

  # automatically determine build type based on files
  if(${CMP_BUILD_TYPE} STREQUAL "auto")
    # find build configuration or libraries
    find_file(FND_CMAKE_LISTS
      NAMES CMakeLists.txt cmakelists.txt CMAKELISTS.txt
      PATHS ${CMP_SOURCE_DIR}
      NO_CACHE NO_DEFAULT_PATH
    )
    find_file(FND_MAKE_FILE
      NAMES Makefile makefile MAKEFILE
      PATHS ${CMP_SOURCE_DIR}
      NO_CACHE NO_DEFAULT_PATH
    )
    find_file(FND_BUILD_SCRIPT
      NAMES ${CMP_BUILD_SCRIPT}
      PATHS ${CMP_SOURCE_DIR}
      NO_CACHE NO_DEFAULT_PATH
    )
    find_file(FND_CMAKE_CONFIG
      NAMES ${CMP_CMAKE_CONFIG}
      HINTS ${CMP_INSTALL_PREFIX}
      PATH_SUFFIXES ${CMP_CONFIG_DIR} "." "cmake" "config"
      NO_CACHE NO_DEFAULT_PATH
    )
    set(FND_LIBRARIES TRUE)
    foreach(CMP_LIBRARY IN ITEMS ${CMP_LIBRARIES})
      if(NOT TARGET ${CMP_LIBRARY})
        find_library(FND_LIBRARY
          NAMES ${CMP_LIBRARY}
          HINTS ${CMP_INSTALL_PREFIX}
          PATH_SUFFIXES ${CMP_LIBRARY_DIR} "." "lib" "lib64"
          NO_CACHE NO_DEFAULT_PATH
        )
        if(NOT FND_LIBRARY)
          set(FND_LIBRARIES FALSE)
        endif()
        unset(FND_LIBRARY)
      endif()
    endforeach()

    # set build type
    if(FND_CMAKE_LISTS)
      set(CMP_BUILD_TYPE "cmake")
    elseif(FND_MAKE_FILE)
      set(CMP_BUILD_TYPE "make")
    elseif(FND_BUILD_SCRIPT)
      set(CMP_BUILD_TYPE "script")
    elseif(FND_CMAKE_CONFIG OR FND_LIBRARIES)
      set(CMP_BUILD_TYPE "none")
    else()
      message(FATAL_ERROR "Cannot find build configuration for ${CMP}")
    endif()
  endif()

  # build component with cmake
  if(${CMP_BUILD_TYPE} STREQUAL "cmake")
    message(STATUS "Building (CMAKE) - ${CMP}")
    find_file(FND_CMAKE_LISTS
      NAMES CMakeLists.txt cmakelists.txt CMAKELISTS.txt
      PATHS ${CMP_SOURCE_DIR}
      NO_CACHE NO_DEFAULT_PATH
    )
    if(NOT FND_CMAKE_LISTS)
      message(FATAL_ERROR "Could not find CMakeLists.txt"
        " in ${CMP_SOURCE_DIR}."
        " Check source_dir in build configuration."
      )
    endif()
    file(MAKE_DIRECTORY ${CMAKE_BINARY_DIR}/${CMP})
    execute_process(
      COMMAND ${CMAKE_COMMAND} -S${CMP_SOURCE_DIR} -B${CMAKE_BINARY_DIR}/${CMP}
        ${CMP_BUILD_ARGS} -DCMAKE_INSTALL_PREFIX=${CMP_INSTALL_PREFIX}
      RESULT_VARIABLE ret
    )
    if(NOT ret EQUAL "0")
      message(FATAL_ERROR "cmake generate failed for ${CMP}")
    endif()
    execute_process(
      COMMAND ${CMAKE_COMMAND} --build ${CMAKE_BINARY_DIR}/${CMP} ${CMAKE_VERBOSE} ${CMAKE_JOBS}
      RESULT_VARIABLE ret
    )
    if(NOT ret EQUAL "0")
      message(FATAL_ERROR "cmake build failed for ${CMP}")
    endif()
    execute_process(
      COMMAND ${CMAKE_COMMAND} --install ${CMAKE_BINARY_DIR}/${CMP}
      RESULT_VARIABLE ret
    )
    if(NOT ret EQUAL "0")
      message(FATAL_ERROR "cmake install failed for ${CMP}")
    endif()
  # build component with make
  elseif(${CMP_BUILD_TYPE} STREQUAL "make")
    message(STATUS "Building (MAKE) - ${CMP}")
    find_file(FND_MAKE_FILE
      NAMES Makefile makefile MAKEFILE
      PATHS ${CMP_SOURCE_DIR}
      NO_CACHE NO_DEFAULT_PATH
    )
    if(NOT FND_MAKE_FILE)
      message(FATAL_ERROR "Could not find Makefile"
        " in ${CMP_SOURCE_DIR}."
        " Check source_dir in build configuration."
      )
    endif()
    execute_process(
      COMMAND ${CMAKE_MAKE_PROGRAM} ${MAKE_JOBS} ${CMP_BUILD_ARGS}
      WORKING_DIRECTORY ${CMP_SOURCE_DIR}
      RESULT_VARIABLE ret
    )
    if(NOT ret EQUAL "0")
      message(FATAL_ERROR "make build failed for ${CMP}")
    endif()
  # build component with script
  elseif(${CMP_BUILD_TYPE} STREQUAL "script")
    message(STATUS "Building (SCRIPT) - ${CMP}")
    find_file(FND_BUILD_SCRIPT
      NAMES ${CMP_BUILD_SCRIPT}
      PATHS ${CMP_SOURCE_DIR}
      NO_CACHE NO_DEFAULT_PATH
    )
    if(NOT FND_BUILD_SCRIPT)
      message(FATAL_ERROR "Could not find ${CMP_BUILD_SCRIPT}"
        " in ${CMP_SOURCE_DIR}."
        " Check source_dir in build configuration."
      )
    endif()
    execute_process(
      COMMAND ${FND_BUILD_SCRIPT} ${CMP_BUILD_ARGS}
      WORKING_DIRECTORY ${CMP_SOURCE_DIR}
      RESULT_VARIABLE ret
    )
    if(NOT ret EQUAL "0")
      message(FATAL_ERROR "script build failed for ${CMP}")
    endif()
  # skip build step
  elseif(${CMP_BUILD_TYPE} STREQUAL "none")
    # No Op
  else()
    message(FATAL_ERROR "Invalid BUILD_TYPE: ${CMP_BUILD_TYPE} for ${CMP}")
  endif()

  # include modules and link libraries
  if("${CMP}" STREQUAL "ESMX_Data")
    add_library(esmx_data ${ESMF_ESMXDIR}/Comps/ESMX_Data.F90)
    target_link_libraries(esmx_data PUBLIC ESMF)
    add_dependencies(esmx_driver esmx_data)
    target_link_libraries(esmx_driver PUBLIC esmx_data)
    if(CMAKE_TEST)
      file(COPY ${ESMF_ESMXDIR}/Tests/esmx-data DESTINATION ${CMAKE_TEST_DIR})
      file(CREATE_LINK ${CMAKE_BINARY_DIR}/esmx
        ${CMAKE_TEST_DIR}/esmx-data/esmx
        SYMBOLIC
      )
      add_test(NAME comp-esmx-data
        COMMAND ${CMAKE_TEST_EXE} ${CMAKE_TEST_TASKS} ./esmx
        WORKING_DIRECTORY ${CMAKE_TEST_DIR}/esmx-data)
    endif()
  else()
    find_file(FND_CMAKE_CONFIG
      NAMES ${CMP_CMAKE_CONFIG}
      HINTS ${CMP_INSTALL_PREFIX} ${CMAKE_BINARY_DIR}/${CMP}
      PATH_SUFFIXES ${CMP_CONFIG_DIR} "." "cmake" "config"
      NO_CACHE NO_DEFAULT_PATH
    )
    if(FND_CMAKE_CONFIG)
      include(${FND_CMAKE_CONFIG})
    endif()
    find_path(FND_FORT_MODULE
      NAMES ${CMP_FORT_MODULE}
      HINTS ${CMP_INSTALL_PREFIX} ${CMAKE_BINARY_DIR}/${CMP}
      PATH_SUFFIXES ${CMP_INCLUDE_DIR} "." "include" "mod"
      NO_CACHE NO_DEFAULT_PATH
    )
    if(FND_FORT_MODULE)
      target_include_directories(esmx_driver PUBLIC ${FND_FORT_MODULE})
    elseif(NOT FND_CMAKE_CONFIG)
      message(FATAL_ERROR "Cannot find fort_module ${CMP_FORT_MODULE}")
    endif()
    foreach(CMP_LIBRARY IN ITEMS ${CMP_LIBRARIES})
      if(TARGET ${CMP_LIBRARY})
        target_link_libraries(esmx_driver PUBLIC ${CMP_LIBRARY})
      else()
        find_library(FND_LIBRARY
          NAMES ${CMP_LIBRARY}
          HINTS ${CMP_INSTALL_PREFIX} ${CMAKE_BINARY_DIR}/${CMP}
          PATH_SUFFIXES ${CMP_LIBRARY_DIR} "." "lib" "lib64"
          NO_CACHE NO_DEFAULT_PATH
        )
        if(NOT FND_LIBRARY)
          message(FATAL_ERROR "Cannot find libraries ${CMP_LIBRARY}")
        endif()
        target_link_libraries(esmx_driver PUBLIC ${FND_LIBRARY})
        unset(FND_LIBRARY)
      endif()
    endforeach()

    # change environment variable to path in variable
    set(TMP_LINK_PATHS "")
    foreach(CMP_LINK_PATH IN ITEMS ${CMP_LINK_PATHS})
      if(DEFINED ENV{${CMP_LINK_PATH}})
        set(CMP_LINK_PATH $ENV{${CMP_LINK_PATH}})
      endif()
      list(APPEND TMP_LINK_PATHS ${CMP_LINK_PATH})
    endforeach()
    set(CMP_LINK_PATHS ${TMP_LINK_PATHS})
    unset(TMP_LINK_PATHS)

    # link external libraries
    foreach(CMP_LINK_LIBRARY IN ITEMS ${CMP_LINK_LIBRARIES})
      if(TARGET ${CMP_LINK_LIBRARY})
        target_link_libraries(esmx_driver PUBLIC ${CMP_LINK_LIBRARY})
      else()
        find_library(FND_LINK_LIBRARY
          NAMES ${CMP_LINK_LIBRARY}
          HINTS ${CMP_LINK_PATHS}
          PATH_SUFFIXES "." "lib" "lib64"
          NO_CACHE
        )
        if(NOT FND_LINK_LIBRARY)
          message(FATAL_ERROR "Cannot find link_libraries ${CMP_LINK_LIBRARY}")
        endif()
        target_link_libraries(esmx_driver PUBLIC ${FND_LINK_LIBRARY})
        unset(FND_LINK_LIBRARY)
      endif()
    endforeach()

  endif()

  # unset variables used for component
  unset(CMP_BUILD_TYPE)
  unset(CMP_SOURCE_DIR)
  unset(CMP_GIT_REPOSITORY)
  unset(CMP_GIT_TAG)
  unset(CMP_GIT_DIR)
  unset(CMP_CMAKE_CONFIG)
  unset(CMP_INSTALL_PREFIX)
  unset(CMP_LIBRARY_DIR)
  unset(CMP_CONFIG_DIR)
  unset(CMP_INCLUDE_DIR)
  unset(CMP_FORT_MODULE)
  unset(CMP_LIBRARIES)
  unset(CMP_LINK_LIBRARIES)
  unset(CMP_LINK_PATHS)
  unset(CMP_BUILD_ARGS)
  unset(CMP_BUILD_SCRIPT)
  unset(FND_CMAKE_LISTS)
  unset(FND_MAKE_FILE)
  unset(FND_BUILD_SCRIPT)
  unset(FND_CMAKE_CONFIG)
  unset(FND_FORT_MODULE)
  unset(FND_LIBRARIES)
endforeach()
