cmake_minimum_required(VERSION 3.5.2)
enable_language(Fortran)

if(NOT DEFINED ESMX_BUILD_FILE)
  set(ESMX_BUILD_FILE "esmxBuild.yaml")
endif()

# Add ESMX driver
add_subdirectory(Driver)

# Specific project settings
project(ESMX VERSION 0.1.0)

# Create ESMX executable
add_executable(esmx esmx.F90)
target_include_directories(esmx PUBLIC ${PROJECT_BINARY_DIR})
target_link_libraries(esmx PUBLIC esmx_driver)

# Install executable
install(
  TARGETS esmx
  EXPORT esmx
  RUNTIME DESTINATION bin
  LIBRARY DESTINATION lib
  ARCHIVE DESTINATION lib)
