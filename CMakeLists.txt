cmake_minimum_required(VERSION 3.18)

# Get the version from the VERSION file.
file(STRINGS "VERSION" pVersion)

project(
  obsproc
  VERSION ${pVersion}
  LANGUAGES Fortran)

list(APPEND CMAKE_MODULE_PATH ${CMAKE_SOURCE_DIR}/cmake)

include(GNUInstallDirs)

# Build type.
if(NOT CMAKE_BUILD_TYPE MATCHES "^(Debug|Release|RelWithDebInfo|MinSizeRel)$")
  message(STATUS "Setting build type to 'Release' as none was specified.")
  set(CMAKE_BUILD_TYPE
      "Release"
      CACHE STRING "Choose the type of build." FORCE)
  set_property(CACHE CMAKE_BUILD_TYPE PROPERTY STRINGS "Debug" "Release"
                                               "MinSizeRel" "RelWithDebInfo")
endif()

# Compiler check.
if(NOT CMAKE_Fortran_COMPILER_ID MATCHES "^(Intel)$")
  message(WARNING "Compiler not officially supported: ${CMAKE_Fortran_COMPILER_ID}")
endif()

# Build and install code.
add_subdirectory(sorc)

# Install scripts.
add_subdirectory(scripts)

# Install utility scripts.
add_subdirectory(ush)

# Install fix files.
add_subdirectory(fix)

# Install module file for this package.
add_subdirectory(modulefiles)
