# Set sorc/ -wide compiler flags.
if(CMAKE_Fortran_COMPILER_ID MATCHES "^(Intel)$")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -g -traceback")
  set(CMAKE_Fortran_FLAGS_RELEASE "-O2")
  set(CMAKE_Fortran_FLAGS_DEBUG "-O0 -check -check noarg_temp_created -check nopointer -fp-stack-check -fstack-protector-all -fpe0 -debug -ftrapuv")
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "^(GNU)$")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -g -fbacktrace")
  set(CMAKE_Fortran_FLAGS_RELEASE "-O3")
  set(CMAKE_Fortran_FLAGS_DEBUG "-O0 -ggdb -fno-unsafe-math-optimizations -frounding-math -fsignaling-nans -ffpe-trap=invalid,zero,overflow -fbounds-check")
  if(${CMAKE_Fortran_COMPILER_VERSION} VERSION_GREATER_EQUAL 10)
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fallow-argument-mismatch -fallow-invalid-boz")
  endif()
endif()

# Find packages.
find_package(NetCDF REQUIRED COMPONENTS Fortran)
find_package(bacio REQUIRED)
find_package(w3emc REQUIRED)
find_package(sp REQUIRED)
find_package(sigio REQUIRED)
find_package(nemsio REQUIRED)
find_package(bufr REQUIRED)
find_package(MKL)

if(bacio_VERSION GREATER_EQUAL 2.5.0)
  set(bacio_name bacio)
else()
  set(bacio_name bacio_4)
endif()

# Build code.
add_subdirectory(gblevents.fd)
add_subdirectory(gdascounts_ave.fd)
add_subdirectory(global_postevents.fd)
add_subdirectory(prepobs_cqcbufr.fd)
add_subdirectory(prepobs_cqcvad.fd)
add_subdirectory(prepobs_glerladj.fd)
add_subdirectory(prepobs_listheaders.fd)
add_subdirectory(prepobs_monoprepbufr.fd)
add_subdirectory(prepobs_mpcopybufr.fd)
add_subdirectory(prepobs_oiqcbufr.fd)
add_subdirectory(prepobs_prepacpf.fd)
add_subdirectory(prepobs_prepacqc.fd)
add_subdirectory(prepobs_prepdata.fd)
add_subdirectory(prepobs_prevents.fd)
add_subdirectory(prepobs_profcqc.fd)
add_subdirectory(syndat_syndata.fd)
add_subdirectory(timetwin.fd)
