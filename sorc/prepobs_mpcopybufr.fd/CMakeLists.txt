if(CMAKE_Fortran_COMPILER_ID MATCHES "^(Intel)$")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -convert big_endian -list -assume noold_ldout_format")
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "^(GNU)$")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fconvert=big-endian")
endif()

list(APPEND fortran_src
  mpcopybufr.f90
)

set(exe_name prepobs_mpcopybufr)
add_executable(${exe_name} ${fortran_src})
target_link_libraries(
  ${exe_name}
  w3emc::w3emc_4
  bufr::bufr_4_DA)

install(TARGETS ${exe_name} RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})
