if(CMAKE_Fortran_COMPILER_ID MATCHES "^(Intel)$")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -convert big_endian -list -assume noold_ldout_format")
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "^(GNU)$")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fconvert=big-endian")
endif()

list(APPEND fortran_src
  prepacqc.f
  acftobs_qc.f
  tranQCflags.f
  output_acqc_prof.f
  output_acqc_noprof.f
  input_acqc.f
  indexc40.f
  sub2mem_mer.f
  sub2mem_um.f
  pmat.f90
  pmat2.f90
  pmat3.f90
  pietc.f90
  pspl.f90
  pkind.f90
)

set(exe_name prepobs_prepacqc)
add_executable(${exe_name} ${fortran_src})
target_link_libraries(
  ${exe_name}
  w3emc::w3emc_4
  bufr::bufr_4)

install(TARGETS ${exe_name} RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})
