if(CMAKE_Fortran_COMPILER_ID MATCHES "^(Intel)$")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -CB")
endif()

list(APPEND fortran_src
  glerldictmod.f90
  glerlmain.f90
  landtowater.f90
  spd_to_dir.f90
  td_to_q.f90
  uv_to_sd.f90
  watertoland.f90
  uz.f
  uzl.f
)

set(exe_name prepobs_glerladj)
add_executable(${exe_name} ${fortran_src})
target_link_libraries(
  ${exe_name}
  w3emc::w3emc_4
  bufr::bufr_4)

install(TARGETS ${exe_name} RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})
