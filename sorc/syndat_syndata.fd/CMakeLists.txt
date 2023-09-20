if(CMAKE_Fortran_COMPILER_ID MATCHES "^(Intel)$")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -convert big_endian -list -assume noold_ldout_format -auto")
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "^(GNU)$")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fconvert=big-endian")
endif()

list(APPEND fortran_src
  syndata.f
)

set(exe_name syndat_syndata)
add_executable(${exe_name} ${fortran_src})
add_dependencies(${exe_name} gblevents)
target_link_libraries(
  ${exe_name}
  gblevents
  bufr::bufr_4)

if(MKL_FOUND)
  target_link_libraries(
    ${exe_name}
    ${MKL_LIBRARIES})
endif()

install(TARGETS ${exe_name} RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})
