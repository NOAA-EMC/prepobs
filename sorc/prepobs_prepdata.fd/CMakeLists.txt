if(CMAKE_Fortran_COMPILER_ID MATCHES "^(Intel)$")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -convert big_endian -list -assume noold_ldout_format -auto")
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "^(GNU)$")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fconvert=big-endian")
endif()

list(APPEND fortran_src
  prepdata.f
  wordlength.f
  w3unpkb7.f
  w3xtovsmnd.f
  iw3unpbf.f
  w3cnvxtovs.f
  w3rtovsunp.f
  w3atovsunp.f
  w3xtovseds.f
)

set(exe_name prepobs_prepdata)
add_executable(${exe_name} ${fortran_src})
add_dependencies(${exe_name} gblevents)
target_include_directories(${exe_name} PRIVATE ${CMAKE_CURRENT_SOURCE_DIR})
target_link_libraries(
  ${exe_name}
  gblevents
  bufr::bufr_4_DA)

if(MKL_FOUND)
  target_link_libraries(
    ${exe_name}
    ${MKL_LIBRARIES})
endif()

install(TARGETS ${exe_name} RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})
