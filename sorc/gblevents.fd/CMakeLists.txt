add_library(gblevents gblevents.f)
target_link_libraries(gblevents PUBLIC nemsio::nemsio
                                       sigio::sigio
                                       ip::ip_4
                                       w3emc::w3emc_4
                                       bacio::${bacio_name}
                                       bufr::bufr_4
                                       NetCDF::NetCDF_Fortran)
