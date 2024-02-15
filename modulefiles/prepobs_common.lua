help([[
Load common modules to build prepobs on all machines
]])

hdf5_ver=os.getenv("hdf5_ver") or "default"
netcdf_c_ver=os.getenv("netcdf_c_ver") or "default"
netcdf_fortran_ver=os.getenv("netcdf_fortran_ver") or "default"
bacio_ver=os.getenv("bacio_ver") or "default"
w3emc_ver=os.getenv("w3emc_ver") or "default"
sp_ver=os.getenv("sp_ver") or "default"
sigio_ver=os.getenv("sigio_ver") or "default"
nemsio_ver=os.getenv("nemsio_ver") or "default"
bufr_ver=os.getenv("bufr_ver") or "default"

load(pathJoin("hdf5", hdf5_ver))
load(pathJoin("netcdf-c", netcdf_c_ver))
load(pathJoin("netcdf-fortran", netcdf_fortran_ver))
load(pathJoin("bacio", bacio_ver))
load(pathJoin("w3emc", w3emc_ver))
load(pathJoin("sp", sp_ver))
load(pathJoin("sigio", sigio_ver))
load(pathJoin("nemsio", nemsio_ver))
load(pathJoin("bufr", bufr_ver))
