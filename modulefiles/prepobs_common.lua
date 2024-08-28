help([[
Load common modules to build prepobs on all machines
]])

hdf5_ver=os.getenv("hdf5_ver") or "None"
netcdf_c_ver=os.getenv("netcdf_c_ver") or "None"
netcdf_fortran_ver=os.getenv("netcdf_fortran_ver") or "None"
bacio_ver=os.getenv("bacio_ver") or "None"
w3emc_ver=os.getenv("w3emc_ver") or "None"
sp_ver=os.getenv("sp_ver") or "None"
sigio_ver=os.getenv("sigio_ver") or "None"
nemsio_ver=os.getenv("nemsio_ver") or "None"
bufr_ver=os.getenv("bufr_ver") or "None"

load(pathJoin("hdf5", hdf5_ver))
load(pathJoin("netcdf-c", netcdf_c_ver))
load(pathJoin("netcdf-fortran", netcdf_fortran_ver))
load(pathJoin("bacio", bacio_ver))
load(pathJoin("w3emc", w3emc_ver))
load(pathJoin("sp", sp_ver))
load(pathJoin("sigio", sigio_ver))
load(pathJoin("nemsio", nemsio_ver))
load(pathJoin("bufr", bufr_ver))
