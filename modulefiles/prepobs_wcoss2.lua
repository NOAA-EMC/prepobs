help([[
Load environment to build prepobs on WCOSS2
]])

PrgEnv_intel_ver=os.getenv("PrgEnv_intel_ver") or "None"
intel_ver=os.getenv("intel_ver") or "None"
cmake_ver=os.getenv("cmake_ver") or "None"
craype_ver=os.getenv("craype_ver") or "None"
cray_mpich_ver=os.getenv("cray_mpich_ver") or "None"

hdf5_ver=os.getenv("hdf5_ver") or "None"
netcdf_ver=os.getenv("netcdf_ver") or "None"
bacio_ver=os.getenv("bacio_ver") or "None"
w3emc_ver=os.getenv("w3emc_ver") or "None"
sp_ver=os.getenv("sp_ver") or "None"
sigio_ver=os.getenv("sigio_ver") or "None"
nemsio_ver=os.getenv("nemsio_ver") or "None"
bufr_ver=os.getenv("bufr_ver") or "None"

load("envvar")
load(pathJoin("PrgEnv-intel", PrgEnv_intel_ver))
load(pathJoin("intel", intel_ver))
load(pathJoin("cmake", cmake_ver))
load(pathJoin("craype", craype_ver))
load(pathJoin("cray-mpich", cray_mpich_ver))

load(pathJoin("hdf5", hdf5_ver))
load(pathJoin("netcdf", netcdf_ver))
load(pathJoin("bacio", bacio_ver))
load(pathJoin("w3emc", w3emc_ver))
load(pathJoin("sp", sp_ver))
load(pathJoin("sigio", sigio_ver))
load(pathJoin("nemsio", nemsio_ver))
load(pathJoin("bufr", bufr_ver))

whatis("Description: prepobs build environment")
