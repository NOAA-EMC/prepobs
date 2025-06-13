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
ip_ver=os.getenv("ip_ver") or "None"
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
-- There isn't a module file for ip yet
--load(pathJoin("ip", ip_ver))
pushenv("ip_ROOT", "/apps/ops/para/libs/intel/19.1.3.304/ip/5.2.0")
pushenv("IP_INC4", "/apps/ops/para/libs/intel/19.1.3.304/ip/5.2.0/include_4")
pushenv("IP_INCd", "/apps/ops/para/libs/intel/19.1.3.304/ip/5.2.0/include_d")
pushenv("IP_LIB4", "/apps/ops/para/libs/intel/19.1.3.304/ip/5.2.0/lib64/libip_4.a")
pushenv("IP_LIBd", "/apps/ops/para/libs/intel/19.1.3.304/ip/5.2.0/lib64/libip_d.a")
pushenv("ip_VERSION", "5.2.0")
load(pathJoin("sigio", sigio_ver))
load(pathJoin("nemsio", nemsio_ver))
load(pathJoin("bufr", bufr_ver))

whatis("Description: prepobs build environment")
