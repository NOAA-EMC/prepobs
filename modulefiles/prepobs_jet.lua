help([[
Load environment to build prepobs on Jet
]])

prepend_path("MODULEPATH", "/lfs4/HFIP/hfv3gfs/role.epic/spack-stack/spack-stack-1.6.0/envs/gsi-addon-dev/install/modulefiles/Core")

stack_intel_ver=os.getenv("stack_intel_ver") or "2021.5.0"
stack_impi_ver=os.getenv("stack_impi_ver") or "2021.5.1"
cmake_ver=os.getenv("cmake_ver") or "3.23.1"

load(pathJoin("stack-intel", stack_intel_ver))
load(pathJoin("stack-intel-oneapi-mpi", stack_impi_ver))
load(pathJoin("cmake", cmake_ver))

-- Load common modules for this package
load("prepobs_common")

whatis("Description: prepobs build environment")
