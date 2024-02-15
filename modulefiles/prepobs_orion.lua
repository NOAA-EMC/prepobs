help([[
Load environment to build prepobs on Orion
]])

prepend_path("MODULEPATH", "/work/noaa/epic/role-epic/spack-stack/orion/spack-stack-1.6.0/envs/gsi-addon-env/install/modulefiles/Core")

stack_intel_ver=os.getenv("stack_intel_ver") or "2022.0.2"
stack_impi_ver=os.getenv("stack_impi_ver") or "2021.5.1"
cmake_ver=os.getenv("cmake_ver") or "3.23.1"

load(pathJoin("stack-intel", stack_intel_ver))
load(pathJoin("stack-intel-oneapi-mpi", stack_impi_ver))
load(pathJoin("cmake", cmake_ver))

-- Load common modules for this package
load("prepobs_common")

whatis("Description: prepobs build environment")
