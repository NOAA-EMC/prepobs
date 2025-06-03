help([[
Load environment to build prepobs on Hercules
]])

prepend_path("MODULEPATH", os.getenv("spack_stack_mod_path"))

stack_oneapi_ver=os.getenv("stack_oneapi_ver") or "2024.2.1"
stack_intel_oneapi_mpi_ver=os.getenv("stack_intel_oneapi_mpi_ver") or "2021.13"
cmake_ver=os.getenv("cmake_ver") or "3.30.2"

load(pathJoin("stack-oneapi", stack_oneapi_ver))
load(pathJoin("stack-intel-oneapi-mpi", stack_intel_oneapi_mpi_ver))
load(pathJoin("cmake", cmake_ver))

-- Load common modules for this package
load("prepobs_common")

-- On Hercules, MKL needs to be loaded separately
load("intel-oneapi-mkl/2024.2.1")

whatis("Description: prepobs build environment")
