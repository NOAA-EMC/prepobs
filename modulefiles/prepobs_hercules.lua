help([[
Load environment to build prepobs on Orion
]])

prepend_path("MODULEPATH", pathJoin("/work/noaa/epic/role-epic/spack-stack/hercules/spack-stack-" .. (os.getenv("spack_stack_ver") or "None"), "/envs/" .. (os.getenv("spack_env") or "None"), "-env/install/modulefiles/Core")

stack_intel_ver=os.getenv("stack_intel_ver") or "2021.9.0"
stack_impi_ver=os.getenv("stack_impi_ver") or "2021.9.0"
cmake_ver=os.getenv("cmake_ver") or "3.23.1"

load(pathJoin("stack-intel", stack_intel_ver))
load(pathJoin("stack-intel-oneapi-mpi", stack_impi_ver))

load(pathJoin("cmake", cmake_ver))

-- Load common modules for this package
load("prepobs_common")

-- On Hercules, MKL needs to be loaded separately
load("intel-oneapi-mkl/2022.2.1")

whatis("Description: prepobs build environment")
