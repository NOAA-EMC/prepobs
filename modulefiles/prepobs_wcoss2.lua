help([[
Load environment to build prepobs on WCOSS2
]])

prepend_path("MODULEPATH", os.getenv("spack_stack_mod_path"))

local stack_intel_ver=os.getenv("stack_intel_ver") or "19.1.3.304"
local stack_cray_mpich_ver=os.getenv("stack_cray_mpich_ver") or "8.1.9"
cmake_ver=os.getenv("cmake_ver") or "3.23.1"

load(pathJoin("stack-intel", stack_intel_ver))
load(pathJoin("stack-cray-mpich", stack_cray_mpich_ver))
load(pathJoin("cmake", cmake_ver))

-- Load common modules for this package
load("prepobs_common")

whatis("Description: prepobs build environment")
