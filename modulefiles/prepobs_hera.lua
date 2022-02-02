help([[
Load environment to build prepobs on Hera
]])

load("cmake/3.20.1")

prepend_path("MODULEPATH", "/scratch2/NCEPDEV/nwprod/hpc-stack/libs/hpc-stack/modulefiles/stack")
load("hpc/1.1.0")
load("hpc-intel/18.0.5.274")
load("hpc-impi/2018.0.4")

-- Load common modules for this package
load("prepobs_common")

whatis("Description: prepobs build environment")
