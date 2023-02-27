help([[
Load environment to build prepobs on Jet
]])

load("cmake/3.20.1")

prepend_path("MODULEPATH", "/lfs4/HFIP/hfv3gfs/role.epic/hpc-stack/libs/intel-18.0.5.274/modulefiles/stack")
load("hpc/1.2.0")
load("hpc-intel/18.0.5.274")
load("hpc-impi/2018.4.274")

-- Load common modules for this package
load("prepobs_common")

whatis("Description: prepobs build environment")
