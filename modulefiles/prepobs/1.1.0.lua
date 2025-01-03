help([[
Sets environment variables for prepobs pacakage
]])

local pkgName = myModuleName()
local pkgVersion = myModuleVersion()
local pkgNameVer = myModuleFullName()

conflict(pkgName)

local base = "/lfs/h2/emc/global/noscrub/david.huber/glopara_ss/git/prepobs/gfsv17_v1.1.0"

setenv("HOMEprepobs", base)
setenv("EXECprepobs", pathJoin(base, "exec"))
setenv("FIXprepobs", pathJoin(base, "fix"))
setenv("SCRIPTSprepobs", pathJoin(base, "scripts"))
setenv("USHprepobs", pathJoin(base, "ush"))

whatis("Name: ".. pkgName)
whatis("Version: " .. pkgVersion)
whatis("Category: Utility")
whatis("Description: This module sets the environment variables for PREPOBS package")
