help([[
Sets environment variables for prepobs pacakage
]])

local pkgName = myModuleName()
local pkgVersion = myModuleVersion()
local pkgNameVer = myModuleFullName()

conflict(pkgName)

local base = "/work/noaa/global/kfriedma/git/prepobs/feature-orion_rocky9"

setenv("HOMEprepobs", base)
setenv("EXECprepobs", pathJoin(base, "exec"))
setenv("FIXprepobs", pathJoin(base, "fix"))
setenv("SCRIPTSprepobs", pathJoin(base, "scripts"))
setenv("USHprepobs", pathJoin(base, "ush"))

whatis("Name: ".. pkgName)
whatis("Version: " .. pkgVersion)
whatis("Category: Utility")
whatis("Description: This module sets the environment variables for PREPOBS package")
