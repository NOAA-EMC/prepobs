#!/bin/bash

set -eux

# Location of PWD and package source directory.
pkg_root=$(cd "$(dirname "$(readlink -f -n "${BASH_SOURCE[0]}" )" )" && pwd -P)

INSTALL_TARGET=${INSTALL_TARGET:-"wcoss2"}
INSTALL_PREFIX=${INSTALL_PREFIX:-"${pkg_root}/install"}
MODULEFILE_INSTALL_PREFIX=${MODULEFILE_INSTALL_PREFIX:-"${INSTALL_PREFIX}/modulefiles"}

target="${INSTALL_TARGET,,}"
if [[ "${target}" =~ ^(wcoss2|hera|orion|jet|hercules)$ ]]; then
  # copy the target specific build.ver and run.ver
  cp "${pkg_root}/versions/build.${target}.ver" "${pkg_root}/versions/build.ver"
  cp "${pkg_root}/versions/run.${target}.ver" "${pkg_root}/versions/run.ver"
  export HOMEprepobs="${pkg_root}"
  source "${pkg_root}/versions/build.ver"
  unset HOMEprepobs
  set +x
  module purge
  module use "${pkg_root}/modulefiles"
  module load "prepobs_${target}"
  module list
  set -x
else
  echo "WARNING: Unknown target"
fi

# Create a build directory and cd into it.
[[ -d build  ]] && rm -rf build
mkdir -p build && cd build

# build and install.
cmake -DCMAKE_INSTALL_PREFIX="${INSTALL_PREFIX}" \
      -DCMAKE_INSTALL_BINDIR=exec \
      -DMODULEFILE_INSTALL_PREFIX="${MODULEFILE_INSTALL_PREFIX}" \
      ..
make -j "${BUILD_JOBS:-6}" VERBOSE="${BUILD_VERBOSE:-}"
make install

# Remove build directory upon successfull build and install
cd "${pkg_root}"
rm -rf build

exit 0
