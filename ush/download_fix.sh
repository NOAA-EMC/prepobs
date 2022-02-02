#!/bin/bash

####  UNIX Script Documentation Block
#
# Script name:   download_fix.sh    Script for downloading binary fix files
#
# Author:        R. Mahajan         Org: NCEP/EMC       Date: 2021-10-21
#
# Abstract: This script downloads the binary fix files that used to be a part of the
# VLab repository.
# The downloaded files are:
#   prepobs_landc
#   syndat_syndata.slmask.t126.gaussian
#
# These files are currently hosted on the EMC FTP server at:
#    https://ftp.emc.ncep.noaa.gov/static_files/public/obsproc/obsproc-fix-HASH.tgz
#
# HASH is the truncated 6 character sha256sum hash of the tar file and can be obtained as:
# $> sha256sum obsproc-fix.tgz
#     1dd0bbaf7ab7452de9eb5949595540b20abc4e59439567a87c0b007de90fab3c
#
# The data can be downloaded manually and into the cloned
# repository by executing this script from the root of the cloned directory
# e.g.
# $> pwd
#     /path/to/obsproc
# $> ls -1
#    fix
#    modulefiles
#    sorc
#    ush
# $> ./ush/download_fix.sh
#
# Usage: ./ush/download_fix.sh [yes|no]
#    Script parameters:
#      yes|no - Force download of fix files to overwrite previous download [Default: NO]
#
####

set -eu

# Force option to overwrite fix binary files from a previous download
force=${1:-"NO"}

URL="https://ftp.emc.ncep.noaa.gov/static_files/public/obsproc"
SHA="1dd0bbaf7ab7452de9eb5949595540b20abc4e59439567a87c0b007de90fab3c"
SHORTSHA=$(echo $SHA | cut -c1-6)
TAR="obsproc-fix-${SHORTSHA}.tgz" # poor-man's version control

# fix files to download (contents of the $TAR should match)
fix_files=( \
  prepobs_landc \
  syndat_syndata.slmask.t126.gaussian \
)

# Toggle download if any of the fix files are not present
download=false
for file in ${fix_files[@]}; do
  if [[ ! -f fix/$file ]]; then
    echo -e "\nFix file $file not found in cloned repository!"
    download=true
    break
  fi
done

# Force download if desired
if [[ "$force" =~ [yYtY] ]]; then
  echo -e "\nUsing force to fresh download of fix files"
  download=true
fi

if $download; then
  echo -e "\nDownloading $TAR from $URL"
  rm -f $TAR
  wget -q $URL/$TAR
  SHA256=$(sha256sum $TAR 2>/dev/null | awk '{print $1}')
  if [[ ! "$SHA256" == "$SHA" ]]; then
    echo -e "\nIncorrect checksum, ABORT!"
    exit 1
  fi
  tar xzf $TAR
  rm -f $TAR
else
  echo -e "\nFix files present in the cloned repository. Nothing to download!\n"
fi
