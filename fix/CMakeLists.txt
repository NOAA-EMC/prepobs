# Set URL, sha256sum and tar name of the file to download.
set(URL "https://ftp.emc.ncep.noaa.gov/static_files/public/obsproc")
set(SHA "1dd0bbaf7ab7452de9eb5949595540b20abc4e59439567a87c0b007de90fab3c")
string(SUBSTRING ${SHA} 0 6 SHORTSHA)
set(TAR "obsproc-fix-${SHORTSHA}.tgz") # poor-man's version control

# Download binary fix files and check SHA256
file(DOWNLOAD
  ${URL}/${TAR}
  ${CMAKE_CURRENT_BINARY_DIR}/${TAR}
  INACTIVITY_TIMEOUT 30
  TIMEOUT 90
  SHOW_PROGRESS
  STATUS status
  EXPECTED_HASH SHA256=${SHA}
  )

# Extract downloaded tarball.
 file(ARCHIVE_EXTRACT INPUT ${TAR})

# Collect list of fix files to install.
list(APPEND fix_files
  gdascounts_avg.awk
  gdascounts_base.shtml
  gdascounts_html
  gdascounts_types
  glerldict.lmd
  prepobs_cqc_statbge
  prepobs_errtable.cdas
  prepobs_errtable.nam
  prepobs_oiqc.oberrs
  prepobs_oiqc.oberrs.cdas
  prepobs_prep.bufrtable
  satellite_counts.tbl
  syndat_weight
  )

list(APPEND fix_files
  ${CMAKE_CURRENT_BINARY_DIR}/fix/prepobs_landc
  ${CMAKE_CURRENT_BINARY_DIR}/fix/syndat_syndata.slmask.t126.gaussian
  )

# Install.
install(FILES ${fix_files}
        DESTINATION "fix")
