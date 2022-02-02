list(APPEND ush_files
  getges.sh
  getges_driver.sh
  getges_nc.sh
  getges_sig.sh
  prepobs_cqcbufr.sh
  prepobs_cqcvad.sh
  prepobs_glerladj.sh
  prepobs_makeprepbufr.sh
  prepobs_oiqcbufr.sh
  prepobs_prepacqc.sh
  prepobs_prevents.sh
  prepobs_profcqc.sh
  prepobs_syndata.sh
  )

install(PROGRAMS ${ush_files}
        DESTINATION "ush")
