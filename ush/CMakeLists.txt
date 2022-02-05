list(APPEND ush_files
  build_mainpage.sh
  gdas_counts.sh
  gdas_countstat.sh
  gdas_summary.sh
  gdascounts_combine.sh
  getges.sh
  getges_driver.sh
  getges_nc.sh
  getges_sig.sh
  global_postevents.sh
  mstr_shp_stn_lst_update.sh
  mstr_shp_stn_lst_update2.sh
  prepobs_cqcbufr.sh
  prepobs_cqcvad.sh
  prepobs_glerladj.sh
  prepobs_makeprepbufr.sh
  prepobs_oiqcbufr.sh
  prepobs_prepacqc.sh
  prepobs_prevents.sh
  prepobs_profcqc.sh
  prepobs_syndata.sh
  satellite_daily.pl
  satellite_html.pl
  satellite_summary.sh
  )

install(PROGRAMS ${ush_files}
        DESTINATION "ush")
