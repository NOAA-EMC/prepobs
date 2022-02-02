!----------------------------------------------------------------------
! The following parameter should NEVER be changed unless more "levels"
!  "levels" are added to category 8 GOES sounding/radiance reports.
!
! ILVLMX_08 is the number of category 08 "levels" that code will
!           attempt to fill for a GOES sounding/radiance report (if any
!           are missing a lesser number will actually be output)
      PARAMETER (ILVLMX_08=13)
!----------------------------------------------------------------------
! The following 7 parameters can be changed without the need to make
!  any changes to the source code
!
! ILVLMX_04 is maximum number of category 04 levels allowed in a report
!           {excluding first (surface) level}
      PARAMETER (ILVLMX_04=254)
!
! ILVLMX_11 is maximum number of category 11 levels allowed in a report
!           {excluding first (surface) level}
      PARAMETER (ILVLMX_11=200)
!
! ILVLMX_12 is maximum number of category 12 levels allowed in a report
      PARAMETER (ILVLMX_12=49)
!
! ILVLMX_13 is maximum number of category 13 levels allowed in a report
      PARAMETER (ILVLMX_13=58)
!
! ILVLMX_15 is maximum number of category 15 levels allowed in a report
!           {excluding first (surface) level}
      PARAMETER (ILVLMX_15=381)
!----------------------------------------------------------------------
! The following 8 parameters are fixed
!
! ILVL_10 is number of category 10 levels allowed in a report
      PARAMETER (ILVL_10=1)
!
! ILVL_14 is number of category 14 levels allowed in a report
      PARAMETER (ILVL_14=1)
!
! IDATS is RDATA index where obs data starts (after header, cat. info)
      PARAMETER (IDATS=53)
!
! IDMAX_VAD  is total # of words in RDATA needed by a VAD wind rpt
      PARAMETER (IDMAX_VAD=(IDATS-1)+((ILVLMX_04+1)*5))
!
! IDMAX_PROF is total # of words in RDATA needed by a wind profiler rpt
      PARAMETER (IDMAX_PROF=(IDATS-1)+(ILVL_10*7)+((ILVLMX_11+1)*11))
!
! IDMAX_SCAT is total # of words in RDATA needed by a scatterometer rpt
      PARAMETER (IDMAX_SCAT=(IDATS-1)+(ILVL_10*7))
!
! IDMAX_GOES is total # of words in RDATA needed by a GOES sndg/rad rpt
      PARAMETER (IDMAX_GOES=(IDATS-1)+(ILVLMX_12*9)+(ILVLMX_08*4)+
     $ (ILVLMX_13*3))
!
! IDMAX_GPS  is total # of words in RDATA needed by a GPS-IPW rpt
      PARAMETER (IDMAX_GPS=(IDATS-1)+(ILVL_14*10))
!
! IDMAX_RASS is total # of words in RDATA needed by a RASS temp. rpt
      PARAMETER (IDMAX_RASS=(IDATS-1)+((ILVLMX_15+1)*3))
!
! IDMAX is maximum RDATA index (currently this should never exceed 3500
!       when called by prepdata.f and 2500 when called by listdumps.f
      PARAMETER (IDMAX=MAX(IDMAX_VAD,IDMAX_PROF,IDMAX_SCAT,IDMAX_GOES,
     $ IDMAX_GPS,IDMAX_RASS))
!----------------------------------------------------------------------
