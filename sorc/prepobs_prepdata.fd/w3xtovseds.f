C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    W3XTOVSEDS
C   PRGMMR: DONG             ORG: NP22       DATE: 2020-01-09
C
C ABSTRACT: READS AND UNPACKS ONE REPORT FROM INPUT RTOVS OR ATOVS
C   RETRIEVAL BUFR (DUMP) FILE INTO SPECIFIED FORMAT. RETURNS
C   INFORMATION IN THE FORMAT OF AN UNPACKED NMCEDS FILE. ONLY THOSE
C   ELEMENTS OF NMCEDS NEEDED BY THE PREPDATA PROGRAM ARE ACTUALLY
C   PROCESSED INTO NMCEDS FORMAT, THE REMAINING WORDS OF THE 140-WORD
C   ARRAY ARE SET TO 7777 "MISSING". THERE IS SOME OTHER RETRIEVAL
C   INFORMATION RETURNED. ALSO, INFORMATION ABOUT THE INPUT DATA SET
C   ITSELF (NAME, CENTER DATE, DUMP TIME) IS RETURNED.
C
C PROGRAM HISTORY LOG:
C 1998-03-10  BERT B. KATZ
C 1998-05-05  D.A. KEYSER/NP22 -- STREAMLINED, CORRECTED ERRORS
C 1998-06-15  D.A. KEYSER/NP22 -- ADDED CALL TO W3FI04 FIRST TIME IN
C      TO OBTAIN MACHINE WORD LENGTH (USED TO HARDWIRE WORD LENGTH
C      OF 8 BYTES AS USED IN XMOVEX SUBROUTINE); FILTER FLAG RETURNED
C      IN IBUFTN(20) NOW HAS SAME MEANING AS FOR TOVS (250KM SUB-
C      SAMPLING), IT'S ACTUAL VALUE IS NOW RETURNED (WAS HARDWIRED
C      TO 0)
C 1998-09-21  D.A. KEYSER/NP22 -- SUBROUTINE NOW Y2K AND FORTRAN 90
C      COMPLIANT
C 1999-01-20 D. A. KEYSER -- INCORPORATED BOB KISTLER'S CHANGES NEEDED
C      TO PORT THE CODE TO THE IBM SP
C 1999-11-02  D. A. KEYSER -- CHANGES TO MAKE CODE MORE PORTABLE
C 1999-11-19  D. A. KEYSER -- INCLUDES NOAA-15 ATOVS DATA (AS WELL AS
C      RTOVS DATA)
C 2000-02-17 D. A. KEYSER -- IF DATE OF INPUT FILE IS PRIOR TO
C      7/1/1998 (CAN ONLY BE RTOVS), THEN DOES NOT USE "TOFF" TO GET
C      FILTER FLAG (NOT VALID AT THIS TIME), RATHER SETS OUTPUT
C      FILTER FLAG TO "-99" WHICH ALERTS CALLING PROGRAM TO PROCESS
C      ONLY EVERY 4'TH RTOVS RETRIEVAL AND EVERY 11'TH ATOVS RETRIEVAL
C      TO SIMULATE 250KM SAMPLING (NEEDED FOR HISTORICAL CDAS RUNS)
C 2000-12-05  D.A. KEYSER/NP22 -- ACCOUNTS FOR NESDIS ERROR IN SCALING
C      BOTTOM PRESSURE VALUE (AND WILL STILL WORK RIGHT IF THEY FIX
C      IT);  PERFORMS A SANITY CHECK ON THE BOTTOM PRESSURE VALUE
C      (ATOVS ONLY); IF FILTER FLAG NOT FOUND FOR ATOVS, SET OUTPUT
C      FILTER FLAG TO "-99" WHICH ALERTS CALLING PROGRAM TO PROCESS
C      ONLY EVERY 11'TH RETRIEVAL TO SIMULATE 250KM SAMPLING (NEEDED
C      FOR CDAS RUNS), WAS SET TO 0
C 2001-07-16  D.A. KEYSER/NP22 -- CONSIDERS ATOVS RETRIEVALS WITH BOTH
C      CLEAR AND CLOUDY BIT "OFF" (=0) IN SATELLITE DATA PROCESSING
C      TECHNIQUE TO BE CLOUDY (BEFORE THEY WERE UNKNOWN - RTOVS ARE
C      STILL UNKNOWN IN THIS CASE)
C 2008-03-10  D.A. KEYSER/NP22 -- HANDLES METOP-2 SATELLITE (BUFR
C      SATELLITE ID 004)
C 2012-11-30  J. WOOLLEN      --  INITIAL PORT TO WCOSS; RESET BMISS TO
C      A VALUE (10E08) WHICH WILL NOT CAUSE INTEGER OVERFLOW WHICH CAN
C      BE UNPREDICTABLE (PRIOR BMISS VALUE WAS 10E10)
C 2013-02-14  D. A. KEYSER -- FINAL CHANGES TO RUN ON WCOSS;  OBTAIN
C      VALUE OF BMISS SET IN CALLING PROGRAM VIA CALL TO BUFRLIB
C      ROUTINE GETBMISS RATHER THAN HARDWIRING IT TO 10E08 (OR 10E10);
C      USE BUFRLIB FUNCTION IBFMS TO DETERMINE IF A VARIABLE READ FROM
C      BUFR FILE IS MISSING (I.E. RETURNED AS BMISS); ADDED BMISS AS
C      INPUT ARGUMENT TO SUBROUTINE XTOVSEDS02; USE FORMATTED PRINT
C      STATEMENTS WHERE PREVIOUSLY UNFORMATTED PRINT WAS USED (WCOSS
C      SPLITS UNFORMATTED PRINT AT 80 CHARACTERS)
C 2014-11-25  D. A. KEYSER -- HANDLES NEW SATELLITES METOP-1 (BUFR
C      SATELLITE ID 3, CONVERTED TO LOCAL SATELLITE ID 10), NOAA-19
C      (BUFR SATELLITE ID 223, CONVERTED TO LOCAL SATELLITE ID 9), AND
C      NPP (BUFR SATELLITE ID 224, CONVERTED TO LOCAL SATELLITE ID 11);
C      NOW ENCODES BUFR SATELLITE ID VALUE (CODE TABLE 0-01-007) INTO
C      IBUFTN(2) (PREVIOUSLY A SPARE SET TO MISSING) FOR DIRECT USE BY
C      SUBROUTINE W3CNVXTOVS
C 2020-01-09  J. Dong -- In subroutine W3XTOVSEDS, changed the windowing
C      decade from 20 to 40 for cases when the year is represented by
C      2 digits instead of 4.
C
C USAGE :   CALL W3XTOVSEDS(IUNIT,IBDATE,IBUFTN,ISATOB,PBOT,DSNAME,
C                           IDSDAT,IDSDMP_8,IERR)
C   INPUT ARGUMENT LIST:
C     IUNIT    - UNIT NUMBER OF INPUT FILE CONTAINING RETRIEVAL DATA
C              - IN BUFR FORMAT.
C     DSNAME   - CHARACTER*8 DATA SET NAME (SAME FOR ALL REPORTS IN
C              - A COMMON INPUT DATA SET - SEE REMARKS FOR IERR=1)
C              - (NOTE: ONLY DEFINED AFTER FIRST CALL TO THIS
C              - SUBROUTINE)
C
C   OUTPUT ARGUMENT LIST:
C     IBDATE   - DATE IN SECTION 1 OF BUFR MESSAGE CONTAINING THIS
C              - RETRIEVAL (IN FORM YYYYMMDDHH)
C     IBUFTN   - CONTAINS A SINGLE DECODED RETRIEVAL IN NMCEDS FORMAT
C              - (140 MACHINE WORD LENGTH INTEGER WORDS)
C     ISATOB   - 3-WORD INTEGER ARRAY CONTAINING RETURNED DATA
C              - (SEE REMARKS FOR CONTENTS)
C     PBOT     - FIRST ABOVE-GROUND RTOVS/ATOVS PRESSURE LEVEL IN MB
C              - (MISSING IS 99999.)
C     DSNAME   - CHARACTER*8 DATA SET NAME (SAME FOR ALL REPORTS IN
C              - A COMMON INPUT DATA SET - SEE REMARKS FOR IERR=1)
C     IDSDAT   - INTEGER DATA SET CENTER DATE IN FORM YYYYMMDDHH (SAME
C              - FOR ALL REPORTS IN A COMMON INPUT DATA SET - SEE
C              - REMARKS FOR IERR=1)
C     IDSDMP_8 - INTEGER*8 DATA SET DUMP TIME IN FORM YYYYMMDDHHMM
C              - (SAME FOR ALL REPORTS IN A COMMON INPUT DATA SET - SEE
C              - REMARKS FOR IERR=1)
C     IERR     - ERROR RETURN CODE
C                 =  0 OBSERVATION READ AND UNPACKED INTO LOCATIONS
C                        'IBUFTN', 'ISATOB', AND 'PBOT'.  SEE REMARKS
C                        FOR CONTENTS. NEXT CALL TO W3XTOVSEDS WILL
C                        RETURN NEXT OBSERVATION IN DATA SET.
C                 =  1 INFORMATION ABOUT THE BUFR DATASET IS RETURNED
C                        IN THE OUTPUT ARGUMENTS DSNAME, IDSDAT,
C                        IDSDMP_8 (SEE OUTPUT ARGUMENT LIST ABOVE)
C
C                        THIS SHOULD ALWAYS OCCUR AFTER THE FIRST CALL
C                        TO THIS SUBROUTINE.  NO REPORT IS UNPACKED AT
C                        THIS POINT, AND ONLY DSNAME, IDSDAT, AND
C                        IDSDMP_8 CONTAIN INFORMATION.  ALL SUBSEQUENT
C                        CALLS TO W3XTOVSEDS SHOULD RETURN THE
C                        OBSERVATIONS IN THIS DATA SET, SEQUENTIALLY,
C                        (IERR=0) UNTIL THE END OF FILE IS ENCOUNTERED
C                        (IERR=2).  THE VALUES STORED IN DSNAME,
C                        IDSDAT, AND IDSDMP_8 WILL CONTINUE TO BE
C                        RETURNED ALONG WITH EACH REPORT WHEN IERR = 0.
C                 =  2 FOR NORMAL END-OF-FILE ENCOUNTERED.)
C                 = -1 FOR END-OF-FILE ON FIRST READ -- EMPTY FILE
C                 = -2 FOR INPUT BUFR FILE NOT Y2K COMPLIANT -- NO
C                        PROCESSING DONE
C                 = -3 CENTER DATE COULD NOT BE OBTAINED FROM INPUT
C                        FILE -- NO PROCESSING DONE
C
C   INPUT FILES:
C     UNIT "IUNIT" - FILE CONTAINING BUFR RTOVS OR ATOVS DATA
C
C   OUTPUT FILES:
C     UNIT 06 - STANDARD OUTPUT PRINT
C
C   SUBPROGRAMS CALLED:
C     UNIQUE        - XTOVSEDS02 WORDLENGTH
C     LIBRARY:
C       W3NCO       - GBYTES   W3DIFDAT
C       BUFRLIB     - DATELEN  DUMPBF   OPENBF   READMG   READSB
C                   - UFBINT   UFBREP   CLOSBF   GETBMISS IBFMS
C
C REMARKS:
C
C     CONTENTS OF OUTPUT ARGUMENT IBUFTN {140 INTEGER WORDS - IN NMCEDS
C       FORMAT EXCEPT WHERE NOTED -- ONLY THE WORDS NOTED BELOW
C       ARE FILLED WITH DATA (THAT NEEDED BY THE PREPDATA PROGRAM) -
C       ALL OTHER WORDS ARE SET TO MISSING - 7777)}:
C        WORD 1 - "Local" or "NESDIS" Satellite ID:
C                   NOAA-08 -----  6
C                   NOAA-09 -----  7
C                   NOAA-10 -----  8
C                   NOAA-11 -----  1
C                   NOAA-12 -----  2
C                   NOAA-14 -----  3
C                   NOAA-15 -----  4
C                   NOAA-16 -----  5
C                   NOAA-17 -----  6
C                   NOAA-18 -----  7
C                   NOAA-19 -----  9
C                   METOP-1(B) -- 10
C                   METOP-2(A) --  8
C                   NPP --------- 11
C        * -  2 - BUFR Satellite ID (from Code Table 0-01-007):
C                   NOAA-08 ---- 200
C                   NOAA-09 ---- 201
C                   NOAA-10 ---- 202
C                   NOAA-11 ---- 203
C                   NOAA-12 ---- 204
C                   NOAA-14 ---- 205
C                   NOAA-15 ---- 206
C                   NOAA-16 ---- 207
C                   NOAA-17 ---- 208
C                   NOAA-18 ---- 209
C                   NOAA-19 ---- 223
C                   METOP-1(B) -   3
C                   METOP-2(A) -   4
C                   NPP -------- 224
C             3 - Day of month * 256 plus hour of day (UTC)
C             4 - Minute * 256 plus second (UTC)
C             5 - Latitude (degrees * 100, N+, S-)
C             6 - Longitude (degrees * 100, E+, W-)
C             8 - Land/sea indicator (Land - elev in meters, Sea - 0)
C             9 - Surface (skin) temperature (K * 10)
C            12 - Retrieval method
C        * - 16 - Nadir proximity indicator (Range: 1-9, where 1
C                  is at Nadir and 9 is at limb)
C            20 - Filter Flag {good = 0; redundant = 1;
C                  prior to 07/01/1998 (RTOVS) or missing (ATOVS only)
C                  = -99}
C            23 - Layer 1 - pressure at lower boundary (mb * 10)
C            24 - Layer 1 - pressure at upper boundary (mb * 10)
C            25 - Layer 1 - mean virtual temperature (K * 10)
C            26 - Layer 1 - always missing (7777)
C            27 - 82 - Repeat 23-27 for Layers 2 - 15
C
C        * - Definition changed from original NMCEDS documentation
C         Missings are set to 7777
C
C     CONTENTS OF OUTPUT ARGUMENT ISATOB (3 INTEGER WORDS) FOR EACH
C      RETRIEVAL:
C        WORD  1 - ORBIT NUMBER
C              2 - DAY/NIGHT QUALIFIER (=0 FOR NIGHT)
C                                      (=1 FOR DAY)
C                                      (BUFR C.T. 0-08-013)
C              3 - SNOW/ICE FLAGS (TERRAIN TYPE)
C                                      (RTOVS: BUFR C.T. 0-13-209)
C                                      (ATOVS: BUFR C.T. 0-13-039)
C         Missings are set to XMISS (99999.)
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$
      SUBROUTINE W3XTOVSEDS(IUNIT,IBDATE,IBUFTN,ISATOB,PBOT,DSNAME,
     $ IDSDAT,IDSDMP_8,IERR)

      DATA IFIRST/0/,JFIRST/0/,IUNITL/-99/,KOUNTR/0/,LWI/0/,LWR/0/,
     $ ITOFF/0/, MFIRST/0/,nfirst/0/

      REAL GEOOPR(18),TLAYER(15),PLAYER(16),RINC(5)

      INTEGER IBUFTN(140),ISATOB(3),JDATE(5),JDUMP(5),IBIT(2)

      INTEGER(8) IDSDMP_8,JDUMP_8(5),KDATE(8),LDATE(8)

      CHARACTER*8 SUBSET,DSNAME
      DATA NUMLVL/0/,ISTLVL/0/

      CHARACTER*50 IDTSTR
      DATA IDTSTR/'FOVN SAID CLAT CLON DAYS HOUR MINU SECO ORBN DNQL '/

      CHARACTER*10 IDTST2
      DATA IDTST2/'TOFF OBQL '/

      CHARACTER*10 SATSTR
      DATA SATSTR/'SDPT LSQL '/

      CHARACTER*20 SRFSTR
      DATA SRFSTR/'PRES SELV TERR TERN '/

      CHARACTER*12 RTVSTR,ATVSTR
      DATA RTVSTR/'TMDB   MIXR '/
      DATA ATVSTR/'TMDBST MIXR '/

      REAL(8) XIDENT_8(12),SATDAT_8(2),SRFDAT_8(4),RETDAT_8(2,41),
     $        AMAXIMUM_8,WOPR_8(15),TMP_8,TMPOPR_8(40)
      REAL(8) BMISS,GETBMISS

      REAL TOVLEV(40)

      SAVE

      DATA TOVLEV/0.1,0.2,0.5,1.0,1.5,2.0,3.0,4.0,5.0,7.0,10.0,15.0,
     $            20.0,25.0,30.0,50.0,60.0,70.0,85.0,100.0,115.0,135.0,
     $            150.0,200.0,250.0,300.0,350.0,400.0,430.0,475.0,500.0,
     $            570.0,620.0,670.0,700.0,780.0,850.0,920.0,950.0,
     $            1000.0/
      DATA  XMISS/99999./,IMISS/99999/

C  FCN PR GETS STD. ATMOS. PRESSURE (MB) FROM HEIGHT (M) (HGHT < 11000M)
C  ---------------------------------------------------------------------

      PR(HGHT) = 1013.25 * (((288.15 - (.0065 * HGHT))/288.15)**5.256)

      IF(IFIRST.EQ.0) THEN

C  FIRST TIME IN, SET DATELEN, GET WORDLENGTH FOR INTEGERS AND REALS,
C   AND GET VALUE FOR "BMISS"
C  ------------------------------------------------------------------

         IFIRST = 1
         call datelen(10)
         CALL WORDLENGTH(LWI,LWR)
         PRINT 678, LWI,LWR
  678 FORMAT('  ===> W3XTOVEDS: MACHINE WORD LENGTH: INTEGERS - ',I1,
     $ '; REALS - ',I1/)

      END IF

      IF(IUNIT.NE.IUNITL)  JFIRST = 0
      IF(IUNIT.NE.IUNITL)  MFIRST = 0
      IF(IUNIT.NE.IUNITL)  NFIRST = 0


      IF(JFIRST.EQ.0) THEN

C  FIRST TIME IN FOR A NEW INPUT FILE, GET CENTER AND DUMP TIME FOR
C   FILE AND TEST FOR CENTER FILE DATE PRIOR TO 07/01/1998
C  -----------------------------------------------------------------

         PRINT'("  ==> W3XTOVSEDS -- WCOSS VERSION 01-09-2020")'
         JFIRST = 1
         IUNITL = IUNIT
         CALL DUMPBF(IUNIT,JDATE,JDUMP)
         print'(" CENTER DATE (JDATE) = ",5(I0,1X))', jdate
         print'(" DUMP DATE   (JDUMP) = ",5(I0,1X))', jdump
         print'(1X)'
         IF(JDATE(1).LE.0)  then
            PRINT'(" ##W3XTOVSEDS - CENTER DATE COULD NOT BE OBTAINED ",
     $       "FROM INPUT FILE ON UNIT ",I0," -- IERR = -3")', IUNIT
            ierr = -3
            return
         END IF
         IF(JDATE(1).LT.100)  THEN

C IF 2-DIGIT YEAR RETURNED IN JDATE(1), MUST USE "WINDOWING" TECHNIQUE
C  TO CREATE A 4-DIGIT YEAR

C IMPORTANT: IF DATELEN(10) IS CALLED, THE DATE HERE SHOULD ALWAYS
C            CONTAIN A 4-DIGIT YEAR, EVEN IF INPUT FILE IS NOT
C            Y2K COMPLIANT (BUFRLIB DOES THE WINDOWING HERE)

            PRINT'(" ##W3XTOVSEDS - THE FOLLOWING SHOULD NEVER ",
     $       "HAPPEN!!!!!")'
            PRINT'(" ##W3XTOVSEDS - 2-DIGIT YEAR IN JDATE(1) RETURNED ",
     $       "FROM DUMPBF (JDATE IS: ",I5,4I3,") - USE WINDOWING ",
     $       "TECHNIQUE TO OBTAIN 4-DIGIT YEAR")', JDATE
C IF JDATE=41~99 THEN JDATE=1941~1999
C IF JDATE=00~40 THEN JDATE=2000~2040
            IF(JDATE(1).GT.40)  THEN
               JDATE(1) = 1900 + JDATE(1)
            ELSE
               JDATE(1) = 2000 + JDATE(1)
            ENDIF
            PRINT'(" ##W3XTOVSEDS - CORRECTED JDATE(1) WITH 4-DIGIT ",
     $       "YEAR, JDATE NOW IS: ",I5,4I3)', JDATE
         ENDIF
         IDSDAT = JDATE(1)*1000000+JDATE(2)*10000+JDATE(3)*100+JDATE(4)
         IF(JDUMP(1).LE.0)  THEN
            IDSDMP_8 = 999999999999_8
         ELSE
            IF(JDUMP(1).LT.100)  THEN

C IF 2-DIGIT YEAR RETURNED IN JDUMP(1), MUST USE "WINDOWING" TECHNIQUE
C  TO CREATE A 4-DIGIT YEAR

C IMPORTANT: IF DATELEN(10) IS CALLED, THE DATE HERE SHOULD ALWAYS
C            CONTAIN A 4-DIGIT YEAR, EVEN IF INPUT FILE IS NOT
C            Y2K COMPLIANT (BUFRLIB DOES THE WINDOWING HERE)

               PRINT'(" ##W3XTOVSEDS - THE FOLLOWING SHOULD NEVER ",
     $          "HAPPEN!!!!!")'
               PRINT'(" ##W3XTOVSEDS - 2-DIGIT YEAR IN JDUMP(1) ",
     $          "RETURNED FROM DUMPBF (JDUMP IS: ",I5,4I3,") - USE ",
     $          "WINDOWING TECHNIQUE TO OBTAIN 4-DIGIT YEAR")', JDUMP
C IF JDUMP=41~99 THEN JDUMP=1941~1999
C IF JDUMP=00~40 THEN JDUMP=2000~2040
               IF(JDUMP(1).GT.40)  THEN
                  JDUMP(1) = 1900 + JDUMP(1)
               ELSE
                  JDUMP(1) = 2000 + JDUMP(1)
               ENDIF
               PRINT'(" ##W3XTOVSEDS - CORRECTED JDUMP(1) WITH 4-DIGIT",
     $          " YEAR, JDUMP NOW IS: ",I5,4I3)', JDUMP
            END IF
            JDUMP_8 = JDUMP
            IDSDMP_8 = JDUMP_8(1)*100000000+JDUMP_8(2)*1000000+
     $       JDUMP_8(3)*10000+JDUMP_8(4)*100+JDUMP_8(5)
         ENDIF
c-----------------------------------------------------------------------
c This logic is needed only because SUBSET is not returned in DUMPBF
c  and we need it in order to return dsname - after dsname is retrieved
c  close data set ..
         call openbf(iunit,'IN',iunit)
         call readmg(iunit,subset,ibdate,iret)
         if(subset.eq.'NC003102')  then
            dsname = 'RTOVS   '
            numlvl = 40
            istlvl =  1
         else if(subset.eq.'NC003104')  then
            dsname = 'ATOVS   '
            numlvl = 41
            istlvl =  2
         else
            dsname = '????????'
         end if
         call closbf(iunit)
c-----------------------------------------------------------------------
         KDATE(1:3) = JDATE(1:3)
         KDATE(4)   = 0
         KDATE(5:6) = JDATE(4:5)
         KDATE(7:8) = 0
         LDATE(1)   = 1998
         LDATE(2)   = 07
         LDATE(3)   = 01
         LDATE(4:8) = 0
         CALL W3DIFDAT(KDATE,LDATE,1,RINC)
         IF(RINC(1).LT.0.0)  THEN
            WRITE(6,3004)
 3004       FORMAT(/'##W3XTOVSEDS - DATE OF INPUT FILE IS PRIOR TO ',
     $       '07/01/1998 - VALUE IN "TOFF" NOT RELATED TO FILTER FLAG ',
     $       'SO SET'/15X,'OUTPUT FILTER FLAG TO -99 FOR ALL REPORTS'/)
            ITOFF = 1
         END IF
c-----------------------------------------------------------------------

C  GET VALUE FOR "BMISS"
C  --------------------

         BMISS = GETBMISS()
         print'(1X)'
         print'(" BUFRLIB value for missing passed into W3XTOVSEDS is:",
     $    1X,G0)', bmiss
         print'(1X)'
c-----------------------------------------------------------------------

         IERR = 1
         RETURN
      ELSE  IF(JFIRST.EQ.1) THEN

C  SECOND TIME IN FOR A NEW INPUT FILE , OPEN BUFR DATASET FOR INPUT
C   AND DECODE FIRST MESSAGE
C  -----------------------------------------------------------------

         JFIRST = 2
         CALL OPENBF(IUNIT,'IN',IUNIT)
         CALL READMG(IUNIT,SUBSET,IBDATE,IRET)
         IF(IRET.NE.0) THEN
            WRITE(6,1009) IUNIT
 1009       FORMAT('##W3XTOVSEDS ERROR: EMPTY FILE IN UNIT ',I5)
            IERR = -1
            RETURN
         ENDIF
         IF(IBDATE.LT.100000000)  THEN
c IF INPUT BUFR FILE DOES NOT RETURN MESSAGES WITH A 4-DIGIT YEAR,
c  SOMETHING IS WRONG (EVEN NON-COMPLIANT BUFR MESSAGES SHOULD
c  CONSTRUCT A 4-DIGIT YEAR AS LONG AS DATELEN(10) HAS BEEN CALLED
            WRITE(6,1209) IUNIT
 1209       FORMAT('##W3XTOVSEDS ERROR: A 10-DIGIT SECT. 1 BUFR ',
     $       'MESSAGE DATE WAS NOT RETURNED IN UNIT',I5,' - PROBLEM ',
     $       'WITH BUFR FILE')
            IERR = -2
            RETURN
         END IF

      ENDIF

 1000 CONTINUE

C  EACH CALL TO READSB INCREASES "KOUNTR" BY 1 (REGARDLESS OF RESULT)
C  ------------------------------------------------------------------

      KOUNTR = KOUNTR + 1

      CALL READSB(IUNIT,IRET)
      IF(IRET.NE.0) THEN
         CALL READMG(IUNIT,SUBSET,IBDATE,IRET)
         IF(IRET.NE.0) THEN

C  ALL BUFR MESSAGES HAVE BEEN READ AND DECODED -- ALL DONE
C  --------------------------------------------------------

            WRITE(6,1001) IUNIT
 1001 FORMAT(//' ==> W3XTOVSEDS: END OF FILE ON UNIT',I3,' -- ALL DONE'
     $ /)
            IERR = 2
            RETURN
         ENDIF
         IF(IBDATE.LT.100000000)  THEN
c IF INPUT BUFR FILE DOES NOT RETURN MESSAGES WITH A 4-DIGIT YEAR,
c  SOMETHING IS WRONG (EVEN NON-COMPLIANT BUFR MESSAGES SHOULD
c  CONSTRUCT A 4-DIGIT YEAR AS LONG AS DATELEN(10) HAS BEEN CALLED
            WRITE(6,1209) IUNIT
            IERR = -2
            RETURN
         END IF
         GO TO 1000
      ENDIF

      IBUFTN = 7777
cppppp
cdak  print'(" INITIALLY -- IBUFTN = .....")'
cdak  print, IBUFTN
cppppp

      PBOT = XMISS
      ISATOB = IMISS

      XIDENT_8 = BMISS
      CALL UFBINT(IUNIT,XIDENT_8,12, 1,IRET,IDTSTR//IDTST2)

      SATDAT_8 = BMISS
      CALL UFBINT(IUNIT,SATDAT_8,2, 1,IRET,SATSTR)

      SRFDAT_8 = BMISS
      CALL UFBINT(IUNIT,SRFDAT_8,4, 1,IRET,SRFSTR)

      RETDAT_8 = BMISS
      IF(DSNAME.EQ.'RTOVS   ')  THEN
         CALL UFBINT(IUNIT,RETDAT_8,2,40,NLEV,RTVSTR)
      ELSE
         CALL UFBREP(IUNIT,RETDAT_8,2,41,NLEV,ATVSTR)
      END IF

C  NADIR PROXIMITY INDICATOR {IBUFTN(16)}
C  --------------------------------------

      IF(IBFMS(XIDENT_8(1)).EQ.0)  THEN
         NADIR = INT(ABS(FLOAT(NINT(XIDENT_8(1))) - 28.5)) / 3
         IF(NADIR.LT.9) NADIR = NADIR + 1
      ENDIF
      IF(NADIR.GT.0.AND.NADIR.LT.10)  THEN
         IBUFTN(16) = NADIR
      ELSE
         WRITE(6,2070) KOUNTR,NADIR
 2070 FORMAT(/'##W3XTOVSEDS WARNING: A DECODED RETRIEVAL (#',I7,') HAS',
     $ ' AN INVALID NADIR PROX. IND. (=',I5,') - SKIP IT')
         GO TO 1000
      ENDIF

C  BUFR SATELLITE IDENTIFIER (CODE TABLE 0-01-007) - IBUFTN(2)
C  -----------------------------------------------------------
C  LOCAL OR NESDIS SATELLITE IDENTIFIER -- IBUFTN(1)
C  -------------------------------------------------

      IF(IBFMS(XIDENT_8(2)).EQ.0)  THEN
         IBUFTN(2) = XIDENT_8(2)
         IF(NINT(XIDENT_8(2)).EQ.4)  THEN
            IBUFTN(1) =  8  ! METOP-A(2)
         ELSE IF(NINT(XIDENT_8(2)).EQ.223) THEN
            IBUFTN(1) =  9  ! NOAA-19
         ELSE IF(NINT(XIDENT_8(2)).EQ.3)  THEN
            IBUFTN(1) = 10  ! METOP-B(1)
         ELSE IF(NINT(XIDENT_8(2)).EQ.224)  THEN
            IBUFTN(1) = 11  ! NPP
         ELSE IF(NINT(XIDENT_8(2)).GT.199.AND.
     $           NINT(XIDENT_8(2)).LT.203)  THEN
            IBUFTN(1) = NINT(XIDENT_8(2)) - 194
                            ! NOAA-08 to 10
         ELSE IF(NINT(XIDENT_8(2)).GT.202.AND.
     $           NINT(XIDENT_8(2)).LT.210) THEN
                            ! NOAA-11 to 18
            IBUFTN(1) = NINT(XIDENT_8(2)) - 202
         ELSE
            WRITE(6,2071) KOUNTR,IBUFTN(2)
 2071 FORMAT(/'##W3XTOVSEDS WARNING: A DECODED RETRIEVAL (#',I7,') HAS',
     $ ' AN UNRECOGNIZED BUFR SATELLITE ID (=',I7,') - SKIP IT')
            GO TO 1000
         ENDIF
      ELSE
         WRITE(6,2072) KOUNTR
 2072 FORMAT(/'##W3XTOVSEDS WARNING: A DECODED RETRIEVAL (#',I7,') HAS',
     $ ' A MISSING BUFR SATELLITE ID - SKIP IT')
         GO TO 1000
      ENDIF

C  LATITUDE {IBUFTN(5)}
C  --------------------

      IF(IBFMS(XIDENT_8(3)).EQ.0)  THEN
         IBUFTN(5)  = NINT(XIDENT_8(3) * 100.)
      ELSE
         WRITE(6,2073) KOUNTR
 2073 FORMAT(/'##W3XTOVSEDS WARNING: A DECODED RETRIEVAL (#',I7,') HAS',
     $ ' A MISSING LATITUDE - SKIP IT')
         GO TO 1000
      ENDIF

C  LONGITUDE {IBUFTN(6)}
C  ---------------------

      IF(IBFMS(XIDENT_8(4)).EQ.0)  THEN
         IBUFTN(6)  = NINT(XIDENT_8(4) * 100.)
      ELSE
         WRITE(6,2074) KOUNTR
 2074 FORMAT(/'##W3XTOVSEDS WARNING: A DECODED RETRIEVAL (#',I7,') HAS',
     $ ' A MISSING LONGITUDE - SKIP IT')
         GO TO 1000
      ENDIF

      AMAXIMUM_8 = MAX(XIDENT_8(5),XIDENT_8(6),XIDENT_8(7),XIDENT_8(8))
      IF(IBFMS(AMAXIMUM_8).EQ.0) THEN

C  DAY OF MONTH (* 256) + HOUR (UTC) {IBUFTN(3)}
C  ---------------------------------------------

         IBUFTN(3)  = (256 * NINT(XIDENT_8(5))) + NINT(XIDENT_8(6))

C  MINUTE (* 256) + SECOND (UTC) {IBUFTN(4)}
C  -----------------------------------------

         IBUFTN(4)  = (256 * NINT(XIDENT_8(7))) + NINT(XIDENT_8(8))
      ELSE
         WRITE(6,2075) KOUNTR
 2075 FORMAT(/'##W3XTOVSEDS WARNING: A DECODED RETRIEVAL (#',I7,') HAS',
     $ ' ONE OR MORE MISSING TIME UNITS - SKIP IT')
         GO TO 1000
      ENDIF

C  RETRIEVAL METHOD {IBUFTN(12)}
C  -----------------------------

      IF(IBFMS(SATDAT_8(1)).EQ.0)  THEN
         ISDPT = NINT(SATDAT_8(1))
         IOFF = (LWI * 8) - 6
         CALL GBYTES(ISDPT,IBIT,IOFF,1,1,2)
cppppp
cdak     print'(" ISDPT,IBIT,ICLR,ICDY:",4(I0,1X))',ISDPT,IBIT,ICLR,ICDY
cppppp
      ELSE
         WRITE(6,2076) KOUNTR
 2076 FORMAT(/'##W3XTOVSEDS WARNING: A DECODED RETRIEVAL (#',I7,') HAS',
     $ ' A MISSING SATELLITE DATA PROCESSING TECHNIQUE - SKIP IT')
         GO TO 1000
      ENDIF

      IF(IBIT(1).EQ.1.AND.IBIT(2).EQ.0) THEN

C  --> Clear path

         MCL = 1
         ICCMVS = 1
      ELSE IF((IBIT(1).EQ.0.AND.IBIT(2).EQ.1).OR.
     $        (IBIT(1).EQ.0.AND.IBIT(2).EQ.0.AND.DSNAME.EQ.'ATOVS   '))
     $ THEN

C  --> Cloudy path

         MCL = 2
         IF(NINT(SATDAT_8(2)).EQ.1) THEN
            ICCMVS = 2
         ELSE
            ICCMVS = 3
         ENDIF
      ELSE

C  --> Unknown path

         WRITE(6,2078) KOUNTR,NINT(SATDAT_8(1)),IBIT
 2078 FORMAT(/'##W3XTOVSEDS WARNING: A DECODED RETRIEVAL (#',I7,') HAS',
     $ ' AN UNKNOWN SATELLITE DATA PROCESSING TECHNIQUE (=',I5,','/
     $ 22X,'WITH ICLR=',I1,' AND ICDY=',I1,') - SKIP IT')
         GO TO 1000
      ENDIF

      IBUFTN(12) = (256 * MCL) + (16 * MCL) + 1

C  FILTER FLAG {IBUFTN(20)}
C  ------------------------

      IF(IBFMS(XIDENT_8(11)).EQ.0)  THEN
C  ... for RTOVS and maybe ATOVS if NESDIS starts encoding filter flag
C       in "TOFF"
c  If date of input file is prior to 7/1/1998, then "TOFF" does not
c   indicate filter flag, set filter flag to "-99" (alerts calling
c   program to process only every 4'th RTOVS and every 11'th ATOVS
c   retrieval to simulate 250km sampling - needed for historical cdas
c   runs)
         IF(ITOFF.EQ.0)  THEN
            IBUFTN(20) = NINT(XIDENT_8(11))
         ELSE
            IBUFTN(20) = -99
         END IF
      ELSE IF(IBFMS(XIDENT_8(12)).EQ.0)  THEN
C  ... for ATOVS if NESDIS starts encoding filter flag in "OBQL"
         IBUFTN(20) = NINT(XIDENT_8(12))
      ELSE
         IF(DSNAME.EQ.'RTOVS   ')  THEN
            WRITE(6,3075) KOUNTR
 3075 FORMAT(/'##W3XTOVSEDS WARNING: A DECODED RETRIEVAL (#',I7,') HAS',
     $ ' A MISSING FILTER FLAG - SKIP IT')
            GO TO 1000
         ELSE
C  Currently filter flag is missing for ATOVS - set to "-99" until
C   NESDIS fixes (alerts calling program to process only every 11'th
C   retrieval to simulate 250km sampling - needed for cdas runs)
            if(nfirst.eq.0)  then
               write(6,9082)
 9082 format(/'##W3XTOVSEDS WARNING: NESDIS HAS NOT STORED A FILTER ',
     $ 'FLAG SO SET OUTPUT FILTER FLAG TO -99 FOR ALL REPORTS'/)
               nfirst=1
            end if
            ibuftn(20) = -99
         END IF
      END IF

C  LAND/SEA INDICATOR (IF LAND ELEV, IF SEA 0) {IBUFTN(8)}
C  -------------------------------------------------------

      IF(IBFMS(SRFDAT_8(2)).EQ.0)  IBUFTN(8) = NINT(SRFDAT_8(2))
      IF(DSNAME.EQ.'RTOVS   ')  THEN
C  ... RTOVS defines "LSQF" incorrectly
         IF(NINT(SATDAT_8(2)).EQ.0) IBUFTN(8) = 0
      ELSE
C  ... ATOVS defines "LSQF" correctly
         IF(NINT(SATDAT_8(2)).EQ.1) IBUFTN(8) = 0
      END IF

C  SURFACE (SKIN) TEMPERATURE {IBUFTN(9)}
C  --------------------------------------

C Must set to 7777 (missing) - it is used by PREPDATA (subr. W3FA07)
C  only to the extent that if it is missing, then the surface
C  temperature is obtained from the lowest layer mean virtual temp.
c  only, rather than from the avg. of the lowest layer mean virt.
C  temperature and this surface (skin) temp. {IBUFTN(9)}.
C   - This may not be correct, but it is consistent with the TOVS
C     processing from before, where even though the skin temp.
C     in IBUFTN(9) was not missing, the 7'th bit (counting from right,
C     where rightmost bit is position 0) of the value in IBUFTN(11)
C     was always 0 (the largest value for IBUFTN(11) in TOVS was 64).
C
C  This test in W3FA07 is:
C
C      TSFC  = IBUFTN(9)/10.0
C                 ....
C                 ....
C      IF(.NOT.(BTEST(IBUFTN(11),7)).OR.IBUFTN(9).EQ.7777)
C    $ TSFC = TBAR(1) + (.0555 * (IP1(1) - PMID(1)))
C     TSFC = (TSFC + TBAR(1) + (.0555 * (IP1(1) - PMID(1))))/2
C
C  This allows us to ignore IBUFTN(11) here (set to 7777, which
C   actually yields a 0 in bit 7 anyway!!)

      IBUFTN(9) = 7777

      IF(NLEV.EQ.NUMLVL)  THEN

C  FILL IN THE LEVEL SOUNDING DATA - MUST HAVE 40 TEMP LEVELS TO ACCEPT
C  (Note: For ATOVS, Cloud-top press. is on level 1 - skip to next lvl)
C  --------------------------------------------------------------------

         LL = 0
         DO  L = ISTLVL,NLEV
            LL = LL + 1
            IF(IBFMS(RETDAT_8(1,L)).NE.0) THEN
               WRITE(6,2082) KOUNTR
 2082 FORMAT(/'##W3XTOVSEDS WARNING: A DECODED RETRIEVAL (#',I7,') ',
     $ 'CONTAINS 1 OR MORE MISSING TEMERATURES IN ITS SOUNDING - SKIP ',
     $ 'IT')
               GO TO 1000
            ENDIF
            TMPOPR_8(LL) = RETDAT_8(1,L)
cppppp
cdak           print'(" AT L = ",I0,"; RETDAT_8(2,L) = ",G0)',
cdak $          L,RETDAT_8(2,L)
cppppp
            TMP_8=RETDAT_8(2,L)*1000._8
            IF(LL.GT.25) WOPR_8(LL-25) = MIN(BMISS,TMP_8)
         ENDDO
         NLEV = 40
      ELSE
         WRITE(6,2081) KOUNTR,NLEV
 2081 FORMAT(/'##W3XTOVSEDS WARNING: DECODED RETR (#',I7,') DOES NOT ',
     $ 'CONTAIN 40 SNDG LVLS (PLUS 1 CLOUD-TOP LVL IF ATOVS) (HAS ',I3,
     $ ' LVLS)-SKIP IT')
         GO TO 1000
      ENDIF

      IF(IBFMS(SRFDAT_8(1)).EQ.0)  THEN
         PBOT = 0.01 * SRFDAT_8(1)
C --> NESDIS is scaling bottom pressure incorrectly, this accounts for
C      this and for a future fix on their part (fixed on 11/07/2001)
         IF(PBOT.LT.200.)  then
            IF(MFIRST.EQ.0)  THEN
               WRITE(6,9081)
 9081 FORMAT(/'##W3XTOVSEDS WARNING: NESDIS IS STORING BOTTOM PRESSURE',
     $ ' AS 0.1 PASCALS INSTEAD OF AS PASCALS, THIS SUBROUTINE WILL ',
     $ 'CORRECT'/24X,'FOR THIS UNTIL NESDIS FIXES THEIR ERROR'/)
               MFIRST=1
            END IF
            PBOT = 0.1 * SRFDAT_8(1)
         end if
         IF(DSNAME.EQ.'ATOVS   ')  THEN

C  For ATOVS (only), do a SANITY check on pressure (NESDIS can have
C   some strange values)

            DVAL = PBOT - PR(REAL(IBUFTN(8)))
            IF(DVAL.LE.-75..OR.DVAL.GE.50.)  THEN
               WRITE(6,2087) KOUNTR,PBOT,IBUFTN(8)
 2087 FORMAT(/'##W3XTOVSEDS WARNING: A DECODED RETRIEVAL (#',I7,') HAS',
     $ ' A BAD BOTTOM PRES LVL (PBOT=',F6.1,' MB, ELEV=',I4,' M) - ',
     $ 'SKIP IT')
               GO TO 1000
            ENDIF
         ENDIF
         DO  L = 40,1,-1
            IF(NINT(PBOT*100.).GT.NINT(TOVLEV(L)*100.)) THEN
               LST = L
               GO TO 1850
            ENDIF
         ENDDO
         WRITE(6,3079) KOUNTR,PBOT
 3079 FORMAT(/'##W3XTOVSEDS WARNING: A DECODED RETRIEVAL (#',I7,') HAS',
     $ ' A STRANGE BOTTOM PRESSURE (=',F8.2,' MB)  - SKIP IT')
         GO TO 1000
 1850    CONTINUE
         PLST = TOVLEV(LST)
      ELSE
         WRITE(6,2079) KOUNTR
 2079 FORMAT(/'##W3XTOVSEDS WARNING: A DECODED RETRIEVAL (#',I7,') HAS',
     $ ' A MISSING BOTTOM PRESSURE - SKIP IT')
         GO TO 1000
      ENDIF

C For now set PTRP (trop pressure) to 0.0 as we can't determine this
C  (this means we may not want to throw out superadiabatic retr. as
C   determined by subr. XTOVSEDS02 since the entire column is checked,
C   when the logic expects to only check sfc to trop for supers)

      PTRP = 0.0
cppppp
cdak  print'(" Going into XTOVSEDS02 -- IBUFTN = .....")'
cdak  print, IBUFTN
cdak  print'(" TMPOPR_8 = ",G0)', TMPOPR_8
cdak  print'(" WOPR_8 = ",G0)', WOPR_8
cdak  print'(" PLST = ",G0)', PLST
cdak  print'(" ICCMVS = ",I0)', ICCMVS
cdak  print'(" PTRP = ",G0)', PTRP
cppppp
      CALL XTOVSEDS02(TMPOPR_8,WOPR_8,PLST,ICCMVS,PTRP,TLAYER,GEOOPR,
     $ PLAYER,ISUPER,BMISS)

      IF(ISUPER.EQ.1)  THEN
         WRITE(6,2080) KOUNTR
 2080 FORMAT(/'##W3XTOVSEDS WARNING: A DECODED RETRIEVAL (#',I7,') IS ',
     $ 'SUPERADIABATIC ACCORDING TO SUBR. XTOVSEDS02- SKIP IT')
         GO TO 1000
      ENDIF

      DO  L = 1,15
        LREV = 16 - L
        IF(TLAYER(L).LT.999.99) THEN
          IBUFTN(4*L+19) = NINT(PLAYER(LREV+1) * 10.)
          IBUFTN(4*L+20) = NINT(PLAYER(LREV) * 10.)
          IBUFTN(4*L+21) = NINT((TLAYER(L) * 10.) + 0.0000001)
        ENDIF
      ENDDO

C  ORBIT NUMBER
C  ------------

      IF(IBFMS(XIDENT_8(9)).EQ.0)  ISATOB(1) = NINT(XIDENT_8(9))

C  DAY-NIGHT INDICATOR
C  -------------------

      IF(IBFMS(XIDENT_8(10)).EQ.0)  ISATOB(2) = NINT(XIDENT_8(10))

C  TERRAIN INDICATOR
C  -----------------

      IF(IBFMS(SRFDAT_8(3)).EQ.0)  THEN
C  ... for RTOVS
         ISATOB(3) = NINT(SRFDAT_8(3))
      ELSE  IF(IBFMS(SRFDAT_8(4)).EQ.0)  THEN
C  ... for ATOVS
         ISATOB(3) = NINT(SRFDAT_8(4))
      END IF

      IERR = 0

      RETURN

      END

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    XTOVSEDS02
C   PRGMMR: KEYSER           ORG: NP22        DATE: 2013-02-14
C
C ABSTRACT: USES RTOVS OR ATOVS 18 MANDATORY-LEVEL GEOPOTENTIAL
C   HEIGHTS AND 40-LEVEL TEMPERATURES TO ARRIVE AT 15-LAYER AVERAGED
C   VIRTUAL TEMPERATURES. IF THE LOW LEVELS ARE MISSING, THEN FILLER
C   (999.99) INSERTED IN PLACE OF THE TEMPERATURES.
C
C PROGRAM HISTORY LOG:
C 1985-06-07  E. BURDSALL (NESDIS)
C 1990-04-30  T. GARDNER (NESDIS/STX)
C 1994-03-22  BERT B. KATZ   ADPATED FOR USE IN CDAS/RE-ANALYSIS
C 2012-11-30  J. WOOLLEN  INITIAL PORT TO WCOSS; RESET BMISS TO A VALUE
C      (10E08) WHICH WILL NOT CAUSE INTEGER OVERFLOW WHICH CAN BE
C      UNPREDICTABLE (PRIOR BMISS VALUE WAS 10E10)
C 2013-02-14  D. A. KEYSER   FINAL CHANGES TO RUN ON WCOSS; BMISS IS
C      NOW PASSED IN FROM CALLING PROGRAM PROGRAM (ADDED AS AN INPUT
C      ARGUMENT HERE)
C
C USAGE:    CALL XTOVSEDS02(TMPOPR_8,WOPR_8,PSFC,ICCMVS,
C                           PTRP,TKLNMC,GPHGT,PNMC,ISUPER,BMISS)
C   INPUT ARGUMENT LIST:
C     TMPOPR_8 - REAL*8 RTOVS/ATOVS 40-LEVEL RETRIEVED TEMPERATURES
C                (DEG. K).
C     WOPR_8   - REAL*8 RTOVS/ATOVS 15-LEVEL RETRIEVED MIXING RATIOS
C                (G / KG).
C     PSFC     - RTOVS/ATOVS MODEL SURFACE LEVEL (MB).
C     ICCMVS   - NESDIS CHANNEL COMBINATION FLAG FOR MVS RETRIEVALS.
C     PTRP     - TROPOPAUSE PRESSURE (MB).
C     BMISS    - REAL*8 BUFRLIB VALUE FOR MISSING
C
C   OUTPUT ARGUMENT LIST:
C     TKLNMC   - RTOVS/ATOVS 15-LAYER RETRIEVED VIRTUAL TEMPS (DEG. K).
C     GPHGT    - RTOVS/ATOVS 18 MANDATORY-LEVEL GEOPOTENTIAL HGHTS (M).
C     PNMC     - PRESSURES AT LAYER INTERFACES (MB).
C     ISUPER   - NESDIS SUPERADIABATIC FLAG.
C
C REMARKS: CALLED BY SUBROUTINE W3XTOVSEDS.
C
C   KEY LOCAL PARAMETERS-
C      VARIABLE                TYPE  FUNCTION
C      --------                ----  --------
C      NPMAN                   I*4   NUMBER OF MANDATORY LEVELS (18)
C      NPRET                   I*4   NUMBER OF RTOVS/ATOVS TEMPERATURE
C                                    RETRIEVAL LEVELS (40)
C      NPMIX                   I*4   NUMBER OF RTOVS/ATOVS MOISTURE
C                                    RETRIEVAL LEVELS (15)
C      C0 - C14                R*4   CONSTANTS USED TO CALCULATE
C                                    LAYER-MEAN TEMPERATURES
C      THK                     R*4   GEOPOTENTIAL THICKNESS (M) BETWEEN
C                                    SELECTED MANDATORY PRESSURE LEVELS
C      PLOC(16)                R*4   NCEP PRESSURE LEVELS
C      NPNMC                   I*4   NUMBER OF NCEP PRESSURE LEVELS (16)
C
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$
      SUBROUTINE XTOVSEDS02(TMPOPR_8,WOPR_8,PSFC,ICCMVS,PTRP,TKLNMC,
     $ GPHGT,PNMC,ISUPER,BMISS)

      REAL PLOC(16),GPHGT(18),PMAN(18),PRET(40)
      REAL DLP(39),TKLNMC(15),PNMC(16),TVIRT(40),ZHGT(40)

      REAL(8)  BMISS,WOPR_8(15),TMPOPR_8(40)

      DATA C0/0.257286585/,C1/0.5/,C2/0.242713415/,C3/0.20751875/,
     $ C4/0.5/,C5/0.29248125/,C6/0.3782354/,C7/0.5/,C8/0.1217646/,
     $ C9/0.1217646/,C10/0.278746475/,C11/0.3782354/,C12/0.221253525/,
     $ C13/0.2435292/,C14/0.034164751/

      LOGICAL L1,L3,L4,L5,L6

      DATA LNMC1/4/,LNMC2/5/,LNMC3/6/,LNMC4/7/,LNMC5/8/,LNMC6/9/,
     $ LNMC7/11/,LNMC8/12/,LNMC9/13/,LNMC10/14/
      DATA NPRET/40/,NPMAN/18/,NPNMC/16/,NLT/15/,FILL/999.99/,NPMIX/15/
      DATA PRET/0.1,0.2,0.5,1.0,1.5,2.0,3.0,4.0,5.0,7.0,10.0,15.0,20.0,
     $ 25.0,30.0,50.0,60.0,70.0,85.0,100.0,115.0,135.0,150.0,200.0,
     $ 250.0,300.0,350.0,400.0,430.0,475.0,500.0,570.0,620.0,670.0,
     $ 700.0,780.0,850.0,920.0,950.0,1000.0/
      DATA DLP/0.69315,0.91629,0.69315,0.40546,0.28768,0.40546,
     $ 0.28768,0.22314,0.33647,0.35667,0.40546,0.28768,0.22314,0.18232,
     $ 0.51083,0.18232,0.15415,0.19416,0.16252,0.13976,0.16034,0.10536,
     $ 0.28768,0.22314,0.18232,0.15415,0.13353,0.07232,0.09953,0.05129,
     $ 0.13103,0.08408,0.07756,0.04380,0.10821,0.08594,0.07914,0.03209,
     $ 0.05129/
      DATA PMAN/1.,3.,7.,10.,20.,30.,50.,70.,100.,150.,200.,250.,300.,
     $ 400.,500.,700.,850.,1000./
      DATA PLOC/0.4,1.0,2.0,5.0,10.0,30.0,50.0,70.0,100.0,200.0,
     $ 300.0,400.0,500.0,700.0,850.0,1000.0/

      DATA CONS/14.6435/
      DATA TSUPLM/10.0/

      INTEGER CCHLKP(7,2),CCLLKP(7),CCWLKP(7)
      DATA CCHLKP/0,2,2,2,6,6,5,0,1,1,1,3,3,4/
      DATA CCLLKP/0,1,2,5,4,6,3/
      DATA CCWLKP/0,1,0,0,0,0,2/

      INTEGER IDXSUP(18)
      DATA IDXSUP/4,7,10,11,13,15,16,18,20,23,24,25,26,28,31,35,37,40/

      DATA NTPRET/0/

      PNMC = PLOC

      L1=ANINT(PSFC).GE.ANINT(PRET(32))

      DO  I = 1,40
         IF(ANINT(PSFC).EQ.ANINT(PRET(I))) THEN
           LST = I
           EXIT
         ENDIF
      ENDDO

C  INITILIZE DO-LOOP LIMITS FOR COMPUTING GEOPOTENTIAL HEIGHTS.

      JSTRT=2
      JSTOP=LST

C  INITIALIZE VIRTUAL TEMPERATURE ARRAY.

      TVIRT = TMPOPR_8

C  ADJUST GEOPOTENTIAL HEIGHT DO-LOOP LIMITS

      ZHGT = 0.0
 
      IF(.NOT.L1) THEN
         JSTOP=NPRET-NPMIX
         GO TO 350
      ENDIF

C  COMPUTE VIRTUAL TEMPERATURES (K).

      LSTART=NPRET-NPMIX+1
      DO  L = LSTART,LST
cppppp
cdak  print'(" WOPR_8(L-NPRET+NPMIX)  = ",G0)', WOPR_8(L-NPRET+NPMIX)
cdak  print'(" TVIRT(L) = ",G0)', tvirt(l)
cppppp
         IF(WOPR_8(L-NPRET+NPMIX).LT.BMISS) TVIRT(L)=
     $    TMPOPR_8(L)*(1.0+.61*WOPR_8(L-NPRET+NPMIX)/
     $             (1000.0+WOPR_8(L-NPRET+NPMIX)))
cppppp
cdak  print'(" TVIRT(L) = ",G0)', tvirt(l)
cppppp
      ENDDO

  350 CONTINUE

C  COMPUTE GEOPOTENTIAL HEIGHTS (METERS) AT RTOVS/ATOVS LEVELS.

      DO  J = JSTRT,JSTOP
         I=JSTOP+1-J
         ZHGT(I)=ZHGT(I+1)+(TVIRT(I)+TVIRT(I+1))*DLP(I)*CONS
      ENDDO

      GPHGT(1:NPMAN) = ZHGT(IDXSUP(1:NPMAN))

      ISUPER=0
      L5=ANINT(PSFC).LT.ANINT(PRET(32))

C  DETERMINE IF NECESSARY TO CHECK FOR SUPERADIABATIC LAPSE RATES.

      IF (L5) GO TO 450

C  DETERMINE FIRST RTOVS/ATOVS PRESSURE LEVEL HIGHER THAN OR AT THE
C    TROPOPAUSE PRESSURE.

      LTROP=2
      DO  I = 2,LST
         J=NPRET+1-I
         IF (PRET(J).GT.PTRP) CYCLE
         LTROP=J
         EXIT
      ENDDO
cppppp
cdak  print'(" After trop loop, LTROP = ",I0," (should be 2)")', LTROP
cppppp

C  DETERMINE FIRST MANDATORY PRESSURE LEVEL AT OR BELOW THE RTOVS/ATOVS
C   LEVEL DETERMINED IN THE PREVIOUS STEP.

      DO  I = 1,NPMAN
         IF (PMAN(I).LT.PRET(LTROP)) CYCLE
         LTROP=I
         EXIT
      ENDDO

cppppp
cdak  print'(" After pman loop, LTROP = ",I0," (should be 1)")', LTROP
cppppp

C DETERMINE FIRST MANDATORY PRESSURE LEVEL AT OR HIGHER THAN THE SURFACE
C  PRESSURE LEVEL.

      DO  I = 1,NPMAN
         J=NPMAN+1-I
         IF (ANINT(PMAN(J)).GT.ANINT(PSFC)) CYCLE
         LSURF=J
         EXIT
      ENDDO

C  CHECK FOR SUPERADIABATIC LAPSE RATES BETWEEN MANDATORY PRESSURE
C   LEVELS 'LTROP' AND 'LSURF'.

      LSTOP=LSURF-1
      DO  J = LTROP,LSTOP

C  IF PRESSURE LEVEL AT OR ABOVE 100MB, CHANGE UNITS OF GEOPOTENTIAL
C   HEIGHTS FROM DECAMETERS TO METERS.

         GPHIGH=GPHGT(J)
         GPLOW=GPHGT(J+1)
         DZ=ABS(GPLOW-GPHIGH)
         IF (DZ.LT.0.000001) CYCLE
         K=J+1
         TMPVAL=(TMPOPR_8(IDXSUP(K))-TMPOPR_8(IDXSUP(J)))*(1000.0/DZ)
         IF (TMPVAL.LE.TSUPLM) CYCLE
         ISUPER=1
         GO TO 450
      ENDDO

C  CHECK FOR SUPERADIABATIC LAPSE RATE BETWEEN LEVELS 'LSURF' AND 'LST',
C   IF NECESSARY.

      IF (ANINT(PMAN(LSURF)).GE.ANINT(PSFC)) GO TO 450
      DO  I = 21,LST
         IF (PRET(I).NE.PMAN(LSURF)) CYCLE
         LEVEL=I
         EXIT
  445    CONTINUE
      ENDDO
      DZ=ABS(ZHGT(LEVEL)-ZHGT(LST))
      IF (DZ.LT.0.000001) GO TO 450
      TMPVAL=(TMPOPR_8(LST)-TMPOPR_8(LEVEL))*(1000.0/DZ)
      IF (TMPVAL.LE.TSUPLM) GO TO 450
      ISUPER=1

  450 CONTINUE
      IF(ISUPER.EQ.1) RETURN

      TKLNMC = FILL

      ICCTHI = CCHLKP(ICCMVS+1,2)
      ICCTLO = CCLLKP(ICCMVS+1)
      ICCW = CCWLKP(ICCMVS+1)
      IF(ICCTHI.LT.1 .OR. ICCTHI.GT.6) ICCTHI = 6
      IF(ICCTLO.LT.0 .OR. ICCTLO.GT.6) ICCTLO = 0
      IF(ICCW.LT.0 .OR. ICCW.GT.2) ICCW = 0
      L1=ISUPER.EQ.1
      L3=ICCTLO.EQ.0
      L4=ICCW.EQ.0.AND.ICCTLO.NE.0.AND.LST.LT.32
      L5=ICCTHI.NE.6
      L6=ICCTHI.EQ.6

C  CHECK FOR AVAILABILITY OF GEOPOTENTIAL HEIGHTS FROM THE SURFACE TO
C   100 MB. IF FOLLOWING TESTS ARE SATISFIED GEOPOTENTIAL HEIGHTS ARE
C   NOT AVAILABLE.

      IF (L1) GO TO 510

C  CHECK IF TROPOSPHERIC TEMPERATURES SOUTH OF 10N ARE NOT TO BE
C   COMPUTED

      IF (L3) GO TO 510

C  CHECK IF SURFACE DATA IS RELIABLE. IF TEST NOT  SATISFIED,
C   GEOPOTENTIAL HEIGHTS ARE AVAILABLE FROM THE SURFACE TO 0.4 MB.

      IF (.NOT.L4) GO TO 530

C  DATA ONLY AVAILABLE TO COMPUTE LAYER-MEAN TEMPERATURES BETWEEN
C   SELECTED NCEP PRESSURE LEVELS FROM 200 TO 0.4 MB. SET POINTER(NUMT)
C   DENOTING NUMBER OF LAYER-MEAN TEMPERATURES  TO BE CALCULATED.

      NUMT=LNMC6
      NTPRET=NTPRET+1

C  SET POINTER(NT) SPECIFYING ATMOSPHERIC LAYER AT WHICH TEMPERATURE
C   CALCULATIONS BEGIN(100-200MB LAYER).

      NT=LNMC4

C  CHECK FOR AVAILABILITY OF RETRIEVAL DATA ABOVE 10 MB.

C  DATA AVAILABLE TO COMPUTE LAYER-MEAN TEMPERATURES ONLY BETWEEN
C   SELECTED NCEP PRESSURE LEVELS FROM 200 TO 10 MB. RESET POINTER(NUMT)
C   FOR NUMBER OF MEAN-LAYER TEMPERATURES TO BE CALCULATED.

      IF (.NOT.L5) NUMT=LNMC2
      GO TO 570

  510 CONTINUE

C  RETRIEVAL DATA IS ONLY AVAILABLE TO COMPUTE LAYER-MEAN TEMPERATURES
C   BETWEEN SELECTED NCEP PRESSURE LEVELS FROM 100 TO 0.4 MB.  SET
C   POINTER(NUMT) FOR NUMBER OF LAYER-MEAN TEMPERATURES TO BE CALCULATED

      NUMT=LNMC5
      NTPRET=NTPRET+1

C  SET POINTER(NT) SPECIFYING ATMOSPHERIC LAYER AT WHICH TEMPERATURE
C   CALCULATIONS BEGIN(70-100 MB LAYER).

      NT=LNMC5

C  CHECK FOR AVAILABILITY OF RETRIEVAL DATA ABOVE 10 MB.

C  DATA AVAILABLE TO COMPUTE LAYER-MEAN TEMPERATURES ONLY BETWEEN
C   SELECTED NCEP PRESSURE LEVELS FROM 100 TO 10 MB. RESET POINTER(NUMT)
C   FOR NUMBER OF TEMPERATURES TO BE CALCULATED.

      IF (.NOT.L5) NUMT=LNMC1

C  INITIALIZE POINTER(M): SPECIFIES THE STARTING MANDATORY PRESSURE
C   LEVEL AT WHICH GEOPOTENTIAL HEIGHT DATA IS USED FOR CALCULATION OF
C   LAYER-MEAN TEMPERATURES.

      M=LNMC6
      GO TO 580

  530 CONTINUE

C  DATA AVAILABLE TO COMPUTE LAYER-MEAN TEMPERATURES BETWEEN SELECTED
C   NCEP PRESSURE LEVELS FROM THE SURFACE TO 0.4 MB. SET POINTER(NUMT)
C   FOR NUMBER OF LAYER-MEAN TEMPERATURES TO BE CALCULATED.

      NUMT=NPNMC-1

C  SET POINTER(NT) SPECIFYING ATMOSPHERIC LAYER AT WHICH TEMPERATURE
C   CALCULATIONS ARE TO BEGIN(1000-850 MB LAYER).

      NT=1

C  CHECK FOR AVAILABILITY OF RETRIEVAL DATA ABOVE 10 MB.

      IF (.NOT.L5) THEN

C  DATA AVAILABLE TO COMPUTE LAYER-MEAN TEMPERATURES BETWEEN SELECTED
C   NCEP PRESSURE LEVELS FROM THE SURFACE TO 10 MB.  RESET POINTER(NUMT)
C   FOR NUMBER OF TEMPERATURES TO BE CALCULATED.

         NUMT=LNMC7
      ENDIF

C  INITIALIZE POINTER(M): SPECIFIES STARTING MANDATORY PRESSURE LEVEL
C   AT WHICH GEOPOTENTIAL HEIGHT DATA IS TO BE USED FOR CALCULATION OF
C   LAYER-MEAN TEMPERATURES.

      M=NPMAN

C  CHECK TO DETERMINE IF SURFACE PRESSURE IS LESS THAN 1000 MB.

      IF(ANINT(PNMC(NPNMC)).EQ.ANINT(PSFC)) GO TO 580

C  ADJUST NUMBER OF NCEP PRESSURE LEVELS WHEN SURFACE PRESSURE IS LESS
C   THAN 1000 MB.

      DO  I = 1,NPNMC-1

C  REVERSE ORDERING SEQUENCE OF NCEP PRESSURE LEVELS (START WITH LEVEL
C   15: E.G. PNMC(15)=850 MB).

         J=NPNMC-I

C  RESET POINTER(NT) SPECIFYING FIRST LAYER WHERE LAYER-MEAN TEMPERATURE
C   CALCULATIONS ARE TO BEGIN.

         NT=I

C  CHECK TO DETERMINE NEW NCEP SURFACE PRESSURE.

         IF(ANINT(PSFC).GT.ANINT(PNMC(J))) EXIT
      ENDDO

C  DEFINE NEW NCEP SURFACE PRESSURE.

      PNMC(J+1)=PSFC

C  RESET POINTER(NUMT) FOR NUMBER OF LAYER-MEAN TEMPERATURES TO BE
C   CALCULATED.

      NUMT=J
      IF (L6) NUMT=NUMT-LNMC1

C  RESET POINTER(M) FOR MANDATORY PRESSURE LEVEL.

      M=J+3
      GO TO 580

  570 CONTINUE

C  RESET POINTER(M) FOR MANDATORY PRESSURE LEVELS( FOR CASES WHEN DATA
C   ONLY AVAILABLE BETWEEN 200-0.4 OR 200-10 MB.

      M=LNMC7

  580 CONTINUE

C  DEFINE LOWER AND UPPER DO-LOOP LIMITS FOR ERROR ESTIMATE CALCULATIONS
C   (USED IN SUBROUTINE CALERR).

      NTL=NT
      NTU=NTL+NUMT-1

C  CALCULATE LAYER-MEAN TEMPERATURES
C  ---------------------------------

      DO I = NTL,NTU

C  REVERSE ORDERING SEQUENCE OF NCEP PRESSURE LEVELS.

         J=NLT-I+1

C  CHECK FOR SPECIAL CASES WHEN GEOPOTENTIAL THICKNESSES MUST BE SUMMED
C   TO COMPUTE LAYER-MEAN TEMPERATURES.

         IF(I.EQ.LNMC3) GO TO 590
         IF(I.EQ.LNMC4) GO TO 600
         IF(I.EQ.LNMC7) GO TO 610

C  CHECK TO ENSURE IF DATA AVAILABLE ABOVE 10 MB. IF TEST SATISFIED
C   CALCULATION OF LAYER-MEAN TEMPERATURES NOT REQUIRED USING RETRIEVAL
C   TEMPERATURE DATA.

         IF(L6) GO TO 690
         GO TO 640

  590    CONTINUE

C  DEFINE LOWER(ML) AND UPPER(MU) DO-LOOP LIMITS FOR SUMMATION OF
C   THICKNESSES BETWEEN 300-200 MB LEVELS.

         ML=11
         MU=12
         GO TO 620

  600    CONTINUE

C  DEFINE ML AND MU FOR SUMMATION OF THICKNESSES BETWEEN 200-100 MB LVLS

         ML=9
         MU=10
         GO TO 620

  610    CONTINUE

C  DEFINE ML AND MU FOR SUMMATION OF THICKNESSES BETWEEN 30-10 MB LEVELS

         ML=4
         MU=5

  620    CONTINUE

C  INITIALIZE SUMMATION OF THICKNESSES.

         SUMTHK=0.

C  SUM THICKNESSES.

         DO  L = ML,MU
            THK=GPHGT(L)-GPHGT(L+1)
            SUMTHK=SUMTHK+THK
         ENDDO
         THK=SUMTHK

C  RESET POINTER(M) TO MANDATORY PRESSURE LEVEL FOR NEXT THICKNESS
C   CALCULATION.

         M=M-1
         GO TO 700

  640    CONTINUE

C  CHECK FOR SPECIAL CASES WHEN RTOVS/ATOVS RETRIEVAL TEMPERATURES ARE
C   REQUIRED TO COMPUTE LAYER-MEAN TEMPERATURES.

         IF(I.EQ.LNMC8) GO TO 650
         IF(I.EQ.LNMC9) GO TO 660
         IF(I.EQ.LNMC10) GO TO 670
         JJ=NPNMC-1
         IF(I.EQ.JJ) GO TO 680
         GO TO 690

  650    CONTINUE

C  COMPUTE LAYER-MEAN TEMPERATURE BETWEEN 10-5 MB LEVELS.

         TKLNMC(I)=C0*TMPOPR_8(11)+C1*TMPOPR_8(10)+C2*TMPOPR_8(9)
         GO TO 710

  660    CONTINUE

C  COMPUTE LAYER-MEAN TEMPERATURE BETWEEN 5-2 MB LEVELS.

         TKLNMC(I)=C9*TMPOPR_8(9)+C10*TMPOPR_8(8)+C11*TMPOPR_8(7)+
     $     C12*TMPOPR_8(6)
         GO TO 710

  670    CONTINUE

C  COMPUTE LAYER-MEAN TEMPERATURE BETWEEN 2-1 MB LEVELS.

         TKLNMC(I)=C3*TMPOPR_8(6)+C4*TMPOPR_8(5)+C5*TMPOPR_8(4)
         GO TO 710

  680    CONTINUE

C  INTERPOLATE RTOVS/ATOVS RETRIEVAL TEMPERATURE TO 0.4 MB LEVEL.

         TEMP=TMPOPR_8(3)+C13*(TMPOPR_8(2)-TMPOPR_8(3))

C  CALCULATE LAYER-MEAN TEMPERATURE BETWEEN 1-0.4 MB LEVEL.

         TKLNMC(I)=C6*TMPOPR_8(4)+C7*TMPOPR_8(3)+C8*TEMP
         GO TO 710

  690    CONTINUE

C  COMPUTE GEOPOTENTIAL THICKNESSES.

         THK=GPHGT(M-1)-GPHGT(M)

  700    CONTINUE

C  COMPUTE LAYER-MEAN TEMPERATURE USING HYPSOMETRIC EQUATION.

         TKLNMC(I)=-C14*THK*(1./ALOG(PNMC(J)/PNMC(J+1)))

  710    CONTINUE

C  RESET POINTER(M) TO MANDATORY PRESSURE LEVEL FOR NEXT THICKNESS
C   CALCULATION.

         M=M-1
      ENDDO

      RETURN

      END
