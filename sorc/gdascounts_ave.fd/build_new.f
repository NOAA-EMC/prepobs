C$$$  MAIN PROGRAM DOCUMENTATION BLOCK
C  
C MAIN PROGRAM:  BUILD
C   PRGMMR: Bill FACEY        ORG: WX12        DATE: 2003-04-9
C
C ABSTRACT: PROGRAM READS DATA COUNTS FROM THE 00Z, 06Z, 12Z, AND 
C           18Z GDAS CREATED BY THE SCRIPTS 18Z.  PROGRAM USES THIS 
C           DATA TO CREATE MONTHLY STATISTICS FOR INPUT TO A
C           SCRIPT THAT BUILDS AN HTML FILE.  THE STATISTICS ARE 
C           BASED UPON DOCUMENTATION THAT CAN BE FOUND AT
C           http://www.emc.ncep.noaa.gov/mmb/papers/keyser/
C           data dumping.doc/
C           User Guide to Interpreting Data Dump Counts.html 
C
C PROGRAM HISTORY LOG:
C   03-04-09  bill facey
C
C USAGE:
C   INPUT FILES:
C     UNIT 10  - 00Z DATA COUNTS FROM RUNSTAT.SH
C     UNIT 11  - 06Z DATA COUNTS FROM RUNSTAT.SH
C     UNIT 13  - 12Z DATA COUNTS FROM RUNSTAT.SH
C     UNIT 14  - 18Z DATA COUNTS FROM RUNSTAT.SH
C
C   OUTPUT FILES: 
C     UNIT 50  - MONTHLY STATISTICS.
C     UNIT 06  - UNIT 6 (STANDARD PRINTFILE)
C
C   WORK FILES:  (INCLUDING SCRATCH FILES)
C     UNIT XX  - UNITS 80 THRU 99 
C
C   SUBPROGRAMS CALLED: (LIST ALL CALLED FROM ANYWHERE IN CODES)
C     UNIQUE:    - NONE
C
C   EXIT STATES:
C     COND =   0 - SUCCESSFUL RUN
C
C REMARKS: NONE
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77
C   MACHINE:  IBM SP
C
C$$$
          INTEGER DUMP
          REAL SYNOP,METAR,SHIPS,DBUOY,MBUOY,LCMAN,TIDEG,
     1    MSLPB,RAOBF,RAOBM,RAOBS,DROPW,PIBAL,PRFL1,PRFL2,NXRDW,GEOST,
     1    ATOVS,AIREP,PIREP,ASDAR,ACARS,RECCO,INFUS,VISUS,PTRUS,
     1    INFJA,VISJA,WVIJA,INFEU,OSBUV,SSMIT,SSMIP,
     1    TRMM,TRMSO,SSMIN,QKSCA,QKSRE,HIRS2,MSU,AMSUA,AMSUB,HIRS3,
     1    GEOIM,SSMIC
          DATA SYNOP,METAR,SHIPS,DBUOY,MBUOY,LCMAN,TIDEG,
     1    MSLPB,RAOBF,RAOBM,RAOBS,DROPW,PIBAL,PRFL1,PRFL2,NXRDW,GEOST,
     1    ATOVS,AIREP,PIREP,ASDAR,ACARS,RECCO,INFUS,VISUS,PTRUS,
     1    INFJA,VISJA,WVIJA,INFEU,OSBUV,SSMIT,SSMIP,
     1    TRMM,TRMSO,SSMIN,QKSCA,QKSRE,HIRS2,MSU,AMSUA,AMSUB,HIRS3,
     1    GEOIM,SSMIC /45*0./
          INTEGER IUNIT(4)
          INTEGER IVAL(6,59)
          DATA IUNIT/10,11,12,13/
          NAMELIST/DATA/SYNOP,METAR,SHIPS,DBUOY,MBUOY,LCMAN,TIDEG,
     1    MSLPB,RAOBF,RAOBM,RAOBS,DROPW,PIBAL,PRFL1,PRFL2,NXRDW,GEOST,
     1    ATOVS,AIREP,PIREP,ASDAR,ACARS,RECCO,INFUS,VISUS,PTRUS,
     1    INFJA,VISJA,WVIJA,INFEU,OSBUV,SSMIT,SSMIP,
     1    TRMM,TRMSO,SSMIN,QKSCA,QKSRE,HIRS2,MSU,AMSUA,AMSUB,HIRS3,
     1    GEOIM,SSMIC
          CHARACTER*10 CHAROUT(6,59)
          CHARACTER*1 EQUAL
          DATA EQUAL /'='/
          CHARACTER*10 WORDOUT
          DATA CHAROUT/ 
     1    'syn00z=','syn06z=','syn12z=','syn18z=','syntot=','syninp=',
     2    'metar00z=','metar06z=','metar12z=','metar18z=','metartot=',
     2    'metarinp=',
     2    'lz00z=','lz06z=','lz12z=','lz18z=','lztot=','lzinp=',
     2    'ship00z=','ship06z=','ship12z=','ship18z=','shiptot=',
     2    'shipinp=',
     2    'dbuoy00z=','dbuoy06z=','dbuoy12z=','dbuoy18z=','dbuoytot=',
     2    'dbuoyinp=',
     2    'mbuoy00z=','mbuoy06z=','mbuoy12z=','mbuoy18z=','mbuoytot=',
     2    'mbuoyinp=',
     2    'cman00z=','cman06z=','cman12z=','cman18z=','cmantot=',
     2    'cmaninp=',
     2    'tideg00z=','tideg06z=','tideg12z=','tideg18z=','tidegtot=',
     2    'tideginp=',
     2    'mslpb00z=','mslpb06z=','mslpb12z=','mslpb18z=','mslpbtot=',
     2    ' ',
     2    'mar00z=','mar06z=','mar12z=','mar18z=','martot=','marinp=',
     2    'fxr00z=','fxr06z=','fxr12z=','fxr18z=','fxrtot=','fxrinp=',
     2    'mlr00z=','mlr06z=','mlr12z=','mlr18z=','mlrtot=','mlrinp=',
     2    'shp00z=','shp06z=','shp12z=','shp18z=','shptot=','shpinp=',
     2    'drop00z=','drop06z=','drop12z=','drop18z=','droptot=',
     2    'dropinp=',
     2    'pibal00z=','pibal06z=','pibal12z=','pibal18z=','pibaltot=',
     2    'pibalinp=',
     2    'prof00z=','prof06z=','prof12z=','prof18z=','proftot=',
     2    'profinp=',
     2    'nexrd00z=','nexrd06z=','nexrd12z=','nexrd18z=','nexrdtot=',
     2    'nexrdinp=',
     2    'lsnd00z=','lsnd06z=','lsnd12z=','lsnd18z=','lsndtot=',
     2    'lsndinp=',
     2    'air00z=','air06z=','air12z=','air18z=','airtot=','airinp=',
     2    'pir00z=','pir06z=','pir12z=','pir18z=','pirtot=','pirinp=',
     2    'amdr00z=','amdr06z=','amdr12z=','amdr18z=','amdrtot=',
     2    'amdrinp=',
     2    'acar00z=','acar06z=','acar12z=','acar18z=','acartot=',
     2    'acarinp=',
     2    'rec00z=','rec06z=','rec12z=','rec18z=','rectot=','recinp=',
     2    'acft00z=','acft06z=','acft12z=','acft18z=','acfttot=',
     2    'acftinp=',
     2    'nons00z=','nons06z=','nons12z=','nons18z=','nonstot=',
     2    'nonsinp=',
     2    'goes00z=','goes06z=','goes12z=','goes18z=','goestot=',
     2    'goesinp=',
     2    'atov00z=','atov06z=','atov12z=','atov18z=','atovtot=',
     2    ' ',
     2    'oz00z=','oz06z=','oz12z=','oz18z=','oztot=','ozinp=',
     2    'sat00z=','sat06z=','sat12z=','sat18z=','sattot=','satinp=',
     2    'usir00z=','usir06z=','usir12z=','usir18z=','usirtot=',
     2    'usirinp=',
     2    'usvi00z=','usvi06z=','usvi12z=','usvi18z=','usvitot=',
     2    ' ',
     2    'usp300z=','usp306z=','usp312z=','usp318z=','usp3tot=',
     2    'usp3inp=',
     2    'jair00z=','jair06z=','jair12z=','jair18z=','jairtot=',
     2    'jairinp=',
     2    'javi00z=','javi06z=','javi12z=','javi18z=','javitot=',
     2    'javiinp=',
     2    'jawv00z=','jawv06z=','jawv12z=','jawv18z=','jawvtot=',
     2    ' ',
     2    'eur00z=','eur06z=','eur12z=','eur18z=','eurtot=','eurinp=',
     2    'swnd00z=','swnd06z=','swnd12z=','swnd18z=','swndtot=',
     2    'swndinp=',
     2    'ssmp00z=','ssmp06z=','ssmp12z=','ssmp18z=','ssmptot=',' ',
     2    'smps00z=','smps06z=','smps12z=','smps18z=','smpstot=',
     2    'smpsinp=',
     2    'ssmt00z=','ssmt06z=','ssmt12z=','ssmt18z=','ssmttot=',' ',
     2    'ssmn00z=','ssmn06z=','ssmn12z=','ssmn18z=','ssmntot=',' ',
     2    'smns00z=','smns06z=','smns12z=','smns18z=','smnstot=',
     2    'smnsinp=',
     2    'dmsp00z=','dmsp06z=','dmsp12z=','dmsp18z=','dmsptot=',' ',
     2    'dmin00z=','dmin06z=','dmin12z=','dmin18z=','dmintot=',
     2    'dmininp=',
     2    'trm00z=','trm06z=','trm12z=','trm18z=','trmtot=',' ',
     2    'trmso00z=','trmso06z=','trmso12z=','trmso18z=','trmsotot=',
     2    'trmsoinp=',
     2    'qks00z=','qks06z=','qks12z=','qks18z=','qkstot=',' ',
     2    'qksqc00z=','qksqc06z=','qksqc12z=','qksqc18z=','qksqctot=',
     2    'qksqcinp=',
     2    'tsfc00z=','tsfc06z=','tsfc12z=','tsfc18z=','tsfctot=',
     2    ' ',
     2    'ssfc00z=','ssfc06z=','ssfc12z=','ssfc18z=','ssfctot=',
     2    'ssfcinp=',
     2    'hirs200z=','hirs206z=','hirs212z=','hirs218z=','hirs2tot=',
     2    'hirs2inp=',
     2    'hirs300z=','hirs306z=','hirs312z=','hirs318z=','hirs3tot=',
     2    'hirs3inp=',
     2    'msu00z=','msu06z=','msu12z=','msu18z=','msutot=','msuinp=',
     2    'msua00z=','msua06z=','msua12z=','msua18z=','msuatot=',
     2    'msuainp=',
     2    'msub00z=','msub06z=','msub12z=','msub18z=','msubtot=',
     2    'msubinp=',
     2    'geor00z=','geor06z=','geor12z=','geor18z=',
     2    'geortot=','georinp=',
     2    'rsat00z=','rsat06z=','rsat12z=','rsat18z=','rsattot=',
     2    'rsatinp=',
     2    'satusd00z=','satusd06z=','satusd12z=','satusd18z=',
     2    'satusdtot=',' ',
     2    'satinp00z=','satinp06z=','satinp12z=','satinp18z=',
     2    'satinptot=','satinpinp='/
          CHARACTER*10 PERCENT(11)
          DATA PERCENT/'prcntls=','prcntmr=','prcntlsnd=',
     1                 'prcntacft=','prcntnsat=','prcntss=',
     1                 'swndprcnt=','dmspprcnt=','ssfcprcnt=',
     1                 'prcntrad=','prcntsat='/
          DO 20 I=1,4
          LUNIT=IUNIT(i)
C --- READ MONTHLY STATS FOR EACH CYCLE AND STORE IN IVAL.  IVAL HAS 
C     VALUE FOR EACH LINE IN THE TABLE DISPLAYED IN THE HTML FILE.
          READ (LUNIT,DATA)
          IVAL(I,1)=SYNOP+0.5
          IVAL(I,2)=METAR +0.5
          IVAL(I,3)=IVAL(I,1)+IVAL(I,2)
          IVAL(I,4)=SHIPS + 0.5
          IVAL(I,5)=DBUOY + 0.5
          IVAL(I,6)=MBUOY + 0.5
          IVAL(I,7)=LCMAN + 0.5
          IVAL(I,8)=TIDEG + 0.5
          IVAL(I,9)=MSLPB + 0.5
          IVAL(I,10)=IVAL(I,4)+IVAL(I,5)+IVAL(I,6)+IVAL(I,7)+IVAL(I,8)
          IVAL(I,11)=RAOBF + 0.5
          IVAL(I,12)=RAOBM + 0.5
          IVAL(I,13)=RAOBS + 0.5
          IVAL(I,14)=DROPW + 0.5
          IVAL(I,15)=PIBAL + 0.5
          IVAL(I,16)=PRFL1 + PRFL2 + 0.5
          IVAL(I,17)=NXRDW + 0.5
          IVAL(I,18)=IVAL(I,11)+IVAL(I,12)+IVAL(I,13)
     a              +IVAL(I,14)+IVAL(I,15)+IVAL(I,16)+IVAL(I,17)
          IVAL(I,19)=AIREP+0.5
          IVAL(I,20)=PIREP+0.5
          IVAL(I,21)=ASDAR+0.5
          IVAL(I,22)=ACARS+0.5
          IVAL(I,23)=RECCO+0.5
          IVAL(I,24)=IVAL(I,19)+IVAL(I,20)+IVAL(I,21)+IVAL(I,22)
     1              +IVAL(I,23)
          IVAL(I,25)=IVAL(I,3)+IVAL(I,10)+IVAL(I,18)+IVAL(I,24)
          IVAL(I,26)=GEOST + 0.5
          IVAL(I,27)=ATOVS + 0.5
          IVAL(I,28)=OSBUV + 0.5
          IVAL(I,29)=IVAL(I,26)+IVAL(I,27)
          IVAL(I,30)=INFUS+0.5
          IVAL(I,31)=VISUS+0.5
          IVAL(I,32)=PTRUS+0.5
          IVAL(I,33)=INFJA+0.5
          IVAL(I,34)=VISJA+0.5
          IVAL(I,35)=WVIJA+0.5
          IVAL(I,36)=INFEU+0.5
          IVAL(I,37)=IVAL(I,30)+IVAL(I,32)+IVAL(I,33)
     1              + IVAL(I,34)+IVAL(I,35)+IVAL(I,36) 
          IVAL(I,38)=(SSMIP+0.5)*0.66*56
          IVAL(I,39)=SSMIC*0.5+0.5
          IVAL(I,40)=(SSMIT+0.5)*0.66*63
          IVAL(I,41)=(SSMIN+0.5)*0.66*31  
          IVAL(I,42)=(SSMIC+0.5)*0.5
          IVAL(I,43)=IVAL(I,38)+IVAL(I,41)
          IVAL(I,44)=IVAL(I,39)+IVAL(I,42)
          IVAL(I,45)=TRMM+0.5
          IVAL(I,46)=TRMSO+0.5
          IVAL(I,47)=QKSCA+0.5
          IVAL(I,48)=QKSRE+0.5
          IVAL(I,49)=IVAL(I,45)+IVAL(I,47)
          IVAL(I,50)=IVAL(I,46)+IVAL(I,48)
          IVAL(I,51)=HIRS2+0.5
          IVAL(I,52)=HIRS3+0.5
          IVAL(I,53)=MSU+0.5
          IVAL(I,54)=AMSUA+0.5
          IVAL(I,55)=AMSUB+0.5
          IVAL(I,56)=GEOIM+0.5
          IVAL(I,57)=IVAL(I,51)+IVAL(I,52)+IVAL(I,53)+IVAL(I,54)
     1              +IVAL(I,55)+IVAL(I,56)
          IVAL(I,58)=IVAL(I,29)+IVAL(I,37)+IVAL(I,43)+IVAL(I,49)
     1              +IVAL(I,57)
          IVAL(I,59)=IVAL(I,27)+IVAL(I,28)+IVAL(I,37)+IVAL(I,44)
     1              +IVAL(I,50)+IVAL(I,57)
  20      CONTINUE
C --- CREATE COLUMN 5.  THESE ARE THE GROSS TOTALS FOR EACH DATA TYPE.
          DO 30 I=1,59
            IVAL(5,I)=IVAL(1,I)+IVAL(2,I)+IVAL(3,I)+IVAL(4,I)
  30      CONTINUE
C --- CREATE COLUMN 6.  THESE ARE THE ACTUAL AMOUNTS INPUT TO THE
C     ANALYSIS.
           DO 40 I=1,59
            IF(CHAROUT(6,I).NE.' ')THEN
              IVAL(6,I)=IVAL(1,I)+IVAL(2,I)+IVAL(3,I)+IVAL(4,I)
            ENDIF
  40      CONTINUE
C --- THESE LINES DO NOT FOLLOW THE GENERAL RULE AND NEED TO BE 
C     ADJUSTED.
          IVAL(6,10)=IVAL(6,4)+IVAL(6,5)+IVAL(6,6)+IVAL(6,7)+IVAL(6,8)
          IVAL(6,29)=IVAL(6,26)+IVAL(6,28)
          IVAL(6,37)=IVAL(6,30)+IVAL(6,32)+IVAL(6,33)
     1              + IVAL(6,34)+IVAL(6,35)+IVAL(6,36) 
          IVAL(6,59)=IVAL(6,27)+IVAL(6,28)+IVAL(6,37)+IVAL(6,44)
     1              +IVAL(6,50)+IVAL(6,57)
C --- JOIN LABELS AND VALUES AND WRITE OUT.
          DO 50 I=1,59
            WORDOUT=CHAROUT(1,I)
            J=INDEX(WORDOUT,EQUAL)
            WRITE(50,*)WORDOUT(1:J),IVAL(1,I)
            WORDOUT=CHAROUT(2,I)
            J=INDEX(WORDOUT,EQUAL)
            WRITE(50,*)WORDOUT(1:J),IVAL(2,I)
            WORDOUT=CHAROUT(3,I)
            J=INDEX(WORDOUT,EQUAL)
            WRITE(50,*)WORDOUT(1:J),IVAL(3,I)
            WORDOUT=CHAROUT(4,I)
            J=INDEX(WORDOUT,EQUAL)
            WRITE(50,*)WORDOUT(1:J),IVAL(4,I)
            WORDOUT=CHAROUT(5,I)
            J=INDEX(WORDOUT,EQUAL)
            WRITE(50,*)WORDOUT(1:J),IVAL(5,I)
            WORDOUT=CHAROUT(6,I)
            J=INDEX(WORDOUT,EQUAL)
            IF(J.GT.1) THEN
              WRITE(50,*)WORDOUT(1:J),IVAL(6,I)
            ENDIF
  50      continue
C  CALCULATE percents 
          P1=IVAL(6,3)
          print *, 'P1=',P1
          P2=(IVAL(6,24)+IVAL(6,59))
          print *, 'P2=',P2
          PRCNT=(P1/P2)*100.
          J=INDEX(PERCENT(1),EQUAL)
          WRITE(50,99)PERCENT(1)(1:J),PRCNT
          P1=IVAL(6,10)
          PRCNT=(P1/P2)*100.
          J=INDEX(PERCENT(2),EQUAL)
          WRITE(50,99)PERCENT(2)(1:J),PRCNT
          P1=IVAL(6,18)
          PRCNT=(P1/P2)*100.
          J=INDEX(PERCENT(3),EQUAL)
          WRITE(50,99)PERCENT(3)(1:J),PRCNT
          P1=IVAL(5,24)
          PRCNT=(P1/P2)*100.
          J=INDEX(PERCENT(4),EQUAL)
          WRITE(50,99)PERCENT(4)(1:J),PRCNT
          P1=IVAL(6,25)
          PRCNT=(P1/P2)*100.
          J=INDEX(PERCENT(5),EQUAL)
          WRITE(50,99)PERCENT(5)(1:J),PRCNT
          P1=IVAL(5,29)
          PRCNT=(P1/P2)*100.
          J=INDEX(PERCENT(6),EQUAL)
          WRITE(50,99)PERCENT(6)(1:J),PRCNT
          P1=IVAL(6,37)
          PRCNT=(P1/P2)*100.
          J=INDEX(PERCENT(7),EQUAL)
          WRITE(50,99)PERCENT(7)(1:J),PRCNT
          P1=IVAL(6,44)
          PRCNT=(P1/P2)*100.
          J=INDEX(PERCENT(8),EQUAL)
          WRITE(50,99)PERCENT(8)(1:J),PRCNT
          P1=IVAL(6,50)
          PRCNT=(P1/P2)*100.
          J=INDEX(PERCENT(9),EQUAL)
          WRITE(50,99)PERCENT(9)(1:J),PRCNT
          P1=IVAL(6,57)
          PRCNT=(P1/P2)*100.
          J=INDEX(PERCENT(10),EQUAL)
          WRITE(50,99)PERCENT(10)(1:J),PRCNT
          P1=IVAL(6,59)
          PRCNT=(P1/P2)*100.
          J=INDEX(PERCENT(11),EQUAL)
          WRITE(50,99)PERCENT(11)(1:J),PRCNT
 99      FORMAT(' ',A,f10.7)
          STOP 
          END

         
