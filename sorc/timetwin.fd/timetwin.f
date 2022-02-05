C$$$  MAIN PROGRAM DOCUMENTATION BLOCK
C
C MAIN PROGRAM: TIMETWIN
C   PRGMMR: LILLY            ORG: NP12        DATE: 2004-04-15
C
C ABSTRACT: FINDS MODEL BUFR WINDS FOR UPPER AIR SITES AND SORTS
C           THEM BEFORE OUTPUT TO BE USED IN THE TIMETWIN CODE
C           WHICH IS LOOKING FOR CURRENT UPPER AIR DATA THAT ARE
C           REALLY DUPLICATES OF PAST DATA

C PROGRAM HISTORY LOG:
C
C 1999-09-15 BRADLEY BALLISH -- ORIGINAL AUTHOR
C 2004-07-16 STEVEN G. LILLY -- ADD SUBROUTINE TWINWIND_HIS_CHECK
C                               AND MOLLIFY/UPDATE SOURCE CODES
C 2006-02-22 BRADLEY BALLISH -- CHANGE OBSERVATION TIME TO BE THE
C                               RAW REPORTED TIME USED IN ARRAY RTIM
C
C USAGE:
C   INPUT FILES:
C     UNIT 10  - GLOBAL MODEL PREPBUFR FILE
C
C   OUTPUT FILES:
C     UNIT 06  - STANDARD OUTPUT PRINT
C     UNIT 61  - FILE CONTAINING CURRENT MODEL UPPERAIR WIND DATA FOR PART TTAA 
C     UNIT 62  - FILE CONTAINING CURRENT MODEL UPPERAIR WIND DATA FOR PART TTBB 
C     UNIT 63  - FILE CONTAINING CURRENT MODEL UPPERAIR WIND DATA FOR PART TTCC 
C     UNIT 64  - FILE CONTAINING CURRENT MODEL UPPERAIR WIND DATA FOR PART TTDD 
C     UNIT 65  - FILE CONTAINING CURRENT MODEL UPPERAIR WIND DATA FOR PART PPBB 
C     UNIT 66  - FILE CONTAINING CURRENT MODEL UPPERAIR WIND DATA FOR PART PPDD 

C   SUBPROGRAMS CALLED:
C     UNIQUE:  - TWINWIND_HIS_CHECK PARTID INDEX41 TWINWIND
C     LIBRARY
C       W3LIB    - DATELEN IW3JDN  W3TAGB W3FC05  W3TAGE   
C       BUFRLIB  - OPENBF READMG READSB UFBINT CLOSBF
C
C   EXIT STATES:
C     COND =   0 - SUCCESSFUL RUN
C
C REMARKS: OUTPUT WINDS ARE BINARY VERSIONS OF ASCII DATA AND ARE SORTED.
C          CURRENT WIND DATA ARE WRITTEN TO COMMON BLOCK/CURRPART/
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE : IBM-SP
C
C$$$

      PROGRAM TIMETWIN
                                                                                
C     PROGRAM TIMETWIN  BY  B. BALLISH                                             
C                                                                               

C     READ PREPBUFR FILE TO GET  ADPUPA  WINDS
                                                                                
      INTEGER       STNID , DATE                                                
      CHARACTER*41 CARD(6,100000),CARDS(6,960000),CARDT(100000)
      CHARACTER*41 HCARDS(6,960000),CCARDS(6,960000)
      CHARACTER*40 HSTR  , TSTR , VWND                                         
      CHARACTER*18 SUID(90000),UID(90000)
      CHARACTER*10 CDATE 
      CHARACTER*8  SUBSET, SID , CSID, IDS(900)
      CHARACTER*8  CDAT8
      CHARACTER*7  IDOLD
      CHARACTER*2  TTXX(6)
      CHARACTER*1   NORS,EORW
      COMMON /HISTPART/HCARDS,IIMAX(6)
      COMMON /CURRPART/CCARDS,JJMAX(6)
      COMMON /PMAXS/IMAX,JMAX,NP


      DIMENSION  HDR(7), SFC(8,255), IIDATE(5), WFC(7,255),WND(4,255)           
      DIMENSION  IGSD(90000),IGSS(90000),IOBD(90000),IOBS(90000)
      DIMENSION  JOBD(90000),JOBS(90000)
      DIMENSION  ITM(900),IQM(90000),PRS(90000),ISTOP(900),ISTART(900)
      DIMENSION  JQM(90000),PRT(90000)
      DIMENSION  FLON(900),FLAT(900),ELEV(900)
      DIMENSION  WDIF(90000),RTIM(90000),INDR(90000)
      DIMENSION  JCON(6,255),JTYP(6),IM(6)
      EQUIVALENCE  (HDR(1),CSID)                                               
                                                                                
      DATA  HSTR /'SID XOB YOB RPT ELV T29 TYP'/                                
      DATA  VWND /' UOB VOB WQM UFC VFC POB CAT '/                             
      DATA  XMSG /10E10/, INFILE/10/

C     CALL W3TAGB('TIMETWIN',??,??,??,'NP12')
      PRINT *, ' '
      PRINT *, '=====> WELCOME TO PROGRAM TIMETWIN - ',
     $ 'VERSION: 07/16/2004'

      CALL DATELEN(10) 
C     TTXX holds abreviations for the 6 upper air parts used
C     TA is short for TTAA PD is short for PPDD
      TTXX(1)='TA'
      TTXX(2)='TB'
      TTXX(3)='TC'
      TTXX(4)='TD'
      TTXX(5)='PB'
      TTXX(6)='PD'
      IM(1)=0
      IM(2)=0
      IM(3)=0
      IM(4)=0
      IM(5)=0
      IM(6)=0
      FCT=1.98
C     SET COUNTS TO ZERO
      ISTA=0
      IW=0

C     READ DATA FROM PREVIOUS CYCLE INTO COMMON/HISTPART/ and
C     CHECK THE STATUS OF THE PREVIOUS SET(S) OF WIND DATA.
      CALL TWINWIND_HIS_CHECK
C     IF FILE IS MISSING OR CORRUPT  IT DOES ERREXIT 33

                                                                                
C     OPEN THE BUFR FILE ...
                                                                                
      CALL OPENBF(INFILE,'IN',INFILE)                                           
                                                                                
C     FIND SUBSET 'ADPUPA' ...                                                  
                                                                                
    1 CONTINUE                                                                  
      CALL READMG(INFILE,SUBSET,IDATE,IRET)                                     
      IF(SUBSET.NE.'ADPUPA') THEN                                               
         IF(IRET.NE.0) GO TO 999                                                
         GO TO 1                                                                
      ENDIF                                                                     
                                                                                
C     SPLIT DATE UP INTO COMPONENTS                                             
                                                                                
      WRITE (CDATE, '(I10)')  IDATE      
      CDAT8(1:8)=CDATE(3:10)                                        
      READ  (CDATE, '(5I2)') IIDATE                                             
                                                                                
C     NOW READ THE SUBSET (ADPUPA)                                              
                                                                                
   10 CONTINUE                                                                  
      CALL READSB(INFILE,IRET)                                                  
      IF(IRET.NE.0) THEN                                                        
   20    CONTINUE                                                               
         CALL READMG(INFILE,SUBSET,IDATE,IRET)                                  
         IF(SUBSET.NE.'ADPUPA') THEN                                            
            IF(IRET.NE.0) GO TO 999                                             
            GO TO 20                                                            
         ENDIF                                                                  
            GO TO 10                                                            
      ENDIF                                                                     
                                                                                
C     NOW GET THE DATA ...                                                      
                                                                                
      CALL UFBINT(INFILE,HDR,7,1,IRET,HSTR)                                     
      IRTP = IFIX(HDR(6))                                                       
      ITYP = IFIX(HDR(7))                                                       
c     WRITE(25,158) IRTP,ITYP
 158  FORMAT(' IRTP ITYP= ',2I6)
      JMATCH=0
      IF(ITYP .EQ. 220 .OR. ITYP .EQ. 221 .OR. ITYP .EQ. 232 ) THEN
      CALL UFBINT(INFILE,WFC,7,255,IRET,VWND) 
c     DATA  VWND /' UOB VOB WQM UFC VFC POB CAT '/                             
c     WRITE(25,159) IRET
 159  FORMAT(' IRET= ',I6)
      ISTA=ISTA+1                                    
       FLON(ISTA) = HDR(2)                                                             
       FLAT(ISTA) = HDR(3)                                                             
       ELEV(ISTA) = HDR(5)                                                             
       IDS(ISTA)=CSID
C     DATA  VWND /' UOB VOB WQM UFC VFC POB CAT '/ 

       DO 44 KZ=1,6
       JTYP(KZ)=0

       DO 45 KL=1,255
       JCON(KZ,KL)=0
 45    CONTINUE

 44    CONTINUE

       IW=0

       DO 46 KL=1,IRET
       IWQM=IFIX(WFC(3,KL))                                                         
       ICAT=IFIX(WFC(7,KL))                                                         
       CALL W3FC05(WFC(1,KL),WFC(2,KL),WND(1,KL),WND(2,KL))                                 
       CALL W3FC05(WFC(4,KL),WFC(5,KL),WND(3,KL),WND(4,KL)) 
       UOB=WFC(1,KL)                                
       VOB=WFC(2,KL)                                
       UGS=WFC(4,KL)                                
       VGS=WFC(5,KL)
       IF(ABS(UOB) .LT. 5.0E+10 .AND. ABS(VOB) .LT. 5.0E+10) THEN 
       IW=IW+1
       WDIF(IW)=FCT*SQRT((UOB-UGS)**2 + (VOB-VGS)**2)
       PRS(IW)=WFC(6,KL)
       RTIM(IW)=HDR(4) - FLOAT(IIDATE(5))
       CALL PARTID(UID,CSID,PRS,RTIM,ICAT,IW,ISTA)
 444   FORMAT(A18)

       DO 47 KZ=1,6
       IF(UID(IW)(1:2) .EQ. TTXX(KZ)) THEN
C JTYP(KZ) is 1 for upper air type KZ=1,6
C JCON(KZ,IW) is one for upper air type KZ and the IWth wind
       JTYP(KZ)=JTYP(KZ)+1
       JCON(KZ,IW)=1
       ENDIF 
 47    CONTINUE

       IQM(IW)=INT(WFC(3,KL))
C Make sure the direction is correct to proper 5 degree choice
       IOBD(IW)=5*INT((WND(1,KL)+1.0)/5.0)
       IGSD(IW)=INT(WND(3,KL))
       IGSS(IW)=INT(FCT*WND(4,KL))
       IOBS(IW)=INT(FCT*WND(2,KL))
       ENDIF                                                                    
  46   CONTINUE

       DO 48 KZ=1,6

       IF(JTYP(KZ) .GT. 0) THEN

       DO 49 KL=1,IW

       IF(JCON(KZ,KL) .GT. 0) THEN
       IM(KZ)=IM(KZ)+1
       WRITE(CARD(KZ,IM(KZ)),137) UID(KL)(3:7),PRS(KL),IOBD(KL),
     & IOBS(KL),WDIF(KL),IQM(KL),CDAT8,UID(KL)(8:9),JTYP(KZ)
 137   FORMAT(A5,' ',F6.1,' ',2I3,F5.1,' ',I2,' ',A8,A2,I3)
       ENDIF

  49   CONTINUE

       ENDIF

  48   CONTINUE

       ENDIF
                                                                                
      GO TO 10                                                                  
                                                                                
C     COME HERE WHEN ALL REPORTS HAVE BEEN READ IN -- CLOSE FILE                
C     AND QUIT.                                                                 
                                                                                
  999 CONTINUE                                                                  
      CALL CLOSBF(INFILE)                                                       
      PRINT 102, IIDATE                                                         
 101  FORMAT(I5)                                                                
 102  FORMAT(' ADPUPA EXTRACTED FOR', 4I3)

      DO 68 KZ=1,6
      MOUT=KZ+60

      DO 67 KJ=1,IM(KZ)
      CARDT(KJ)=CARD(KZ,KJ)
 67   CONTINUE

C Do heap sort on array CARDT, store sorted locations in INDR
      CALL INDEX41(IM(KZ),CARDT,INDR)

      DO 69 I=1,IM(KZ)
      CARDS(KZ,I)=CARDT(INDR(I))
      CCARDS(KZ,I)=CARDT(INDR(I))
 69   CONTINUE

C CARDS is now sorted
C CARDS is CHARACTER*41 and holds
C 1:5 is the site ID, 7:12 the pressure, 14:16 the wind direction of the observation
C 17:19 the observation speed in knots, 20:24 the vector difference OB - GES
C 26:27 the wind quality mark, 29:36 the last 6 characters of the date
C that is 00Z 17 March 2004 is 04031700
C 37:38 are the relative time of the observation hour plus 3
C that is 00Z is 03 02z is 05 19Z is 04 etc
C 39:41 is the number of winds in that part, a maximum of 255 is allowed

      WRITE(MOUT) IM(KZ)
      JJMAX(KZ)=IM(KZ)
      WRITE(MOUT) (CARDS(KZ,KL),KL=1,IM(KZ))
c     WRITE(MOUT,105) (CARDS(KZ,KL),KL=1,IM(KZ))
 105  FORMAT(A)
 68   CONTINUE

C      CALL TWINWIND TO COMPARES THE CURRENT WINDS WITH THE LAST 35
C      DAYS OF WINDS TO SEE IF CURRENT DATA IS REALLY OLD
C      IT USES AN INCREMENTAL SORT (INCSORT) TO MERGE ALREADY
C      SORTED OLD AND NEW DATA WHILE FINDING INDIVUAL LEVELS OF
C      WINDS FROM THE SAME ID THAT MATCH FOR OLD AND CURRENT TIMES
C      SUBROUTINE CDUPS CHECKS ALL THESE WIND MATCHES TO SEE IF
C      TWO DIFFERENT MODEL RUNS HAVE ENOUGH WIND MATCHES TO
C      QUALIFY AS A TWINWIND
      CALL TWINWIND

C      CALL W3TAGE('TIMETWIN')
      STOP                                                                      
      END
      SUBROUTINE PARTID(UID,CSID,PRS,RTIM,ICAT,IW,ISTA)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    PARTID      DESCRIPTIVE TITLE NOT PAST COL 70
C   PRGMMR: STEVE LILLY           ORG: W/NP12   DATE: 2004-04-15
C
C ABSTRACT: DOCUMENTATION WILL BE ADDED LATER.
C
C PROGRAM HISTORY LOG:
C 1999-09-15  BRAD BALLISH DEVELOPED ORIGINAL CODE
C
C USAGE:    CALL PARTID(UID,CSID,PRS,RTIM,ICAT,IW,ISTA)
C   INPUT ARGUMENT LIST:
C     CSID is CHARACTER*8 Upper Air Station ID
C     PRS is the pressure of the wind
C     RTIM is the relative time of the report compared to main analysis time
C     ICAT is a BUFR category for the winds ICAT=0 for a surface wind etc
C     IW is an integer counter for the winds
C     ISTA is an integer counter for upper air stations
C
C   OUTPUT ARGUMENT LIST:
C     UID is a CHARACTER*18 array with location 1:2 holding an
C     abbreviation for the upper air type, location 8:9 holds
C     a time used in the timetwin code
C     location 3:7 holds the first 5 characters of the station ID
C
C
C REMARKS: LIST CAVEATS, OTHER HELPFUL HINTS OR INFORMATION
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  IBM
C
C$$$

      DIMENSION PRS(1),RTIM(1)
      CHARACTER*18 UID(900)
      CHARACTER*5 CSID    
      UID(IW)(1:2)='XX'

      IF(ICAT .EQ. 1) THEN
      IF(NINT(PRS(IW)) .GE. 100) UID(IW)(1:2)='TA'
      IF(NINT(PRS(IW)) .LT. 100) UID(IW)(1:2)='TC'
      ENDIF

      IF(ICAT .EQ. 3) THEN
      IF(NINT(PRS(IW)) .GE. 100) UID(IW)(1:2)='TB'
      IF(NINT(PRS(IW)) .LT. 100) UID(IW)(1:2)='TD'
      ENDIF

      IF(ICAT .EQ. 4) THEN
      IF(NINT(PRS(IW)) .GE. 100) UID(IW)(1:2)='PB'
      IF(NINT(PRS(IW)) .LT. 100) UID(IW)(1:2)='PD'
      ENDIF

      UID(IW)(3:7)=CSID
      JTIME=3+NINT(RTIM(IW))
      WRITE(UID(IW)(8:9),45) JTIME
 45   FORMAT(I2.2)
      IPRS=10*IFIX(PRS(IW))
      WRITE(UID(IW)(10:14),46) IPRS 
 46   FORMAT(I5.5)
      WRITE(UID(IW)(15:18),47) ISTA 
 47   FORMAT(I4.4)
      RETURN
      END

       SUBROUTINE INDEX41 (N,ARRIN,INDX)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    INDEX41      DO HEAP SORT FOR ARRAY ARRIN
C   PRGMMR: STEVE LILLY        ORG: W/NP12    DATE: 2004-04-15
C
C ABSTRACT: DOES HEAP SORT ON ARRAY AARIN WITH INDEX OF SORT
C           IN ARRAY INDX
C
C PROGRAM HISTORY LOG:
C   99-09-15 ORIGINAL AUTHOR BRAD BALLISH
C
C USAGE:    CALL INDEX41(IM(KZ),CARDT,INDR)
C   INPUT ARGUMENT LIST:
C     IM(KZ)  THE NUMBER OF ENTRIES IN ARRAY CARDT
C     CARDT   A CHARACTER*41 ARRAY HOLDING
C
C   OUTPUT ARGUMENT LIST:      (INCLUDING WORK ARRAYS)
C     INDX   A INTEGER ARRAY GIVING THE SORT RESULTS
C
C
C ATTRIBUTES:
C   LANGUAGE: f90
C   MACHINE:  IBM
C
C$$$

C                                                                       
C      INDEXS AN ARRAY ARRIN(1...N)                                     
C      INPUT:           N SPAN OF SORT                                  
C                                      ARRIN - ARRAY TO BE SORTED - NOT 
C      OUTPUT: INDX - ARRAY OF INDEXES SUCH THAT ARRAY(INDX(I)) IS      
C                                      IN ASCENDING ORDER FOR J = 1,... 
C                                                                       
       INTEGER INDX(*)                                                  
       CHARACTER*41 ARRIN(*),Q                                           
                                                                        
       DO 10 J = 1,N                                                    
           INDX(J) = J                                                  
10     CONTINUE                                                         
                                                                        
       L = N/2 + 1                                                      
                                                                        
       IR = N                                                           
33     CONTINUE                                                         
       IF (L.GT.1) THEN                                                 
           L = L - 1                                                    
           INDXT=INDX(L)                                                
           Q = ARRIN(INDXT)                                             
       ELSE                                                             
           INDXT = INDX(IR)                                             
           Q = ARRIN(INDXT)                                             
           INDX(IR) = INDX(1)                                           
           IR = IR - 1                                                  
           IF ( IR.EQ. 1) THEN                                          
               INDX(1) = INDXT                                          
               RETURN                                                   
           ENDIF                                                        
       ENDIF                                                            
                                                                        
       I = L                                                            
       J = L*2                                                          
                                                                        
30     CONTINUE                                                         
           IF ( J.LE.IR) THEN                                           
               IF (J.LT.IR.AND.ARRIN(INDX(J)).LT.ARRIN(INDX(J+1))) THEN 
                       J = J +1                                         
               ENDIF                                                    
               IF ( Q.LT.ARRIN(INDX(J)) ) THEN                          
                   INDX(I) = INDX(J)                                    
                   I = J                                                
                   J = J + I                                            
               ELSE                                                     
                   J = IR + 1                                           
               ENDIF                                                    
           ENDIF                                                        
       IF ( J.LE.IR) GOTO 30                                            
       INDX(I) = INDXT                                                  
       GOTO 33
       RETURN
       END                                                              
      SUBROUTINE TWINWIND_HIS_CHECK
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM: TWINWIND_HIS_CHECK
C   PRGMMR: LILLY            ORG: NP12        DATE: 2004-04-19
C
C ABSTRACT:  CHECKING THE STATUS OF THE PREVIOUS (-06 HR) UPPERAIR WIND DATA.
C
C PROGRAM HISTORY LOG:
C
C 1999-09-15 BRADLEY BALLISH -- ORIGINAL AUTHOR
C
C USAGE:
C   INPUT FILES:
C     UNIT 24  - FILE CONTAINING 36 DAYS OF PREVIOUS UPPERAIR WIND DATA FOR PART TTAA
C     UNIT 25  - FILE CONTAINING 36 DAYS OF PREVIOUS UPPERAIR WIND DATA FOR PART TTBB
C     UNIT 26  - FILE CONTAINING 36 DAYS OF PREVIOUS UPPERAIR WIND DATA FOR PART TTCC
C     UNIT 27  - FILE CONTAINING 36 DAYS OF PREVIOUS UPPERAIR WIND DATA FOR PART TTDD
C     UNIT 28  - FILE CONTAINING 36 DAYS OF PREVIOUS UPPERAIR WIND DATA FOR PART PPBB
C     UNIT 29  - FILE CONTAINING 36 DAYS OF PREVIOUS UPPERAIR WIND DATA FOR PART PPDD
C
C   OUTPUT FILES:
C
C   SUBPROGRAMS CALLED:
C     UNIQUE
C     LIBRARY
C
C   EXIT STATES:
C     COND =   0 - PREVIOUS UPPERAIR WIND DATA IS NON-CORRUPT
C     COND =  33 - FIRST LINE OF PREVIOUS UPPERAIR WIND DATA IS MISSING OR
C                - PREVIOUS UPPERAIR WIND DATA IS CORRUPT
C
C REMARKS: INPUT DATA FILES (UNITS 24-29) ARE PRETESTED BY SCRIPT TO BE NON-CORRUPT.
C          INPUT WINDS ARE BINARY VERSIONS OF ASCII DATA.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE : IBM-SP
C
C$$$

      CHARACTER*41 HCARDS(6,960000)
      COMMON /HISTPART/HCARDS,IIMAX(6)

      READ(24,err=910,end=910) IIIMAX
      IIMAX(1)=IIIMAX
      READ(24,err=911,end=911) (HCARDS(1,K),K=1,IIIMAX)
      READ(25,err=910,end=910) IIIMAX
      IIMAX(2)=IIIMAX
      READ(25,err=911,end=911) (HCARDS(2,K),K=1,IIIMAX)
      READ(26,err=910,end=910) IIIMAX
      IIMAX(3)=IIIMAX
      READ(26,err=911,end=911) (HCARDS(3,K),K=1,IIIMAX)
      READ(27,err=910,end=910) IIIMAX
      IIMAX(4)=IIIMAX
      READ(27,err=911,end=911) (HCARDS(4,K),K=1,IIIMAX)
      READ(28,err=910,end=910) IIIMAX
      IIMAX(5)=IIIMAX
      READ(28,err=911,end=911) (HCARDS(5,K),K=1,IIIMAX)
      READ(29,err=910,end=910) IIIMAX
      IIMAX(6)=IIIMAX
      READ(29,err=911,end=911) (HCARDS(6,K),K=1,IIIMAX)
      GO TO 999
 910  CONTINUE
      WRITE(6,*) 'First line of previous upperair wind data missing'
      CALL ERREXIT(33)
      GO TO 999
 911  CONTINUE
      WRITE(6,*) 'previous upperair wind data is corrupt'
      CALL ERREXIT(33)
 999  CONTINUE
      RETURN
      END
      SUBROUTINE TWINWIND
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM: TWINWIND
C   PRGMMR: LILLY            ORG: NP12        DATE: 2004-04-15
C
C ABSTRACT: THIS SUBROUTINE COMPARES THE CURRENT WINDS WITH THE LAST 35
C           DAYS OF WINDS TO SEE IF CURRENT DATA IS REALLY OLD
C           IT USES AN INCREMENTAL SORT (INCSORT) TO MERGE ALREADY
C           SORTED OLD AND NEW DATA WHILE FINDING INDIVUAL LEVELS OF
C           WINDS FROM THE SAME ID THAT MATCH FOR OLD AND CURRENT TIMES
C           SUBROUTINE CDUPS CHECKS ALL THESE WIND MATCHES TO SEE IF
C           TWO DIFFERENT MODEL RUNS HAVE ENOUGH WIND MATCHES TO
C           QUALIFY AS A TIMETWIN

C PROGRAM HISTORY LOG:
C
C 1999-09-15 BRADLEY BALLISH -- ORIGINAL AUTHOR
C
C USAGE:
C   INPUT FILES:
C     UNIT 05  - ONE LINE OF TEXT WITH DATE INFORMATION FROM 35 DAYS AGO AND
C              - THE CURRENT DATE
C
C   OUTPUT FILES:
C     UNIT 06  - STANDARD OUTPUT PRINT
C     UNIT 54  - FILE CONTAINING LATEST 36 DAYS OF UPPERAIR WIND DATA FOR PART TTAA 
C     UNIT 55  - FILE CONTAINING LATEST 36 DAYS OF UPPERAIR WIND DATA FOR PART TTBB 
C     UNIT 56  - FILE CONTAINING LATEST 36 DAYS OF UPPERAIR WIND DATA FOR PART TTCC 
C     UNIT 57  - FILE CONTAINING LATEST 36 DAYS OF UPPERAIR WIND DATA FOR PART TTDD 
C     UNIT 58  - FILE CONTAINING LATEST 36 DAYS OF UPPERAIR WIND DATA FOR PART PPBB 
C     UNIT 59  - FILE CONTAINING LATEST 36 DAYS OF UPPERAIR WIND DATA FOR PART PPDD 
C     UNIT 74  - FILE CONTAINING DIAGNOSTIC PRINT
C     UNIT 75  - FILE CONTAINING TIME-TWIN WIND MATCHES IF FOUND
C     UNIT 76  - DEBUG DIAGNOTICS PRINTOUTS IF TURNED ON IN FORTRAN CODE
C     UNIT 77  - DEBUG DIAGNOTICS PRINTOUTS IF TURNED ON IN FORTRAN CODE INCSORT
C   SUBPROGRAMS CALLED:
C     UNIQUE:  - INCSORT CDUPS INDEX26
C     LIBRARY
C       W3LIB    - IW3JDN  W3TAGB   W3TAGE   
C       BUFRLIB  - NONE
C
C   EXIT STATES:
C     COND =   0 - SUCCESSFUL RUN
C
C REMARKS: PREVIOUS WIND DATA ARE PRETESTED BY TWINHIS_CHECK TO BE NON-CORRUPT
C          AND ARE IN COMMON/HISTPART/ CURRENT WINDS ARE IN COMMON/CURRPART/
C          THESE WINDS ARE BINARY VERSIONS OF ASCII DATA AND ARE SORTED.
C          OUTPUT UNIT 75 INDICATES CASES WHERE THE CURRENT DATA IS REALLY A DUPLICATE
C          OF OLD DATA
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE : IBM-SP
C
C$$$


       CHARACTER*41 CARDO(800000),CARDN(70000),CARDS(960000)
       CHARACTER*41 CARDD(90000),CARD1(90000)
       CHARACTER*41 HCARDS(6,960000),CCARDS(6,960000)
       CHARACTER*8   DATE
       CHARACTER*10  DAYL2
       CHARACTER*6   DAYL
       CHARACTER*5   IDOLD,IDMAX
       CHARACTER*4   PARTS(6)
       CHARACTER*4   PART
       COMMON /HISTPART/HCARDS,IIMAX(6)
       COMMON /CURRPART/CCARDS,JJMAX(6)
       COMMON /PMAXS/IMAX,JMAX,NP


C      DATE IS THE CURRENT DATE, WHILE DAYL IS 35 DAYS EARLIER
C      BUT WITH THE FIRST 2 CHARCTERS REMOVED
C      DAYL IS USED TO GET RID OF DATA 35 DAYS OLDER THAN TODAY
       READ(5,943) DAYL2,DATE
 943   FORMAT(A10,1X,A8)
       DAYL(1:6) = DAYL2(3:8)
       WRITE(74,944) DATE,DAYL
 944   FORMAT(' CURRENT DATE AND REMOVAL DATE ARE= ',A8,' ',A6)
       PARTS(1)='TTAA'
       PARTS(2)='TTBB'
       PARTS(3)='TTCC'
       PARTS(4)='TTDD'
       PARTS(5)='PPBB'
       PARTS(6)='PPDD'

C loop over 24-29 covers input from 6 upper air types

       DO 10 INN=24,29
       NP=INN-23
       PART=PARTS(NP)
c      WRITE(75,144) PARTS(NP)
c944   FORMAT(' DAYL= ',A6)
 144   FORMAT(' ',A4,' TIME TWINS FOLLOW ')
       JNN=74
C      THE PRESORTED OLD AND NEW WIND DATA IS PASSED TO INCSORT.
C      WITH OUTPUT ARRAY CARDS HOLDING NSORT LINES OF SORTED MERGED DATA WITH
C      CALM WINDS REMOVED AND DATA OLDER THAN 35 DAYS AGO REMOVED
C      ARRAY CARDD HAS NDUPS LINES OF WIND MATCHES THAT MAY BE DUE
C      TO TIMETWINS BUT ARE MOSTLY MATCHES BY LUCK 
       CALL INCSORT(CARDO,CARDN,CARDS,CARDD,INN,NDUPS,NSORT,DAYL)
C      CARDD HAS PAIRS OF LINES WITH MATCHING ID PRESSURE AND WIND
C      DIRECTION AND SPEED
       WRITE(74,946)
 946   FORMAT(' NDUPS is NUMBER OF WIND TWINS, NSORT is the NUMBER 
     &  of SORTED CARDS FOR OUTPUT')
       WRITE(74,947) NDUPS,NSORT
 947   FORMAT(' NDUPS NSORT= ',2I8)
 
c      DO 35 KB=1,NDUPS
c35    WRITE(74,948) CARDD(KB)
c948   FORMAT(A41)
 
       IDOLD='     '
       INUM=0
 
       DO 20 I=1,NDUPS
         IF(CARDD(I)(1:5) .NE. IDOLD) THEN
          IF(INUM .GE. 6) THEN
C         check number of new cards in twins   need 3 do run
 
 105        FORMAT(A)
 
c      WRITE(76,813)
 813   FORMAT('CARD1s for CDUP follow ')

c      DO 36 KB=1,INUM
c36    WRITE(76,948) CARD1(KB)
 
            CALL CDUPS(CARD1,INUM,PART,DATE)
          ENDIF
 
       INUM=0
       IDOLD=CARDD(I)(1:5)
       ENDIF
 
       IF(CARDD(I)(1:5) .EQ. IDOLD) THEN
       INUM=INUM+1
       CARD1(INUM)=CARDD(I)
       ENDIF
 
   20  CONTINUE
 
       IF(INUM .GE. 3) CALL CDUPS(CARD1,INUM,PART,DATE)
   10  CONTINUE
 
       RETURN
       END

      SUBROUTINE CDUPS(CARDS,KNUM,PART,DATE)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    CDUPS      DESCRIPTIVE TITLE NOT PAST COL 70
C   PRGMMR: STEVE LILLY           ORG: W/NP12   DATE: 2004-04-15
C
C ABSTRACT: THIS SUBROUTINE TAKES ALL THE MATCHING WINDS
C           FOR ONE ID TO SEE IF TWO DIFFERENT RUNS CONTAIN 
C           ENOUGH MATCHES TO BE CALLED A TIMETWIN
C           SUBROUTINE INDEX26 SORTS THE DATA FOR DIFFERENT PAIRS
C           OF RUNS
C
C PROGRAM HISTORY LOG:
C 1999-09-15  BRAD BALLISH DEVELOPED ORIGINAL CODE
C
C USAGE:    CALL CDUPS(CARD1,INUM,PART,DATE)
C   INPUT ARGUMENT LIST:
C     CARDS - HAS INUM LINES OF DATA FOR MATCHING WIND PAIRS ALL FOR ONE ID
C     INUM - HAS THE NUMBER OF LINES TO USE IN ARRAY CARD1
C     PART - CHARACTER*4 GIVES THE TYPE OF DATA (TTAA PPBB ETC.)
C     DATE - CHARACTER*10 NEEDED TO GET THE CENTURY
C
C   OUTPUT ARGUMENT LIST:      NONE
C
C   OUTPUT FILES:  (DELETE IF NO OUTPUT FILES IN SUBPROGRAM)
C     FT75 TIME-TWIN INFORMATION IF ANY FOUND
C
C REMARKS: LIST CAVEATS, OTHER HELPFUL HINTS OR INFORMATION
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  IBM
C
C$$$

      CHARACTER*41 CARDS(1),DUPA(69000),DUPB(69000)
      CHARACTER*41 DUP1(69000),DUP2(69000)
      CHARACTER*26 CDAT(69000)
      CHARACTER*20 SDAT(69000)
      CHARACTER*20 JDOLD
      CHARACTER*19 WDOLD
      CHARACTER*8  DATE 
      CHARACTER*4  PART
      DIMENSION INDR(69000)
      II=0
      IMSTOR=0
      ISTOR=0
 500  II=II+1
      ISTART=II
      IFIN=ISTART
      WDOLD=CARDS(II)(1:19)

 600  IF(CARDS(IFIN+1)(1:19) .EQ. WDOLD(1:19)) THEN
      IFIN=IFIN+1
      GO TO 600
      ENDIF

      INUM = IFIN - ISTART + 1
      IFIN1=IFIN-1

      DO 15 KO=ISTART,IFIN1,2
      KN=KO+1
  745 FORMAT(I6)
  746 FORMAT(I2)
      ISTOR=ISTOR+1
      CDAT(ISTOR)(1:10)=CARDS(KO)(29:38)
      CDAT(ISTOR)(11:20)=CARDS(KN)(29:38)
      CDAT(ISTOR)(21:26)=CARDS(KO)(7:12)
      DUPA(ISTOR)(1:41)=CARDS(KO)(1:41)
      DUPB(ISTOR)(1:41)=CARDS(KN)(1:41)
  15  CONTINUE

      II=IFIN
      IF(II .LT. KNUM) GO TO 500
      IF(ISTOR .GE. IMSTOR) IMSTOR=ISTOR
      CALL INDEX26(ISTOR,CDAT,INDR)

c     WRITE(77,985)
c985  FORMAT('CDUP TESTS FOLLOW')

      DO 34 I=1,ISTOR
      SDAT(I)=CDAT(INDR(I))
      DUP1(I)=DUPA(INDR(I))
      DUP2(I)=DUPB(INDR(I))
c     WRITE(77,986) SDAT(I)
c     WRITE(77,987) DUP1(I)
c     WRITE(77,987) DUP2(I)
 986  FORMAT(A20)
 987  FORMAT(A41)
 34   CONTINUE

      JJ=0
      JSTOR=0
 550  JJ=JJ+1
      JSTART=JJ
      JFIN=JSTART
      JDOLD=SDAT(JJ)(1:20)
c650  IF(SDAT(JFIN+1)(1:20) .EQ. JDOLD(1:20)) THEN
 650  IF(SDAT(JFIN+1)(1:20) .EQ. JDOLD .AND. JFIN+1 .LE. ISTOR) THEN
      JFIN=JFIN+1
      GO TO 650
      ENDIF
      JNUM = JFIN - JSTART + 1
      NI=0
      NJ=0
      IF( JNUM .GE. 3) THEN
      READ(DUP1(JSTART)(40:41),746) NI
      READ(DUP2(JSTART)(40:41),746) NJ
      NN=NI
      IF(NJ .LT. NN) NN=NJ
      ENDIF
        IF(JNUM .GT. NN/2 .AND. JNUM .GE. 3) THEN
      READ(DUP1(JSTART)(29:30),772) IYR
      READ(DUP1(JSTART)(31:32),772) MONTH
      READ(DUP1(JSTART)(33:34),772) IDAY 
      READ(DUP1(JSTART)(35:36),772) IRUN
      READ(DUP1(JSTART)(37:38),772) ICHR
      READ(DATE(1:2),772) IYY
      IYEAR=100*IYY+IYR
      IJULS=IW3JDN(IYEAR,MONTH,IDAY)
      ITIME=24*IJULS+ICHR+IRUN
 703  FORMAT(' ITIME,ICDATE,ICHR,IRUN= ',I8,' ',I6,' ',I2,' ',I2)
 772  FORMAT(I2)
      IHR1=IRUN+ICHR-3
      IF(IHR1 .LT. 0) IHR1=24+IHR1
      READ(DUP2(JSTART)(29:30),772) IYR
      READ(DUP2(JSTART)(31:32),772) MONTH
      READ(DUP2(JSTART)(33:34),772) IDAY 
      READ(DUP2(JSTART)(35:36),772) JRUN
      READ(DUP2(JSTART)(37:38),772) JCHR
c     IYEAR=1900+IYR
      IYEAR=100*IYY+IYR
      IJULS=IW3JDN(IYEAR,MONTH,IDAY)
      JTIME=24*IJULS+JCHR+JRUN
      IDDT=IABS(JTIME-ITIME)
         IF(IDDT .GE. 3) THEN
      IHR2=JRUN+JCHR-3
      IF(IHR2 .LT. 0) IHR2=24+IHR2
      WRITE(75,871) 
 871  FORMAT(' ')
      WRITE(75,771) DUP2(JSTART)(1:5),DUP2(JSTART)(35:36),
     *DUP2(JSTART)(29:34),PART,IHR2
 771  FORMAT(' Station ',A5,' For ',A2,'Z',' ',A6,' Type ',A4,
     * ' Time ',I2,'Z')
      WRITE(75,872) IDDT
 872  FORMAT(' Is a',I4,' Hour Time Twin with')
      WRITE(75,771) DUP1(JSTART)(1:5),DUP1(JSTART)(35:36),
     *DUP1(JSTART)(29:34),PART,IHR1
 671  FORMAT(' TWINS= ',A6,' RUN ',A2,'Z',' TIME ',I2,'Z','    ',
     * A6,' RUN ',A2,'Z',' TIME ',I2,'Z',' Time Dif= ',I4,'Hours')
        WRITE(75,304)  
 304  FORMAT(' Wind Matches Follow ')
        WRITE(75,305)  
 305  FORMAT(' ID    Press Dir Sp  Dif QM Run Date   No')

        DO 76 KR=JSTART,JFIN
        WRITE(75,105)  DUP2(KR)
        WRITE(75,105)  DUP1(KR)
  76    CONTINUE

        ENDIF

      JJ=JFIN
      ENDIF    
c     IF(JJ .LT. ISTOR-2) GO TO 550 is key (-2)
      IF(JJ .LT. ISTOR -2) GO TO 550
 105  FORMAT(A)
      RETURN
      END

       SUBROUTINE INDEX26 (N,ARRIN,INDX)  
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    INDEX26      DO HEAP SORT FOR ARRAY AARIN
C   PRGMMR: STEVE LILLY        ORG: W/NP12    DATE: 2004-04-15
C
C ABSTRACT: DOES HEAP SORT ON ARRAY AARIN WITH INDEX OF SORT
C           IN ARRAY INDX
C
C PROGRAM HISTORY LOG:
C   99-09-15 ORIGINAL AUTHOR BRAD BALLISH 
C
C USAGE:    CALL INDEX26(ISTOR,CDAT,INDR)
C   INPUT ARGUMENT LIST:
C     ISTOR  THE NUMBER OF ENTRIES IN ARRAY CDAT
C     CDAT   A CHARACTER*41 ARRAY HOLDING 
C
C   OUTPUT ARGUMENT LIST:      (INCLUDING WORK ARRAYS)
C     INDX   A INTEGER ARRAY GIVING THE SORT RESULTS 
C
C
C ATTRIBUTES:
C   LANGUAGE: f90
C   MACHINE:  IBM
C
C$$$
C
C                                                                       
C      INDEXS AN ARRAY ARRIN(1...N)                                     
C      INPUT:           N SPAN OF SORT                                  
C                                      ARRIN - ARRAY TO BE SORTED - NOT 
C      OUTPUT: INDX - ARRAY OF INDEXES SUCH THAT ARRAY(INDX(I)) IS      
C                                      IN ASCENDING ORDER FOR J = 1,... 
C                                                                       
       INTEGER INDX(*)                                                  
       CHARACTER*26 ARRIN(*),Q                                           
                                                                        
       DO 10 J = 1,N                                                    
           INDX(J) = J                                                  
10     CONTINUE                                                         
                                                                        
       L = N/2 + 1                                                      
                                                                        
       IR = N                                                           
33     CONTINUE                                                         
       IF (L.GT.1) THEN                                                 
           L = L - 1                                                    
           INDXT=INDX(L)                                                
           Q = ARRIN(INDXT)                                             
       ELSE                                                             
           INDXT = INDX(IR)                                             
           Q = ARRIN(INDXT)                                             
           INDX(IR) = INDX(1)                                           
           IR = IR - 1                                                  
           IF ( IR.EQ. 1) THEN                                          
               INDX(1) = INDXT                                          
               RETURN                                                   
           ENDIF                                                        
       ENDIF                                                            
                                                                        
       I = L                                                            
       J = L*2                                                          
                                                                        
30     CONTINUE                                                         
           IF ( J.LE.IR) THEN                                           
               IF (J.LT.IR.AND.ARRIN(INDX(J)).LT.ARRIN(INDX(J+1))) THEN 
                       J = J +1                                         
               ENDIF                                                    
               IF ( Q.LT.ARRIN(INDX(J)) ) THEN                          
                   INDX(I) = INDX(J)                                    
                   I = J                                                
                   J = J + I                                            
               ELSE                                                     
                   J = IR + 1                                           
               ENDIF                                                    
           ENDIF                                                        
       IF ( J.LE.IR) GOTO 30                                            
       INDX(I) = INDXT                                                  
       GOTO 33                                                          
       END                                                              
       SUBROUTINE INCSORT(CARDO,CARDN,CARDS,CARDD,INN,NDUPS,NSORT,DAYL)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    INCSORT IS PASSED BOTH THE OLD AND NEW DATA CARDS ON WINDS
C   PRGMMR: STEVE LILLY        ORG: W/NP12    DATE: 2004-04-15
C
C ABSTRACT: USES OLD AND NEW PRESORTED WIND DATA
C           DOES INCREMENTAL SORT ON MERGE OF OLD AND NEW
C           ALSO FINDS CURRENT WINDS THAT MATCH OLD FOR ONE PRESSURE LEVEL
C           THESE MATCHING WINDS ARE FURTHER TESTED IN CDUPS TO
C           FIND TIME-TWINS (CURRENT SOUNDING DATA IS REALLY A DUPLICATE OF OLD)
C
C PROGRAM HISTORY LOG:
C   99-09-15 ORIGINAL AUTHOR BRAD BALLISH 
C
C USAGE:       CALL INCSORT(CARDO,CARDN,CARDS,CARDD,INN,NDUPS,NSORT,DAYL)
C   INPUT ARGUMENT LIST:
C     INN       INN=24,29 FOR 6 UPPER AIR DATA TYPES
C     DAYL      CHARACTER*6  IS A LIMIT USED TO REMOVE OLD DATA
C
C   OUTPUT ARGUMENT LIST:      (INCLUDING WORK ARRAYS)
C     CARDS   HAS NSORT LINES OF SORTED AND MERGED (OLD AND NEW) WIND DATA
C     CARDO   HAS THE OLD SORTED WIND DATA READ FROM INPUT
C     CARDN   HAS THE NEW SORTED WIND DATA READ FROM INPUT
C     CARDD   HAS NDUPS LINES OF WIND MATCHES BETWEEN OLD AND NEW
C     NDUPS   IS THE NUMBER OF WIND MATCHES BETWEEN OLD AND NEW
C
C   OUTPUT FILES:  (DELETE IF NO OUTPUT FILES IN SUBPROGRAM)
C     UNIT 06  - STANDARD OUTPUT PRINT
C     UNIT 54  - FILE CONTAINING LATEST 36 DAYS OF UPPERAIR WIND DATA FOR PART TTAA
C     UNIT 55  - FILE CONTAINING LATEST 36 DAYS OF UPPERAIR WIND DATA FOR PART TTBB
C     UNIT 56  - FILE CONTAINING LATEST 36 DAYS OF UPPERAIR WIND DATA FOR PART TTCC
C     UNIT 57  - FILE CONTAINING LATEST 36 DAYS OF UPPERAIR WIND DATA FOR PART TTDD
C     UNIT 58  - FILE CONTAINING LATEST 36 DAYS OF UPPERAIR WIND DATA FOR PART PPBB
C     UNIT 59  - FILE CONTAINING LATEST 36 DAYS OF UPPERAIR WIND DATA FOR PART PPDD
C     UNIT 74  - FILE CONTAINING DIAGNOSTIC PRINT
C     UNIT 75  - FILE CONTAINING TIME-TWIN WIND MATCHES IF FOUND
C     UNIT 77  - DEBUG DIAGNOTICS PRINTOUTS IF TURNED ON IN FORTRAN CODE INCSORT

C
C REMARKS: LIST CAVEATS, OTHER HELPFUL HINTS OR INFORMATION

C ATTRIBUTES:
C   LANGUAGE: f90
C   MACHINE:  IBM
C
C$$$
C

       CHARACTER*41 CARDO(800000)
       CHARACTER*41 CARDN(70000)
       CHARACTER*41 CARDS(960000),CARDT(960000)
       CHARACTER*41 CARDD(90000)
       CHARACTER*41 CARDX,CARDA
       CHARACTER*41 HCARDS(6,960000),CCARDS(6,960000)
       COMMON /HISTPART/HCARDS,IIMAX(6)
       COMMON /CURRPART/CCARDS,JJMAX(6)
       COMMON /PMAXS/IMAX,JMAX,NP

       CHARACTER*6 DAYL
       JNN=INN+10
       KNN=INN+20
       LNN=INN+30
       CARDO='123456789a123456789a123456789a123456789a1'
       CARDN='123456789a123456789a123456789a123456789a1'
  105  FORMAT(A)
 797   CONTINUE
C
       IMAX=IIMAX(NP)
       JMAX=JJMAX(NP)
       DO 7 IX=1,IMAX
         CARDO(IX)=HCARDS(NP,IX)
    7  CONTINUE
       DO 8 JX=1,JMAX
         CARDN(JX)=CCARDS(NP,JX)
    8  CONTINUE
c  start with incremental sort and write
        I=1
        J=1
        K=0
        N=0
   77   CONTINUE

        IF(CARDO(I) .LT. CARDN(J)) THEN

          IF(CARDO(I)(14:19) .NE. '  0  0') THEN
          K=K+1
          CARDT(K)=CARDO(I)
          ENDIF

        I=I+1

        ELSEIF(CARDO(I) .GE. CARDN(J)) THEN

          IF(CARDN(J)(14:19) .NE. '  0  0') THEN
          K=K+1
          CARDT(K)=CARDN(J)
          ENDIF

        J=J+1
        ENDIF
        IF(I .LE. IMAX .AND. J .LE. JMAX) GO TO 77
 
        IF(I .GT. IMAX ) THEN

        DO 42 JJ=J,JMAX
          IF(CARDN(JJ)(14:19) .NE. '  0  0') THEN
          K=K+1
          CARDT(K)=CARDN(JJ)
          ENDIF
  42    CONTINUE

        ENDIF

        IF(J .GT. JMAX ) THEN

        DO 43 II=I,IMAX
          IF(CARDO(II)(14:19) .NE. '  0  0') THEN
          K=K+1
          CARDT(K)=CARDO(II)
          ENDIF
  43    CONTINUE

        ENDIF
      
       CARDS(1)=CARDT(1)
       KS=1
 
       DO 44 KT=2,K
 
       IF(CARDT(KT)(1:38) .NE. CARDS(KS)(1:38)) THEN
 
       IF(CARDT(KT)(29:34) .GE. DAYL) THEN
       KS=KS+1
       CARDS(KS)=CARDT(KT)
       ENDIF
 
       ENDIF
 
  44   CONTINUE
  
       NSORT=KS
       WRITE(LNN) KS
 713   FORMAT(I8)
       WRITE(LNN) (CARDS(JJ),JJ=1,KS)
c  done with incremental sort and write
 
c  Start sort on 1:19 to find card twins only
        I=1
        J=1
        N=0
   88   CONTINUE

        IF(CARDO(I)(1:19) .LT. CARDN(J)(1:19)) THEN
        I=I+1

        ELSEIF(CARDO(I)(1:19) .EQ. CARDN(J)(1:19)) THEN
         IFIN=I

 600     IF(CARDO(IFIN+1)(1:19) .EQ. CARDO(I)(1:19)) THEN
         IF(IFIN .LT. IMAX) THEN
         IFIN=IFIN+1
         GO TO 600
         ENDIF

         ENDIF

         JFIN=J

 605     IF(CARDN(JFIN+1)(1:19) .EQ. CARDN(J)(1:19)) THEN

         IF(JFIN .LT. JMAX) THEN
         JFIN=JFIN+1
         GO TO 605
         ENDIF

         ENDIF

        DO 611 JJ=J,JFIN

        DO 612 II=I,IFIN
        N=N+1
        CARDD(N)=CARDO(II)
        N=N+1
        CARDD(N)=CARDN(JJ)
 612    CONTINUE

 611    CONTINUE

        I=IFIN+1
        J=JFIN+1

        ELSEIF(CARDO(I)(1:19) .GT. CARDN(J)(1:19)) THEN
        J=J+1
        ENDIF 

        IF(I .LE. IMAX .AND. J .LE. JMAX) GO TO 88
       NDUPS=N
 
       RETURN
       END
