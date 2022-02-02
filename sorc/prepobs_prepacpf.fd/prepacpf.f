C$$$  MAIN PROGRAM DOCUMENTATION BLOCK
C
C MAIN PROGRAM:  PREPOBS_PREPACPF
C   PRGMMR: D. Keyser        ORG: NP22        DATE: 2017-01-12
C
C ABSTRACT: This program reads in aircraft profile data as output from
C   the NRL-based prepobs_prepacqc program and appends the nearest (in
C   time and distance) surface data available.  Surface data is pulled
C   from METAR reports in the corresponding ADPSFC dump file. 
C
C PROGRAM HISTORY LOG:
C 2011-03-10  J. Whiting -- Original code copied from offline mk_coloc
C              routine of 10 Mar 2011 15:03
C 2011-03-10  J. Whiting -- Adapted to run operationally; added W3TAG*
C              and ERREXIT calls; added docblock and comments
C 2012-11-20  J. Woollen -- Initial port to WCOSS
C 2013-02-12  D. Keyser  -- Final changes to run on WCOSS: Set BUFRLIB
C              missing (BMISS) to 10E8 rather than 10E10 to avoid
C              integer overflow; rename all REAL(8) variables as *_8;
C              use formatted print statements where previously
C              unformatted print was > 80 characters; traps to avoid
C              manipulating BUFRLIB missing values prior to encoding
C              them into output BUFR file (to avoid floating point
C              invalid error in BUFRLIB routine IPKS when debugging
C              turned on)
C 2017-01-12  D. Stokes  -- Check for, and skip, all comment lines when
C             reading contents of METAR station dictionary file ("!" in
C             character 1). Also fix to avoid a divide by zero which
C             slips by on tide/gyre with version 12.1 of the compiler,
C             but gets caught by newer compilers.
C             BENEFIT: Code more robust.  Comment lines were added to
C                      METAR dictionary on November 22, 2016 and since
C                      that time this program has been silently aborting
C                      due to an internal formatted read error.
C 2017-01-12  D. Keyser  -- Increase the maximum number of stations that
C             can be read from METAR station dictionary (MAXSTN) from
C             8000 to 20000.
C             BENEFIT: There are currently 7790 stations in the METAR
C                      dictionary. There are still stations missing that
C                      will need to be added.  A 20000 limit should
C                      suffice for quite some time,
C
C USAGE:
C   INPUT FILES:
C     UNIT 11  - METAR station dictionary file, sorted by longitude
C     UNIT 12  - ADPSFC dump file containing METAR surface reports
C     UNIT 13  - BUFR output from prepobs_prepacqc program containing
C                aircraft profile data with no surface level
C                information
C
C   OUTPUT FILES:  (INCLUDING SCRATCH FILES)
C     UNIT 51  - BUFR file as in unit 13, except now with nearest METAR
C                surface report appended to each aircraft profile
C
C   SUBPROGRAMS CALLED:
C     UNIQUE:    - GCIRC_QC (function) -- computes great circle 
C                  distances using the Haversine formula
C     UNIQUE:    - 
C     LIBRARY:
C       W3NCO    - W3TAGB  W3TAGE ERREXIT W3MOVDAT W3DIFDAT
C       W3EMC    - ORDERS
C       BUFRLIB  - DATELEN  UFBTAB UFBMEM  UFBTAM OPENBF   UFBSEQ 
C                  UFBINT   OPENMB WRITSB  COPYMG CLOSMG   CLOSBF
C                  IBFMS    SETBMISS GETBMISS
C
C   EXIT STATES:
C     COND =   0 - SUCCESSFUL RUN
C          =   9 - wrong input BUFR message type encounted, failure
C          =  99 - memory allocation error, failure
C
C REMARKS: None.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$
      program PREPOBS_PREPACPF

c quick look at msgs in BUFR files (mcl == read aircraft)

      real*8     bmiss       ! BUFR missing value
      real*8     getbmiss    ! Function to return current bmiss value

c-- station table vars
      character    line*132
      logical      eof        / .false. /
      CHARACTER    sid*8, si4*4, sname*32, st*2, cn*2, chrx*20
   
      parameter    (MAXSTN=20000 )               ! max # stations
c     character*8  stid(MAXSTN)                  ! stn ID
      character*4  stid(MAXSTN)                  ! stn ID - 4char metar
      real         slat(MAXSTN), slon(MAXSTN)    ! stn lat, lon
      integer      ishgt(MAXSTN)                 ! stn height

c     parameter    (NPTR=72 )       ! 5-degree bins
c     parameter    (NPTR=120)       ! 3-degree bins
      parameter    (NPTR=180)       ! 2-degree bins
c     parameter    (NPTR=360)       ! 1-degree bins
      integer      lptr(NPTR)       ! pointers into longitude array

c-- logical unit numbers
      integer      lunstn   / 11 /  ! input, surface station table
      integer      luntru   / 12 /  ! input, ground truth file (adpsfc)
      integer      lunpro   / 13 /  ! input, profile file
      integer      lunout   / 51 /  ! output profile file


c-- ground truth file vars
      parameter    (MXTS=9, MXPS=7)
      real(8)      usr8_8 
      character*80 tstr,pstr
      data         tstr /'YEAR MNTH DAYS HOUR MINU RPID CLAT CLON SELV'/  ! MXTS
      data         pstr /'ALSE PMSL PRLC  TMDP TMDB  WDIR WSPD'/          ! MXPS
      REAL(8),ALLOCATABLE :: gtru_t_8(:,:)
      REAL(8),ALLOCATABLE :: gtru_p_8(:,:)
      INTEGER,ALLOCATABLE :: NDX(:)
      parameter (PI=3.14159274, DEG_RAD=PI/180.)   ! conversion factors


c-- profile file vars
      character*80 cnem
      character*8  ctyp, ctyp0
      character*8  cval(5), rpid, cstr
      real*8       rval_8(5), rstr_8, prlc_8
      equivalence (cval(1),rval_8(1)), (cstr,rstr_8)
      parameter    (MXNM=25)
      real*8       hdr_8(MXNM),out1_8(MXNM),pro2_8(MXNM,255),
     &             out2_8(MXNM,255)
c     real(8)      pro3_8(MXNM,255,255)
      real*8       usr_8(MXNM),usr2_8(MXNM,255)
c     real*8       usr3d_8(20,10000,255) ! events: #mnems, #levs, #evnts
      real*8       t_tru_dp_8,t_tru_db_8,p_tru_8,altim_8,z_tru_8,pstn_8,
     &             ws_tru_8,wd_tru_8,w_u_8,w_v_8,ws_tru_k_8,x_gtru_8,
     &             x_tru_8,y_tru_8,dt_tru_8,dtlow_8,rpt_8,dhr_8


c-- time arrays
      integer idat(8)    ! cycle time (yr,mon,day,tzone,hr,min,sec,msec)
      real    rinc(5)    ! time increment (day,hr,min,sec,msec)
      integer itob(8)    ! obs time == cycle time + time increment
      integer itru0(8)   ! gr truth ob time
      integer itru(8)    ! matching gr truth ob time

c-- print output flags
      logical   bylev/.false./
      logical   wr_hdr/.true./, wr_stn/.true./, wr_match/.true./
      logical   wr_prof/.true./

c-- Function to convert Altimeter Setting to Station Pressure
c -   from prepdata.f (of Jun 03 [2008] 09:58, lines 13907-11);
C     subprogram SFCDTA
c -  formula ultimately from Smithsonian Meteorological Tables
c
C FCN P_f_A GETS PSTN (MB) FROM ALTIMETER SETTING (MB) AND ELEVATION (M)
      P_f_A(ALT,ELEV) = (ALT**0.190284 - (((1013.25**0.190284)*
     $ 0.0065/288.15)*ELEV))**5.2553026
ccccc      P_f_A(ALT,ELEV) = (ALT**0.190284 - (((1013.25**0.190284)*
ccccc     $ 0.0065/288.15)*ELEV))**5.2553026 + 0.3


c====67=10========20========30========40========50========60========70=2

      CALL W3TAGB('PREPOBS_PREPACPF',2017,012,71,'NP20')

      write(*,*) 'Welcome to PREPOBS_PREPACPF - Version 01-12-2017'

C On WCOSS should always set BUFRLIB missing (BMISS) to 10E8 to avoid
C  overflow when either an INTEGER*4 variable is set to BMISS or a
C  REAL*8 (or REAL*4) variable that is missing is NINT'd
C -------------------------------------------------------------------
ccccc call setbmiss(10E10_8)
      call setbmiss(10E8_8)
      bmiss = getbmiss()
      print *
      print *, 'BUFRLIB value for missing is: ',bmiss
      print *

c     wr_hdr=.false.
c     wr_stn=.false.
      bylev=.true.
c     wr_match=.false.
c     wr_prof=.false.

c   - read static station file (sfile) into memory, sort/index
c-------------------------------------------------------------
c--Read Station Table
      OPEN(LUNSTN,FORM='FORMATTED')

      ns=0                                         ! station counter
      do while ( .not. eof ) 
        read(lunstn,'(a)',end=200) line
        if(line(1:1).eq.'!')cycle
        ns=ns+1

        if (ns .gt. MAXSTN) then
          write(*,*) 'ERROR - too many stations', ns, MAXSTN
          exit
        endif ! ns > MAXSTN

        if ( ns .lt. 10 .and. .false. )            ! diagnostic print
     &    write(*,'(1x,i4,1x,a)') ns,"'" // line(1:96) // "'"

        read(line,1500)      ! format from subroutine tbastn.f (gemlib)
     &         sid, isn, sname,st,cn, lat,lon,   ihgt,  ispr, chrx
 1500   format( a, 1x,i6, 3(1x,a),  1x,i5,1x,i6, 1x,i5, 1x,i2, a )

        stid(ns)=sid(1:4)                          ! metar ids - 4char
        slat(ns)=float(lat)/100.
        slon(ns)=float(lon)/100.
        ishgt(ns)=ihgt

      enddo ! while not eof
      write(*,*) 'eof reached; ns=',ns

 200  continue
      write(*,*) '--rd Station Table: #stns=',ns

      nstot=ns  ! total # of stations

      close(lunstn)


c--set up pointers into lon array
c--- [ lptr(n) == metar line number of first lon w/in bin 'n' ]
c
      binwidth=360./float(NPTR)
      write(*,'(1x,a,1x,f6.2)') ' -pointer binwidth=',binwidth
      lptr0=0
      ns=1
      do j=1,NPTR
        b1=  ( float(j-1) * binwidth ) - 180.
        do while ( slon(ns) .lt. b1 )
          ns=ns+1
        enddo ! while slon(ns) < b1
        lptr(j)=ns

        if ( .false. ) then ! write diagnostic for station tbl pointers
          nl=lptr(j)-lptr0
          lptr0=lptr(j)
          b2=  b1 + binwidth
          write(*,'(1x,i3," ==",1x,i4,2(1x,f6.1),1x,i3,2f8.2)')
     &      j,ns,b1,b2,nl,slon(lptr(j)),slon(ns-1)
        endif ! true/false -- stn tbl pointers diagnostics

      enddo ! j=1,NPTR


c--Read ground truth file
c------------------------
      call DATELEN(10)                          ! set dates to 10-digit
      OPEN(luntru,FORM='UNFORMATTED')


c--Read Gr Truth value into memory
      write(*,'(a,$)') ' --rd GrTru file: ' 
      call ufbtab(-luntru,usr8_8,1,1,MXTRU,' ')      ! gets # of rpts
      call ufbmem(luntru,0,imsg,munit)
      write(*,*) ' #rpts=',MXTRU,' #msgs=',imsg

      allocate(gtru_t_8(MXTS,MXTRU),STAT=I);IF(I.NE.0) GOTO 901
      call ufbtam(gtru_t_8,MXTS,MXTRU,ntab,tstr)
      write(*,'("  -tstr=""",A,"""")') tstr

      allocate(gtru_p_8(MXPS,MXTRU),STAT=I);IF(I.NE.0) GOTO 901
      call ufbtam(gtru_p_8,MXPS,MXTRU,ntab,pstr)
      write(*,'("  -pstr=""",A,"""")') pstr

      allocate(ndx(MXTRU)        ,STAT=I);IF(I.NE.0) GOTO 901
      do i=1,MXTRU ; ndx(i)=i ; enddo



      if (.false.) then ! true/false skip (sorting)
         write(*,*) ' -gtru arrays sorted'

c--Sort GrTru values (RPID CLON CLAT DAYS HOUR MINU)
c ---sort keys go from low to high priority (ie, last call is highest)

      call orders(12,ndx,gtru_t_8(1,1),ntab,MXTS,8,2)   ! MINU
      call orders(12,ndx,gtru_t_8(2,1),ntab,MXTS,8,2)   ! HOUR
      call orders(12,ndx,gtru_t_8(3,1),ntab,MXTS,8,2)   ! DAYS
      call orders(12,ndx,gtru_t_8(7,1),ntab,MXTS,8,2)   ! CLAT
      call orders(12,ndx,gtru_t_8(8,1),ntab,MXTS,8,2)   ! CLON
      call orders(10,ndx,gtru_t_8(6,1),ntab,MXTS,8,2)   ! RPID

      write(*,'(a,$)') 'ndx(1-10): '
      write(*,'(1x,i5,$)') (ndx(i),i=1,10)
c...jaw -- sorting

      else  ! true/false skip it (sorting)
         write(*,*) ' -gtru arrays NOT sorted'
      endif ! true/false skip it (sorting)


c debug print
c     gtru_t_8      YEAR MNTH DAYS HOUR MINU RPID CLAT CLON SELV
c                     1    2    3    4    5    6    7    8    9
c     gtru_p_8      ALSE PRES PRLC  TMDP TMDB  WDIR WSPD
c     gtru_p_8      ALSE PMSL PRLC  TMDP TMDB  WDIR WSPD
c                     1    2    3     4    5     6    7
      if ( .false. ) then  ! skip it true/false -- debug gtru
      write(*,'(1x,a)') '-debug echo of gtru values'
      write(*,'(1x,a)') ' tstr >'//tstr//'<'
      write(*,'(1x,a)') ' pstr >'//pstr//'<'
      j1=16044  ; j2=j1+5
      j1=1      ; j2=j1+5
      j1=1      ; j2=MXTRU
c     nprlc=0                                      ! track PRLC content
      do j=j1,j2

        prlc_8=gtru_p_8( 3,j)                      ! track PRLC content
        if (ibfms(prlc_8).ne.0) cycle
        nprlc=nprlc+1 ; if (nprlc.gt.10) cycle

c       if ( ( j.gt.3 .and. j.lt.16046 ) .or.      ! sample each dtyp
c    &       ( j.gt.16048 .and. j.lt.16728 ) .or. 
c    &         j.gt.16730 ) cycle

c       write(*,'(2x,$)')
        write(*,'(1x,i5,$)') j                              ! rpt #
        write(*,6) (int(gtru_t_8(i,j)),i=1,5)               ! date/time
        write(*,'(2x,f6.2,1x,f7.2,$)') (gtru_t_8(i,j),i=7,8)! CLAT,CLON
        rval_8(1)=gtru_t_8(6,j) ; write(*,'(2x,a,$)')cval(1)! RPID
        write(*,'(1x,i5,$)') int( gtru_t_8(9,j) )           ! SELV


        write(*,'(1x,a,$)') '|p'
        write(*,'(3(1x,f7.0),$)') (gtru_p_8(i,j),i=1,3)! ALSE,PMSL,PRLC
        write(*,'(1x,a,$)') '|t'
        write(*,'(1x,f6.2,$)') gtru_p_8( 4,j)          ! TMDP
        write(*,'(1x,f6.2,$)') gtru_p_8( 5,j)          ! TMDB
        write(*,'(1x,a,$)') '|w'
        write(*,9) (gtru_p_8(i,j),i= 6, 7)             ! WDIR WSPD
        write(*,*) ! linefeed
      enddo
    6 format (1x,i4,2i2.2,".",2i2.2,$)
    9 format (1x,f4.0,1x,f5.1,$)

      write(*,'(1x,a,2i6)') '-nprlc,MXTRU=',nprlc,MXTRU   ! track PRLC
                                                          !  content

      write(*,'(  1x,a)') '--debug stop' ; stop
      endif ! true/false skip it -- debug gtru


      write(*,*) '--closing GrTru',luntru
      close(luntru)


      if ( .not. wr_prof ) write(*,*) '--NOT writing prof file',lunpro

c--Read aircraft profile prepbufr file
c-------------------------------------
      if ( .true. ) then ! false/true skip it (lunpro)

      dxymax=0   ! counter for max xy separation

c-- Open input a/c profile file
c     OPEN(lunpro,FORM='UNFORMATTED')
c     CALL OPENBF(0,'QUIET',1)                  ! debug
c     CALL MESGBC(lunpro,MSGT,ICOMP)

      write(*,*) '--open input a/c profile', lunpro
      call OPENBF(lunpro,'IN',lunpro)


c-- Open output a/c profile file
c     OPEN(lunout,FORM='UNFORMATTED')
      if ( wr_prof ) then
        write(*,*) '--open output a/c profile', lunout
        call OPENBF(lunout,'OUT',lunpro)
      endif ! wr_prof


c--Loop thru aircraft profile prepbufr file
      jmmin=400 ! counter for RPID matches in GTru
      jmmax=0   ! counter for RPID matches in GTru
      njm0=0    ! cntr for # profs where jm=0 -- rejected for output

      nr=0                        ! rpt counter
      nmsg=0                      ! msg counter
      nr1=0                       ! rpt counter -- non-skipped
      nmsg1=0                     ! msg counter -- non-skipped
      nhi=0                       ! rpt counter -- rejected high (idhgt)
      nrare=0                     ! rpt counter -- rejected high ( )
      nfar=0                      ! rpt counter -- rejected far (dmin)
      nt3=0                       ! rpt counter -- rejected typ=3xx
      nro=0                       ! rpt counter -- # written out

      ctyp0=" "
      do while( IREADMG(lunpro,ctyp,idate).EQ.0 )
      nmsg=nmsg+1
      msub = nmsub(lunpro)

      if ( ctyp(1:6) .ne. 'AIRCFT' .and. ctyp(1:6) .ne. 'AIRCAR' ) then 
        write(*,*)
        write(*,*) 'ctyp=>',ctyp,'< -- not supported - exiting'
c       stop 9
        CALL W3TAGE('PREPOBS_PREPACPF')
        call errexit(9)
      endif ! ctyp != AIRCFT or AIRCAR

      if ( ctyp .ne. ctyp0 ) then 
        if ( ctyp0 .ne. " " .and. .false. ) write(*,*) ! linefeed
        ctyp0=ctyp
        write(*,*) ' ctyp="'//ctyp//'" idate=',idate
      endif ! ctyp != ctyp0

c-- skip AIRCFT reports
ccc   if ( ctyp(1:6) .ne. 'AIRCAR' .and. .true. ) then 
      if ( ctyp(1:6) .eq. 'AIRCFT' .and. .false. ) then 
        nr=nr+msub
        cycle
      endif ! not AIRCFT
      nmsg1=nmsg1+1


      do while (iREADSB(lunpro) .eq. 0) 
      nr=nr+1

c     if ( nr .lt. 524                  ) cycle          ! poaf=6
ccc   if ( nr .lt. 397 .or. nr .gt. 397 ) cycle
ccc   if ( nr .lt. 236 .or. nr .gt. 238 ) cycle
ccc   if ( nr.ne.236 .and. nr.ne.237 .and. .false. .and. ! nrmin, nrmax
ccc  &     nr.ne.238 .and. nr.ne.257 .and. nr.ne.270 ) cycle
ccc   if ( nr1.gt.5 .and. nr.ne.270 .and. nr.ne.272 .and.        ! jm=0
ccc  &     nr.ne.289 .and. nr.ne.296 .and. nr.ne.302 ) cycle

c...jaw -- special prof filtering
      nr1=nr1+1

c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
c AIRCFT/AIRCAR processing

c AIRCAR/AIRCFT == HEADR ACID/RCT {PRSLEVLA}
c HEADR ==
c  SID  XOB YOB DHR ELV TYP T29 TSB  ITP  SQN PROCN  RPT  TCOR <RSRD_SEQ>
c      lon lat                     instr           otime
      
      call ufbseq(lunpro,hdr_8,MXNM,1,nhdr,'HEADR')
      rval_8(1)=hdr_8(1)                               ! SID
      rlon0 = hdr_8(2)
      if ( rlon0 .gt. 180 ) rlon0=rlon0-360.
      rlat0 = hdr_8(3)

      dhr_8 = hdr_8(4)                                 ! t_ob - t_cycle
      iz0 = int( hdr_8(5) )                            ! elev (ELV)

c--         lon lat drft_t ph_flt turb Pres Temp Hgt PrQM HgtQM nrlQM
c     cnem='XDR YDR HRDR    POAF  DGOT  POB TOB  ZOB  DDO FFO UOB VOB'
       ! v1 btab

      cnem='XDR YDR HRDR    POAF  TRBX  POB TOB  ZOB  DDO FFO UOB VOB'
       ! v2 btab

      usr2_8=bmiss
      call ufbint(lunpro,usr2_8,MXNM,255,nlev,cnem)


c-- Get lat/lon/elev for minimum height ob
c--- assume minimum height obs occur at 1st replication (lev=1)

      llow=1
      izmin = int( usr2_8(8,llow) )  ! DAK: could usr2_8(8,llow) missing
                                     !      cause a problem here??
      MAXHGT=5000
      if ( izmin.gt.MAXHGT .and. .false. ) then  ! exclude 'high' rpts
        nhi=nhi+1                        ! max hgt in metar.tbl = 4057
        nr1=nr1-1   ! reset counter
        cycle
      endif ! izmin > MAXHGT

      ityp=int( hdr_8(6) )
      if ( ityp/100 .eq. 3 .and. .true. ) then        ! exclude typ=3xx 
        nt3=nt3+1
        cycle
      endif ! ityp/100 = 3

      rlonmin = usr2_8(1,llow)
      if ( rlonmin .gt. 180 ) rlonmin=rlonmin-360.
      rlatmin = usr2_8(2,llow)
        dtlow_8 = usr2_8(3,llow)                    ! hrdr of low prof


c--Begin output

c---Columns header
      if (wr_hdr) then 
        wr_hdr = .false.
        write(*,50) 'rpt','XOB','YOB','ZOB'
        write(*,51) 'stn','lon','lat','hgt'
c       write(*,52) 'dXY','dhgt'
      endif ! wr_hdr
   50 format(/,1x,a,3(4x,a),$)
   51 format(3x,a,1x,2(4x,a),3x,a,$)
   52 format(3x,a,3x,a,$)


c--Test: what is max delta xy (j-1 -> j) for lowest levs? 
      if ( .false. ) then  ! true/false skip it -- dxy
        dxyjmax=0
        do j=2,min( nlev, 6 )
          dxyj = dsqrt( ( usr2_8(1,j) - usr2_8(1,j-1) )**2 + 
     &                  ( usr2_8(2,j) - usr2_8(2,j-1) )**2 )
          if ( abs(dxyj) .gt. dxyjmax ) then 
            dxyjmax=abs(dxyj)
            jmax=j
          endif
        enddo ! j=2,nlev
        write(*,'(1x,a,1x,f6.1,1x,i3, $)') 'x=',dxyjmax*111., jmax
        if ( dxyjmax .gt. 5./111. .and. .false. ) bylev = .true.
      endif ! true/false -- dxy


c--Find Nearest Station in Table
c--- lon,lat,hgt == rlonmin, rlatmin, izmin
c ( b1 + 180 ) / binwidth + 1 = j

c-find index into station table to start search
      rlon= rlonmin - binwidth              ! backup 1 bw (ease factor)
      j=  int( (rlon + 180.)/binwidth ) + 1
      b1= slon(lptr(j))                     ! j bin start
      b2= b1 + binwidth * 3.                ! start lon + 3 bins
c     write(*,'(1x,a,1x,f7.2,1x,i3,2(1x,f7.2),$)') '|',rlon,j, b1,b2
c     write(*,'(a,3(1x,i4),$)')' ||',lptr(j),lptr(j+3),lptr(j+3)-lptr(j)

c-loop thru stations & calc xy distance.
c     write(*,*) ! linefeed
      ns=0
      dmin=1.d5
      l=lptr(j)
c     l=1                                     ! *** debug: all stns
      lmin=l
cppppppppppppppp
cdak  print *, 'initially: l,ns = ',l,ns
cppppppppppppppp
      do while (slon(l) .le. b2 .or. .false.) ! *** db: true == all stns

        slon2 = slon(l)
        slat2 = slat(l)
        d = gcirc_qc(slat2,slon2,rlatmin,rlonmin)/ 1.d3  ! dist in km
        if ( d .lt. dmin) then 
          dmin = d
          lmin = l
        endif ! d < dmin

c       write(*,'(/,3x,i4,3x,i4,1x,f7.2,1x,f6.2,1x,f9.3,$)')
c    &    ns,l,slon2,slat2,d

        l=l+1
        ns=ns+1
cppppppppppppppp
cdak  print *, 'l,ns,nstot = ',l,ns,nstot
cppppppppppppppp
        if ( l.gt.nstot ) then  ! added by DAK 3/6/13 to avoid slon array
                                !  overflow
           print *, 'WARNING: l in loop exceeds nstot - exit loop'
cccccccc   print *, ' -- Jeff Whiting needs to look into this!!!!!'
           exit
        endif
c       if ( ns.ge.nstot ) exit                  ! *** db: ck all stns
      enddo ! while slon < b2

c-write out results of search
      idhgt=izmin-ishgt(lmin)                    ! dHgt = ZOB(1)-stnHgt

      if (wr_stn) then

c     write(*,'(/,i3,i4,$)') nmsg, nr
      write(*,'(/,   i4,$)')       nr

c     write(*,'(1x,a,$)') cval(1)                ! SID
      write(*,61) rlonmin ,rlatmin               ! XOB(1), YOB(1)
   61 format(1x,f7.2,1x,f6.2,$)

      write(*,'(1x,i5,$)') izmin                 ! ZOB(1)
c     write(*,'(1x,f9.5 ,$)') dtlow_8            ! HRDR(1) (hrs)

      write(*,85)  ' |',stid(lmin),slon(lmin),slat(lmin),ishgt(lmin)
   85 format(a,1x,a,1x,f7.2,1x,f6.2,1x,i4                ,$)
      write(*,'(1x,a,i4,$)') '|| #stn chkd=',ns
      write(*,86)  ' |',dmin,idhgt                       ! Far, Hi
   86 format(                             a,1x,f4.1,1x,i5,$)
      endif ! wr_stn


c-skip profs that are too high (meters/pressure) or too far (km)
      if (.true.) then ! true/false skip it -- rejections
      LOW=300 ; RARE=30 ; rNEAR=10
      if (idhgt.gt.LOW .and. .true.) then
        if (wr_stn) write(*,'(2(1x,a,i5),$)') '||hi:',idhgt,'>',LOW
        nhi=nhi+1
        cycle
      endif ! Idhgt<LOW
          ! DAK: to avoid compiler initialization warning, set dpres to
          !      zero - appears if-test below never satisfied anyway
          !      since .false. in if-test
             dpres = 0.
      if ( dpres.gt.RARE .and. .false.) then
        if (wr_stn) write(*,'(2(1x,a,i5),$)') '||dp:',dpres,'>',RARE
        nrare=nrare+1
        cycle
      endif ! dpres<RARE
      if (dmin.gt.rNEAR .and. .true.) then
        nfar=nfar+1
        if (wr_stn) 
     &    write(*,'(1x,a,f4.1,1x,a,f4.1,$)') '||far:',dmin,'>',rNEAR
        cycle
      endif ! dmin<rNEAR
      endif ! true/false skip it -- rejections

c ---400mb total column height
c ...jaw - GDiMego suggests above filter


c--Output by level
      if ( bylev ) then 

      nlevpr=min(nlev,5)      ! number of levs to print
      nlevpr=min(nlev,255)    ! number of levs to print
c     if ( nmsg.eq.31 .and. nr.eq.117 ) nlevpr=nlev
      if ( nlevpr.gt.1 ) 
     &  write(*,'(/, 3x,a,i3,1x,2a,$)') 'n=',nlev,
     &    ' lon    lat   Z(m)     hrdr   dX(km) dt(min)',
     &    '  kph   dZ(m)  phF  P(mb)    T(c)' //
     &    '  W(deg) W(m/s)   W-u   W-v'

      do k=1,nlevpr
        j=k                               ! print out 1st nlevpr-2 levs
        if (k.eq.nlevpr-1) j=nlev-1       !  followed by the nlevpr-1 &
        if (k.eq.nlevpr) j=nlev           !  nlevpr levs

        if ( nlev.gt.1 ) then 
c         write(*,'(/,i3,i4 ,$)') nmsg, nr                  ! nmsg, nr
          write(*,'(/,3x,1x,i2 ,$)') j                      ! j

          rlon=usr2_8(1,j)
          if ( rlon .gt. 180 ) rlon=rlon-360.
          write(*,'(1x,f7.2,1x,f6.2 ,$)') rlon, usr2_8(2,j) ! lon(XDR),
                                                            ! lat(YDR)

c-- Observation height
          iz = int(usr2_8(8,j))
          write(*,'(1x,i5 ,$)') iz                  ! hgt (m) (ZOB)

c-- Drift time (t_drift - t_cycle)
          t = usr2_8(3,j)
          write(*,'(1x,f9.5 ,$)') t                 ! time (HRDR)

c-- horizontal offset in km (plane geometry)
          dxy = dsqrt( ( rlon-rlon0 )**2 + ( usr2_8(2,j)-rlat0 )**2 )
          dxy = dxy * 1.11d2
          write(*,'(1x,f6.1 ,$)') dxy               ! horiz dist (km)

          if (dxy.gt.dxymax .and. .true.) dxymax=dxy

c-- time offset in minutes
c      (t_drift-t_cycle) - (t_ob-t_cycle) == t_drift-t_ob
          dt = (usr2_8(3,j) - dhr_8) * 60.d0
          write(*,'(1x,f6.2 ,$)') dt                ! elapsed time (hr)

c-- horizontal velocity
          if ( j .gt. 1 ) then 
            dTj   = ( usr2_8(3,j) - usr2_8(3,j-1) ) * 60.d0 !! dT in min
            dlonj = usr2_8(1,j) - usr2_8(1,j-1)
            dlatj = usr2_8(2,j) - usr2_8(2,j-1)
            dXj = dsqrt( ( usr2_8(1,j) - usr2_8(1,j-1) )**2 + 
     &                   ( usr2_8(2,j) - usr2_8(2,j-1) )**2 ) * 1.11d2
                                                         !! dX in km
c           write(*,'(1x,f6.1,1x,f8.5 ,$)') dXj, dTj     ! dX(km),
                                                         ! dT(min)
            if ( dTj .gt. 1.d-5 ) then
              vj = dXj / dTj * 60.d0                       !! v in kph
              write(*,'(1x,f7.1 ,$)') vj * 1.d5 / 2.54 / 12. / 5280.
                                                    ! velocity in kph
            else
              write(*,'(1x,a7 ,$)') 'n/a '
            endif ! dTj > .00001
          else
ccccccccc   write(*,'( 8x ,$)')
            write(*,'(1x,a7 ,$)') '       '
          endif ! j ne 0

        endif ! nlev > 1

c-- hgts offset from elevation
        idz = iz - iz0                              ! ZOB - ELV
c       idz = int( float(idz) / 2.54d-2 / 12.d0 )   ! convert hgt to ft
        write(*,'(1x,i6 ,$)') idz

c-- Phase of aircraft flight
c  POAF == 0-1:RSV 2:UNS 3:LVL 4:LVw/W 5:ASC 6:DES 7:MSG' # 0-08-004
        ipoaf = int(usr2_8(4,j))
        if (ibfms(usr2_8(4,j)).ne.0) ipoaf = -1
        write(*,62) ipoaf                 ! phase of flight (POAF)
   62 format(1x,i3, $)

        write(*,63) usr2_8(6,j)           ! pressure (mb) (POB)
   63 format(1x,f7.1, $)

        write(*,64) usr2_8(7,j)           ! temperature (c) (TOB)
   64 format(1x,f7.1, $)

        write(*,65) (usr2_8(i,j),i=9,10)  ! wind dir, spd (DDO, FFO)
   65 format(2x, 1x,f4.0,1x,f5.1,$)

        write(*,66) (usr2_8(i,j),i=11,12) ! wind u-, v-  (UOB, VOB)
   66 format(2x,    2(1x,f5.1),$)

      enddo ! k=1,nlev

      endif ! bylev
c+++++

c     if ( nr .ge. 3 .and. .true.  ) then 
c       print '(/,a)', "debug stop"
c       stop
c     endif

c AIRCFT/AIRCAR processing
c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


c---Get time of lowest profile observation
c --- dtlow_8 == t(1) == hrdr(1) == usr2_8(3,1) == t{lowest ob}-t{cyc} (hr)

c     write(*,'(/,8x,a,$)') 'dtlow_8, dtlow_8*60, dhr_8, dhr_8*60: '
c     write(*,'(2(1x,f9.5,1x,f6.3,$)') dtlow_8,dtlow_8*6.d1,dhr_8,
c    &                                 dhr_8*6.d1

c--cycle time (idat: year, mon, day, tzone, hr, min, sec, msec)
      iyr=idate/1000000
      imm=mod(idate/10000,100)
      idd=mod(idate/100,100)
      ihr=mod(idate,100)

c     integer idat(8)   ! cycle time (yr,mon,day,tzone,hr,min,sec,msec)
      idat=0
      idat(1)=iyr
      idat(2)=imm
      idat(3)=idd
      idat(5)=ihr

c--time increment (rinc: day, hr, min, sec, msec)
c     real    rinc(5)     ! time increment (day,hr,min,sec,msec)
      rinc=0
      rinc(2)=dtlow_8     ! HRDR(1): dt of low prof
      
c--time of (lowest) ob
c  w3movdat(rinc,idat,itob): itob(ymd/hmss)= idat(ymd/hmss)+rinc(dh/mss)
      call w3movdat(rinc,idat,itob)
      if (wr_match) then
        write(*,'(/,1x,a,$)') 't_ob_low:'
        write(*,7) (itob(i),i=1,3),(itob(i),i=5,8)! low ob date/time
        write(*,'(1x     ,f8.5,$)') dtlow_8       ! dt: low prof - cycle
      endif ! wr_match


c---Search GTru array for match
      rpid=stid(lmin) 
      jm=0                     ! counter for # of matching gtru stns
      tdif0=120.               ! minimum time diff threshold (hrs)
      do l=1,MXTRU
        j=ndx(l)
        rval_8(1)=gtru_t_8(6,j)        ! RPID
        if ( rpid.eq.cval(1) ) then 
          jm=jm+1

c-Gr Truth Time in NCEP time format (yr,mon,d,t_zone,h,min,s,ms)
          itru0=0
          itru0(1)=int(gtru_t_8(1,j))  ! year
          itru0(2)=int(gtru_t_8(2,j))  ! month
          itru0(3)=int(gtru_t_8(3,j))  ! day
          itru0(5)=int(gtru_t_8(4,j))  ! hour
          itru0(6)=int(gtru_t_8(5,j))  ! minute

c-low profile time - gr_tru time
          rinc=0
          call w3difdat(itob,itru0,0,rinc)        ! rinc = itob - itru0

          tdif = (( rinc(5)/1.d3 + rinc(4) ) / 60.d0 ! secs to minutes
     &            +                rinc(3) ) / 60.d0 ! minutes to hours
     &            +                rinc(2)           ! hours
     &            +                rinc(1) * 24.d0   ! days to hours

c-find minimum time diff & note g_tru record #
          if ( abs(tdif).lt.abs(tdif0) ) then 
            tdif0=tdif                      ! min t: low prof - gr_tru
            ngtru=j                         ! gtru record w/ min tdif
            itru=itru0                      ! matching gtru time values
          endif ! tdif < tdif0


c--Echo all matching stations
c tstr        YEAR MNTH DAYS HOUR MINU RPID CLAT CLON SELV
c gtru_t_8      1    2    3    4    5    6    7    8    9
          if ( wr_match ) then  
            write(*,'(/,2x,i2,$)') jm                      ! match #
            write(*,'(1x,i5,$)') j                         ! rpt #
c           write(*,7) (int(gtru_t_8(i,j)),i=1,5),0,0      ! date/time
            write(*,7) (itru0(i),i=1,3),(itru0(i),i=5,8)   ! date/time
c           write(*,'(2x,f6.2,1x,f7.2,$)') (gtru_t_8(i,j),i=7,8)! CLAT,
                                                                ! CLON
            write(*,'(2x,f7.2,1x,f6.2,$)')(gtru_t_8(i,j),i=8,7,-1)!CLON,
                                                                  !CLAT
                           write(*,'(2x,a,$)') cval(1)(1:4)     ! RPID
            write(*,'(1x,i5,$)') int( gtru_t_8(9,j) )           ! SELV

c           write(*,'(  1x,a,$)') '|| tdif:'
c           write(*,28) (abs(int(rinc(i))),i=1,5)
c  28 format (1x,    i2.2,".",3i2.2,'.',i3.3,$)        ! packed print
cj          write(*,18) (int(rinc(i)),i=1,5)
cj 18 format (1x,  i2,3(1x,i3.2),1x,i4.3,$)            ! unpacked print

            write(*,'(1x,a,1x,f8.5,$)') '| dt:',tdif
            write(*,'(1x,a,1x,f8.5,1x,i6,$)') '||',tdif0, ngtru

          endif ! wr_match
        endif ! rpid = cval(1) [matching gtru stns]
      enddo ! l=1,MXTRU
      if ( jm.lt.jmmin ) then ; jmmin=jm ; nrmin=nr ; endif
      if ( jm.gt.jmmax ) then ; jmmax=jm ; nrmax=nr ; endif


c Trap case where no matching station is found
      if ( jm.eq.0 ) then 
        njm0=njm0+1
        if ( .true. ) write(*,'(/,a,2(1x,i4),$)') 
     &    'ERROR -- no matches found; jm,njm0: ',jm,njm0
        cycle                               ! break out of lunpro loop
c...jaw -- is this right?
      endif ! jm=0


c--Designate parms for output
c  tstr        YEAR MNTH DAYS HOUR MINU RPID CLAT CLON SELV
c  gtru_t_8      1    2    3    4    5    6    7    8    9
c  pstr        ALSE PMSL PRLC  TMDP TMDB  WDIR WSPD        ! metar-ish
c  gtru_p_8      1    2    3     4    5     6    7

c-time (drift info)       ! DHR(rinc) = cycletime(idat) - stntime(itru)
      rinc=0
      call w3difdat(itru,idat,0,rinc)            ! dt: stn - cycle time
      dt_tru_8 =                                 ! DHR (hr)
     &           (( rinc(5)/1.d3 + rinc(4) ) / 60.d0  ! secs to minutes
     &            +                rinc(3) ) / 60.d0  ! minutes to hours
     &            +                rinc(2)            ! hours
     &            +                rinc(1) * 24.d0    ! days to hours


c-location (CLON, CLAT, ...  izmin SELV)
      x_gtru_8=gtru_t_8(8,ngtru)                  ! CLON (deg)
      x_tru_8=x_gtru_8
      if (x_tru_8.lt.0.) x_tru_8=x_tru_8+360.     ! XOB/XDR (deg 0-360)
      y_tru_8=gtru_t_8(7,ngtru)                   ! CLAT (deg)
      z_tru_8=gtru_t_8(9,ngtru)                   ! SELV (m)

c-pressure -- convert Altimeter Setting (ALSE) to Station Pressure
c     p_tru_8=gtru_p_8(2,ngtru)                   ! PMSL (P)
      altim_8=gtru_p_8(1,ngtru)                   ! ALSE (P)

      if(ibfms(z_tru_8).eq.0 .and. ibfms(altim_8).eq.0)  then
         pstn_8=p_f_a(altim_8*0.01, z_tru_8)      ! stn pres (mb)
         p_tru_8=pstn_8
      else
         p_tru_8=bmiss
      endif

c-temperature (dewpoint & dry bulb)
      t_tru_dp_8=gtru_p_8(4,ngtru)                ! TMDP (K)
      t_tru_db_8=gtru_p_8(5,ngtru)                ! TMDB (K)

c-winds (dir & spd)
      wd_tru_8=gtru_p_8(6,ngtru)                  ! WDIR (deg)
      ws_tru_8=gtru_p_8(7,ngtru)                  ! WSPD (m/s)

      if(ibfms(ws_tru_8).eq.0 .and. ibfms(wd_tru_8).eq.0) then
         w_u_8=ws_tru_8 * cos(wd_tru_8*DEG_RAD)   ! u- wind (m/s)
         w_v_8=ws_tru_8 * sin(wd_tru_8*DEG_RAD)   ! v- wind (m/s)
      else
         w_u_8=bmiss
         w_v_8=bmiss
      endif

      if(ibfms(ws_tru_8).eq.0) then
         ws_tru_k_8=ws_tru_8 * 1.944d0            ! wspd (knots)
      else                                        ! (1 m/s= 1.944 knots)
         ws_tru_k_8=bmiss
      endif

c--Echo matching station info
      if (wr_stn) then 

ccc     write(*,'(/,3x,1x,i2 ,$)') 0
ccc     write(*,'(1x,f7.2,1x,f6.2 ,$)') x_gtru_8,y_tru_8  ! stn lon,lat
ccc     write(*,'(1x,i5 ,$)') int(z_tru_8)                ! stn elv (m)
ccc     write(*,'(1x,f9.5 ,$)') t                         ! time (hrdr)

c       write(*,'(/,1x,a,1x,i5,$)') 'stn |#',ngtru
        write(*,'(/,1x,a,1x,i5,$)') 'Stn:',ngtru

c       write(*,'(1x,a,$)') '| xyz:'
        write(*,'(1x,f7.2,1x,f6.2,$)') x_gtru_8,y_tru_8  ! stn lon,lat
        write(*,'(2x,a,$)') rpid(1:4)                    ! gr stn id (rpid)
        cstr=rpid                                  ! -- equiv to rstr_8
        write(*,'(1x,i5,$)') int(z_tru_8)            ! stn elv (m)

c       write(*,'(1x,a,$)') 't_ob:'
c       write(*,7) (itob(i),i=1,3),(itob(i),i=5,8)   ! low prof time

c- Pres,Temp,Winds
        write(*,'(1x,a        ,$)') '|P'
c       write(*,'(1x     ,f6.1,$)')       p_tru_8/1.d2  ! pressure (mb)
        write(*,'(1x     ,f6.1,$)')       p_tru_8       ! pressure (mb)
c       write(*,'(1x,a        ,$)') '|T'
        write(*,'(1x     ,f5.2,$)')      t_tru_db_8-273.15 ! temp db (c)
        write(*,'(     1x,f5.2,$)')      t_tru_dp_8-273.15 ! temp dp (c)
c       write(*,'(1x,a,$)') '|W'
        write(*,9)                wd_tru_8,ws_tru_8      ! wind dir,spd
c       write(*,'(1x,a,$)') '|uv'
        write(*,'(2(1x,f5.1,$))') w_u_8,w_v_8       ! wind u-, v- (m/s)

c- Times
c       write(*,'(/,1x,a,$)') 'stn_t:'
c       write(*,'(1x,a,$)') '|t'
c       write(*,8) (idat(i),i=1,3),(idat(i),i=5,6)  ! cycle time
c...jaw -- dt_tru_8 needs work
        write(*,8) (itru(i),i=1,3),(itru(i),i=5,6)  ! matching gtru time
        write(*,'(1x     ,f8.5,$)') dt_tru_8        ! dt: cycle - gtru
c       write(*,'(1x,a,1x,f5.3,$)') '|dt',tdif0
c       write(*,'(1x,a,1x,f8.5,$)') '|td0',tdif0
        write(*,'(1x     ,f8.5,$)') tdif0           ! dt: low prof - gtru

        write(*,'(1x     ,f8.5,$)') tdif0-dt_tru_8  ! dt: low prof - cycle

    7 format (1x,i4,2i2.2,".",3i2.2,'.',i3.3,$)   ! ccyymmdd.hhmiss.msec
    8 format (1x,i4,2i2.2,".",2i2.2         ,$)   ! ccyymmdd.hhmi

c-jaw #gt_mtch sometimes = 0  -- mod: make stn tbl from gtru file
        write(*,'(1x,a,i3,$)') '|| #gt=', jm

        if ( ( nr.eq.nrmin .or. nr.eq.nrmax ) .and. .false. ) 
     &    write(*,'(/,3x,a,2x,a,4(2x,i5),$)') '-> ',
     &    rpid,jm,jmmin,jmmax,nr

        write(*,*)
      endif ! wr_stn



c-- Write output prepbufr profile records

      if ( wr_prof ) then 

c-open output message as needed
      call openmb(lunout,ctyp,idate)

c AIRCAR => HEADR ACID {PRSLEVLA}

c HEADR =>
C     SID XOB YOB DHR ELV TYP T29 TSB ITP SQN PROCN RPT TCOR <RSRD_SEQ>
c     write(*,'(/,a,$)') '-headr'
c     rval_8(1)=hdr_8(1)
c     write(*,*) cval(1),(hdr_8(i),i=2,13)
ccc   call ufbseq(lunout,hdr_8,MXNM,1,iret,'HEADR')
      usr_8=bmiss
      cnem='SID XOB YOB DHR ELV TYP T29 TSB ITP SQN PROCN RPT TCOR'
      call ufbint(lunpro,usr_8,MXNM,1,iret,cnem)
c--populate HEADR output mnems w/ ground stn info
c     usr_8(1)=bmiss                                ! stn id -> SID
      usr_8(2)=x_tru_8                              ! stn lat -> XOB
      usr_8(3)=y_tru_8                              ! stn lon -> YOB
c     usr_8(4)=dt_tru_8                             ! stn time offset -> DHR
      usr_8(4)=dtlow_8                              ! low obs time -> DHR
      if(ibfms(z_tru_8).eq.0) usr_8(5)=int(z_tru_8) ! stn SELV -> ELV

c -- synch RPT w/ ground stn time (not lev1 from prof)..done to here JAW
      rpt_8=dt_tru_8 ; if ( rpt_8.lt.0 ) rpt_8=rpt_8+24.
      usr_8(12)=rpt_8                  ! t_tru - t_cycle (24hr) -> RPT

      call ufbint(lunout,usr_8,MXNM,1,iret,cnem)

c RSRD_SEQ == RSRD EXPRSRD
      cnem='RSRD EXPRSRD'     
      call ufbint(lunpro,usr_8,MXNM,1,iret,cnem)

c SID (gr stn ID), ACID (tail #)
      cnem='SID ACID'                  ! in: SID == tail#; ACID == flt#
      call ufbint(lunpro,usr_8,MXNM,1,iret,cnem)

      usr_8(2)=usr_8(1)                  ! ACID <- tail# (SID)
      usr_8(1)=rstr_8                    ! SID <- gr stn ID (rpid)
      call ufbint(lunout,usr_8,MXNM,1,iret,cnem)

c PRSLEVLA =>
c   RCT ROLF MSTQ IALR CAT 
c       <P___INFO> <Q___INFO> <T___INFO> <Z___INFO> <W___INFO>
c       <DRFTINFO> <ACFT_SEQ> <TURB1SEQ> <TURB2SEQ> {TURB3SEQ}  ! ver2
c       {PREWXSEQ} {CLOUDSEQ} {AFIC_SEQ}  NRLQMS                ! ver2

      nnem=5        ! # of mnemonics
      cnem='RCT ROLF MSTQ IALR CAT'
      call ufbint(lunpro,pro2_8,MXNM, 255,nlev,cnem)
      out2_8=bmiss 
      out2_8(1:nnem,2:nlev+1)=pro2_8(1:nnem,1:nlev)
      out2_8(5,1)=0                ! CAT = 0 == surface report
      nlev=nlev+1
      call ufbint(lunout,out2_8,MXNM,nlev,iret,cnem)

c P___INFO == [P__EVENT] <P__BACKG> <P__POSTP>
c == [POB PQM PPC PRC] <POE PFC <PFCMOD>> <PAN <PCL PCS> POETU PVWTG
C     PVWTA>
      nnem=13       ! # of mnemonics
      cnem='POB PQM PPC PRC POE PFC PFCMOD PAN PCL PCS ' // 
     &     'POETU PVWTG PVWTA'
      call ufbint(lunpro,pro2_8,MXNM, 255,nlev,cnem)
      out2_8=bmiss
      out2_8(1:nnem,2:nlev+1)=pro2_8(1:nnem,1:nlev)
c     out2_8(1,1)=p_tru_8/1.d2       ! p_tru_8 (mb)
      out2_8(1,1)=p_tru_8            ! p_tru_8 (mb)
      nlev=nlev+1
      call ufbint(lunout,out2_8,MXNM,nlev,iret,cnem)
c     write(*,'(a,$)') 'pr'; write(*,'(f8.2,$)')(pro2_8(1,j),j=1,nlev)
c     write(*,'(/,a,$)') 'pr'; write(*,'(f8.2,$)')(out2_8(1,j),j=1,nlev)

c Q___INFO == [Q__EVENT] TDO <Q__BACKG> <Q__POSTP>
c == [QOB QQM QPC QRC] TDO <QOE QFC <QFCMOD>> <QAN <QCL QCS> QOETU
c     QVWTG QVWTA ESBAK>
      nnem=15
      cnem='QOB QQM QPC QRC TDO QOE QFC QFCMOD QAN QCL QCS ' //
     &     'QOETU QVWTG QVWTA ESBAK'
      call ufbint(lunpro,pro2_8,MXNM, 255,nlev,cnem)
      out2_8=bmiss
      out2_8(1:nnem,2:nlev+1)=pro2_8(1:nnem,1:nlev)
                                    ! t_tru_dp_8 = gtru_p_8(TMDP)-273.15
      if(ibfms(t_tru_dp_8).eq.0) out2_8(5,1)=t_tru_dp_8-273.15
      nlev=nlev+1
      call ufbint(lunout,out2_8,MXNM,nlev,iret,cnem)

c T___INFO == [T__EVENT] TVO <T__BACKG> <T__POSTP>
c  == [TOB TQM TPC TRC] TVO <TOE TFC <TFCMOD>> <TAN <TCL TCS> TOETU
c      TVWTG TVWTA>

      nnem=14
      cnem='TOB TQM TPC TRC TVO TOE TFC TFCMOD TAN TCL TCS ' // 
     &     'TOETU TVWTG TVWTA'
      call ufbint(lunpro,pro2_8,MXNM, 255,nlev,cnem)
      out2_8=bmiss
      out2_8(1:nnem,2:nlev+1)=pro2_8(1:nnem,1:nlev)
                                    ! t_tru_db_8 = gtru_p_8(TMDB)-273.15
      if(ibfms(t_tru_db_8).eq.0) out2_8(1,1)=t_tru_db_8-273.15
      nlev=nlev+1
      call ufbint(lunout,out2_8,MXNM,nlev,iret,cnem)

c Z___INFO == [Z__EVENT] <Z__BACKG> <Z__POSTP>
c  == [ZOB ZQM ZPC ZRC] <ZFC <ZFCMOD>> <ZAN <ZCL ZCS>>
      nnem=9
      cnem='ZOB ZQM ZPC ZRC ZFC ZFCMOD ZAN ZCL ZCS'
      call ufbint(lunpro,pro2_8,MXNM, 255,nlev,cnem)
      out2_8=bmiss
      out2_8(1:nnem,2:nlev+1)=pro2_8(1:nnem,1:nlev)
      out2_8(1,1)=ishgt(lmin)                  ! sfc hgt -> ZOB(1)
      nlev=nlev+1
      call ufbint(lunout,out2_8,MXNM,nlev,iret,cnem)

c W___INFO == [W__EVENT] [DF_EVENT] <W__BACKG> <W__POSTP> 
c  == [UOB VOB WQM WPC WRC] [DDO FFO DFQ DFP DFR] 
c        <WOE UFC VFC <UFCMOD VFCMOD>> <UAN VAN <UCL UCS VCL VCS> 
c           WOETU WVWTG WVWTA RF10M>

      nnem=10
      cnem='UOB VOB WQM WPC WRC DDO FFO DFQ DFP DFR '
      call ufbint(lunpro,pro2_8,MXNM, 255,nlev,cnem)
      out2_8=bmiss
      out2_8(1:nnem,2:nlev+1)=pro2_8(1:nnem,1:nlev)
      out2_8(1,1)=w_u_8       ! wind u- == f(WDIR,WSPD) (m/s)
      out2_8(2,1)=w_v_8       ! wind v- == f(WDIR,WSPD) (m/s)
      out2_8(6,1)=wd_tru_8    ! ddo = wd_tru_8 = gtru_p_8(WDIR)
      out2_8(7,1)=ws_tru_k_8  ! ffo = ws_tru_k_8 = gtru_p_8(WSPD) (knots)
                              !  (1 m/s = 1.944 knots)
      nlev=nlev+1
      call ufbint(lunout,out2_8,MXNM,nlev,iret,cnem)

      nnem=15
      cnem='WOE UFC VFC UFCMOD VFCMOD UAN VAN UCL UCS VCL VCS ' //
     &     'WOETU WVWTG WVWTA RF10M'
      call ufbint(lunpro,pro2_8,MXNM, 255,nlev,cnem)
      out2_8=bmiss
      out2_8(1:nnem,2:nlev+1)=pro2_8(1:nnem,1:nlev)
c     no changes to surface level in this sequence
      nlev=nlev+1
      call ufbint(lunout,out2_8,MXNM,nlev,iret,cnem)

c <DRFTINFO> <ACFT_SEQ> <TURB1SEQ> <TURB2SEQ> =>
c  == XDR YDR HRDR  PCAT POAF  TRBX  TRBX10 TRBX21 TRBX32 TRBX43
      nnem=10
      cnem='XDR YDR HRDR PCAT POAF TRBX TRBX10 TRBX21 TRBX32 TRBX43'
      call ufbint(lunpro,pro2_8,MXNM, 255,nlev,cnem)
      out2_8=bmiss
      out2_8(1:nnem,2:nlev+1)=pro2_8(1:nnem,1:nlev)
      out2_8(1,1)=x_tru_8                          ! stn lat -> XDR(1)
      out2_8(2,1)=y_tru_8                          ! stn lon -> YDR(1)
      out2_8(3,1)=dt_tru_8                         ! stn dt -> HRDR(1)
      nlev=nlev+1
      call ufbint(lunout,out2_8,MXNM,nlev,iret,cnem)


c {TURB3SEQ} => DGOT HBOT HTOT
      nnem=3
      cnem='DGOT HBOT HTOT'
      call ufbint(lunpro,pro2_8,MXNM, 255,nlev,cnem)
      out2_8=bmiss
      out2_8(1:nnem,2:nlev+1)=pro2_8(1:nnem,1:nlev)
c     no changes to surface level in this sequence
      nlev=nlev+1
      call ufbint(lunout,out2_8,MXNM,nlev,iret,cnem)
c ...jaw -- this needs updating for full replication output


c {PREWXSEQ} => PRWE
      nnem=1
      cnem='PRWE'
      call ufbint(lunpro,pro2_8,MXNM, 255,nlev,cnem)
      out2_8=bmiss
      out2_8(1:nnem,2:nlev+1)=pro2_8(1:nnem,1:nlev)
c     no changes to surface level in this sequence
      nlev=nlev+1
      call ufbint(lunout,out2_8,MXNM,nlev,iret,cnem)
c ...jaw -- this needs updating for full replication output

c {CLOUDSEQ} =>
c  == VSSO CLAM CLTP  HOCB HOCT
      nnem=5
      cnem='VSSO CLAM CLTP HOCB HOCT'
      call ufbint(lunpro,pro2_8,MXNM, 255,nlev,cnem)
      out2_8=bmiss
      out2_8(1:nnem,2:nlev+1)=pro2_8(1:nnem,1:nlev)
c     no changes to surface level in this sequence
      nlev=nlev+1
      call ufbint(lunout,out2_8,MXNM,nlev,iret,cnem)
c ...jaw -- this needs updating for full replication output
 

c {AFIC_SEQ} == AFIC HBOI HTOI
      nnem=3
      cnem='AFIC HBOI HTOI'
      call ufbint(lunpro,pro2_8,MXNM, 255,nlev,cnem)
      out2_8=bmiss
      out2_8(1:nnem,2:nlev+1)=pro2_8(1:nnem,1:nlev)
c     no changes to surface level in this sequence
      nlev=nlev+1
      call ufbint(lunout,out2_8,MXNM,nlev,iret,cnem)
c ...jaw -- this needs updating for full replication output


c NRLQMS
      nnem=1
      cnem='NRLQMS'
      call ufbint(lunpro,pro2_8,MXNM, 255,nlev,cnem)
      out2_8=bmiss
      out2_8(1:nnem,2:nlev+1)=pro2_8(1:nnem,1:nlev)
c     no changes to surface level in this sequence
      nlev=nlev+1
      call ufbint(lunout,out2_8,MXNM,nlev,iret,cnem)
c ...jaw -- this needs updating for full replication output


cjaw -- for writing out event stack
cjaw  *** DKeyser suggests using UFBCPY then UFBINT ***
cjaw  cnem='NRLQM'
cjaw  call ufbevn(lunpro,pro3_8,MXNM, 255,nlev,nevn,cnem)
cjaw  call ufbseq(lunout,pro3_8,MXNM,nlev,iret,nevn,'NRLQMSEQ')
cjaw  write(*,*) 'nrlqm',nlev,nevn,iret
cjaw -- for writing out event stack

c...jaw -- done to here

c *** to repeat events, use UFBIN3 ***
c ************************************
cjaw  cnem='POB PQM PPC PRC'
cjaw  call ufbin3(lunpro,pro3_8,20,10000,255,nlev,nevn,cnem)
cjaw  write(*,'(1x,a,2i3,$)') 'P_ev',nlev,nevn

cjaw  do j=1,nlev
cjaw  write(*,'(/,i2,$)')  j
cjaw  write(*,'(i2,4(1x,f7.2,$))') (k,(pro3_8(i,j,k),i=1,4),k=1,nevn)
cjaw  enddo ! j=1,nlev
c ************************************
c *** to repeat events, use UFBIN3 ***



c-commit subset/report to output file
      call writsb(lunout) 

      nro=nro+1                ! increment counter, # rpts written out

      endif ! wr_prof


c--Exit on print or outfile thresholds
      nr1max=4
      nr1max=0
      if ( nr1 .ge. nr1max .and. nr1max.ne.0 ) exit ! debug

      nromax=12
      nromax=0
      if ( nro .ge. nromax .and. nromax.ne.0 ) exit ! debug


c--- end of read subset loop -------------------------------------------
      end do ! while ireadsb

c-make exact copy to output
      if ( .false. ) then  ! skip it (copymg)
        call copymg(lunpro,lunout)
      endif ! skip it (copymg)

      nr1max=4
      nr1max=0
      if ( nr1 .ge. nr1max .and. nr1max.ne.0 ) then
        write(*,*)
        write(*,*) ' -exiting w/ nr1=',nr1
        exit ! debug
      endif ! nr1 > nr1max

      nromax=12
      nromax=0
      if ( nro .ge. nromax .and. nromax.ne.0 ) then
        write(*,*)
        write(*,*) ' -exiting w/ nro=',nro
        exit ! debug
      endif ! nro > nromax

      if ( nmsg1.ge.  3 .and. .false.) then 
        write(*,*)
        write(*,*) ' -exiting w/ nmsg1=',nmsg1
        exit ! debug
      endif ! nmsg1 > 3

      end do ! while ireadmg

      if ( wr_prof) call closmg(lunout)
      if ( wr_prof) call closbf(lunout)

      write(*,*) ! closing linefeed, debug?
      write(*,*) '# prof considered =',nr1
      if (nhi.ne.0) write(*,*) '# rejected: hgt (maxhgt) =',nhi,LOW
      if (nrare.ne.0) write(*,*) '# rejected: hgt (maxdP) =',nrare,RARE
      if (nfar.ne.0) write(*,*) '# rejected: far (maxdXY) =',nfar,rNEAR
      if (nt3.ne.0) write(*,*) '# rejected: typ 3xx =',nt3
      if (njm0.ne.0) write(*,*) '# rejected: no stn match(njm0):',njm0
      write(*,*) '# prof output=',nro

      write(*,'(" max dxy=",F12.5)'), dxymax

      endif ! false/true skip it (lunpro)


c-number of matching stations found in gtru
c     write(*,*) 'nrmin,jmmin=',nrmin,jmmin
c     write(*,*) 'nrmax,jmmax=',nrmax,jmmax

      write(*,*) 'Normal end of program PREPOBS_PREPACPF'
      CALL W3TAGE('PREPOBS_PREPACPF')
      stop

 901  write(*,*) 'error alloc gtru_*, stat=',i
      write(*,*) '######ABNORMAL end of program PREPOBS_PREPACPF'
c     stop 99
      CALL W3TAGE('PREPOBS_PREPACPF')
      call errexit(99)

      end ! program PREPOBS_PREPACPF


c ###################################################################
c                            function gcirc_qc
c ###################################################################
c
      function gcirc_qc(rlat1,rlon1,rlat2,rlon2)
c
c This fcn computes great circle distances using the Haversine formula.
c Reference: http://www.census.gov/cgi-bin/geo/gisfaq?Q5.1
c Programmer: P.M. Pauley 2/24/2000
c
c Additional reference (JAW 9/13/2007):
c   http://www.movable-type.co.uk/scripts/GIS-FAQ-5.1.html.
c
      implicit none
c
      real       pi,deg_rad,radius
      parameter (pi = 3.14159274, deg_rad = pi/180.) ! conversion factor
      parameter (radius = 6371229.)                  ! earth's radius(m)
c
      real         gcirc_qc     ! great circle distance
      real         rlat1        ! first latitude (degrees)
     $,            rlat2        ! second latitude (degrees)
     $,            rlon1        ! first longitude (degrees)
     $,            rlon2        ! second longitude (degrees)
      real         dlon         ! difference in longitude / 2 (radians)
     $,            dlat         ! difference in latitude / 2 (radians)
     $,            arg          ! argument for the arcsin
c
      dlon = (rlon2 - rlon1) * deg_rad * 0.5
      dlat = (rlat2 - rlat1) * deg_rad * 0.5
c
c What if longitudes are equal?
c -----------------------------
      if(ifix(rlon1*100.0).eq.ifix(rlon2*100.0)) then
        gcirc_qc = radius * abs(rlat2 - rlat1) * deg_rad
c
c What if latitudes are equal?
c ----------------------------
      elseif(ifix(rlat1*100.0).eq.ifix(rlat2*100.0)) then
        arg = abs(cos(rlat1*deg_rad) * sin(dlon))
        gcirc_qc = radius * 2.0 * asin(min(1.0,arg))
c
c What if neither are equal?
c --------------------------
      else
        arg = (sin(dlat))**2
     $      + cos(rlat1*deg_rad) * cos(rlat2*deg_rad) * (sin(dlon))**2
        gcirc_qc = radius * 2.0 * asin(min(1.0,sqrt(arg)))
      endif
c
      return
      end  ! function gcirc_qc
c
C-----------------------------------------------------------------------

