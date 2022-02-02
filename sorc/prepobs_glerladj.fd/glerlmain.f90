program prepobs_glerladj

!$$$ Main Program Documentation Block
!
! Main Program: prepobs_glerladj
!   Programmer: D.Keyser/S.Levine        Org: NCEP/EMC   Date: 2017-07-14
!
! Abstract:  Performs GLERL adjustments to observations in RTMA/URMA prepbufr files.
!    It is intended to be invoked as a step within prepobs_prepdata observation
!    processing. It reads in a PREPBUFR file containing all reports, and also
!    a dictionary of stations.  Stations in the dictionary will either be re-located
!    or have a new, pseudo-ob generated based on original observed value and water 
!    temperature.  If a station is on the wrong side of the RTMA land/sea mask, it
!    will have it's observation moved to a site on the correct side.  In this case it's 
!    lat and lon withh change, but its other observed values will not change (however 
!    changes will be noted in the reason codes).  Certain land
!    observations are used to generate over-water representative conditions.  New obs,
!    with a new type, will be located at a point over the water, and the original ob
!    will remain at it's original point.  All relevant stations are near the Great Lakes.
!    The formula used to adjust the observations originated with John Kelley, formerly of GLERL.
!    The code for obs adjustment originated in glofs v1.0.3 (see edit_sfcmarobs_new.f), 
!    but had to be modified to work with prepbufr and with using both land/water 
!    obs in the RTMA/URMA.
!
! Program History Log:
!   2017-07-14  levine  new code
!
! Usage:
!  Input Files:
!    Unit 11 - water temperatures file (".../CYMD/wtxtbul/glsea-temps.dat")
!    Unit 12 - pre-generated observation 'dictionary' ("fix/glerldict.lmd")
!    Unit 13 - PREPBUFR file containing all obs, prior to any processing by this program
!    Unit 15 - text namelist file ("./dateinfo_input")
!    Unit 91 - copy of PREPBUFR file in Unit 13 (temorary, working file)
!
!  Output Files:
!    Unit 06 - Standard output print
!    Unit 51 - PREPBUFR file identical to input except containing GLERL adjusted obs
!
!   Subprograms called:
!     Unique:    - finddict2  land2water  spddir  td_to_q  uvspd  uzl  water2land 
!     LIBRARY:
!       W3NCO:   - w3fs13  w3tagb    w3tage  errexit
!       BUFRLIB: - closbf  closmg    copybf  copymg  datelen  openbf  openmb  readmg  
!                  readsb  setbmiss  ufbcnt  ufbcpy  ufbevn   ufbint  ufbpos  ufbqcd
!                  writsb 
!
!   Exit states:
!     Cond =   0 - successful run
!             31 - unable to read in lake water temperatures
!             32 - unable to read in station dictionary
!             33 - 2 piece ob with conflicting/unexpected ob type arrangement
!             34 - too many iterations on Z0 in subroutine uzl - check meteorological data
!
!   Remarks:
!      Input Namelist switches (namelist &dateinfo_input):
!            inyear     - year
!           inmonth     - month
!             inday     - day
!
! In event of failure, obsproc_prep should be re-run with the GLERL adjustment turned off
! (ie set GLERLBUFR to "NO" in prepobs_makeprepbufr.sh)
!
! Attributes:
!   Language: FORTRAN 90
!   Machine:  NCEP WCOSS
!
!$$$

!! DAK: The sequence numbers (SQN) for the new mass/wind subset pairs of type
!!      x96, x97, x98, x99 are currently the same as the original mass/wind
!!      subset pair read from the input PREPBUFR file. These new mass/wind
!!      subset pairs should have a unique SQN value (for the message type they
!!      are in since SQN identified as unique mass/wind pair!!

  use glerl_dictionary

  implicit none

  ! DAK: Need dynamic allocation on dimension of imatch

  integer(4),dimension(8000,600)::imatch
  integer(4)::isub,nmsub
  real(8)::sqn_8,sqn_8_last
  integer(4),dimension(dictmax)::types,lake,date,mask,act,anht
  integer(4)::type,stnum,lnum,dsize,iout,ifound,a,b,c,typ,idate,idatd,iret,nlevs,i,j,k
  integer(4)::t29,readvar,sbvar,actflag,maskflag
  integer(4)::recnum,subnum,amsg,asub,bmsg,bsub,amsg_last,asub_last,amsg_1
  integer(4),dimension(dictmax)::dirflaga,dirflagb,dirflagc,dirflagd,dirflage,dirflagf,dirflagg,dirflagh,elev
  integer(4)::ireadmg,ireadsb,year,doy,doyb,inyear,inmonth,inday
  real(8),dimension(6)::watertemp
  real(8),dimension(18)::arra,arrb
  real(8),dimension(255,20)::tpc
  real(8),dimension(2,255,20)::tobs
  real(8),dimension(4)::tmparr,mstarr
  real(8),dimension(3)::hdrarr
  real(8),dimension(5)::wndarr,sobarr
  real(4),dimension(dictmax)::wlat,wlon,llat,llon,olat,olon
  real(8)::bmiss,getbmiss
  real(4)::vtcd,pcode
  real(4)::newlat,newlon,tdiff,newtyp,otyp,rpt,newrpt,newtypmass,windtyp
  real(4)::wlatnew,wlonnew,llatnew,llonnew
  real(4)::lat,lon,temp,dewpt,uob,vob,wspd,wdir,wtemp,sob,ddo
  real(4)::newtemp,newdpt,newu,newv,newspd,newdir,newq!,newlat,newlon
  real(4)::tqm,qqm,wqm,qob,dfq,pob,pqm
  real(4)::cd,ch,z0,fl,windht !vars needed for conversion to 10 m wind speed
  real(4),external::uz
  character(len=8)::stnid(dictmax),ctyp,sid,sidb,newsid,ctyp0,ctypd,tmpsid
  character(len=20),dimension(dictmax)::desc
  character(len=5)::tempchar
  character(len=80)::nems
  character(len=15)::tmpnems,mstnems,hdrnems
  character(len=30)::wndnems,sobnems
  logical::match,double,dupob,convert

  namelist/dateinfo/inyear,inmonth,inday

  data nems /"SID XOB YOB RPT TYP TOB TDO UOB VOB TQM QQM WQM QOB SOB DDO DFQ POB PQM"/
  data hdrnems /"TYP YOB XOB"/
  data tmpnems /"TOB TQM TPC TRC"/
  data mstnems /"QOB QQM QPC QRC"/
  data wndnems /"UOB VOB WQM WPC WRC"/
  data sobnems /"SOB DDO DFQ DFP DFR"/

  call w3tagb('PREPOBS_GLERLADJ',2017,0178,0070,'NP22')

  CALL SETBMISS(10E8_8)
  bmiss=getbmiss()
  print'(1X)'
  print'(" BUFRLIB value for missing is: ",G0)', bmiss
  print'(1X)'

  open(15,file="dateinfo_input",form="formatted")
  read(15,dateinfo)
  close(15)

  !read water temperatures
  print*, "Reading water temperatures..."
  do c=1,10
     read(11,'(A5)') tempchar
  enddo
  do c=1,99999
     read(11,152,end=90,err=80) year,doy,watertemp(1),watertemp(2),watertemp(3),watertemp(4),watertemp(5),watertemp(6)
     ! 1=Lake Superior, 2=Lake Michigan, 3=Lake Huron, 4=Lake Erie, 5=Lake Ontario 6=Lake St. Clair (not used)
  enddo
  ! If error in reading in water temperatures, fail and return original prepbufr file
80 print*, "FATAL ERROR: Unable to read in lake water temperatures!"
  call w3tage('PREPOBS_GLERLADJ')
  call errexit(31)
152 format (I4,1X,I3,2X,6(F6.2,2X))
111 format (A8,1X,I1,1X,I1,1X,A20,1X,I1,1X,I1,1X,6(F6.2,1X),I3,1X,I2,2X,8I1)
112 format (A8,1X,I1,1X,I1,1X,A20,1X,I1,1X,I1,1X,F5.2,2X,F6.2,2X,F6.2,2X,F6.2,2X,I2,1X,I3,2X,8I1)
90 close(11)
!check that day of year matches
  call w3fs13(inyear,inmonth,inday,doyb)
  if (doy.ne.doyb) then
     print*, "WARNING: Lake temperature date does not equal current date.  Current DOY, lake temp DOY=",doy,doyb
  endif

  !initialize dictionary values
  stnid(:)="MISSING "
  wlat(:)=bmiss
  wlon(:)=bmiss
  llat(:)=bmiss
  llon(:)=bmiss
  olat(:)=bmiss
  olon(:)=bmiss
  elev(:)=misint

  print*, "Reading in changeable stations from dictionary..."
  !read dictionary and store in array
  readdict:  do a=1,99999 !size of newerdict
     read(12,111,end=50,err=55) stnid(a),types(a),lake(a),desc(a),mask(a),act(a),olat(a),olon(a),llat(a),llon(a),wlat(a),wlon(a),elev(a),anht(a),dirflaga(a),dirflagb(a),dirflagc(a),dirflagd(a),dirflage(a),dirflagf(a),dirflagg(a),dirflagh(a) !err=55
     if (stnid(a).eq."XXXXXXXX") then
        goto 50
     endif
     ! LAKE NUMBER ADJUSTMENTXS
     ! change flag to reflect how lake obs are being read in
     ! Georgian Bay is counted as Lake Huron
     ! Lake St. Clair does not exist in land/sea mask, so counting as Lake Erie
     ! 1=Lake Superior, 2=Lake Michigan, 3=Lake Huron, 4=Lake Erie, 5=Lake Ontario
     if (lake(a).eq.3) then
        lake(a) = 4
     elseif (lake(a).eq.6) then
        lake(a) = 2
     endif
     lake(a)=lake(a)+1
     if (lake(a).gt.3) then
        lake(a)=lake(a)-1
     endif
  enddo readdict
  close(12)
! If station dictionary is not read in properly, fail and return original prepbufr file
55 print*, "FATAL ERROR: Unable to read in station dictionary!"
  call w3tage('PREPOBS_GLERLADJ')
  call errexit(32)
50 continue
  dsize=a !-1
  print*, "Number of stations in dictionary: ",dsize

  bmiss=getbmiss()
!vvvvvvvvvvvvvvvvvvvvvvvvvvvvv
! this section reads through PREPBUFR file to mark subsets that are "orphans"
!  (i.e., wind or mass "pieces" that have no partner - these get imatch=0)
! otherwise, 1st of 2 pieces (subset containing wind) gets imatch=1
!            2nd of 2 pieces (subset containing mass) gets imatch=2  
! info will be used later when we march through PREPBUFR file to actually
! generate the output GLERL adjusted PREPBUFR file

! open input PREPBUFR file, set date/time to 10-digit default
  call openbf(13,'IN',13)
  call datelen(10)

  ctyp0="abcd1234" !random default
  imatch = 0
  isub = 0
  amsg_last = 0
  asub_last = 0
  do while(ireadmg(13,ctyp,idate).eq.0)
     if (ctyp.ne.ctyp0) then
        sqn_8_last = -99
     endif
     ctyp0=ctyp
        ! skip message types that are not part of GLERL adjustment
     if (ctyp.ne."ADPSFC  ".and.ctyp.ne."MSONET  ".and.ctyp.ne."SFCSHP  ") cycle
     do while (ireadsb(13).eq.0)
        call ufbcnt(13,amsg,asub)
        isub = isub+1
        sqn_8 = bmiss
        call ufbint(13,sqn_8,1,1,iret,'SQN')
        if(isub.eq.1) then
           amsg_1 = amsg
           sqn_8_last = sqn_8
           amsg_last = amsg
           asub_last = asub
           imatch(amsg,asub) = 0
           cycle
        endif
 ! SQN (sequence number) is the same in two subsets that are a report pair
        if(sqn_8.eq.sqn_8_last) then
           imatch(amsg_last,asub_last) = 1
           imatch(amsg,asub) = 2
        else
           imatch(amsg,asub) = 0
        endif
   100  format('isub,isub-1,sqn_8,sqn_8_last,amsg,asub,amsg_last,asub_last,imatch(amsg,asub),imatch(amsg_last,asub_last): ',i7,i7,i6,i6,i10,i5,i8,i6,i5,i4)
        sqn_8_last = sqn_8
        amsg_last = amsg
        asub_last = asub
     enddo
  enddo
  call closbf(13)
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

! Copy input PREPBUFR file in unit 13 to unit 91, latter will be read in tamdem
!  with former simply to hold 1st subset (wind piece) in cases of a report pair
!  in internal memory
  call copybf(13,91)

! open input (units 13 and 91) and output (unit 51) PREPBUFR files
  print*, "Opening input prepbufr files..."
  call openbf(13,'IN',13)
  ! use internal dctnry from unit 13 for unit 91 to avoid table mismatches later
  call openbf(91,'IN',13)
  ! Obtain program code (VTCD) associated with "VIRTMP" step
  call ufbqcd(13,'VIRTMP',vtcd)
  print*, "Opening output prepbufr file..."
  call openbf(51,"OUT",13)
  print*, "Prepbufr vtcd value=",vtcd
  call ufbqcd(13,'GLERL',pcode)
  print*, "GLERL program code=",pcode

  ctyp0="abcd1234" !random default
  print*, "Reading through original prepbufr file plus its copy..."
  arra(:)=bmiss !initialize default
  arrb(:)=bmiss
  recnum=0
  print*, "==> Unless otherwise noted Ob found in dictionary was taken over land"

  moremsg: do while(ireadmg(13,ctyp,idate).eq.0)
!-------------------------------------------------------------
! read next message in unit 13 (original input PREPBUFR file)
!-------------------------------------------------------------
     if (ctyp.ne.ctyp0) print*, "New BUFR message type:",ctyp
! simply copy messages that are not part of GLERL adjustment
     if (ctyp.ne."ADPSFC  ".and.ctyp.ne."MSONET  ".and.ctyp.ne."SFCSHP  ") then
        if (ctyp0 .eq."ADPSFC  ".or.ctyp0.eq."MSONET  ".or.ctyp0.eq."SFCSHP  ") call closmg(51)
        call copymg(13,51)
        ctyp0=ctyp
        cycle moremsg
     endif

     call openmb(51,ctyp,idate)
     if (ctyp.ne.ctyp0) print*, "Opening new message due to message type change from ",ctyp0," to ",ctyp," ..."

     moresubs: do while (ireadsb(13).eq.0)
!-------------------------------------------------------------
! read next subset in unit 13 (original input PREPBUFR file)
!-------------------------------------------------------------
! amsg & asub identify location of subset read into unit 13 internal memory
           call ufbcnt(13,amsg,asub)
! using amsg & asub, read same subset into unit 91 internal memory
           call ufbpos(91,amsg,asub,ctypd,idatd)
           call ufbint(13,arra,18,1,iret,nems)
           write(sid,'(a)') arra(1)
!ppppp
!!!        print *, 'new subset read: msgn, subn, sid: ',amsg,asub,sid
!ppppp
           lat=real(arra(3),4)
           if (arra(2).gt.180) then
              lon=real(arra(2),4)-360
           else
              lon=real(arra(2),4)
           endif
! see if station id associated with this subset is in dictionary
           call finddict2(sid,stnid,lat,lon,olat,olon,stnum)
           searchdict: if (stnum.eq.misint) then
              ifound = 0
           else
              ifound = 1
              actflag=act(stnum)
              maskflag=mask(stnum)
              llatnew=llat(stnum)
              llonnew=llon(stnum)+360
              wlatnew=wlat(stnum)
              wlonnew=wlon(stnum)+360
              !assign aneometer height based on dictionary.  Default to 10 m if missing.
              if (anht(stnum).eqv."  ") then
                 windht=10.
              else
                 windht=anht(stnum)
              endif
              if (lake(stnum).eq.9) then
                 wtemp=bmiss
              else
                 wtemp=real(watertemp(lake(stnum)),4)
              endif
           endif searchdict
           indict: if (ifound.eq.0) then
              ! station id associated with this subset NOT found in dictionary
              ! copy subset as is to output PREPBUFR file and move to next subset
!ppppp
!!!           print*, "Station NOT found in dictionary:",sid
!ppppp
              call ufbcpy(13,51)
              call writsb(51)
              arra(:)=bmiss !set arra back to empty
              arrb(:)=bmiss !set arrb back to empty (just to be safe)
              cycle moresubs
           else
              ! station id associated with this subset IS FOUND in dictionary, attempt GLERL adjustment
              print'("Station found in dictionary:",A8,"; if nothing more added, this report was not adjusted/converted/moved, it was simply copied to output as is")',sid

              if(imatch(amsg,asub).eq.0) then
                 ! initial PREPBUFR file read showed this subset is an orphan (wind or mass piece
                 !  that has no partner), copy as is to output PREPBUFR file, move to next subset
                 call ufbcpy(13,51)
                 call writsb(51)
                 cycle moresubs
              endif
!ppppp
!!!           print *, 'amsg,asub,imatch(amsg,asub): ',amsg,asub,imatch(amsg,asub)
!ppppp
              if(imatch(amsg,asub).eq.2) print'("Very odd situation where apparent first piece of a pair seems to be the mass piece -= investigate!!")'
!-------------------------------------------------------------------
! this (wind) subset DOES have a mass piece in the next subset
! read next (mass) subset in unit 13 (original input PREPBUFR file)
! (note that the wind subset will remain in unit 91 internal memory)
!-------------------------------------------------------------------
              call readsb(13,iret)
              newmsg: if (iret.lt.0) then !no more subsets in this message
                                          !read next message from unit 13
                 ctyp0=ctyp
                 call readmg(13,ctyp,idate,iret)
!ppppp
!!!    print *, '*READ NEW MESSAGE: ctyp, nmsub(13): ',ctyp, nmsub(13)
!ppppp
                 if (iret.eq.0.and.ctyp.ne.ctyp0) then
! if new message just read has different type than previous message, last subset
!  read (wind, still in unit 13 internal memory) is an orphan (no subsequent
!  mass piece), copy as is to output PREPBUFR file, move to next msg or subset
! NOTE: Don't think this can actually happen since initial read through PREPBUFR
!       file would have set this subset's imatch value to ZERO and it would have
!       been trapped prior to this point.
                    call ufbcpy(13,51)
                    call writsb(51)
                    print*, "New BUFR message type:",ctyp
                    ! simply copy messages that are not part of GLERL adjustment
                    if (ctyp.ne."ADPSFC  ".and.ctyp.ne."MSONET  ".and.ctyp.ne."SFCSHP  ") then
                       if (ctyp0 .eq."ADPSFC  ".or.ctyp0.eq."MSONET  ".or.ctyp0.eq."SFCSHP  ") call closmg(51)
                       call copymg(13,51)
                       ctyp0=ctyp
                       cycle moremsg
                    endif
      ! if new message type is part of GLERL adj. then ready to read next subset
                    cycle moresubs
                 endif
                 if (iret.ne.0) then
! otherwise if there are no more messages we have hit the end of the file, last
!  subset read (wind, still in unit 13 internal memory) is an orphan (no
!  subsequent mass piece) copy as is to output PREPBUFR file & exit moremsg loop
! NOTE: Don't think this can actually happen since initial read through PREPBUFR
!       file would have set this subset's imatch value to ZERO and it would have
!       been trapped prior to this point.
                    print*, "End of file!"
                    call ufbcpy(13,51)
                    call writsb(51)
                    call closmg(51)
                    exit moremsg
                 else
! otherwise, the 2nd (mass) piece of the report pair is the first subset in the
!  next message just read, read it in now
                    call readsb(13,iret)
                    call ufbint(13,arrb,18,1,iret,nems)
                 endif
              else
                 call ufbint(13,arrb,18,1,iret,nems)
                 write(sidb,'(a)') arrb(1)
              endif newmsg
           endif indict
!+++++++++++++++++++++++++++++++++++++++++++++++++
!         GLERL ADJUSTMENT WORK BEGINS
!+++++++++++++++++++++++++++++++++++++++++++++++++
           !a (piece 1, wind) and b (piece 2, mass) have both been loaded and we know they match
           ! eventually move whole blockw of code below back 3 spaces
              write(sid,'(a)') arra(1)
              if (sid(8:8).eq.'x'.or.sid(8:8).eq.'a') then
                 tmpsid = sid(:7) // ' '
                 sid=tmpsid
              endif
              lat=real(arra(3),4)
              lon=real(arra(2),4)
              rpt=real(arra(4),4)
              checktyp: if (arra(5).ge.200.and.arrb(5).lt.200) then
                 !go into events stack to pull out sensible temperature
                 tpc(:,:)=bmiss
                 call ufbevn(13,tpc,1,255,20,nlevs,'TPC')
                 tobs(:,:,:)=bmiss
                 call ufbevn(13,tobs,2,255,20,nlevs,'TOB TQM')
                 temp = bmiss
                 tqm = bmiss
                 if(tobs(1,1,1).lt.bmiss) then
! process temperature if non-missing
                 !assume we have sensible temp with first call, but we need to check and see if it's a virtual temp.
                 !If it is a virtual temp, sensible should be next in stack
!!! DAK: Since the RTMA/URMA analyses SENSIBLE temperature, we should not encode
!!!      VIRTUAL temperature into PREPBUFR prior to this code in VIRTMP in
!!!      gblevents - it's a waste as both here, and I assume in RTMA/URMA GSI,
!!!      we have to peel back events to get to SENSIBLE temperature
                 do k=1,nlevs  ! DAK: again nlevs here should always be 1
                    do j=1,20
                       temp=real(tobs(1,k,j),4)
                       tqm=real(tobs(2,k,j),4)
                       if (nint(tpc(k,j)).eq.nint(vtcd)) then
                          temp=real(tobs(1,k,j+1),4)
                          tqm=real(tobs(2,k,j+1),4)
                          !go ahead and assume sensible sits after virtual in stack.  Use program code to check this.
                          !If not, use that value anyway (because it's probably sensible), but print this warning message:
                          if (nint(tpc(k,j+1)).ne.1) then
                             print*, "WARNING: Temp in stack after virtual may not be sensible.  TPC (k,j+1) should be 1, but it's:",tpc(k,j+1)
                          endif
                          exit !found sensible temperature, exiting loop
                       endif
                       if (tpc(k,j+1)>=bmiss) exit !end of events stack, exit loop
                    enddo
                 enddo
                 endif
                 if (wtemp.ne.bmiss) then
                    tdiff = temp-wtemp
                 else
                    tdiff=bmiss
                 endif
                 dewpt = real(arrb(7),4)
                 uob = real(arra(8),4)
                 vob = real(arra(9),4)
                 qqm = real(arrb(11),4)
                 wqm = real(arra(12),4)
                 qob = real(arrb(13),4)
                 sob = real(arra(14),4)
                 ddo = real(arra(15),4)
                 dfq = real(arra(16),4)
                 pob = real(arra(17),4)
                 pqm = real(arra(18),4)
                 call td_to_q(dewpt,pob,newq)
              else
                 print*, "FATAL ERROR: 2 piece ob with conflicting/unexpected ob type arrangement for station,typea,typeb: ",sid,arra(5),arrb(5)
                 call w3tage('PREPOBS_GLERLADJ')
                 call errexit(33)
              endif checktyp
              call spddir(uob,vob,wspd,wdir,bmiss)
              if ((abs(wspd-sob).gt.0.5)) then
                 print*, "WARNING: UOB/VOB not consistent with wspd/wdir.  sid,time,uob,vob,wspd,wdir,sob,ddo=",sid,rpt,uob,vob,wspd,wdir,sob,ddo
              endif
              !DOES OB NEED TO BE CONVERTED, OR JUST MOVED?
              convert=.false.
              if (wdir.le.360.) then
                 if (wdir.lt.22.5.or.wdir.ge.337.5) then
                    if (dirflaga(stnum).eq.1) then
                       convert=.true.
                    endif
                 elseif(wdir.lt.67.5) then
                    if (dirflagb(stnum).eq.1) then
                       convert=.true.
                    endif
                 elseif (wdir.lt.112.5) then
                    if (dirflagc(stnum).eq.1) then
                       convert=.true.
                    endif
                 elseif (wdir.lt.157.5) then
                    if (dirflagd(stnum).eq.1) then
                       convert=.true.
                    endif
                 elseif (wdir.lt.202.5) then
                    if (dirflage(stnum).eq.1) then
                       convert=.true.
                    endif
                 elseif (wdir.lt.247.5) then
                    if (dirflagf(stnum).eq.1) then
                       convert=.true.
                    endif
                 elseif (wdir.lt.292.5) then
                    if (dirflagg(stnum).eq.1) then
                       convert=.true.
                    endif
                 else !if wdir.lt.337.5
                    if (dirflagh(stnum).eq.1) then
                       convert=.true.
                    endif
                 endif
              endif
              if (actflag.eq.1) then
                 if (convert) then
                    print*, "Land Ob needs to be converted:",sid
                    !convert wind speed to 10 m level if needed
                    if (windht.ne.10.) then
                       call uzl(wspd,windht,tdiff,windht,cd,ch,z0,fl)
                       wspd=uz(10.,wspd,cd,z0,fl)
                    endif
                    !used new wind speed in conversion
                    print*, "Converting station (land to water):",sid,rpt
                    call land2water(temp,dewpt,wspd,wdir,wtemp,tdiff,newtemp,newdpt,newspd,newdir,bmiss)
                    call uvspd(newspd,newdir,newu,newv,bmiss)
                    newq = bmiss
                    if(newdpt.lt.bmiss) call td_to_q(newdpt,pob,newq)
                    !copy old ob
!------------------------------------------------------------------------------
!  since copy of PREPBUFR file holds 1st (wind) piece of pair in its (unit 91)
!   internal memory, don't need to waste time calling ufbpos to place previous
!   (wind) subset in unit 13 internal memory, and then later calling ufbpos
!   again to restore following (mass) subset into unit 13 internal memory
!   (since first ufbpos not called, mass subset remains in unit 13 internal
!   memory)
!  NOTE: This will happen many times from here on in code - this is only place
!        where it is commented.
                    call ufbcpy(91,51)
                    call writsb(51)
                    call ufbcpy(13,51)
                    call writsb(51)
!------------------------------------------------------------------------------
                    !copy old ob again and convert to new ob
                    call ufbcpy(91,51)
                    wndarr(1)=newu
                    wndarr(2)=newv
                    wndarr(3)=wqm !orig wqm
                    wndarr(4)=pcode !pcode=17 (new bufr table)
                    wndarr(5)=adjust
                    sobarr(1)=newspd
                    sobarr(2)=newdir
                    sobarr(3)=dfq
                    sobarr(4)=pcode
                    sobarr(5)=adjust
                    tmparr(1)=newtemp
                    tmparr(2)=tqm
                    tmparr(3)=pcode
                    tmparr(4)=adjust
                    mstarr(1)=newq
                    mstarr(2)=qqm
                    mstarr(3)=pcode
                    mstarr(4)=adjust
                    if (arra(5).ge.200) then
                       hdrarr(1)=297.
                       hdrarr(2)=wlatnew
                       hdrarr(3)=wlonnew
                       call ufbint(51,hdrarr,3,1,iret,hdrnems)
                       call ufbint(51,wndarr,5,1,iret,wndnems)
                       call ufbint(51,sobarr,5,1,iret,sobnems)
                    else
                       print*, "FATAL ERROR: 2 piece ob with conflicting/unexpected ob type arrangement for station,typea,typeb: ",sid,arra(5),arrb(5)
                       call w3tage('PREPOBS_GLERLADJ')
                       call errexit(33)
                    endif
                    call writsb(51)
                    !copy over and adjust b array
                    print*, "Copying over original ob again...piece b..."
                    call ufbcpy(13,51)
                    if (arrb(5).lt.200) then
                       hdrarr(1)=197.
                       hdrarr(2)=wlatnew
                       hdrarr(3)=wlonnew
                       call ufbint(51,hdrarr,3,1,iret,hdrnems)
                       call ufbint(51,tmparr,4,1,iret,tmpnems)
                       call ufbint(51,mstarr,4,1,iret,mstnems)
                    else
                       print*, "FATAL ERROR: 2 piece ob with conflicting/unexpected ob type arrangement for station,typea,typeb: ",sid,arra(5),arrb(5)
                       call w3tage('PREPOBS_GLERLADJ')
                       call errexit(33)
                    endif
                    call writsb(51)
                 else
                    !ob does not need to be converted
                    !does it need to be moved?
                    if (olat(stnum).eq.llat(stnum).and.olon(stnum).eq.llon(stnum)) then
                       !ob does not need to be moved
                       !copy ob and no worries
                       call ufbcpy(91,51)
                       call writsb(51)
                       call ufbcpy(13,51)
                       call writsb(51)
                    else
                       print*, "Ob needs to be relocated to match with mask..."
                       print'("Old lat/lon, new lat/lon:",4(2x,g0))', lat,lon,llatnew,llonnew
                       !ob needs to be moved
                       !get original values
                       call ufbint(91,arra,18,1,iret,nems)
                       call ufbint(13,arrb,18,1,iret,nems)
                       !assign new lat and lon
                       tmparr(1)=temp
                       tmparr(2)=tqm
                       tmparr(3)=pcode
                       tmparr(4)=moved !moved ob
                       mstarr(1)=qob
                       mstarr(2)=qqm
                       mstarr(3)=pcode
                       mstarr(4)=moved
                       wndarr(1)=uob
                       wndarr(2)=vob
                       wndarr(3)=wqm
                       wndarr(4)=pcode
                       wndarr(5)=moved
                       sobarr(1)=sob
                       sobarr(2)=ddo
                       sobarr(3)=dfq
                       sobarr(4)=pcode
                       sobarr(5)=moved
                       call ufbcpy(91,51)
                       if (arra(5).ge.200) then
                          hdrarr(1)=298.
                          hdrarr(2)=llatnew
                          hdrarr(3)=llonnew
                          call ufbint(51,hdrarr,3,1,iret,hdrnems)
                          call ufbint(51,wndarr,5,1,iret,wndnems)
                          call ufbint(51,sobarr,5,1,iret,sobnems)
                       else
                          print*, "FATAL ERROR: 2 piece ob with conflicting/unexpected ob type arrangement for station,typea,typeb: ",sid,arra(5),arrb(5)
                          call w3tage('PREPOBS_GLERLADJ')
                          call errexit(33)
                       endif
                       call writsb(51)
                       !now piece b
                       call ufbcpy(13,51)
                       if (arrb(5).lt.200) then
                          hdrarr(1)=198
                          hdrarr(2)=llatnew
                          hdrarr(3)=llonnew
                          call ufbint(51,hdrarr,3,1,iret,hdrnems)
                          call ufbint(51,tmparr,4,1,iret,tmpnems)
                          call ufbint(51,mstarr,4,1,iret,mstnems)
                       else
                          print*, "FATAL ERROR: 2 piece ob with conflicting/unexpected ob type arrangement for station,typea,typeb: ",sid,arra(5),arrb(5)
                          call w3tage('PREPOBS_GLERLADJ')
                          call errexit(33)
                       endif
                       call writsb(51)
                    endif
                 endif
              elseif (actflag.eq.0) then
                 print*, "Ob was taken over water."
                 if (convert) then
                    print*, "Water ob needs to be converted!"
                    if (windht.ne.10.) then
                       call uzl(wspd,windht,tdiff,windht,cd,ch,z0,fl)
                       wspd=uz(10.,wspd,cd,z0,fl)
                    endif
                    print*, "Converting station (water to land):",sid,rpt
                    call water2land(temp,dewpt,wspd,wdir,wtemp,tdiff,newtemp,newdpt,newspd,newdir,bmiss)
                    newq = bmiss
                    if(newdpt.lt.bmiss) call td_to_q(newdpt,pob,newq)
                    call uvspd(newspd,newdir,newu,newv,bmiss)
                    !copy old ob
                    call ufbcpy(91,51)
                    call writsb(51)
                    call ufbcpy(13,51)
                    call writsb(51)
                    !copy old ob again and convert to new ob
                    call ufbcpy(91,51)
                    !assign land lat/lon coords
                    wndarr(1)=newu
                    wndarr(2)=newv
                    wndarr(3)=wqm !orig wqm
                    wndarr(4)=pcode !pcode=17 (new bufr table)
                    wndarr(5)=adjust
                    sobarr(1)=newspd
                    sobarr(2)=newdir
                    sobarr(3)=dfq
                    sobarr(4)=pcode
                    sobarr(5)=adjust
                    tmparr(1)=newtemp
                    tmparr(2)=tqm
                    tmparr(3)=pcode
                    tmparr(4)=adjust
                    mstarr(1)=newq
                    mstarr(2)=qqm
                    mstarr(3)=pcode
                    mstarr(4)=adjust
                    if (arra(5).ge.200) then
                       hdrarr(1)=299.
                       hdrarr(2)=llatnew
                       hdrarr(3)=llonnew
                       call ufbint(51,hdrarr,3,1,iret,hdrnems)
                       call ufbint(51,wndarr,5,1,iret,wndnems)
                       call ufbint(51,sobarr,5,1,iret,sobnems)
                    else
                       print*, "FATAL ERROR: 2 piece ob with conflicting/unexpected ob type arrangement for station,typea,typeb: ",sid,arra(5),arrb(5)
                       call w3tage('PREPOBS_GLERLADJ')
                       call errexit(33)
                    endif
                    call writsb(51)
                    !now same with b piece
                    call ufbcpy(13,51)
                    !assign land lat/lon coords
                    if (arrb(5).lt.200) then
                       hdrarr(1)=199.
                       hdrarr(2)=llatnew
                       hdrarr(3)=llonnew
                       call ufbint(51,hdrarr,3,1,iret,hdrnems)
                       call ufbint(51,tmparr,4,1,iret,tmpnems)
                       call ufbint(51,mstarr,4,1,iret,mstnems)
                    else
                       print*, "FATAL ERROR: 2 piece ob with conflicting/unexpected ob type arrangement for station,typea,typeb: ",sid,arra(5),arrb(5)
                       call w3tage('PREPOBS_GLERLADJ')
                       call errexit(33)
                    endif
                    call writsb(51)
                 else
                    !ob does not need to be converted
                    !does it need to be moved
                    if (olat(stnum).eq.wlat(stnum).and.olon(stnum).eq.wlon(stnum)) then
                       !ob should not be moved
                       !copy ob and no worries
                       call ufbcpy(91,51)
                       call writsb(51)
                       call ufbcpy(13,51)
                       call writsb(51)
                    else
                       !ob needs to be moved
                       !get original values
                       print*, "Ob needs to be moved to match with mask."
                       print'("Original lat/lon, new lat/lon:",4(2x,g0))', lat,lon,wlatnew,wlonnew
                       call ufbint(91,arra,18,1,iret,nems)
                       call ufbint(13,arrb,18,1,iret,nems)
                       tmparr(1)=temp
                       tmparr(2)=tqm
                       tmparr(3)=pcode
                       tmparr(4)=moved !moved ob
                       mstarr(1)=qob
                       mstarr(2)=qqm
                       mstarr(3)=pcode
                       mstarr(4)=moved
                       wndarr(1)=uob
                       wndarr(2)=vob
                       wndarr(3)=wqm
                       wndarr(4)=pcode
                       wndarr(5)=moved
                       sobarr(1)=sob
                       sobarr(2)=ddo
                       sobarr(3)=dfq
                       sobarr(4)=pcode
                       sobarr(5)=moved
                       call ufbcpy(91,51)
                       if (arra(5).ge.200) then
                          hdrarr(1)=296.
                          hdrarr(2)=wlatnew
                          hdrarr(3)=wlonnew
                          call ufbint(51,hdrarr,3,1,iret,hdrnems)
                          call ufbint(51,wndarr,5,1,iret,wndnems)
                          call ufbint(51,sobarr,5,1,iret,sobnems)
                       else
                          print*, "FATAL ERROR: 2 piece ob with conflicting/unexpected ob type arrangement for station,typea,typeb: ",sid,arra(5),arrb(5)
                          call w3tage('PREPOBS_GLERLADJ')
                          call errexit(33)
                       endif
                       call writsb(51)
                       !now piece b
                       call ufbcpy(13,51)
                       if (arrb(5).lt.200) then
                          hdrarr(1)=196.
                          hdrarr(2)=wlatnew
                          hdrarr(3)=wlonnew
                          call ufbint(51,hdrarr,3,1,iret,hdrnems)
                          call ufbint(51,tmparr,4,1,iret,tmpnems)
                          call ufbint(51,mstarr,4,1,iret,mstnems)
                       else
                          print*, "FATAL ERROR: 2 piece ob with conflicting/unexpected ob type arrangement for station,typea,typeb: ",sid,arra(5),arrb(5)
                          call w3tage('PREPOBS_GLERLADJ')
                          call errexit(33)
                       endif
                       call writsb(51)
                    endif
                 endif
              else
                 print*, "FATAL ERROR: Improper actual value for station in dictionary file:",sid,a,actflag
              endif
     enddo moresubs
     ctyp0=ctyp
  enddo moremsg
! Hit end of file in unit 13 - all done, close all BUFR files and exit program
  call closbf(51)
  call closbf(13)
  call closbf(91)

  call w3tage('PREPOBS_GLERLADJ')
  stop
end program prepobs_glerladj
