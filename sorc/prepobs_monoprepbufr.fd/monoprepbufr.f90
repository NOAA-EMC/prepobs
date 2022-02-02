              PROGRAM PREPOBS_MONOPREPBUFR
!$$$  MAIN PROGRAM DOCUMENTATION BLOCK
!
! MAIN PROGRAM: PREPOBS_MONOPREPBUFR
!   PRGMMR: KEYSER           ORG: NP22        DATE: 2013-03-06
!
! ABSTRACT: MERGES MULTIPLE PARTIAL PREPBUFR FILES INTO A MONOLITHIC
!   PREPBUFR FILE THAT CONTAINS ALL DATA.  GROUPS LIKE-NAMED TABLE A
!   MESSAGES TOGETHER IN THE EXPECTED ORDER.
!
! PROGRAM HISTORY LOG:
! 1999-07-29  KISTLER -- ORIGINAL AUTHOR
! 2013-03-06  KEYSER  -- CHANGES TO RUN ON WCOSS
!
! USAGE:
!   INPUT FILES:
!     PARM     - NAMELIST /NAMIN/ (
!              - VARIABLE "NFILES"   - THE NUMBER OF INPUT BUFR FILES
!                                      TO MERGE;
!              - VARIABLE "NHEADERS" - THE NUMBER OF UNIQUE TABLE A
!                                      ENTRIES FOR BUFR MESSAGES WITHIN
!                                      ALL INPUT PREPBUFR FILES;
!              - VARIABLE "CHEADERS" - LIST OF UNIQUE CHARACTER*8 TABLE
!                                      A ENTRIES FOR BUFR MESSAGES
!                                      WITHIN ALL INPUT PREPBUFR FILES;
!              - VARIABLE "MSGS"     - THE NUMBER OF BUFR MESSAGES FOR
!                                      A PARTICULAR TABLE A ENTRY IN A
!                                      PARTICULAR INPUT PREPBUFR FILE
!     UNIT 11  - PREPBUFR FILE 1
!     UNIT 12  - PREPBUFR FILE 2
!       ...              ...
!       ...              ...
!     UNIT XX  - (WHERE "XX" = 10+"NFILES"), PREPBUFR FILE "NFILES"
!
!   OUTPUT FILES:
!     UNIT 06  - STANDARD PRINTFILE
!     UNIT 51  - MONOLITHIC PREPBUFR FILE
!
!   SUBPROGRAMS CALLED:
!     LIBRARY:
!       W3NCO    - W3TAGB   W3TAGE
!       BUFRLIB  - DATELEN  OPENBF   IREADMG  COPYMG   CLOSMG
!                  CLOSBF
!
!   EXIT STATES:
!     COND =   0 - SUCCESSFUL RUN
!
! REMARKS: NONE.
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!   MACHINE:  NCEP WCOSS
!
!$$$

! program PREPOBS_MONOPREPBUFR

implicit none

character*8  subset,sid

integer      maxfiles,maxmsgs
parameter(maxfiles=31,maxmsgs=50)

character*80 filenames(maxfiles)
character*25 file
character*8  cheaders(maxmsgs),cheader
integer      m,i,nfiles ,ier,j,idate,ireadmg
integer      msg,msgs(maxfiles,maxmsgs),nheaders,ksum(maxmsgs)
integer      kend(maxfiles),lunin(maxfiles),lunot,msum(maxmsgs)

namelist /namin/ nfiles,nheaders,cheaders,msgs

      CALL W3TAGB('PREPOBS_MONOPREPBUFR',2013,0065,0085,'NP22')

print'(1X)'
print'(" Welcome to PREPOBS_MONOPREPBUFR - Version 03-07-2013")'
print'(1X)'

filenames=''
cheaders=''
msgs=0
read (5,namin)
write(6,namin)

print'(1X)'
print'(1X)'
print'(1X)'
print'(3x,<nheaders>(a8))',(cheaders(i),i=1,nheaders)
msum=0
do j=1,nfiles
    print'(i2,1x,<nheaders>(i6,2x))',j,(msgs(i,j),i=1,nheaders)
    do i=1,nheaders
        msum(i)=msum(i)+msgs(i,j)
    enddo
enddo


!  open the input and output files
!  -------------------------------

call datelen(10)

do j=1,nfiles
    lunin(j)=10+j
    call openbf(lunin(j),'IN',lunin(1))
enddo
lunot=51

!
! merge the input file messages
! 
!----------------------------------------------------------

call openbf(lunot,'OUT',lunin(1))
do i=1,nheaders
   ksum=0 ! msg count for each type of header
   do j=1,nfiles
      m=0
      print'(" writing",I4," messages of ",A," to unit",I3," for file",I3)', &
       msgs(i,j),cheaders(i),lunot,j
      if(msgs(i,j).gt.0) then
         do while(ireadmg(lunin(j),subset,idate).eq.0)
            if(m.le.msgs(i,j)) then
               m=m+1
               do while ( subset.ne.cheaders(i))
                  print'(" mismatch ",A," .ne. ",A)', subset,cheaders(i)
                  if(ireadmg(lunin(j),subset,idate).ne.0) then
                     print'(" terminating file",I3)', j
                     exit
                  endif
               enddo
!!!!!!!!!!!!!!!print'(" copy file ",I0," ",A,1X,I," of total ",I0)', &
!!!!!!!!!!!!!!! j,subset,m,msgs(i,j)
               call closmg(lunot)
               call copymg(lunin(j),lunot)
               ksum(i)=ksum(i)+1
            endif
            if(m.eq.msgs(i,j)) exit
         enddo
      endif
!!!!!!print'(1X,A," finished with file ",I0)', cheaders(i),j
   enddo
   print'(1X,A," wrote a total of ",I4," messages - expecting to write",I4,&
    " messages")', cheaders(i),ksum(i),msum(i)
   print'(1X)'
enddo
call closbf(lunot)

do j=1,nfiles
   call closbf(lunin(j))
enddo

      print'(1X)'
      print'(" All done.")'
      print'(1X)'
      CALL W3TAGE('PREPOBS_MONOPREPBUFR')

stop
end
