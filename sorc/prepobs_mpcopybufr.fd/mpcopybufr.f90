              PROGRAM PREPOBS_MPCOPYBUFR
!$$$  MAIN PROGRAM DOCUMENTATION BLOCK
!
! MAIN PROGRAM: PREPOBS_MPCOPYBUFR
!   PRGMMR: KEYSER           ORG: NP22        DATE: 2013-03-06
!
! ABSTRACT: READS THROUGH USER-SPECIFIED NUMBER OF INPUT BUFR (DATA
!   DUMP) FILES AND COPIES SELECTED MESSAGES FROM EACH INTO UNIQUE
!   OUTPUT BUFR (DATA DUMP SUBSET) FILES (1-1 INPUT/OUTPUT).
!   THIS PROGRAM IS EXECUTED WITHIN A SCRIPT WHICH RUNS IN A PARALLEL
!   ENVIRONMENT (EITHER UNDER POE/MPI) OR IN BACKGROUND THREADS). THE
!   NUMBER OF MESSAGES SELECTED IS BASED ON THE NUMBER OF PARTS INTO
!   WHICH THE INPUT BUFR (DATA DUMP) FILES ARE TO BE DIVIDED - THIS WAY
!   ALL FILES WILL BE EVENLY DIVIDED.  THE LOCATION OF THE SELECTED
!   MESSAGES IS BASED ON THE TASK NUMBER (FOR POE/MPI) OR ON THE
!   BACKGROUND THREAD NUMBER FOR THIS RUN OF THE SCRIPT. THE RESULT OF
!   RUNNING THIS PROGRAM IN A PARALLEL SCRIPTS IS TO SPLIT EACH SET OF
!   INPUT BUFR FILES INTO UNIQUE (TO EACH SCRIPT EXECUTED) SUBSETS
!   WHICH ARE THEN PASSED INTO PREPDATA (ALLOWS FOR LOAD BALANCING).
!
! PROGRAM HISTORY LOG:
! 1999-06-29  KISTLER -- ORIGINAL AUTHOR
! 2013-03-06  KEYSER  -- CHANGES TO RUN ON WCOSS
!
! USAGE:
!   INPUT FILES:
!     PARM     - NAMELIST /NAMIN/ (VARIABLE "NFILES" - THE NUMBER
!              - OF INPUT BUFR FILES); NAMELIST MP (VARIABLE
!              - "MP_PROCESS" - THE TASK NUMBER (MPI/POE) OR BACKGROUND
!                THREAD NUMBER FOR THIS RUN; VARIABLE "NPROCS" - THE
!                NUMBER OF PARTS INTO WHICH THE INPUT BUFR FILES ARE TO
!                BE DIVIDED)
!     UNIT 11  - COMPLETE BUFR FILE 1
!     UNIT 12  - COMPLETE BUFR FILE 2
!       ...              ...
!       ...              ...
!     UNIT XX  - (WHERE "XX" = 10+"NFILES"), COMPLETE BUFR FILE
!              - "NFILES"
!
!   OUTPUT FILES:
!     UNIT 06  - STANDARD PRINTFILE
!     UNIT 51  - SELECTED MESSAGES FROM INPUT BUFR FILE 1
!     UNIT 52  - SELECTED MESSAGES FROM INPUT BUFR FILE 2
!       ...              ...
!       ...              ...
!     UNIT XX  - (WHERE "XX" = 50+"NFILES"), SELECTED MESSAGES FROM
!              - INPUT BUFR FILE "NFILES"
!
!   SUBPROGRAMS CALLED:
!     LIBRARY:
!       UNIQUE   - MP_IREADMG
!       W3NCO    - W3TAGB   W3TAGE
!       BUFRLIB  - DATELEN  OPENBF   IREADMG  CLOSMG   COPYMG
!                  READMG   CLOSBF
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

! PROGRAM PREPOBS_MPCOPYBUFR

CHARACTER*8 SUBSET

namelist /namin/nfiles


DATA LUNIN /11    /
DATA LUNOT /51    /

      CALL W3TAGB('PREPOBS_MPCOPYBUFR',2013,0065,0085,'NP22')

read(5,namin)
write(6,namin)

CALL DATELEN(10)

do i=1,nfiles

    CALL OPENBF(LUNIN,'IN ',LUNIN)
    CALL OPENBF(LUNOT,'OUT',LUNIN)

        ! copy first two messages (center and dump time) to all files
        ! -----------------------------------------------------------

        do k=1,2
                if(IREADMG(LUNIN,SUBSET,IDATE).EQ.0) then
                        CALL CLOSMG(LUNOT)
                        CALL COPYMG(LUNIN,LUNOT)
                endif
        enddo

    DO WHILE(MP_IREADMG(LUNIN,SUBSET,IDATE).EQ.0)
       CALL COPYMG(LUNIN,LUNOT)
    ENDDO

    CALL CLOSBF(LUNIN)
    CALL CLOSBF(LUNOT)

    LUNIN=LUNIN+1
    LUNOT=LUNOT+1

ENDDO

      CALL W3TAGE('PREPOBS_MPCOPYBUFR')

STOP
END

function mp_ireadmg(lunin,subset,idate)
implicit none
integer mp_ireadmg
integer lunin,idate,iret,ierr,mp_process,ifirst,n,nprocs,kount
character*8 subset
namelist /mp/mp_process,nprocs
data ifirst/0/,kount/0/
save ifirst,mp_process,nprocs

if (ifirst.eq.0) then
    read(5,mp)
    write(6,mp)
    ifirst=1
endif

do
    call readmg(lunin,subset,idate,iret)
    if (iret .ne. 0 ) then
!print*,subset,' iret,kount = ',subset,' ',iret,kount
        exit
    else
        kount=kount+1
        if (mod(kount,nprocs).eq.mp_process) then
!print*,subset,' mod(',kount,',',nprocs,') = ',mod(kount,nprocs),mp_process
            exit
        endif
    endif
enddo
mp_ireadmg=iret
return
end
