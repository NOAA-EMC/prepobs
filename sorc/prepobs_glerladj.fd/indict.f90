subroutine finddict2(stname,lat,lon,newlat,newlon,stnum,llats,llons,wlats,wlons,acts,names,dsize)

  implicit none

  integer(4),parameter,intent(in)::dsize
  integer(4),dimension(:),intent(in)::acts(dsize)
  integer(4),intent(out)::stnum
  real(8),intent(out)::newlat,newlon
  real(8),intent(in)::lat,lon
  real(8),dimension(:),intent(in)::wlats(dsize),wlons(dsize),llats(dsize),llons(dsize)
  character(len=8),dimension(:),intent(in)::names(dsize)
  character(len=8),intent(in)::stname
  real(8)::wlat,wlon,llat,llon
  integer(4)::a,act

  !initial/default values
  stnum=10**11
  newlat=10**11
  newlon=10**11

  !print*, "TEST2 first name in dictionary=",names(1)
  indict: do a=1,dsize
     !print*, "Looking in dictionary at station number:",a
     found: if (stname.eq.names(a)) then
        print*, "Found station:",stname
        stnum=a
        act=acts(a)
        if (act.eq.1) then
           newlat=wlats(a)
           newlon=wlons(a)
        else !act eq 0
           newlat=llats(a)
           newlon=llons(a)
        endif
        return
     endif found
  enddo indict

end subroutine finddict2
