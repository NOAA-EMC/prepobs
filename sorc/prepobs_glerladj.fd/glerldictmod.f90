module glerl_dictionary
  implicit none
  integer(4),parameter::dictmax=9999
  integer(4),parameter::misint=-999
  character(len=8),parameter::mischar="MISSING "
  real(8),parameter::adjust=1. !adjusted ob reason code
  real(8),parameter::moved=2. !moved ob reason code
contains
  subroutine finddict2(stname,names,lat,lon,olats,olons,stnum)
    integer(4),intent(out)::stnum
    real(4),intent(in)::lat,lon
    real(4),dimension(:),intent(in)::olats(dictmax),olons(dictmax)!,llats(dictmax),llons(dictmax)
    character(len=8),dimension(:),intent(in)::names(dictmax)
    character(len=8),intent(in)::stname
    real(4)::wlat,wlon,llat,llon
    integer(4)::a,act

    !initial/default values
    stnum=misint
    dictloop: do a=1,dictmax
       namecheck: if (stname.eq.names(a)) then
          if (abs(olats(a)-lat).lt.0.1.and.abs(olons(a)-lon).lt.0.1) then
             stnum=a
             return
          elseif (names(a).eq.mischar) then
             return
          endif
       endif namecheck
    enddo dictloop
  end subroutine finddict2
end module glerl_dictionary
