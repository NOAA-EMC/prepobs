subroutine spddir(u,v,spd,dir,bmiss)
  !abstract: calculates wind speed and direction given u and v
  use glerl_dictionary
  implicit none
  real(4),intent(in)::u,v
  real(4),intent(out)::spd,dir
  real(4)::tempagl,pi
  real(8)::bmiss

  !initialize temp variables
  pi = 3.14519
  tempagl=0
  !find speed
  spd=sqrt((u*u)+(v*v))
  !find direction
  if(u.eq.0.and.v.lt.0) then
     dir = 360
  elseif(u.eq.0.and.v.gt.0) then
     dir = 180
  elseif(u.lt.0.and.v.eq.0) then
     dir = 70
  elseif(u.gt.0.and.v.eq.0) then
     dir = 270
  elseif (u.eq.0.and.v.eq.0) then
     dir = 360
  elseif (u.gt.9999.or.v.gt.9999) then
     dir = bmiss
     spd = bmiss
  else
     tempagl=atan(v/u)*180/pi
     if (u.lt.0.and.v.lt.0) then
        dir=270-tempagl-180
     elseif (u.gt.0.and.v.lt.0) then
        dir = 90+tempagl+180
     elseif (u.lt.0.and.v.gt.0) then
        dir=270+tempagl-180
     elseif (u.gt.0.and.v.gt.0) then
        dir=90-tempagl+180
     endif
  endif
end subroutine spddir
