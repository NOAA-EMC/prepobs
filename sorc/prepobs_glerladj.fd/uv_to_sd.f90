subroutine uvspd(spd,dir,u,v,bmiss)

  use glerl_dictionary

  implicit none

  real(4),intent(in)::spd,dir
  real(4),intent(out)::u,v
  real(4)::rad
  real(8)::bmiss

  rad=4.0*atan(1.0)/180
  u = -spd*sin(rad*dir)
  v = -spd*cos(rad*dir)

  if (spd.gt.9999) then
     u=bmiss
     v=bmiss
  endif

end subroutine uvspd
