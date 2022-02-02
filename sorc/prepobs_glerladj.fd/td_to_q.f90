subroutine td_to_q (dewpt,pressure,q)

  implicit none

  !derived from https://www.eol.ucar.edu/projects/ceop/dm/documents/refdata_report/eqns.html, from Bolton 1980.

  real(4),parameter::alpha=243.5
  !real(8),parameter::beta=440.8
  !real(8),parameter::gamma=19.48
  !real(8),parameter::eta=273.15
  real(4),parameter::eps=0.62197
  real(4),intent(in)::dewpt,pressure
  real(4),intent(out)::q
  real(4)::loge,e,qv

  e=6.112*exp((17.67*dewpt)/(dewpt+alpha))
  q=(eps*e)/(pressure - (0.378*3))
  q=q*1000000 !convert from kg/kg to mg/kg

end subroutine td_to_q
