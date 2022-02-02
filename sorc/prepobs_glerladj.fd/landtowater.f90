subroutine land2water (temp,dewpt,wspd,wdir,wtemp,tdiff,newtemp,newdpt,newspd,newdir,bmiss)
  !abstract: modifies ob according to Kelly paper on GLERL

  use glerl_dictionary

  implicit none
  real(4),intent(in)::temp,dewpt,wspd,wdir,tdiff,wtemp
  real(4),intent(out)::newtemp,newdpt,newspd,newdir
  real(4)::delwd
  real(8)::bmiss

  !print variables for record keeping
  print'("Original Airtemp,WaterTemp,tdiff,dewpoint,wspd,wdir=",6(2x,g0))', temp,wtemp,tdiff,dewpt,wspd,wdir
  !adjust temp
  if (temp.lt.bmiss.and.wtemp.lt.bmiss) then
     newtemp = 0.4*temp+0.6*(wtemp)
  elseif (temp.ge.bmiss) then
     newtemp=bmiss
     print*, "Can't make l2w adjusted temperature: missing observed air temperature!"
  elseif (wtemp.ge.bmiss) then
     newtemp=bmiss
     print*, "Can't make l2w adjusted temperature: missing observed water temperature!"
  else
     newtemp=bmiss
     print*, "Can't make l2w adjusted temperature: Invalid observed air temp and water temp:",temp,wtemp
  endif
  !adjust dewpoint
  if (dewpt.lt.bmiss.and.wtemp.lt.bmiss) then
     newdpt = -1.31 + 0.7*dewpt+0.35*(wtemp)
  elseif (dewpt.ge.bmiss) then
     newdpt=bmiss
     print*, "Can't make l2w adjusted dew point: missing observed dew point!"
  elseif (wtemp.ge.bmiss) then
     newdpt=bmiss
     print*, "Can't make l2w adjusted dew point: missing observed water temperature!"
  else
     newdpt=bmiss
     print*, "Can't make l2w adjusted dew point: Invalid observed dew point and water temp:",dewpt,wtemp
  endif
  !adjust wind speed
  if (wspd.lt.bmiss.and.tdiff.lt.bmiss) then
     if (wspd.gt.0) then
        newspd = wspd*(1.2+(1.85/wspd))
        if (tdiff.ne.0.and.tdiff.lt.100) newspd=newspd*(1.-(tdiff/abs(tdiff))*(abs(tdiff)/1920.)**0.3333) ! if tdiff > 100, temp is missing
        !adjust wind direction
        delwd=0.
        if(tdiff.ne.0) delwd=(12.5-1.5*tdiff)-(0.38 - 0.03*tdiff)*newspd
        newdir=wdir+delwd
        if(newdir.lt.0) newdir=newdir+360
        if(newdir.gt.360) newdir=newdir-360
     else !set to calm for calm wind or missing temperature
        newdir=wdir
        newspd=wspd
     endif
  elseif (wspd.ge.bmiss) then
     print*, "Can't make l2w adjusted wind speed: missing observed wind speed!"
     newdir=bmiss
     newspd=bmiss
  elseif (tdiff.ge.bmiss) then
     print*, "Can't make l2w adjusted wind speed: missing air-land temperature difference!"
     newdir=bmiss
     newspd=bmiss
  else
     print*, "Can't make l2w adjusted wind speed: invalid observed wind and air-land temp diff:",wspd,tdiff
     newdir=bmiss
     newspd=bmiss
  endif
  if (newspd.lt.0) then
     print*, "WARNING: Adjusted wind speed (l2w) is less than zero.  Defaulting to 0..."
     print*, "Adjusted wind speed value is:",newspd
     newspd=0.
  endif
  print'("Adjusted Airtemp,dewpoint,wspd,wdir=",4(2x,g0))', newtemp,newdpt,newspd,newdir

end subroutine land2water
