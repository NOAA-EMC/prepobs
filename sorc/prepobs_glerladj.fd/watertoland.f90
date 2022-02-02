subroutine water2land (temp,dewpt,wspd,wdir,wtemp,tdiff,newtemp,newdpt,newspd,newdir,bmiss)
  !abstract: inverse of land2wat, adjust obs over water to represent those over land

  use glerl_dictionary

  implicit none
  real(4),intent(in)::temp,dewpt,wspd,wdir,tdiff,wtemp
  real(4),intent(out)::newtemp,newdpt,newspd,newdir
  real(4)::delwd
  real(8)::bmiss

  !print original obs for record keeping/dupcheck
  print'("Orig Airtemp,WaterTemp,tdiff,dewpoint,wspd,wdir=",6(2x,g0))', temp,wtemp,tdiff,dewpt,wspd,wdir
  !adjust temp
  if (temp.lt.bmiss.and.wtemp.lt.bmiss) then
     newtemp=2.5*temp-1.5*wtemp
  elseif (temp.ge.bmiss) then
     newtemp=bmiss
     print*, "Can't make adjusted w2l temperature.  Missing observed temperature."
  elseif (wtemp.ge.bmiss) then
     newtemp=bmiss
     print*, "Can't make adjusted w2l temperature.  Missing water temperature."
  else
     newtemp=bmiss
     print*, "Can't make adjusted w2l temperature.  Invalid air and water temps:",temp,wtemp
  endif
  !adjust dewpoint
  if (dewpt.lt.bmiss.and.wtemp.lt.bmiss) then
     newdpt=1.8714+(dewpt/0.7)-(wtemp/2)
  elseif (dewpt.ge.bmiss) then
     newdpt=bmiss
     print*, "Can't make adjusted w2l dew point.  Missing observed dew point."
  elseif (wtemp.ge.bmiss) then
     newdpt=bmiss
     print*, "Can't make adjusted w2l dew point.  Missing water temperature."
  else
     newdpt=bmiss
     print*, "Can't make adjusted w2l dew point.  Invalid dew point and water temps:",dewpt,wtemp
  endif
  !adjust wind speed
  if (wspd.lt.bmiss.and.tdiff.lt.bmiss) then
     if (wspd.gt.0) then
        !first, adjust for stability  this is the inverse of the stability adjustment for l2w
        !if tdiff > 100, than one temp is missing
        !remove stability temperature adjustment
        !if (tdiff.ne.0.and.tdiff.lt.100) newspd=(1.-(tdiff/abs(tdiff))*(abs(tdiff)/1920.)**0.3333)/wspd
        newspd=(wspd-1.85)/1.21
        !adjust wind direction
        delwd=0.
        if(tdiff.ne.0) delwd=(12.5-1.5*tdiff)-(0.38 - 0.03*tdiff)*newspd
        newdir=wdir+delwd
        if(newdir.lt.0) newdir=newdir+360
        if(newdir.gt.360) newdir=newdir-360
        !if (newspd.lt.0) newspd=0
     else
        newdir=wdir
        newspd=wspd
     endif
  elseif (wspd.ge.bmiss) then
     print*, "Can't make adjusted w2l winds.  Missing observed wind speed."
     newdir=bmiss
     newspd=bmiss
  elseif (tdiff.ge.bmiss) then
     print*, "Can't make adjusted w2l winds.  Missing air-water temp difference."
     newdir=bmiss
     newspd=bmiss
  else
     print*, "Can't make adjusted w2l winds.  Invalid wind speed and air-water temp difference:",wspd,tdiff
     newdir=bmiss
     newspd=bmiss
  endif
  if (newspd.lt.0) then
     print*, "WARNING: Adjusted wind speed (w2l) is less than zero.  Defaulting to 0..."
     print*, "Adjusted wind speed value is:",newspd
     newspd=0.
  endif
  print'("Adjusted Airtemp,dewpoint,wspd,wdir=",4(2x,g0))', newtemp,newdpt,newspd,newdir

end subroutine water2land
