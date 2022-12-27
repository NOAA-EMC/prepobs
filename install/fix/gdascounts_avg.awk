  BEGIN {
  min = 1000000
  max = -1
    }
  {
  s += $1
  if ($1 <= min) min = $1
  if ($1 >= max) max = $1 
  }
#END { print FILENAME,"(",var,")","average=",s/NR,"min=", min, "max=", max  }
END { print var,"=",int(s/NR + 0.5) }


