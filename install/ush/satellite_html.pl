#!/usr/bin/perl     
#--------------------------------------------------------------------------
#
#  satellite_html.pl
#
#  This PERL script inputs the daily average satellite counts by type from
#  satellite_daily.pl and creates the three HTML web pages.  The pages are
#  for "received" observations, "selected" observations and "assimilated"
#  observations.
#
#  History:
#  L. Sager   09/07  Modified the script to work properly in operations.
#--------------------------------------------------------------------------
#
#
#  Make the html pages for the observational 
#    counts website
#
$table="satellite_counts.tbl";
$MONTH=$ARGV[0];
$YEAR=$ARGV[1];
open (FIX,$table);
$kc=0;
while ($first=<FIX> )
{
    ($ia,$ib,$ic)=split(/;/,$first);
    $newm{$kc}=$ia;
    $name{$kc}=$ib;
    $para{$kc}=$ic;
    print " table is $kc  $newm{$kc} $name{$kc} $para{$kc}";
    $outrec[$kc][0]=$ia;
    $outsel[$kc][0]=$ia;
    $outass[$kc][0]=$ia;
    $outrec[$kc][1]=$ib;
    $outsel[$kc][1]=$ib;
    $outass[$kc][1]=$ib;
    $outrec[$kc][2]=$ic;
    $outsel[$kc][2]=$ic;
    $outass[$kc][2]=$ic;
    $kc++;
}
close (FIX);
$i=0;
do {
    print " out is $outrec[$i][0] $outrec[$i][1] $outrec[$i][2] ";
    $i++;
}  until $i > $kc;

$j=0;
#$types="/ptmp/wx12ls/counts/test/tmpsat_types";
$types="tmpsat_types";
open (LISTS,$types);
$kl=0;
while ($second=<LISTS> )
{
   ($ia,$ib,$ic)=split(/ /,$second);
   $i=0;
   $ilo=0;
   do {
      $iw = length($ib);
      $iz = substr($outrec[$i][0], 0, $iw);
      if ( $ib eq $iz ) {
         $ilo=1;
      }
      $i++;
   } until $i == $kc;
   if ( $ilo == 0) {
#
#     Add new data type to end of out array
#
      $kc++;
      $outrec[$kc][0]= $ib;
      $outsel[$kc][0]= $ib;
      $outass[$kc][0]= $ib;
      $outrec[$kc][1]= "*";
      $outsel[$kc][1]= "*";
      $outass[$kc][1]= "*";
      $ib=~tr/a-z]/A-Z/  ;
      $outrec[$kc][2]=$ib;
      $outsel[$kc][2]=$ib;
      $outass[$kc][2]=$ib;
      print " adding $outrec[$kc][0] at $kc";
   }
   $kl++;
}
close (LISTS);
#
#     Finally, add the total of all satellites for this cycle
#
 $kc++;
 $outrec[$kc][0]= "Totalsats";
 $outsel[$kc][0]= "Totalsats";
 $outass[$kc][0]= "Totalsats";
 $outrec[$kc][1]= "Total";
 $outsel[$kc][1]= "Total";
 $outass[$kc][1]= "Total";
 $outrec[$kc][2]= "Satellites Received";
 $outsel[$kc][2]= "Satellites Selected";
 $outass[$kc][2]= "Satellites Assimilated";
 print " adding $outrec[$kc][0] at $kc";

print "\n\n  the master template:\n";
$i=0;
do {
  
  print " $outrec[$i][0] $outrec[$i][1] $outrec[$i][2]  ";
  $i++;
} until $i > $kc;

$filenam[1]="tmpsat_davg00" ;
$filenam[2]="tmpsat_davg06";
$filenam[3]="tmpsat_davg12" ;
$filenam[4]="tmpsat_davg18" ;
$OUTFILE[1]="tmpsat_00" ;
$OUTFILE[2]="tmpsat_06";
$OUTFILE[3]="tmpsat_12" ;
$OUTFILE[4]="tmpsat_18" ;
$i=1;
$granr=0;
$grans=0;
$grana=0;
do {
   $k=0;
   $totr=0;
   $tota=0;
   $tots=0;
   $name{0}=~//;
   $subname{0}=~//;
   open (ACESS,$filenam[$i]);
   open (FINAL,">$OUTFILE[$i]");
   while ($second=<ACESS> )
   {
      ($ia,$ib,$ic,$id,$ie,$if)=split(/ /,$second);
      if ( $ib =~ "sndr" ) {
         $ib="sndr";
      }
      $iflag=0;
      if ( $name{$k} ne $ib ) {
         if ( $k > 0 ) {
            print FINAL " $name{$k} $count1{$k} $count2{$k} $count3{$k} \n";
         }
        $k++;
        $name{$k}=$ib;
        $subname{$k}=$ic;
        $count1{$k}=$id;
        $count2{$k}=$ie;
        $count3{$k}=$if;
        $count4{$k}=1;
        $totalr=$totalr+$id;
      }
      else {
         $count1{$k}=$count1{$k}+$id;
         $count2{$k}=$count2{$k}+$ie;
         $count3{$k}=$count3{$k}+$if;
         $count4{$k}++
      }
      $totr=$totr+$id;
      $tots=$tots+$ie;
      $tota=$tota+$if;
   }
   print FINAL " $name{$k} $count1{$k} $count2{$k} $count3{$k} \n"; 
   print FINAL " Totalsats $totr $tots $tota      \n";
   close (ACESS);
   $i++;  
   $granr=$granr+$totr;
   $grans=$grans+$tots;
   $grana=$grana+$tota;
   if ( $i >  4 ) {
      print FINAL " Overalltotal $granr $grans $grana \n";
   }
   close (FINAL);
} until $i > 4;
  
#
#  Fill in the html output array
#
$INFILE[1]="tmpsat_00" ;
$INFILE[2]="tmpsat_06";
$INFILE[3]="tmpsat_12" ;
$INFILE[4]="tmpsat_18" ;
  
$i=1;
do  {
   open (FOX,$INFILE[$i]);
   while ($third=<FOX> )
   {
      ($ia,$ib,$ic,$id,$ie)=split(/ /,$third);
      $k=0;
      $iw = length($ib);  
      do {
         $iz = substr($outrec[$k][0], 0, $iw);
         if ( $ib eq $iz ) {
            $outrec[$k][$i+2]=$ic;
            $outrec[$k][7]=$outrec[$k][7]+$ic;
            $outsel[$k][$i+2]=$id;
            $outsel[$k][7]=$outsel[$k][7]+$id;
            $outass[$k][$i+2]=$ie; 
            $outass[$k][7]=$outass[$k][7]+$ie;
         }
         $k++;
      } until $k > $kc;
   } 
   $i++;
} until $i > 4;
close (FOX);
print " \n  received case\n\n\n";
$i=0;
do {
print " $outrec[$i][0] $outrec[$i][1] $outrec[$i][2]  \n";
print " $outrec[$i][3] $outrec[$i][4] $outrec[$i][5] $outrec[$i][6] \n";
print " $outrec[$i][7] \n\n";
$i++;
} until $i > $kc; 

$ofil1="received_counts.html";
$ofil2="selected_counts.html";
$ofil3="assimilated_counts.html";
open (MF1, ">${ofil1}");
open (MF2, ">${ofil2}");
open (MF3, ">${ofil3}");
$k=1;
do {          
 $TYPE="RECEIVED";
 $MF=MF1;
 $filen="$outrec";
  if ( $k == 2 ) {
     $TYPE="SELECTED";
     $MF=MF2;
     $filen="$outsel";
  }
  if ( $k == 3 ) {
     $TYPE="ASSIMILATED";
     $MF=MF3;
     $filen="$outass";
  }
 print $MF  <<"ENDOFTEXT";
<HTML>
<HEAD>
   <TITLE> $MONTH $YEAR GDAS SATELLITE DATA DUMPS (DAILY AVERAGE $TYPE)</TITLE> </HEAD>
<BODY BGCOLOR="#E7FFFF" text="#000000" link="#00009C" alink="#D9D9F3" vlink="#87
1F78" background="backgrnd.gif" ;">
<CENTER><H2> $MONTH $YEAR GDAS SATELLITE DATA DUMPS (DAILY AVERAGE $TYPE)</H2></CENTER>
<table border="1" cols=6 width="100%">
<TR VALIGN="TOP"><TD><FONT SIZE="-1">Category</FONT></TD>
  <TD><FONT SIZE="-1">Subcategory</FONT></TD>
  <TD><FONT SIZE="-1">T00Z</FONT></TD>
  <TD><FONT SIZE="-1">T06Z</FONT></TD>
  <TD><FONT SIZE="-1">T12Z</FONT></TD>
  <TD><FONT SIZE="-1">T18Z</FONT></TD>
  <TD><FONT SIZE="-1">Total Number</FONT></TD></TR>
ENDOFTEXT
$k++;
}   until $k > 3;
$i=0;
$MF=MF1;
do {
 print $MF  <<"ENDOFTEXT";
<TR VALIGN="TOP"><TD><FONT SIZE="-1">$outrec[$i][1]</FONT></TD>
  <TD><FONT SIZE="-1">$outrec[$i][2]</FONT></TD>
  <TD><FONT SIZE="-1">$outrec[$i][3]</FONT></TD>
  <TD><FONT SIZE="-1">$outrec[$i][4]</FONT></TD>
  <TD><FONT SIZE="-1">$outrec[$i][5]</FONT></TD>
  <TD><FONT SIZE="-1">$outrec[$i][6]</FONT></TD>
  <TD><FONT SIZE="-1">$outrec[$i][7]</FONT></TD></TR>

ENDOFTEXT

$i++
} until $i>$kc;

$i=0;
$MF=MF2;
do {
 print $MF  <<"ENDOFTEXT";
<TR VALIGN="TOP"><TD><FONT SIZE="-1">$outsel[$i][1]</FONT></TD>
  <TD><FONT SIZE="-1">$outsel[$i][2]</FONT></TD>
  <TD><FONT SIZE="-1">$outsel[$i][3]</FONT></TD>
  <TD><FONT SIZE="-1">$outsel[$i][4]</FONT></TD>
  <TD><FONT SIZE="-1">$outsel[$i][5]</FONT></TD>
  <TD><FONT SIZE="-1">$outsel[$i][6]</FONT></TD>
  <TD><FONT SIZE="-1">$outsel[$i][7]</FONT></TD></TR>

ENDOFTEXT

$i++
} until $i>$kc;

$i=0;
$MF=MF3;
do {
 print $MF  <<"ENDOFTEXT";
<TR VALIGN="TOP"><TD><FONT SIZE="-1">$outrec[$i][1]</FONT></TD>
  <TD><FONT SIZE="-1">$outass[$i][2]</FONT></TD>
  <TD><FONT SIZE="-1">$outass[$i][3]</FONT></TD>
  <TD><FONT SIZE="-1">$outass[$i][4]</FONT></TD>
  <TD><FONT SIZE="-1">$outass[$i][5]</FONT></TD>
  <TD><FONT SIZE="-1">$outass[$i][6]</FONT></TD>
  <TD><FONT SIZE="-1">$outass[$i][7]</FONT></TD></TR>

ENDOFTEXT

$i++
} until $i>$kc;

$i=1;
do {
$MF=MF1;
if ( $i == 2 ){
 $MF=MF2;
}
if ( $i == 3 ){
 $MF=MF3;
}

 print $MF  <<"ENDOFTEXT";
</TABLE><BR WP="BR1"><BR WP="BR2">
  <TD><FONT SIZE="-1">** Counts include AMSU-A radiances from the AQUA satellite
 .<br></FONT></TD>
  <BR>
  <BR>
  <BR>
</BODY>
</HTML>
ENDOFTEXT

$i++;
} until $i>3;
close MF1;
close MF2;
close MF3;
