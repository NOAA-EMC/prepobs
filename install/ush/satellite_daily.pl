#!/usr/bin/perl     
#--------------------------------------------------------------------------
#
#  satellite_daily.pl    
#
#  This PERL script inputs the satellite observational data counts files
#  and outputs lists of daily average  satellite counts by type .             
#
#  History:
#  L. Sager   09/07  Modified the script to work properly in operations.
#--------------------------------------------------------------------------
#
$i=1;
@hh=("00","00","06","12","18");
do {
   $filename[$i]="tmpsat_final$hh[$i]";
   $OUTFILE[$i]="tmpsat_davg$hh[$i]";
   $filesat[$i]="tmpdat1_file$hh[$i]";
   $fileqks[$i]="tmpdat2_file$hh[$i]";
   $fileqk2[$i]="tmpdat3_file$hh[$i]";
   $timesat[$i]="tmpdat_days$hh[$i]";
   $i++;
}  until $i > 4;
print " OUTFILE is $OUTFILE[2] \n";
$i=1;
do {
   $k=0;
   $name{0}=~//;
   $subname{0}=~//;
   open (ACESS,$filename[$i]);
   open (LISTS,">>tmpsat_list");
   while ($second=<ACESS> )
   {
      $second=~s/\s+/ /g;
      ($ia,$ib,$ic,$id,$ie,$if,$ig,$ih)=split(/ /,$second);
      $factor=1;
      if ( $ic eq pcp ) {
         $ih=$ig;
         $ig=$if;
         $if=$ie;
         $ie=$id;
         $id="pcp";
      }
      if( $ie eq "ssmi" ) {
         $factor=10.3;
         $if=int($if * $factor + 0.5);
         $ig=int($ig * $factor + 0.5);
         $ih=int($ih * $factor + 0.5);
      }
      if ( $ie eq "pcp_ssmi" ) { 
         $factor=18.8;
         $if=int($if * $factor + 0.5);
         $ig=int($ig * $factor + 0.5);
         $ih=int($ih * $factor + 0.5);
      }  
      if ( $name{$k} ne $id ||  $subname{$k} ne $ie) {
         if ( $k > 0 ) {
            if ( $subname{$k} =~ "sndrd" ) {
                 $subname{$k} = "sndr";
            }
            print LISTS " $subname{$k}   \n";
         }
        $k++;
        $name{$k}=$id;
        $subname{$k}=$ie;
        $count1{$k}=$if;
        $count2{$k}=$ig;
        $count3{$k}=$ih;
        $count4{$k}=1;
      }
      else {
         $count1{$k}=$count1{$k}+$if;
         $count2{$k}=$count2{$k}+$ig;
         $count3{$k}=$count3{$k}+$ih;
         $count4{$k}++;
      }
   }
   print LISTS " $subname{$k}  \n";
   close (ACESS);
#
#   Get counts for the SATWINDS
#
   open (SATW,$filesat[$i]);
   open (DAYS,$timesat[$i]);
   $totsatw=0;
   while ($fourth=<SATW>)
   {
      ($ia,$ib,$ic) = split(/S/,$fourth);
      $totsatw=$totsatw + $ib;  
   }
   $k++;
   $name{$k}="satw";
   $subname{$k}="satw";
   $count1{$k}=$totsatw;
   $count2{$k}=$totsatw;
   $count3{$k}=$totsatw;
   $fifth=<DAYS>;
   ($ia,$ib,$ic) = split(/S/,$fifth);
   $daystot=$ib;
   $count4{$k}=$daystot;
   close(SATW);
   open (QKSC,$fileqks[$i]);
   open (QKS2,$fileqk2[$i]);
   $totqks=0;
   while ($sixth=<QKSC>)
   {
      ($ia,$ib,$ic) = split(/S/,$sixth);
      $totqks=$totqks + $ib;  
      print " index $i  qickscat $ib    totquk is $totqks \n";
   }
   $totqk2=0;
   while ($seven=<QKS2>)
   {
      ($ia,$ib,$ic) = split(/WRITTEN ../,$seven);
      $totqk2=$totqk2 + $ib;  
      print " index $i  qickstot $ib    totqk2 is $totqk2 \n";
   }

   $k++;
   $name{$k}="scat";
   $subname{$k}="scat";
   $count1{$k}=$totqks * 3;
   $count2{$k}=$totqks ;
   $count3{$k}=$totqk2 ;
   $count4{$k}=$daystot;
   close(QKSC);
   close(QKS2);
   close(DAYS);

   open (DAVGS,">$OUTFILE[$i]");
  
   $j=1;
   while ( $j <= $k )  {
      $tmp1=int ($count1{$j}/$count4{$k} + 0.5 );
      $tmp2=int ($count2{$j}/$count4{$k} + 0.5 );
      $tmp3=int ($count3{$j}/$count4{$k} + 0.5 );
      print DAVGS " $subname{$j} $name{$j} $tmp1 $tmp2 $tmp3  \n";
      $j++;
   }
   $i++;  
   close (DAVGS);
} until $i > 4;
close (LISTS);
