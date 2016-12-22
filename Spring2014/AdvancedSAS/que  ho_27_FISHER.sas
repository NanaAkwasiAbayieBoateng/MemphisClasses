


  /* Fisher Randomization Test on data in eg 1, p410.  */;
 dm output 'clear'; ;
     dm log 'clear';

       options pageno=1 nodate  formdlim='*';
	      ods  html  close;
		    ods html;  
  
   data xx;
     input y1-y12;
        array ss(*) y1-y12;
   nn=12; m=5;

   do n1=1 to nn-m+1;
     do n2=n1+1 to nn-m+2;
     do n3=n2+1 to nn-m+3;
        do n4= n3+1 to nn-m+4;
         do n5= n4+1 to nn-m+5;
     sum=ss(n1)+ss(n2)+ss(n3)+ss(n4)+ss(n5);
      if sum <= 0 then t=1; else t=0;
         output;
     end;end;end;end;end;

  datalines;
  0 1 1 0 -2 6 7 7 4 -3 9 14
  run;
  proc print data=xx;
    proc univariate noprint;
    var t;
      output out=temp n=nn sum=tt;

   data xx;
    set temp;
      pp=tt*2/nn;
     proc print data=xx;
      var tt pp;

   run;
                  /*       
                            Obs    tt       pp

                             1     11    0.027778             */

