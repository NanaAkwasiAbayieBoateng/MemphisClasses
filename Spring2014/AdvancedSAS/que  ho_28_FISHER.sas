

       /* Fisher Randomization Test on data in eg 2 , p413.  */;

    dm log 'clear';
	  dm output 'clear';
   dm output 'clear'; ;
     dm log 'clear';

       options pageno=1 nodate  formdlim='*';
	      ods  html  close;
		    ods html;

    data xx;
     input y1-y7;
        array ss(*) y1-y7;
            array x(*) z1-z7;

   do n1=1 to 2; if n1=1 then x(1)=ss(1); else x(1)=0;
   do n2=1 to 2; if n2=1 then x(2)=ss(2); else x(2)=0;
   do n3=1 to 2; if n3=1 then x(3)=ss(3); else x(3)=0;
   do n4=1 to 2; if n4=1 then x(4)=ss(4); else x(4)=0;
   do n5=1 to 2; if n5=1 then x(5)=ss(5); else x(5)=0;
   do n6=1 to 2; if n6=1 then x(6)=ss(6); else x(6)=0;
   do n7=1 to 2; if n7=1 then x(7)=ss(7); else x(7)=0;
     sum=x(1)+x(2)+x(3)+x(4)+x(5)+x(6)+x(7);
      if sum <= 6 then t=1; else t=0;
         output;
     end;end;end;end;end;end;end;

    datalines;
     16 4 7 3 5 1 10 
   run;

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
                              Obs    tt      pp

                               1      8    0.125
                                                                 */
