
    * Find binomial probabilities for x = k, k=1 to n 
                when p is estimated and n is given. Perform Goodness of Fit test.
               data given in Gibbon p55.   n=6;
     dm log 'clear';   dm output 'clear';
	  options nodate pageno=1 formdlim='*';
   data x;
     input x fre @@;
    datalines;
   0 2  1  7  2 10  3 8   4 3   5 0   6 0    
   run ; 
    proc means noprint;
     freq fre;
      output out=xx mean=mm n=nn;  run;
   data xx;
         set xx;
      n=6;
           p=mm/n;
/*  file 'c:\mydata\nonpar\binoprobyy_out' print;  */
    do k=0 to n;
  pp=pdf('binomial',k,p,n);
    npk=nn*pp;
	   output;
      end;       run;

	  proc print;   
	     var npk;
		    format npk 7.2;     run;

   proc freq data=x;
      weight fre/zero;
	     table x /chisq testf=(2.26 7.31 9.84 7.06 2.85 0.61 0.06);
		    exact chisq;
      run;
	 
	    /*Calculate the p value for 1 less df. */

	  data ;
	     p= 1-probchi(.8447,5);
		  proc print;  run;
	  quit;

