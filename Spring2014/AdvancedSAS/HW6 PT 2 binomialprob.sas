/* Fisher Randomization Test on data in eg 2 , p413.  */;
/* A MACRO PROGRAM TO REPLACE ALL THE DO LOOPS AND GENERALISE IT SO AS TO ALLOW GIVING THE NUMBER ODF SAMPLES IN THE CALL STATEMENT*/

 

 
   TITLE "HW6-Q2";
 * Find binomial probabilities for x = k, k=1 to n 
   when p is estimated and n is given. Perform Goodness of Fit test.
   data given in Gibbon p55.   n=6;
     
   dm log 'clear';   dm output 'clear';options nodate pageno=1 formdlim='*';
  
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
    end;       
    run;

    proc print;   
    var npk;
    format npk 7.2;     run;

	data ww; 
	  set xx;
	    call symput('np'||trim(left(_n_)), npk);
		run;

		%macro pp;
		 %do i =  1 %to 7;
		   &&np&i
           %end;
       %mend;

    proc freq data=x;
    weight fre/zero;
    table x /chisq testf=(%pp);
    exact chisq;
    run;
	 
    /*Calculate the p value for 1 less df. */
     data ;
     p= 1-probchi(.8447,5);
     proc print;  run;
     quit;

