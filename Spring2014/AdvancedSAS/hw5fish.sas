dm log 'clear';
dm output 'clear';
ods html close; ods html;
options nodate pageno=1 formdlim='*';
title " homework 5";
options mlogic mprint symbolgen;
ods html close; /* close previous */
ods html; /* open new */

%macro fisher(file,m,nn);
data xx;
infile "&file";
     input y1-y&nn;
        array ss(*) y1-y&nn;
   *nn=12; 
*m=5;

do n1=1 to &nn-&m+1;
*%do i=1 %to &m	;
%do i=2 %to &m;

     
    		do n&i=n%eval(&i-1)+1 to &nn-&m+&i;%end;		
           
	%let sum=0;
 %do j=1 %to &m;
	%let sum=&sum+ss(n&j);%end	;
if &sum<=0 then t=1;
else t=0;	
output;

			 %do k=1 %to &m;
			 end;
			 %end
  
	
%mend fisher;

%fisher(%str(F:\temp\fisher.txt),5,12);


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
