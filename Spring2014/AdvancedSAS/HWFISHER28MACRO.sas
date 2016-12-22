/* Fisher Randomization Test on data in eg 1, p410.  */;
dm output 'clear'; ;
dm log 'clear';
options pageno=1 nodate  formdlim='*';
ods  html  close;
ods html;  
/*Create a macro program simplifying the do loops*/

options mlogic 
%macro fisher2(file,nn,m,v);
data xx;
infile "&file";

input y1-y&nn;
	array ss(*) y1-y&nn@@;
	array x(*) z1-z&v;

	%do i=1 %to &m;
 do n&i=1 to 2; if n&i=1 then x(&i)=ss(&i); else x(&i)=0;
%end;


 %let sum=0;
%do j=1 %to &m;
	 %let sum=&sum+x(&j);
	%end;
	if &sum <= 6 then t=1; else t=0;
		output;
%do k=1 %to &m;

	 end;
	%end;
%mend fisher2;
%fisher2(%str(F:/temp/fisher2.txt),7,7,7);


proc univariate noprint;
    var t;
      output out=temp n=nn sum=tt;

  data xx;
    set temp;
      pp=tt*2/nn;

     proc print data=xx;
      var tt pp;

   run;
