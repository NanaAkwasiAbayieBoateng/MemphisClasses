/* Fisher Randomization Test on data in eg 1, p410.  */;
dm output 'clear'; ;
dm log 'clear';
options pageno=1 nodate  formdlim='*';
ods  html  close;
ods html;  
/*Create a macro program simplifying the do loops*/

options mlogic mprint symbolgen;
%macro fisher1(file,nn,m);
data xx;
infile "&file";

input y1-y&nn;
	array ss(*) y1-y&nn;
do n1=1 to &nn-&m+1;
%do i=2 %to &m;
	do n&i=n%eval(&i-1)+1 to &nn-&m+&i;
	%end;
%let sum=0;
%do j=1 %to &m;
	%let sum=&sum+ss(n&j);
	%end;
	if &sum <= 0 then t=1; else t=0;
		output;
%do k=1 %to &m;
	 end;
	%end;
%mend ttest;
%fisher1(%str(F:/temp/fisher.txt),12,5);

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

