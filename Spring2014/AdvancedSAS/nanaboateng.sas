dm log 'clear';
dm output 'clear';
ods html close; ods html;
options nodate pageno=1 formdlim='*';
title " NANA BOATENG";

options mlogic mprint;

data d1;
input      x y z;
datalines;
   1 1 1
   1 1 1
   1 2 1
   2 1 2
   1 1 3 
   2 1 3
   2 2 2
   2 1 2

run;
data d2;
input      u v w z;
datalines;
1 1 1 2
1 1 1 3
1 1 2 3
2 2 2 2
2 1 1 2
2 2 1 3
2 2 2 3
2 2 2 2 

run;
data d3;
input     xx$ yy$ zz$ ;
datalines;
a d f
a d c
a d g
a e f
b e f
b d g
b e g


run;
data d4;
input     day sale ;
datalines;
1 78 
2 88
3 72
4 66
5 58
6 81
7 62
8 69


run;
proc print data=d4;
run;

title 'midterm';

%macro table1(file=,  vars=x y z);

	proc freq data=&file;
    tables &vars*&vars;
		run;
%mend table1;
 %table1(file=d1,vars= x z);




%macro table2(file,  var1,var2,options);

	proc freq data=&file;
    tables &var1*(&var2);
	*tables &var1*(&var2)/norow nopercent nocol;
	where &options;
		run;
%mend table2;

%table2(d1,x,z);

%table2(d2,u,v,"w=1");


%macro table3(file=,  vars=x y z ,options=);

	proc freq data=&file;
    tables &vars*&vars*&vars;
	where &options;
		run;
%mend table3;
 %table3(file=d1,vars= z x y,options="w=1");
