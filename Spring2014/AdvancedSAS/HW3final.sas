dm 'output;clear;log;clear';
dm log 'clear';
options pageno=1 nodate formdlim='*';
ods html close;
ods html;
/*****************************
*****************************/
data d1;
input gender $ y x1 x2 x3;
datalines;
female   50      2     5     9
male    45      5     6    12
male    51      6     8    13
female   55      4     6    10
female   49      2     4      8
female   57      5     8    11
female   51      3     5     8
male    49      7    8    12
male    53      8    9    14
female   59      7    9    15
female   46      2     3     7
female   55      4     8    12
male    51     6      9    13
male   44     4       5    11
male   59     9    12    15
male    51      8    9    12
female   50      4    6    10
female   49     3     3    6
;
run;
data d2;
input gender $ y x1 x2 x3;
datalines;
male    50     3    11   20
male    60     4    17   13
male    75     5    13   23
male    80     7    14   29
female  61     7    9    15
female  63     9    9    16
female  70     11   13   20
female  64     14   18   23
female  70     18   20    29
run;


%macro a(file,vars,stats,gen);
	title "&stats of &vars for data &file";
	proc means data= &file &stats;
	var &vars;
	run;
%mend a;
options mlogic mprint sgen;
%a(file=d1, vars=x3, stats=mean)
%a(file=d2, vars=x2 x3, stats=std)
%a(file=d2, vars=x1, stats=max)

%macro b(f1,f2,v1,v2,v3,s1,s2,s3);
	%a(file=&f1, vars=&v3, stats=&s1)
	%a(file=&f2, vars=&v2 &v3, stats=&s2)
	%a(file=&f2, vars=&v1, stats=&s3)
		
%mend b;
options mlogic mprint sgen;
%b(d1,d2,x1,x2,x3,mean,std,max)
