dm log 'clear';
dm output 'clear';
ods html close; ods html;
options nodate pageno=1 formdlim='*';
title " NANA BOATENG ";

data d1;
input gender$     y     x1    x2    x3;
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
run;

*proc print data=d1;
*run;

*proc reg data=d1;
*	model y = x1 x2 x3;
*	where gender ='female' or 'male';
*run;
*/
options mlogic mprint;


%macro my_regression (file=, gender=, indvars=x1 x2 x3, depvars= y);
	title "data = &file and x = &indvars";
	proc reg data=&file;
		model &depvars = &indvars;
		where gender = &gender;
		run;
%mend my_gender_regression;
/*
%my_regression(file=d1, gender='female', indvars=x1 x3, depvars= y);
%my_regression(file=d1, gender=gender, indvars=x2 x3, depvars= y);
%my_regression(file=d1, gender=gender);
%my_regression(file=d1, gender='female', indvars=x1 x2 x3, depvars= y);
*/
ods html close; /* close previous */
ods html; /* open new */
options mlogic mprint;
%macro my_regression1 /PARMBUFF;
	%let file = %scan(&syspbuff, 1);
	%let num_regressions = %scan(&syspbuff, 2);
	title "regression = &num_regressions";
	%do i=1 %to &num_regressions;
		%let j= %eval(&i+2);
		%let indvars= %scan(&syspbuff, &j);
			proc reg data=&file;
				model y = &indvars;				
			run;
	%end;
%mend my_regression1;



%macro my_regression3(file=,gender=,indvars=x1 x2 x3,depvars=y);

%my_regression(file=d1, gender=gender, indvars=x2 x3, depvars= y);
%my_regression1(d1, 3, x1, x3,x2);

%mend my_regression3;




%my_regression3(file=d1,gender=gender);
%macro m3 / parmbuff;
%let num = %scan(&syspbuff, 1);
%do k=1 %to &num;
%let nn=%eval((&k-1)*3+3);
%let nn2=%eval((&k-1)*3+4);
%let nn3=%eval((&k-1)*3+2);
%let independent=%scan(&syspbuff, &nn3, sr(,));
%let filename=%scan(&syspbuff, &nn, str(,));
%let dependent=%scan(&syspbuff, &nn2, str(,));
proc reg data=&filename;
model &dependent=&independent;
%end;
run;
%mend m3;
%m3(2, x2 x3, d1, y, x1 x2 x3, d1, y,);
