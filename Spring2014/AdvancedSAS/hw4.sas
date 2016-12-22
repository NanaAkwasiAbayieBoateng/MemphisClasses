dm log 'clear';
dm output 'clear';
ods html close; ods html;
options nodate pageno=1 formdlim='*';
title 'homework 4';
data answer;
  infile 'F:\temp\hw4data.txt';
  input name$ test @@;
   do k=1 to 20;  input ans $ @@;  output;
   end;
run;

proc print data=answer;
run;

%macro hw4;
	data ee;
	set answer;
		call symput(trim(name)||trim(left(test))||trim(left(k)), ans);
	run;	
%mend hw4;

%put _user_;

%hw4;

%macro num_correct_answers(student=, test=);
	data q1;
	%let m = 0;
	%do k=1 %to 20;
		%if &&&student&test&k = &&AA&test&k
		%then %let m = %eval(&m+1);		
	%end;
	count = %eval(&m);
	run;
%mend num_correct_answers;

options mlogic mprint sgen;

%num_correct_answers(student=TOM, test=2);
proc print data=q1;
run;

%macro total_num_correct_answers(student=);
	data q2;
	%let m = 0;
	%do k=1 %to 20;
		%do test=1 %to 4;
			%if &&&student&test&k = &&AA&test&k
			%then %let m = %eval(&m+1);		
		%end;
	%end;
	count = %eval(&m);
	percentage = %sysevalf(&m*100/(20*4));
	*percentage = %eval(&m*100/(20*4));
	run;
%mend total_num_correct_answers;

%total_num_correct_answers(student=Jenny);
proc print data=q2;
run;

%macro total_num_correct_answers1(test=, student=, begin=, end=);
	data q3;
	%let m = 0;
	%do k= &begin %to &end;
		%if &&&student&test&k = &&AA&test&k
		%then %let m = %eval(&m+1);		
	%end;
	count = %eval(&m);
	percentage = %sysevalf(&m*100/(20*4));
	*percentage = %eval(&m*100/(20*4));
	run;
%mend total_num_correct_answers;
%total_num_correct_answers1(test=1, student=Jenny, begin=3, end=15);
proc print data=q3;
run;
