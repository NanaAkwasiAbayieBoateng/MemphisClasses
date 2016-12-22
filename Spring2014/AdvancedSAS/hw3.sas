dm log 'clear';
	dm output 'clear';
	ods html close; ods html;
	options nodate pageno=1 formdlim='*';
	options mprint symbolgen;
 title 'homework 3';

data data1;
	infile "F:\temp\d1.txt";
	input gender $ y x1 x2 x3;
run;

data data2;
	infile "F:\temp\d2.txt";
	input gender $ y x1 x2 x3 ;
run;

%macro stat(file=, vars=, gender=, options=);
	proc means data=&file &options;
	where gender=&gender;
	var &vars;
	run;
%mend stat;

%stat(file=data1, vars=x3, gender='female', options=mean);
%stat(file=data2, vars=x2 x3, gender=gender, options=mean std);
%stat(file=data2, vars=x1, gender=gender, options=max);
