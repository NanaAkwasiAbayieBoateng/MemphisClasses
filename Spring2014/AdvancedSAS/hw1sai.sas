


dm log 'clear';
dm output 'clear';
options nodate pageno=1 formdlim='*';
ods html close; /* close previous */
ods html; /* open new */
options mlogic mprint;
title " --- ";

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

proc print data=d1;
run;

proc reg data=d1;
	model y = x1 x2 x3;
	where gender ='female' or 'male';
run;

%macro meanprocedure(dataname,means,var,gender);
proc means data=&dataname &means;
var &var;
where &gender;
run;
%mend meanprocedure;
%meanprocedure(d1,mean,x3,gender='female');
%meanprocedure(d1,mean std,x2 x3);

%meanprocedure(d1,max,x1,);

*Quesion 2*;
%macro m/parmbuff;
%let i=%scan(&syspbuff,1);
%do k=1 %to &i;
%let n=%eval((&k-1)*4+2);
%let n1=%eval((&k-1)*4+3);
%let n2=%eval((&k-1)*4+4);
%let n3=%eval((&k-1)*4+5);
%let dataname=%scan(&syspbuff,&n,%str(,));
%let means=%scan(&syspbuff,&n1,%str(,));
%let var=%scan(&syspbuff,&n2,%str(,));
%let con=%scan(&syspbuff,&n3,%str(,));
proc means data=&dataname &means;
var &var;
&con;
run;
%end;
%mend m;
%m(3,d1,mean,x3,where gender='female',d2,mean std,x2,x3,,d2);



/*
model &dependent=&independent;
%end
%run;
%mend m2;
%m2(2,x1,x3,d1,y);
%m2(3,x1,x2,x3,d1,y);

*/

title'question 3';
%macro m3/parmbuff;
%let num=%scan(&syspbuff,1);
%do k=1 %to &num;
%let nn=%eval((&k-1)*3+3);
%let nn2=%eval((&k-1)*3+4);
%let nn3=%eval((&k-1)*3+2);
%let independent=%scan(&syspbuff,&nn3,str(,));
%let dataname=%scan(&syspbuff,&n,%str(,));
%let means=%scan(&syspbuff,&n1,%str(,));
%let var=%scan(&syspbuff,&n2,%str(,));
%let con=%scan(&syspbuff,&n3,%str(,));
proc reg  data=&filename;
model &dependent=&independent;
%end;
run;
%end;
%mend m3;
%m3(2,x2 x3,d1,y,x1 x2 x3,d1,y,);

*/
