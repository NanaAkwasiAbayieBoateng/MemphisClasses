dm 'output;clear;log;clear';
dm log 'clear';
options pageno=1 nodate formdlim='*';
ods html close;
ods html; 
title'nana boateng'
title 'question 1';
data Z1;
  infile 'F:\temp\Z1.txt';
 
    input id name $ 6-23  gender $24-30 dom MMDDYY9. birthdate MMDDYY9. dept $ 
      salary  ;
run;
proc print data=Z1;
run;
data Z2;
  infile 'F:\temp\Z2.txt';
 
     input id name $ 6-23  gender $24-30 dom MMDDYY9. birthdate MMDDYY9. dept $ 
      salary  ;
run;
proc print data=Z2;
run;

data Z3;
  infile 'F:\temp\Z3.txt';
 
     input id name $ 6-23  gender $24-30 test1 ;
run;
proc print data=Z3;
run;



data Z4;
  infile 'F:\temp\Z4.txt';
 
     input subject first_name $6-12 last_name $ 13-23  sex $24-30 test2 ;
run;
proc print data=Z4;
run;

proc sql;
title'1';
	*select dept, salary1 from z3 group by dept order by dept desc, salary1 desc;
	select dept,count(*)as count from Z1 where salary<32000 group by dept order by dept desc;
	
      run;
	 proc print data=Z1;
run;


title'2';
proc sql;
 select dept,avg(salary)as avgsalary from Z2 group by dept  having  calculated avgsalary>(select avg(salary) from Z2) ;
run;

title'3A';
proc sql;
select count(*) as count 
	 from((select id,name from Z1) intersect (select id,name from Z2) );
	 quit;
run;

title'3B';
proc sql;
select count(*) as count 
	 from((select id,name from Z2) except   select id,name from  Z1);
	 quit;
run;
  
title'4';
proc sql;
select coalesce(Z3.id,Z4.subject)as id,coalesce(scan(Z3.name,1),Z4.first_name) as 
firstname,coalesce(scan(Z3.name,-1),Z4.last_name)
as lastname,Z3.gender,Z3.test1,Z4.test2 from Z3 full join z4
on scan(Z3.name,-1)=Z4.last_name and scan(Z3.name,1)=Z4.first_name;


title'5';

/*
proc sql;
update Z4
set test2=0 where test2 is missing;
update Z3
set test1=0 where test1 is missing;
insert into Z4 (test1)

select test1 from Z3;
select* from Z4;

alter table Z4
add average num;
update Z4
set average=(test1+test2)/2 where test1 is not missing and test2 is not missing;
(select test1 from Z3 intersect  select test2 from Z4)

*/
/*proc sql;
update Z4
set test2=0 where test2 is missing;

alter table Z4
add average num;
update Z4
set average=(test1+test2)/2  where test1 is not missing and test2 is not missing;
select first_name , last_name ,average from Z4;*/

proc sql;
update Z4
set test2=0 where test2 is missing;
select coalesce (scan(Z3.name,1),Z4.first_name) as first_name,
coalesce(scan(Z3.name,-1),Z4.last_name) as last_name,mean(Z3.test1,Z4.test2) as average_score
from Z3,Z4
where (Z4.test2>0)and (scan(Z3.name,-1)=Z4.last_name) and scan(Z3.name,1)=Z4.first_name;
