dm 'output;clear;log;clear';
dm log 'clear';
options pageno=1 nodate formdlim='*';
ods html close;
ods html;
title"question 1";
proc sql;
create table table1
(Id num primary key,
Last_name  char(30)not null,
First_name char(20) not null,
Test1 num,
Test2 num,
Days_missing num,
constraint id_chk check(id le 100 and id ge 1));
title"question 2";
proc sql;
describe table table1;
title"question 3";
proc sql;
insert into table1
values(23,'Arthur','Barbara',60,.,3)
values(33,'Cahill','Marshall',77,55,1)
values(9,'Carter','Dorithy',.,69,4)
values(53,'Cooper','Anthony',86,98,1)
values(15,'Dean','Sharon',.,.,10)
values(10,'Dunlap','Donna',77,88,0)
values(67,'Gold','Albert',70,70,2);
proc sql;
describe table table1;
title"question 4";
select id,Last_name,First_name,Test1,Test2,Days_missing from table1;

title"question 5";
alter table table1
/*add Last_namechar(20) */
/*add  Last_name char(20)*/
modify  Last_name char(20)
 modify First_name char(15);
select* from table1;
quit;
proc sql;
select id,Last_name,First_name,Test1,Test2,Days_missing from table1;

title"question 6";
proc sql;
delete from table1
where test1 is missing and test2 is missing;
select id,Last_name,First_name,Test1,Test2,Days_missing from table1;
quit;
proc sql;
update table1
set test1=0 where test1=.;
update table1
set test2=0 where test2=.;
select id,Last_name,First_name,Test1,Test2,Days_missing from table1;
alter table table1
add average num;
update table1
set average=(test1+test2)/2;
select* from table1;



title"question 7";
proc sql;
alter table table1
add Grade char(1);
update table1
set Grade='A' where  (average le 100 and average gt 0) ;
update table1
set Grade='B' where (average le 90 and average ge 80) ;
update table1
set Grade='C' where (average le 80 and average ge 70);
update table1
set Grade='D' where(average le 70 and average ge 60);
update table1
set grade='F' where (average lt 60);
select* from  table1
quit;
 title"question 8";
 proc sql;
 update table1
 set test2=test2+5 where test2 ne 0;
 select* from table1;

 title"question 9 ";
 update table1
 set average =average+5 where Days_missing=0;
 update table1
set average =average+3 where Days_missing=1;
update table1
set average =average+2 where Days_missing=2;
update table1
set average =average+0 where Days_missing>2;
 select* from table1;
 quit;
Title 'Question #10b';
proc sql;
update table1
	set average=average+
	case Days_missing	when Days_missing=0 then 5
				when Days_missing=1 then 3
				when Days_missing=2 then 2
				else 0
				end;
select * from table1;
quit;

title"question 11 ";
%macro retrieve(Last_name=, option=);
	proc sql;
	select Last_name, &option from table1 where Last_name=&Last_name;
	quit;
%mend retrieve;

%retrieve(Last_name='Cahill', option=Grade);
%retrieve(Last_name='Cahill', option=test2);
title"question 12 ";
%macro test_avg(option=);
	proc sql;
	select avg(&option) from table1;
	quit;
%mend test_avg;

%test_avg(option=test1);
