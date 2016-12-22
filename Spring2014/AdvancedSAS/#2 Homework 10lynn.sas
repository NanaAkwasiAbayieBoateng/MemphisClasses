dm log 'clear';
dm output 'clear';
options nodate pageno=1 formdlim='*';
Title 'Homework 10: 04-18-2014';

Title 'Question #1';
proc sql;
create table dd
(ID num primary key, constraint ID_check check (ID le 100 and ID ge 1),
LastName char(30) not null,
FirstName char(20) not null,
Test1 num, Test2 num, Days num);
quit;

Title 'Question #2';
proc sql;
describe table dd;
quit;

Title 'Question #3';
proc sql;
insert into dd
values (23,'Arthur','Barbara',60,.,3)
values (33,'Cahill','Marshall',77,55,1)
values (9,'Carter','Dorithy',.,69,4)
values (53,'Cooper','Anthony',86,98,1)
values (15,'Dean','Sharon',.,.,10)
values (10,'Dunlap','Donna',77,88,0)
values (67,'Gold','Albert',70,70,2);
select * from dd;
quit;

Title 'Question #4';
proc sql;
select * from dd;
quit;

Title 'Question #5';
*Done by hand;

Title 'Question #6';
proc sql;
alter table dd
modify LastName char(20),
	FirstName char(15);
select * from dd;
quit;

Title 'Question #7';
proc sql;
delete from dd
	where Test1 is missing AND Test2 is missing;
update dd
	set Test1=0 where Test1 is missing;
update dd
	set Test2=0 where Test2 is missing;
alter table dd
	add Average num;
update dd
	set Average=((Test1+Test2)/2);
select * from dd;
quit;

Title 'Question #8';
proc sql;
alter table dd
	add Grade char(1);
update dd
	set Grade='A' where (90<=Average<=100);
update dd
	set Grade='B' where (80<=Average<90);
update dd
	set Grade='C' where (70<=Average<80);
update dd
	set Grade='D' where (60<=Average<70);
update dd
	set Grade='F' where (Average<60);
select * from dd;
quit;

Title 'Question #9';
proc sql;
update dd
	set Test2=Test2+5 where Test2>0;
select * from dd;
quit;

Title 'Question #10';
proc sql;
update dd
	set Average=Average+
	case Days 	when Days=0 then 5
				when Days=1 then 3
				when Days=2 then 2
				else 0
				end;
select * from dd;
quit;

Title 'Question #11';
