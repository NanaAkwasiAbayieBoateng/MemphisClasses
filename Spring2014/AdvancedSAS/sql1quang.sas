dm log 'clear';
  dm output 'clear';
ods html close; ods html;
option mprint symbolgen;

data z3;
input id name $ 6-23  gender $24-30 dom MMDDYY9. birthdate MMDDYY9. dept $ 
      salary1 salary2 salary3 ; 
datalines;
   1 joan  a. smith    female 09 22 96 12 20 64  shoe      40300 48800 50000
   2 john c.  park     male   07 13 90 09 25 65  clothing  50000 77000 78000
   3  mike    m. katz  male   03 15 98 06 30 60  applianc  27000 34000 33500
   4 jenny m. park     female 12 07 95 11 13 62  clothing  45500 62300 66000
   5 mike j. flemming  male   03 15 95 10 27 70  shoe      27800 33800 44000
   6 john p.   manley  male   04 18 97 02 18 75  clothing  23000 28800 31500
   7 susy m. smith     female 09 14 99 03 30 89  clothing  19500  .     .
   8 lisa k. kapling   female 08 23 97 12 15 75  applianc  37900 52000  .
   9 bob  k. park      male   11 12 93 10 10 72  clothing  22000 24500 33800     
  10  nancy p. kwan    female 10 15 94 09 16 73  clothing  23500 26000 29500
  11      miny d. king female 10 22 98 11 22 79  toy       21000 23000 25500
  12 jenny a. moby     female 08 06 99 09 30 80  toy       19000 20000 21000
run;

  data address;
input id name $ 6-23  address $ 25-70;
  
  datalines;
   1 joan     smith    1245 first street,  memphis,  tn  38123
   2 john     park     345  second avenue, southhaven, tn 38145
   3  mike       katz  2145   third street,  memphis,  tn 38123
   5 mike    flemming  297  park ave,  southhaven,  tn 38145 
   6 john      manley  214 poplar, northhaven,  tn 38145
   7 susy    smith     159 highland, memphis, tn 38129
   9 bob     park      345  second avenue, southhaven, tn 38145
  10  nancy  kwan    5345  orangetown, memphis,  tn  38142
  11      miny d. king 246  king street,  memphis,  tn 38145
 run;

data rating;
input  id  score1  score2  rater1  rater2   status $;
datalines;
	 3  78  67  5  7  m
	 5  88  97  8  9  s
	10  88  86  9  7  s
	 9  55  62  7  6  m
	 8  66  72  6  5  m
	 6  77  73  7  8  m
	 2  94  99  6  7  s
	 1  77  72  8  9  s
	12  66  69  6  6  s
run;

proc print data = z3;

proc sql;
	*1;
title'1';
	select name, id from z3 where SCAN(name,3) like "m%";
	*2;
	title'2';
	select name from z3 where gender='female' and SCAN(name,3) like "m%";
	*3;
	title'3';
	select gender, avg(salary1) from z3 group by gender;
	*4;
    title'4';
	select name, avg(salary1) from z3 where gender='female' group by dept having dept = 'clothing';
	
	*5;
	 title'5';
	select name, id from z3 where mod(id,2)=1;
	*6;
	title'6';
	select name from z3 where gender='female' and (salary1+salary2)/2>30000;
	*7;
	title'7';
	select sum(salary1) as total_salary1, max(salary1) as highest_salary1 from z3;
run;
	*8;
title'8';
proc sql outobs = 2;
	select name, salary1 from z3 order by salary1 desc ;
	select name, salary1 from z3 order byname, salary1 desc ;
run;
	*9;
title'9';
proc sql;
	select range(salary1), dept from z3 group by dept;

	*10;
	title'10';
	select id from z3 where (salary1+salary2)/2<30000;
	select count(id) from z3 where (salary1+salary2)/2<30000 ;
	*11;
	title'11';
	select dept, salary1 from z3 group by dept order by dept desc, salary1 desc;
	*12;
	select name, id from z3 where id in (select id from rating where rater1 > 7);
	*13;
	select name from z3 where id in (select id from rating where (rater1+rater2)/2 > 7);
	*14;
	select name, z3.id from z3, rating 
	where z3.id = rating.id and z3.gender = 'female' and rating.rater1>7 and rating.rater2>7;
	*15;
	select name from z3 where z3.id not in (select id from rating);

	
	*16;
	select dept, avg(score1), avg(score2) from z3, rating where z3.id = rating.id group by z3.dept; 
	*17;
	select name, (rater1+rater2)/2 as average_rater_1_2 from z3, rating where z3.id = rating.id;
	*18;	
	select name from z3, rating 
	where z3.id = rating.id and (rater1+rater2)/2 < (select avg((rater1+rater2)/2) from rating);
	*19;
	select a.id, a.name, b.id, b.name, a.salary1-b.salary1 from z3 as a, z3 as b where a.salary1-b.salary1>20000;
	*20;
	select name, id, dept from z3 
	where (salary1+salary2+salary3) in (select max(salary1+salary2+salary3) from z3 group by dept);
	*21;
	select name, yrdif(birthdate,today(),'AGE') from z3 where yrdif(dom,today(),'AGE') > 10;
	*22;
	title'22';
	select dept, sum(salary1), (sum(salary1)*100.0/(select sum(salary1) from z3)) from z3 group by dept;
	*23;
	select name, (case when rater1 > rater2 then rater1 else rater2 end) from z3, rating where z3.id = rating.id;
	select name, max(rater1,rater2) from z3, rating where z3.id = rating.id;
	*24;
	select dept, count((rater1+rater2)/2 <= 7) from z3, rating where z3.id = rating.id group by dept;
	*25;
	select z3.name, address from z3, rating, address 
	where z3.id = rating.id and address.id = z3.id and status = 's' and gender = 'female';
	*26;
	select z3.name, address from z3, address 
	where address.id = z3.id and address like '%145';
quit;
