

dm 'output;clear;log;clear';
dm log 'clear';
options pageno=1 nodate formdlim='*';
ods html close;
ods html;


 
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

   data z3b;


	 proc sql;
	 title '1';
	 select name,id from z3 where scan(name,3) like 'm%';
	
      run;
	  


	 proc sql;
	 title '2';
	 select name,gender from z3 where scan(name,3) like 'm%' and gender='female';
	
      run;

	
	proc sql;
	 title '3';
	 select gender, avg(salary1) label='average salary' format=dollar12.2 from z3   group by gender;
	
      run;

	proc sql;
	 title '4';
	 select name,id from z3 where  gender='female' and dept='clothing';
	
      run;

	proc sql;
	 title '5';
	 select name, id from z3 where mod(id,2)=1 ;
	
      run;

	  
	proc sql;
	 title '6';
	 select name  from z3 where gender='female' and ((salary1+salary2)/2)>30000 ;
	
      run;

	  proc sql ;
	 title '7';
	 select sum(salary1)as  total format=dollar10.2,max(salary1) label='maximum salary' from z3;
	
      run;

	  proc sql outobs=2;
	 title '8';
	 select name,salary1 from z3 order by salary1  ;
	
      run;
 proc sql outobs=2;
	 title '8';
	 select name,salary1 from z3 order by salary1  ;
	
      run;	proc sql;
	 title '9';
	 select range(salary1) from z3 group by dept ;
	
      run;

	proc sql;
	 title '10';
	 select count(name) from z3 where ((salary1+salary2)/2)<30000;
	
      run;
	 


	  
	proc sql;
	 title '11';
	 select dept,salary1 from z3   group by dept order by dept,salary1 desc;
	
      run;

 
	proc sql;
	 title '12';
	
  select id,rater1 from rating where rater1>7

               and id in (select id from z3);
	
      run;


	proc sql;
	 title '12';
	
  select a.id,b.name from rating as a ,z3 as b where a.id=b.id and rater1>7 ;
	
      run;


	  proc sql;
	 title '13';
	
  select a.id,b.name from rating as a ,z3 as b where a.id=b.id and mean(rater1,rater2)>7 ;
	
      run;

	 proc sql;
	 title '13b';
	
  select name from rating as a ,z3 as b where a.id=b.id and mean(rater1,rater2)>7 ;
	
      run;
	  proc sql;
	 title '14';
	
   select name from rating as a ,z3 as b where a.id=b.id  and (rater1>7 and rater2>7) and gender='female';
	
      run;


	    proc sql;
	 title '15';
	
   select name from z3 where z3.id  not in (select id  from rating ) ;
	
      run;

	   proc sql;
	 title '16';
	
  select dept, mean(score1) as averagescore1,mean(score2) as averagescore2 from z3 as a  ,rating as b
where a.id=b.id
group by dept ;
	
      run;

	  
	   proc sql;
	 title '17';
	 select name, mean(rater1,rater2) as averagerater from z3 as a  ,rating as b
where a.id=b.id
;
      run;

	   proc sql;
	 title '18';
select name,a.id from rating as a ,z3 as b where a.id=b.id having mean(rater1,rater2)<avg((rater1+rater2)/2); 
  


proc sql;
title'19';
select a.name,a.salary1,b.name,b.salary1 from z3 as a,z3 as b  where (a.salary1-b.salary1)>20000;
title'19b';
select a.name,b.name,a.id,a.salary1,b.id,b.salary1 from z3 as a,z3 as b  where (a.salary1-b.salary1)>20000;


	proc sql;
	 title '20';
	
  select name,dept,mean(salary1,salary2,salary3) as salary from z3 group by dept  having salary=max(salary);

proc sql;
title'21';
select name,((today()-birthdate)/365.25) as age from z3 having ((dom-birthdate)/365.25)>10;


	

proc sql;
title'22';
*select name,dept,sum(salary1) as deptsalary1,sum(calculated deptsalary1)as totalsalary1,(calculated deptsalary1/calculated totalsalary1)*100 as precent from z3 group by dept  ;
	select dept, sum(salary1), (sum(salary1)*100.0/(select sum(salary1) from z3))as percent from z3 group by dept;

proc sql;
title'22';
select name,dept,sum(salary1) as deptsalary1 from z3 group by dept  ;


	   proc sql;
	 title '23';
	 select name, max(rater1,rater2) as maxgerater from z3 as a  ,rating as b
where a.id=b.id
;
      run;

	    proc sql;
	 title '24';
	* select name, count((rater1+rater2)/2)<=7) as averagerater from z3 as a ,z3 as b ,rating as b where a.id=b.id group by dept
     ;

	 select dept, count((rater1+rater2)/2 <= 7)as averagerater from z3, rating where z3.id = rating.id group by dept;
 title '24b';
select dept, count((rater1+rater2)/2 <= 7)as averagerater from z3 as a, rating as b where a.id = b.id group by dept;


	 	*25;
	  title '25';
	select z3.name, address from z3, rating, address 
	where z3.id = rating.id and address.id = z3.id and status = 's' and gender = 'female';
	*26;
	 title '26';
	select z3.name, address from z3, address 
	where address.id = z3.id and address like '%145';



	 /* For the following problems use the dataset given in last week.

	Problem 8.  Find the ids of employees whose score2 is higher than
	                   all score1s.*/
  

	   proc sql;
	 title'8';
	 select id, score2 from rating
     having score2>max(score1)
;
      run;


	/*Problem 9.  Find the ids of employees whose score2 is lowerr than
	                  any score1. */
   proc sql;
	 title'9';
	 select id, score2 from rating
     having score2<max(score1)
;
      run;
 
	/*Problem 10. Give the names of employees who have address but no rating.*/
	   title '10';
	select id,name,address from address
	where id not in(select id from rating);
