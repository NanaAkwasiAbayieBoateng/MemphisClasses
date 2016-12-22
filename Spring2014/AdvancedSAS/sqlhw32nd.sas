dm 'output;clear;log;clear';
dm log 'clear';
options pageno=1 nodate formdlim='*';
ods html close;
ods html;

	data x1;
	input id name $ grade $;
	datalines;
	1  jones  A
	3  long   B
	.  wyet   D       
	4  plank  C
	5  singh  A
	12 wall   B
	15 looney C
	.  gold   A
	11  Chen  B
	run;
	data x2;
	input id name $ class $ test1 ;
	datalines;
	.  looney so  55
	16  gold  so  63
	7  smith  so  49
	1  jones  sr  90
	8  bly    sr  99
	3  long   jr  89
   17  wyet   so  60
	4  plank  sr  70
	.  singh  so  67
	9  king   sr  80
	run;
	data x3;
	input id name $ class $ test2 ;
	datalines;
	16  gold  so  73
	7  smith  so  59
	1  jones  sr  80
	8  bly    sr  94
	3  long   jr  81
	.  wyet   so  70
	4  plank  sr  .
	5  singh  so  77
	.  king   sr  85
	run;
  
  
    /*   Problem 1. Show ID, name, grade and test1 for those that are
	                in both x1 or  x2. */
proc sql;
title'1';

select coalesce(x1.id,x2.id) as ID,coalesce(x1.name,x2.name)as name,grade,test1
from x1 full join x2 on x1.name=x2.name
 order by test1;


 


    /*   Problem 2a. Show ID, name, grade and test1 for those that are
	                in only x1.  */
	      title'2a';
select coalesce(x1.id,x2.id) as ID,coalesce(x1.name,x2.name)as name,grade,test1
from x1 left join x2 on x1.name=x2.name;
	/*   Problem 2b. Show ID, name, grade and test1 for those that are
	                in only x2.  */
      title'2b';
select coalesce(x1.id,x2.id) as ID,coalesce(x1.name,x2.name)as name,grade,test1
from x1 right join x2 on x1.name=x2.name;
    /*  Problem 3. List id, name, grade, class and test1 for those
                         with name appearing in both tables x1 and x2.   */
	     

select coalesce(x1.id,x2.id) as ID,coalesce(x1.name,x2.name)as name,grade,test1,class
from x1 inner join x2 on x1.name=x2.name
 order by name;
	/*  Problem 4. List id, name, grade, class and test1 for those
                         with id appearing in both tables x1 and x2.  */
	      title'4';
select coalesce(x1.id,x2.id) as ID,coalesce(x1.name,x2.name)as name,grade,test1,class
from x1 inner join x2 on x1.id=x2.id
where x1.id and x2.id is not missing;
    /*  Problem 5. Add column test1 from table x2 and test2 from table x3 
                       to table x1.            */

title'5';
select coalesce(x1.id,x2.id,x3.id) as ID,coalesce(x1.name,x2.name,x3.name)as name,grade,test1,test2 from x1 
left join x2 on x1.name=x2.name 
left join x3 on x2.name=x3.name ;

    /*  Problem 6. Give ID, name, and grade of those with test1 score  
	               higher than test2 score     */

title'6';
select coalesce(x1.id,x2.id,x3.id) as ID,coalesce(x1.name,x2.name,x3.name)as name,grade,test1,test2 from x1 
full join x2 on x1.name=x2.name 
full join x3 on x2.name=x3.name 
where test1>test2 and test1 is not missing and test2 is not missing ;
	/*  Problem 7. Give the number of students with test1 score 
	               higher than test2 score.*/

title'7';
select count(*) as count from x1 
full join x2 on x1.name=x2.name 
full join x3 on x2.name=x3.name 
where test1>test2 and test1 is not missing and test2 is not missing ;

	 /* For the following problems use the dataset given in last week.

	Problem 8.  Find the ids of employees whose score2 is higher than
	                   all score1s.





	Problem 9.  Find the ids of employees whose score2 is lowerr than
	                  any score1.                                        
	Problem 10. Give the names of employees who have address but no rating.*/
	                   
quit;
