
   dm log 'clear';   dm output 'clear';

  

	  data y2001;
      input id name $ salary;
	  datalines;
	  12  aaa  35000
	   9  bkg  45500
	   6  klo  32880
	   8  sou  28900
	  24  der  19000
	  11  prt  39500
	  15  qqq  29990
	  run;
	  
	   data y2002;
      input ssn last $ salary;
	  datalines;
	  12  aaa  36000
	  35  xoy  28900
	   9  bkg  47500
	   6  klo  32880
	  24  der  23000
	  11  prt  45000
	  10  tty  43900
	  31  qrs  41000
	  8   sou  38900
	 run;
	
	   data y2003;
      input id name $ salary;
	  datalines;
	  12  aaa  39000
	  35  xoy  32300
	   6  klo  37000
	  24  der  29000
	  17  vop  35800
	  15  wer  50900
	  10  tty  46900
	  41  lle  31500
	  43  obd   52000
	  24  der  25900
	  9   bkg   45000
	 run;
	   
	  /* Problem 1.  Create a table that contains DISTINCT id  and name for any
	                employee in year 2003 or before.   */
	  	 proc sql;
title'1';
*select distinct  from y2003 inner join y2001 on  y2003.id=y2001.id ;
	      



	 quit;

	 proc sql;
   title 'Inner Join';
   select *
      from  y2003 as l, y2001 as r
      where l.id=r.id;
	   /* Problem 2.  Find the number of employees who worked in any of 
				         the three years.   */

        /* Problem 3.  Create a table that contains id and name
	               of any former employee who is not in year 2003    */
	
	    /* Problem 4.  Create a table that contains year, id, name, and  
	        salary  for any employee in year 2003 or before.   */

       /* Problem 5. List names of employees who worked either in 2001 or 2002
	                 but not both.

	 /* Problem 6. List names of employees who worked in only one year.

	 /* Problem 7. List names of employees who worked in all three years.

	 /* Problem 8. List id, and names of employees who worked in 2001 and 2002
	               but not in 2003.
