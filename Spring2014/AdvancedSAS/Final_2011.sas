
dm 'output;clear;log;clear';
dm log 'clear';
options pageno=1 nodate formdlim='*';
ods html close;
ods html;
    /*  Please Note:

         * Use only Proc SQL.
         * You cannot change the data set or the way the data are read.
         * You have two hours.
         * Give a title for the output of each problem.  
         * Turn in a hard copy of your codes and output   */

  dm log 'clear';
  dm output 'clear';

 
  data z1;

  input id name $ 6-23  gender $24-30 dom MMDDYY9. birthdate MMDDYY9. dept $ 
      salary  ; 
  
  datalines;
   1 joan  a. smith    female 09 22 96 12 20 64  shoe      30300 
   2 john c.  park     male   07 13 90 09 25 65  clothing  30000 
   3  mike    m. katz  male   03 15 98 06 30 60  applianc  37000 
   4 jenny m. park     female 12 07 95 11 13 62  clothing  25500 
   5                   male   03 15 95 10 27 70  shoe      27800 
   6 john p.   manley  male   04 18 97 02 18 75  clothing  23000 
   7 susy m. smith     female 09 14 99 03 30 89  clothing  19500 
   8 lisa k. kapling   female 08 23 97 12 15 75  applianc  37900 
   9 bob  k. park      male   11 12 93 10 10 72  clothing  23000 
  10  nancy p. kwan    female 10 15 94 09 16 73  clothing  23500 
  11      miny d. king female 10 22 98 11 22 79  toy       21000 
  12 jenny a. moby     female 08 06 99 09 30 80  toy       19000 
   run;

   data z2;
  
  input id name $ 6-23  gender $24-30 dom MMDDYY9. birthdate MMDDYY9. dept $ 
      salary  ; 
  
  datalines;
  14 debby  f. sloan   female 01 22 01 12 20 74  shoe      20300 
   2 john c.  park     male   07 13 90 09 25 65  clothing  31000 
  13  mike  e. silver  male   02 15 01 06 30 80  shoe      20000 
   4 jenny m. park     female 12 07 95 11 13 62  clothing  27500 
   5 mike j. flemming  male   03 15 95 10 27 70  applianc  38800 
   6                   male   04 18 97 02 18 75  clothing  24000 
   7 susy m. smith     female 09 14 99 03 30 89  clothing  21500 
   8 lisa k. kapling   female 08 23 97 12 15 75  applianc  38900 
   9 bob  k. park      male   11 12 93 10 10 72  clothing  23000 
  10  nancy p. kwan    female 10 15 94 09 16 73  clothing  24500 
  11      miny d. king female 10 22 98 11 22 79  clothing  . 
  12 jenny a. moby     female 08 06 99 09 30 80  toy       19500 
   run;
 
  
             /* Problem 1. Use z1 to find for each department the number of employees
                           who have salary less than 32,000. List the departments
                           in decreasing order of the number.
	
	           Problem 2. List the departments in z2 whose employees' average salary is 
                          higher than that of the whole company.
			
               Problem 3. (a) Give the number of employees who are listed in both z1 and z2.   
                          (b) Give the number of employees who are listed in z2 but not in z1
             */
		 

     data z3;
	   input id name $ 6-23  gender $24-30 test ;
  datalines;
   1 joan  a. smith    female  67
   . john c.  park     male    58
   3  mike   m. katz   male    78
   4 jenny m. park     female  65
   5 john  e. clark    male    87
   . susy  a. smith    female  67
   ;

    data z4;
	   input subject first_name $6-12 last_name $ 13-23  sex $24-30 test ;
  datalines;
   1 joan     smith           76
   2 john     park     male   68
   3  mike    katz     male   .
   4 jenny    park     female 74 
   . john     manley   male   .
   7 susy     smith    female  .
   . Ted      sloan    male   89
   run;

     /*  Problem 4. Merge z3 and z4 so that the new table will contain the id,
         the full name (with middle initial, if available), the gender and 
         the two test scores. Print the new table. (Hint. Merge by last name.)  
	              
         Problem 5. Replace the test score in z4 by 0 if it is missing.
                    Find the average of the two test scores only if both are
                    available. List names with their average sscores.         */
                 
		       
title"question 1";
	
