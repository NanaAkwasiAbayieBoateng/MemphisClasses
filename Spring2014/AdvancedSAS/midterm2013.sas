
  
  Data
    set #1:                set #2              set #3                        set #4           
   x   y  z               u  v  w  z          xx   yy   zz                  day    sale
   1   1  1               1  1  1   2          a     d     f                 1      78
   1   1  1               1  1  1   3          a     d     c                 2      88
   1   2  1               1  1  2   3          a     d     g                 3      72
   2   1  2               2  2  2   2          a     e     f                 4      66 
   1  1   3               2  1  1   2          b     e     f                 5      58
   2   1  3               2  2  1   3          b     e     c                 6      81
   2   2  2               2  2  2   3          b     d     g                 7      62
   2   1  2               2  2  2   2          b     e     g                 8      69

Problems:
	Write a macro that prints out two way tables from any file using proc freq.
Use it to get 
(a) table x*z from file #1, 
(b) table u*v  from file #2  where w=1.


Example.

proc freq data=xx;
 tables a*b;
    where gender = 'male';
	   run;
	  
