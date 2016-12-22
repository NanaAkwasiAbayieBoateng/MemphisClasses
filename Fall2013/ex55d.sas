
ods html close; /* close previous */
ods html; /* open new */
	
*#2=MUMPS >= 5*5mm,10*10mm for others
#1=MUMPS <5*5mm,10*10mm for others*;
*PPD;
data exercise5C;
   
   input time  Status Treatment;

   datalines;                      	
184 0  1        1,1,1,2,2,2,2,1,2,1,1,1,2,1,1,1,2,1,NA,2,2,2,2,2,2,2,2,2,2,2,2,2,2    
64  1   1
242  0  1
392 1  2
66  1  2
180 1  2
70  1  2 
621 1  1
173 0  2
191  1 1
273  0  1
433 0  1
141 1  2
157 0  1
164 0  1
403 0 1
173 1 2
388  0  1
956 0 .
158 1 2
51 1  2
117 0  2
114 1  2
116 0 2
24 1  2
76  1  2 
8  1 2
803 0  2
91  1 2
51 1  2
219 0 2
108 0 2
29 1   2
;

proc print data=exercise5C;
run;

PROC LIFETEST DATA=exercise5C;
TIME time*Status(0);
strata Treatment/TEST=(LOGRANK WILCOXON PETO);
run;
