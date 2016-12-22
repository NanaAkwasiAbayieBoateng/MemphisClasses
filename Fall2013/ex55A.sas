
ods html close; /* close previous */
ods html; /* open new */
	
*#2=MUMPS >= 5*5mm,10*10mm for others
#1=MUMPS <5*5mm,10*10mm for others*;
*monilia;
data exercise5A;
   
   input time  Status Treatment;

   datalines;                      	
184 0  2
64  1  1
242  0 2
392 1  2
66  1 1
180 1 1
70  1  1
621 1 2
173 0 2
191  1 1
273  0 2
433 0  2
141 1  2
157 0  2
164 0 2
403 0 1
173 1 2
388  0 2
956 0 2
158 1 2
51 1 1
117 0 1
114 1 2
116 0 2
24 1 1
76  1 2
8  1 2
803 0 1
91  1 1
51 1 2
219 0 2
108 0 2
29 1 1
;

proc print data=exercise5A;
run;

PROC LIFETEST DATA=exercise5A;
TIME time*Status(0);
strata Treatment/TEST=(LOGRANK WILCOXON PETO);
run;
