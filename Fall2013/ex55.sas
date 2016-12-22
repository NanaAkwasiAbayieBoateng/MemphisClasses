
ods html close; /* close previous */
ods html; /* open new */
	
*Treatment 1=6-MP,Treatment 2=Placebo*;

data exercise5;
   
   input time  Status Treatment;

   datalines;                      	
184
64
242
392
66
180
70
621
173
191
273
433
141
157
164
403
173
388
956
158
51
117
114
116
24
76
8
803
91
51
219
108
29
;

proc print data=exercise5;
run;

PROC LIFETEST DATA=exercise5;
TIME time*Status(0);
strata Treatment/TEST=(LOGRANK WILCOXON PETO);
run;
