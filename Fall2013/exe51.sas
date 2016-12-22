
ods html close; /* close previous */
ods html; /* open new */
	


data exerciseone;
   
   input time  Status Treatment;

   datalines;                      	
3.9	 1   1
5.4	 1   1
6.9	 1   2
7.7	 1   2
7.8  0	 2
7.9	 0	 1
8.2  0   2
8.2  0	 2
8.3	 1   2
10.5 1	 1
10.8 0	 2
11.0 0	 2
12.2 0	 2
12.5 0	 2
14.8 0	 2
16.0 0	 2
16.6 0	 1
16.9 0	 1
17.1 0	 1
18.1 0	 2
19.5 1	 1
21.4 0	 2
23.0 0	 2
23.8 0	 1
24.4 1	 2 
24.8 0	 2
26.9 0	 2
33.7 0	 1
33.7 0  1

;

proc print data=exerciseone;
run;

PROC LIFETEST DATA=exerciseone;
TIME time*Status(0);
strata Treatment/TEST=(LOGRANK WILCOXON PETO);
run;
