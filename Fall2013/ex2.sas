
ods html close; /* close previous */
ods html; /* open new */
	


data exercisetwo;
   
   input time  Status Treatment;

   datalines;
1.8  1  1   
2.3 1   1
3.8 1   1
5.5 1   1
6.3  1  1 
6.4  1  1
16.6 0  1
17.1 0  1
23.8 0  1
33.7 0  1
33.7 0  1
2.8  1  2
3.0  1  2
4.3  1  2
4.5  1  2
5.8  1  2
6.8  1  2
7.8  0  2
8.2  0  2
8.2  0  2
9.2  1  2
9.2  1  2
10.8 0  2
11.0 0  2
15.9 1  2
18.1 0  2
21.4 0  2
22.1 1  2
23.0 0  2
26.9 0	2 


;

proc print data=exercisetwo;
run;

PROC LIFETEST DATA=exercisetwo;
TIME time*Status(0);
strata Treatment/TEST=(LOGRANK WILCOXON PETO);
run;
