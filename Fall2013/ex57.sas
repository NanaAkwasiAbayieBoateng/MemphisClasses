
ods html close; /* close previous */
ods html; /* open new */
	


data exerciseseven;
   
   input time  Status Treatment;

   datalines;                      	
4	1  1		
5   1	1
8   1	2
8	1  3
9	1  1
10	1  1
10	1  1
10	1  2                			
10	1  2
10	1 3			
1   1  3
1	1  1
12	1  2
12	1  3
12	0  1   		
13	1  2
14	1  2
20	1  1
20	0  1
23	1   3
23	1   3
25	1   3
25	1 1	  	
28	1  1
28	1  1
28	1  3
28	1  3
28	1  1
29	1  1
31	1  3
31	1  3
31	1  1 		
32	1  1
37	1  3
40	1   1
41	1  1
41	1  2		
48	1  3
48	1  1
57	1  1
62	1  2
70	1  1
74	1  2
75	1  3
89	1  1
99	1  1
100	1  2
103	1  3
124	1  1
139	 1  3		
143	 1  3
159	 0  2   
161	 0  2
162	 1  2
169	 1  2
190  0	 3
195	 1  2
196  0	3
197 0	 3
199  0	 2
205  0 	2
217  0	3	
219  0	3
220	 1  2
245  0	2
258 0	1
269 0   1
;

proc print data=exerciseseven;
run;

PROC LIFETEST DATA=exerciseseven;
TIME time*Status(0);
strata Treatment/TEST=(LOGRANK WILCOXON PETO);
run;
