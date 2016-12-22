
ods html close; /* close previous */
ods html; /* open new */
*low-fat=1  saturated fat=2 unsaturated fat=3*;	


data exercisethree;
   
   input time  Status Treatment;

   datalines;
140 1 1
177 1  1
50 1 1
65   1 1
86 1 1
153 1 1
181 1 1
191 1 1
77  1 1
84  1 1
87 1 1
56 1 1
66 1 1
73 1 1
119 1 1
140 0 1
200 0 1
200 0 1
200 0 1
200 0 1
200 0 1
200 0 1
200 0 1
200 0 1
200 0 1
200 0 1
200 0 1
200 0 1
200 0 1
200 0 1
124 1  2
58  1 2
56 1 2
68 1 2
79 1 2
89 1 2

107 1 2
86 1 2
142 1  2
 110 1 2
 96 1 2 
 142 1 2
 86 1 2 
 75 1  2
 117 1 2
 98 1  2
 105 1 2
 126 1 2
 43 1 2
 46 1 2
 81 1 2
 133 1 2
 165 1 2
 170 0 2
 200 0 2
 200 0 2
 200 0 2
 200 0 2
 200 0 2
 200 0 2
 112  1 3
 68 1 3
 84 1 3
 109 1 3
 153 1 3
 143 1 3
 60 1 3
 70 1 3
 98 1 3
 164 1 3
 63 1 3
 63 1 3
 77 1 3
 91 1 3
 91 1 3
 66 1 3
 70 1 3
 77 1 3
 63 1 3
 66 1 3
 66 1 3
 94 1 3
 101 1 3
 105 1 3
 108 1 3
 112 1 3
 115  1 3
 126 1 3
 161 1 3
 178 1 3
;

proc print data=exercisethree;
run;

PROC LIFETEST DATA=exercisethree;
TIME time*Status(0);
strata Treatment/TEST=(LOGRANK WILCOXON PETO);
run;



proc npar1way data = exercisethree;
  class Treatment;
  var time  Status ;
run;
