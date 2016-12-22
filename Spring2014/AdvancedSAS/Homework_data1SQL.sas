

    
  dm log 'clear';
  dm output 'clear';


 
  data z3;

  input id name $ 6-23  gender $24-30 dom MMDDYY9. birthdate MMDDYY9. dept $ 
      salary1 salary2 salary3 ; 
  
  datalines;
   1 joan  a. smith    female 09 22 96 12 20 64  shoe      40300 48800 50000
   2 john c.  park     male   07 13 90 09 25 65  clothing  50000 77000 78000
   3  mike    m. katz  male   03 15 98 06 30 60  applianc  27000 34000 33500
   4 jenny m. park     female 12 07 95 11 13 62  clothing  45500 62300 66000
   5 mike j. flemming  male   03 15 95 10 27 70  shoe      27800 33800 44000
   6 john p.   manley  male   04 18 97 02 18 75  clothing  23000 28800 31500
   7 susy m. smith     female 09 14 99 03 30 89  clothing  19500  .     .
   8 lisa k. kapling   female 08 23 97 12 15 75  applianc  37900 52000  .
   9 bob  k. park      male   11 12 93 10 10 72  clothing  22000 24500 33800     
  10  nancy p. kwan    female 10 15 94 09 16 73  clothing  23500 26000 29500
  11      miny d. king female 10 22 98 11 22 79  toy       21000 23000 25500
  12 jenny a. moby     female 08 06 99 09 30 80  toy       19000 20000 21000
    run;
   data address;

  input id name $ 6-23  address $ 25-70;
  
  datalines;
   1 joan     smith    1245 first street,  memphis,  tn  38123
   2 john     park     345  second avenue, southhaven, tn 38145
   3  mike       katz  2145   third street,  memphis,  tn 38123
   5 mike    flemming  297  park ave,  southhaven,  tn 38145 
   6 john      manley  214 poplar, northhaven,  tn 38145
   7 susy    smith     159 highland, memphis, tn 38129
   9 bob     park      345  second avenue, southhaven, tn 38145
  10  nancy  kwan    5345  orangetown, memphis,  tn  38142
  11      miny d. king 246  king street,  memphis,  tn 38145
 run;
   data rating;
     input  id  score1  score2  rater1  rater2   status $;
	 datalines;
	 3  78  67  5  7  m
	 5  88  97  8  9  s
	10  88  86  9  7  s
	 9  55  62  7  6  m
	 8  66  72  6  5  m
	 6  77  73  7  8  m
	 2  94  99  6  7  s
	 1  77  72  8  9  s
	12  66  69  6  6  s
	 run;
