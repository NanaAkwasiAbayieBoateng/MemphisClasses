dm log 'clear';
  dm output 'clear';
ods html close; ods html;
option mprint symbolgen;

  

	  data y2001;
      input id name $ salary;
	  datalines;
	  12  aaa  35000
	   9  bkg  45500
	   6  klo  32880
	   8  sou  28900
	  24  der  19000
	  11  prt  39500
	  15  qqq  29990
	  run;
	  
	   data y2002;
      input ssn last $ salary;
	  datalines;
	  12  aaa  36000
	  35  xoy  28900
	   9  bkg  47500
	   6  klo  32880
	  24  der  23000
	  11  prt  45000
	  10  tty  43900
	  31  qrs  41000
	  8   sou  38900
	 run;
	
	   data y2003;
      input id name $ salary;
	  datalines;
	  12  aaa  39000
	  35  xoy  32300
	   6  klo  37000
	  24  der  29000
	  17  vop  35800
	  15  wer  50900
	  10  tty  46900
	  41  lle  31500
	  43  obd   52000
	  24  der  25900
	  9   bkg   45000
	 run;
