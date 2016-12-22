************* HW3 **************;
   dm log 'clear';
  
  
Data answer;
  input name$ test @@;
   do k=1 to 20;  input ans $ @@;  output;
    
           end;
 
Datalines;
AA 1  a a a a a b b b b b c c c c c d d d d d
AA 2  b b b b b c c c c c d d d d d a a a a a
AA 3  a b c a b c a b c a b c a b c a b c d d
AA 4  d d d d d d d d d d d d d d d d d d d d
Tom  1 a b c d a b c d a b c d a b c d a b c d
Tom  2 a a a a a a a a a a a a a a a a a a a a
Tom  3 b b b b b b b b b b b b b b b b b b b b
Tom  4 d d d d d d d d d d d d d d d d d d d d
Dick 1 c c c c c c c c c c c c c c c c c c c c
Dick 2 d d d d d d d d d d d d d d d d d d d d
Dick 3 a a a a a a a a a a a a a a a a a a a a
Dick 4 b b b b b b b b b b b b b b b b b b b b
Jenny 1 d c b a d c b a d c b a d c b a d c b a 
Jenny 2 d d d d d d d d d d d d d d d d d d d d
Jenny 3 c c c c c c c c c c b b b b b b b b b b
Jenny 4 a a a a a a a a a a b b b b b b b b b b
 run;
 
 /* Problem:
  
   1. Write a marco that would give the number of correct answers for any test 
      of any student. Find the number of correct answers in test #2 for Tom.

   2. Write a macro that would give for any student the total number and percent
      of correct answers of all tests. Find the total number and percent of 
      correct answers for Jenny.

   3. Write a macro that can give the total number of correct answers of all 
      tests or the number of correct answers for any specified test. 
      Use this macro to answer problems 1 and 2.
