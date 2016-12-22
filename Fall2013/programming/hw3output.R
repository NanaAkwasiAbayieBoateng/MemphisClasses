> exponential.canonical.nr(x,y,alpha0=0.9,beta0=2.9,tol=10^-6)
The alpha is 0.9266663 
The beta is 3.156383 
The alpha is 0.9278861 
The beta is 3.202168 
The alpha is 0.9278553 
The beta is 3.203261 
The alpha is 0.9278553 
The beta is 3.203261 
$alpha
[1] 0.9278553

$beta
[1] 3.203261

[[3]]
[1] 2.778619e-07 2.280463e-07

[[4]]
          [,1]       [,2]
[1,] -1.962548 -0.6181910
[2,] -0.618191 -0.4517824

$number.iterations
[1] 4

> 
> system.time(exponential.canonical.nr(x,y,alpha0=0.9,beta0=2.5,tol=10^-6))
The alpha is 0.9317891 
The beta is 2.985285 
The alpha is 0.9289674 
The beta is 3.182069 
The alpha is 0.9278677 
The beta is 3.203067 
The alpha is 0.9278553 
The beta is 3.203261 
The alpha is 0.9278553 
The beta is 3.203261 
   user  system elapsed 
      0       0       0 



> exponential.nr(x,y,alpha0=0.9,beta0=2.9,tol=10^-6)
The alpha is 1.043935 
The beta is 2.91298 
The alpha is 1.059615 
The beta is 2.905817 
The alpha is 1.059736 
The beta is 2.905739 
The alpha is 1.059736 
The beta is 2.905739 
$alpha
[1] 1.059736

$beta
[1] 2.905739

[[3]]
[1] 4.585409e-09 1.362246e-09

[[4]]
           [,1]       [,2]
[1,] -1.1658000 -0.6163601
[2,] -0.6163601 -0.5797279

$number.iterations
[1] 4

