rm(list=ls())
v<-c(2, 24, 3, 4, 5, 13, 6, 1)
if(length(v)==1){
show( "enter a vector of length more than 1")
}else{

#itemcount initially set to length of v
  itemCount <- length(v)

#repeating process
  repeat {
    hasChanged <- FALSE

#updating itemcount

    itemCount <- itemCount -1
    for(i in 1:(length(v)-1)) {
      if ( v[i] > v[i+1] ) {
        t <- v[i]
        v[i] <- v[i+1]
        v[i+1] <- t
        hasChanged <- TRUE

      }

cat("The loop vector is",v,"\n")
    }
    if ( !hasChanged ) break;
  }



}

print(v)
 










 



