
v=c(2, 24, 3, 4, 5, 13, 6, 1)
#haschanged initially set to true 
haschanged=1
count=length(v)

while(haschanged==1){
haschanged=0

count = count - 1

for(i in seq(1, length(v)-1)){
if (v[i]>v[i+1]){
#swapping stage
t=v[i]
v[i]=v[i+1]

v[i+1]=t
 count = count - 1
haschanged = 1
}
cat("The loop vector is",v,"\n")


}
if ( !haschanged ) break;

}

print(v)






