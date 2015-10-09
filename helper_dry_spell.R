###helper function to calculate dry spells
####
calcDrySpell <-function(dat,window,cutoff=2.5){
  #input data vector,dry spell window, and rainfall cutoff
  z<-which(dat<cutoff)
  counter=1
  #s=1
 #bucket=rep(0,50)
  if(length(z)<window) {dat=0}
  else{
    for(i in 2:(length(z)))
    {
      #print(x[z[i]])
      if((z[i]==z[i-1]+1)) {
        counter=counter+1
        if(i != length(z)){
          next
        }
      }
     # else {
       # else if(d>=13){x[z[i+1]]=1}
      
      if(counter>=window) {
        print(counter)
        if(i==length(z)){dat[z[(i-counter+1):(i)]]=-999}
       else{
         #print(z[(i-d):(i-1)])
        dat[z[(i-counter):(i-1)]]=-999
         }
      }
      counter=1
    }
  }
  return (dat)
  #return(z)
}

###


####

# drought14 <-function(x){
#   z<-which(x<2.5)
#   d=1
#   s=0
#   bucket=rep(0,50)
#   if(length(z)<14) {s=0}
#   else{
#     for(i in 1:(length(z)-1))
#     {
#       if(z[i+1]==z[i]+1) {
#         d=d+1
#         if(i<(length(z)-1)){
#           next
#         }
#       }
#       if(d>=14) {s=s+1}
#       d=1
#     }
#   }
#   return (s)
# }