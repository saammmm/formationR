###test

mvnpdfT=function(x,mean,varcovM,Log=TRUE){
  for (i in 1:ncol[x]){
   new[i]=1/(2*pi)^(nrow(x)/2)*exp(-0.5*(x[i]-mean[i]))
  }


  list=c(x,new)
  return(list)
}
