Heart<-read.csv("/Users/AliceLan/Desktop/R Class/Heart.csv")
my.bootstrapci.ml <-function(vec0,nboot=10000,alpha=0.1)
{
  #extract sample size, mean and standard deviation from the original data
  n0<-length(vec0)
  mean0<-mean(vec0)
  sd0<-sqrt(var(vec0))
  # create a vector to store the location of the bootstrap studentized deviation vector
  bootvec<-NULL
  #create the bootstrap distribution using a for loop
  for( i in 1:nboot){
    vecb<-sample(vec0,replace=T)
    #create mean and standard deviation to studentize
    meanb<-mean(vecb)
    sdb<-sqrt(var(vecb))
    #note since resampling full vector we can use n0 for sample size of vecb
    bootvec<-c(bootvec,(meanb-mean0)/(sdb/sqrt(n0)))
  }
  #Calculate lower and upper quantile of the bootstrap distribution
  lq<-quantile(bootvec,alpha/2)
  uq<-quantile(bootvec,1-alpha/2)
  #incorporate into the bootstrap confidence interval (what algebra supports this?) and output result
  LB<-mean0-(sd0/sqrt(n0))*uq
  UB<-mean0-(sd0/sqrt(n0))*lq
  #since I have the mean and standard deviation calculate the normal confidence interval here as well
  NLB<-mean0-(sd0/sqrt(n0))*qt(1-alpha/2,n0-1)
  NUB<-mean0+(sd0/sqrt(n0))*qt(1-alpha/2,n0-1)
  list(bootstrap.confidence.interval=c(LB,UB),normal.confidence.interval=c(NLB,NUB))
}
vec0<-Heart[[4]]
my.bootstrapci.ml(vec0,nboot=10000,alpha=0.1)
  