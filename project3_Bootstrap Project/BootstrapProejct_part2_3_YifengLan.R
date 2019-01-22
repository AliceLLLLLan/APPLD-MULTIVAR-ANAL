SIMULATIONFUNC<-function(mu.val=3,n=15,nsim=1000)
{
  #create coverage indicator vectors for bootstrap and normal
  cvec.boot<-NULL
  cvec.norm<-NULL
  #calculate real mean
  mulnorm<-(exp(mu.val+1/2))
  #run simulation
  for(i in 1:nsim){
    if((i/10)==floor(i/10)){
      print(i)
      #let me know computer hasn’t died
    }
    #sample the simulation vector
    vec.sample<-rlnorm(n,mu.val)
    #bootstrap it
    boot.list<-my.bootstrapci.ml(vec.sample)
    boot.conf<-boot.list$bootstrap.confidence.interval
    norm.conf<-boot.list$normal.confidence.interval
    #calculate if confidence intervals include mu
    #count up the coverage by the bootstrap interval
    cvec.boot<-c(cvec.boot,(boot.conf[1]<mulnorm)*(boot.conf[2]>mulnorm))
    #count up the coverage by the normal theory interval
    cvec.norm<-c(cvec.norm,(norm.conf[1]<mulnorm)*(norm.conf[2]>mulnorm))
  }
  #calculate and output coverage probability estimates
  list(boot.coverage=(sum(cvec.boot)/nsim),norm.coverage=(sum(cvec.norm)/nsim)) 
}
SIMULATIONFUNC(mu.val=3,n=15,nsim=1000)

