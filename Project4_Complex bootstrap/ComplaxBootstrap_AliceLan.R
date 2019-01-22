#Load AUTO data
library(ISLR)
attributes(Auto)
#subseting AUTO data into newdata with only 7 variables for modeling
myvars <- c("mpg", "cylinders", "displacement", "horsepower", "weight", "acceleration", "year")
newdata <- Auto[myvars]  
#matrixlize newdata
newdata.mat<-as.matrix(newdata)
#Choose "acceleration" as dependent variable, 
#"horsepower" as the index independent variable that seperate the data into 9/10 and 1/10 bease on percentile
low90<-subset(newdata, horsepower <= quantile(newdata$horsepower, 0.9))
high10<-subset(newdata, horsepower > quantile(newdata$horsepower, 0.9))
low90.mat<-as.matrix(low90)
high10.mat<-as.matrix(high10)
x<-low90.mat[,c("mpg", "cylinders", "displacement", "horsepower", "weight", "year")]
y<-low90.mat[,c("acceleration")]
xnew<-high10.mat[,c("mpg", "cylinders", "displacement", "horsepower", "weight", "year")]
ynew<-high10.mat[,c("acceleration")]

regpred<-function(xpred,xmat,y){
  #calculate regression
  ls.str<-lsfit(xmat,y)
  #calculate prediction
  if(is.matrix(xpred)){
    ypred<-(cbind(1,xpred)%*%ls.str$coef) 
  }
  else{
    ypred<-cbind(1,xpred)%*%ls.str$coef
  }
  #use ls.diag to extract covariance matrix
  ycov<-ls.diag(ls.str)$cov.unscaled
  #use ls.diag to extract std deviation
  std.dev<-ls.diag(ls.str)$std.dev
  #variance of data around line
  v1<-std.dev^2
  #variance of prediction
  if(is.matrix(xpred)){
    vpred<-diag(v1*(cbind(1,xpred))%*%ycov%*%t(cbind(1,xpred)))
  }
  else{
    vpred<-v1*(cbind(1,xpred))%*%ycov%*%t(cbind(1,xpred))
    
  }
  df=length(y)-length(diag(ycov))
  list(pred=ypred,sd=sqrt(vpred),df=df)
}


predregboot0<-function(x, y, xnew,  leaps=T, select.n=5,plot.it=T)
{
  if(plot.it)
    #if want to plot the graph
    {par(mfrow=c(1,2))} #arrange the position of two graphs
  if(leaps){
    # if leaps=TRUE
    leaps.str<-leaps(x,y)
    # assign the estimation line to leaps.str
    Cpvec<-leaps.str$Cp
    mod.size<-leaps.str$size 
    which.str<-leaps.str$which #true-flase table
    o1<-order(Cpvec)#order the cpvec to choose the model with smallest cp
    if(plot.it){   
    plot(Cpvec, mod.size, main="Cp vs Model Size, leaps", log="y")
      #plot the relationship between Cp and size of the model
      }
    mod.size.sel<-mod.size[o1][c(1:select.n)]
    #order model size depend on the accending order of cp
    I1<-mod.size.sel==min(mod.size.sel) 
    #choose the min cp with min model size
    model.id<-(o1[c(1:select.n)][I1])[1]
    model.indc<-which.str[model.id,] 
    # get the TRUE table of the selected best model to see which independent factors are selected
    x.model<-x[,model.indc] 
    #collect all the data with selected variables
    ls.str<-lsfit(x.model,y)#test the model
    vinf<-regpred(xnew[,model.indc],x.model,y)# culculate regression and prediction
    if(is.matrix(xnew[,model.indc])){
    pred0<-xnew[,model.indc]%*%c(ls.str$coef[-1])+ls.str$coef[1]
    yhat<-x.model%*%c(ls.str$coef[-1])+ls.str$coef[1]
    
  }
  else{
    pred0<-xnew[,model.indc]*ls.str$coef[-1]+ls.str$coef[1]
    yhat<-x.model*(ls.str$coef[-1])+ls.str$coef[1]
    
  }
    Beta.out<-list(coef=ls.str$coef,model.indicator=model.indc,pred=pred0,sd=vinf$sd,df=vinf$df,resid=ls.str$resid,yhat=yhat)
  }else{
    #if leap=FALSE, use Lars
    lars.str<-lars(x,y)
    Cpvec<-lars.str$Cp
    mod.size<-lars.str$df
    o1<-order(Cpvec)
    mod.size.sel<-mod.size[o1][c(1:select.n)]
    I1<-mod.size.sel==min(mod.size.sel)
    model.id<-o1[c(1:select.n)][I1][1]
    #all these above are similar to the leaps part
    lambda<-lars.str$lambda[model.id]
    pred0<-predict.lars(lars.str,xnew,lambda,type="fit",mode="lambda")$fit #predict 1/10 part
    yhat<-predict.lars(lars.str,x,lambda,type="fit",mode="lambda")$fit #predict 9/10 part
    resid0<-y-yhat
    if(plot.it){   
      plot(mod.size, Cpvec, main="Cp vs Model Size, lars", log="y")
      }
    Beta.out<-list(coef=coef.lars(lars.str,lambda,mode="lambda"),pred=pred0,resid=resid0,yhat=yhat)
  }
  Beta.out
}

quantile.alpha<-function(vec,alpha=.1){
  c(quantile(vec,alpha/2),quantile(vec,1-alpha/2))
}


predregboot1<-function(Beta.str,Beta0,ind.leaps=T){
  #library(leaps)
  #library(lars)
  if(ind.leaps){
    #Beta.out<-list(coef=coef,model.indicator=model.ind,pred=pred0,sd=vinf$sd,df=vinf$df,resid=ls.str$resid,yhat=yhat)
    stud.pred.mat<-NULL
    pred0<-Beta0$pred 
    #Beta.out is the Beta.out from predregboot1(Beta.str,Beta.out,leaps.ind), culculated by predregboot0(x,y,xnew,leaps.ind,select.n),
    n1<-length(Beta.str)
    for(i in 1:n1){
      predvec0<-(Beta.str[[i]]$pred-pred0)/Beta.str[[i]]$sd # calculate the predicted mean/sd
      stud.pred.mat<-rbind(stud.pred.mat,c(predvec0)) 
      #rbind() function combines vector, matrix or data frame by rows
    }
    bounds<-apply(stud.pred.mat,2,quantile.alpha)
    #APPLY() returns a vector or array or list of values obtained by applying a function to margins of an array or matrix
    pred.bounds<-cbind(c(pred0-bounds[2,]*Beta0$sd),c(pred0-bounds[1,]*Beta0$sd))
  }else{
    #Beta.out<-list(coef=coef,pred=pred0,resid=resid0,yhat=yhat)
    stud.pred.mat<-NULL
    pred0<-Beta0$pred
    n1<-length(Beta.str)
    for(i in 1:n1){
      predvec0<-(Beta.str[[i]]$pred-pred0)
      stud.pred.mat<-rbind(stud.pred.mat,c(predvec0))
    }
    bounds<-apply(stud.pred.mat,2,quantile.alpha)
    pred.bounds<-cbind(c(pred0-bounds[2,]),c(pred0-bounds[1,]))
  }
  pred.bounds
}


predregboot<-function(x, y, xnew, ynew=NULL, leaps.ind=T, resid.boot=T, select.n=5,nboot=10000)
{
  #loading leaps and lars
  library(leaps)
  library(lars)
  Beta.str<-list() #list() is a functions to construct, coerce and check for both kinds of R lists.
  Beta.out<-predregboot0(x,y,xnew,leaps.ind,select.n)
  #bootstrap section
  n1<-length(y)
  for(i in 1:nboot){
    #if(floor(i/100)==(i/100)){
    #  print(i)
    #}
    bn<-(sample(n1,replace=T))
    if(resid.boot){
      resid<-Beta.out$resid
      yhat<-Beta.out$yhat
      bresid<-resid[bn]
      yb<-yhat+bresid
      Beta.out1<-predregboot0(x,yb,xnew,leaps.ind,select.n,plot.it=F)
      Beta.str[[i]]<-Beta.out1
    }else{
      xb<-x[bn,]
      yb<-y[bn]
      Beta.out1<-predregboot0(xb,yb,xnew,leaps.ind,select.n,plot.it=F)
      Beta.str[[i]]<-Beta.out1
    }
  }
  bounds<-predregboot1(Beta.str,Beta.out,leaps.ind)
  bounds
}

boundsLeaps<-predregboot(x, y, xnew, ynew=NULL, leaps.ind=T, resid.boot=T, select.n=5,nboot=1000)
boundsLars<-predregboot(x, y, xnew, ynew=NULL, leaps.ind=F, resid.boot=T, select.n=5,nboot=1000)

CompareLeaps<-array(dim = length(ynew));
for(i in 1:length(ynew)){
  if((ynew[i]>=boundsLeaps[i,1])&&(ynew[i]<=boundsLeaps[i,2])){
    CompareLeaps[i]=TRUE
  }else if(ynew[i]<boundsLeaps[i,1]){
    CompareLeaps[i]='LOW'
  }else{
    CompareLeaps[i]='HIGH'
  }
}
CompareLeaps.mat<-as.matrix(CompareLeaps)
ynew.mat<-as.matrix(ynew);
boundsLeaps<-cbind(boundsLeaps,ynew.mat,CompareLeaps.mat)
colnames(boundsLeaps) <- c("lowerbound", "upperbound", "ynew", "comparison")
boundsLeaps

CompareLars<-array(dim = length(ynew));
for(i in 1:length(ynew)){
  if((ynew[i]>=boundsLars[i,1])&&(ynew[i]<=boundsLars[i,2])){
    CompareLars[i]=TRUE
  }else if(ynew[i]<boundsLars[i,1]){
    CompareLars[i]='LOW'
  }else{
    CompareLars[i]='HIGH'
  }
}
CompareLars.mat<-as.matrix(CompareLars)
ynew.mat<-as.matrix(ynew);
boundsLars<-cbind(boundsLars,ynew.mat,CompareLars.mat)
colnames(boundsLars) <- c("lowerbound", "upperbound", "ynew", "comparison")
boundsLars

