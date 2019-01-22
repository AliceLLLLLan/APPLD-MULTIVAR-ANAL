delay.map.make<-function(mat.pred,ycol,xcol,lag){
  xmat.pred<-NULL
  lagvec<-c(1:(lag-1))
  y.pred<-(mat.pred[,ycol])[-c(lagvec,lag)]
  print(dim(mat.pred))
  print(length(y.pred))
  nyp<-length(y.pred)
  x1 <-mat.pred[, xcol]
  n1<-length(mat.pred[,1])
  lag1 <- max(lagvec)
  delag <- 1
  for(i in lagvec) {
    v1 <- c((n1 - (delag + i - 1)):n1)
    if(lag1 > i) {
      v2 <- c(1:(lag1 - i))
      x2a <- x1[ - v1,  ]
      x2 <- x2a[ - v2,  ]
    }
    else {
      x2 <- x1[ - v1,  ]
    }
    xmat.pred <- cbind(xmat.pred, x2)
  }
  xmat.pred<-xmat.pred[1:nyp,]
  print(dim(xmat.pred))
  list(y=diff(y.pred),x=(xmat.pred[-length(xmat.pred[,1]),]))
}

BeijingNA<-read.csv("/Users/AliceLan/Desktop/R Class/Final project R/FiveCitiePMData/BeijingPM20100101_20151231.csv",header = TRUE,na.strings = "NA")
#any(is.na(BeijingNA))
#str(BeijingNA)
sum(is.na(BeijingNA))
Beijing<-na.omit(BeijingNA) #removes all rows that contains missing data
#sum(is.na(Beijing))

Beijing$season[Beijing[[3]]==1]<- 4
Beijing$season[Beijing[[3]]==2]<- 4
Beijing$season[Beijing[[3]]==12]<- 4
Beijing$season[Beijing[[3]]==3]<- 2
Beijing$season[Beijing[[3]]==4]<- 2
Beijing$season[Beijing[[3]]==5]<- 2
Beijing$season[Beijing[[3]]==6]<- 1
Beijing$season[Beijing[[3]]==7]<- 1
Beijing$season[Beijing[[3]]==8]<- 1
Beijing$season[Beijing[[3]]==9]<- 3
Beijing$season[Beijing[[3]]==10]<- 3
Beijing$season[Beijing[[3]]==11]<- 3

Beijing1014<- Beijing[ which(Beijing$year<2015), ]
Beijing15<- Beijing[ which(Beijing$year==2015), ]

#mat.pred<-as.matrix(Beijing1014)
mat.pred<-as.matrix(Beijing1014[,c(7:11)])
#xcol<-c(1:14)
xcol<-c(1:5)
#ycol<-7
ycol<-1

#lag= hour behind+1


#create data struction for the linear regression of the stochastic time series predicting
trial<-delay.map.make(mat.pred,ycol,xcol,4)
ls.print(lsfit(trial$x,trial$y))
summary(lars(trial$x,trial$y))
predict.lars(lars(trial$x,trial$y))
predict.lars(lars(trial$x,trial$y),trial$x)
dum<-predict.lars(lars(trial$x,trial$y),trial$x)
dum$fit
plot(lars(trial$x,trial$y))
plot(predict.lars(lars(trial$x,trial$y),trial$x)$fit[,15],trial$y)
par(mfrow=c(1,1))
plot(predict.lars(lars(trial$x,trial$y),trial$x)$fit[,15],trial$y)
cor(predict.lars(lars(trial$x,trial$y),trial$x)$fit[,15],trial$y)
