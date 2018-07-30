# Hello,
# This is Mulitiple Linear Regession package
# Please  input your X and y . please add the dummy before get the final X
# linrefit(X,y,eta=0.000000000001,lmda_l1=10,lmda_l2=10,epoch=2000)
# GET THE FIT---- fit= linrefit(X,y,eta=0.000000000001,lmda_l1=10,lmda_l2=10,epoch=2000)
# linrepredict(fit,X,y )

require(ggplot2)
require(dplyr)

linrefit <- function(X,y,eta=0.000000000001,lmda_l1=0,lmda_l2=0,epoch=1000){
   set.seed(123)

  w<-as.matrix(rnorm(ncol(X)))
  J=character()
  for(i in 0:epoch){
      yhat<- as.matrix(X)%*%w
      J[i]<-(t(y-yhat)%*%(y-yhat) +lmda_l1*sum(abs(w))+lmda_l2*sum(abs(t(w)%*%w)))
      w= w-eta*(t(X)%*%(yhat-y)  ++lmda_l2*w+ lmda_l1*sign(w))
   }
   yhat<- as.matrix(X)%*%w
   R2<- 1-sum((y-yhat)^2)/sum((y-mean(y))^2)
   plot(J, col="blue", ylab="residuals", main=" Residual")
   MRE<- abs(y-yhat)/y
   ME<- median(MRE)
  Quan<-quantile(MRE, probs = 0.9)
  result<-list(w=w,coefficient_determination=R2, Median_Relative_error=ME,quantile=Quan)
  return(result)
}

linrepredict<-function(fit,X,y){
   w<-result$w
   y_hat<- as.matrix(X)%*%w
   R2<- 1-sum((y-y_hat)^2)/sum((y-mean(y))^2)
   MRE <- abs(y-y_hat)/y
   ME <- median(MRE)
   Quan<-quantile(MRE, probs = 0.9)
   testresult <-list(coefficient_determination= R2,Median_Relative_error= ME,quantile=Quan)
   return(testresult)
}
