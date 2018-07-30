# Hello,
#
# This is logistic Regession package for binary and Multiclassification
# Please just input your X and y . it will do the one hot encoding Y and add the dummy for X
#
#library(logispac)
#logisfit(X,y,eta=0.0001,epoch=2000,lmda_l2=0,lmda_l1=0)  ----you can change parameters
#get fit  ------fit=logisfit(X,y,eta=0.001,epoch=2000,lmda_l2=0,lmda_l1=0)
#logis_predict(fit,X,y)


require(ggplot2)
require(pROC)
require(ramify)

sigmoid <- function(h) {
  return(1 / (1 + exp(-h)))
}
crossentropy <- function(n, p) {
  return(-sum(y * log(p)) + (1 - y) * log(1 - p))
}
multclass_crossentropy <- function(Y, P) {
  return (-(sum(Y*log(P))))
}
softmax<-function(H){
  eH=exp(H)
  return ( eH/unlist(apply(eH,1,sum)))
}
binary_classification_rate<-function(y,p){
  return(mean(round(p)==y))
}
multclass_classification_rate<-function(y,P){
  return( mean(argmax(P,rows = TRUE)==y))
}
logisfit <- function(X,y,eta=0.001,epoch=2000,lmda_l2=0,lmda_l1=0){

  K<-length(unique(y))
  N<-nrow(X)
  D<-ncol(X)
  # train for binary logistic regression ( If K = 2)
  if(K==2){

    w <-as.matrix(rnorm(D+1))
    dummy <- matrix(rep(1, N))
    X<-as.matrix(X)
    PHI <- cbind(dummy, X)
    J <- vector()
    for (i in 1:epoch) {
      p <- sigmoid(PHI %*% w)
      J[i] <- crossentropy(y, p)+lmda_l1*sum(abs(w))+lmda_l2*sum(abs(t(w)%*%w))
      w = w - eta * (t(PHI) %*% (p - y)+lmda_l2*w+ lmda_l1*sign(w))
    }
    p <- sigmoid(PHI %*% w)
    J_plt<-plot(J, col="blue", ylab="residuals")
    title(main = "Cost Function(J)")
    class_rate<-binary_classification_rate(y,p)
    roc_obj <- roc(y,round(p))
    auc<-auc(roc_obj)
    p<-plot(roc_obj, col="red", ylab="residuals")
    title(main = "ROC Plot")
    result<-list(Classification_Rate=class_rate, ROC=p,AUC=auc, Weights=w)
    return(result)

}
 # train for multinomail logistic regression ( If K is more than 3)
  else{
    Y<-matrix(0,N,K)
    for (i in 1:N){
      Y[i,y[i]]=1
    }
    W<-matrix(rnorm(N),D+1,K)

    dummy <- matrix(rep(1, N))
    X<-as.matrix(X)
    PHI <- cbind(dummy, X)
    J <- vector()

    for (i in 1:epoch) {
      P <- softmax(PHI %*% W)
      J[i] <- multclass_crossentropy(Y, P)+lmda_l1*sum(abs(W))+lmda_l2*sum(abs(t(W)%*%W))
      W = W - eta * (t(PHI) %*% (P - Y)+lmda_l2*W+ lmda_l1*sign(W))
    }
    P<- softmax(PHI %*% W)
    plt<-plot(J, col="blue", ylab="residuals", main=" Residual")
    class_rate<-multclass_classification_rate(y,P)
    result<-list(Classification_Rate=class_rate,Weights=W)
    return(result)
  }
  }

logis_predict<-function(fit,X,y){
  K<-length(unique(y))
  N<-nrow(X)
  D<-ncol(X)

  #predict for binary logistic regression ( If K =2)
  if(K==2){
    w<-fit$Weights
    dummy <- matrix(rep(1, N))
    X<-as.matrix(X)
    PHI <- cbind(dummy, X)
    p <- sigmoid(PHI %*% w)
    class_rate<-binary_classification_rate(y,p)
    roc_obj <- roc(y,round(p))
    auc<-auc(roc_obj)
    p<-plot(roc_obj, col="red")
    title(main = "ROC Plot")

    result<-list(Classification_Rate=class_rate, ROC=p,AUC=auc)

    return(result)
  }
  #predict for multinomail logistic regression ( If K is more than 3)
  else{
    W<-fit$Weights
    Y<-matrix(0,N,K)
    for (i in 1:N){
      Y[i,y[i]]=1
    }
    X<-as.matrix(X)
    dummy <- matrix(rep(1, N))
    PHI <- cbind(dummy, X)
    P<- softmax(PHI %*% W)
    class_rate<-multclass_classification_rate(y,P)

    return(paste("Classification Rate",class_rate))
  }
}





