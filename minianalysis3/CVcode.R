CVK<-function(yy,xx,K,nb=1000,plotout=F) {
  ### Performs K-fold CV 
  library(leaps)
  #
  rleaps<-regsubsets(xx,yy,int=T,nbest=nb,nvmax=(dim(xx)[2]+1),really.big=T,method=c("ex")) ## all subset models
  cleaps<-summary(rleaps,matrix=T) ## True/False matrix. The r-th is a True/False statement about which
  Models<-cleaps$which
  Models<-rbind(c(T,rep(F,dim(xx)[2])),Models)
  #####
  ii<-sample(seq(1,length(yy)),length(yy))
  foldsize<-floor(length(yy)/K)
  sizefold<-rep(foldsize,K)
  restdata<-length(yy)-K*foldsize
  if (restdata>0) {
    sizefold[1:restdata]<-sizefold[1:restdata]+1}
  #
  Prederrors<-matrix(0,dim(Models)[1],K)
  # a matrix to store the prediction errors in
  iused<-0
  Xmat<-as.matrix(cbind(rep(1,dim(xx)[1]),xx))
  for (k in (1:K)) {
    itest<-ii[(iused+1):(iused+sizefold[k])] ## the k-fold test set
    itrain<-ii[-c((iused+1):(iused+sizefold[k]))] ## the k-fold training set
    iused<-iused+length(itest)
    for (mm in (1:dim(Models)[1])) {
      betahat<-solve(t(Xmat[itrain,Models[mm,]])%*%Xmat[itrain,
                                                        Models[mm,]])%*%t(Xmat[itrain,Models[mm,]])%*%yy[itrain]
      ypred<-Xmat[itest,Models[mm,]]%*%betahat ## predictions
      Prederrors[mm,k]<-sum((yy[itest]-ypred)^2) } }
  PE<-apply(Prederrors,1,sum)/length(yy)  ## final prediction errors, average across all folds.
  #
  winmod<-Models[which.min(PE),]
  if (plotout==T) {
  #print(winmod) 
  }
  # 
  if (plotout==T) {
  jj<-sort.list(PE)[1:5]
  #print(as.matrix(Models[jj,]))
  #
  #plot(c(1,apply(cleaps$which,1,sum)),PE,xlab="model size",ylab="CV error") 
  }
  #
  return(list(winmod=winmod,Models=Models,PE=PE))
}