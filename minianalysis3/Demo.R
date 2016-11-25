### R code from vignette source 'Lecture9-2015.Rnw'

###################################################
### code chunk number 1: selldl1
###################################################
#SA<-data.frame(read.table("SA.dat",header=T)) ## read in the data
#SAuse<-SA
#SAuse$ldl<-log(SA$ldl)
#SAuse$obesity<-log(SA$obesity)
#SAuse$age<-log(SA$age)
#yy<-SAuse[,12]
#xx<-SAuse[,-12]
d <- gen(1000,9,2)
xx<-d$data[,-10]
yy<-d$data[,10]
##
library(leaps)
rleaps<-regsubsets(xx,yy,int=T,nbest=250,nvmax=250,really.big=T,method=c("ex")) 
## all subset models
cleaps<-summary(rleaps,matrix=T) 
## True/False matrix. The r-th is a True/False statement about which
Models<-cleaps$which
Models<-rbind(c(T,rep(F,dim(xx)[2])),Models) 
## adding the empty model (just an intercept) to the model matrix


###################################################
### code chunk number 2: selldl2
###################################################
K<-10
ii<-sample(seq(1,length(yy)),length(yy)) ## random perturbation of observations first.
foldsize<-floor(length(yy)/K)
sizefold<-rep(foldsize,K)
restdata<-length(yy)-K*foldsize
if (restdata>0) {
  sizefold[1:restdata]<-sizefold[1:restdata]+1 }
## creates the size for each fold


###################################################
### code chunk number 3: selldl3
###################################################
Prederrors<-matrix(0,dim(Models)[1],K)
# a matrix to store the prediction errors in
iused<-0
Xmat<-as.matrix(cbind(rep(1,dim(xx)[1]),xx)) # the design matrix
for (k in (1:K)) {
  itest<-ii[(iused+1):(iused+sizefold[k])] ## the k-fold test set
  itrain<-ii[-c((iused+1):(iused+sizefold[k]))] ## the k-fold training set
  iused<-iused+length(itest)
  for (mm in (1:dim(Models)[1])) {
    betahat<-solve(t(Xmat[itrain,Models[mm,]])%*%
                     Xmat[itrain,Models[mm,]])%*%t(Xmat[itrain,Models[mm,]])%*%yy[itrain]
    ypred<-Xmat[itest,Models[mm,]]%*%betahat ## predictions
    Prederrors[mm,k]<-sum((yy[itest]-ypred)^2) } }
PE<-apply(Prederrors,1,sum)/length(yy)  ## final prediction errors, average across all folds.


###################################################
### code chunk number 4: selldl4
###################################################
jj<-sort.list(PE)[1:5]
print(as.matrix(Models[jj,]))


###################################################
### code chunk number 5: selldl4b
###################################################
z<-data.frame(as.matrix(cbind(Prederrors[jj,],PE[jj])))
colnames(z) <- c("Fold1","Fold2","Fold3","Fold4","Fold5","Fold6","Fold7",
                 "Fold8","Fold9","Fold10","PE")
row.names<-c(seq(1,5))
library(xtable)
xtable(z, digits=c(0, rep(0,10),3),
       caption="Prederrors in different folds and total",label="tab:CVldl")


###################################################
### code chunk number 6: cvldl5
###################################################
winmod<-Models[which.min(PE),]
print(winmod)


###################################################
### code chunk number 7: selldl6
###################################################
mm<-lm(log(ldl)~log(age)+sbp+adiposity+log(obesity)+typea+alcohol+alcind+
         tobacco+tobind+as.factor(chd)+as.factor(famhist),data=SA)
ss<-step(mm,trace=F) ## backward selection (using AIC)
print(ss)


###################################################
### code chunk number 8: sel1
###################################################
Models<-cleaps$which
Models<-rbind(c(T,rep(F,dim(xx)[2])),Models)
nullrss<-sum((yy-mean(yy))^2)
RSS<-cleaps$rss
RSS<-c(nullrss,RSS)
modsize<-apply(Models==T,1,sum)
BIC<-length(yy)*log(RSS)+modsize*log(length(yy))
mse<-min(RSS)/(length(yy)-max(modsize))
CP<-(RSS+2*modsize*mse)/mse-length(yy)
AIC<-length(yy)*log(RSS)+2*modsize


###################################################
### code chunk number 9: sel1a
###################################################
plot(modsize,CP,xlab="modelsize",main="Cp")


###################################################
### code chunk number 10: sel1b
###################################################
plot(modsize,AIC,xlab="modelsize",main="AIC")


###################################################
### code chunk number 11: sel1c
###################################################
plot(modsize,BIC,xlab="modelsize", main="BIC")


###################################################
### code chunk number 12: selone
###################################################
rleaps<-regsubsets(xx,yy,int=T,nbest=1,nvmax=dim(xx)[2]+1,really.big=T,method=c("ex")) 
## nbest=1: the best model of each size
cleaps<-summary(rleaps,matrix=T)
# just the best model of every size
Models<-cleaps$which
Models<-rbind(c(T,rep(F,dim(xx)[2])),Models)
nullrss<-sum((yy-mean(yy))^2)
RSS<-cleaps$rss
RSS<-c(nullrss,RSS)
modsize<-apply(Models==T,1,sum)
BIC<-length(yy)*log(RSS)+modsize*log(length(yy))
mse<-min(RSS)/(length(yy)-max(modsize))
CP<-(RSS+2*modsize*mse)/mse-length(yy)
AIC<-length(yy)*log(RSS)+2*modsize
par(mfrow=c(1,1))
plot(modsize,CP-min(CP),type="l",main="Cp-black,AIC-blue,BIC-red")
lines(modsize,AIC-min(AIC),col="blue")
lines(modsize,BIC-min(BIC),col="red")
abline(h=0,lty=2)


###################################################
### code chunk number 13: selcomp
###################################################
cpmod<-Models[CP=min(CP),]
aicmod<-Models[AIC==min(AIC),]
bicmod<-Models[BIC==min(BIC),]
print(cpmod)
print(aicmod)
print(bicmod)

