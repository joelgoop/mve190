ModelSelection<-function(yy,xx,nb=1000,plotout=F) {
##
# plotout=T if you want to see the output plots and tables as the code runs.
library(leaps)
rleaps<-regsubsets(xx,yy,int=T,nbest=nb,nvmax=dim(xx)[2]+1,really.big=T,method=c("ex")) 
cleaps<-summary(rleaps,matrix=T) 
## True/False matrix. The r-th is a True/False statement about which
Models<-cleaps$which
Models<-rbind(c(T,rep(F,dim(xx)[2])),Models) 
#
nullrss<-sum((yy-mean(yy))^2)
RSS<-cleaps$rss
RSS<-c(nullrss,RSS)
modsize<-apply(Models==T,1,sum)
BIC<-length(yy)*log(RSS)+modsize*log(length(yy))
mse<-min(RSS)/(length(yy)-max(modsize))
CP<-(RSS+2*modsize*mse)/mse-length(yy)
AIC<-length(yy)*log(RSS)+2*modsize
Rsq<-cleaps$rsq
#
if (plotout==T) {
 par(mfrow=c(2,2))
 plot(modsize,CP,xlab="modelsize",main="Cp")
 plot(modsize,AIC,xlab="modelsize",main="AIC")
 plot(modsize,BIC,xlab="modelsize", main="BIC") }
#
rleaps<-regsubsets(xx,yy,int=T,nbest=1,nvmax=dim(xx)[2]+1,really.big=T,method=c("ex")) 
cleaps<-summary(rleaps,matrix=T)
# just the best model of every size
Models1<-cleaps$which
Models1<-rbind(c(T,rep(F,dim(xx)[2])),Models1)
nullrss<-sum((yy-mean(yy))^2)
RSS1<-cleaps$rss
RSS1<-c(nullrss,RSS1)
modsize1<-apply(Models1==T,1,sum)
BIC1<-length(yy)*log(RSS1)+modsize1*log(length(yy))
mse<-min(RSS1)/(length(yy)-max(modsize1))
CP1<-(RSS1+2*modsize1*mse)/mse-length(yy)
if (plotout==T) {
AIC1<-length(yy)*log(RSS1)+2*modsize1
plot(modsize1,CP1-min(CP1),type="l",main="Cp-black,AIC-blue,BIC-red")
lines(modsize1,AIC1-min(AIC1),col="blue")
lines(modsize1,BIC1-min(BIC1),col="red")
abline(h=0,lty=2) }
#
cpmod<-Models[CP==min(CP),]
aicmod<-Models[AIC==min(AIC),]
bicmod<-Models[BIC==min(BIC),]
#
modtab<-rbind(cpmod,aicmod,bicmod)
if (plotout==T) {
print(modtab) }
#
return(list(Models=Models,AIC=AIC,BIC=BIC,CP=CP,Rsq=Rsq,modtab=modtab))
}

