yy<-1/Auto.Train[complete.cases(Auto.Train),1] ## training data
xx<-as.matrix(Auto.Train[complete.cases(Auto.Train),-1])
yyt<-1/Auto.Test[complete.cases(Auto.Test),1] ## training data
xxt<-as.matrix(Auto.Test[complete.cases(Auto.Test),-1])
###
rleaps<-regsubsets(xx,yy,int=T,nbest=1,nvmax=dim(Auto.Train)[2],really.big=T,method=c("backward")) ## backward selection
cleaps<-summary(rleaps,matrix=T) ## True/False matrix. The r-th row is a True/False statement about which
## variables are included in model r.
tt<-apply(cleaps$which,1,sum) ## size of each model in the matrix
mses<-cleaps$rss/length(yy) ## corresponding MSEs
###
plot(tt,mses,xlab="number of variables",ylab="MSE",main="MSE for backward selection models", type="l",ylim=c(min(mses),1.25*max(mses)))
##
ss$anova
cleaps$which
# compare with step: same order of dropping variables
# prediction performance
pmses<-mses
for (ta in (2:dim(cleaps$which)[1])) {
  mmr<-lm(yy~xx[,cleaps$which[ta,-1]==T])
  PEcp<-sum((yyt-cbind(rep(1,dim(xxt)[1]),xxt[,cleaps$which[ta,-1]==T])%*%mmr$coef)^2)/length(yyt)
  pmses[ta]<-PEcp }
pmses[1]<-max(pmses)
lines(tt,pmses,lwd=2,col=2)
# prediction errors are larger than RSS on training data
ptmin<-which.min(pmses)
pmod<-cleaps$which[ptmin,]
pmod
ss$anova
### Compare seelcted model based on prediction vs F-test
#################################
### all subset selection
rleaps<-regsubsets(xx,yy,int=T,nbest=5000,nvmax=dim(Auto.Train)[2],really.big=T,method=c("ex")) ## all subset selection
cleaps<-summary(rleaps,matrix=T) ## True/False matrix. The r-th is a True/False statement about which
## variables are included in model r.
tt<-apply(cleaps$which,1,sum) ## size of each model in the matrix
mses<-cleaps$rss/length(yy) ## corresponding MSEs
###
points(tt,mses)
##
tmin<-min(tt)
tmax<-max(tt)
tsec<-seq(tmin,tmax)
# prediction errors
pmses<-mses
for (ta in (2:dim(cleaps$which)[1])) {
  mmr<-lm(yy~xx[,cleaps$which[ta,-1]==T])
  PEcp<-sum((yyt-cbind(rep(1,dim(xxt)[1]),xxt[,cleaps$which[ta,-1]==T])%*%mmr$coef)^2)/length(yyt)
  pmses[ta]<-PEcp }
pmses[1]<-max(pmses)
points(tt,pmses,col=2,pch=2)
# Best prediction models are NOT found when doing backward selection!!!
ptmin<-which.min(pmses)
pmod<-cleaps$which[ptmin,]
pmod
#
cleaps$which[sort.list(pmses)[1:10],]

