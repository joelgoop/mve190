library(leaps)
par(mfrow=c(1,1))

noimp.auto.train <- Auto.Train
noimp.auto.test <- Auto.Test

# 210 possible outlier, but prediction better with it
# imp.auto.train <- imp.auto.train[!rownames(imp.auto.train) %in% c("210"),]

# Not imputed
#yy<-1/noimp.auto.train[complete.cases(noimp.auto.train),1] ## training data
#xx<-as.matrix(noimp.auto.train[complete.cases(noimp.auto.train),-1])
#yyt<-1/noimp.auto.test[complete.cases(noimp.auto.test),1] ## training data
#xxt<-as.matrix(noimp.auto.test[complete.cases(noimp.auto.test),-1])

# Imputed
yy<-1/imp.auto.train[complete.cases(imp.auto.train),1] ## training data
xx<-as.matrix(imp.auto.train[complete.cases(imp.auto.train),c(-1,-8)])
yyt<-1/imp.auto.test[complete.cases(imp.auto.test),1] ## training data
xxt<-as.matrix(imp.auto.test[complete.cases(imp.auto.test),c(-1,-8)])

###
rleaps<-regsubsets(xx,yy,int=T,nbest=1,nvmax=dim(Auto.Train)[2],really.big=T,method=c("backward")) ## backward selection
cleaps<-summary(rleaps,matrix=T) ## True/False matrix. The r-th row is a True/False statement about which
## variables are included in model r.
tt<-apply(cleaps$which,1,sum) ## size of each model in the matrix
mses<-cleaps$rss/length(yy) ## corresponding MSEs
###

pmses<-mses
for (ta in (1:dim(cleaps$which)[1])) {
  mmr<-lm(yy~xx[,cleaps$which[ta,-1]==T])
  mses[ta]<-sum((1/yy-1/(cbind(rep(1,dim(xx)[1]),xx[,cleaps$which[ta,-1]==T])%*%mmr$coef))^2)/length(yy)
  PEcp<-sum((1/yyt-1/(cbind(rep(1,dim(xxt)[1]),xxt[,cleaps$which[ta,-1]==T])%*%mmr$coef))^2)/length(yyt)
  #mses[ta]<-sum((yy-(cbind(rep(1,dim(xx)[1]),xx[,cleaps$which[ta,-1]==T])%*%mmr$coef))^2)/length(yy)
  #PEcp<-sum((yyt-(cbind(rep(1,dim(xxt)[1]),xxt[,cleaps$which[ta,-1]==T])%*%mmr$coef))^2)/length(yyt)
  pmses[ta]<-PEcp }
#pmses[1]<-max(pmses)
#mses[1]<-max(mses)

modsel <- data.frame(nvar=tt-1,mse=mses,pmse=pmses)

# prediction errors are larger than RSS on training data
ptmin<-which.min(pmses)
pmod<-cleaps$which[ptmin,]
pmod
### Compare seelcted model based on prediction vs F-test
#################################
### all subset selection
rleaps<-regsubsets(xx,yy,int=T,nbest=5000,nvmax=dim(Auto.Train)[2],really.big=T,method=c("ex")) ## all subset selection
cleaps<-summary(rleaps,matrix=T) ## True/False matrix. The r-th is a True/False statement about which
## variables are included in model r.
tt<-apply(cleaps$which,1,sum) ## size of each model in the matrix
mses<-cleaps$rss/length(yy) ## corresponding MSEs
###
##
tmin<-min(tt)
tmax<-max(tt)
tsec<-seq(tmin,tmax)
# prediction errors
pmses<-mses
for (ta in (1:dim(cleaps$which)[1])) {
  mmr<-lm(yy~xx[,cleaps$which[ta,-1]==T])
  mses[ta]<-sum((1/yy-1/(cbind(rep(1,dim(xx)[1]),xx[,cleaps$which[ta,-1]==T])%*%mmr$coef))^2)/length(yy)
  PEcp<-sum((1/yyt-1/cbind(rep(1,dim(xxt)[1]),xxt[,cleaps$which[ta,-1]==T])%*%mmr$coef)^2)/length(yyt)
  #mses[ta]<-sum((yy-(cbind(rep(1,dim(xx)[1]),xx[,cleaps$which[ta,-1]==T])%*%mmr$coef))^2)/length(yy)
  #PEcp<-sum((yyt-(cbind(rep(1,dim(xxt)[1]),xxt[,cleaps$which[ta,-1]==T])%*%mmr$coef))^2)/length(yyt)
  pmses[ta]<-PEcp }
#pmses[1]<-max(pmses)
#mses[1]<-max(mses)

# Best prediction models are NOT found when doing backward selection!!!
ptmin<-which.min(pmses)
pmod<-cleaps$which[ptmin,]
pmod
#
cleaps$which[sort.list(pmses)[1:10],]

exsel <- data.frame(nvar=tt-1,mse=mses,pmse=pmses)

p <- (ggplot(modsel,aes(x=nvar)) + geom_line(aes(y=mse,colour="mse"),size=1.2) + geom_line(aes(y=pmse,colour="pmse"),size=1.2)
     + labs(x="Number of variables",y="MSE")
     + scale_colour_discrete(breaks=c("mse", "pmse"),labels=c("Training", "Prediction"))
     + geom_point(data=exsel,aes(x=nvar,y=mse,colour="mse"),shape=21,fill="white",size=2.5,stroke=1.2)
     + geom_point(data=exsel,aes(x=nvar,y=pmse,colour="pmse"),shape=21,fill="white",size=2.5,stroke=1.2)
     + ylim(0,30)
     + theme(legend.position = c(1, 1),legend.justification=c(1,1),legend.title=element_blank()))

