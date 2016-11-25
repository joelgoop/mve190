SelAndPred<-function(dataset,yindex,K=10,nb=1000,trainindex,po=F) {
# Runs CV selection and BIC,AIC and Cp
# Applies selected models for prediction on testset
trdata<-dataset[trainindex,]
testdata<-dataset[-trainindex,]
#
cvk<-CVK(trdata[,yindex],trdata[,-yindex],K,nb,plotout=po)
mm<-ModelSelection(trdata[,yindex],trdata[,-yindex],nb,plotout=po)

#
cvmod<-cvk$winmod[2:length(cvk$winmod)]
if (sum(cvmod)>=1) {
   xx<-trdata[,-yindex]
   ll<-lm(trdata[,yindex]~as.matrix(xx[,cvmod==T]))
   xtest<-testdata[,-yindex]
   xtest<-as.matrix(xtest[,cvmod==T])
   pred.cv<-ll$coef[1]+xtest%*%ll$coef[-1] }
if (sum(cvmod)==0) {
   pred.cv<-rep(mean(trdata[,yindex]),dim(testdata)[1])  }
#
aicmod<-mm$modtab[2,2:length(cvk$winmod)]
if (sum(aicmod)>=1) {
  xx<-trdata[,-yindex]
  ll<-lm(trdata[,yindex]~as.matrix(xx[,aicmod==T]))
  xtest<-testdata[,-yindex]
  xtest<-as.matrix(xtest[,aicmod==T])
  pred.aic<-ll$coef[1]+xtest%*%ll$coef[-1] }
if (sum(aicmod)==0) {
  pred.aic<-rep(mean(trdata[,yindex]),dim(testdata)[1])  }
#
bicmod<-mm$modtab[3,2:length(cvk$winmod)]
if (sum(bicmod)>=1) {
  xx<-trdata[,-yindex]
  ll<-lm(trdata[,yindex]~as.matrix(xx[,bicmod==T]))
  xtest<-testdata[,-yindex]
  xtest<-as.matrix(xtest[,bicmod==T])
  pred.bic<-ll$coef[1]+xtest%*%ll$coef[-1] }
if (sum(bicmod)==0) {
  pred.bic<-rep(mean(trdata[,yindex]),dim(testdata)[1])  }
#
cpmod<-mm$modtab[1,2:length(cvk$winmod)]
if (sum(cpmod)>=1) {
  xx<-trdata[,-yindex]
  ll<-lm(trdata[,yindex]~as.matrix(xx[,cpmod==T]))
  xtest<-testdata[,-yindex]
  xtest<-as.matrix(xtest[,cpmod==T])
  pred.cp<-ll$coef[1]+xtest%*%ll$coef[-1] }
if (sum(cpmod)==0) {
  pred.cp<-rep(mean(trdata[,yindex]),dim(testdata)[1])  }
#####
PEcv<-sum((testdata[,yindex]-pred.cv)^2)/length(testdata[,yindex])
PEcp<-sum((testdata[,yindex]-pred.cp)^2)/length(testdata[,yindex])
PEaic<-sum((testdata[,yindex]-pred.aic)^2)/length(testdata[,yindex])
PEbic<-sum((testdata[,yindex]-pred.bic)^2)/length(testdata[,yindex])
####
AA<-rbind(cvmod,cpmod,aicmod,bicmod)
AA<-cbind(round(c(PEcv,PEcp,PEaic,PEbic),3),AA)
if (po==T) {
#print(AA) 
}
#
return(list(AA=AA))
}