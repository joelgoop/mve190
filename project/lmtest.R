printf <- function(...) cat(sprintf(...))

# tedata<-recs.sets$val
runs <- 20

m <- matrix(0, ncol = ncol(recs.clean), nrow = runs)
used <- data.frame(m)
colnames(used) <- names(recs.clean)

lmmse <- matrix(0,runs,1)

for (k in 1:runs){
  printf("Loop %i of %i\n",k,runs)
  
  iie<-sample(seq(1,dim(recs.clean)[1]),3000)
  trdata<-recs.clean[-iie,]
  tedata<-recs.clean[iie,]
  
  printf("Fit base models\n")
  null <- lm(KWHSPH ~ 1, data=trdata)
  full <- lm(KWHSPH ~ ., data=trdata)
  
  printf("Stepwise\n")
  sm <- step(null, scope=list(lower=null,upper=full),steps=15,trace=0,direction="forward")
  
  tnames <- attr(sm$terms,"term.labels")
  used[k,names(trdata) %in% tnames]=1
  
  
  for (col in names(which(sapply(trdata, is.factor)))) {
    uvals <- unique(trdata[,col])
    todelete <- which(!tedata[,col] %in% uvals)
    if (length(todelete)>0) {
      tedata <- tedata[-todelete,]
    }
  }
  
  predsm <- predict(sm,newdata=tedata)
  testy<-tedata[,"KWHSPH"]
  lmmse[k,1] <- sum((testy-predsm)^2)/length(predsm)

}

pctused <- data.frame(used=-sort(-sapply(used,mean)))

# Add descriptions
desc <- read.csv("descriptions.csv",sep=";",header=FALSE,row.names = 1)
pctused.desc.lm <-cbind(pctused,desc[rownames(pctused),])
colnames(pctused.desc.lm) <- c("used","desc")

save(lmmse,pctused.desc.lm,file="lm-used.RData")
#predlm<-predict(full,newdata=tedata)



#mselm <- sum((testy-predlm)^2)/length(predlm)
#msesm <- sum((testy-predsm)^2)/length(predsm)
#print(mselm)
#print(msesm)

#fixes <- names(recs.clean)
#fixes <- fixes[fixes!="KWHSPH"]

#for (i in 1:length(fixes)) {
#  id <- which(!(tedata[,fixes[i]] %in% unique(recs.sets$train[,fixes[i]])))
#  tedata[id,fixes[i]] <- NA
#}