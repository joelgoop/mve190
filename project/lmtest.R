tedata<-recs.sets$val

for (col in names(which(sapply(recs.train, is.factor)))) {
  print(col)
  uvals <- unique(recs.train[,col])
  todelete <- which(!tedata[,col] %in% uvals)
  if (length(todelete)>0) {
    print(todelete)
    tedata <- tedata[-todelete,]
  }
}

#fixes <- names(recs.clean)
#fixes <- fixes[fixes!="KWHSPH"]

#for (i in 1:length(fixes)) {
#  id <- which(!(tedata[,fixes[i]] %in% unique(recs.sets$train[,fixes[i]])))
#  tedata[id,fixes[i]] <- NA
#}

predlm<-predict(full,newdata=tedata)
predsm <- predict(sm,newdata=tedata)
testy<-tedata[,"KWHSPH"]

mselm <- sum((testy-predlm)^2)/length(predlm)
msesm <- sum((testy-predsm)^2)/length(predsm)
print(mselm)
print(msesm)
