

iie<-sample(seq(1,dim(recs.clean)[1]),3000)
trdata<-recs.clean[-iie,]
tedata<-recs.clean[iie,]

null <- lm(KWHSPH ~ 1, data=trdata)
full <- lm(KWHSPH ~ ., data=trdata)

sm <- step(null, scope=list(lower=null,upper=full),steps=20,trace=1,direction="forward")
bsm <- step(full,steps=15,trace=1,direction="backward")


tm <- rpart(KWHSPH ~ ., data=trdata, cp=0.0001,xval=10)
cps <- data.frame(tm$cptable)

mincp <- tm$cptable[which.min(tm$cptable[,"xerror"]),"CP"]
ptm <- prune(tm,cp=mincp)

tedata <- recs.sets$val

predy <- predict(ptm,newdata=tedata)
print(sum((predy-tedata$KWHSPH)^2)/length(predy))

testy <- tedata$KWHSPH
save(testy,predy,file="ptm.RData")

ttm <- tree(KWHSPH ~ ., data=recs.clean)

bsm <- step(full, scope=list(lower=null,upper=full),steps=3,trace=1,direction="backward")

save(tm,sm,ptm,file="models.RData")
