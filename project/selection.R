null <- lm(KWHSPH ~ 1, data=recs.train)
full <- lm(KWHSPH ~ ., data=recs.train)

sm <- step(null, scope=list(lower=null,upper=full),steps=10,trace=1,direction="forward")
tm <- rpart(KWHSPH ~ ., data=recs.train, cp=0.00001,xval=10)
cps <- data.frame(tm$cptable)

mincp <- tm$cptable[which.min(tm$cptable[,"xerror"]),"CP"]
ptm <- prune(tm,cp=mincp)

tedata <- recs.sets$val

predy <- predict(ptm,newdata=tedata)
print(sum((predy-tedata$KWHSPH)^2)/length(predy))

ttm <- tree(KWHSPH ~ ., data=recs.clean)

bsm <- step(full, scope=list(lower=null,upper=full),steps=3,trace=1,direction="backward")
