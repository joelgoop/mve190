dataset <- Auto.Train

# Remove all-na rows
ind <- apply(dataset, 1, function(x) all(is.na(x)))
dataset <- dataset[!ind,]

mw <- lm(weight ~ displacement + acceleration + horsepower, data=dataset[!rownames(dataset)=="266",])
my <- lm(year ~ weight + horsepower, data=dataset)
md <- lm(displacement ~ cylinders + horsepower + weight, data=dataset[!rownames(dataset)=="266",])
ma <- lm(acceleration ~ horsepower + weight, data=dataset[!rownames(dataset)=="266",])
mh <- lm(horsepower ~ weight + acceleration, data=dataset[!rownames(dataset)=="266",])
#mc <- lm(cylinders ~ displacement, data=imputed)

print(sapply(dataset,function(x) sum(is.na(x))))

impute <- function(data) {
  ind <- apply(data, 1, function(x) all(is.na(x)))
  imputed <- data[!ind,]
  
  missWeight<-seq(1,dim(imputed)[1])[is.na(imputed$weight)]
  missYear<-seq(1,dim(imputed)[1])[is.na(imputed$year)]
  missDispl<-seq(1,dim(imputed)[1])[is.na(imputed$displacement)]
  missAcc<-seq(1,dim(imputed)[1])[is.na(imputed$acceleration)]
  missCyl<-seq(1,dim(imputed)[1])[is.na(imputed$cylinders)] 
  missHP<-seq(1,dim(imputed)[1])[is.na(imputed$horsepower)]
  
  imputed$weight[missWeight] <- predict(mw, newdata = imputed[missWeight,])
  imputed$year[missYear] <- predict(my, newdata = imputed[missYear,])
  imputed$displacement[missDispl] <- predict(md, newdata = imputed[missDispl,])
  imputed$acceleration[missAcc] <- predict(ma, newdata = imputed[missAcc,])
  #imputed$cylinders[missCyl] <- round(predict(mc, newdata = imputed[missCyl,]),0)
  imputed$horsepower[missHP] <- predict(mh, newdata = imputed[missHP,])
  
  return(imputed)
}

imp.auto.train <- impute(Auto.Train)
sapply(imp.auto.train,tabled.summary)

imp.auto.test <- impute(Auto.Test)
sapply(imp.auto.test,tabled.summary)

# Add gpm
imp.auto.train$gpm <- 1/imp.auto.train$mpg
imp.auto.test$gpm <- 1/imp.auto.test$mpg
noimp.auto.train <- Auto.Train
noimp.auto.test <- Auto.Test
noimp.auto.train$gpm <- 1/noimp.auto.train$mpg
noimp.auto.test$gpm <- 1/noimp.auto.test$mpg

###################
# Test regression
###################
m.imp <- lm(gpm ~ weight+year, data=imp.auto.train)
m.imp.outl <- lm(gpm ~ weight+year, data=imp.auto.train[!rownames(imp.auto.train) %in% c("210"),])
m.noimp <- lm(gpm ~ weight+year, data=noimp.auto.train[complete.cases(noimp.auto.train),])

p.imp <- predict(m.imp,newdata=imp.auto.test[complete.cases(imp.auto.test),],se.fit=T,interval="predict")
p.noimp <- predict(m.noimp,newdata=noimp.auto.test[complete.cases(noimp.auto.test),],se.fit=T,interval="predict")

mm <- lm(gpm ~ .-mpg, data=imp.auto.train)

mm2 <- lm(gpm ~ .-mpg-displacement, data=imp.auto.train)
anova(mm,mm2)

mm3 <- lm(gpm ~ .-mpg-displacement-acceleration, data=imp.auto.train)
anova(mm2,mm3)

mm4 <- lm(gpm ~ .-mpg-displacement-acceleration-cylinders, data=imp.auto.train)
anova(mm3,mm4)

mm5 <- lm(gpm ~ .-mpg-displacement-acceleration-cylinders-horsepower, data=imp.auto.train)
anova(mm4,mm5)

mm6 <- lm(gpm ~ .-mpg-displacement-acceleration-cylinders-horsepower-year, data=imp.auto.train)
anova(mm5,mm6)

summary(mm2)


pred <- data.frame(observed=imp.auto.test[complete.cases(imp.auto.test),1],predicted=1/p.imp$fit[,1],up=1/p.imp$fit[,2],lo=1/p.imp$fit[,3])
       

