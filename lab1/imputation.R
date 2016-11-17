# Impute weight
imp.auto.train <- Auto.Train

missWeight<-seq(1,dim(Auto.Train)[1])[is.na(Auto.Train$weight)]
missYear<-seq(1,dim(Auto.Train)[1])[is.na(Auto.Train$year)]
missDispl<-seq(1,dim(Auto.Train)[1])[is.na(Auto.Train$displacement)]
missAcc<-seq(1,dim(Auto.Train)[1])[is.na(Auto.Train$acceleration)]
missCyl<-seq(1,dim(Auto.Train)[1])[is.na(Auto.Train$cylinders)] 
missHP<-seq(1,dim(Auto.Train)[1])[is.na(Auto.Train$horsepower)]

# Impute weight
mw <- lm(weight ~ displacement + acceleration + horsepower, data=Auto.Train)
imp.auto.train$weight[missWeight] <- predict(mw, newdata = Auto.Train[missWeight,])

# Impute year
my <- lm(year ~ weight+horsepower, data=Auto.Train)
imp.auto.train$year[missYear] <- predict(my, newdata = Auto.Train[missYear,])

# Impute displacement

# Add gpm
imp.auto.train$gpm <- 1/imp.auto.train$mpg
mm <- lm(gpm ~ .-mpg, data=imp.auto.train)

mm2 <- lm(gpm ~ .-mpg-displacement, data=imp.auto.train)
anova(mm,mm2)

mm3 <- lm(gpm ~ .-mpg-displacement-acceleration, data=imp.auto.train)
anova(mm2,mm3)


summary(mm2)
