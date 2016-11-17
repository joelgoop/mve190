setwd('~/git/mve190/lab1/')
library(ggplot2)

load('Lab1data.RData')
load('itest.RData')
Auto.Train<-newAuto[-itest,]
Auto.Test<-newAuto[itest,]

pairs(Auto.Train)

trauto <- Auto.Train[complete.cases(Auto.Train),]
# Transform 1/y
trauto$mpg <- 1/trauto$mpg
# Rename to gpm
names(trauto)[names(trauto)=="mpg"] <- "gpm"

m1 <- lm(log(gpm) ~ ., data=trauto)
par(mfrow=c(2,2))
summary(m1)
plot(m1)

redtrauto <- trauto
# Remove outliers
# 210, (260) from Q-Q plot
# 266 for high leverage
redtrauto <- redtrauto[!rownames(redtrauto) %in% c(210,266),]
m2 <- lm(gpm ~ weight+year+cylinders+horsepower+acceleration+displacement, data=redtrauto)
summary(m2)
plot(m2)

ss<-step(m2,direction='backward')
print(ss$anova)

null<-lm(gpm~1,data=trauto)
ss2<-step(null,direction='forward',scope=list(lower=null,upper=m2))
print(ss2$anova)


m3 <- lm(sqrt(gpm) ~ ., data=redtrauto)
summary(m2)
plot(m2)


regsubsets.out <-
  regsubsets(gpm ~ .,
             data = trauto,
             nbest = 1,       # 1 best model for each number of predictors
             nvmax = NULL,    # NULL for no limit on number of variables
             method = "exhaustive",
             really.big=T)
summary(regsubsets.out,matrix=TRUE)[2]



# naivelm <- lm(1/mpg ~ ., data=no.na)
# naivelm <- lm(1/mpg ~ weight + year, data=no.na)