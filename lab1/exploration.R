setwd('~/git/mve190/lab1/')
library(ggplot2)

load('Lab1data.RData')
load('itest.RData')
Auto.Train<-newAuto[-itest,]
Auto.Test<-newAuto[itest,]

pairs(Auto.Train)

# naivelm <- lm(1/mpg ~ ., data=no.na)
# naivelm <- lm(1/mpg ~ weight + year, data=no.na)