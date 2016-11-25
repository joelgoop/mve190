nruns <- do.runs(p=8,ns=c(50,70,100,150,300,1000,5000),nloops=100)
save(nruns,file='nruns_1.RData')

sd15 <- do.runs(p=8,ns=c(50,70,100,150,300,1000),nloops=50,sd=1.5)
save(sd15,file='sd15_1.RData')

sd20 <- do.runs(p=8,ns=c(50,70,100,150,300,1000),nloops=50,sd=2)
save(sd20,file='sd20_1.RData')
