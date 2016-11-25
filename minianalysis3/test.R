
#n <- 40
p <- 6
sd <- 1

nloops <- 20
cumul.r2 <- 0

coeffs <- gen.coeffs(p)
truemod <- 1*(abs(coeffs[-1])>0)

output <- matrix(,nrow=0,ncol=6)

for (n in c(50,100,300,1000,10000)) {}
  
  for (i in 1:nloops) {
    printf("Loop %i\n",i)
    
    data <- gen.data(n,p,coeffs,sd)
    m <- lm(data[,p+1]~data[,-(p+1)])
    #print(summary(m))
    r2 <- summary(m)$r.squared
    cumul.r2 <- cumul.r2 + r2
    
    ti <- sample(seq(dim(data)[1]),floor(dim(data)[1]/4))
    
    
    
    res <- SelAndPred(data,p+1,K=10,trainindex=ti,po=F)
    nvars <- apply(res$AA[,-1],1,sum)
    nwrong <- apply(t(t(res$AA[,1:p+1])-truemod),1,sum)
    
    output <- rbind(output,c(n,p,r2,1,nwrong[1],nvars[1]))
    output <- rbind(output,c(n,p,r2,2,nwrong[2],nvars[2]))
    output <- rbind(output,c(n,p,r2,3,nwrong[3],nvars[3]))
    output <- rbind(output,c(n,p,r2,4,nwrong[4],nvars[4]))
    
  }
}

results <- data.frame(output)
colnames(results) <- c("n","p","r2","sel","err","size")
results[,4] <- sapply(results[,4], as.factor)
levels(results$sel) <- c("cv","cp","aic","bic")

retu

p <- (ggplot(results, aes(x=n,y=err,colour=sel))
      + geom_point(shape=21,fill="white",size=4,stroke=1.3))
#print(nvars)
#print(nwrong)
#print(sum(coeffs>0))
#print(truemod)

#printf("Average R^2 is %f, average nbr of errors %f\n",cumul.r2/nloops,mean(abs(nwrong)))

#print(apply(nvars,2,mean))

#print(apply(nwrong,2,sum))
