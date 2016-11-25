do.runs <- function(ns=c(50,100,300,1000,10000),p=6,sd=1,nloops=20) {
  
  cumul.r2 <- 0
  
  coeffs <- gen.coeffs(p)
  truemod <- 1*(abs(coeffs[-1])>0)
  
  output <- matrix(,nrow=0,ncol=7)
  count <- 0
  totloops <- length(ns)*nloops
  
  for (n in ns) {
    
    for (i in 1:nloops) {
      count <- count+1
      printf("Loop %i of %i\n",count,totloops)
      
      data <- gen.data(n,p,coeffs,sd)
      m <- lm(data[,p+1]~data[,-(p+1)])
      #print(summary(m))
      r2 <- summary(m)$r.squared
      cumul.r2 <- cumul.r2 + r2
      
      ti <- sample(seq(dim(data)[1]),floor(dim(data)[1]/2))
      
      
      
      res <- SelAndPred(data,p+1,K=10,trainindex=ti,po=F)
      nvars <- apply(res$AA[,-1],1,sum)
      nwrong <- apply(abs(t(res$AA[,2:(p+1)])-truemod),2,sum)
      pes <- res$AA[,1]
      
      output <- rbind(output,c(n/2,p,r2,1,nwrong[1],nvars[1],pes[1]))
      output <- rbind(output,c(n/2,p,r2,2,nwrong[2],nvars[2],pes[2]))
      output <- rbind(output,c(n/2,p,r2,3,nwrong[3],nvars[3],pes[3]))
      output <- rbind(output,c(n/2,p,r2,4,nwrong[4],nvars[4],pes[4]))
      
    }
  }
  
  results <- data.frame(output)
  colnames(results) <- c("n","p","r2","sel","err","size","pe")
  results[,4] <- sapply(results[,4], as.factor)
  levels(results$sel) <- c("cv","cp","aic","bic")
  
  return(results)
}
