gen.coeffs <- function(p) {
  # Generate a model randomly
  # coeffs <- runif(p+1,min = -1,max = 1)
  coeffs <- rep(1,p+1)
  
  coeffs[sample(seq(2,p+1),floor(p/2))] <- 0 
  return(coeffs)
}

gen.model <- function(data,coeffs,sd=1) {
  # Add intercept to model
  ddim <- dim(data)
  data <- cbind(rep(1,ddim[1]),data)
  
  # Re-calculate dimensions
  ddim <- dim(data)

  
  return(data %*% coeffs + rnorm(ddim[1],mean=0,sd=sd))
}

gen.data <- function(n,p,coeffs,noise) {
  data <- matrix(, nrow=n, ncol=p)
  
  # Generate xs
  for (i in 1:p) {
    data[,i] <- rnorm(n)
  }
  
  ncorr <- floor(p/2)
  is <- sample(seq(p),ncorr)
  
  
  corrwith <- sample(seq(p),ncorr)
  
  for (k in 1:ncorr) {
    i <- sample(seq(p)[-k],1)
    j <- sample(seq(p)[c(-i,-k)],1)
    data[,i] <- data[,j]+rnorm(length(data[,j]),sd=0.1*sd(data[,j]))
  }
  
  # Use model to generate ys
  data <- cbind(data,gen.model(data,coeffs,sd=noise))
  
  return(data)
}

gen <- function(n,p,noise) {
  coeffs <- gen.coeffs(p)
  data <- gen.data(n,p,coeffs,noise)
  return(list("coeffs"=coeffs,"data"=data))
}

do.fit <- function(n,p,coeffs,noise) {
  data <- gen.data(n,p,coeffs,noise)
  m <- lm(data[,ncol(data)] ~ data[,-ncol(data)])
  return(m)
}

avg.r2 <- function(loops,data) {
  rs <- rep(0,loops)
  for (i in 1:loops) {
    m <- lm(data[,ncol(data)] ~ data[,-ncol(data)])
    sm <- summary(m)
    rs[i] <- sm$r.squared
  }
  return(mean(rs))
}

printf <- function(...) cat(sprintf(...))
