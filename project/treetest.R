B<-10
numvars <- dim(recs.clean)[2]-1
# number of random splits
Nbrleaves<-matrix(0,B,1)
UsedMatrix<-matrix(0,B,numvars)
UsedFirst<-matrix(0,B,numvars)
MSEMatrix<-matrix(0,B,3)
for (bb in (1:B)) {
  print(bb)
  #
  iie<-sample(seq(1,dim(recs.clean)[1]),11500)
  trtree<-recs.clean[-iie,]
  tetree<-recs.clean[iie,]
  tree1<-tree(KWHSPH ~ .,data=trtree,mindev=0.001)
  cvtree1<-cv.tree(tree1,K=10)
  mim<-min(cvtree1$dev)
  sizesel<-max(2,max(cvtree1$size[cvtree1$dev==mim]))
  ptree1<-prune.tree(tree1,best=sizesel)
  Nbrleaves[bb]<-length(unique(ptree1$where))
  predtree<-predict(ptree1,newdata=tetree)
  testy<-tetree[,"KWHSPH"]
  MSEMatrix[bb,1]<-sum((testy-predtree)^2)/length(predtree)
  predtree<-predict(tree1,newdata=tetree)
  MSEMatrix[bb,2]<-sum((testy-predtree)^2)/length(predtree)
  UsedMatrix[bb,as.numeric(summary(ptree1)$used)-1]<-UsedMatrix[bb,as.numeric(summary(ptree1)$used)-1]+1
  UsedFirst[bb,as.numeric(summary(ptree1)$used)[1]-1]<-UsedFirst[bb,as.numeric(summary(ptree1)$used)[1]-1]+1
  
  for (col in names(which(sapply(trtree, is.factor)))) {
    uvals <- unique(trtree[,col])
    todelete <- which(!tetree[,col] %in% uvals)
    if (length(todelete)>0) {
      tetree <- tetree[-todelete,]
    }
  }
  vars <- sapply(trtree,var)
  trtree <- trtree[,which(vars>5e-2)]
  tetree <- tetree[,which(vars>5e-2)]
  testy <- tetree[,"KWHSPH"]
  null <- lm(KWHSPH ~ 1, data=trtree)
  full <- lm(KWHSPH ~ ., data=trtree)
  
  printf("Stepwise\n")
  sm <- step(null, scope=list(lower=null,upper=full),steps=5,trace=0,direction="forward")
  plinmod <- predict(sm, newdata=tetree)
  MSEMatrix[bb,3] <- mean((testy-plinmod)^2)
}

# Calculate percent of cases where used
usedmean <- apply(UsedMatrix,2,mean)
pctused <- data.frame(used=-sort(-usedmean),row.names = names(trdata)[order(-usedmean)])

# Add descriptions
desc <- read.csv("descriptions.csv",sep=";",header=FALSE,row.names = 1)
pctused.desc <-cbind(pctused,desc[rownames(pctused),])
colnames(pctused.desc) <- c("used","desc")
#save(pctused.desc,file="tree-used.RData")

