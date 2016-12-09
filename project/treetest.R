B<-10
numvars <- dim(recs.clean)[2]-1
# number of random splits
Nbrleaves<-matrix(0,B,1)
UsedMatrix<-matrix(0,B,numvars)
UsedFirst<-matrix(0,B,numvars)
MSEMatrix<-matrix(0,B,2)
for (bb in (1:B)) {
  print(bb)
  #
  iie<-sample(seq(1,dim(recs.clean)[1]),3000)
  trdata<-recs.train
  tedata<-recs.sets$val
  tree1<-tree(KWHSPH ~ .,data=trdata)
  cvtree1<-cv.tree(tree1,K=10)
  mim<-min(cvtree1$dev)
  sizesel<-max(2,max(cvtree1$size[cvtree1$dev==mim]))
  ptree1<-prune.tree(tree1,best=sizesel)
  Nbrleaves[bb]<-length(unique(ptree1$where))
  predtree<-predict(ptree1,newdata=tedata)
  testy<-tedata[,"KWHSPH"]
  MSEMatrix[bb,1]<-sum((testy-predtree)^2)/length(predtree)
  predtree<-predict(tree1,newdata=tedata)
  MSEMatrix[bb,2]<-sum((testy-predtree)^2)/length(predtree)
  UsedMatrix[bb,as.numeric(summary(ptree1)$used)-1]<-UsedMatrix[bb,as.numeric(summary(ptree1)$used)-1]+1
  UsedFirst[bb,as.numeric(summary(ptree1)$used)[1]-1]<-UsedFirst[bb,as.numeric(summary(ptree1)$used)[1]-1]+1
}