get.singlelevel <- function(data) {
  l<-sapply(data,function(x)  (is.factor(x) && length(unique(x))==1))
  return(l)
}

load(file="recs.RData")

# Remove imputation flag columns
recs.clean <- recs[,grep("^[^Z].+",names(recs))]
recs.imp <- recs[,grep("^Z.+",names(recs))]

ncol1 <- ncol(recs.clean)

# Remove total KWH
recs.clean <- recs.clean[,1:482][,-481]

ncol2 <- ncol(recs.clean)
print(ncol1-ncol2)

# Remove outliers
#recs.clean <- recs.clean[!rownames(recs.clean) %in% c(2206,1999),]

# Set ID as row name and remove NWEIGHT
recs.clean <- data.frame(recs.clean[,-1], row.names=recs.clean[,1])
recs.clean <- subset(recs.clean, select=-c(NWEIGHT,TOTSQFT_EN,TOTUSQFT,TOTCSQFT,TOTUCSQFT))

# Remove column USEEL (everyone uses electricity), and others that are almost all the same value (small variance)
vars <- sapply(recs.clean,var)
recs.clean <- recs.clean[,which(vars>5e-2)]

recs.cont <- recs.clean

# Separate factor and non-factor columns
nonfactor <- c("HDD65","CDD65","HDD30YR","CDD30YR","YEARMADE","NUMFLRS","NUMAPTS","NAPTFLRS",
               "BEDROOMS","NCOMBATH","NHAFBATH","OTHROOMS","TOTROOMS","FINBASERMS","FINATTRMS",
               "STOVEN","STOVE","OVEN","NUMFRIG","MONRFRI2","MONRFRI3","TVCOLOR","NUMPC","PCPRINT",
               "HEATROOM","NUMTHERM","TEMPHOME","TEMPGONE","TEMPNITE","ACROOMS","TEMPHOMEAC",
               "TEMPGONEAC","TEMPNITEAC","NUMBERAC","NUMCFAN","LGT12","LGT12EE","LGT4","LGT4EE",
               "LGT1","LGT1EE","NOUTLGTNT","LGTOEE","NGASLIGHT","DOOR1SUM","NOCRCASH","NKRGALNC",
               "NUMCORDS","NHSLDMEM","HHAGE","TELLDAYS","TOTSQFT","TOTSQFT_EN","TOTHSQFT",
               "TOTUSQFT","TOTCSQFT","TOTUCSQFT","KWHSPH","YEARMADERANGE","OCCUPYYRANGE",
               "PCTBSTHT","PCTBSTCL","PCTATTHT","PCTATTCL","SIZEOFGARAGE","SIZEOFDETACH",
               "AMTMICRO","AGERFRI1","AGERFRI2","AGERFRI3","SIZFREEZ","AGEFRZR","SIZFREEZ2",
               "AGEFRZR2","AGEDW","WASHLOAD","AGEHHMEMCAT2","AGEHHMEMCAT3","AGEHHMEMCAT4",
               "AGEHHMEMCAT5","AGEHHMEMCAT6","AGEHHMEMCAT7","AGEHHMEMCAT8","AGEHHMEMCAT9",
               "AGEHHMEMCAT10","AGEHHMEMCAT11","AGEHHMEMCAT12","AGEHHMEMCAT13","AGEHHMEMCAT14",
               "MONEYPY")
factor.cols <- !names(recs.clean) %in% nonfactor

recs.clean[factor.cols] <- lapply(recs.clean[factor.cols] , factor)

save(recs.clean,file="recs-clean.RData")

# Split into training, selection and testing
recs.el <- recs.clean[recs.clean$ELWARM==1,]
recs.el <- rbind(recs.el, recs.clean[recs.clean$ELECAUX==1,])
recs.el <- recs.el[recs.el$FUELHEAT==5,]

recs.sets <- split(recs.el, sample(c("train","train","sel","sel","val"), nrow(recs.el), replace=T))
recs.train <- rbind(recs.sets$train,recs.sets$sel)

l1 <- get.singlelevel(recs.train)
l2 <- get.singlelevel(recs.sets$val)

recs.train[l1] <- list(NULL)
recs.sets$val[l1] <- list(NULL)

recs.train[l2] <- list(NULL)
recs.sets$val[l2] <- list(NULL)

# Continuous data
recs.cont.el <- recs.cont[recs.cont$ELWARM==1,]
recs.cont.el <- rbind(recs.cont.el, recs.cont[recs.cont$ELECAUX==1,])
recs.cont.el <- recs.cont.el[recs.cont.el$FUELHEAT==5,]

# Remove columns with very low variance
vars <- sapply(recs.cont.el,var)
recs.cont.el <- recs.cont.el[,vars>1e-5]

recs.cont.sets <- split(recs.cont.el, sample(c("train","train","sel","sel","val"), nrow(recs.cont.el), replace=T))

recs.cont.train <- rbind(recs.cont.sets$train,recs.cont.sets$sel)

iie<-sample(seq(1,dim(recs.clean)[1]),3000)
trdata<-recs.clean[-iie,]
tedata<-recs.clean[iie,]

