get.singlelevel <- function(data) {
  l<-sapply(data,function(x)  (is.factor(x) && length(unique(x))==1))
  return(l)
}

load(file="recs.RData")

# Remove imputation flag columns (starting with Z)
recs.clean <- recs[,grep("^[^Z].+",names(recs))]
recs.imp <- recs[,grep("^Z.+",names(recs))]

# Remove total KWH
recs.clean <- recs.clean[,1:482]

# Remove outliers
#recs.clean <- recs.clean[!rownames(recs.clean) %in% c(2206,1999),]

# Set ID as row name and remove NWEIGHT and other columns
recs.clean <- data.frame(recs.clean[,-1], row.names=recs.clean[,1])
recs.clean <- subset(recs.clean, 
                     select=-c(NWEIGHT,KWH,TOTSQFT_EN,TOTUSQFT,TOTCSQFT,
                               TOTUCSQFT))

# Remove columns that have very small variance
vars <- sapply(recs.clean,var)
recs.clean <- recs.clean[,which(vars>5e-2)]

# Separate factor and non-factor columns
nonfactor <- c("HDD65","CDD65","HDD30YR","CDD30YR","YEARMADE","NUMFLRS",
               "NUMAPTS","NAPTFLRS","BEDROOMS","NCOMBATH","NHAFBATH","OTHROOMS",
               "TOTROOMS","FINBASERMS","FINATTRMS","STOVEN","STOVE","OVEN",
               "NUMFRIG","MONRFRI2","MONRFRI3","TVCOLOR","NUMPC","PCPRINT",
               "HEATROOM","NUMTHERM","TEMPHOME","TEMPGONE","TEMPNITE","ACROOMS",
               "TEMPHOMEAC","TEMPGONEAC","TEMPNITEAC","NUMBERAC","NUMCFAN",
               "LGT12","LGT12EE","LGT4","LGT4EE","LGT1","LGT1EE","NOUTLGTNT",
               "LGTOEE","NGASLIGHT","DOOR1SUM","NOCRCASH","NKRGALNC","NUMCORDS",
               "NHSLDMEM","HHAGE","TELLDAYS","TOTSQFT","TOTSQFT_EN","TOTHSQFT",
               "TOTUSQFT","TOTCSQFT","TOTUCSQFT","KWHSPH","YEARMADERANGE",
               "OCCUPYYRANGE","PCTBSTHT","PCTBSTCL","PCTATTHT","PCTATTCL",
               "SIZEOFGARAGE","SIZEOFDETACH","AMTMICRO","AGERFRI1","AGERFRI2",
               "AGERFRI3","SIZFREEZ","AGEFRZR","SIZFREEZ2","AGEFRZR2","AGEDW",
               "WASHLOAD","AGEHHMEMCAT2","AGEHHMEMCAT3","AGEHHMEMCAT4",
               "AGEHHMEMCAT5","AGEHHMEMCAT6","AGEHHMEMCAT7","AGEHHMEMCAT8",
               "AGEHHMEMCAT9","AGEHHMEMCAT10","AGEHHMEMCAT11","AGEHHMEMCAT12",
               "AGEHHMEMCAT13","AGEHHMEMCAT14","MONEYPY")
factor.cols <- !names(recs.clean) %in% nonfactor

recs.clean[factor.cols] <- lapply(recs.clean[factor.cols] , factor)

# Select households that use electricity for heating
recs.el <- recs.clean[recs.clean$ELWARM==1,]
recs.el <- subset(recs.el, select=-c(ELWARM))

# Remove low-variance cols again
vars <- sapply(recs.el,var)
recs.el <- recs.el[,which(vars>5e-2)]

