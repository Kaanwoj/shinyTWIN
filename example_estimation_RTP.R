source("estimateRTP")
dat.a <- read.table("simDataRTPa.txt", sep=";", header=TRUE)
dat.v <- read.table("simDataRTPv.txt", sep=";", header=TRUE)
estimateRTP(dat.merge)