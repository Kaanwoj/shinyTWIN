source("estimateFAP")
dat <- read.table("simDataFAP.txt", sep=";", header=TRUE)
estimateFAP(dat)
