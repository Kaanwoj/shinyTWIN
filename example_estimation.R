source("estimateFAP.R")
dat <- read.table("simDataFAP.txt", sep=";", header=TRUE)
est <- estimate.fap(dat)
