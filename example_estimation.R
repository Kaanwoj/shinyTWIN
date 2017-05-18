source("estimateFAP.R")
source("plotHelpers.R")

data <- read.table("simDataFAP.txt", sep=";", header=TRUE)

est <- estimate.fap(data)

plotEstPred(data, est)
