##################
### Test cases ###
##################

### FAP simulation
source("simulateFAP.R")

# parameter values
# stimulus onset asynchrony
soa <- c(-200, -100, -50, 0, 50, 100, 200)
nsoa <- length(soa)

# processing time of visual/auditory stimulus on stage 1
lambdaA <- 100       # = 1/lambdaA
lambdaV <- 50

# mean processing time and sd on stage 2
mu <- 150
sigma <- 25

# width of the time window of integration
omega <- 200

# size of cross-modal interaction effect
delta <- 50

# number of observations per SOA
N <- 40

data <- simulate.fap(soa, lambdaA, lambdaV, mu, sigma, omega, delta, N)

# save data in a file
df <- as.data.frame(data)
names(df) <- paste0("soa", soa)

write.table(df, "simDataFAP.txt", sep=";", row.names=FALSE)
