##################
### Test cases ###
##################

### RTP simulation
source("simulateRTP.R")

# parameter values
# stimulus onset asynchrony
soa <- c(0, 50, 100, 200)
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

data <- simulate.rtp(soa, lambdaA, lambdaV, mu, sigma, omega, delta, N)

# save auditory data in a file
df.a <- as.data.frame(data.a)
names(df.a) <- paste0("soa", soa)

write.table(df.a, "simDataRTPa.txt", sep=";", row.names=FALSE)

## save visual data in file
df.v <- as.data.frame(data.v)
names(df.v) <- paste0("soa", soa)

write.table(df.v, "simDataRTPv.txt", sep=";", row.names=FALSE)

