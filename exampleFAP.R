##################
### Test cases ###
##################

source("simulateFAP.R")
source("estimate.R")
source("plotHelpers.R")

### FAP simulation
# parameter values
# stimulus onset asynchrony
soa <- c(-200, -100, -50, 0, 50, 100, 200)

# processing time of visual/auditory stimulus on stage 1
proc.A <- 100       # = 1/lambdaA
proc.V <- 50

# mean processing time and sd on stage 2
mu <- 150
sigma <- 25

# width of the time window of integration
omega <- 200

# size of cross-modal interaction effect
delta <- 50

# number of observations per SOA
N <- 500

data <- simulate.fap(soa, proc.A, proc.V, mu, sigma, omega, delta, N)

## FAP estimation
est <- estimate(data, paradigm = "fap")

plotPredObs.fap(data, est)
