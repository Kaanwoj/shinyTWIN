################
### Examples ###
################

source("simulateFAP.R")
source("estimate.R")
source("plotHelpers.R")
source("simulateRTP.R")

### Focused Attention Paradigm
##############################

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

# FAP simulation
data <- simulate.fap(soa, proc.A, proc.V, mu, sigma, omega, delta, N)

# FAP estimation
est <- estimate(data, paradigm = "fap")

# Plot estimates
plotPredObs.fap(data, est)

### Redundant Target Paradigm
#############################

# parameter values
# stimulus onset asynchrony
soa <- c(0, 50, 100, 200)

# processing time of visual/auditory stimulus on stage 1
proc.A <- 150       # = 1/lambdaA
proc.V <- 100

# mean processing time and sd on stage 2
mu <- 200
sigma <- mu/5

# width of the time window of integration
omega <- 200

# size of cross-modal interaction effect
delta <- 50

# number of observations per SOA
N <- 500

# RTP simulation
data <- simulate.rtp(soa, proc.A, proc.V, mu, sigma, omega, delta, N)

# RTP estimation
est <- estimate(data, paradigm = "rtp")

# Plot estimates
plotPredObs.rtp(data, est)

# check if predictions are alright
pred <- predict.rt.rtp(c(proc.A, proc.V, mu, omega, delta), colnames(data))
