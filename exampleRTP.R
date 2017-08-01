###################
### Example RTP ###
###################

source("simulateRTP.R")
source("estimate.R")
source("plotHelpers.R")

### RTP simulation
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

data <- simulate.rtp(soa, proc.A, proc.V, mu, sigma, omega, delta, N)

### RTP estimation
est <- estimate(data, paradigm = "rtp")

plotEstPred.rtp(data, est)

# check if predictions are alright
pred <- predict.rt.rtp(c(proc.A, proc.V, mu, omega, delta), colnames(data))
