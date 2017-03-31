##################
### Simulation ###
##################

simulate.fap <- function(soa, lambdaA, lambdaV, mu, sigma, omega, delta, N) {

  # draw random samples for processing time on stage 1 (A and V) or 2 (M)
  A <- matrix(rexp(N * nsoa, rate = 1 / lambdaA), ncol = nsoa)
  V <- matrix(rexp(N * nsoa, rate = 1 / lambdaV), ncol = nsoa)
  M <- matrix(rnorm(N * nsoa, mean = mu, sd = sigma), ncol = nsoa)

  data <- matrix(nrow=40, ncol=7)

  for (i in 1:N) {
    for(j in 1:nsoa) {
      # is integration happening or not?
      if (((soa[j] + A[i,j]) < V[i,j]) &&
         (V[i,j] < (soa[j] + A[i,j] + omega))) {
           data[i,j] <- V[i,j] + M[i,j] - delta
      } else
          data[i,j] <- V[i,j] + M[i,j]

  return(data)
}}
}

#################
### Test case ###
#################

# parameter values
# stimulus onset asynchrony
# soa <- c(-200, -100, -50, 0, 50, 100, 200)
# nsoa <- length(soa)

# processing time of visual/auditory stimulus on stage 1
# lambdaA <- 100       # = 1/lambdaA
# lambdaV <- 50

# mean processing time and sd on stage 2
# mu <- 150
# sigma <- 25

# width of the time window of integration
# omega <- 200

# size of cross-modal interaction effect
# delta <- 50

# number of observations per SOA
# N <- 40

# data <- simulate.fap(soa, lambdaA, lambdaV, mu, sigma, omega, delta, N)
