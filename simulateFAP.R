##################
### Simulation ###
##################



simulate.fap <- function(soa, lambdaA, lambdaV, mu, sigma, omega, delta, N) {

  # draw random samples for processing time on stage 1 (A and V) or 2 (M)
  A <- matrix(rexp(N * nsoa, rate = lambdaA), ncol = nsoa)
  V <- matrix(rexp(N * nsoa, rate = lambdaV), ncol = nsoa)
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
  }}
  return(data)
}
