##################
### Simulation ###
##################

simulate.fap <- function(soa, lambdaA, lambdaV, mu, sigma, om, del, N) {

  # draw random samples for processing time on stage 1 (A and V) or 2 (M)
  nsoa <- length(soa)
  A <- matrix(rexp(N * nsoa, rate = 1 / lambdaA), ncol = nsoa)
  V <- matrix(rexp(N * nsoa, rate = 1 / lambdaV), ncol = nsoa)
  M <- matrix(rnorm(N * nsoa, mean = mu, sd = sigma), ncol = nsoa)
  
  dimnames <- list(tr=c(1:N), name=c("SOA(-200ms)", "SOA(-100ms)","SOA(-50ms)","SOA(0ms)","SOA(50ms)","SOA(100ms)","SOA(-200ms)"))
  data <- matrix(0,nrow=N, ncol=7,  dimnames= dimnames)
 

  for (i in 1:N) {
    for(j in 1:nsoa) {
      # is integration happening or not?
      if (((soa[j] + A[i,j]) < V[i,j]) &&
         (V[i,j] < (soa[j] + A[i,j] + om))) {
           data[i,j] <- V[i,j] + M[i,j] - del
      } else
          data[i,j] <- V[i,j] + M[i,j]
  }}
  return(data)
  
}
