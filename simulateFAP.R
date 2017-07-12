##################
### Simulation ###
##################

simulate.fap <- function(soa, proc.A, proc.V, mu, sigma, omega, delta, N) {

  # draw random samples for processing time on stage 1 (A and V) or 2 (M)
  nsoa <- length(soa)
  A <- matrix(rexp(N * nsoa, rate = 1/proc.A), ncol = nsoa)
  V <- matrix(rexp(N * nsoa, rate = 1/proc.V), ncol = nsoa)
  M <- matrix(rnorm(N * nsoa, mean = mu, sd = sigma), ncol = nsoa)
  names <- sub("-", "neg", paste0("SOA.", soa))
  dimnames <- list(c(1:N), names)

  data <- matrix(nrow=N, ncol=nsoa,  dimnames= dimnames)

  for (i in 1:N) {
    for(j in 1:nsoa) {
      # is integration happening or not?
      # integration occurs only if (a) the auditory stimulus wins the race in
        # the first stage opening the time window of integration, such that (b)
        # the termination of the visual peripheral process falls into the
        # window (Kandil, Diederich, & Colonius, 2014)
        # I = { A + tau < V < A + tau + omega }
      if (((soa[j] + A[i,j]) < V[i,j]) &&
        (V[i,j] < (soa[j] + A[i,j] + omega))) {
          data[i,j] <- V[i,j] + M[i,j] - delta
      } else
          data[i,j] <- V[i,j] + M[i,j]
  }}

  return(data)
}
