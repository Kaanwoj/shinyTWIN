##################
### Simulation ###
##################

simulate.first.modality <- function(label, soa, proc.first, proc.second, mu, sigma, omega,
                              delta, N) {
  nsoa <- length(soa)

  # draw random samples for processing time on stage 1 (both modalities) or 2 (M)
  first <- matrix(rexp(N * nsoa, rate = 1/proc.first), ncol = nsoa)
  second <- matrix(rexp(N * nsoa, rate = 1/proc.second), ncol = nsoa)
  M <- matrix(rnorm(N * nsoa, mean = mu, sd = sigma), ncol = nsoa)

  names <- paste0("SOA.", soa)

  # empty data matrix for RTs
  data <- matrix(nrow = N, ncol = nsoa)
  colnames(data) <- paste0(label, names)

  for (i in 1:N) {
    for(j in 1:nsoa) {
      # is integration happening or not?
      if (max(first[i,j], second[i,j] + soa[j]) < min(second[i,j] + soa[j],
                                                      first[i,j]) + omega) {
        data[i,j] <- first[i,j] + M[i,j] - delta
      } else
        data[i,j] <- first[i,j] + M[i,j]
    }}

  return(data)
}

simulate.rtp <- function(soa, proc.A, proc.V, mu, sigma, omega, delta, N) {

  return(
         cbind(auditory = simulate.first.modality(label = "aud", soa,
                                                   proc.first = proc.A,
                                                   proc.second = proc.V, mu,
                                                   sigma, omega, delta, N),
                visual   = simulate.first.modality(label = "vis", soa,
                                                   proc.first = proc.V,
                                                   proc.second = proc.A, mu,
                                                   sigma, omega, delta, N)
            )
         )
}
