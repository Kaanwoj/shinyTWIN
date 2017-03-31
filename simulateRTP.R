##################
### Simulation ###
##################

simulate.rtp <- function(soa, proc.A, proc.V, mu, sigma, omega, delta, N) {
  
  lambdaA <- 1/proc.A
  lambdaV <- 1/proc.V
  
  # draw random samples for processing time on stage 1 (only A) or 2 (M)
  A <- matrix(rexp(N * nsoa, rate = proc.A), ncol = nsoa)
  V <- matrix(rexp(N * nsoa, rate = proc.V), ncol = nsoa)
  M <- matrix(rnorm(N * nsoa, mean = mu, sd = sigma), ncol = nsoa)
  
  data.a <- matrix(nrow=N, ncol=nsoa) # data matrix for RTs
  
  for (i in 1:N) {
    for(j in 1:nsoa) {
      # is integration happening or not?
      if (max(A[i,j],V[i,j]+soa[j]) < min(V[i,j]+soa[j], A[i,j]) + omega) { # yes
        data.a[i,j] <- A[i,j] + M[i,j] - delta
      } else  # no
        data.a[i,j] <- A[i,j] + M[i,j]
    }}
  return(data.a)
  
######## for visual
  
data.v <- matrix(nrow=N, ncol=nsoa) # data matrix for RTs

for (i in 1:N) {
  for(j in 1:nsoa) {
    # is integration happening or not?
    if (max(A[i,j]+soa[j],V[i,j]) < min(V[i,j], A[i,j]+soa[j]) + omega) {  #yes
      data.v[i,j] <- V[i,j] + M[i,j] - delta
    } else  #no
      data.v[i,j] <- V[i,j] + M[i,j]
  }}
return(data.v)
}
