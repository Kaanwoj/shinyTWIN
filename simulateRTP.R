##################
### Simulation ###
##################

simulate.rtp <- function(soa, proc.A, proc.V, mu, sigma, omega, delta, N) {
  
  lambdaA <- 1/proc.A
  lambdaV <- 1/proc.V
  
  # draw random samples for processing time on stage 1 (only A) or 2 (M)
  nsoa <- length(soa)
  A <- matrix(rexp(N * nsoa, rate = proc.A), ncol = nsoa)
  V <- matrix(rexp(N * nsoa, rate = proc.V), ncol = nsoa)
  M <- matrix(rnorm(N * nsoa, mean = mu, sd = sigma), ncol = nsoa)
  dimnames_a <- list(tr=c(1:N), name=c("SOA(0ms)_A","SOA(50ms)_A","SOA(100ms)_A","SOA(200ms)_A"))
  dimnames_v <- list(tr=c(1:N), name=c("SOA(0ms)_V","SOA(50ms)_V","SOA(100ms)_V","SOA(200ms)_V"))
  
  
  data.a <- matrix(nrow=N, ncol=nsoa,dimnames = dimnames_a) # data matrix for RTs
  
  for (i in 1:N) {
    for(j in 1:nsoa) {
      # is integration happening or not?
      if (max(A[i,j],V[i,j]+soa[j]) < min(V[i,j]+soa[j], A[i,j]) + omega) { # yes
        data.a[i,j] <- A[i,j] + M[i,j] - delta
      } else  # no
        data.a[i,j] <- A[i,j] + M[i,j]
    }}
  
######## for visual
  
data.v <- matrix(nrow=N, ncol=nsoa, dimnames = dimnames_v) # data matrix for RTs

for (i in 1:N) {
  for(j in 1:nsoa) {
    # is integration happening or not?
    if (max(A[i,j]+soa[j],V[i,j]) < min(V[i,j], A[i,j]+soa[j]) + omega) {  #yes
      data.v[i,j] <- V[i,j] + M[i,j] - delta
    } else  #no
      data.v[i,j] <- V[i,j] + M[i,j]
  }}
data.merge <- cbind(data.a,data.v)
return(data.merge)


}
