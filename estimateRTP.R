##################
### Estimation ###
##################

# Probability of integration
soa <- c(0, 50, 100, 200)
dat.merge <- cbind(dat.a,dat.v)

# observed RTs
obs.m <- colSums(dat.merge) / N
obs.se <- apply(dat.merge, 2, sd) / sqrt(N)

# predicted RTs

# formula (2) in Kandil et al. (2014)
predict.rt <- function(soa, param) {

    lambdaA <- 1/param[1]
    lambdaV <- 1/param[2]
    mu      <- param[3]
    sigma   <- param[4]
    omega   <- param[5]
    delta   <- param[6]

    predv <- numeric(length(soa))
    preda <- numeric(length(soa))

  # Probability for Integration when the VISUAL stimulus is presented first
    
    for (i in seq_along(soa)) {

      tau <- soa[i]

      # case 1: visual (=first) wins
      if (tau < omega) {
        P.Iv1 <- 1/(lambdaV+lambdaA) * (lambdaV * (1-exp(lambdaA*(-omega+tau)))+lambdaA*(1-exp(-lambdaV*tau)))
      }
      # case 2: omega <= tau, no integration
      else  {
        P.Iv1 <- lambdaA/(lambdaV+lambdaA)*(exp(-lambdaV*tau)*(-1+exp(lambdaV*omega))) 
      }
      # case 3: acoustical (=second) wins, always integration
      P.Iv2 <- lambdaA/(lambdaV+lambdaA)*(exp(-lambdaV*tau)-exp(-lambdaV*(omega+tau)))
      
      P.Iv = P.Iv1 + P.Iv2

      predv[i] <- 1/lambdaV-exp(-lambdaV*tau)*(1/lambdaV-1/(lambdaV+lambdaA))+ mu - delta * P.Iv
    }
    return(predv)
    
    # Probability for Integration when the ACOUSTICAL stimulus is presented first
    
    for (j in seq_along(soa)) {
      
      tau <- soa[j]
      
      # case 1: acoustical (=first) wins
      if (tau < omega) {
        P.Ia1 <- 1/(lambdaA+lambdaV) * (lambdaA * (1-exp(lambdaV*(-omega+tau)))+lambdaV*(1-exp(-lambdaA*tau)))
      }
      # case 2: omega <= tau, no integration
      else  {
        P.Ia1 <- lambdaV/(lambdaA+lambdaV)*(exp(-lambdaA*tau)*(-1+exp(lambdaA*omega))) 
      }
      # case 3: visual (=second) wins, always integration
      P.Ia2 <- lambdaV/(lambdaV+lambdaA)*(exp(-lambdaA*tau)-exp(-lambdaA*(omega+tau)))
      
      P.Ia = P.Ia1 + P.Ia2
      
      preda[j] <- 1/lambdaA-exp(-lambdaA*tau)*(1/lambdaA-1/(lambdaA+lambdaV)) + mu - delta * P.Ia
    }
    return(preda)
    pred.merge <- cbind(preda,predv)
}


objective.function <- function(param, obs.m, obs.se, soa) {
  
  # check if parameters are in bounds
  if (param[1] < 5 && param[1] > 250 ) return(Inf)  # proc.A
  if (param[2] < 5 && param[2] > 250 ) return(Inf)  # proc.A
  if (param[3] < 0                   ) return(Inf)  # mu
  if (param[4] < 5 && param[4] > 1000) return(Inf)  # omega
  if (param[5] < 0 && param[5] > 175 ) return(Inf)  # delta
  
  pred.merge <- predict.rt(soa, param)
  
  sum(( (obs.m - pred.merge) / obs.se)^2)
}

estimateFAP <- function(dat, max.iter=100) {
  
  N <- nrow(dat)
  
  # Probability of integration
  soa <- c(0, 50, 100, 200)
  
  param.start <- c(
    runif(1, 5, 250),   # proc.A
    runif(1, 5, 250),   # proc.A
    runif(1, 0, 1000),  # mu)
    runif(1, 5, 1000),  # omega)
    runif(1, 5, 175)   # delta
  )
  
  # observed RTs
  obs.m <- colSums(dat) / N
  obs.se <- apply(dat, 2, sd) / sqrt(N)
  
  nlm(objective.function, param.start, obs.m = obs.m, obs.se = obs.se, soa
      = soa, iterlim = max.iter)
}



