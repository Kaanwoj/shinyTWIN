##################
### Estimation ###
##################

# formula (2) in Kandil et al. (2014)
predict.rt <- function(soa, param) {

    lambdaA <- 1/param[1]     # lambdaA
    lambdaV <- 1/param[2]     # lambdaV
    mu      <-   param[3]     # mu
    omega   <-   param[4]     # omega
    delta   <-   param[5]     # delta

    pred <- numeric(length(soa))

    for (i in seq_along(soa)) {

      tau <- soa[i]

      # case 1: tau < tau + omega < 0
      if ((tau <= 0) && (tau + omega < 0)) {
        P.I <- (lambdaV / (lambdaV + lambdaA)) * exp(lambdaA * tau) *
               (-1 + exp(lambdaA * omega))
      }
      # case 2: tau < 0 < tau + omega (should be tau <= 0)
      #         add tau + omega = 0, to include that case here, case 3 would
      #         return Inf
      else if (tau + omega == 0) {
        P.I <- 0
      }
      else if ((tau <= 0) && (tau + omega > 0)) {
        P.I <- 1 / (lambdaV + lambdaA) *
               ( lambdaA * (1 - exp(-lambdaV * (omega + tau))) +
                 lambdaV * (1 - exp(lambdaA * tau)) )
      }
      # case 3: 0 < tau < tau + omega
      else {
        P.I <- (lambdaA / (lambdaV + lambdaA)) *
               (exp(-lambdaV * tau) - exp(-lambdaV * (omega + tau)))
      }
      pred[i] <- 1/lambdaV + mu - delta * P.I
    }

    return(pred)
}

objective.function <- function(param, obs.m, obs.se, soa) {

    # check if parameters are in bounds
    if (param[1] < 5 && param[1] > 250 ) return(Inf)  # proc.A
    if (param[2] < 5 && param[2] > 250 ) return(Inf)  # proc.A
    if (param[3] < 0                   ) return(Inf)  # mu
    if (param[4] < 5 && param[4] > 1000) return(Inf)  # omega
    if (param[5] < 0 && param[5] > 175 ) return(Inf)  # delta

    pred <- predict.rt(soa, param)

    sum(( (obs.m - pred) / obs.se)^2)
}

estimateFAP <- function(dat, max.iter=100) {

    N <- nrow(dat)

    # Probability of integration
    soa <- c(-200, -100, -50, 0, 50, 100, 200)

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


