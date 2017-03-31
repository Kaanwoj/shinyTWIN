##################
### Estimation ###
##################

dat <- read.table("simDataFAP.txt", sep=";", header=TRUE)

N <- nrow(dat)

# Probability of integration
soa <- c(-200, -100, -50, 0, 50, 100, 200)

param.start <- c(
               "lambdaA" = 100,
               "lambdaV" = 50,
               "mu"      = 150,
               "sigma"   = 25,
               "omega"   = 200,
               "delta"   = 50
               )


# observed RTs
obs.m <- colSums(dat) / N
obs.se <- apply(dat, 2, sd) / sqrt(N)

# predicted RTs

# formula (2) in Kandil et al. (2014)
predict.rt <- function(soa, param) {

    lambdaA <- param["lambdaA"]
    lambdaV <- param["lambdaV"]
    mu      <- param["mu"]
    sigma   <- param["sigma"]
    omega   <- param["omega"]
    delta   <- param["delta"]

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
      else if ((tau <= 0) && (tau + omega >= 0)) {
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


objective.function <- function(param, obs.m, obs.se) {

    pred <- predict.rt(soa, param)

    sum(( (obs.m - pred) / obs.se)^2)

}

optim(param.start, objective.function)
