##################
### Estimation ###
##################

source("estimationHelpers.R")

# formula (2) in Kandil et al. (2014)
predict.rt <- function(par, column.names) {

    lambdaA <- 1/par[1]     # lambdaA
    lambdaV <- 1/par[2]     # lambdaV
    mu      <-   par[3]     # mu
    omega   <-   par[4]     # omega
    delta   <-   par[5]     # delta

    soa <- as.numeric(sub("neg", "-", sub("SOA.", "", column.names)))

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

objective.function <- function(param, obs.m, obs.se, column.names) {

    pred <- predict.rt(par, column.names)

    sum(( (obs.m - pred) / obs.se )^2)
}

estimate.fap <- function(dat) {

    # number of observations
    N <- nrow(dat)

    # observed RTs
    obs.m <- colSums(dat) / N
    obs.se <- apply(dat, 2, sd) / sqrt(N)

    # stimulus onset asynchonies
    # todo: remove hard coding of soa
    # soa <- c(-200, -100, -50, 0, 50, 100, 200)

    # lower and upper bounds for parameter estimates
    bounds <- list(lower = c("proc.A" = 5,
                             "proc.V" = 5,
                             "mu"     = 0,
                             "omega"  = 5,
                             "delta"  = 0),
                   upper = c("proc.A" = 250,
                             "proc.V" = 250,
                             "mu"     = Inf,
                             "omega"  = 1000,
                             "delta"  = 175)
                             )

    # draw starting values for parameters
    param.start <- c(
                     draw_start("proc.A", bounds),
                     draw_start("proc.A", bounds),
                     draw_start("mu", bounds),
                     draw_start("omega", bounds),
                     draw_start("delta", bounds)
                    )

    # estimate parameters
    est <- optim(par = param.start, fn = objective.function,
                  lower = bounds$lower,
                  upper = bounds$upper,
                  method = "L-BFGS-B",
                  obs.m = obs.m,
                  obs.se = obs.se,
                  column.names = colnames(dat)
                 )

    list(est = est, param.start = param.start)
}


