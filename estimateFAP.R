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

    pred <- predict.rt(soa, param)

    sum(( (obs.m - pred) / obs.se )^2)
}

draw_start <- function(par, bounds) {
    low <- bounds$lower[par]
    up  <- bounds$upper[par]

    value <- switch(par,
                    proc.A = rexp(1, 1/100),
                    proc.V = rexp(1, 1/100),
                    mu     = rnorm(1, 100, 25),
                    omega  = rnorm(1, 200, 25),
                    delta  = rnorm(1, 50, 25)
                    )
    # todo: stop if value == NULL

    # Is starting value out of bounds? Draw again!
    if (value < low || value > up) {
        value <- draw_start(par, bounds)
    }

    value
}

estimate.fap <- function(dat) {

    # number of observations
    N <- nrow(dat)

    # observed RTs
    obs.m <- colSums(dat) / N
    obs.se <- apply(dat, 2, sd) / sqrt(N)

    # stimulus onset asynchonies
    soa <- c(-200, -100, -50, 0, 50, 100, 200)

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
                  obs.se = obs.se, soa = soa
                 )

    list(est = est, param.start = param.start)
}


