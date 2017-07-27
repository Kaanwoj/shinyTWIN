##################
### Estimation ###
##################

source("estimationHelpers.R")

# Probability of integration
soa <- c(0, 50, 100, 200)
dat.merge <- cbind(dat.a,dat.v)

# observed RTs
obs.m <- colSums(dat.merge) / N
obs.se <- apply(dat.merge, 2, sd) / sqrt(N)

# predicted RTs

predict.rt <- function(soa, param) {

    lambdaA <- 1/param[1]
    lambdaV <- 1/param[2]
    mu      <- param[3]
    omega   <- param[4]
    delta   <- param[5]

    predv <- numeric(length(soa))
    preda <- numeric(length(soa))

    # formulae in appendix of Diederich & Colonius (2015) with changes from
    # matlab code of Kandil et al (2014)
    #   p1 = Pr(A + tau < V < A + tau + omega)  -> acoustic wins
    #   p2 = Pr(V < A + tau < V + omega)        -> visual wins

    # Probability for Integration when the ACOUSTIC stimulus is presented first
    for (j in seq_along(soa)) {

      tau <- soa[j]

      # case 1: acoustical (=first) wins
      if (tau < omega) {
        p1 <- 1/(lambdaA + lambdaV) *
                (lambdaA * (1 - exp(lambdaV * (-omega + tau))) +
                 lambdaV * (1 - exp(-lambdaA * tau)))
      }
      # case 2: omega <= tau, no integration
      else  {
        p1 <- lambdaV/(lambdaA+lambdaV) *
                (exp(lambdaA * (-tau + omega)) - exp(-lambdaA * tau))
      }
      # case 3: visual (=second) wins, always integration
      p2 <- lambdaV/(lambdaV + lambdaA) *
                (exp(-lambdaA * tau) - exp(-lambdaA * (omega + tau)))

      P.Ia <- p1 + p2

      preda[j] <- 1/lambdaA - exp(-lambdaA * tau) *
                        (1/lambdaA - 1/(lambdaA + lambdaV)) +
                  mu - delta * P.Ia
    }

    # Probability for Integration when the VISUAL stimulus is presented first
    for (i in seq_along(soa)) {

      tau <- soa[i]

      # case 1: visual (=first) wins
      if (tau < omega) {
          p2 <- 1/(lambdaV + lambdaA) *
                    (lambdaV * (1 - exp(-lambdaA * (omega - tau))) +
                     lambdaA * (1 - exp(-lambdaV * tau)))
      } else {
      # case 2: omega <= tau, no integration
          p2 <- lambdaA/(lambdaV+lambdaA) *
                    (exp(-lambdaV * (tau - omega)) - exp(-lambdaV * tau))
      }
      # case 3: acoustical (=second) wins, always integration
          p1 <- lambdaA/(lambdaV+lambdaA) *
                    (exp(-lambdaV * tau) - exp(-lambdaV * (omega + tau)))

      P.Iv = p1 + p2

      predv[i] <- 1/lambdaV - exp(-lambdaV * tau) *
                        (1/lambdaV - 1/(lambdaV + lambdaA)) +
                  mu - delta * P.Iv
    }

    pred.merge <- cbind(preda,predv)
}


objective.function <- function(param, obs.m, obs.se, soa) {

    pred <- predict.rt(soa, param)

    sum(( (obs.m - pred) / obs.se)^2)
}

estimate.rtp <- function(dat) {

    # number of observations
    N <- nrow(dat)

    # observed RTs
    obs.m <- colSums(dat) / N
    obs.se <- apply(dat, 2, sd) / sqrt(N)

    # stimulus onset asynchonies
    # todo: remove hard coding of soa
    soa <- c(0, 50, 100, 200)

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



