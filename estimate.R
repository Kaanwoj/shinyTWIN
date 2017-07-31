##################
### Estimation ###
##################

source("estimationHelpers.R")

# Predict mean reaction times for focused attention paradigm
predict.rt.fap <- function(par, column.names) {

    lambdaA <- 1/par[1]     # lambdaA
    lambdaV <- 1/par[2]     # lambdaV
    mu      <-   par[3]     # mu
    omega   <-   par[4]     # omega
    delta   <-   par[5]     # delta

    soa <- as.numeric(sub("neg", "-", sub("SOA.", "", column.names)))

    pred <- numeric(length(soa))

    for (i in seq_along(soa)) {

        tau <- soa[i]

        # formula (2) in Kandil et al. (2014)
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

    pred
}


# Predict mean reaction times for redundant target paradigm
predict.rt.rtp <- function(par, column.names) {

    lambdaA <- 1/par[1]
    lambdaV <- 1/par[2]
    mu      <- par[3]
    omega   <- par[4]
    delta   <- par[5]

    pred <- numeric(length(column.names))

    for (i in seq_len(length(column.names))) {

        first.stimulus <- unlist(strsplit(column.names[i], "SOA."))[1]
        tau <- as.numeric(unlist(strsplit(column.names[i], "SOA."))[2])

        if (first.stimulus == "aud") {
            lambda.first <- lambdaA
            lambda.second <- lambdaV
        } else {
            lambda.first <- lambdaV
            lambda.second <- lambdaA
        }

        # formulae in appendix of Diederich & Colonius (2015) with changes from
        # matlab code of Kandil et al (2014)
        #   P.I = Pr(A + tau < V < A + tau + omega)  -> acoustic wins
        #         + Pr(V < A + tau < V + omega)      -> visual wins

        # case 1: first stimulus wins
        if (tau < omega) {
          p1 <- 1/(lambda.first + lambda.second) *
                  (lambda.first * (1 - exp(-lambda.second * (omega - tau))) +
                   lambda.second * (1 - exp(-lambda.first * tau)))
        }
        # case 2: omega <= tau, no integration
        else  {
          p1 <- lambda.second/(lambda.first+lambda.second) *
                  (exp(-lambda.first * (tau - omega)) - exp(-lambda.first * tau))
        }
        # case 3: second stimulus wins, always integration
        p2 <- lambda.second/(lambda.second + lambda.first) *
                  (exp(-lambda.first * tau) - exp(-lambda.first * (omega + tau)))

        P.I <- p1 + p2

        pred[i] <- 1/lambda.first - exp(-lambda.first * tau) *
                          (1/lambda.first - 1/(lambda.first + lambda.second)) +
                    mu - delta * P.I
    }

    names(pred) <- column.names

    pred
}

# Least Squares objective function
objective.function <- function(par, obs.m, obs.se, column.names, paradigm) {

    if (paradigm == "fap") {
        pred <- predict.rt.fap(par, column.names)
    } else if (paradigm == "rtp") {
        pred <- predict.rt.rtp(par, column.names)
    }

    sum(( (obs.m - pred) / obs.se )^2)
}

# Estimation function calling optimizer (optim())
estimate <- function(dat, paradigm) {

    # number of observations
    N <- nrow(dat)

    # observed RTs
    obs.m <- colSums(dat) / N
    obs.se <- apply(dat, 2, sd) / sqrt(N)

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
                 lower = bounds$lower, upper = bounds$upper,
                 method = "L-BFGS-B",
                 obs.m = obs.m, obs.se = obs.se,
                 column.names = colnames(dat), paradigm = paradigm
                 )

    list(est = est, param.start = param.start)
}
