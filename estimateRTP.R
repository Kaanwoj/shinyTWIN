##################
### Estimation ###
##################

source("estimationHelpers.R")

# predicted RTs

# parameter:
# par: numeric vector of model parameters
# column.names: column names of data matrix
# value:
# numeric vector of predicted reaction times
predict.rt <- function(par, column.names) {

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

    return(pred)
}


objective.function <- function(par, obs.m, obs.se, column.names) {


    pred <- predict.rt(par, column.names=colnames(data))

    sum(( (obs.m - pred) / obs.se)^2)
}

estimate.rtp <- function(dat) {

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
                  lower = bounds$lower,
                  upper = bounds$upper,
                  method = "L-BFGS-B",
                  obs.m = obs.m,
                  obs.se = obs.se,
                  column.names=colnames(dat)
                 )

    list(est = est, param.start = param.start)
}



