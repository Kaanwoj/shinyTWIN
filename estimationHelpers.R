# Draw starting values for optimizer, within parameter bounds
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

