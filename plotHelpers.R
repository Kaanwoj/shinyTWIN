### Estimation Tab Plots ###

# Plot predicted and observed RTs for FAP
plotPredObs.fap <- function(data, est) {

    # get SOA's
    soa <- as.numeric(sub("neg", "-", sub("SOA.", "", colnames(data))))

    # predict RTs from estimations
    pred <- predict.rt.fap(par = est$est$par, column.names = colnames(data))

    # mean and SE of observed reaction times
    obs <- colMeans(data)
    obs_se <- apply(data, 2, sd)/sqrt(nrow(data))

    # plot observed RTs
    plot(obs ~ soa, type="p", col="darkred", xlab="SOA",
         ylab="Reaction time (ms)", xaxt="n",
         ylim=c(min(obs-obs_se), max(obs+obs_se)))
    axis(1, at=soa)

    # add SE
    arrows(soa, obs-obs_se, soa, obs+obs_se, code=3, length=0.02, angle = 90,
           col="darkred")

    # plot predicted RTs
    points(pred ~ soa, type="l")

    legend("bottomright", legend=c("Mean observed RT (+- SE)",
                                   "Mean predicted RT"),
           lty=0:1, pch=c(1, NA), col=c("darkred", "black"))

}


# Plot predicted and observed RTs for RTP
plotPredObs.rtp <- function(data, est) {

    # get SOA's
    soa <- as.numeric(unlist(regmatches(colnames(data),
                                 gregexpr('\\(?[0-9]+',
                                          colnames(data)))))

    # predict RTs from estimations
    pred <- predict.rt.rtp(par = est$est$par, column.names = colnames(data))
    idx.aud <- grep("aud", colnames(data))
    idx.vis <- grep("vis", colnames(data))

    # mean and SE of observed reaction times
    obs <- colMeans(data)
    obs_se <- apply(data, 2, sd)/sqrt(nrow(data))

    # plot observed RTs
    par(mfrow=c(1,2))

    for (first in c("aud", "vis")) {

        if (first == "aud") {
            idx <- idx.aud
            title <- "auditory stimulus first"
        } else if (first == "vis") {
            idx <- idx.vis
            title <- "visual stimulus first"
        }

        plot(obs[idx] ~ soa[idx], type="p", col="darkred", xlab="SOA",
             ylab="Reaction time (ms)", xaxt="n",
             ylim=c(min(obs[idx]-obs_se[idx], pred[idx]),
                    max(obs[idx]+obs_se[idx], pred[idx])),
            main = title)
        axis(1, at=soa)

        # add SE
        arrows(soa[idx], obs[idx]-obs_se[idx], soa[idx],
               obs[idx]+obs_se[idx], code=3, length=0.02, angle = 90,
               col="darkred")

        # plot predicted RTs
        points(pred[idx] ~ soa[idx], type="l")

        legend("bottomright", legend=c("Mean observed RT (+- SE)",
                                       "Mean predicted RT"),
               lty=0:1, pch=c(1, NA), col=c("darkred", "black"))
    }

}
