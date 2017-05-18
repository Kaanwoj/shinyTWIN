plotEstPred <- function(data, est) {

    # get SOA's
    soa <- as.numeric(sub("neg", "-", sub("SOA.", "", colnames(data))))

    # predict RTs from estimations
    pred <- predict.rt(soa, param = est$est$par)

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
