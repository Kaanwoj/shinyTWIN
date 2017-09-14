library(ggplot2)
source("simulateFAP.R")
source("simulateRTP.R")
source("estimate.R")
source("plotHelpers.R")

server <- shinyServer(function(input, output, session) {

  ########################
  ### Introduction Tab ###
  ########################

  # Action Buttons that redirect you to the corresponding tab
  observeEvent(input$theorybutton, {
    updateNavbarPage(session, "page", selected = "Theory")
  })

  observeEvent(input$parambutton, {
    updateNavbarPage(session, "page", selected = "Para")
  })

  observeEvent(input$simbutton, {
    updateNavbarPage(session, "page", selected = "Sim")
  })

  observeEvent(input$estbutton, {
    updateNavbarPage(session, "page", selected = "Est")
  })

  ######################
  ### Parameters Tab ###
  ######################

  ### Plot distribution of first stage ###
  output$uni_data_t <- renderPlot({

    # x-sequence for plotting the unimodal distributions
    x <- seq(0,300)
    density.plot <- ggplot(data.frame(x=x),aes(x=x))

    if (input$distPar == "expFAP") {
      density.plot <- density.plot +
        stat_function(fun=dexp,geom = "line", size=1, col= "blue", args =
                      (mean=1/input$mu_nt)) +
        stat_function(fun=dexp,geom = "line", size=1, col= "red", args =
                      (mean=1/input$mu_t))

    } else if (input$distPar == "normFAP") {
      density.plot <- density.plot +
        stat_function(fun=dnorm, geom = "line", size=1, col= "blue", args =
                      list(mean=input$mun_s1, sd = input$sd_s1)) +
        stat_function(fun=dnorm, geom = "line", size=1, col= "red", args =
                      list(mean=input$mun_s2, sd = input$sd_s2))
    } else {

    # check if given values make sense (kept it in here because we need an
    # interval. Gives error message )
      validate(
        need(input$range_s1[2] - input$range_s1[1] > 0,
             "Please check your input data for the first stimulus!"),
        need(input$range_s2[2] - input$range_s2[1] > 0,
             "Please check your input data for the second stimulus!")
      )
      density.plot <- density.plot +
        stat_function(fun=dunif, geom = "line", size=1, col= "blue", args =
                      list(min=input$range_s1[1], max = input$range_s1[2])) +
        stat_function(fun=dunif, geom = "line", size=1, col= "red", args =
                      list(min=input$range_s2[1], max = input$range_s2[2]))
    }
    density.plot <- density.plot +
      labs(title = "First Stage Processing Time Density Function", x =
           "time (ms)", y = "density") +
      theme(aspect.ratio=0.75) +
      theme(plot.title = element_text(size=16, face="bold", hjust = 0.5,
                                      margin = margin(10, 0, 10, 0)))
    density.plot
  })

  ### Plot simulated RT means as a function of SOA ###

  # different stimulus onsets for the non-target stimulus
  tau <- c(-200 ,-150, -100, -50, -25, 0, 25, 50)
  SOA <- length(tau)
  n <- 1000 # number of simulated observations

  # Simulation of fist stage, depending on chosen distribution
  # target stimulus
  stage1_t <- reactive({
    if (input$distPar == "expFAP") (rexp(n, rate = 1/input$mu_t))
    else if (input$distPar == "normFAP") (rnorm(n, mean = input$mun_s1, sd =
                                             input$sd_s1))
    else (runif(n, min = input$range_s1[1], max = input$range_s1[2]))
  })

  # non-target stimulus
  stage1_nt <- reactive({
    if (input$distPar == "expFAP") (rexp(n, rate = 1/input$mu_nt))
    else if (input$distPar == "normFAP") (rnorm(n, mean = input$mun_s2, sd =
                                             input$sd_s2))
    else (runif(n, min = input$range_s2[1], max = input$range_s2[2]))
  })

  # Simulation of the second stage (assumed normal distribution)
  stage2 <- reactive({
    rnorm(n, mean = input$mu_second, sd = input$sd_second)
  })

  # the matrix where integration is simulated in (later on)
  integration <- matrix(0, nrow = n, ncol = SOA)

  output$data <- renderPlot({
    # check if the input variables make sense for the uniform data


    # unimodal reaction times (first plus second stage, without integration)
    obs_t <- stage1_t() + stage2()

    # integration can only occur if the non-target wins the race
    # and the target falls into the time-window <=> non-target + tau < target

    for(i in 1:SOA){
      integration[,i] <- stage1_t() - (stage1_nt() + tau[i])
    } # > 0 if non-target wins, <= 0 if target wins

    # no integration if target wins (preparation for further calculation)
    integration[integration <= 0] <- Inf

    # probability for integration:
    # if the difference between the two stimuli falls into the time-window,
    # integration occurs

    # matrix with logical entries indicating for each run 
    # whether integration takes place (1) or not (0)
    integration <- as.matrix((integration <= input$omega), mode = "integer")

    # bimodal simulation of the data:
    # first stage RT of the target stimulus, second stage processing time
    # and the amount of integration if integration takes place
    obs_bi <- matrix(0, nrow = n, ncol = SOA)
    obs_bi <- stage1_t() + stage2() - integration*input$delta

    # set negative values to zero
    obs_bi[obs_bi < 0] <- 0

    # calculate the RT means of the simulated data for each SOA
    means <- vector(mode = "integer", length = SOA)
    mean_t <- vector(mode = "integer", length = SOA)
    for (i in 1:SOA){
      means[i] <- mean(obs_bi[,i])
      mean_t[i] <- mean(obs_t)
    }

    # store the results in a data frame
    results <- data.frame(tau, mean_t, means)

    # maximum value of the data frame (as threshold for the plot)
    max <- max(mean_t, means) + 50


    # plot the means against the SOA values
    ggplot(data = results) +
      geom_line( aes(tau, means), color = "red", size = 1) +
      geom_line (aes(tau, mean_t), color = "blue", size = 1) +
      labs(x = "stimulus-onset asynchrony (SOA)",
           y = "reaction time (ms)",
           title = "Mean Predicted Reaction Times for the \nUnimodal and Crossmodal Condition") +
      ylim(c(0,max)) +
      theme(aspect.ratio=0.75) +
      theme(plot.title = element_text(size=16, face="bold", hjust = 0.5, margin
                                      = margin(10, 0, 10, 0)))
  })

  ### Plot probability of integration as a function of SOA
  # check the input data of the uniform distribution
  output$prob <- renderPlot({

    # same integration calculation as for the other plot
    for (i in 1:SOA) {
      integration[,i] <- stage1_t() - (stage1_nt() + tau[i])
    }
    integration[integration <= 0] <- Inf
    integration <- as.matrix((integration <= input$omega), mode = "integer")

    # calculate the probability for each SOA that integration takes place
    prob_value <- vector(mode = "integer", length = SOA)
    for (i in 1:SOA) {
      prob_value[i] <- sum(integration[,i]) / length(integration[,i])
      # probability of integration for all runs
    }

    # put the results into a data frame
    results <- data.frame(tau, prob_value)

    # plot the results
    if (input$distPar == "uniFAP" && (input$range_s1[2] == input$range_s1[1] || input$range_s2[2] == input$range_s2[1]))
      ggplot()  # plot nothing if it doesnt make sense (they didnt tick an interval, but a single number)

    else(
      ggplot(data= results, aes(x=tau, y=prob_value)) + geom_line(size=1, color= "blue")
        + labs(title="Probability of Integration as a Function of SOA",
             x = "stimulus-onset asynchrony (SOA)",
             y = "probability of integration")
      + theme(aspect.ratio=0.75)
      + theme(plot.title = element_text(size=16, face="bold", hjust = 0.5,
                                        margin = margin(10, 0, 10, 0)))
    )
  })

  ######################
  ### Simulation Tab ###
  ######################

  # Fix variance on second stage
  sigma <- reactive({
      input$mu / 5
  })

  # Input for SOAs
  output$soa_input <- renderUI({
    if (input$paradigmSim == "fap") {
      default.soa <- "-200,-100,-50,0,50,100,200"
    } else if (input$paradigmSim == "rtp") {
      default.soa <- "0,50,100,200"
    }
    textInput("soa.in","Stimulus onset asynchronies (SOAs, comma delimited)",
              default.soa)
  })


  # Get SOAs from input
  soa <- eventReactive(input$sim_button, {
    validate(need(
    tryCatch(soa <- sort(as.numeric(unlist(strsplit(input$soa.in, ",")))),
             error=function(e){}, warning=function(w){}),
                 "SOA input can not be used. Make sure its only comma-separated numbers."))
    soa
  })

  # Simulate data
  dataset <- eventReactive(input$sim_button, {
    if (input$paradigmSim == "fap") {
      list(data = simulate.fap(soa=soa(), proc.A=input$proc.A,
                               proc.V=input$proc.V, mu=input$mu, sigma=sigma(),
                               omega=input$sim.omega, delta=input$sim.delta,
                               N=input$N),
           paradigm = "fap",
           trueValues = c(proc.A=input$proc.A, proc.V=input$proc.V,
                          mu=input$mu, omega=input$sim.omega,
                          delta=input$sim.delta)
           )
    }
    else if (input$paradigmSim == "rtp") {
      list(data = simulate.rtp(soa=soa(), proc.A=input$proc.A,
                                proc.V=input$proc.V, mu=input$mu,
                                sigma=sigma(), omega=input$sim.omega,
                                delta=input$sim.delta, N=input$N),
           paradigm = "rtp",
           trueValues = c(proc.A=input$proc.A, proc.V=input$proc.V,
                          mu=input$mu, omega=input$sim.omega,
                          delta=input$sim.delta)
        )
    }
  })

  # Show data in a table
  output$simtable <- renderTable({
      head(dataset()$data, input$nrowShow)
    })

  # Plot data
  output$simplot <- renderPlot({
      if (dataset()$paradigm == "fap") {
        boxplot(dataset()$data, ylab="reaction time (ms)",
                xlab="stimulus-onset asynchrony (soa)", main="", xaxt="n")
        axis(1, at=1:length(soa()), labels=soa())
      } else if (dataset()$paradigm == "rtp"){
        par(mfrow=c(1,2))
        boxplot(dataset()$data[ , grep("aud", colnames(dataset()$data))],
                ylab="reaction time (ms)", xlab="stimulus-onset asynchrony (soa)",
                main="auditory target", xaxt="n")
        axis(1, at=1:length(soa()), labels=soa())
        boxplot(dataset()$data[ , grep("vis", colnames(dataset()$data))],
                ylab="reaction time (ms)", xlab="stimulus-onset asynchrony (soa)",
                main="visual target", xaxt="n")
        axis(1, at=1:length(soa()), labels=soa())
      }
  })

  # Download the Simulation output

  # downloadHandler() takes two arguments, both functions.
  # The content function is passed a filename as an argument, and
  #   it should write out data to that filename.
  output$downloadData <- downloadHandler(

    # This function returns a string which tells the client
    # browser what name to use when saving the file.
    filename = function() {
      paste("dat-", Sys.Date(), ".csv", sep="")
    },

    # This function should write data to a file given to it by
    # the argument 'file'.
    content = function(file) {
        write.table(dataset()$data, file, quote = FALSE, sep = ";",
                  row.names = FALSE)
    }
  )

  ###################### ESTIMATION ###########################

  correct_colnames <- function(column.names, paradigm) {
    if (paradigm == "rtp") {
      first.stimulus <- unlist(strsplit(column.names, "SOA."))[1]
      suppressWarnings(
        # as.numeric introduces NA, this warning is suppressed
        tau <- as.numeric(unlist(strsplit(column.names, "SOA.")))[2])
      if ((first.stimulus == "aud" | first.stimulus == "vis") &
           !is.na(tau)) {
        NULL
      } else {
        "Did you choose the right paradigm? Column names must be in the form of
        'audSOA.0'. See column names in Simulation tab."
      }
    } else if (paradigm == "fap") {
      suppressWarnings(
        soa <- as.numeric(sub("neg", "-", sub("SOA.", "", column.names))))
        # as.numeric creates NA, this warning is suppressed
      if (!any(is.na(soa))) {
        NULL
      } else {
        "Did you choose the right paradigm? Column names must be in the form of
        'SOA.neg50' or 'SOA.50'. See column names in Simulation tab."
      }
    }
  }

  # Upload data file
  dataUpload <- reactive({
    inFile <- input$file1
    if (is.null(inFile)) return(NULL)
    # read in file
    validate(need(tryCatch(
      data <- read.table(inFile$datapath, header=TRUE, sep=";"),
      error=function(e) {}, warning=function(w) {}),
      "File could not be read in. Make sure it meets the requirements."))

    out <- list(
      data = data,
      paradigm = input$paradigmUpload,
      trueValues = FALSE)

    validate(correct_colnames(colnames(out$data), out$paradigm))
    out
  })

  datasetEst <- reactive({
    if (input$whichDataEst == "sim") {
      # why does this not show?
      validate(need(dataset(), "Simulate data first"))
      dataset()
    }
    else {
      validate(need(dataUpload(), "Upload data first"))
      dataUpload()
    }
  })

  # Estimate parameters according to paradigm
  est.out <- eventReactive(input$est_button, {
    estimate(datasetEst()$data, paradigm=datasetEst()$paradigm)
  })

  # Show parameter estimates
  estTab <- eventReactive(input$est_button, {
    est <- est.out()
      tab <- rbind(round(est$est$par, digits=2),
                   round(est$param.start, digits=2),
                   datasetEst()$trueValues)

      dimnames(tab) <- list(c("estimated value", "start value", "true value"),
                            c("1&#8260&#955<sub>A</sub>",
                              "1&#8260&#955<sub>B</sub>", "&#956", "&#969",
                              "&#948"))

    suppressWarnings( if (datasetEst()$trueValues == FALSE) {
        tab <- tab[1:2,]
    })

      tab
  })

  output$estTextOut <- renderTable({
      estTab()
  }, rownames=TRUE, sanitize.text.function=function(x) x)

  # Plot predicted and observed RTs as a function of SOA
  predObsPlot <- eventReactive(input$est_button, {
    est <- est.out()
    if (datasetEst()$paradigm == "fap") {
        plotPredObs.fap(datasetEst()$data, est)
    } else if (datasetEst()$paradigm == "rtp") {
        plotPredObs.rtp(datasetEst()$data, est)
    }
    })

  output$plotPredObs <- renderPlot({
    predObsPlot()
  })
})

