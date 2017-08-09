library(ggplot2)
library(plyr)
library(xtable)
source("simulateFAP.R")
source("simulateRTP.R")
source("estimate.R")
source("plotHelpers.R")

server <- shinyServer(function(input, output, session) {

  ########################
  ### Introduction Tab ###
  ########################

  # Action Buttons that redirect you to the corresponding tab
  observeEvent(input$parambutton, {
    updateNavbarPage(session, "page",
                     selected = "Para")
  })

  observeEvent(input$simbutton, {
    updateNavbarPage(session, "page",
                     selected = "Sim")
  })

  observeEvent(input$estbutton, {
    updateNavbarPage(session, "page",
                     selected = "Est")
  })

  ######################
  ### Parameters Tab ###
  ######################

  ### Plot distribution of first stage ###
  output$uni_data_t <- renderPlot({

    # x-sequence for plotting the unimodal distributions
    x <- seq(0,300)

    if (input$distPar == "expFAP") {

    ggplot(data.frame(x=seq(0,300)),aes(x=x)) +
      stat_function(fun=dexp,geom = "line", size=1, col= "blue", args =
                    (mean=1/input$mu_nt)) +
      stat_function(fun=dexp,geom = "line", size=1, col= "red", args =
                    (mean=1/input$mu_t)) +
      labs(title = "Distribution of 1st stage processing",
           x = "Time (ms)", y = "Density function") +
      theme(aspect.ratio=0.75) +
      theme(plot.title = element_text(size=16, face="bold", hjust = 0.5,
                                      margin = margin(10, 0, 10, 0)))

    } else if (input$distPar == "normFAP") {

      ggplot(data.frame(x=seq(0,300)),aes(x=x)) +
        stat_function(fun=dnorm, geom = "line", size=1, col= "blue", args =
                      list(mean=input$mun_s1, sd = input$sd_s1)) +
        stat_function(fun=dnorm, geom = "line", size=1, col= "red", args =
                      list(mean=input$mun_s2, sd = input$sd_s2)) +
        labs(title = "Distribution of 1st stage processing",
             x = "Time (ms)", y = "Density function") +
        theme(aspect.ratio=0.75) +
        theme(plot.title = element_text(size=16, face="bold", hjust = 0.5,
                                      margin = margin(10, 0, 10, 0)))
    } else {

    # check if given values make sense (kept it in here because we need an interval. Gives error message )
      validate(
        need(input$range_s1[2] - input$range_s1[1] > 0,
             "Please check your input data for the first stimulus!"),
        need(input$range_s2[2] - input$range_s2[1] > 0,
             "Please check your input data for the second stimulus!")
      )
      ggplot(data.frame(x=seq(0,300)),aes(x=x)) + 
        stat_function(fun=dunif, geom = "line", size=1, col= "blue", args =
                      list(min=input$range_s1[1], max = input$range_s1[2])) +
        stat_function(fun=dunif, geom = "line", size=1, col= "red", args =
                      list(min=input$range_s2[1], max = input$range_s2[2])) +
        labs(title = "Distribution of 1st stage processing",
             x = "Time (ms)", y = "Density function") +
        theme(aspect.ratio=0.75) +
        theme(plot.title = element_text(size=16, face="bold", hjust = 0.5,
                                        margin = margin(10, 0, 10, 0)))
      }
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
      labs(x = "Stimulus-onset asynchrony \n(SOA)",
           y = "Reaction Times (ms)",
           title = "Mean Reaction Times for the unimodal \nand bimodal task condition") +
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
        + labs(title="Integration function",
             x = "SOA",
             y = "Probability of integration")
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
    if (input$paradigmSim == "fap") {
      soa <- sort(as.numeric(unlist(strsplit(input$soa.in, ","))))
    } else if (input$paradigmSim == "rtp") {
      soa <- sort(as.numeric(unlist(strsplit(input$soa.in, ","))))
    }
  })

  # Simulate data
  dataset <- eventReactive(input$sim_button, {
    if (input$paradigmSim == "fap") {
      list(data = simulate.fap(soa=soa(), proc.A=input$proc.A,
                               proc.V=input$proc.V, mu=input$mu, sigma=sigma(),
                               omega=input$sim.omega, delta=input$sim.delta,
                               N=input$N),
           paradigm = "fap")
    }
    else if (input$paradigmSim == "rtp") {
      list(data = simulate.rtp(soa=soa(), proc.A=input$proc.A,
                                proc.V=input$proc.V, mu=input$mu,
                                sigma=sigma(), omega=input$sim.omega,
                                delta=input$sim.delta, N=input$N),
            paradigm = "rtp")
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
      first.stimulus <- unlist(strsplit(column.names[i], "SOA."))[1]
      tau <- as.numeric(unlist(strsplit(column.names[i], "SOA.")))[2]
      if ((first.stimulus == "aud" | first.stimulus == "vis") &
           !is.na(tau)) {
        NULL
      } else {
      "Did you choose the right paradigm? Column names must be in the form of
      'audSOA.0'. See column names in Simulation tab."}
    } else if (paradigm == "fap") {
      soa <- as.numeric(sub("neg", "-", sub("SOA.", "", column.names)))
      if (!any(is.na(soa))) {
        NULL
      } else {
      "Did you choose the right paradigm? Column names must be in the form of
      'SOA.neg50' or 'SOA.50'. See column names in Simulation tab."}
      }
  }

  # Upload data file
  dataUpload <- reactive({
    inFile <- input$file1
    if (is.null(inFile)) return(NULL)
    out <- list(
      data = read.table(inFile$datapath, header= TRUE, sep = ";"),
      paradigm = input$paradigmUpload)
    validate(
      correct_colnames(colnames(out$data), out$paradigm)
    )
    out
  })

  datasetEst <- reactive({
    if (input$whichDataEst == "sim") dataset()
    else dataUpload()
  })

  # Estimate parameters according to paradigm
  est.out <- eventReactive(input$est_button, {
    estimate(datasetEst()$data, paradigm=datasetEst()$paradigm)
  })

  # Show parameter estimates
  output$estTextOut <- renderTable({
    tab <- rbind(est.out()$est$par, est.out()$param.start,
                 c(proc.A=input$proc.A, proc.V=input$proc.V, mu=input$mu,
                   omega=input$sim.omega, delta=input$sim.delta))

    dimnames(tab) <- list(c("estimated value", "start value", "true value"),
                          c("1&#8260&#955<sub>A</sub>",
                            "1&#8260&#955<sub>B</sub>", "&#956", "&#969",
                            "&#948"))
    tab
  }, rownames=TRUE, sanitize.text.function=function(x) x)

  # Plot predicted and observed RTs as a function of SOA
  output$plotEstPred <- renderPlot({
    if (isolate(datasetEst()$paradigm) == "fap") {
        plotEstPred.fap(isolate(datasetEst()$data), est.out())
    } else if (isolate(datasetEst()$paradigm) == "rtp") {
        plotEstPred.rtp(isolate(datasetEst()$data), est.out())
    }
  })
})

