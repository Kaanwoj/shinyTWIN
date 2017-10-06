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

  # Set fixed parameters
  # different stimulus onsets for the non-target stimulus
  tau <- c(-200 ,-150, -100, -50, -25, 0, 25, 50, 100, 150, 200)
  SOA <- length(tau)
  n <- 1000 # number of simulated observations
  x <- seq(0, 300) # range for density

  # Density and simulation of first stage, depending on chosen distribution
  # target stimulus
  stage1_t <- reactive({
    if (input$distPar == "expFAP") {
      density.t <- dexp(x, rate=1/input$mu_t)
      sim.t <- rexp(n, rate = 1/input$mu_t)
    } else if (input$distPar == "normFAP") {
      density.t <- dnorm(x, mean=input$mun_s1, sd=input$sd_s1)
      sim.t <- rnorm(n, mean = input$mun_s1, sd = input$sd_s1)
    } else if (input$distPar =="uniFAP") {
      # check if given values make sense (kept it in here because we need an
      # interval. Gives error message )
      validate(
        need(input$range_s1[2] - input$range_s1[1] > 0,
             "Please check your input data for the first stimulus!")
      )
      density.t <- dunif(x, min=input$range_s1[1], max=input$range_s1[2])
      sim.t <- runif(n, min = input$range_s1[1], max = input$range_s1[2])
    }
    list(sim=sim.t, density=density.t)
  })

  # non-target stimulus
  stage1_nt <- reactive({
    if (input$distPar == "expFAP") {
      density.nt <- dexp(x, rate=1/input$mu_nt)
      sim.nt <- rexp(n, rate = 1/input$mu_nt)
    } else if (input$distPar == "normFAP") {
      density.nt <- dnorm(x, mean=input$mun_s2, sd=input$sd_s2)
      sim.nt <- rnorm(n, mean = input$mun_s2, sd = input$sd_s2)
    } else if (input$distPar =="uniFAP") {
      validate(
        need(input$range_s2[2] - input$range_s2[1] > 0,
             "Please check your input data for the second stimulus!")
      )
      density.nt <- dunif(x, min=input$range_s2[1], max=input$range_s2[2])
      sim.nt <- runif(n, min = input$range_s2[1], max = input$range_s2[2])
    }
    list(sim=sim.nt, density=density.nt)
  })

  # Simulation of the second stage (assumed normal distribution)
  stage2 <- reactive({
    rnorm(n, mean = input$mu_second, sd = input$sd_second)
  })

  # the matrix where integration is simulated in
  integr.matrix <- reactive({
    integr.matrix <- matrix(0, nrow = n, ncol = SOA)
    V <- stage1_t()$sim
    A <- stage1_nt()$sim
    if (input$Parampar == "fap") {
      for (i in 1:SOA) {
          # Integration occurs if:
          # NT + tau < T < NT + tau + omega
          integr.matrix[,i] <- (A + tau[i] < V) & (V < A + input$omega + tau[i])
      }
    } else if (input$Parampar == "rtp") {
      for (i in 1:SOA) {
          # Integration occurs if:
          # {A + tau < V < A + tau + omega} U {V < A + tau > V + omega}
          # acoustic wins U visual wins, visual stimulus is presented at t=0
          integr.matrix[,i] <- ((A + tau[i] < V) &
                                (V < A + input$omega + tau[i])) |
                               ((V < A + tau[i]) &
                                (A + tau[i] < V + input$omega))
      }
    }
    integr.matrix
  })

  ### Plots ###
  # Plot distribution of first stage
  output$stage1_density <- renderPlot({
    plot(stage1_t()$density, type = "l", xlim = c(0, 300), col = "red",
      main = "First Stage Processing Time Density Function", xlab = "time (ms)",
      ylab = "density")
    lines(stage1_nt()$density, xlim = c(0, 300), col = "blue")
    legend("topright", c("visual stimulus", "auditory stimulus"),
           col = c("red", "blue"), lty = 1)
  })

  # Plot probability of integration as a function of SOA
  output$prob <- renderPlot({
    # calculate the probability for each SOA that integration takes place
    prob_value <- colSums(integr.matrix()) / nrow(integr.matrix())

    plot(tau, prob_value, type = "b", col = "blue",
         main = "Probability of Integration as a Function of SOA",
         xlab = "stimulus-onset asynchrony (SOA)",
         ylab = "probability of integration")
  })

  # Plot uni- and bi-modal reaction times as a function of SOA
  output$data <- renderPlot({
    # unimodal reaction times (first plus second stage, without integration)
    rt_uni <- stage1_t()$sim + stage2()

    # bimodal reaction times:
    if (input$Parampar == "fap") {
      # E(RT_VA) = E(V) + mu + P(I_FAP) * delta
      rt_bi <- stage1_t()$sim + stage2() - integr.matrix() * input$delta
    } else if (input$Parampar == "rtp") {
      # E(RT_VA) = E(min(V, A)) + mu + P(I_RTP) * delta
      rt_bi <- sapply(seq_along(stage1_t()$sim),
                    function(i) min(stage1_t()$sim[i], stage1_nt()$sim[i])) +
               stage2() - integr.matrix() * input$delta
    }

    # calculate the RT means of the simulated data for each SOA
    means_bi <- colMeans(rt_bi)
    mean_uni <- mean(rt_uni)

    # plot the means against the SOA values
    # maximum value of the data frame (as threshold for the plot)
    max <- max(mean_uni, means_bi) + 50
    min <- min(means_bi, mean_uni, 0)

    plot(tau, means_bi, type = "b", col = "red", ylim=c(min, max),
         ylab = "reaction times (ms)", xlab = "stimulus-onset asynchrony (SOA)",
         main = "Mean Predicted Reaction Times for the \nUnimodal and Crossmodal Condition")
    points(tau, rep(mean_uni, SOA), type = "l", col = "blue")
    legend("bottomright", title = "Condition",
           legend = c("crossmodal", "unimodal (visual)"), col = c("red", "blue"), lty = 1)
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

  output$chisqValue <- renderTable({
      paste("Objective function value: &#967<sup>2</sup> = ",
            signif(est.out()$est$value, digits=4))
  }, colnames=FALSE, sanitize.text.function=function(x) x)

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

