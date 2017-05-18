library(ggplot2)
library(plyr)
source("simulateFAP.R")
source("estimateFAP.R")
source("simulateRTP.R")
source("plotHelpers.R")
library(xtable)

server <- shinyServer(function(input, output) {
  
  # different stimulus onsets for the non-target stimulus
  tau <- c(-200 ,-150, -100, -50, -25, 0, 25, 50)
  SOA <- length(tau)
  n <- 1000 # number of simulated observations
  
  
  # unimodal simulation of the first stage according to the chosen distribution
  # target stimulus
  stage1_t <- reactive({ if(input$dist == "expFAP")(rexp(n, rate = 1/input$mu_t))
    else if (input$dist == "normFAP")(rnorm(n,mean = input$mun_s1, sd = input$sd_s1))
    else (runif(n,min= input$min_s1, max = input$max_s1))
  })
  
  # non-target stimulus
  stage1_nt <- reactive({ if(input$dist == "expFAP")(rexp(n, rate = 1/input$mu_nt))
    else if (input$dist == "normFAP")(rnorm(n, mean = input$mun_s2, sd = input$sd_s2))
    else (runif(n, min = input$min_s2, max = input$max_s2))
  })
  
  # simulation of the second stage (assumed normal distribution)
  stage2 <- reactive({rnorm(n, mean = input$mu_second, sd = input$sd_second)})
  
  # the matrix where integration is simulated in (later on)
  integration <- matrix(0, 
                        nrow = n,
                        ncol = SOA)
  
  # x-sequence for plotting the unimodeal distributions
  
  x <- seq(0,300)
  
  
  ## --- PLOT THE DISTRIBUTION OF THE FIRST STAGE RT: FIRST STAGE ALWAYS EXPONENTIAL
  output$uni_data_t <- renderPlot({ 
    
    if (input$dist == "expFAP")
      
    ggplot(data.frame(x=seq(0,300)),aes(x=x)) + 
      stat_function(fun=dexp,geom = "line",size=1, col= "blue",args = (mean=1/input$mu_nt)) + 
      stat_function(fun=dexp,geom = "line",size=1,col= "red", args = (mean=1/input$mu_t)) +
     labs(title = "Distribution of 1st stage processing",
           x = "Time (ms)",
           y = "Density function") + 
   theme(aspect.ratio=0.75) +
    theme(plot.title = element_text(size=16, face="bold", hjust = 0.5,
           margin = margin(10, 0, 10, 0)))
    
    else if (input$dist == "normFAP")
      ggplot(data.frame(x=seq(0,300)),aes(x=x)) + 
      stat_function(fun=dnorm,geom = "line",size=1, col= "blue",args = list(mean=input$mun_s1, sd = input$sd_s1)) + 
      stat_function(fun=dnorm,geom = "line",size=1,col= "red", args = list(mean=input$mun_s2, sd = input$sd_s2)) +
      labs(title = "Distribution of 1st stage processing",
           x = "Time (ms)",
           y = "Density function") + 
      theme(aspect.ratio=0.75) +
      theme(plot.title = element_text(size=16, face="bold", hjust = 0.5,
                                      margin = margin(10, 0, 10, 0)))
      else {
        # check if given values make sense
        validate(
          need(input$max_s1 - input$min_s1 > 0,
               "Please check your input data for the first stimulus!"),
          need(input$max_s2 - input$min_s2 > 0, "Please check your input data for the second stimulus!")
        )
          ggplot(data.frame(x=seq(0,300)),aes(x=x)) + 
            stat_function(fun=dunif,geom = "line",size=1, col= "blue",args = list(min=input$min_s1, max = input$max_s1)) + 
            stat_function(fun=dunif,geom = "line",size=1,col= "red", args = list(min=input$min_s2, max = input$max_s2)) +
            labs(title = "Distribution of 1st stage processing",
                 x = "Time (ms)",
                 y = "Density function") + 
            theme(aspect.ratio=0.75) +
            theme(plot.title = element_text(size=16, face="bold", hjust = 0.5,
                                            margin = margin(10, 0, 10, 0)))
        
      }
    
    
  })
  
  
  
 
  
  ### --- PLOT THE REACTION TIME MEANS AS FUNCTION OF THE SOA ---
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
    # if the difference between the two stimuli falls into the time-window, integration occurs
    
    # matrix with logical entries indicating for each run 
    # whether integration takes place (1) or not (0)
    integration <- as.matrix((integration <= input$omega), mode = "integer")
    
    # bimodal simulation of the data:
    # first stage RT of the target stimulus, second stage processing time
    # and the amount of integration if integration takes place
    obs_bi <- matrix(0, 
                     nrow = n,
                     ncol = SOA)
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
         title ="Mean Reaction Times for the unimodal \nand bimodal task condition") +
    ylim(c(0,max))+
    theme(aspect.ratio=0.75)+
    theme(plot.title = element_text(size=16, face="bold", hjust = 0.5, 
     margin = margin(10, 0, 10, 0)))
  
  
  })
  
  ### PLOT THE PROBABILITY OF INTEGRATION ---
  # check the input data of the uniform distribution
  output$prob <- renderPlot({
    
    
    # same integration calculation as for the other plot
    for(i in 1:SOA){
      integration[,i] <- stage1_t() - (stage1_nt() + tau[i])
    }
    integration[integration <= 0] <- Inf
    integration <- as.matrix((integration <= input$omega), mode = "integer")
    
    # calculate the probability for each SOA that integration takes place
    prob_value <- vector(mode = "integer", length = SOA)
    for (i in 1:SOA){
      prob_value[i] <- sum(integration[,i]) / length(integration[,i]) 
      # probability of integration for all runs
    }
    
    # put the results into a data frame
    results <- data.frame(tau, prob_value)
    
    # plot the results
    if(input$dist == "uniFAP" && (input$max_s1 < input$min_s1 || input$max_s2 < input$min_s2))
      ggplot()  # plot nothing if it doesnt make sense

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
  
  
  output$dt1 <- renderDataTable({
    infile <- input$file1
    if(is.null(infile))
      return(NULL)
    read.csv(infile$datapath, header = TRUE)
    
  })
  
  ##################### PDF of article 
  
  output$frame <- renderUI({
    tags$iframe(src="http://jov.arvojournals.org/article.aspx?articleid=2193864.pdf", height=600, width=535)
  })

  ################### Generate the Simulation 

  # Simulation with FAP or RTP Paradigm
  sigma <- reactive({
      input$mu / 5
  })

  output$soa_input <- renderUI({
    if (input$dist2 == "expFAP"){
      default.soa <- "-200,-100,-50,0,50,100,200"
    } else if (input$dist2 == "expRSP"){
      default.soa <- "0,50,100,200"
    }
    textInput("soa.in","Stimulus onset asynchronies (SOAs, comma delimited)",
              default.soa)
  })

  soa <- eventReactive(input$sim_button, {
    if (input$dist2 == "expFAP"){
      soa <- as.numeric(unlist(strsplit(input$soa.in, ",")))
    } else if (input$dist2 == "expRSP"){
      soa <- as.numeric(unlist(strsplit(input$soa.in, ",")))
    }
  })

  dataset <- eventReactive(input$sim_button, {
    if (input$dist2 == "expFAP"){
      simulate.fap(soa=soa(), proc.A=input$proc.A, proc.V=input$proc.V,
                   mu=input$mu, sigma=sigma(), omega=input$sim.omega,
                   delta=input$sim.delta,
                   N=input$N)
    }
    else if (input$dist2 == "expRSP"){
      simulate.rtp(soa=soa(), proc.A=input$proc.A, proc.V=input$proc.V,
                   mu=input$mu, sigma=sigma(), omega=input$sim.omega,
                   delta=input$sim.delta,
                   N=input$N)
    }
  })

  output$simtable <- renderTable({
      head(dataset(), input$nrowShow)
    })

  output$simplot <- renderPlot({
      if (isolate(input$dist2) == "expFAP"){
        boxplot(dataset(), ylab="reaction time (ms)",
                xlab="stimulus-onset asynchrony (soa)", main="", xaxt="n")
        axis(1, at=1:length(soa()), labels=soa())
      } else if (isolate(input$dist2) == "expRSP"){
        par(mfrow=c(1,2))
        boxplot(dataset()[ , grep("aud", colnames(dataset()))],
                ylab="reaction time (ms)", xlab="stimulus-onset asynchrony (soa)",
                main="auditory target", xaxt="n")
        axis(1, at=1:length(soa()), labels=soa())
        boxplot(dataset()[ , grep("vis", colnames(dataset()))],
                ylab="reaction time (ms)", xlab="stimulus-onset asynchrony (soa)",
                main="visual target", xaxt="n")
        axis(1, at=1:length(soa()), labels=soa())
      }
  })



  ################### Download the Simulation output

  #output$table <- renderTable({
  #data
  #}) 

  # downloadHandler() takes two arguments, both functions.
  # The content function is passed a filename as an argument, and
  #   it should write out data to that filename.
  output$downloadData <- downloadHandler(

    # This function returns a string which tells the client
    # browser what name to use when saving the file.

    filename = function() {
      #paste(input$dataset, input$filetype, sep = ".")
      paste("dat-", Sys.Date(), ".csv", sep="")
    },
    
    # This function should write data to a file given to it by
    # the argument 'file'.
    
    
    content = function(file) {
      
      #sep <- switch(input$filetype, "csv" = ",", "tsv" = "\t")
      
      #write.table(data, file, sep = sep,
      # row.names = FALSE)
      
      
      write.csv(dataset(), file)
    }
  )

  ###################### ESTIMATION ###########################

  output$data_input <- renderUI({
      if (input$whichDataEst == "sim") {
          # actionButton("resim_button", "Re-simulate!")
      } else {
          fileInput('file1', 'Choose file to upload (not yet possible)',
          accept = c(
            'text/csv',
            'text/comma-separated-values',
            'text/tab-separated-values',
            'text/plain',
            '.csv',
            '.tsv'))
      }
  })

  # newdataset <- eventReactive(input$resim_button, {
  #   if (input$dist2 == "expFAP"){
  #     simulate.fap(soa=soa(), proc.A=input$proc.A, proc.V=input$proc.V,
  #                  mu=input$mu, sigma=sigma(), omega=input$sim.omega,
  #                  delta=input$sim.delta,
  #                  N=input$N)
  #   }
  #   else if (input$dist2 == "expRSP"){
  #     simulate.rtp(soa=soa(), proc.A=input$proc.A, proc.V=input$proc.V,
  #                  mu=input$mu, sigma=sigma(), omega=input$sim.omega,
  #                  delta=input$sim.delta,
  #                  N=input$N)
  #   }
  # })

  est.out <- eventReactive(input$est_button, {
                # if(is.null(newdataset())) {
                #     print("null")
                #     data <- dataset()
                # } else {
                #     print("not null")
                #     data <- newdataset()
                # }

                    switch(input$whichDataEst,
                           sim    = estimate.fap(dataset()),
                           upload = NULL)
  })

  output$estTextOut <- renderTable({
                    est <- matrix(est.out()$est$par, nrow=1)
                    dimnames(est) <- list(NULL, c("1/lambdaA", "1/lambdaV",
                                                  "mu", "omega", "delta"))
                    est
  })

  output$startParamTextOut <- renderTable({
                    par <- matrix(est.out()$param.start, nrow=1)
                    dimnames(par) <- list(NULL, c("1/lambdaA", "1/lambdaV", "mu",
                                                  "omega", "delta"))
                    par
  })

  output$plotEstPred <- renderPlot({
                plotEstPred(dataset(), est.out())
  })
})

