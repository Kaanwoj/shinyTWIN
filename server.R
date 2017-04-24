library(ggplot2)
library(plyr)
source("simulateFAP.R")
source("estimateFAP.R")
source("simulateRTP.R")
library(xtable)

server <- shinyServer(function(input, output) {
  
  # different stimulus onsets for the non-target stimulus
  tau <- c(-200 ,-150, -100, -50, -25, 0, 25, 50)
  SOA <- length(tau)
  n <- 1000 # number of simulated observations
  
  
  # unimodal simulation of the first stage according to the chosen distribution
  # target stimulus
  stage1_t <- reactive({ if(input$dist == "expFAP")(rexp(n, rate = 1/input$mu_t))
    else (input$dist == "expRSP")(rexp(n, rate = 1/input$mu_t))
  })
  
  # non-target stimulus
  stage1_nt <- reactive({ if(input$dist == "expFAP")(rexp(n, rate = 1/input$mu_nt))
    else (input$dist == "expRSP")(rexp(n, rate = 1/input$mu_nt))
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

    ggplot(data.frame(x=seq(0,300)),aes(x=x)) + 
      stat_function(fun=dexp,geom = "line",size=1, col= "blue",args = (mean=1/input$mu_nt)) + 
      stat_function(fun=dexp,geom = "line",size=1,col= "red", args = (mean=1/input$mu_t)) +
     labs(title = "Distribution of 1st stage processing",
           x = "Time (ms)",
           y = "Density function") + 
    theme(plot.title = element_text(size=16, face="bold", hjust = 0.5,
           margin = margin(10, 0, 10, 0)))
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
    if(input$dist == "unif" && (input$max_t < input$min_t || input$max_nt < input$min_nt))
      (ggplot())
    else(
      ggplot(data= results, aes(x=tau, y=prob_value)) + geom_line(size=1, color= "blue")
        + labs(title="Integration function",
             x = "SOA",
             y = "Probability of integration")
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
  
  # Simulation with FAP Paradigm
  sigma <- 25

  
  soa <- reactive({
    if (input$dist2 == "expFAP"){
      soa <- as.numeric(unlist(strsplit(input$soa.in, ",")))  ### added 
     # soa <- c(-200,-100,-50,0,50,100,200)
    }
     else if (input$dist2 == "expRSP"){
       soa <- rep(as.numeric(unlist(strsplit(input$soa.in, ","))),2)  ### added
      #soa <- rep(c(0,50,100,200), 2)
     }
  
  })
  
   dataset <- reactive({
    if (input$dist2 == "expFAP"){
      soa <- as.numeric(unlist(strsplit(input$soa.in, ",")))  ### added
      #soa <- c(-200,-100,-50,0,50,100,200)
      simulate.fap(soa=soa, proc.A=input$proc.A, proc.V=input$proc.V,
                   mu=input$mu, sigma=sigma, omega=input$sim.omega,
                   delta=input$sim.delta,
                   N=input$N)
    }
    else if (input$dist2 == "expRSP"){
      soa <- rep(as.numeric(unlist(strsplit(input$soa.in, ","))),2) ### added
      #soa <- c(0,50,100,200)
      simulate.rtp(soa=soa, proc.A=input$proc.A, proc.V=input$proc.V,
                   mu=input$mu, sigma=sigma, omega=input$sim.omega,
                   delta=input$sim.delta,
                   N=input$N)
     }
  })


  output$simtable <- renderTable(head(dataset(), input$nrowShow))

  output$simplot <- renderPlot({
      boxplot(dataset(), ylab="Reaction time (ms)", xlab="Stimulus-onset asynchrony (SOA)",
         main="Distribution of reaction times for each SOA", xaxt="n")
      axis(1, at=1:length(soa()), labels=soa())
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
  est.out <- reactive(estimate.fap(dataset()))

  output$estTextOut <- renderTable({
                    est <- matrix(est.out()$est$par, nrow=1)
                    dimnames(est) <- list(NULL, c("1/lambdaA", "1/lambdaV", "mu",
                                                  "omega", "delta"))
                    est
  })

  output$startParamTextOut <- renderTable({
                    par <- matrix(est.out()$param.start, nrow=1)
                    dimnames(par) <- list(NULL, c("1/lambdaA", "1/lambdaV", "mu",
                                                  "omega", "delta"))
                    par
  })
})

