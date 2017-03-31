library(shiny)
library(plyr)
source("simulateFAP.R")
library(xtable)

shinyServer(function(input, output) {
  
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
  
  ## --- PLOT THE DISTRIBUTION OF THE FIRST STAGE RT
  output$uni_data_t <- renderPlot({
    # exponential distribution with rate lambda = 1/mu for FAP
    if(input$dist == "expFAP")
      (plot(dexp(x, rate = 1/input$mu_t),
            type = "l",
            lwd=3,
            ylab = "Density Function",
            xlab = "first stage (target stimulus)"))
    # exp distribution for RSP
    else (input$dist == "expRSP")
    (plot(dexp(x, rate = 1/input$mu_t),
          type = "l",
          lwd=3,
          ylab = "Density Function",
          xlab = "first stage (target stimulus)"))
  })
  
  # do the same for the non-target distribution
  output$uni_data_nt <- renderPlot({
    if(input$dist == "expFAP")
      (plot(dexp(x, rate = 1/input$mu_nt),
            type = "l",
            lwd=3,
            ylab = "Density Function",
            xlab = "first stage (non-target stimulus)"))
    
    else (input$dist == "expRSP")
    (plot(dexp(x, rate = 1/input$mu_nt),
          type = "l",
          lwd=3,
          ylab = "Density Function",
          xlab = "first stage (non-target stimulus)"))
    
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
    plot(results$tau, results$means, 
         type = "b", col = "red", 
         lwd=3,
         ylim=c(0, max),
         ylab = "Reaction Times",
         xlab = "SOA"
         #legend('topright',)
         )
    par(new = T)
    plot(results$tau, results$mean_t, 
         type = "l", col = "blue", 
         lwd=3,
         ylim = c(0, max),
         ylab = " ",
         xlab = " ")
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
      (plot())
    else(plot(results$tau, 
              results$prob_value, 
              type = "b", 
              lwd=3,
              col = "blue",
              ylab = "Probability of Integration",
              xlab = "SOA"))
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
  
  soa <- c(-200, -100, -50, 0, 50, 100, 200)
  sigma <- 25
 
   output$simtable <- renderTable({
    
    simulate.fap (soa, input$proc.A, input$proc.V, input$mu, sigma, input$om, input$del, input$N)
   
 
  })
  

  ################### Download the Simulation output 
   
   
   
   output$table <- renderTable({
     data
   })
   
   # downloadHandler() takes two arguments, both functions.
   # The content function is passed a filename as an argument, and
   #   it should write out data to that filename.
   output$downloadData <- downloadHandler(
     
     # This function returns a string which tells the client
     # browser what name to use when saving the file.
     filename = function() {
       paste(input$dataset, input$filetype, sep = ".")
     },
     
     # This function should write data to a file given to it by
     # the argument 'file'.
     content = function(file) {
       sep <- switch(input$filetype, "csv" = ",", "tsv" = "\t")
       
       # Write to a file specified by the 'file' argument
       write.table(data, file, sep = sep,
                   row.names = FALSE)
     }
   )
  
})
