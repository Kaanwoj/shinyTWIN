library(shiny)
library(plyr)


shinyServer(function(input, output) {
  
  # different stimulus onsets for the non-target stimulus
  tau <- c(-200 ,-150, -100, -50, -25, 0, 25, 50)
  SOA <- length(tau)
  n <- 1000 # number of simulated observations
  
  # unimodal simulation of the first stage according to the chosen distribution
  # target stimulus
  stage1_t <- reactive({ if(input$dist == "exp")(rexp(n, rate = 1/input$mu_t))
    else if(input$dist == "norm")(rnorm(n, mean = input$mun_t, sd = input$sd_t))
    else (runif(n, min = input$min_t, max = input$max_t))
  })
  # non-target stimulus
  stage1_nt <- reactive({ if(input$dist == "exp")(rexp(n, rate = 1/input$mu_nt))
    else if(input$dist == "norm")(rnorm(n, mean = input$mun_nt, sd = input$sd_nt))
    else (runif(n, min = input$min_nt, max =  input$max_nt))
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
    # exponential distribution with rate lambda = 1/mu
    if(input$dist == "exp")
      (plot(dexp(x, rate = 1/input$mu_t),
            type = "l",
            lwd=3,
            ylab = "Probability",
            xlab = "first stage (target stimulus)"))
    # normal distribution
    else if(input$dist == "norm")
      (plot(dnorm(x, mean = input$mun_t, sd = input$sd_t),
            type = "l",
            lwd=3,
            ylab = "Probability",
            xlab = "first stage (target stimulus)"))
    # uniform distribution
    else{
      # check if the given values make sense
      validate(
        need(input$max_t - input$min_t > 0, 
             "Please check your input data for the target!"))
      plot(dunif(x, min = input$min_t, max = input$max_t),
           type = "l",
           lwd=3,
           ylab = "Probability",
           xlab = "first stage (target stimulus")}
  })
  
  # do the same for the non-target distribution
  output$uni_data_nt <- renderPlot({
    if(input$dist == "exp")
      (plot(dexp(x, rate = 1/input$mu_nt),
            type = "l",
            lwd=3,
            ylab = "Probability",
            xlab = "first stage (non-target stimulus)"))
    else if(input$dist == "norm")
      (plot(dnorm(x, mean = input$mun_nt, sd = input$sd_nt),
            type = "l",
            lwd=3,
            ylab = "Probability",
            xlab = "first stage (non-target stimulus)"))
    else{
      validate(
        need(input$max_nt - input$min_nt > 0, 
             "Please check your input data for the non-target!"))
      plot(dunif(x, min = input$min_nt, max = input$max_nt),
           type = "l",
           lwd=3,
           ylab = "Probability",
           xlab = "first stage (non-target stimulus)")}
  })
  
  ### --- PLOT THE REACTION TIME MEANS AS FUNCTION OF THE SOA ---
  output$data <- renderPlot({
    # check if the input variables make sense for the uniform data
    if(input$dist == "uni"){
      validate(
        need(input$max_nt > input$min_nt && input$max_t > input$min_t, 
             "Please check your input data"))
    }
    
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
         xlab = "SOA of the non-target stimulus")
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
    if(input$dist == "uni"){
      validate(
        need(input$max_nt > input$min_nt && input$max_t > input$min_t, 
             "Please check your input data"))
    }
    
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
              xlab = "SOA of the non-target stimulus"))
  })
  
  output$dt1 <- renderDataTable({
    infile <- input$file1
    if(is.null(infile))
      return(NULL)
    read.csv(infile$datapath, header = TRUE)
    
  })
  

  output$frame <- renderUI({
    tags$iframe(src="http://jov.arvojournals.org/article.aspx?articleid=2193864.pdf", height=600, width=535)
  })
  
})
