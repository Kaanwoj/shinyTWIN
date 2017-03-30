library(shiny)

# Define UI for miles per gallon application
shinyUI(fluidPage(
  
  # Application title
  headerPanel("The Time-Window of INtegration Model (TWIN)"),
  
  tabsetPanel(id = "TWINTabset",
              tabPanel("Simulation", value = "Sim", 
  fluidRow(
    column(4,
           wellPanel(
             
             selectInput("dist", "Assumed Distribution: ",
                         choices = c("exponential" = "exp",
                                     "normal" = "norm",
                                     "uniform" = "uni")),
             conditionalPanel( condition = ("input.dist == 'exp'"),
                               sliderInput("mu_t",
                                           "Mean (target): ",
                                           min = 1,
                                           max = 100,
                                           value = 50),
                               sliderInput("mu_nt",
                                           "Mean (non-target): ",
                                           min = 1,
                                           max = 100,
                                           value = 50)
             ),
             conditionalPanel( condition = ("input.dist == 'norm'"),
                               sliderInput("mun_t",
                                           "Mean (target): ",
                                           min = 1,
                                           max = 150,
                                           value = 50),
                               sliderInput("sd_t",
                                           "Standard deviation (target):",
                                           min = 1, 
                                           max = 50,
                                           value = 25),
                               sliderInput("mun_nt",
                                           "Mean (non-target): ",
                                           min = 1,
                                           max = 150,
                                           value = 50),
                               sliderInput("sd_nt",
                                           "Standard Deviation (non-target):",
                                           min = 1, 
                                           max = 50,
                                           value = 25)
             ),
             conditionalPanel( condition = ("input.dist == 'uni'"),
                               sliderInput("min_t",
                                           "Minimum (target): ",
                                           min = 1,
                                           max = 300,
                                           value = 50),
                               sliderInput("max_t",
                                           "Maximum (target):",
                                           min = 1,
                                           max = 300,
                                           value = 150),
                               sliderInput("min_nt",
                                           "Minimum (non-target): ",
                                           min = 1,
                                           max = 300,
                                           value = 50),
                               sliderInput("max_nt",
                                           "Maximum (non-target):",
                                           min = 1,
                                           max = 300,
                                           value = 150)
             ),
             sliderInput("mu_second",
                         "2nd stage processing time: ",
                         min = 100,
                         max = 500,
                         value = 200),
             sliderInput("sd_second",
                         "2nd stage standard deviation:",
                         min = 0,
                         max = 100,
                         value = 50),
             sliderInput("delta",
                         "Amount of Integration: ",
                         min = -300,
                         max = 300,
                         value = 100),
             sliderInput("omega",
                         "Width of the time-window:",
                         min = 0,
                         max = 500,
                         value = 200)
           )
    ),

    column(4,
           plotOutput("uni_data_t"),
           plotOutput("uni_data_nt")),
    
    column(4,
           plotOutput("data"),
           plotOutput("prob")
    )
  )),
  tabPanel("Estimation", value = "Est",
          fluidRow(
          column(4,
           wellPanel(
                    selectInput("dist2", "Assumed Distribution: ",
                                choices = c("exponential" = "exp",
                                            "normal" = "norm",
                                            "uniform" = "uni")),
                    sliderInput("SlidE",
                                "Slider Example1",
                                min = 1,
                                max = 100,
                                value = 50),
                    sliderInput("SlidE2",
                                "Slider Example2",
                                min = 1,
                                max = 100,
                                value = 50),
                    sliderInput("SlidE3",
                                "Slider Example3",
                                min = 1,
                                max = 100,
                                value = 50),
                    p("Text goes here"), 
                    fileInput('file1', 'Choose file to upload',
                              accept = c(
                                'text/csv',
                                'text/comma-separated-values',
                                'text/tab-separated-values',
                                'text/plain',
                                '.csv',
                                '.tsv'
                              )
                    )
                   
           )
           
  )
  ))
  
)))
