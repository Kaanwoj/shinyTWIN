library(shiny)

# Define UI for miles per gallon application
shinyUI(fluidPage(
  # Application title
  
  #Custom Colored Sliderbars
  tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: #000090}")),
  tags$style(HTML(".js-irs-1 .irs-single, .js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar {background: #000070}")),
   fluidRow(
    column(6,
           img(src = "tquant100.png", align = "left"))),
   br(),
   
   headerPanel(h1("The Time-Window of INtegration Model (TWIN)", align = "center")),
  
  h4("Focused Attention Paradigm & Redundant Signals Paradigm", align = "center"),
  
  
  tabsetPanel(id = "TWINTabset",
              tabPanel("Simulation", value = "Sim", 
  fluidRow(
    column(4,
           wellPanel(
             
             selectInput("dist", "Assumed Distribution: ",
                         choices = c("Exponential FAP" = "exp",
                                     "Normal FAP" = "norm",
                                     "Uniform FAP" = "uni",
                                     "Exponential RSP" = "expRSP",
                                     "Normal RSP" = "normRSP",
                                     "Uniform RSP" = "uniRSP")),
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
             conditionalPanel( condition = ("input.dist == 'expRSP'"),
                               sliderInput("mu_s1",
                                           "Mean (Stimulus 1): ",
                                           min = 1,
                                           max = 100,
                                           value = 50),
                               sliderInput("mu_s2",
                                           "Mean (Stimulus 2): ",
                                           min = 1,
                                           max = 100,
                                           value = 50)
             ),
             conditionalPanel( condition = ("input.dist == 'normRSP'"),
                               sliderInput("mun_s1",
                                           "Mean (Stimulus 1): ",
                                           min = 1,
                                           max = 150,
                                           value = 50),
                               sliderInput("sd_s1",
                                           "Standard deviation (Stimulus 1):",
                                           min = 1, 
                                           max = 50,
                                           value = 25),
                               sliderInput("mun_s2",
                                           "Mean (Stimulus 2): ",
                                           min = 1,
                                           max = 150,
                                           value = 50),
                               sliderInput("sd_s2",
                                           "Standard Deviation (Stimulus 2):",
                                           min = 1, 
                                           max = 50,
                                           value = 25)
             ),
             conditionalPanel( condition = ("input.dist == 'uniRSP'"),
                               sliderInput("min_s1",
                                           "Minimum (Stimulus 1): ",
                                           min = 1,
                                           max = 300,
                                           value = 50),
                               sliderInput("max_s1",
                                           "Maximum (Stimulus 1):",
                                           min = 1,
                                           max = 300,
                                           value = 150),
                               sliderInput("min_s2",
                                           "Minimum (Stimulus 2): ",
                                           min = 1,
                                           max = 300,
                                           value = 50),
                               sliderInput("max_s2",
                                           "Maximum (Stimulus 2):",
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
                                choices = c("Exponential FAP" = "exp",
                                            "Normal FAP" = "norm",
                                            "Uniform FAP" = "uni",
                                            "Exponential RSP" = "expRSP",
                                            "Normal RSP" = "normRSP",
                                            "Uniform RSP" = "uniRSP")),
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
                    fileInput('file1', 'Choose file to upload',
                              accept = c(
                                'text/csv',
                                'text/comma-separated-values',
                                'text/tab-separated-values',
                                'text/plain',
                                '.csv',
                                '.tsv'
                              )
                    ), 
                  
                    fluidRow(column(8,align="left",
                                    a("Click to learn more", href="http://jov.arvojournals.org/article.aspx?articleid=2193864", target="_blank")
                    )             
                    )
               
        
                   
           )
           
  ),
  column(8,
  p("Content goes here"), 
  numericInput("n", "Number Input:", min = 0, max = 1000, value = 50),
  br(),
  actionButton("AButton", "ActionButton"),
  p("Click the button to engage action.")
          )),
  dataTableOutput("dt1"))
  
  

  ))
  
)
