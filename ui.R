library(shiny)

# Define UI for miles per gallon application
shinyUI(fluidPage(
  # Application title
  
  
   fluidRow(
    column(6,
           img(src = "tquant100.png", align = "left"))),
   br(),
   headerPanel(h1("The Time-Window of INtegration Model (TWIN)", align = "center")),
  
  h4("Focused Attention Paradigm & Redundant Signals Paradigm", align = "center"),
  
  tabsetPanel(id = "TWINTabset",
              tabPanel("Parameters", value = "Para", 
              
  fluidRow(
    column(4,
           wellPanel(
             
             selectInput("dist", "Assumed Distribution: ",
                         choices = c("Focused Attention Paradigm" = "expFAP",
                                    "Redundant Signals Paradigm" = "expRSP")),
             
             conditionalPanel( condition = ("input.dist == 'expFAP'"),
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
  
  ################### Simulation tab
  
  tabPanel("Simulation", value = "Sim",
  fluidRow(
  column(12,
  
  sidebarLayout(
  sidebarPanel(
    
            
    #sliderInput("soa",
   #             "SOA:",
    #            min = 0,
     #           max = 500,
      #          value = 200),
    
    sliderInput("lambdaA",
                "Intensity of Auditory Stimuli:",
                min = 20,
                max = 150,
                value = 50),
    
    sliderInput("lambdaV",
                "Intensity of Visual Stimuli:",
                min = 20,
                max = 150,
                value = 50),
    
    sliderInput("mu",
                "Duration of 2nd stage:",
                min = 50,
                max = 200,
                value = 100),
    
   # sliderInput("sigma",
    #            "Standard Deviation:",
     #           min = 500,
      #          max = 200,
       #         value = 50),
           
    sliderInput("om",
                "Width of the window:",
                min = 100,
                max = 300,
                value = 200),
    
    sliderInput("del",
                "Amount of integration:",
                min = 20,
                max = 100,
                value = 50),
    
    sliderInput("N",
                "Trial Number:",
                min = 1,
                max = 200,
                value = 10),
   
   tags$style(HTML('#SimButton1{background-color:orange}')),
   actionButton("SimButton1", "Simulate"),
    
    radioButtons("filetype", "File type:",
                 choices = c("csv", "tsv")),
           downloadButton('downloadData', 'Download')
  ),
  mainPanel(
  tableOutput("simtable")
  )))
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
                    numericInput("n1", "Number Input:", min = 0, max = 1000, value = 50),
                    numericInput("n2", "Number Input:", min = 0, max = 1000, value = 50),
                    numericInput("n3", "Number Input:", min = 0, max = 1000, value = 50),
                    actionButton("AButton", "ActionButton"),
                    tags$style(HTML('#AButton{background-color:orange}')),
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
  p("Content goes here")
          )),
  dataTableOutput("dt1")),
  
  #Custom Colored Items
  tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: #000090}")),
  tags$style(HTML(".js-irs-1 .irs-single, .js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar {background: #000070}")),
  tags$style(HTML(".js-irs-2 .irs-single, .js-irs-2 .irs-bar-edge, .js-irs-2 .irs-bar {background: #000090}")),
  tags$style(HTML(".js-irs-3 .irs-single, .js-irs-3 .irs-bar-edge, .js-irs-3 .irs-bar {background: #000070}")),
  tags$style(HTML(".js-irs-4 .irs-single, .js-irs-4 .irs-bar-edge, .js-irs-4 .irs-bar {background: #000090}")),
  tags$style(HTML(".js-irs-5 .irs-single, .js-irs-5 .irs-bar-edge, .js-irs-5 .irs-bar {background: #000070}")), 
  tags$style(HTML(".js-irs-6 .irs-single, .js-irs-6 .irs-bar-edge, .js-irs-6 .irs-bar {background: #000090}")),
  tags$style(HTML(".js-irs-7 .irs-single, .js-irs-7 .irs-bar-edge, .js-irs-7 .irs-bar {background: #000070}")),
  tags$style(HTML(".js-irs-8 .irs-single, .js-irs-8 .irs-bar-edge, .js-irs-8 .irs-bar {background: #000070}")),
  tags$style(HTML(".js-irs-9 .irs-single, .js-irs-9 .irs-bar-edge, .js-irs-9 .irs-bar {background: #000070}")),
  tags$style(HTML(".js-irs-10 .irs-single, .js-irs-10 .irs-bar-edge, .js-irs-10 .irs-bar {background: #000070}"))
  
  

  ))
  
)
