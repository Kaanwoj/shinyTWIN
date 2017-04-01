library(shiny)

ui <- shinyUI(fluidPage(
    withMathJax(),
  # Application title
  fluidRow(
    column(6,
           img(src = "tquant100.png", align = "left"))),
  br(),
  headerPanel(h1("The Time-Window of INtegration Model (TWIN)", align = "center")),
  
  h4("Focused Attention Paradigm & Redundant Signals Paradigm", align = "center"),
  
  tabsetPanel(id = "TWINTabset",

              
              tabPanel("Introduction", value = "intro",

                       fluidRow(
                         column(4,
                                wellPanel(
                                  selectInput("topic", "Select the topic: ",
                                              choices = c("About this app" = "aboutapp",
                                                          "The TWIN model" = "twinmod",
                                                          "Parameters" = "param", "Model equations" = "equ"

                                                          )))),
                         
                         
             mainPanel(conditionalPanel (condition = ("input.topic == 'aboutapp'"),
                 p(h2("Welcome!", align = "center")), 
                 br(),
                 p(h5("This Shiny App helps you to learn more about the Time-Window Ingration Model (TWIN), developed by
                      Hans Colonius, Adele Diederich, and colleagues.", align = "center")), 

                 p(h5("It allows you to simulate and estimate the parameteres and even to upload your own data file.", align = "center")),
                 p(h5("Feel free to explore the app and navigate through the tabs!", align ="center")),
                 br(),
                 p(h5(strong("Based on the original Shiny App by Annika Thierfelder, this app was expanded by:"), align = "center")),
                 br(),
                 p(h5("Aditya Dandekar", img(src="bremen.png", width = "150"),
                 "Amalia Gomoiu", img(src="glasgow.png", width = "150"), align = "center")),
                 p(h5("António Fernandes", img(src="lisbon.png", width = "200"),
                 "Katharina Dücker", img(src="oldenburg.png", width = "110", height = "90"), align = "center")),
                 p(h5("Katharina Naumann", img(src="tubingen.png", width = "150"),
                 "Martin Ingram", img(src="glasgow.png", width = "150"), align = "center")),
                 p(h5("Melanie Spindler", img(src="oldenburg.png", width = "110", height = "90"),
                 "Silvia Lopes", img(src="lisbon.png", width = "200"), align = "center"))
                         ),
                         conditionalPanel (condition = ("input.topic == 'twinmod'"),
                                           p(h4("Crossmodal interaction is defined as the situation in which the
                                                perception of an event as measured in terms of one modality
                                                is changed in some way by the concurrent stimulation of one or
                                                more other sensory modalities (Welch & Warren, 1986).", align = "left")),
                                           p(h4("Multisensory integration is defined as the (neural) mechanism underlying 
                                                crossmodal interaction.", align = "left")),
                                           p(h4("The Time-Window of Integration Model (TWIN) postulates that a crossmodal
                                                stimulus triggers a race mechanism among the activations in very early, 
                                                peripheral sensory pathways. This first stage is followed by a compound
                                                stage of converging subprocesses that comprise neural integration of the
                                                input and preparation of a response. The second stage is defined by
                                                default: It includes all subsequent processes that are not part of the
                                                peripheral processes in the first stage (Diederich, Colonius, & Kandil, 2016).",
                                                align = "left")),
                                           p(img(src="modeltwin.png", width = "550"), align = "center"),
                                           p(h4("At the behavioral level, multisensory integration translates itself into 
                                                faster reaction times, higher detection probabilities, and improved discrimination.")),
                                           p(h4("At the neural level, multisensory integration translates itself into an increased total
                                                number of responses, as well as shorter response latencies.")),
                                           p(h4("In the Focused Attention Paradigm (FAP), two or more stimuli are presented
                                                simultaneously or with a short delay between the stimuli. 
                                                The participant is asked to respond as quickly as possible
                                                to a stimulus of a pre-defined modality (target) and ignore
                                                the other stimulus (non-target modality).")),
                                           p(img(src="fap.jpeg", width = "500", align = "center")),
                                           p(h4("In the Redundant Signals Paradigm (RSP), two or more stimuli
                                                are also presented simultanously or with a short delay between 
                                                the stimulus. However, the participant is asked to respond to
                                                a stimulus of any modality detected first.")),
                                           p(img(src="rtp.jpeg", width = "500", align = "center"))
                                           
                                           
                                           
                                           
                                           ),
                         
                         conditionalPanel (condition = ("input.topic == 'param'"),
                                           p(h4("Intensity of the auditory stimuli"), img(src="lamba.jpeg", width = "50"), align = "left"),
                                           p(h4("Intensity of the visual stimuli"), img(src="lambv.jpeg", width = "50"), align = "left"),
                                           p(h4("Processing time for the second stage"), img(src="u.jpeg", width = "40"), align = "left"),
                                           p(h4("Width of the time window of integration"), img(src="w.jpeg", width = "40"), align = "left"),
                                           p(h4("Effect size / Amount of integration"), img(src="delta.jpeg", width = "40"), align = "left")
                                           
                         ),
                         
                         conditionalPanel (condition = ("input.topic == 'equ'"),
                                           p(h4("Requirement for multisensory integration"), img(src="itw.jpeg", width = "300"), align = "left"),
                                           p(h4("Mean reaction times in the unimodal and bimodal conditions"), img(src="rts.jpeg", width = "300"), align = "left"),
                                           p(h4("Observable reaction time following the logic of the model"), img(src="rt.jpeg", width = "400"), align = "left"),
                                           p(h4("Objective function"), img(src="objfun.jpeg", width = "300"), align = "left"),
                                           br(),
                                           p(h4("Three cases for the probability of integration P(I)")),
                                           p(img(src="p1.jpeg", width = "400", align = "left")),
                                           p(img(src="p2.jpeg", width = "400", align = "left")),
                                           p(img(src="p3.jpeg", width = "400", align = "left"))
                                           
                         ),
                         
                         br(),
                         
                         fluidRow(column(8,align="left",
                                         a("Click to learn more", href="http://jov.arvojournals.org/article.aspx?articleid=2193864", target="_blank")
                         )             
                         )
                         
                         
                         
                                           )
                         
                         
                         
                                           )),
              tabPanel("Parameters", value = "Para", 
                       
                       fluidRow(
                         column(4,
                                wellPanel(
                  

                                  selectInput("dist", "Distribution ",
                                              choices = c("Exponential" = "expFAP",
                                                          " " = "expRSP")),
                                  
                                  conditionalPanel( condition = ("input.dist == 'expFAP'"),
                                                    sliderInput("mu_t",
                                                                "Mean (target / stimulus 1): ",
                                                                min = 1,
                                                                max = 100,
                                                                value = 50),
                                                    sliderInput("mu_nt",

                                                                 "Mean (non-target / stimulus 2): ",

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
                       #fluidRow(
                         #column(12,
                            sidebarLayout(
                                sidebarPanel(
                                selectInput("dist2", "Choose paradigm",
                                              choices = c("Focused Attention Paradigm" = "expFAP",
                                                         "Redundant Target Paradigm" = "expRSP")),
                                  #conditionalPanel( condition = ("input.dist == 'expFAP'"),
                                  #                  conditionalPanel( condition = ("input.dist == 'expRSP'"))),
                                #
                                h4("First stage"),
                                    sliderInput("proc.A",
                                                "Auditory processing time (\\(\\frac{1}{\\lambda_A}\\))",
                                                min = 20,
                                                max = 150,
                                                value = 100),
                                    
                                    sliderInput("proc.V",
                                                "Visual processing time (\\(\\frac{1}{\\lambda_V}\\))",
                                                min = 20,
                                                max = 150,
                                                value = 50),
                                    
                                  h4("Second stage"),
                                    sliderInput("mu",
                                              "... processing time (\\(\\mu\\))",
                                                min = 50,
                                                max = 500,
                                                value = 100),
                                    
                                    # sliderInput("sigma",
                                    #            "Standard Deviation:",
                                    #           min = 500,
                                    #          max = 200,
                                    #         value = 50),
                                    
                                    sliderInput("om",
                                              "Window width (\\(\\omega\\))",
                                                min = 100,
                                                max = 300,
                                                value = 200),
                                    
                                    sliderInput("del",
                                              "Amount of integration (\\(\\delta\\))",
                                                min = 20,
                                                max = 100,
                                                value = 50),
                                    
                                    sliderInput("N",
                                                "Trial Number:",
                                                min = 1,
                                                max = 1000,
                                                value = 500),
                                    
                                    downloadButton('downloadData', 'Download (CSV)')
                                    
                                    
                                  ),
                                  mainPanel(
                                    h4("Simulated Data"),
                                    numericInput("nrowShow",
                                                 "Number of rows displayed",
                                                 min=1,
                                                 max=60,
                                                 value=10),
                                    tableOutput("simtable"),
                                    plotOutput("simplot")
                                  ))
             #              )
             #          )
              ),
              
              tabPanel("Estimation", value = "Est",
                       fluidRow(
                         column(4,
                                wellPanel(
                                 # selectInput("estParadigm", "Chosen Paradigm:",
                                 #             choices = c("Focused Attention Paradigm" = "fap",
                                 #                         "Redundant Signal Paradigm" = "rsp")),
                                 # h4("First stage"),
                                 # sliderInput("est_procV",
                                 #             "Visual processing time (\\(\\frac{1}{\\lambda_V}\\))",
                                 #             min = 1,
                                 #             max = 100,
                                 #             value = 50),
                                 # sliderInput("est_procA",
                                 #             "Auditory processing time (\\(\\frac{1}{\\lambda_A}\\))",
                                 #             min = 1,
                                 #             max = 100,
                                 #             value = 50),
                                 # h4("Second stage"),
                                 # sliderInput("est_mu",
                                 #             "... processing time (\\(\\mu\\))",
                                 #             min = 100,
                                 #             max = 500,
                                 #             value = 50),
                                 # sliderInput("est_omega",
                                 #             "Window width (\\(\\omega\\))",
                                 #             min = 1,
                                 #             max = 100,
                                 #             value = 50),
                                 # sliderInput("est_delta",
                                 #             "Amount of integration (\\(\\delta\\))",
                                 #             min = 1,
                                 #             max = 100,
                                 #             value = 50),
                                 #actionButton("AButton", "ActionButton"),
                                 #tags$style(HTML('#AButton{background-color:orange}')),
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
                                h2("Estimated values"),
                                tableOutput("estTextOut")
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

























