library(shiny)
library(shinydashboard)

shinyUI(
  navbarPage(title = "TWIN Model", theme = "style.css", id = "page",
    tabPanel("Introduction", value = "intro",
      withMathJax(),
      h2("The Time-Window of INtegration Model (TWIN)", align = "center"),
      p("This Shiny App helps you to learn more about the Time-Window of
        Integration Model (TWIN), developed by Hans Colonius, Adele Diederich,
        and colleagues", align = "center",
        a("(Colonius & Diederich, 2004).",
          href="https://www.uni-oldenburg.de/fileadmin/user_upload/psycho/ag/kogn/colonius/Jcogn.pdf", target="_blank")),
      p("It allows you to simulate and estimate the model parameters either
        from virtual data or from your own datafile.", align = "center"),
      # define action buttons, using raw html. Redirecting doesnt work in CSS
        # because of reasons. ;)
      actionButton("parambutton",HTML("<strong>Parameters</strong><br><p> To
                                      play around and visualize <br> the model
                                      parameters</p>"),icon("area-chart"),
                                      style = "background-color: #5fdc5f",
                                      width="300px"),
      actionButton("simbutton", HTML("<strong>Simulation</strong> <br><p>To
                                     simulate virtual data using different <br>
                                     start parameters and SOAs</p>"),
                                     icon("dashboard"),
                                     style="background-color:
                                     #ed3f40",width="300px"),
      actionButton("estbutton", HTML("<strong>Estimation</strong> <br> <p>To
                                     estimate the parameters either from <br>
                                     previously created data (Simulation), <br>
                                     or your own data</p>"),
                                     icon("paper-plane"),
                                     style="background-color: #2f84ff",
                                     width="300px"),
        # adding footer: <div class="footer">Footer text</div>
        tags$div(class = "footer", tags$p("Contact: ---ADD EMAIL HERE---"),
                 a(icon("github"),"Github",
                   href ="https://github.com/Kaanwoj/shinyTWIN"))
    ),

    navbarMenu("About the Model",
        source(file.path("ui", "ui_About_FAP.R"), local = TRUE)$value,
        source(file.path("ui", "ui_About_RTP.R"), local = TRUE)$value
    ),
        tabPanel("Parameters", value = "Para",
            sidebarLayout(
                sidebarPanel(
                selectInput("dist", "Distribution ",
                      choices = c("Exponential" = "expFAP",
                                  "Normal" = "normFAP",
                                  "Uniform" = "uniFAP")),
            conditionalPanel(condition = ("input.dist == 'expFAP'"),
                        sliderInput("mu_nt", "Auditory processing time (\\(\\frac{1}{\\lambda_A}\\))",
                                    min = 1,
                                    max = 100,
                                    value = 50),
                        sliderInput("mu_t","Visual processing time (\\(\\frac{1}{\\lambda_V}\\))",
                                    min = 1,
                                    max = 100,
                                    value = 50)),
            conditionalPanel(condition = ("input.dist == 'expRSP'"),
                        sliderInput("mu_s1", "Mean (Stimulus 1):",
                                    min = 1,
                                    max = 100,
                                    value = 50),
                        sliderInput("mu_s2", "Mean (Stimulus 2):",
                                    min = 1,
                                    max = 100,
                                    value = 50)),
            conditionalPanel( condition = ("input.dist == 'normFAP'"),
                        sliderInput("mun_s1","Mean (Stimulus 1):",
                                    min = 1,
                                    max = 150,
                                    value = 50),
                        sliderInput("sd_s1", "Standard deviation (Stimulus 1):",
                                    min = 1, 
                                    max = 50,
                                    value = 25),
                        sliderInput("mun_s2","Mean (Stimulus 2): ",
                                    min = 1,
                                    max = 150,
                                    value = 50),
                        sliderInput("sd_s2","Standard Deviation (Stimulus 2):",
                                    min = 1, 
                                    max = 50,
                                    value = 25)),
            conditionalPanel( condition = ("input.dist == 'uniFAP'"),
                        sliderInput("min_s1","Minimum (Stimulus 1):",
                                    min = 1,
                                    max = 300,
                                    value = 50),
                        sliderInput("max_s1","Maximum (Stimulus 1):",
                                    min = 1,
                                    max = 300,
                                    value = 150),
                        sliderInput("min_s2","Minimum (Stimulus 2): ",
                                    min = 1,
                                    max = 300,
                                    value = 50),
                        sliderInput("max_s2","Maximum (Stimulus 2):",
                                    min = 1,
                                    max = 300,
                                    value = 150)),
                        sliderInput("mu_second","2nd stage processing time (\\(\\mu\\))",
                                    min = 100,
                                    max = 500,
                                    value = 200),
                        sliderInput("sd_second","2nd stage standard deviation",
                                    min = 0,
                                    max = 100,
                                    value = 50),
                        sliderInput("delta","Amount of integration (\\(\\delta\\))",
                                    min = -300,
                                    max = 300,
                                    value = 100),
                        sliderInput("omega","Window width (\\(\\omega\\))",
                                    min = 0,
                                    max = 500,
                                    value = 200)),
                              mainPanel(
                                fluidRow(
                                  column(6,
                                         plotOutput("uni_data_t"),
                                         plotOutput("prob")),
                                  column(6,
                                         plotOutput("data"),
                                         ########### Parameters Tab Description #########
                                         br(),
                                         tags$div(class = "help-tip", tags$p("This tab shows you the different dependencies of the model parameters.")),
                                         # <div class="help-tip">
                                         # <p>Help tip text blabla.</p>
                                         # </div>)
                                         br(),
                                         strong("1)"),
                                         p("The topleft graph shows the exponential distribution of the intensity measure lambda for both", 
                                           span("visual (target)", style = "color:red"), "and", span("auditory (non-target) stimuli.", style = "color:blue")),
                                         br(),
                                         strong("2)"),
                                         p("The topright graph shows the mean RTs of the different SOAs for both the", span("unimodal", style = "color:blue"), "and", span("bimodal", style = "color:red"), "task condition. \n
                                           A decrease in RTs in the bimodal task condition compared to the unimodal condition implies fascilitation."),
                                         br(),
                                         strong("3)"),
                                         p("The bottom graph displays the probability of integration depending on the length of the SOA.")
                                         ))))),
                   
                   tabPanel("Simulation", value = "Sim",
                            sidebarLayout(
                              sidebarPanel(
                                selectInput("dist2", h4("1. Choose paradigm"),
                                            choices = c("Focused Attention Paradigm" = "expFAP","Redundant Target Paradigm" = "expRSP")),
                                h4("2. Set parameter values"),
                                uiOutput("soa_input"),
                                sliderInput("N","Number of trials:",
                                            min = 1, max = 1000, value = 500),
                                h4("First stage"),
                                sliderInput("proc.A","Auditory processing time (\\(\\frac{1}{\\lambda_A}\\))",
                                            min = 20, max = 150, value = 100),
                                sliderInput("proc.V","Visual processing time (\\(\\frac{1}{\\lambda_V}\\))",
                                            min = 20, max = 150, value = 50),
                                h4("Second stage"),
                                sliderInput("mu","... processing time (\\(\\mu\\))",
                                            min = 100, max = 500, value = 200),
                                p("The standard deviation of the second stage processing time is
                                  fixed to \\(^\\mu/_5\\)."),
                                #sliderInput("sigma",
                                #            "Standard Deviation",
                                #            min = 500, max = 200, value = 50),
                                sliderInput("sim.omega","Window width (\\(\\omega\\))",
                                            min = 100, max = 300, value = 200),
                                sliderInput("sim.delta","Amount of integration (\\(\\delta\\))",
                                            min = 20, max = 100, value = 50)),
                              mainPanel(
                                fluidRow(
                                  column(3,
                                         h4("3. Simulate data"),
                                         tags$div(class = "help-tip", tags$p("To simulate your own data, first specify which paradigm
                                                                      and parameter values you want to use. In order to apply your changes and start 
                                                                      the simulation, press Simulate!")),
                                         actionButton("sim_button", "Simulate!")),
                                  column(9,
                                         h4("4. To download your simulated data, press on the button below"),
                                         downloadButton('downloadData', 'Download (.csv)'))),
                                h3("Simulated Data"),
                                h4("Boxplots of reaction times for each SOA"),
                                plotOutput("simplot"),
                                h4("Table of reaction times for each SOA"),
                                numericInput("nrowShow","Number of rows displayed",
                                             min=1, max=60, value=10),
                                tableOutput("simtable")
                              ))),

                   ######################
                   ### Estimation Tab ###
                   ######################

                   tabPanel("Estimation", value = "Est",
                            sidebarLayout(
                              sidebarPanel(
                                radioButtons("whichDataEst", h4("1. Choose data for parameter estimation"),
                                             c("Simulated data (from tab before)" = "sim",
                                               "Upload data" = "upload")),
                                uiOutput("data_input"),
                                a("See publication on TWIN estimation (Kandil, Diederich & Colonius,
                                  2014)",
                                  href="http://jov.arvojournals.org/article.aspx?articleid=2193864",
                                  target="_blank")),
                              mainPanel(
                                h4("2. Estimate parameters"),
                                actionButton("est_button", "Estimate!"),
                                         h4("Parameter values"),
                                         tableOutput("estTextOut"),
                                         dataTableOutput("dt1"),
                                         h4("Predicted and observed reaction times"),
                                         plotOutput("plotEstPred")
                              ))),

        source(file.path("ui", "ui_Team.R"), local = TRUE)$value,
                   
                   # Custom Colored Items
                   tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0
                                   .irs-bar {background: #000090}")),
                   tags$style(HTML(".js-irs-1 .irs-single, .js-irs-1 .irs-bar-edge, .js-irs-1
                                   .irs-bar {background: #000070}")),
                   tags$style(HTML(".js-irs-2 .irs-single, .js-irs-2 .irs-bar-edge, .js-irs-2
                                   .irs-bar {background: #000090}")),
                   tags$style(HTML(".js-irs-3 .irs-single, .js-irs-3 .irs-bar-edge, .js-irs-3
                                   .irs-bar {background: #000070}")),
                   tags$style(HTML(".js-irs-4 .irs-single, .js-irs-4 .irs-bar-edge, .js-irs-4
                                   .irs-bar {background: #000090}")),
                   tags$style(HTML(".js-irs-5 .irs-single, .js-irs-5 .irs-bar-edge, .js-irs-5
                                   .irs-bar {background: #000070}")),
                   tags$style(HTML(".js-irs-6 .irs-single, .js-irs-6 .irs-bar-edge, .js-irs-6
                                   .irs-bar {background: #000090}")),
                   tags$style(HTML(".js-irs-7 .irs-single, .js-irs-7 .irs-bar-edge, .js-irs-7
                                   .irs-bar {background: #000070}")),
                   tags$style(HTML(".js-irs-8 .irs-single, .js-irs-8 .irs-bar-edge, .js-irs-8
                                   .irs-bar {background: #000070}")),
                   tags$style(HTML(".js-irs-9 .irs-single, .js-irs-9 .irs-bar-edge, .js-irs-9
                                   .irs-bar {background: #000070}")),
                   tags$style(HTML(".js-irs-10 .irs-single, .js-irs-10 .irs-bar-edge,
                                   .js-irs-10 .irs-bar {background: #000070}"))
                   
                   
                   
                   ))
