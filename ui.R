library(shiny)
library(shinydashboard)

shinyUI(
  navbarPage(title = "TWIN Model", theme = "style.css", id = "page",
                   tabPanel("Introduction", value = "intro",
                            withMathJax(), h2("The Time-Window of INtegration Model (TWIN)", align = "center"),
                            p("This Shiny App helps you to learn more about the Time-Window of Integration Model (TWIN), developed by Hans Colonius, Adele Diederich, 
                              and colleagues", align = "center",
                              a("(Colonius & Diederich, 2004).",
                                href="https://www.uni-oldenburg.de/fileadmin/user_upload/psycho/ag/kogn/colonius/Jcogn.pdf", target="_blank")),
                            p("It allows you to simulate and estimate the model parameters either from 
                              virtual data or from your own datafile.", align = "center"),
                            # define action buttons, using raw html. Redirecting doesnt work in CSS because of reasons. ;)
                            actionButton("parambutton",HTML("<strong>Parameters</strong><br><p> 
                                          To play around and visualize <br> the model parameters</p>"),icon("area-chart"),
                                           style = "background-color: #5fdc5f", width="300px"),
                            actionButton("simbutton", HTML("<strong>Simulation</strong> <br><p>To simulate virtual data using different <br> start parameters and SOAs</p>"), icon("dashboard"),
                                         style="background-color: #ed3f40",width="300px"),
                            actionButton("estbutton", HTML("<strong>Estimation</strong> <br> <p>To estimate the parameters either from <br> previously created data (Simulation), <br> or your own data</p>"), icon("paper-plane"),
                                         style="background-color: #2f84ff", width="300px"),
                              # adding footer: <div class="footer">Footer text</div>
                              tags$div(class = "footer", tags$p("Contact: ---ADD EMAIL HERE---"), a(icon("github"),"Github", href ="https://github.com/Kaanwoj/shinyTWIN"))
                         ),
                   
    navbarMenu("About the Model",
        tabPanel("Focused Attention Paradigm (FAP)", value = "fap",
        h2("The Focused Attention Paradigm (FAP)"),
        withMathJax(p("In the Focused Attention Paradigm, one stimulus modality is pre-defined as the target stimulus to respond to. The other stimulus is called the non-target stimulus. 
                      In this experiment, we assume that the visual stimulus is the target modality, and the auditory stimulus is the non-target modality.
                      Let \\(I\\) denote the event that multisensory integration occurs. For FAP, the condition for multisensory integration is defined as  $$I_{FAP} = {A + \\tau < V < A + \\tau + \\omega}$$ with the probability of integration \\(P(I)\\).
                      Here, \\(\\tau\\) denotes the stimulus-onset asynchrony (SOA), which is determined by the experimental setup, and \\(\\omega\\) as the window width of integration, the time window, in which integration may occur.
                      A positive \\(\\tau\\) value thus indicates that that the visual stimulus is presented before the auditory, and a negative \\(\\tau\\) value indicates the reverse presentation order. In theory, \\(V\\) is always presented at \\(t = 0\\)."),
        h4("The Probability of Integration"),
        p("The probability of integration can be described as a function of SOA and window width:  
          Seeing that the peripheral processing time for the visual (\\(V\\)) and auditory (\\(A\\)) stimulus are assumed to be exponentially distributed and statistically independent (as they refer to very early sensory processing), 
          with parameters \\(\\lambda_V\\) and \\(\\lambda_A\\) and expected values of \\(1/\\lambda_V\\) and \\(1/\\lambda_A\\),
          $$\\begin{align}
          f_V(t) = \\lambda_{V}e^{-\\lambda_{V}t} \\\\
          f_A(t) = \\lambda_{A}e^{-\\lambda_{A}t}
          \\end{align}$$
          for \\(t \\geq 0\\), and \\(f_V(t) = f_A(t) \\equiv 0\\) for \\(t < 0.\\) The corresponding distribution functions are \\(F_V(t)\\) and  \\(F_A(t)\\), respectively. 
          The model can therefore be rewritten as:"),
        p("$$\\begin{align}
          P(I_{\\tau\\omega}) &= Pr(A + \\tau < V < A + \\tau + \\omega) \\\\
          &= \\int_{0}^{\\infty} \\! f_A(x)\\{F_V(x + \\tau + \\omega) \\\\ 
          &- F_V(x+\\tau)\\}\\,\\mathrm{d}x.
          \\end{align}$$"),
        p("Thus, the integration function for \\(P(I)\\) depends on the three cases for the sign of \\(\\omega\\) + \\(\\tau\\):"),
        p("(i) \\(\\tau\\) < \\(\\tau\\) + \\(\\omega\\) < 0", align = "center",
          "$$\\begin{align}
          P(I_{\\tau,\\omega}) &= \\int_{-\\tau-\\omega}^{-\\tau} \\! \\lambda_{A}e^{-\\lambda_{A}x}{1-e^{-\\lambda_V(x+\\tau+\\omega)}} \\, \\mathrm{d}x 
          -\\int_{-\\tau}^{\\infty} \\! \\lambda_{A}e^{-\\lambda_{A}x}{e^{-\\lambda_V(x+\\tau)} - e^{-\\lambda_V(x+\\tau+\\omega)}} \\, \\mathrm{d}x \\\\
          &= \\frac{\\lambda_V}{\\lambda_V+\\lambda_A} e^{\\lambda_A\\tau}(-1 + e^{\\lambda_A\\omega});
          \\end{align}$$"),
        p("(ii)  \\(\\tau\\) < 0 < \\(\\tau\\) + \\(\\omega\\)", align = "center"),
        p("$$\\begin{align}
          P(I_{\\tau,\\omega})  &= \\int_{0}^{-\\tau} \\! \\lambda_{A}e^{-\\lambda_{A}x}\\{1-e^{-\\lambda_V(x+\\tau+\\omega)}\\} \\, \\mathrm{d}x
          + \\int_{-\\tau}^{\\infty} \\! \\lambda_{A}e^{-\\lambda_{A}x}\\{e^{-\\lambda_V(x+\\tau)} - e^{-\\lambda_V(x+\\tau+\\omega)}\\} \\, \\mathrm{d}x \\\\
          &= \\frac{1}{\\lambda_V+\\lambda_A} \\{\\lambda_A(1-e^{-\\lambda_V(\\omega+\\tau)})+ \\lambda_V(1-e^{\\lambda_A\\tau})\\};
          \\end{align}$$"),
        p("(iii) \\(0 < \\tau < \\tau + \\omega\\)", align = "center"),
        p("$$\\begin{align}
          P(I_{\\tau,\\omega}) &= \\int_{0}^{\\infty} \\! \\lambda_{A}e^{-\\lambda_{A}x}\\{e^{-\\lambda_V(x+\\tau)}-e^{-\\lambda_V(x+\\tau+\\omega)}\\} \\mathrm{d}x \\\\
          &= \\frac{\\lambda_A}{\\lambda_V+\\lambda_A} \\{e^{-\\lambda_V(\\tau)}-e^{-\\lambda_V(\\omega+\\tau)}.
          \\end{align}$$"),
        h4("Reaction time analysis"),
        p("Reaction times are evaluated for the unimodal condition to compare against the crossmodal condition.
          Let \\(M1\\) and \\(M2\\) denote the random processing time for stage 1 and 2, so that the overall processing time of the crossmodal reaction time becomes
          $$RT_{VA} = M1 + M2$$.
          Expected reaction times for the unimodal condition: 
          $$\\begin{align}
          E[RT_V] = \\tfrac{1}{\\lambda_V} + \\mu 
          \\end{align}$$
          Expected Reaction times for the crossmodal condition:
          $$\\begin{align}
          E[RT_{VA}] = \\tfrac{1}{\\lambda_V} + \\mu - P(I) \\cdot \\Delta 
          \\end{align}$$
          where \\(\\mu\\) is the mean processing time of stage two."),
        h4("The Objective Function"),
        p("For the estimation, the parameters were generated by minimizing the \\(\\chi^2\\)  statistic:
          $$\\begin{align}
          \\chi^2 = \\sum_{\\text{all conditions}}[\\frac{\\text{mean}[RT_{gen}]-[RT_{pred}]}{\\text{standard error}[RT_{gen}]}]^2
          \\end{align}$$
          Thereby, the following boundaries were set for the parameters:")
    )),
                              
    tabPanel("Redundant Signals Paradigm (RSP)", value = "rsp",
        h2("The Redundant Target Paradigm (RTP)"),
        withMathJax(p("In the Redundant Target/ Signals Paradigm, the participant is instructed to respond to the first stimulus, regardless of the modality.  "),
        p("For RTP, the condition for multisensory integration $$I_{RTP} = {max(V,A + \\tau) < min(V,A + \\tau) + \\omega}$$ 
        with the probability of Integration \\(P(I)\\) holds"),
        h4("Probability of Integration"),
            tags$ol(tags$li(
                  p("The Probalility of Integration for cases, in which the", strong("visual"), "stimulus is presented first (e.g., SOAs of 0, 50 and 100 ms) depending on the 
                    signs of \\(\\omega\\) + \\(\\tau\\):"),
                  p("(i) ", align = "center"),
                  p("$$\\begin{align}
                    P(I) = 
                    \\end{align}$$"),
                  p("(ii)", align = "center"),
                  p("$$\\begin{align}
                    P(I) = 
                    \\end{align}$$"),
                  p("(iii)", align = "center"),
                  p("$$\\begin{align}
                    P(I) = 
                    \\end{align}$$")),
            tags$li(   
                  p("The Probalility of Integration for cases, in which the", strong("auditory"), "stimulus is presented first (e.g., SOAs of -100, -50 and 0 ms):"),
                  p("(i) ", align = "center"),
                  p("$$\\begin{align}
                    P(I) = 
                    \\end{align}$$"),
                  p("(ii)", align = "center"),
                  p("$$\\begin{align}
                    P(I) = 
                    \\end{align}$$"),
                  p("(iii)", align = "center"),
                  p("$$\\begin{align}
                    P(I) = 
                    \\end{align}$$")))
          ))),
                   
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
                                selectInput("paradigm.est", h4("1. Choose paradigm"),
                                            choices = c("Focused Attention Paradigm" = "expFAP",
                                                        "Redundant Target Paradigm" = "expRSP")),
                                radioButtons("whichDataEst",h4("2. Choose data for parameter estimation"),
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
                                fluidRow(
                                  column(5,
                                         h4("Parameter values"),
                                         tableOutput("estTextOut"),
                                         dataTableOutput("dt1")),
                                  column(7,
                                         h4("Predicted and observed reaction times"),
                                         plotOutput("plotEstPred")))
                              ))),
    
          tabPanel("Team", value = "Team",
                   h5(strong("This Shiny app is based on the previous app by Annika Thierfelder, and was extended by:"), align = "center"),
                   br(),
                   fluidRow(
                     column(3, "Aditya Dandekar", br(),
                            img(src="bremen.png", width = "150")),
                     column(3, "Amalia Gomoiu", br(),
                            img(src="glasgow.png", width = "150")),
                     column(3, "António Fernandes", br(),
                            img(src="lisbon.png", width = "200")),
                     column(3, "Katharina Dücker", br(),
                            img(src="oldenburg.png", width = "110", height = "90"))
                   ),
                   fluidRow(
                     column(3, "Katharina Naumann", br(),
                            img(src="tubingen.png", width = "150")),
                     column(3, "Martin Ingram", br(),
                            img(src="glasgow.png", width = "150")),
                     column(3, "Melanie Spindler", br(),
                            img(src="oldenburg.png", width = "110", height = "90")),
                     column(3, "Silvia Lopes", br(),
                            img(src="lisbon.png", width = "200"))
                   ),
                   br(),
                   h5(strong("under supervision by Prof. Dr. Hans Colonius of the University of Oldenburg, Germany."),align = "center")),
                   
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
