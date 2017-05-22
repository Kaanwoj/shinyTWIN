library(shiny)

ui <- shinyUI(fluidPage(
  withMathJax(),
  # Application title
  fluidRow(
    column(6,
      img(src="tquant100.png", align="left", width="300"))),

  headerPanel(h1("The Time-Window of INtegration Model (TWIN)",
                 align = "center"), windowTitle="shinyTWIN"),

  h4("Focused Attention Paradigm & Redundant Signals Paradigm",
     align = "center"),

  tabsetPanel(id = "TWINTabset",

    ########################
    ### Introduction Tab ###
    ########################

    tabPanel("Introduction", value = "intro",
             h2("Welcome!", align = "center"),
             br(),
             p("This Shiny App helps you to learn more about the Time-Window
               Integration Model (TWIN), developed by Hans Colonius, Adele
               Diederich, and colleagues", align = "center",
               a("(Colonius & Diederich, 2004).",
                 href="https://www.uni-oldenburg.de/fileadmin/user_upload/psycho/ag/kogn/colonius/Jcogn.pdf",
                 target="_blank")),
             p("It allows you to simulate and estimate the model parameters either from 
               virtual data or from your own datafile.", align = "center"),
             p("If you have any questions or remarks, please send an email to XXXXXXXXXXX",
               align ="center"),
             br(),
             h5(strong("This Shiny app is based on the app by Annika
                       Thierfelder, and was extended by:"), align = "center"),
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
        )
    ),

    ##########################
    ### The TWIN Model Tab ###
    ##########################

    tabPanel("The TWIN Model", value="twinmod",
             
             sidebarLayout(
               sidebarPanel(
                 selectInput("topic", "Select the topic:",
                             choices = c("The TWIN model" = "twinmod",
                                         "Focused Attention Paradigm" = "fap",
                                         "Redundant Target Paradigm" = "rtp")),
                 a("See to TWIN publication (Kandil, Diederich & Colonius, 2014)",
                   href="http://jov.arvojournals.org/article.aspx?articleid=2193864",
                   target="_blank")
               ),
               
               mainPanel(
                 conditionalPanel(condition = ("input.topic == 'twinmod'"),
                                  
                                  h2("Theoretical Background"),
                                  p("Multisensory interaction is defined as the (neural) mechanism underlying crossmodal interaction. 
                                    Crossmodal interaction describes a situation in which the perception of a stimulus or eventof a given modality is in some way influenced
                                    by the presence of one or more stimuli of othersensory modalities. On the behavioral level, multisensory integration is characterized by 
                                    faster reaction times, higher detection probabilities and improved discrimination. 
                                    On the neural level,it is observed in form of an increased number of total responses as well as shorter response latencies.(Diederich,
                                    Colonius, & Kandil, 2016)."),
                                  p(img(src="modeltwin.png", width = "550"), align = "center"),
                                  p("In tasks measuring multisensory integration, participants are asked to respond to a cross-modal set of stimuli - usually visual and auditive stimuli -, 
                                    that are presented simultaneously or at certain interstimulus intervals. The task is then either to respond to a target stimulus of acertain modality 
                                    (usually the visual stimulus, see Focused Attention Paradigm (FAP)), or to respond to the first stimulus thatis perceived (see Redundant Target Paradigm (RTP)), by button press.
                                    Following the TWIN model, the general processing can be split into two stages:",br(),"
                                    In the", em("First-Stage Assumption"),",
                                    a crossmodal (audiovisual) stimulus triggers a race mechanism in the very early, peripheral sensory pathways. Here, the pathways for different modalities are initially separated.",br()," 
                                    In the", em("Stage-Two Assumption"),
                                    ", all subsequent, possibly temporally overlapping, processes that are not part of the peripheral processes in the first stage are included (neural integration of the input and preparation of a response). 
                                    Multisensory integration is then obvservable in an increase or decrease in second-stage processing time.",br(),"
                                    At this, the", em("Time-Window-of-Integration Assumption"),"
                                    postulates that multisensory integration can only occur if the processes of the first stage all terminate within a given time window (the time window of integration).
                                    "),
                                  p("Then, the amount of interaction is defined in the", em("Assumption of Temporal Separability"), "where an increase or decrease of second-stage processing time is a function of crossmodal stimulus features but does
                                    not depend on the presentation asynchrony (SOA) of the stimuli."),
                                  p(img(src="fap.jpeg", width = "500", align = "center")),
                                  p(img(src="rtp.jpeg", width = "500", align = "center"))
                                  
                                  ),
                 
                 conditionalPanel(condition = ("input.topic == 'fap'"),
                                  h2("The Focused Attention Paradigm (FAP)"),
                                  withMathJax(p("In the Focused Attention Paradigm, one stimulus modality is pre-defined as the target stimulus to respond to. The other stimulus is called the non-target stimulus. 
                                                In this experiment, we assume that the visual stimulus is the target modality, and the auditory stimulus is the non-target modality.
                                                Let \\(I\\) denote the event that multisensory integration occurs. For FAP, the condition for multisensory integration is defined as  $$I_{FAP} = {A + \\tau < V < A + \\tau + \\omega}$$ with the probability of integration \\(P(I)\\).
                                                Here, \\(\\tau\\) denotes the stimulus-onset asynchrony (SOA), which is determined by the experimental setup, and \\(\\omega\\) as the window width of integration, the time window, in which integration may occur.
                                                A positive \\(\\tau\\) value thus indicates that that the visual stimulus is presented before the auditory, and a negative \\(\\tau\\) values indicates the reverse presentation order. In theory, \\(V\\) is always presented at \\(t = 0\\)."),
                                              h4("The Probability of Integration"),
                                              p("The probability of integration can be described as a function of SOA and window width:  
                                                Seeing that the peripheral processing time for the visual (\\(V\\)) and auditory (\\(A\\)) stimulus are assumed to be exponentially distributed and statistically independent (as they refer to very early sensory processing), 
                                                with parameters \\(\\lambda_V\\) and \\(\\lambda_A\\) and expected values of \\(1/\\lambda_V\\) and \\(1/\\lambda_A\\),
                                                $$\\begin{align}
                                                f_V(t) = \\lambda_{V}e^{-\\lambda_{V}t} \\\\
                                                f_A(t) = \\lambda_{A}e^{-\\lambda_{A}t}
                                                \\end{align}$$
                                                for \\(t \\geq 0\\), and \\(f_V(t) = f_A(t) \\equiv 0\\) for \\(t < 0.\\) The corresponding distribution functions are \\(F_V(t)\\) and  \\(F_A(t)\\), respectively. 
                                                The model can therefore be rewritten as:")),
                                  withMathJax(p("$$\\begin{align}
                                                P(I_{\\tau\\omega}) &= Pr(A + \\tau < V < A + \\tau + \\omega) \\\\
                                                &= \\int_{0}^{\\infty} \\! f_A(x)\\{F_V(x + \\tau + \\omega) \\\\ 
                                                &- F_V(x+\\tau)\\}\\,\\mathrm{d}x.
                                                \\end{align}$$"
                                  )),
                                  withMathJax(p("Thus, the integration function for \\(P(I)\\) depends on the three cases for the sign of \\(\\omega\\) + \\(\\tau\\):")),
                                  withMathJax(p("(i) \\(\\tau\\) < \\(\\tau\\) + \\(\\omega\\) < 0", align = "center",
                                                "$$\\begin{align}
                                                P(I_{\\tau,\\omega}) &= \\int_{-\\tau-\\omega}^{-\\tau} \\! \\lambda_{A}e^{-\\lambda_{A}x}{1-e^{-\\lambda_V(x+\\tau+\\omega)}} \\, \\mathrm{d}x \\\\
                                                &-\\int_{-\\tau}^{\\infty} \\! \\lambda_{A}e^{-\\lambda_{A}x}{e^{-\\lambda_V(x+\\tau)} - e^{-\\lambda_V(x+\\tau+\\omega)}} \\, \\mathrm{d}x \\\\
                                                &= \\frac{\\lambda_V}{\\lambda_V+\\lambda_A} e^{\\lambda_A\\tau}(-1 + e^{\\lambda_A\\omega});
                                                \\end{align}$$
                                                ")),
                                  withMathJax(p("(ii)  \\(\\tau\\) < 0 < \\(\\tau\\) + \\(\\omega\\)", align = "center")),
                                  withMathJax(p("$$\\begin{align}
                                                P(I_{\\tau,\\omega})  &= \\int_{0}^{-\\tau} \\! \\lambda_{A}e^{-\\lambda_{A}x}\\{1-e^{-\\lambda_V(x+\\tau+\\omega)}\\} \\, \\mathrm{d}x \\\\
                                                &+ \\int_{-\\tau}^{\\infty} \\! \\lambda_{A}e^{-\\lambda_{A}x}\\{e^{-\\lambda_V(x+\\tau)} - e^{-\\lambda_V(x+\\tau+\\omega)}\\} \\, \\mathrm{d}x \\\\
                                                &= \\frac{1}{\\lambda_V+\\lambda_A} \\{\\lambda_A(1-e^{-\\lambda_V(\\omega+\\tau)})+ \\lambda_V(1-e^{\\lambda_A\\tau})\\};
                                                \\end{align}$$")),
                                  withMathJax(p("(iii) \\(0 < \\tau < \\tau + \\omega\\)", align = "center")),
                                  withMathJax(p("$$\\begin{align}
                                                P(I_{\\tau,\\omega}) &= \\int_{0}^{\\infty} \\! \\lambda_{A}e^{-\\lambda_{A}x}\\{e^{-\\lambda_V(x+\\tau)}-e^{-\\lambda_V(x+\\tau+\\omega)}\\} \\mathrm{d}x \\\\
                                                &= \\frac{\\lambda_A}{\\lambda_V+\\lambda_A} \\{e^{-\\lambda_V(\\tau)}-e^{-\\lambda_V(\\omega+\\tau)}.
                                                \\end{align}$$")),
                                  h4("Reaction time analysis"),
                                  withMathJax(p("Reaction times are evaluated for the unimodal condition to compare against the crossmodal condition.
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
                                                where \\(\\mu\\) is the mean processing time of stage two."))
                                  ),
                 
                 conditionalPanel(condition = ("input.topic == 'rtp'"),
                                  h2("The Redundant Target Paradigm (RTP)"),
                                  withMathJax(p("For RTP, the following condition for multisensory integration $$I_{RTP} = {max(V,A + \\tau) < min(V,A + \\tau) + \\omega}$$ 
                                                with the probability of Integration \\(P(I)\\) holds.
                                                ")),
                                  h4("Probability of Integration"),
                                  p("Probalility of Integration for cases, in which the visual stimulus is presented first (e.g., SOAs of 0, 50 and 100 ms)....
                                    Probalility of Integration for cases, in which the auditory stimulus is presented first (e.g., SOAs of -100, -50 and 0 ms)...")
                                  
                                  )))),

    ######################
    ### Parameters Tab ###
    ######################

    tabPanel("Parameters", value = "Para",
      sidebarLayout(
        sidebarPanel(
          selectInput("dist", "Distribution ",
                      choices = c("Exponential" = "expFAP",
                                       "Normal" = "normFAP",
                                      "Uniform" = "uniFAP")
          ),
          conditionalPanel(condition = ("input.dist == 'expFAP'"),
            sliderInput("mu_nt",
                        "Auditory processing time
                        (\\(\\frac{1}{\\lambda_A}\\))",
                        min = 1,
                        max = 100,
                        value = 50),
            sliderInput("mu_t",
                        "Visual processing time
                        (\\(\\frac{1}{\\lambda_V}\\))",
                        min = 1,
                        max = 100,
                        value = 50)
          ),
          conditionalPanel(condition = ("input.dist == 'expRSP'"),
            sliderInput("mu_s1",
                        "Mean (Stimulus 1):",
                        min = 1,
                        max = 100,
                        value = 50),
            sliderInput("mu_s2",
                        "Mean (Stimulus 2):",
                        min = 1,
                        max = 100,
                        value = 50)
           ),
            conditionalPanel( condition = ("input.dist == 'normFAP'"),
              sliderInput("mun_s1",
                          "Mean (Stimulus 1):",
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
          conditionalPanel( condition = ("input.dist == 'uniFAP'"),
            sliderInput("min_s1",
                        "Minimum (Stimulus 1):",
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
                      "2nd stage processing time (\\(\\mu\\))",
                      min = 100,
                      max = 500,
                      value = 200),
          sliderInput("sd_second",
                      "2nd stage standard deviation",
                      min = 0,
                      max = 100,
                      value = 50),
          sliderInput("delta",
                      "Amount of integration (\\(\\delta\\))",
                      min = -300,
                      max = 300,
                      value = 100),
          sliderInput("omega",
                      "Window width (\\(\\omega\\))",
                      min = 0,
                      max = 500,
                      value = 200)
      ),

      mainPanel(
        fluidRow(
          column(6,
            plotOutput("uni_data_t"),
            plotOutput("prob")),
          column(6,
            plotOutput("data"),
 ########### Parameters Tab Description #########
            br(),
            strong("1)"),
            p("The topleft graph shows the exponential distribution of the intensity measure lambda for both", 
            span("visual (target)", style = "color:red"), "and", span("auditory (non-target) stimuli", style = "color:blue")),
            br(),
            strong("2)"),
            p("The topright graph shows the mean RTs of the different SOAs for both the", span("unimodal", style = "color:blue"), "and", span("bimodal", style = "color:red"), "task condition. \n
              A decrease in RTs in the bimodal task condition compared to the unimodal condition implies fascilitation."),
            br(),
            strong("3)"),
            p("The bottom graph displays the probability of integration depending on the length of the SOA.")
           )
    )))),

    ######################
    ### Simulation Tab ###
    ######################

    tabPanel("Simulation", value = "Sim",

      sidebarLayout(
        sidebarPanel(
          selectInput("dist2", "1. Choose paradigm",
                      choices = c("Focused Attention Paradigm" = "expFAP",
                                  "Redundant Target Paradigm" = "expRSP")),
          h4("2. Set parameter values"),
          uiOutput("soa_input"),
          sliderInput("N",
                      "Number of trials:",
                      min = 1, max = 1000, value = 500),
          h4("First stage"),
          sliderInput("proc.A",
                      "Auditory processing time (\\(\\frac{1}{\\lambda_A}\\))",
                      min = 20, max = 150, value = 100),
          sliderInput("proc.V",
                      "Visual processing time (\\(\\frac{1}{\\lambda_V}\\))",
                      min = 20, max = 150, value = 50),
          h4("Second stage"),
          sliderInput("mu",
                      "... processing time (\\(\\mu\\))",
                      min = 100, max = 500, value = 200),
          p("The standard deviation of the second stage processing time is
            fixed to \\(^\\mu/_5\\)."),
         #sliderInput("sigma",
         #            "Standard Deviation",
         #            min = 500, max = 200, value = 50),
          sliderInput("sim.omega",
                      "Window width (\\(\\omega\\))",
                      min = 100, max = 300, value = 200),
          sliderInput("sim.delta",
                      "Amount of integration (\\(\\delta\\))",
                      min = 20, max = 100, value = 50)
        ),
        mainPanel(
          fluidRow(
            column(3,
              h4("3. Simulate data"),
              actionButton("sim_button", "Simulate!")),
            column(9,
              h4("(4.) To download your simulated data, press on the button below"),
              downloadButton('downloadData', 'Download (.csv)'))
          ),
          h3("Simulated Data"),
          h4("Boxplots of reaction times for each SOA"),
          plotOutput("simplot"),
          h4("Table of reaction times for each SOA"),
          numericInput("nrowShow",
                       "Number of rows displayed",
                       min=1, max=60, value=10),
          tableOutput("simtable")
    ))),

    ######################
    ### Estimation Tab ###
    ######################

    tabPanel("Estimation", value = "Est",
      sidebarLayout(
        sidebarPanel(
          radioButtons("whichDataEst",
                       "1. Choose data for parameter estimation",
                       c("Simulated data (from tab before)" = "sim",
                         "Upload data" = "upload")),
          uiOutput("data_input"),
          a("See publication on TWIN estimation (Kandil, Diederich & Colonius,
            2014)",
          href="http://jov.arvojournals.org/article.aspx?articleid=2193864",
          target="_blank")
        ),
        mainPanel(
          h4("2. Estimate parameters"),
          actionButton("est_button", "Estimate!"),

          fluidRow(
            column(5,
              h2("Parameter values"),
              tableOutput("estTextOut"),
              dataTableOutput("dt1")),
            column(7,
              h2("Predicted and observed reaction times"),
              plotOutput("plotEstPred"))
          )
    ))),

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
)))

























