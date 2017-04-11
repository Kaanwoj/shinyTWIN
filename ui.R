library(shiny)

ui <- shinyUI(fluidPage(
    withMathJax(),
  # Application title
  fluidRow(
    column(6,
      img(src = "tquant100.png", align = "left"))),
  br(),
  headerPanel(h1("The Time-Window of INtegration Model (TWIN)",
                 align = "center")),

  h4("Focused Attention Paradigm & Redundant Signals Paradigm",
     align = "center"),

  tabsetPanel(id = "TWINTabset",

    ########################
    ### Introduction Tab ###
    ########################

    tabPanel("Introduction", value = "intro",

      sidebarLayout(
        sidebarPanel(
          selectInput("topic", "Select the topic:",
                      choices = c("About this app" = "aboutapp",
                                  "The TWIN model" = "twinmod",
                                  "Parameters" = "param",
                                  "Model equations" = "equ"
          )),
          a("See to TWIN publication (Kandil, Diederich & Colonius, 2014)",
            href="http://jov.arvojournals.org/article.aspx?articleid=2193864",
            target="_blank")
        ),

        mainPanel(
          conditionalPanel(condition = ("input.topic == 'aboutapp'"),
            h2("Welcome!", align = "center"),
            br(),
            p("This Shiny App helps you to learn more about the Time-Window
              Ingration Model (TWIN), developed by Hans Colonius, Adele
              Diederich, and colleagues.", align = "center"),
            p("It allows you to simulate and estimate the parameteres and even
              to upload your own data file.", align = "center"),
            p("Feel free to explore the app and navigate through the tabs!",
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
            )),

          conditionalPanel(condition = ("input.topic == 'twinmod'"),
            p("Crossmodal interaction is defined as the situation in which the
              perception of an event as measured in terms of one modality is
              changed in some way by the concurrent stimulation of one or more
              other sensory modalities (Welch & Warren, 1986)."),
            p("Multisensory integration is defined as the (neural) mechanism
              underlying crossmodal interaction."),
            p("The Time-Window of Integration Model (TWIN) postulates that a
              crossmodal stimulus triggers a race mechanism among the
              activations in very early, peripheral sensory pathways. This
              first stage is followed by a compound stage of converging
              subprocesses that comprise neural integration of the input and
              preparation of a response. The second stage is defined by
              default: It includes all subsequent processes that are not part
              of the peripheral processes in the first stage (Diederich,
              Colonius, & Kandil, 2016)."),
            p(img(src="modeltwin.png", width = "550"), align = "center"),
            p("At the behavioral level, multisensory integration translates
              itself into faster reaction times, higher detection
              probabilities, and improved discrimination."),
            p("At the neural level, multisensory integration translates itself
              into an increased total number of responses, as well as shorter
              response latencies."),
            p("In the Focused Attention Paradigm (FAP), two or more stimuli are
              presented simultaneously or with a short delay between the
              stimuli. The participant is asked to respond as quickly as
              possible to a stimulus of a pre-defined modality (target) and
              ignore the other stimulus (non-target modality)."),
            p(img(src="fap.jpeg", width = "500", align = "center")),
            p("In the Redundant Signals Paradigm (RSP), two or more stimuli
              are also presented simultanously or with a short delay between
              the stimulus. However, the participant is asked to respond to
              a stimulus of any modality detected first."),
            p(img(src="rtp.jpeg", width = "500", align = "center"))
          ),

          conditionalPanel(condition = ("input.topic == 'param'"),
            h4("Intensity of the auditory stimuli"),
            img(src="lamba.jpeg", width = "50"),
            h4("Intensity of the visual stimuli"),
            img(src="lambv.jpeg", width = "50"),
            h4("Processing time for the second stage"),
            img(src="u.jpeg", width = "40"),
            h4("Width of the time window of integration"),
            img(src="w.jpeg", width = "40"),
            h4("Effect size / Amount of integration"),
            img(src="delta.jpeg", width = "40")
          ),

          conditionalPanel(condition = ("input.topic == 'equ'"),
            h4("Requirement for multisensory integration"),
            img(src="itw.jpeg", width = "300"),
            h4("Mean reaction times in the unimodal and bimodal conditions"),
            img(src="rts.jpeg", width = "300"),
            h4("Observable reaction time following the logic of the model"),
            img(src="rt.jpeg", width = "400"),
            h4("Objective function"),
            img(src="objfun.jpeg", width = "300"),
            br(),
            h4("Three cases for the probability of integration P(I)"),
            img(src="p1.jpeg", width = "400", align = "left"),
            img(src="p2.jpeg", width = "400", align = "left"),
            img(src="p3.jpeg", width = "400", align = "left")
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
            plotOutput("uni_data_nt")),
          column(6,
            plotOutput("data"),
            plotOutput("prob"))
    )))),

    ######################
    ### Simulation Tab ###
    ######################

    tabPanel("Simulation", value = "Sim",

      sidebarLayout(
        sidebarPanel(
          selectInput("dist2", "Choose paradigm",
                      choices = c("Focused Attention Paradigm" = "expFAP",
                                  "Redundant Target Paradigm" = "expRSP")),
          sliderInput("N",
                      "Amount of trials:",
                      min = 1,
                      max = 1000,
                      value = 500),
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
         #sliderInput("sigma",
         #            "Standard Deviation",
         #            min = 500,
         #            max = 200,
         #            value = 50),
          sliderInput("sim.omega",
                      "Window width (\\(\\omega\\))",
                      min = 100,
                      max = 300,
                      value = 200),
          sliderInput("sim.delta",
                      "Amount of integration (\\(\\delta\\))",
                      min = 20,
                      max = 100,
                      value = 50),
         h5("To download your simulated data, press on the button below:"),
          downloadButton('downloadData', 'Download (CSV)')
        ),
        mainPanel(
          h2("Simulated Data"),
          plotOutput("simplot"),
          numericInput("nrowShow",
                       "Number of rows displayed",
                       min=1,
                       max=60,
                       value=10),
          tableOutput("simtable")
       
    ))),

    ######################
    ### Estimation Tab ###
    ######################

    tabPanel("Estimation", value = "Est",
      sidebarLayout(
        sidebarPanel(
          fileInput('file1', 'Choose file to upload',
                    accept = c(
                      'text/csv',
                      'text/comma-separated-values',
                      'text/tab-separated-values',
                      'text/plain',
                      '.csv',
                      '.tsv')
          ),
          a("See publication on TWIN estimation (Kandil, Diederich & Colonius,
            2014)",
          href="http://jov.arvojournals.org/article.aspx?articleid=2193864",
          target="_blank")
        ),
        mainPanel(
          h2("Estimated values"),
          tableOutput("estTextOut"),
          h2("Starting values for parameters"),
          tableOutput("startParamTextOut"),
          dataTableOutput("dt1"))
    )),

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

























