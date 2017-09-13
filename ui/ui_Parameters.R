tabPanel("Parameters", value = "Para",
  p(class="text-info", "Play around with the parameter values and see how
    that affects the distribution of the first stage processing times, the
    predicted mean reaction times, and the probability of integration."),
  fluidRow(
    column(2,
      selectInput("distPar", "Distribution ",
        choices = c("Exponential" = "expFAP",
                    "Normal" = "normFAP",
                    "Uniform" = "uniFAP"))),
    column(10,
      # exponential distribution
      conditionalPanel(condition = ("input.distPar == 'expFAP'"),
      fluidRow(
        column(4,
        sliderInput("mu_nt", "Auditory processing time (\\(\\frac{1}{\\lambda_A}\\))",
                    min = 1, max = 100, value = 50)),
        column(4,
        sliderInput("mu_t","Visual processing time (\\(\\frac{1}{\\lambda_V}\\))",
                    min = 1, max = 100, value = 50)))),
      # normal distribution
      conditionalPanel( condition = ("input.distPar == 'normFAP'"),
        fluidRow(
          column(3,
        sliderInput("mun_s1","Mean (Stimulus 1):",
                    min = 1, max = 150, value = 50)),
          column(3,
        sliderInput("sd_s1", "Standard deviation (Stimulus 1):",
                    min = 1, max = 50, value = 25)),
          column(3,
        sliderInput("mun_s2","Mean (Stimulus 2): ",
                    min = 1, max = 150, value = 50)),
          column(3,
        sliderInput("sd_s2","Standard Deviation (Stimulus 2):",
                    min = 1, max = 50, value = 25)))),
      # uniform distribution
      conditionalPanel( condition = ("input.distPar == 'uniFAP'"),
        fluidRow(
          column(4,
        sliderInput("range_s1","Range (Stimulus 1):",
                    min = 1, max = 300, value = c(50,150))),
          column(4,
        sliderInput("range_s2","Range (Stimulus 2): ",
                    min = 1, max = 300, value = c(50,150)))))
      )),
    fluidRow(
    column(3,
        sliderInput("mu_second","2nd stage processing time (\\(\\mu\\))",
                    min = 100, max = 500, value = 200)),
    column(3,
        sliderInput("sd_second","2nd stage standard deviation",
                    min = 0, max = 100, value = 50)),
    column(3,
        sliderInput("delta","Amount of integration (\\(\\Delta\\))",
                    min = -300, max = 300, value = 100)),
    column(3,
        sliderInput("omega","Window width (\\(\\omega\\))",
                    min = 0, max = 500, value = 200))),
      fluidRow(
        column(4,
               plotOutput("uni_data_t")),
        column(4,
               plotOutput("prob")),
        column(4,
               plotOutput("data"))),
               ########### Parameters Tab Description #########
               br(),
               tags$div(class = "help-tip", tags$p("This tab shows you the
                   different dependencies of the model parameters.")),
               # <div class="help-tip">
               # <p>Help tip text blabla.</p>
               # </div>)
               br(),
               tags$ol(
                 tags$li("The topleft graph shows the exponential
                         distribution of the intensity measure lambda for
                         both", span("visual (target)", style =
                         "color:red"), "and", span("auditory (non-target)
                         stimuli.", style = "color:blue")),
                 tags$li("The topright graph shows the mean RTs of the
                         different SOAs for both the", span("unimodal",
                         style = "color:blue"), "and", span("bimodal",
                         style = "color:red"), "task condition. \n A
                         decrease in RTs in the bimodal task condition
                         compared to the unimodal condition implies
                         fascilitation."),
                 tags$li("The bottom graph displays the probability of
                         integration depending on the length of the SOA."))
)
