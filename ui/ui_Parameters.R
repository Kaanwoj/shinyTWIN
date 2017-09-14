tabPanel("Parameters", value = "Para",
  p(class="text-info", "Play around with the parameter values and see how
    that affects the distribution of the first stage processing times, the
    predicted mean reaction times, and the probability of integration."),
  h5("First stage processing time (in ms)"),
  fluidRow(
    column(2,
      selectInput("distPar", "Distribution",
        choices = c("Exponential" = "expFAP",
                    "Normal" = "normFAP",
                    "Uniform" = "uniFAP"))),
    column(10,
      # exponential distribution
      conditionalPanel(condition = ("input.distPar == 'expFAP'"),
      fluidRow(
        column(5,
        sliderInput("mu_nt", "Auditory stimulus mean (\\(\\frac{1}{\\lambda_A}\\))",
                    min = 1, max = 100, value = 50)),
        column(5,
        sliderInput("mu_t","Visual stimulus mean (\\(\\frac{1}{\\lambda_V}\\))",
                    min = 1, max = 100, value = 100)))),
      # normal distribution
      conditionalPanel( condition = ("input.distPar == 'normFAP'"),
        fluidRow(
          column(3,
        sliderInput("mun_s1","Auditory stimulus mean",
                    min = 1, max = 150, value = 50)),
          column(3,
        sliderInput("sd_s1", "Auditory stimulus standard deviation",
                    min = 1, max = 50, value = 25)),
          column(3,
        sliderInput("mun_s2","Visual stimulus mean",
                    min = 1, max = 150, value = 75)),
          column(3,
        sliderInput("sd_s2","Visual stimulus standard deviation",
                    min = 1, max = 50, value = 40)))),
      # uniform distribution
      conditionalPanel( condition = ("input.distPar == 'uniFAP'"),
        fluidRow(
          column(4,
        sliderInput("range_s1","Auditory stimulus range",
                    min = 1, max = 300, value = c(50,110))),
          column(4,
        sliderInput("range_s2","Visual stimulus range",
                    min = 1, max = 300, value = c(70,150)))))
      )),
    h5("Second stage processing time (in ms)"),
    fluidRow(
    column(3,
        sliderInput("mu_second","Mean (\\(\\mu\\))",
                    min = 100, max = 500, value = 200)),
    column(3,
        sliderInput("sd_second","Standard deviation",
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
