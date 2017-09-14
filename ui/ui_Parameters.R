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
            sliderInput("mu_t","Mean for target stimulus
                        (\\(\\frac{1}{\\lambda_V}\\))", min = 1, max = 100,
                        value = 100)),
          column(5,
            sliderInput("mu_nt", "Mean for non-target stimulus
                        (\\(\\frac{1}{\\lambda_A}\\))", min = 1, max = 100,
                        value = 50)))),
      # normal distribution
      conditionalPanel( condition = ("input.distPar == 'normFAP'"),
        fluidRow(
          column(3,
            sliderInput("mun_s1","Mean for target stimulus",
                    min = 1, max = 150, value = 50)),
          column(3,
            sliderInput("sd_s1","Standard deviation for target stimulus",
                    min = 1, max = 50, value = 25)),
          column(3,
            sliderInput("mun_s2","Mean for non-target stimulus",
                    min = 1, max = 150, value = 75)),
          column(3,
            sliderInput("sd_s2", "Standard deviation for non-target stimulus ",
                    min = 1, max = 50, value = 40)))),
      # uniform distribution
      conditionalPanel( condition = ("input.distPar == 'uniFAP'"),
        fluidRow(
          column(4,
            sliderInput("range_s1","Range for target stimulus",
                    min = 1, max = 300, value = c(50,110))),
          column(4,
            sliderInput("range_s2","Range for non-target stimulus",
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
        tags$div(class = "help-tip",
          p("A decrease in RTs in the bimodal task condition compared to the
            unimodal condition implies facilitation.")),
        plotOutput("data")))
)
