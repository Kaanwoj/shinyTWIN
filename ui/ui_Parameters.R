tabPanel("Parameters", value = "Para",
  p(class="text-info", "Play around with the parameter values and see how
    that affects the distribution of the first stage processing times, the
    predicted mean reaction times, and the probability of integration."),
  fluidRow(
    column(2,
      selectInput("Parampar",h4("1. Choose Paradigm"), 
                  choices = c("Focused Attention Paradigm" = "fap",
                                "Redundant Target Paradigm" = "rtp"))
      ),
    column(2,
      selectInput("distPar", h4("2. Select Distribution of First Stage processing Time"),
        choices = c("Exponential" = "expFAP",
                    "Normal" = "normFAP",
                    "Uniform" = "uniFAP"))
      ),
    h4(tags$div(class = "help-tip",
                p("In FAP, the visual stimulus is set as the target stimulus. In RTP, the 
                  stimulus detected first functions as the target, independent from its modality.")), "3. Select Parameter values for the first stage (in ms)"),
    column(8,
# exponential distribution
      conditionalPanel(condition = ("input.distPar == 'expFAP'"),
        fluidRow(
          column(5,
            sliderInput("mu_t","Mean for visual stimulus
                        (\\(\\frac{1}{\\lambda_V}\\))", min = 1, max = 100,
                        value = 100)),
          column(5,
            sliderInput("mu_nt", "Mean for auditory stimulus
                        (\\(\\frac{1}{\\lambda_A}\\))", min = 1, max = 100,
                        value = 50)))),
      # normal distribution
      conditionalPanel( condition = ("input.distPar == 'normFAP'"),
        fluidRow(
          column(3,
            sliderInput("mun_s1","Mean for visual stimulus",
                    min = 1, max = 150, value = 50)),
          column(3,
            sliderInput("sd_s1","Standard deviation for visual stimulus",
                    min = 1, max = 50, value = 25)),
          column(3,
            sliderInput("mun_s2","Mean for auditory stimulus",
                    min = 1, max = 150, value = 75)),
          column(3,
            sliderInput("sd_s2", "Standard deviation for auditory stimulus ",
                    min = 1, max = 50, value = 40)))),
# uniform distribution
      conditionalPanel( condition = ("input.distPar == 'uniFAP'"),
        fluidRow(
          column(4,
            sliderInput("range_s1","Range for visual stimulus",
                    min = 1, max = 300, value = c(50,110))),
          column(4,
            sliderInput("range_s2","Range for auditory stimulus",
                    min = 1, max = 300, value = c(70,150))))
))),
  
  tags$hr(),
    h4("4. Select Second stage parameters (in ms)"),
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
        plotOutput("stage1_density")),
      column(4, 
        plotOutput("prob")),
      column(4,
             tags$div(class = "help-tip",
                      p("- A decrease in RTs in the bimodal task condition compared to the
                   unimodal condition implies facilitation.", br(),
                        "- Negative reaction times may be simulated due to
                   unrealistic parameter settings.")),
        plotOutput("data")))
)
