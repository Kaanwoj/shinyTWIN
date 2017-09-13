tabPanel("Simulation", value = "Sim",
  p(class="text-info", "To simulate your own data, first specify which paradigm and
    parameter values you want to use. In order to apply your changes and
    start the simulation, press Simulate!"),
  selectInput("paradigmSim", h4("1. Choose paradigm"), choices =
              c("Focused Attention Paradigm" = "fap",
                "Redundant Target Paradigm" = "rtp")),
  fluidRow(
    column(3,
      h4("2. Set parameter values"),
      uiOutput("soa_input"),
      sliderInput("N","Number of trials:",
                  min = 1, max = 1000, value = 500)),
    column(3,
      h5("First stage processing time"),
      tags$div(class = "help-tip",
        p("Processing time is simulated from an exponential distribution.
          Select its expected value.")),
      sliderInput("proc.A","for auditory stimulus
                  (\\(\\frac{1}{\\lambda_A}\\))",
                  min = 20, max = 150, value = 100),
      sliderInput("proc.V","Visual processing time
                  (\\(\\frac{1}{\\lambda_V}\\))",
                  min = 20, max = 150, value = 50)),
    column(3,
      h5("Second stage processing time"),
      tags$div(class = "help-tip",
        p("Processing time is simulated from a normal distribution. Select
          its expected value.")),
      sliderInput("mu","\\(\\mu\\)",
                  min = 100, max = 500, value = 200),
      p("The standard deviation of the second stage processing time is
        fixed to \\(\\frac{\\mu}{5}\\).")),
    column(3,
      sliderInput("sim.omega","Window width (\\(\\omega\\))",
                  min = 100, max = 300, value = 200),
      sliderInput("sim.delta","Amount of integration (\\(\\Delta\\))",
                  min = 20, max = 100, value = 50))),
  fluidRow(
    column(3,
      h4("3. Simulate data"),
      actionButton("sim_button", "Simulate!")),
    column(9,
      h4("4. To download your simulated data, press on the button
         below"),
      downloadButton('downloadData', 'Download (.csv)'))),
    h4("5. View Simulated Data"),
    tabsetPanel(
      tabPanel("Plot",
        h5("Boxplots of reaction times for each SOA"),
        plotOutput("simplot")),
      tabPanel("Table",
        h5("Table of reaction times for each SOA"),
        numericInput("nrowShow","Number of rows displayed",
                     min=1, max=60, value=10),
        tableOutput("simtable"))
))
