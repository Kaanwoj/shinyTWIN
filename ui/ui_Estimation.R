tabPanel("Estimation", value = "Est",
  sidebarLayout(
    sidebarPanel(
    p(class="text-info", "To estimate the parameters from your simulation,
      please go to the Simulation Tab and simulate the data first. You can also
      upload your own datafile (.csv), then you don't need to do the simulation
      first."),
      radioButtons("whichDataEst",
                   h4("1. Choose data for parameter estimation"),
                   c("Simulated data (from tab before)" = "sim",
                                          "Upload data" = "upload")),
      conditionalPanel(
        p(class="text-info", 'The file must contain a header in the first line giving
          information about the durations of SOA. See the table in the
          Simulation tab. The data in the file must be semicolon-separated (;).'),
        condition = "input.whichDataEst == 'upload'",
          fileInput('file1', 'Choose file to upload',
                    accept = c('text/csv', 'text/comma-separated-values', '.csv')),
          radioButtons("paradigmUpload", "Which paradigm was used?",
                         c("Focused Attention" = "fap",
                           "Redundant Target" = "rtp"))),
      h4("2. Estimate parameters"),
      actionButton("est_button", "Estimate!"), br(),
      a("How does the estimation procedure work?",
        onclick="fakeClick('Theory')")
    ),
    mainPanel(
      h4("Parameter values"),
      tableOutput("estTextOut"),
      tableOutput("chisqValue"),
      h4("Predicted and observed reaction times"),
      plotOutput("plotPredObs")
)))
