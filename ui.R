library(shiny)
library(shinyBS)

shinyUI(
  navbarPage(title = "TWIN Model", theme = "style.css", id = "page",

    ########################
    ### Introduction Tab ###
    ########################

    tabPanel("Introduction", value = "intro",
      withMathJax(),
      tags$head(tags$script(HTML('
        var fakeClick = function(tabName) {
          var dropdownList = document.getElementsByTagName("a");
          for (var i = 0; i < dropdownList.length; i++) {
            var link = dropdownList[i];
            if(link.getAttribute("data-value") == tabName) {
              link.click();
            };
          }
        };
      '))),
      h2("The Time-Window of Integration Model (TWIN)", align = "center"),
      p("This Shiny App helps you to learn about the Time-Window of
        Integration Model (TWIN), developed by Hans Colonius, Adele Diederich,
        and colleagues", align = "center",
        a("(Colonius & Diederich, 2004).",
          onclick="fakeClick('References')")),
      p("It allows you to visualize model predictions, simulate data, and
        estimate the model parameters either from simulated data or from your
        own datafile.", align = "center"),
      # define action buttons, using raw html. Redirecting doesnt work in CSS
        # because of reasons. ;)
      fluidRow(
        column(3, offset=3,
      actionButton("theorybutton",
        HTML("<strong>Theory</strong><br><p> To learn about the theoretical
             <br> background of the experimental <br> paradigms and the TWIN
             </p>"),icon("book"), style = "background-color: #ffff99",
             width="300px")),
        column(3,
      actionButton("parambutton",
        HTML("<strong>Parameters</strong><br><p> To play around and visualize
             <br> the model predictions of <br> the Focused Attention Paradigm
             </p>"), icon("area-chart"), style = "background-color: #5fdc5f",
             width="300px"))),
      fluidRow(
        column(3, offset=3,
      actionButton("simbutton",
        HTML("<strong>Simulation</strong> <br><p>To simulate virtual data using
             different <br> parameter values for both <br>
             paradigms</p>"), icon("dashboard"), style="background-color:
             #ed3f40",width="300px")),
        column(3,
      actionButton("estbutton",
        HTML("<strong>Estimation</strong> <br> <p>To estimate the parameters
             either from <br> previously created data (simulation), <br> or
             your own data</p>"), icon("paper-plane"), style="background-color:
             #2f84ff", width="300px"))),
        # adding footer: <div class="footer">Footer text</div>
        tags$div(class = "footer",
          fluidRow(
          column(6,
            p(class="text-info", "This project is part of",
              a(href="https://tquant.eu/", target="_blank",
             img(src="tquant100.png", width = "30%")), align="left")),
          column(6,
                 p("Contact us on", a(icon("github"),"Github",
                   href ="https://github.com/Kaanwoj/shinyTWIN"))))
    )),

      source(file.path("ui", "ui_Theory.R"), local = TRUE)$value,

    ######################
    ### Parameters Tab ###
    ######################

    tabPanel("Parameters", value = "Para",
      sidebarLayout(
        sidebarPanel(
          selectInput("distPar", "Distribution ",
            choices = c("Exponential" = "expFAP",
                        "Normal" = "normFAP",
                        "Uniform" = "uniFAP")),
          conditionalPanel(condition = ("input.distPar == 'expFAP'"),
            sliderInput("mu_nt", "Auditory processing time (\\(\\frac{1}{\\lambda_A}\\))",
                        min = 1, max = 100, value = 50),
            sliderInput("mu_t","Visual processing time (\\(\\frac{1}{\\lambda_V}\\))",
                        min = 1, max = 100, value = 50)),
         #conditionalPanel(condition = ("input.distPar == 'expRSP'"),
         #  sliderInput("mu_s1", "Mean (Stimulus 1):",
         #              min = 1, max = 100, value = 50),
         #  sliderInput("mu_s2", "Mean (Stimulus 2):",
         #              min = 1, max = 100, value = 50)),
          conditionalPanel( condition = ("input.distPar == 'normFAP'"),
            sliderInput("mun_s1","Mean (Stimulus 1):",
                        min = 1, max = 150, value = 50),
            sliderInput("sd_s1", "Standard deviation (Stimulus 1):",
                        min = 1, max = 50, value = 25),
            sliderInput("mun_s2","Mean (Stimulus 2): ",
                        min = 1, max = 150, value = 50),
            sliderInput("sd_s2","Standard Deviation (Stimulus 2):",
                        min = 1, max = 50, value = 25)),
          conditionalPanel( condition = ("input.distPar == 'uniFAP'"),
            sliderInput("range_s1","Range (Stimulus 1):",
                        min = 1, max = 300, value = c(50,150)),
            sliderInput("range_s2","Range (Stimulus 2): ",
                        min = 1, max = 300, value = c(50,150))),
            sliderInput("mu_second","2nd stage processing time (\\(\\mu\\))",
                        min = 100, max = 500, value = 200),
            sliderInput("sd_second","2nd stage standard deviation",
                        min = 0, max = 100, value = 50),
            sliderInput("delta","Amount of integration (\\(\\Delta\\))",
                        min = -300, max = 300, value = 100),
            sliderInput("omega","Window width (\\(\\omega\\))",
                        min = 0, max = 500, value = 200)),
        mainPanel(
          fluidRow(
            column(6,
                   plotOutput("uni_data_t"),
                   plotOutput("prob")),
            column(6,
                   plotOutput("data"),
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
    ))))),

    ######################
    ### Simulation Tab ###
    ######################

    source(file.path("ui", "ui_Simulation.R"), local = TRUE)$value,

    ######################
    ### Estimation Tab ###
    ######################

    source(file.path("ui", "ui_Estimation.R"), local = TRUE)$value,

    source(file.path("ui", "ui_Team.R"), local = TRUE)$value,
    source(file.path("ui", "ui_References.R"), local = TRUE)$value
))
