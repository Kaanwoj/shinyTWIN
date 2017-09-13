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

    source(file.path("ui", "ui_Parameters.R"), local = TRUE)$value,

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
