tabPanel("Team", value = "Team",
    h5(strong("This Shiny app is based on the previous app by Annika
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
    ),
    br(),
    h5(strong("under supervision by Prof. Dr. Hans Colonius of the University of
              Oldenburg, Germany."), align = "center")
)
