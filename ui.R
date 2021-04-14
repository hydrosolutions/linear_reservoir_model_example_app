
ui <- fluidPage(
  titlePanel("Linear reservoir modelling example"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("k", "Storage coefficient k:", 0.2, min = 0.1, max = 1),
      h5("Measured discharge"),
      rHandsontableOutput("dataTable", height = "90px")
    ),
    mainPanel(
      h5("Simulated and measured discharge"),
      plotOutput(outputId = "distPlot"),
    )
  )
)




