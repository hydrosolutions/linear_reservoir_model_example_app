
ui <- fluidPage(
  titlePanel("Linear reservoir modelling example"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("k", "Storage coefficient k:", 0.2, min = 0.1, max = 1),
      rHandsontableOutput("dataTable")
    ),
    mainPanel(
      withMathJax("$$(R-Q) \\cdot dt = k \\cdot dQ$$"),
      plotOutput(outputId = "distPlot")
    )
  )
)




