
ui <- fluidPage(
  titlePanel("Linear reservoir modelling example"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("k", "Storage coefficient:", 0.2, min = 0.1, max = 1),
      rHandsontableOutput("dataTable")
    ),
    mainPanel(
      #withMathJax("$$(P-Q) \\cdot dt = k \\cdot dQ$$"),
      plotOutput(outputId = "distPlot")
    )
  )
)




