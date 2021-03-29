library(shiny)
library(tidyverse)
library(tsibble)

ui <- fluidPage(
  titlePanel("Linear reservoir modelling example"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("k", "Storage coefficient:", 0.2, min = 0.1, max = 1)
    ),
    mainPanel(
      #withMathJax("$$(P-Q) \\cdot dt = k \\cdot dQ$$"),
      plotOutput(outputId = "distPlot")
    )
  )
)

server <- function(input, output) {
  output$distPlot <- renderPlot({
    experiment_table <- tsibble(`Time [s]` = seq(1,11,1),
                                `V [ml]` = c(0,29,82,137,184,221,250,268,275, 275, 275),
                                index = `Time [s]`) %>%
      mutate(`Q [ml/s]` = c(0, diff(`V [ml]`)))


    deltat <- 0.2
    Q0 <- 0

    C1 <- deltat / (2 * input$k + deltat)
    C2 <- (2 * input$k - deltat) / (2 * input$k + deltat)

    dt <- 1
    measurements <- tsibble(second = seq(1, 14, dt),
                            R = 0,
                            index = second)
    measurements$R[1:(3*1/dt)] <- rep(275*dt/3, 3*1/dt)
    L <- dim(measurements)[1]

    measurements$Q <- Q0
    for (t in c(2:L)) {
      measurements$Q[t] <- 2 * C1 * measurements$R[t] + C2 * measurements$Q[t-1]
    }

    colSums(measurements)

    ggplot(measurements) +
      geom_col(aes(second, R), fill = "blue", alpha = 0.8) +
      geom_col(aes(second, Q), alpha = 0.8) +
      geom_point(data = experiment_table, aes(`Time [s]`, `Q [ml/s]`)) +
      labs(x = "Time [s]", y = "R (blue), Q (grey) [ml]") +
      theme_bw()
  })
}

shinyApp(ui, server)



