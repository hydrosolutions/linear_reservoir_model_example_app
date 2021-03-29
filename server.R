library(shiny)
library(tidyverse)
library(tsibble)
library(rhandsontable)


server <- function(input, output) {

  experiment_table <- tsibble(`Time [s]` = seq(1,11,1),
                              `V [g]` = c(0,29,82,137,184,221,250,268,275, 275, 275),
                              index = `Time [s]`) %>%
    mutate(`Q [ml/s]` = c(0, diff(`V [g]`)))

  reactive_table <- reactiveValues()

  observe({
    if (!is.null(input$dataTable)) {
      reactive_table[["previous"]] <- isolate(reactive_table[["DF"]])
      experiment_table = hot_to_r(input$dataTable)
    } else {
      if (is.null(reactive_table[["DF"]]))
        experiment_table <- experiment_table
      else
        experiment_table <- reactive_table[["DF"]]
    }
    experiment_table$`Q [ml/s]` <- c(0, diff(experiment_table$`V [g]`))
    reactive_table[["DF"]] <- experiment_table
  })

  output$dataTable <- renderRHandsontable({
    data2show <- reactive_table[["DF"]]
    rhandsontable(data2show) %>%
      hot_col(col = "Time [s]", readOnly = TRUE) %>%
      hot_validate_numeric(col = "V [g]", min = 0, max = 10000) %>%
      hot_col(col = "Q [ml/s]", readOnly = TRUE)
  })

  output$distPlot <- renderPlot({
    data2show <- reactive_table[["DF"]]
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
      geom_point(data = data2show, aes(`Time [s]`, `Q [ml/s]`)) +
      labs(x = "Time [s]", y = "Recharge (blue), Discharge (grey) [ml]") +
      theme_bw()
  })
}





