library(ggplot2)
library(shiny)

histogram <- function(x1, x2, binwidth = 0.1, xlim = c(-3, 3)) {
  df <- data.frame(
    x = c(x1, x2),
    Distributions = c(rep("Distribution 1", length(x1)), rep("Distribution 2", length(x2)))
  )
  
  ggplot(df, aes(x, fill = Distributions)) +
    geom_histogram(binwidth = binwidth,colour='black', size=1) +
    coord_cartesian(xlim = xlim)
}

t_test <- function(x1, x2) {
  test <- t.test(x1, x2)
  
  sprintf(
    "p value: %0.3f\nconfidence interval:[%0.2f, %0.2f]",
    test$p.value, test$conf.int[1], test$conf.int[2]
  )
}

ui <- fluidPage(
  fluidRow(
    column(9, plotOutput("hist", height="485px")),
    column(3, verbatimTextOutput("ttest"))
  ),
  fluidRow(
    column(4, 
           "Distribution 1",
           numericInput("n1", label = "Sample size", value = 2000, min = 1),
           numericInput("mean1", label = "Mean", value = 0, step = 0.1),
           numericInput("sd1", label = "Standart Deviation", value = 0.7, min = 0.1, step = 0.1)
    ),
    column(4, 
           "Distribution 2",
           numericInput("n2", label = "Sample size", value = 2000, min = 1),
           numericInput("mean2", label = "Mean", value = 0, step = 0.1),
           numericInput("sd2", label = "Standart Deviation", value = 0.7, min = 0.1, step = 0.1)
    ),
    column(4,
           "Histogram",
           numericInput("binwidth", label = "Bin width", value = 0.08, step = 0.1),
           sliderInput("range", label = "Range of vertical line", value = c(-3, 3), min = -7, max = 7)
    )
  )
  
)

server <- function(input, output, session) {
  x1 <- reactive(rnorm(input$n1, input$mean1, input$sd1))
  x2 <- reactive(rnorm(input$n2, input$mean2, input$sd2))
  
  output$hist <- renderPlot({
    histogram(x1(), x2(), binwidth = input$binwidth, xlim = input$range)
  }, res = 96)
  
  output$ttest <- renderText({
    t_test(x1(), x2())
  })
}

shinyApp(ui, server)