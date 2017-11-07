library(shiny)
library(data.table)

# load data
DT <- fread('data/wages.csv', select=c("experience", "salary"))
setnames(DT, c("experience", "salary"), c("x", "y"))


# values that change
rv <- reactiveValues(
  train = NULL,
  test = NULL
)

# evaluation functions
rmse <- function(y, yhat) {sqrt(mean((y-yhat)^2))}

ui <- fluidPage(
  tags$head(tags$style("#regression_plot{height: 75vh !important;}")),
  titlePanel("Polynomial Regression Analyzer"),
  fluidPage(
    column(12,plotOutput("regression_plot")),    
    column(1),
    column(5, numericInput("seed", "Select Seed", value=1, min=1, max=Inf), align="left"),
    column(5, sliderInput("degree", "Degree of Polynomial:", value=1, min=1, max=20), align="right"),
    column(1)
  )
)


server <- function(input, output) {
  # split data
  observeEvent(c(input$degree, input$seed), {
    if (!is.integer(input$seed)) return()
    set.seed(input$seed)
    indices <- DT[, sample(.N, 0.8*.N)]
    rv$train <- DT[ indices]
    rv$test  <- DT[-indices]
  }, priority=1)
  
  
  output$regression_plot <- renderPlot({
    
    # fit model and make predictions
    fit <- lm(y~poly(x, input$degree), rv$train)
    
    # plot fit
    par(cex=1.25)
    y <- predict(fit, data.table(x <- seq(0, 16, 0.01)))
    plot(x, y, type="l", lwd=2, xlim=c(1, 13), ylim=c(0,150000),
         xlab="Years of Experience", ylab="Salary ($)",
         main=sprintf("Polynomial Regression (degree=%d)", input$degree))
    grid()  
    
    # plot dataset
    points(rv$train, pch=3,  col="blue")
    points(rv$test,  pch=16, col="red")
    legend(x=1, y=1.25e5, lwd=2, lty=c(1, NA, NA), pch=c(NA, 3, 16),
           col=c("black", "blue", "red"), legend=c(
             sprintf("Polynomial Regression (degree=%d)", input$degree),
             "Training Data",
             "Validation Data"
           )
    )
    
    # plot evaluation metrics
    preds_train <- predict(fit, rv$train)
    text(x=0.9, y=1.45e5, pos=4, col="blue",
         sprintf("Training Root Mean Squared Error (RMSE): %0.3f", rmse(rv$train$y, preds_train)))
    
    preds_test <- predict(fit, rv$test)
    text(x=0.9, y=1.35e5, pos=4, col="red",
         sprintf("Validation Root Mean Squared Error (RMSE): %0.3f", rmse(rv$test$y, preds_test)))
    
    # error guides
    lines(rv$test[, .(rep(x, each=3), c(rbind(preds_test, y, NA)))], col="red")
  })
}

shinyApp(ui = ui, server = server)