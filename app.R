library(shiny)
library(data.table)

# load data
DT <- fread('data/wages.csv')
setnames(DT, c("x", "y"))

# values that change
rv <- reactiveValues(
  train = NULL,
  test = NULL,
  fit = NULL
)

# evaluation functions
rmse <- function(y, yhat) {sqrt(mean((y-yhat)^2))}
mape <- function(y, yhat) {100*mean(abs(y-yhat)/y)}

ui <- fluidPage(
  titlePanel("Polynomial Regression Analyzer"),
  fluidPage(
    column(12,plotOutput("regression_plot")),    
    column(6, numericInput("seed", "Select Seed", value=0, min=0, max=Inf), align="left"),
    column(6, sliderInput("degree", "Degree of Polynomial:", value=1, min=0, max=10), align="right"),
    column(12, verbatimTextOutput("summary"))
  )
)


server <- function(input, output) {
  observeEvent(c(input$degree, input$seed), {
    
    # split data
    if (!is.integer(input$seed)) return()
    set.seed(input$seed)
    indices <- DT[, sample(.N, 0.8*.N)]
    rv$train <- DT[ indices]
    rv$test  <- DT[-indices]
    
    # reset variables features
    rv$train <- rv$train[, .(x, y)]
    rv$test <- rv$test[, .(x, y)]
    
    # create polynomial features
    for (k in 0:input$degree) {
      var_name <- paste0("x", k)
      set(rv$train, j=var_name, value=rv$train$x^k)
      set(rv$test,  j=var_name, value=rv$test$x^k)
    }
  }, priority=1)
  
  
  output$regression_plot <- renderPlot({
    
    # fit model
    rv$fit <- lm(y~.-x-1, rv$train)
    theta <- rv$fit$coefficients
    
    # plot fit
    x <- seq(0, 16, 0.1)     
    y <- Reduce(`+`, lapply(0:input$degree, function(k) theta[k+1] * x^k))  
    plot(x, y, type="l", xlim=c(1, 15), ylim=c(0,150000),
         xlab="Years of Experience", ylab="Salary ($)",
         main=sprintf("Polynomial Regression (degree=%d)", input$degree))
    
    # plot dataset
    points(rv$train[, .(x, y)], pch=16, col="red", cex=1.25)
    points(rv$test[,  .(x, y)], pch=4, col="blue", cex=1.25)
    grid()  
    
    # plot evaluation metrics
    preds <- predict(rv$fit, rv$test)
    text(x=1, y=140000, pos=4, paste(
      sprintf("Root Mean Squared Error (RMSE): %0.3f", rmse(rv$test$y, preds)),
      sprintf("Mean Absolute Precentage Error (MAPE): %0.3f%%", mape(rv$test$y, preds)),
      sep="\n"
    ))
  })
  
  # summarize fit
  output$summary <- renderPrint(summary(rv$fit))
}

shinyApp(ui = ui, server = server)

