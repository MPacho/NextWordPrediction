
library(shiny)
source("04_stupid_backoff_model.R")

shinyServer(function(input, output) {
  
  text <-  reactive({
      input$typeText
    })

  prediction <- reactive({
    if (input$typeText == "")
      c("","","") 
    else 
      predictStupidBackoff(text())
    })
  
  output$wordPrediction1 <- renderText({
    prediction()[1]
  })
  
  output$wordPrediction2 <- renderText({
    prediction()[2]
  })
  
  output$wordPrediction3 <- renderText({
    prediction()[3]
  })
  
})

