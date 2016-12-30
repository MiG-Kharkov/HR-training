library(shiny)
library(caret)


function(input, output) {
  
  dataset <- reactive({
    y_hat <- ifelse(ds$prediction > input$threshold, 1, 0)
    y_hat<-as.factor(y_hat)
    cm <- confusionMatrix(ds$left, y_hat)
  })
  output$table <- renderDataTable({
    dataset()$table
  })
  output$txt <- renderText(
    as.character(c("Accuracy ",dataset()$overall["Accuracy"],
                   "Kappa ", dataset()$overall["Kappa"]
                   ))
  )
}