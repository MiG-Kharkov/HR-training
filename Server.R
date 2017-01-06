library(shiny)
library(caret)


function(input, output) {
  
  dataset <- reactive({
    y_hat <- ifelse(ds$predicted > input$threshold, 1, 0)
    y_hat<-as.factor(y_hat)
    cm <- confusionMatrix(y_hat, ds$left)
  })
  
  output$plot <- renderPlot(
    ggplot(ds, aes(x = ds$predicted, y= ds$left ))+ 
      geom_point(color = ifelse(ds$predicted>input$threshold, 1,2) )
  )
  output$table <- renderDataTable(
    dataset()$table, 
    options = list(paging = FALSE, searching = FALSE)
  )
  
  output$txt1 <- renderText(
    paste("Accuracy with threshold ", input$threshold, " is ", 
          round(dataset()$overall["Accuracy"],3)
          )
  )
  
  output$txt2 <- renderText(
    paste("Kappa with threshold ", input$threshold, " is ", 
          round(dataset()$overall["Kappa"],3)
          )
  )
  
}