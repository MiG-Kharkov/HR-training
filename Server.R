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
    paste("Accuracy is ", 
          round(dataset()$overall["Accuracy"],5)
          )
  )
  
  output$txt2 <- renderText(
    paste("Kappa with threshold is ", 
          round(dataset()$overall["Kappa"],5)
          )
  )

  output$txt3 <- renderText(
    paste("Sensitivity is ", 
          round(dataset()$byClass[1],5)
    )
  )
  
  output$txt4 <- renderText(
    paste("Specificity is ", 
          round(dataset()$byClass[2],5)
    )
  )
  
}