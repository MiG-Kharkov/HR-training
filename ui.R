library(shiny)
library(caret)

fluidPage(
  
  title = "Confusion matrix",
  fluidRow(
    column(3,
           h4("Threshold"),
           sliderInput('threshold', 'Set Threshold', 
                       min=0.2, max=0.8,
                       value=0.5, 
                       step=0.01, round=0.01)
           
    )  
  ),
  # Create a new row for the table.
  fluidRow(
    titlePanel ("Confusion matrix"),
    dataTableOutput("table")
  ),
  # Summory information 
  fluidRow(
    textOutput("txt1"), 
    textOutput("txt2")
  )
)