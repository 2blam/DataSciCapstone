#setwd("C:/[DATA]/CapStone/shiny")
library(shiny)

shinyUI(
  pageWithSidebar(
    headerPanel("Data Science Capstone - Predictive Text Model"),
    
    sidebarPanel(
      textInput("phrase", label = "Please input a phrase (multiple words):", value=""),
      actionButton('submit', 'submit'),
      
      h4("Note:"),
      p("To save the computation time, only 1% of the data from the dataset (Coursera-SwiftKey.zip) was used in this application.")
    ),
    
    mainPanel(
      h5("Top 10 Next Prediction Word:"),
      htmlOutput('result')
    )        
  )
)