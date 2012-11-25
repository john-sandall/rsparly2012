library(shiny)

shinyUI(pageWithSidebar(

  # Application title
  headerPanel("Word Frequencies"),

  sidebarPanel(
	textInput("term", "Search for a term:", "tax")
 
  ),

  # Show the caption and plot of the requested variable against mpg
  mainPanel(
    plotOutput("wordPlot"),
    
    #verbatimTextOutput("summary")
    tableOutput("view")
  )
))