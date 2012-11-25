library(shiny)

shinyUI(pageWithSidebar(

  # Application title
  headerPanel("MP Topics"),

  sidebarPanel(
	textInput("mp", "Search for an MP:", "Paul Burstow")
 
  ),

  # Show the caption and plot of the requested variable against mpg
  mainPanel(
    plotOutput("wordPlot")
  )
))