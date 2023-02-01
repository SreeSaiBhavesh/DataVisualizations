library(shiny)
#install.packages("shinydashboard")
library(shinydashboard)
shinyServer(pageWithSidebar(
  headerPanel("My First Application"),  # heading in one of the sidebar using headerPanel
  sidebarPanel(
    selectInput("Distribution", 'Please select distribution type', choices = c("Normal", "Exponential")),
    sliderInput("SampleSize", 'Please select Sample Size', min=100, max=5000, value=1000, step = 100),
    conditionalPanel(condition = "input.Distribution"=="Normal",textInput(
      "Mean","Pls select mean",10),textInput("StandardDeviation","pls select sd",3 )),
    conditionalPanel(condition = "Input.Distribution"=="Exponential", textInput("lamda", "Pls select Exp lambda", 1))
  ),
  mainPanel(plotOutput("myplot"))
))