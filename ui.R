
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
shinyUI(fluidPage(
  sidebarLayout(
    sidebarPanel(
      h2("Test parameters"),
      p("Enter the counts associated with the sensitivity, specificity and prevalence of the test and if the result was positive or negative"),
      #code('install.packages("shiny")'),
      br(),
      
      fluidRow( column(6,numericInput("sensn", "sensitivity n", value = 50)),
                column(6,numericInput("sensk", "sensitivity k", value = 25))), 
      fluidRow( column(6,numericInput("specn", "specificity n", value = 50)),
                column(6,numericInput("speck", "specificity k", value = 25))),
      fluidRow( column(6,numericInput("prevn", "prevalence n", value = 50)),
                column(6,numericInput("prevk", "prevalence k", value = 25))),
    radioButtons("result", label = h3("Test result"),
                 choices = list("Positive" = "sick", "Negative" = "well"),selected = "sick")
    
  ),
  mainPanel(
    h1("Test Results"),
    p("See below for some details about your test result."),
#    h2("Your results"),
#    p("The RAMAS test result manager takes information from the location where you've taken a medical test, your genetic background, as well as the specific test you've taken and how old you are to determine your risk of being sick."),
#    p("You've given us some information, lets calculate how likely it is that you are sick or not:"),
    plotOutput("distPlot")
  )
)
))



