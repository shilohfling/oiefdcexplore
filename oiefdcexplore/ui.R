#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)

ui <- dashboardPage((skin="blue"),
  dashboardHeader("Survey Results"),
  dashboardSidebar(
    fluidRow(
      h2("Choose demographics")
    )
  ),
  dashboardBody(
    fluidRow(
      h1("School/College")
    ),
    fluidRow(
      h1("Department")
    ),
    fluidRow(
      h1("Major")
    )
  )
)


# # Define UI for application that draws a histogram
# shinyUI(fluidPage(
#   
#   # Application title
#   titlePanel("Survey Results"),
#   
#   # Sidebar with a slider input for number of bins 
#   sidebarLayout(
#     sidebarPanel(
#        sliderInput("bins",
#                    "Number of bins:",
#                    min = 1,
#                    max = 50,
#                    value = 30)
#     ),
#     
#     # Show a plot of the generated distribution
#     mainPanel(
#        plotOutput("distPlot")
#     )
#   )
# ))
