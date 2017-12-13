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

ui <- dashboardPage(
        skin="blue",
  dashboardHeader(
          title = "Survey Results"
          ),
  dashboardSidebar(
          sidebarMenu(
                  menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
                  menuItem("Results", tabName = "widgets", icon = icon("th")),
                  menuItem("Sign In", tabName = "widgets", icon = icon("th"))
          )
    ),
  dashboardBody(
          tabItems(
                  # First tab content
                  tabItem(tabName = "results",
                          fluidRow(
                                  box(plotOutput("plot1", height = 250)),
                                  
                                  box(
                                          title = "Controls",
                                          sliderInput("slider", "Number of observations:", 1, 100, 50)
                                  )
                          )
                  ),
                  
                  # Second tab content
                  tabItem(tabName = "widgets",
                          h2("Sign in")
                  )
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
