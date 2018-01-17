library(shiny)
library(shinythemes)

##Load data from export
##TODO: Shape the data into clean RDS for quick loading
data <- readRDS("~/Data/oiefdcexplore/data.RDS")

campus_choices <- unique(sort(data$`Campus.(not.scrubbed)` )) 
dept_choices <- unique(sort(data$`Department.(from.Recipients.and.Response.Rates.Data.Set)`))
major_choices <- unique(sort(data$`Major.1.(from.Recipients.and.Response.Rates.Data.Set)`))

##Which columns to include in the out DT
#cols <- c(
#          )

shinyServer(
        function(input, output) {

                vals <- reactiveValues()
                vals$Data <- data
                
                output$mainbody <- renderUI({
                        fluidPage(
                                theme = shinytheme("yeti"),
                                #shinythemes::themeSelector(),##Enable theme selector widget
                                
                                title = "Outcomes Survey Results",
                                
                                tags$h1(HTML("Outcomes Survey Results")), 
                                
                                selectInput("dept", label = h3("Choose the Campus(es)"), 
                                            choices = campus_choices, 
                                            multiple = TRUE),
                                selectInput("dept", label = h3("Choose the Department(s)"), 
                                            choices = dept_choices, 
                                            multiple = TRUE),
                                selectInput("dept", label = h3("Choose the Major(s)"), 
                                            choices = major_choices, 
                                            multiple = TRUE),
                                # function(input, output) {
                                #         output$plot1 <- renderPlot({
                                #                 hist(rnorm(input$n))
                                #         })
                        )
                })
        })
