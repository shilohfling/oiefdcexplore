library(shiny)
library(shinythemes)
library(ggplot2)

##Load data from export
##TODO: Shape the data into clean RDS for quick loading
data <- readRDS("~/Data/oiefdcexplore/data.RDS")

campus_choices <- unique(sort(data$`Campus.(not.scrubbed)`)) 
dept_choices <- unique(sort(data$`Department.(from.Recipients.and.Response.Rates.Data.Set)`))
major_choices <- unique(sort(data$`Major.1.(from.Recipients.and.Response.Rates.Data.Set)`))

shinyServer(
        function(input, output) {

                vals <- reactiveValues()
                vals$Data <- data
                
                output$mainbody <- renderUI({
                        fluidPage(
                                titlePanel("Outcomes Survey Results"),
                                theme = shinytheme("spacelab"),
                                #shinythemes::themeSelector(),##Enable theme selector widget
                                
                                tags$h1(HTML("Outcomes Survey Results")), 

                                fluidRow(
                                  column(4,
                                         selectInput("campus",
                                                     "Campus:",
                                                     c("All",
                                                       campus_choices))
                                  ),
                                  column(4,
                                         selectInput("dept",
                                                     "Department:",
                                                     c("All",
                                                       dept_choices))
                                  ),
                                  column(4,
                                         selectInput("major",
                                                     "Major:",
                                                     c("All",
                                                       major_choices))
                                  )
                                ),
                                # Create a new row for the table.
                                fluidRow(
                                  DT::dataTableOutput("table")
                                ),
                                
                                function(input, output) {

                                  # Filter data based on selections
                                  output$table <- DT::renderDataTable(DT::datatable({
                                    data <- readRDS("~/Data/oiefdcexplore/data.RDS")
                                    if (input$campus != "All") {
                                      data <- data[data$campus == input$campus,]
                                    }
                                    if (input$dept != "All") {
                                      data <- data[data$dept == input$dept,]
                                    }
                                    if (input$major != "All") {
                                      data <- data[data$major == input$major,]
                                    }
                                    data
                                  }))

                                }
                        )
                })
        })


## After the subsetting option, I want to display a data table and then have the option to download as various file types
## Future work will hopefully include some plotting options to help visualize the data further
## Hopefully it will also have to ask users to enter credentials before having access to the data like this example:
## https://shiny.rstudio.com/gallery/authentication-and-database.html
