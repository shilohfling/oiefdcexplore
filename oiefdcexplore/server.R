library(shiny)
library(shinythemes)
library(ggplot2)
library(dplyr)
library(lubridate)

##Load data from export
##TODO: Shape the data into clean RDS for quick loading
data <- readRDS("~/Data/oiefdcexplore/data.RDS")

campus_choices <- unique(sort(data$`Campus.(not.scrubbed)`)) 
dept_choices <- unique(sort(data$`Department.(from.Recipients.and.Response.Rates.Data.Set)`))
major_choices <- unique(sort(data$`Major.1.(from.Recipients.and.Response.Rates.Data.Set)`))

membership <- data.frame(
  user = "xxx",
  password = "xxx")

shinyServer(function(input, output, session) {
        # Get the current user's username
        user <- reactive({
    
        curUser <- session$user
    
        # Not logged in. Shiny Server Pro should be configured to prevent this.
        if (is.null(curUser)){
          return(NULL)
        }
    
        # Look up the user in the database to load all the associated data.
        user <- as.data.frame(
            filter(membership_db, user==curUser)      
          )
    
        # No user in the database
        if (nrow(user) < 1){
            return(NULL)
          }
    
          user[1,]    
        })
        
        df <- reactive({
                head(data, input$nrows)
        })
        output$plot <- renderPlot({
                plot(df())
        })
        output$table <- renderTable({
                df()
        })
        # Filter data based on selections
        output$table <- DT::renderDataTable(DT::datatable({
                #data <- readRDS("~/Data/oiefdcexplore/data.RDS")
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
                })
        )
})



## After the subsetting option, I want to display a data table and then have the option to download as various file types
## https://shiny.rstudio.com/gallery/download-knitr-reports.html
## Future work will hopefully include some plotting options to help visualize the data further
## Hopefully it will also have to ask users to enter credentials before having access to the data like this example:
## https://shiny.rstudio.com/gallery/authentication-and-database.html
## https://gist.github.com/trestletech/9793754



