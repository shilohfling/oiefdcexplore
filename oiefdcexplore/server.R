library(shiny)
library(shinythemes)
library(ggplot2)
library(dplyr)
library(lubridate)
library(rmarkdown)

##Load data from export
##TODO: Shape the data into clean RDS for quick loading
data <- readRDS("~/Data/oiefdcexplore/data.RDS")

campus_choices <- unique(sort(data$`Campus.(not.scrubbed)`)) 
dept_choices <- unique(sort(data$`Department.(from.Recipients.and.Response.Rates.Data.Set)`))
major_choices <- unique(sort(data$`Major.1.(from.Recipients.and.Response.Rates.Data.Set)`))

## Password options for authentication screen
# membership <- data.frame(
#   user = "xxx",
#   password = "xxx")

shinyServer(function(input, output, session) {
        ## Authentication function, still working on it
        # # Get the current user's username
        # user <- reactive({
        # 
        # curUser <- session$user
        # 
        # # Not logged in. Shiny Server Pro should be configured to prevent this.
        # if (is.null(curUser)){
        #   return(NULL)
        # }
        # 
        # # Look up the user in the database to load all the associated data.
        # user <- as.data.frame(
        #     filter(membership_db, user==curUser)      
        #   )
        # 
        # # No user in the database
        # if (nrow(user) < 1){
        #     return(NULL)
        #   }
        # 
        #   user[1,]    
        # })
        
        ## Download the data table as excel, docx, csv, or pdf file
        output$downloadReport <- downloadHandler(
                filename = function() {
                        paste('my-report', sep = '.', switch(
                                input$format, PDF = 'pdf', CSV = 'csv', Word = 'docx', Excel = "xlsx"
                        ))
                },
                
                content = function(file) {
                        src <- normalizePath('report.Rmd')
                        
                        # temporarily switch to the temp dir, in case you do not have write
                        # permission to the current working directory
                        owd <- setwd(tempdir())
                        on.exit(setwd(owd))
                        file.copy(src, 'report.Rmd', overwrite = TRUE)
                        
                        library(rmarkdown)
                        out <- render('report.Rmd', switch(
                                input$format,
                                PDF = pdf_document(), HTML = html_document(), Word = word_document()
                        ))
                        file.rename(out, file)
                }
        )
        
        ## Make reactive data table
        df <- reactive({
                head(data, input$nrows)
                })
        
        ## Display plot
        output$plot <- renderPlot({
                plot(df())
                })
        ## Display data table
        output$table <- renderTable({
                df()
                })
        
        ## Filter data based on selections
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




