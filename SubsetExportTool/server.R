library(shiny)
library(shinythemes)
library(DT)
library(ggplot2)
library(dplyr)

##Create a character vector of the columns to make available for export
cols <- c("Campus" = "Campus.(not.scrubbed)", 
          "Major" = "Major.1.(from.Recipients.and.Response.Rates.Data.Set)",
          "Department" = "Department.(from.Recipients.and.Response.Rates.Data.Set)",
          "StudentID" = "Student.ID.(Complete)",
          "Q33" = "Please.rate.your.level.of.satisfaction.with.the.following.Webster.University.processes..-.a..Applying.to.and.being.admitted.by.Webster")

##Load the data object from disk
data <- readRDS("~/Data/oiefdcexplore/data.RDS")
data <- data[, cols]
colnames(data) <- names(cols)

##Create subset choice vectors
campus_choices <- unique(sort(data$Campus)) 
dept_choices <- unique(sort(data$Department))
major_choices <- unique(sort(data$Major))

shinyServer(function(input, output) {
        vals <- reactiveValues()
        vals$data <- data
        
        output$mainbody <- renderUI({
                fluidPage(
                        theme = shinytheme("yeti"),
                        #shinythemes::themeSelector(),
                        title = "Outcomes Subset Export Tool",
                        
                        HTML("Outcomes Subset Export Tool"),
                        
                        sidebarLayout(
                                 sidebarPanel(
                                         selectInput(inputId = "campus",
                                                     label = "Campus(es):",
                                                     choices = campus_choices,
                                                     multiple = TRUE),
                                         selectInput(inputId = "dept",
                                                     label = "Department(s):", 
                                                     choices = dept_choices,
                                                     multiple = TRUE),
                                         selectInput(inputId = "major",
                                                     label = "Major(s):",
                                                     choices = major_choices,
                                                     multiple = TRUE)
                                 ),
                                 
                                 mainPanel(
                                ## View the subsetted options into two tabs - Table and Plot
                                tabsetPanel(type = "tabs",
                                             tabPanel("Data table",
                                                     DT::dataTableOutput("table")
                                             ),

                                             tabPanel("Plot", plotOutput("plot"))

                                 )
                                 
                                 )
                         )
                )
        })
        
        output$table <- DT::renderDataTable({
                DT <- vals$data
                
                DT <- DT[DT$Campus %in% input$campus, ]
                
                if (!is.null(input$dept)){
                        DT <- DT[DT$Department %in% input$dept, ]
                }

                if (!is.null(input$major)){
                        DT <- DT[DT$Major %in% input$major, ]   
                }

                DT::datatable(DT, select = "none",
                              extensions = "Buttons",
                              options = list(dom = "Bfrtip",
                              buttons = c("csv", "excel", "pdf", "print")))
        })
        
        output$plot <- renderPlot({
                DT <- vals$data
                
                DT <- DT[DT$Campus %in% input$campus, ]
                
                if (!is.null(input$dept)){
                        DT <- DT[DT$Department %in% input$dept, ]
                }
                
                if (!is.null(input$major)){
                        DT <- DT[DT$Major %in% input$major, ]   
                }
                
                ggplot(DT, aes(Q33, fill = Campus)) + geom_histogram() + xlim(0.5,4.5)
        })
        
})
