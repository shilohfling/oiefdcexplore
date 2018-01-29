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
          "Q33" = "Please.rate.your.level.of.satisfaction.with.the.following.Webster.University.processes..-.a..Applying.to.and.being.admitted.by.Webster",
          "Q34" = "Please.rate.your.level.of.satisfaction.with.the.following.Webster.University.processes..-.b..Initial.orientation.to.Webster.and.my.program.of.study",
          "Q35" = "Please.rate.your.level.of.satisfaction.with.the.following.Webster.University.processes..-.c..Academic.advising")

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
                        titlePanel("Outcomes Subset Export Tool"),
                        
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
                                                     multiple = TRUE),
                                         ## Trying to figure out how to add the questions
                                         ## to be selected by check box
                                         ## Hopefully can wrap this inside of a drop down menu
                                         # conditionalPanel(
                                         #         'input.dataset === "data"',
                                         #         checkboxGroupInput("show_vars", "Select question(s):",
                                         #                            names(data), selected = names(data))
                                         # )
                                         downloadButton("downloadData", "Download")
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
                              options = list(lengthMenu = c(5, 10, 25, 50, 100), pageLength = 10))
        })
        
        selectedData <- reactive ({
                switch(data,
                       "Campus" = campus_choices,
                       "Department" = dept_choices,
                       "Major" = major_choices)
        })
        
        output$info <- renderPrint ({
                selectedData()
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
                
                ## Want to facet wrap this by selected questions
                ## Currently just facet wrap by campus for demonstration
                ggplot(DT, aes(Q33, fill = Department)) + geom_histogram() + xlim(0.5,4.5) + facet_wrap(~Campus)
        })
        
        output$downloadData <- downloadHandler(
                filename = function() {
                        paste("Outcomes", ".csv", sep = "")
                },
                content = function(file) {
                        write.csv(selectedData(), file)
                }
        )
        
        ## Trying to figure out how to add the questions 
        # data2 = data[sample(nrow(data), 1000), ]
        # output$mytable1 <- DT::renderDataTable({
        #         DT::datatable(data2[, input$show_vars, drop = FALSE])
        # })
        
})
