library(shiny)
library(shinythemes)
library(DT)
library(ggplot2)
library(dplyr)
library(purrr)

##Create a character vector of the columns to make available for export
cols <- c("Campus" = "Campus.(not.scrubbed)", 
          "Major" = "Major.1.(from.Recipients.and.Response.Rates.Data.Set)",
          "Department" = "Department.(from.Recipients.and.Response.Rates.Data.Set)",
          "StudentID" = "Student.ID.(Complete)")

questionsIndex <- read.csv("~/Data/oiefdcexplore/questionIndex.csv")

##Load the data object from disk
dataraw <- readRDS("~/Data/oiefdcexplore/data.RDS")

datanew <- dataraw[, cols]
colnames(datanew) <- names(cols)

data <- dataraw[, as.character(questionsIndex$Qualtrics)]
colnames(data) <- questionsIndex$Question
data <- cbind(datanew, data)

##Create subset choice vectors
campus_choices <- unique(sort(data$Campus)) 
dept_choices <- unique(sort(data$Department))
major_choices <- unique(sort(data$Major))

##Make a named list of questions
question_choices <- questionsIndex %>% split(questionsIndex$Category) %>%
        map(~ as.character(.x$Shortname))

shinyServer(function(input, output) {

        datasetInput <- reactive({
                DTX <- data
                
                DTX <- DTX[DTX$Campus %in% input$campus, ]
                
                if (!is.null(input$dept)){
                        DTX <- DTX[DTX$Department %in% input$dept, ]
                }
                
                if (!is.null(input$major)){
                        DTX <- DTX[DTX$Major %in% input$major, ]   
                }
                
                DTX
        })
        
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
                                         selectizeInput(inputId = "questions",
                                                     label = "Question(s):",
                                                     choices = question_choices,
                                                     multiple = TRUE),
                                         downloadButton("downloadData", "Download")
                                 ),
                                 
                                mainPanel(
                                ## View the subsetted options into two tabs - Table and Plot
                                tabsetPanel(type = "tabs",
                                             tabPanel("Data table", DT::dataTableOutput("table")),
                                             tabPanel("Plot", plotOutput("plot"))
                                        )
                                 )
                         )
                )
        })
        
        output$table <- DT::renderDataTable({
                DT::datatable(datasetInput(), select = "none",
                              options = list(lengthMenu = c(5, 10, 25, 50, 100), pageLength = 10))
        })
        
        output$info <- renderPrint ({
                selectedData()
        })
        
        output$plot <- renderPlot({
                ## Want to facet wrap this by selected questions
                ## Currently just facet wrap by campus for demonstration
                ggplot(datasetInput(), aes(`33`, fill = Department)) + geom_histogram() + xlim(0.5,4.5) + facet_wrap(~Campus)
        })
        
        output$value <- renderPrint({
                input$checkGroup
                })
        
        output$downloadData <- downloadHandler(
                filename = "mydownload.csv",
                content = function(file) {
                        write.csv(datasetInput(), file)
                }
        )
})
