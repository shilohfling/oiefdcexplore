library(shiny)
library(shinythemes)
library(DT)
library(ggplot2)
library(dplyr)
library(purrr)
library(sjPlot)
library(sjmisc)
library(rmarkdown)
library(tidyr)

source("functions.R")

##Create a character vector of the columns to make available for export
cols <- c("School" = "School.(not.scrubbed)",
          "Department" = "Department.(from.Recipients.and.Response.Rates.Data.Set)",
          "Major" = "Major.1.(from.Recipients.and.Response.Rates.Data.Set)",
          "Program.Level" = "Program.Code",
          "Finished" = "Finished",
          "Campus" = "Campus.(not.scrubbed)",
          "Degree" = "Degree.(not.scrubbed)")

questionsIndex <- read.csv("sources/questionIndex.csv")
##questionsIndex <- read.csv("~/Data/oiefdcexplore/questionIndex.csv")

##Load the data object from disk
#dataraw <- readRDS("~/Data/oiefdcexplore/data.RDS")
dataraw <- readRDS("sources/data.RDS")

datanew <- dataraw[, cols]
colnames(datanew) <- names(cols)

data <- dataraw[, 33:102]
colnames(data) <- questionsIndex$Question
#data <- lapply(data, as.factor)
data <- cbind(datanew, data)


##Create subset choice vectors
campus_choices <- c(unique(sort(data$Campus)), "Select All")
dept_choices <- unique(sort(data$Department))
major_choices <- unique(sort(data$Major))
school_choices <- unique(sort(data$School))

##Make a named list of questions
question_choices <- questionsIndex %>% split(questionsIndex$Category) %>%
        map(~ as.character(.x$Shortname))

#Make a named vector to correlate input options to column name
question_shortname_column_index <- questionsIndex$Question
names(question_shortname_column_index) <- questionsIndex$Shortname





shinyServer(function(input, output, session) {

        datasetInput <- reactive({
                DTX <- data
                
                ##Subsetting
                if("Select All" %in% input$campus) {
                      selected_campus_choices <- setdiff(campus_choices, "Select All")
                      updateSelectInput(session, "campus", selected = selected_campus_choices)
                }
                
                DTX <- DTX[DTX$School %in% input$school, ]
                
                if (!is.null(input$dept)){
                        DTX <- DTX[DTX$Department %in% input$dept, ]
                }
                
                if (!is.null(input$major)){
                        DTX <- DTX[DTX$Major %in% input$major, ]   
                }
                
                if (!is.null(input$campus)){
                        DTX <- DTX[DTX$Campus %in% input$campus, ]
                }
                
                ##Exract the questions and join back the ones select
                #DTXquestions <- DTX %>% select(starts_with("Q"))
                #DTX <- DTX %>% select(-starts_with("Q"))
                
                #DTXquestions <- DTXquestions[, question_shortname_column_index[input$questions]]
                
                ##If the length is one, the vector is returned and the name of the object is the colname
                ##This isn't elegant, but lets us control the colname of a single question
                #if(length(input$questions) == 1) {
                #      DTX <- cbind(DTX, "Q" = DTXquestions)
                #} else {
                #      DTX <- cbind(DTX, DTXquestions)
                #}
                
                DTX
        })
        
        output$mainbody <- renderUI({
                fluidPage(
                        #shinythemes::themeSelector(),
                        theme = shinytheme("yeti"),
                        titlePanel("Outcomes Subset Export Tool"),
                        
                        sidebarLayout(
                                 sidebarPanel(
                                         selectInput(inputId = "school",
                                                     label = "School/ College:",
                                                     choices = school_choices,
                                                     multiple = TRUE),
                                         selectInput(inputId = "dept",
                                                     label = "Department(s):", 
                                                     choices = dept_choices,
                                                     multiple = TRUE),
                                         selectInput(inputId = "major",
                                                     label = "Major(s):",
                                                     choices = major_choices,
                                                     multiple = TRUE),
                                         hr(),
                                         selectInput(inputId = "campus",
                                                     label = "Campus(es):",
                                                     choices = campus_choices,
                                                     multiple = TRUE),
                                         hr(),
                                         selectizeInput(inputId = "questions",
                                                     label = "Question(s):",
                                                     choices = question_choices,
                                                     multiple = TRUE),
                                         downloadButton("downloadData", "Download Data"),
                                         downloadButton("downloadReport", "Download Report")
                                 ),
                                 
                                mainPanel(
                                ## View the subsetted options into two tabs - Table and Plot
                                tabsetPanel(type = "tabs",
                                            tabPanel("Data table", DT::dataTableOutput("table")),
                                            tabPanel("Report", uiOutput("report")),
                                            tabPanel("Likert Plot", plotOutput("plot"))
                                        )
                                 )
                         )
                )
        })
        
        output$table <- DT::renderDataTable({
                DT::datatable(datasetInput(), select = "none",
                              options = list(lengthMenu = c(5, 10, 25, 50, 100), pageLength = 10))
        })
        
        output$plot <- renderPlot({
              if(nrow(datasetInput()) > 1 && length(input$questions) >= 1) {
                    testQ(datasetInput())
              }
        })
        
        output$table1 <- renderTable({
                Table1(datasetInput())
        })

        output$table2 <- renderTable({
                TableQ(datasetInput(), c("Q101", "Q102"))
        })
        
        output$table3 <- renderTable({
              TableQ(datasetInput(), paste0("Q", 83:90))
        })
        
        output$report <- renderUI({
              tagList(
                    h4("Response Rates"), 
                    "Table 1. Outcomes Survey Response Rates by Program Level", 
                    br(), 
                    tableOutput("table1"), 
                    hr(), 
                    h4("Overall Satisfaction with Webster"),
                    "Table 2.", br(), tableOutput("table2"),
                    h4("Mission Outcomes"),
                    "Table 3.", br(), tableOutput("table3")
                    )
        })
        
        output$downloadReport <- downloadHandler(
              filename = "myreportpdf.pdf",
              content = function(file) {
                    out <- render("download_report.Rmd", pdf_document())
                    file.rename(out, file)
              }
        )
        
        output$downloadData <- downloadHandler(
                filename = "mydownload.csv",
                content = function(file) {
                        write.csv(datasetInput(), file)
                }
        )
})
