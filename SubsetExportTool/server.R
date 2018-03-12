##################################################
# This dashboard prototype is for subsetting
# data and creating customized reports through a 
# friendly Shiny based GUI.
##################################################
## Load packages
## Must already be installed in RStudio
library(shiny)
library(shinythemes)
library(DT)
library(ggplot2)
library(dplyr)
library(purrr)
#library(sjPlot)
#library(sjmisc)
library(rmarkdown)
library(tidyr)
library(openxlsx)

##################################################
## Load sources
source("functions.R")
source("questionIndex.R")
source("loadData.R")

##################################################
##Create subset choice vectors
school_choices <- unique(sort(data$X.School))
campus_choices <- c(unique(sort(data$X.Campus1)), "Select All")
dept_choices <- unique(sort(data$X.Dept))
major_choices <- unique(sort(data$X.Major))

##Make a named list of questions
question_choices <- questionsIndex %>% split(questionsIndex$Category) %>%
        map(~ as.character(.x$Shortname))

#Make a named vector to correlate input options to column name
question_shortname_column_index <- questionsIndex$Question
names(question_shortname_column_index) <- questionsIndex$Shortname

##################################################
## Beginning of server where most of the magic happens

shinyServer(function(input, output, session) {

        datasetInput <- reactive({
                DTX <- data
                
                ##Subsetting
                if("Select All" %in% input$campus) {
                      selected_campus_choices <- setdiff(campus_choices, "Select All")
                      updateSelectInput(session, "campus", selected = selected_campus_choices)
                }
                
                DTX <- DTX[DTX$X.School %in% input$school, ]
                
                if (!is.null(input$dept)){
                        DTX <- DTX[DTX$X.Dept %in% input$dept, ]
                }
                
                if (!is.null(input$major)){
                        DTX <- DTX[DTX$X.Major %in% input$major, ]   
                }
                
                if (!is.null(input$campus)){
                        DTX <- DTX[DTX$X.Campus1 %in% input$campus, ]
                }
                
                DTX
        })
        
        # observeEvent(input$downloadReport, {
        #         print(paste("Your report is downloading. Please wait."))
        # }, once = TRUE)
        
        output$mainbody <- renderUI({
                fluidPage(
                        ## Custom Webster branded theme
                        theme = "mystyle.css",
                        img(src="1711-campus-fall-88.jpg", alt = "Webster Hall Copyrighted Image"),
                        br(), br(),
                        titlePanel("Subset Export Tool"),
                        br(), br(),
                        
                        sidebarLayout(
                                 sidebarPanel(
                                         h3("Please select data: "),
                                         br(), hr(),
                                         selectInput(inputId = "school",
                                                     label = h5("School/ College:"),
                                                     choices = school_choices,
                                                     multiple = TRUE),
                                         selectInput(inputId = "dept",
                                                     label = h5("Department(s):"), 
                                                     choices = dept_choices,
                                                     multiple = TRUE),
                                         selectInput(inputId = "major",
                                                     label = h5("Major(s):"),
                                                     choices = major_choices,
                                                     multiple = TRUE),
                                         selectInput(inputId = "campus",
                                                     label = h5("Campus(es):"),
                                                     choices = campus_choices,
                                                     multiple = TRUE),
                                         hr(),
                                         downloadButton("downloadData", h5("Download Data")),
                                         downloadButton("downloadReport", h5("Download Report")),
                                         br(), br(),
                                         img(src="401px-Webster_University_Logo.svg.png", alt = "Webster University Logo")
                                 ),
                                 
                                mainPanel(
                                ## View the subsetted options into two tabs - Table and Preview report
                                tabsetPanel(type = "tabs",
                                            tabPanel("Data table", class = "one",
                                                     div(HTML("<br><h2><b><center>Outcomes Survey Responses</center></b></h2></br>")),
                                                     DT::dataTableOutput("table")),
                                            tabPanel("Preview Report", class = "one",
                                                     div(HTML("<br><h2><b><center>Outcomes Survey Responses</center></b></h2></br>")),
                                                     uiOutput("report"))
                                            )
                                )
                         )
                )
        })
        
        ##### Design for first tab (the data table) #####
        output$table <- DT::renderDataTable({
                DTpreview <- PreviewDT(datasetInput(), cnm)
                
                DT::datatable(DTpreview, select = "none",
                              options = list(lengthMenu = c(5, 10, 25, 50, 100), pageLength = 5),
                              rownames = FALSE)
        })
        
        
        ##### Design for second tab (the report) #####
        output$report <- renderUI({
              tagList(
                    ## Suppress warning messages  
                    tags$style(type="text/css",
                                 ".shiny-output-error { visibility: hidden; }",
                                 ".shiny-output-error:before { visibility: hidden; }"
                    ),
                    h4("Response Rates"), br(),
                    h5("Table 1. Outcomes Survey Response Rates by Program Level"), 
                    br(), 
                    tableOutput("table1"), 
                    hr(), 
                    
                    ## Start of Overall Satisfaction with Webster Section
                    h4("Overall Satisfaction with Webster"), br(),
                    h5("Table 2."), br(), tableOutput("table2"),
                    h6("Scale: Strongly Agree (4); Mostly Agree (3); Mostly Disagree (2); Strongly Disagree (1)"),
                    hr(),
                    ## End of Overall Satisfaction with Webster Section
                    
                    ## Start of Mission Outcomes Section
                    h4("Mission Outcomes"), br(),
                    h5("Table 3."), br(), tableOutput("table3"),
                    h6("Scale: Strongly Agree (4); Mostly Agree (3); Mostly Disagree (2); Strongly Disagree (1)"),
                    hr(), 
                    ## End of Mission Outcomes Section
                    
                    ## Start of Learning Environment Section
                    h4("Learning Environment"), br(),
                    h5("Table 4."), br(), tableOutput("table4"),
                    h6("Scale: Strongly Agree (4); Mostly Agree (3); Mostly Disagree (2); Strongly Disagree (1)"),
                    br(), br(),
                    h5("Table 5."), br(), tableOutput("table5"),
                    h6("Scale: Strongly Agree (4); Mostly Agree (3); Mostly Disagree (2); Strongly Disagree (1)"),
                    br(), br(),
                    h5("Table 6. (Undergraduates only)"), br(), tableOutput("table6"),
                    h6("Scale: Very Significant (4); Somewhat Significant (3); Not Very Significant (2); Not At All Significant (1)"),
                    br(), br(),
                    h5("Table 7."), br(), tableOutput("table7"),
                    h6("Scale: Very Satisfied (4); Mostly Satisfied (3); Mostly Dissatisfied (2); Very Dissatisfied (1)"),
                    hr(), 
                    ## End of Learning Environment Section
                    
                    ## Start of Career Outcomes Section
                    h4("Career Outcomes"), br(),
                    h5("Table 8."), br(), tableOutput("table8"),
                    hr(),
                    ## End of Career Outcomes Section
                    
                    ## Start of Career Outcomes - Employment Section
                    h4("Career Outcomes - Employment"), br(),
                    h5("Table 9."), br(), tableOutput("table9"),
                    h6("Scale: Strongly Agree (4); Mostly Agree (3); Mostly Disagree (2); Strongly Disagree (1)"), 
                    br(),
                    h6("Note: Averages are based on respondents who indicated they were employed full- or part-time as their primary activity."), 
                    br(), br(),
                    h5("Table 10."), br(), tableOutput("table10"), br(),
                    h6("Note: Percentages are based on respondents who indicated they were employed full- or part-time as their primary activity."),
                    br(), br(),
                    h5("Table 11."), br(), tableOutput("table11"), br(),
                    h6("Note: Percentages are based on respondents who indicated they were employed full- or part-time as their primary activity."),
                    br(), br(),
                    h5("Table 12."), br(), tableOutput("table12"), br(),
                    h6("Note: Averages are based on respondents who indicated they were employed full- or part-time as their primary activity."),
                    hr(),
                    ## End of Career Outcomes - Employment Section
                    
                    ## Start of Career Outcomes - Student/ Continuing Education Section ##
                    h4("Career Outcomes - Student/ Continuing Education"), br(),
                    h5("Table 13."), br(), tableOutput("table13"), br(),
                    h6("Note: Percentages are based on respondents who indicated student/ continuing education as their primary activity."),
                    br(), br(),
                    h5("Table 14."), br(), tableOutput("table14"), br(),
                    h6("Note: Percentages are based on respondents who indicated student/ continuing education as their primary activity."),
                    br(), br(),
                    h5("Table 15."), br(), tableOutput("table15"), br(),
                    h6("Note: Percentages are based on respondents who indicated student/ continuing education as their primary activity."),
                    hr(),
                    ## End of Career Outcomes - Student/ Continuing Education Section ##
                    
                    ## Start of Career Outcomes - Military Service ##
                    h4("Career Outcomes - Military Service"), br(),
                    h5("Table 16."), br(), tableOutput("table16"), br(),
                    h5("Table 17."), br(), tableOutput("table17"), br(),
                    h6("Note: Percentages are based on respondents who indicated they were serving or had served in the US military."),
                    br(), br(),
                    h5("Table 18."), br(), tableOutput("table18"), br(),
                    h6("Note: Percentages are based on respondents who indicated they were serving or had served in the US military."),
                    br(), br(),
                    h5("Table 19."), br(), tableOutput("table19"), br(),
                    h6("Note: Percentages are based on respondents who indicated they were serving or had served in the US military."),
                    br(), br(),
                    h5("Table 20."), br(), tableOutput("table20"), br(),
                    h6("Note: Averages are based on respondents who indicated they were serving or had served in the US military."),
                    br(), br(),
                    hr()
                    ## End of Career Outcomes - Military Service ##
                    )
        })
        
        output$plot <- renderPlot({
              if(nrow(datasetInput()) > 1 && length(input$questions) >= 1) {
                    testQ(datasetInput())
              }
        })
        
        ##### Table designs #####
        output$table1 <- renderTable({
                Table1(datasetInput(), cnm)
        })

        output$table2 <- renderTable({
                TableA(datasetInput(), paste0("Q", 101:102), questionsIndex, cnm)
        })
        
        output$table3 <- renderTable({
                TableA(datasetInput(), paste0("Q", 83:90), questionsIndex, cnm)
        })
        
        output$table4 <- renderTable({
                TableA(datasetInput(), paste0("Q", 71:73), questionsIndex, cnm)
        })
        
        output$table5 <- renderTable({
                TableA(datasetInput(), paste0("Q", 43:48), questionsIndex, cnm)
        })
        
        output$table6 <- renderTable({
                Table6(datasetInput(), paste0("Q", 74:82), questionsIndex, cnm)
        })
        
        output$table7 <- renderTable({
                TableC(datasetInput(), paste0("Q", 33:42), questionsIndex, cnm)
        })

        output$table8 <- renderTable({
                TableB(datasetInput(), paste0("Q", 19), questionsIndex, cnm)
        })
        
        output$table9 <- renderTable({
                TableA(datasetInput(), paste0("Q", 108:109), questionsIndex, cnm)
        })
        
        output$table10 <- renderTable({
                TableB(datasetInput(), paste0("Q", 111), questionsIndex, cnm)
        })
        
        output$table11 <- renderTable({
                TableB(datasetInput(), paste0("Q", 115), questionsIndex, cnm)
        })
        
        output$table12 <- renderTable({
                TableC(datasetInput(), paste0("Q", 118), questionsIndex, cnm)
        })
        
        output$table13 <- renderTable({
                TableB(datasetInput(), paste0("Q", 132), questionsIndex, cnm)
        })
        
        output$table14 <- renderTable({
                TableB(datasetInput(), paste0("Q", 138), questionsIndex, cnm)
        })
        
        output$table15 <- renderTable({
                TableB(datasetInput(), paste0("Q", 136), questionsIndex, cnm)
        })
        
        output$table16 <- renderTable({
                TableB(datasetInput(), paste0("Q", 148), questionsIndex, cnm)
        })
        
        output$table17 <- renderTable({
                TableB(datasetInput(), paste0("Q", 149), questionsIndex, cnm)
        })
        
        output$table18 <- renderTable({
                TableB(datasetInput(), paste0("Q", 151), questionsIndex, cnm)
        })
        
        output$table19 <- renderTable({
                TableB(datasetInput(), paste0("Q", 152), questionsIndex, cnm)
        })
        
        output$table20 <- renderTable({
                TableC(datasetInput(), paste0("Q", 155), questionsIndex, cnm)
        })
        
        ##### Download options #####
        
#         output$downloadReport <- withProgress(message = 'Making plot', value = 0, {
#                 n <- 2
#                 
#                 for(i in 1:n) {
#                         
#                 downloadHandler(
#                 filename = "myreportpdf.pdf",
#                 content = function(file) {
#                     out <- render("download_report.Rmd", pdf_document())
#                     file.rename(out, file)
#                                 }
#                 )
#                 }
#         }
# )
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

## End of server
##################################################
