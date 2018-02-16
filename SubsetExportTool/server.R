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
library(openxlsx)

source("functions.R")
source("questionIndex.R")
source("loadData.R")

##Create subset choice vectors
campus_choices <- c(unique(sort(data$X.Campus1)), "Select All")
dept_choices <- unique(sort(data$X.Dept))
major_choices <- unique(sort(data$X.Major))
school_choices <- unique(sort(data$X.School))

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
                        #theme = shinytheme("yeti"),
                        theme = "mystyle.css",
                        br(), br(),
                        titlePanel(div(HTML("<b><center>Outcomes Subset Export Tool</b></center>"))), br(), br(),
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
                                         downloadButton("downloadReport", h5("Download Report"))
                                 ),
                                 
                                mainPanel(
                                ## View the subsetted options into two tabs - Table and Plot
                                tabsetPanel(type = "tabs",
                                            tabPanel("Data table", class = "one",
                                                     div(HTML("<h2><b><center>AY 2016-2017 Responses</center></b></h2>")),
                                                     DT::dataTableOutput("table")),
                                            tabPanel("Report", class = "one",
                                                     div(HTML("<h2><b><center>AY 2016-2017 Responses</center></b></h2>")),
                                                     uiOutput("report"))
                                            )
                                )
                         )
                )
        })
        
        output$table <- DT::renderDataTable({
                DT::datatable(datasetInput(), select = "none",
                              options = list(lengthMenu = c(5, 10, 25, 50, 100), pageLength = 5))
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
        
        output$table4 <- renderTable({
                TableQ(datasetInput(), paste0("Q", 71:73))
        })
        
        output$table5 <- renderTable({
                TableQ(datasetInput(), paste0("Q", 43:48))
        })
        
        output$table6 <- renderTable({
                Table6(datasetInput(), paste0("Q", 74:82))
        })
        
        output$table7 <- renderTable({
                TableQ(datasetInput(), paste0("Q", 33:42))
        })
        
        output$table8 <- renderTable({
                TableQ(datasetInput(), paste0("Q", 105))
        })
        
        output$table9 <- renderTable({
                TableQ(datasetInput(), paste0("Q", 108:109))
        })
        
        output$report <- renderUI({
              tagList(
                    ## Suppress warning messages  
                    # tags$style(type="text/css",
                    #              ".shiny-output-error { visibility: hidden; }",
                    #              ".shiny-output-error:before { visibility: hidden; }"
                    # ),
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
                    hr()
                    ## End of Career Outcomes - Employment Section
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



