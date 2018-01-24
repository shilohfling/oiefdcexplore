library(shiny)
library(shinythemes)
library(ggplot2)

shinyUI(fluidPage(
        theme = shinytheme("yeti"),
        titlePanel("Student Outcomes Survey Results"),
        sidebarLayout(
                sidebarPanel(
                        selectInput(inputId = "campus",
                                    label = "Campus(es):",
                                    choices = c("All", campus_choices),
                                    multiple = TRUE),
                        selectInput(inputId = "dept",
                                    label = "Department(s):", 
                                    choices = c("All", dept_choices),
                                    multiple = TRUE),
                        selectInput(inputId = "major",
                                    label = "Major(s):",
                                    choices = c("All",major_choices),
                                    multiple = TRUE),
                        downloadButton('downloadReport')
                ),
                mainPanel(
                        uiOutput("table")
                        )
                )
        )
)