##################################################
# This dashboard prototype is for subsetting
# data and creating customized reports through a 
# friendly Shiny based GUI.
##################################################

## Read data set 
x <- read.xlsx("sources/questionIndex.xlsx")

##Create the "front" of a question that applies to all the qs in a matrix
x$QA <- gsub("(^.*)\\.\\.\\w*.*$", "\\1", x$Qualtrics)
x$QA <- gsub("\\.", " ", x$QA)
x$QA <- gsub(" - \\w{1}$", "", x$QA)

##Create the "end" of a question that has the specific items's details
x$QB <- gsub("^.*\\.\\.(\\w*.*$)", "\\1", x$Qualtrics)
x$QB <- gsub("\\.", " ", x$QB)

questionsIndex <- x
remove(x)

subqCategories <- c("SOE Outcomes", "SOC Enhanced Ability", "Student Development", "Counseling")