x <- read.csv("SubsetExportTool/sources/questionIndex.csv")

##Create the "front" of a question that applies to all the qs in a matrix
x$QA <- gsub("(^.*)\\.\\.\\w*.*$", "\\1", x$Qualtrics)
x$QA <- gsub("\\.", " ", x$QA)
x$QA <- gsub(" - \\w{1}$", "", x$QA)

##Create the "end" of a question that has the specific items's details
x$QB <- gsub("^.*\\.\\.(\\w*.*$)", "\\1", x$Qualtrics)
x$QB <- gsub("\\.", " ", x$QB)

subqCategories <- c("SOE Outcomes", "SOC Enhanced Ability", "Student Development", "Counseling")