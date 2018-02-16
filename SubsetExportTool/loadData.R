##Move this section to own file to source
##Load the data object from disk
data <- readRDS("sources/data.RDS")

##Set the column names from the index file
colnames(data) <- questionsIndex$Question

##Set column types, tidy, etc

