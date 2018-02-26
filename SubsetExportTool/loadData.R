##################################################
# This dashboard prototype is for subsetting
# data and creating customized reports through a 
# friendly Shiny based GUI.
##################################################

##Load the data object from disk
data <- readRDS("sources/data.RDS")

##Set the column names from the index file
colnames(data) <- questionsIndex$Question

##Set column types, tidy, etc

