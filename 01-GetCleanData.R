##Load Custom Functions
source("00-Functions.R")

##Load raw data from excel file
data_raw <- read.xlsx("../sources/RM_Outcomes Survey Data_2-20-2017_Working Response - macro.xlsm", 
                     "Response Data_Working")

##Create a blank data frame of student ids to put scrubbed data into
data_clean <- data.frame("id.student" = data_raw$`Student.ID.(Complete)`)

##Stuffing the data frame with demographics information
data_clean$program.code <- data_raw$Program.Code
data_clean$degree <- data_raw$`Degree.(not.scrubbed)`
data_clean$campus <- data_raw$`Combined.Campus.Location.(FOR.REPORTING)`
data_clean$major <- data_raw$`Major.1.(from.Recipients.and.Response.Rates.Data.Set)`
data_clean$department <- data_raw$`Department.(from.Recipients.and.Response.Rates.Data.Set)`
data_clean$school <- data_raw$`School.(not.scrubbed)`

##Add open-ended text questions to scrubbed data frame
data_clean$comment.overall <- as.character(data_raw[,168])
data_clean$comment.definingmoments <- as.character(data_raw[,103])
data_clean$comment.recommendations <- as.character(data_raw[,104])

