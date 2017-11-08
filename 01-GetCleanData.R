##Load packages
library(openxlsx)
library(mscstexta4r)
library(httr)
library(sentimentr) ##Calculate Text Polarity Sentiment

#Load Configuration Details

##Initializes mscstexta4r with URL and Key passed here from file .mscskeys.json
textaInit()

##Load raw data from excel file
data_raw <- read.xlsx("../sources/RM_Outcomes Survey Data_2-20-2017_Working Response - macro.xlsm", 
                     "Response Data_Working")

##Create a blank data frame of student ids to put scrubbed data into
data_clean <- data.frame("id.student" = data_raw$`Student.ID.(Complete)`)

##Add open-ended text questions to scrubbed data frame
data_clean$comment.overall <- as.character(data_raw[,168])



######EXPLORATION#########



rsent <- sentiment(data_clean$comment.overall)
sentences <- get_sentences(rsent)


results <- textaSentiment(comment[[2]]) ##From the mscstexta4r package

