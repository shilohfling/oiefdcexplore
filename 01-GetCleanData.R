##Load packages
library(openxlsx)
library(mscstexta4r)
library(httr)
#library(sentimentr) ##A package I am exploring
                     ##Calculate Text Polarity Sentiment

##Call the API by logging in with the URL and Key
##Values passed here from file .mscskeys.json
textaInit()

##Load raw data from excel file
data_raw <- read.xlsx("../sources/RM_Outcomes Survey Data_2-20-2017_Working Response - macro.xlsm", 
                     "Response Data_Working")

##Create a blank data frame to put scrubbed data into
data_clean <- as.data.frame(matrix(1, nrow(data_raw)))

##Make a new column in the "clean" data frame that holds the qualitative data
data_clean$comment.overall <- as.character(data_raw[,168])

##This needs to be developed and wrapped into a function
comment <- data_clean$comment.overall[16:17]

comment <- strsplit(comment, "\\.|\\?|\\!")
comment <- lapply(comment, function(x) x <- x[!x == ""])

results <- textaSentiment(comment[[2]])

##Develop a function that compares the twinword sentiment API to the Azure sentiment API
# df <- df$twinword
# df <- df$azure
# df$twinword <- getTwinwordSentiment(as.data.frame(comment[[2]]))
# df$azure <- textaSentiment(comment[[2]])

##This function from the sentimentr package determines polarity from -1 to +1
##It also breaks paragraphs and scores each sentenece
##It also counts the words
##I would like to use this already developed function instead of writing a new one
sentiment(data_clean$comment.overall[16:17])
