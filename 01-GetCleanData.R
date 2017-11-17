##Load packages

##Package for opening/ reading Excel files
library(openxlsx)
##Microsoft Sentiment Analysis API Package
library(mscstexta4r)
##Used to for working with URLs and HTTP
library(httr)
##Calculate Text Polarity Sentiment
library(sentimentr) 
##These two packages are for social network analysis
library(sna)
library(igraph)

##Load Configuration Details

##Initializes mscstexta4r with URL and Key passed here from file .mscskeys.json
textaInit()

##Load raw data from excel file
data_raw <- read.xlsx("../sources/RM_Outcomes Survey Data_2-20-2017_Working Response - macro.xlsm", 
                     "Response Data_Working")

##Create a blank data frame of student ids to put scrubbed data into
data_clean <- data.frame("id.student" = data_raw$`Student.ID.(Complete)`)

##Add open-ended text questions to scrubbed data frame
data_clean$comment.overall <- as.character(data_raw[,168])





####################EXPLORATION####################
##Make a new data frame of only the comments from the survey
sentdf <- makeSentenceDF(data_clean, "comment.overall")

##Because of the nature of the data set, there were comments where people responded with "No" or "N/A"
##This caused confusion in the algorithm, so we set a threshold in order to change them to NA values
##We did this because we weren't gaining sentiment from the comments anyways 
##and it would save on the number of calls we made to the Microsoft API, which costs money after 5,000 calls
sentdf$sentences[nchar(sentdf$sentences) < 4] <- NA ## character length < 4 was our threshold because it only changed comments that were short and lacked sentiment


TWdf <- addSentimentTW(sentdf, mykey)           ##Twinword dataframe and call
sentdf <- cbind(sentdf, TWdf)                   ##Binding the twinword sentiment scores back to the dataframe
MSdf <- getSentimentMS(sentdf$sentences[1:100]) ##Microsoft Azure sentiment call. Limit 100 per minute.
sentdf <- cbind(sentdf, MSdf)                   ##Bind the Azure sentiment data to the data frame

##Back up data frame for testing
testMSdf <- as.data.frame(MSdf)
##Rounding the sentiment scores 
testMSdf <- round(testMSdf$MSscore,digits = 3)

