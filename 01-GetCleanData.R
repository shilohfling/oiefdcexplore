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





######EXPLORATION#########
sentdf <- makeSentenceDF(data_clean, "comment.overall")

TWlist <- addSentimentTW(sentlist, mykey)
name <- cbind(sentlist, TWlist)
results <- textaSentiment(comment[[2]]) ##From the mscstexta4r package



##Social Networking Analysis exploration
##packages sna and igraph will be used here
##Watched a Lynda tutorial on it




