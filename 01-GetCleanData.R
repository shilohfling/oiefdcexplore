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


##Back up data frames for messing with
bananas <- data_clean
monkeysUncle <- name

##Trying to remove the NA values, but doesn't work because each one has a unique element id.
# monkeysUncle <- unique(monkeysUncle) 
monkeysUncle <- monkeysUncle[complete.cases(monkeysUncle), ]

##Exploring ways to best visualize the data
pltName <- ggplot(name, aes(sentiment,word_count, color = sentType)) + geom_point()
pltName         ##Not great. Really messy because of outliers.

pltMU <- ggplot(monkeysUncle, aes(word_count,sentiment, color = sentiment)) + geom_point()
pltMU           ##Hard to read

pltName2 <- ggplot(name, aes(sentScore, word_count, color=sentType)) + geom_point()
pltName2        ##Best representation of the data so far
                ##I want to explore a way to count the exact number of positive, negative and neutral
                ##I also think we should remove any comments that have literal "n/a" or wrote "none"
                ##Exploring a way to normalize the data and make sure it is scaled between -1 and +1


##Social Networking Analysis exploration
##packages sna and igraph will be used here
##Watched a Lynda tutorial on it



