##Load packages

##Package for opening/ reading Excel files
library(openxlsx)
##Microsoft Sentiment Analysis API Package
library(mscstexta4r)
##Used for working with URLs and HTTP
library(httr)
##Calculate Text Polarity Sentiment
library(sentimentr) 
##These two packages are for social network analysis
library(sna)
library(igraph)
##A package used for data frame manipulation
library(dplyr)

##Load Configuration Details

##Initializes mscstexta4r with URL and Key passed here from file .mscskeys.json
textaInit()

##Load raw data from excel file
data_raw <- read.xlsx("../sources/RM_Outcomes Survey Data_2-20-2017_Working Response - macro.xlsm", 
                     "Response Data_Working")

##Add open-ended text questions to scrubbed data frame
data_clean$comment.overall <- as.character(data_raw[,168])
##Create a blank data frame of student ids to put scrubbed data into
data_clean <- data.frame("id.student" = data_raw$`Student.ID.(Complete)`)
data_clean$comment.overall <- data_raw$`The.following.is.the.final.survey.question..Choosing.next.after.the.question.will.submit.your.responses..Final.Question:.Do.you.have.any.recommendations.to.improve.the.overall.Webster.experience.or.final.comments.you.would.like.to.share?`

##Because of the nature of the data set, there were comments where people responded with "No" or "N/A"
##This caused confusion in the algorithm, so we set a threshold in order to change them to NA values
##We did this because we weren't gaining sentiment from the comments anyways 
##and it would save on the number of calls we made to the Microsoft API, which costs money after 5,000 calls
sentdf$sentences[nchar(sentdf$sentences) < 4] <- NA ## character length < 4 was our threshold because it only changed comments that were short and lacked sentiment


TWdf <- addSentimentTW(sentdf, mykey)           ##Twinword dataframe and call
sentdf <- cbind(sentdf, TWdf)                   ##Binding the twinword sentiment scores back to the dataframe
MSdf <- getSentimentMS(sentdf$sentences)        ##Microsoft Azure sentiment call. Limit 100 per minute.
sentdf <- cbind(sentdf, MSdf)                   ##Bind the Azure sentiment data to the data frame

saveRDS(sentdf, "comment.overall.RDS")          ##Save the sentiment data frame as an RDS 
write.csv(sentdf, "comment.overall.csv")        ##Save the sentiment data frame as a .csv file

##A function that asks for a data frame and the maximum rows in the data frame and adds an element ID
##This will be used to rejoin data frames
elementID <- function(df, maxID) {
        n <- 0
        data_clean$element_id <- NA
        while (n < maxID) {
                n <- n + 1
                data_clean$element_id[[n]]<- n
                print(n)
        }
        return(data_clean)
}
data_clean <- elementID(data_clean, 6227)
##Need to join sentdf and data_clean using dplyr
##Need to be brought together through the common element ID since the sentences are parsed
        
sentdf <- makeSentenceDF(data_clean, "comment.overall")








####################EXPLORATION####################
##Back up data frame for testing
testMSdf <- as.data.frame(MSdf)
##Rounding the sentiment scores 
testMSdf <- round(testMSdf$MSscore,digits = 3)

