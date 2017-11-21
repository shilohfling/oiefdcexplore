##This script uses the data_clean file creares in 01-GetCleanData
##to build a data frame of sentiment and topic data for specified columns
##open-ended text.  The result is a saved RDS file.

##The TwinWord API loads key calles "TWApiKey" from .Renviron
source("00-Functions.R")

##Initializes mscstexta4r with URL and Key passed here from file .mscskeys.json
textaInit()

######Configurations
##Character of the full text column to generate data for
text_col <- "comment.definingmoments"
rds_folder <- "../sources/comments"


##Perform a check to see if this object already exists, stop if it does
checkExists(rds_folder, text_col)

##Create a dataframe of sentences to build sentiment and topic data into
##Because of the nature of the data set, there are comments 
##where people responded with "No" or "N/A".  This causes confusion 
##in the sentiment services, so we set a threshold in order to change 
##them to NA values.  Suggested threshold is 4.
sentdf <- makeSentenceDF(data_clean, text_col, 4)

##Functions to add sentiment and topic data
sentRdf <- sentiment(sentdf$sentences)    ##Rsentiment scores (created locally)
sentdf <- cbind(sentdf, sentRdf)          ##Binding the twinword sentiment scores back to the dataframe
TWdf <- addSentimentTW(sentdf)            ##Twinword dataframe and call
sentdf <- cbind(sentdf, TWdf)             ##Binding the twinword sentiment scores back to the dataframe
MSdf <- addSentimentMS(sentdf)            ##Microsoft Azure sentiment call. Limit 100 per minute.
sentdf <- cbind(sentdf, MSdf)             ##Bind the Azure sentiment data to the data frame

##Save the object with the data added
wd <- getwd()
file_name <- paste0(text_col, ".RDS")
setwd(rds_folder)
saveRDS(sentdf, file_name)                ##Save the sentiment data frame as an RDS 
setwd(wd)

