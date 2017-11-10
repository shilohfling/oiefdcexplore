##FEATURES:
##First check the .Renviron for the appropriate API key
##Second, it should use a key passed as an argument
##How can we work on keeping this versatile and calling mutliple columns as a vector?


# stripWhiteSpace <- function(data_clean) {
#   n <- 0
#   for (a in data_clean) {
#     n <- n + 1
#     data_clean$comment.overall[n] <- strsplit(comment, "\\.|\\?|\\!")
#     data_clean$comment.overall[n] <- lapply(comment, function(x) x <- x[!x == ""])
#   }
#   return(data_clean$comment.overall)
# }

##TwinWord API call
callTW <- function(char, key) {
      headers <- c("X-Mashape-Key" = key)                           
      url <- parse_url("https://twinword-sentiment-analysis.p.mashape.com/analyze/")
      url$query <- list("text" = char)                                            
      response <- POST(build_url(url), add_headers(headers),         
                       content_type("application/x-www-form-urlencoded"),   
                       accept_json())
      content <- content(response, "parsed")
      return(content)
}


##Get the sentiment type, score, and ratio for tweets using the twinword API
getSentimentTW <- function(sentence_list, key = NULL) {
      output <- list()        ##A list to store the output information
      
      n <- 0                  ##A loop that gets a list of sentences
      for(a in sentence_list) {
            if(is.na(a)) {    ##Determines if the sentences is NA value (TRUE or FALSE)
                  n <- n + 1
                  output[[n]] <- NA 
            } else {
                  ##Creates a dataframe to store the sentiment scores
                  outputdf <- data.frame("sentType" = NA, "sentScore" = NA, "sentRatio" = NA)

                  n <- n + 1
                  for (b in a) {
                        ##Initializes the Twinword Sentiment API
                        callTW(b, key)      
                        ##Sentiment scores are stored in the dataframe      
                        outputdf$sentType[n] <- content$type
                        outputdf$sentScore[n] <- content$score
                        outputdf$sentRatio[n] <- content$ratio
                  }
                  ##Dataframe is stored in a list
                  output[[n]] <- outputdf
            }
      }
      return(output)
}



##Analyze the sentiment of the same comments using Microsoft Azure Sentiment API
getSentimentAZ <- function(data_clean, n = 10) {
  data_clean$MINaz <- NA
  data_clean$MAXaz <- NA
  data_clean$MEANaz <- NA
  
    n <- 0
    for(a in data_clean$comment.overall) {
      n <- n + 1
        ##INITIALIZE AZURE API
        ##SKIP N/A VALUES
        ##GET COMMENT
        ##SET MINIMUM THRESHOLD FOR CHARACTER LENGTH
        ##RUN AZURE API CALCULATIONS
        textaSentiment(data_clean$comment.overall) ##Sentiment scores - how can we isolate min, max, mean?
        textaKeyPhrases()           ##Determine themes
        ##RETURN VALUE TO RESPECTIVE COLUMN IN DATA_CLEAN BEFORE EXITING LOOP
        data_clean$MINaz <- min ##I need to look up what exactly the values are
        data_clean$MAXaz <- max
        data_clean$MEANaz <- mean 
    }
  return(data_clean$comment.overall)
}

##Make a function that calls Twinword and Azure to calculate sentiment
##Use this to validate sentiment scores and compare the API's to see which is more accurate
compareSentiment <- function(data_clean) {
  getTwinwordSentiment()            ##Not fully sure how this is going to play out yet, just an idea
  getAzureSentiment()  
}