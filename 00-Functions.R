##FEATURES:
##First check the .Renviron for the appropriate API key
##Second, it should use a key passed as an argument
##How can we work on keeping this versatile and calling mutliple columns as a vector?


##Not sure how this function is applicable for this project
##Came from our twitter sentiment project
# WUTIL$wordListtoDF <- function(list) {
#   words <- unlist(lapply(list, function(x) lapply(x, "[[", "word")))
#   score <- unlist(lapply(list, function(x) lapply(x, "[[", "score")))
#   data_clean <- data.frame(words = words, score = score)
#   
#   return(df)
# }


stripWhiteSpace <- function(data_clean) {
  n <- 0
  for (a in data_clean)
    n <- n + 1
    data_clean$comment.overall[n] <- strsplit(comment, "\\.|\\?|\\!")
    data_clean$comment.overall[n] <- lapply(comment, function(x) x <- x[!x == ""])
}

##Get the sentiment type, score, and ratio for tweets using the twinword API
getTwinwordSentiment <- function(data_clean, key = NULL, n = 100) {
    data_clean$sentType[n] <- NA      ##MIN,MAX,MEAN not availble with twinword sentiment API
    data_clean$sentScore[n] <- NA     ##Twinword Sentiment calculates sent Score and Ratio
    data_clean$sentRatio[n] <- NA     ##How can we work on making this into desired values?

      n <- 0
      for(a in data_clean$comment.overall) {
            n <- n + 1
            
            headers <- c("X-Mashape-Key" = key)                           
            url <- parse_url("https://twinword-sentiment-analysis.p.mashape.com/analyze/")
            url$query <- list("text" = a)                                            
            response <- POST(build_url(url), add_headers(headers),         
                             content_type("application/x-www-form-urlencoded"),   
                             accept_json())
            #response <<- response
            stripWhiteSpace()
            
            content <- content(response, "parsed")
            
            print(paste(n, content$result_msg, sep = " "))
            
            data_clean$sentType[n] <- content$type
            data_clean$sentScore[n] <- content$score
            data_clean$sentRatio[n] <- content$ratio
      }
      
      return(data_clean$comment.overall)
}

##Analyze the sentiment of the same comments using Microsoft Azure Sentiment API
getAzureSentiment <- function(data_clean, n = 10) {
  data_clean$MINaz <- NA
  data_clean$MAXaz <- NA
  data_clean$MEANaz <- NA
  
    n <- 0
    for(a in data_clean$comment.overall) {
      n <- n + 1
        ##CALL AZURE API
        ##GET COMMENT
        ##CALL STRINGSPLIT FUNCTION
        stripWhiteSpace()
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