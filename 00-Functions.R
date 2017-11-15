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
      results <- content(response, "parsed")
      return(results)
}

makeSentenceDF <- function(df, colname) {
        x <- df[[colname]]
        sent_list <- sentiment(x)
        sentences <- get_sentences(sent_list)
        sent_list$sentences <- unlist(sentences)
        return(sent_list)
}

addSentimentTW <- function(sentence_list, key = NULL) {
        ##A loop that gets a list of sentences
        sentType <- character()
        sentScore <- numeric()
        sentRatio <- numeric()
        n <- 0
        
        for(sentence in sentence_list$sentences) {
                n <- n + 1
                        if(is.na(sentence)) {
                                sentType[n] <- NA
                                sentScore[n] <- NA
                                sentRatio[n] <- NA
                        } else {
                                
                                TWresults <- callTW(sentence, key)
                                
                                ##Sentiment scores are stored in the dataframe      
                                #outputdf$sentType[nn] <- TWresults$type
                                sentType[n] <- as.character(TWresults$type)
                                #outputdf$sentScore[nn] <- TWresults$score
                                sentScore[n] <- as.numeric(TWresults$score)
                                #outputdf$sentRatio[nn] <- TWresults$ratio
                                sentRatio[n] <- as.numeric(TWresults$ratio)
                        }
                }
                outputdf <- data.frame(sentType, sentScore, sentRatio)
                ##Dataframe is stored in a list
                print(n)
        
                return(outputdf)
        }



##Get the sentiment type, score, and ratio for tweets using the twinword API
##for a character vector
# getSentimentTW <- function(sentence_list, key = NULL) {
#       output <- list()        ##A list to store the output information
#       
#       n <- 0                  ##A loop that gets a list of sentences
#       for(comments in sentence_list) {
#               n <- n + 1
#               nn <- 0
#               sentType <- character()
#               sentScore <- numeric()
#               sentRatio <- numeric()
#               #outputdf <- data.frame("sentType" = NA, "sentScore" = NA, "sentRatio" = NA)
#               for (sentence in comments) {
#                       nn <- nn + 1
#                       
#                       if(is.na(sentence)) {
#                               break
#                       } else {
#                             
#                               TWresults <- callTW(sentence, key)
# 
#                               ##Sentiment scores are stored in the dataframe      
#                               #outputdf$sentType[nn] <- TWresults$type
#                               sentType[nn] <- as.character(TWresults$type)
#                               #outputdf$sentScore[nn] <- TWresults$score
#                               sentScore[nn] <- as.numeric(TWresults$score)
#                               #outputdf$sentRatio[nn] <- TWresults$ratio
#                               sentRatio[nn] <- as.numeric(TWresults$ratio)
#                       }
#                 }
#               outputdf <- data.frame(sentType, sentScore, sentRatio)
#               ##Dataframe is stored in a list
#               output[[n]] <- outputdf
#               print(n)
#             }
#       return(output)
# }


##A function that uses the Microsoft Azure Sentiment API
getSentimentMS <- function(sentence_list) {
                nn <- 0

                MSscore <- numeric()
                MStopics <- character()
                #outputdf <- data.frame("sentType" = NA, "sentScore" = NA, "sentRatio" = NA)
                for (sentence in sentence_list) {
                        nn <- nn + 1
                        
                        if(is.na(sentence)) {
                                MSscore[nn] <- NA
                                MStopics[nn] <- NA
                        } else {
                                
                                MSresults <- textaSentiment(sentence)
                                MStopicsResults <- textaKeyPhrases(sentence)
                                ##Sentiment scores are stored in the dataframe      
                                #outputdf$sentType[nn] <- TWresults$type
                                MSscore[nn] <- MSresults$results$score
                                MStopics[nn] <- unlist(MStopicsResults$results$keyPhrases)
                        }
                        yyy <<- MSscore     
                        xxx <<- MStopics
                outputdf <- data.frame(MSscore, MStopics)
                ##Dataframe is stored in a list
                print(nn)
                }
                return(outputdf)
        }



##Make a function that calls Twinword and Azure to calculate sentiment
##Use this to validate sentiment scores and compare the API's to see which is more accurate
compareSentiment <- function(df) {
  addSentimentTW()            ##Not fully sure how this is going to play out yet, just an idea
  getSentimentMS() 
  sentimentr()
}