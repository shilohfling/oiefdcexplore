##Package for opening/ reading Excel files
library(openxlsx)
##Microsoft Sentiment Analysis API Package
library(mscstexta4r)
##Used for working with URLs and HTTP
library(httr)
##Calculate Text Polarity Sentiment
library(sentimentr) 
##A package used for data frame manipulation
library(dplyr)
##Text analysis tools for dplyre
library(tidytext)

##TwinWord API call
callTW <- function(char) {
      if (Sys.getenv("TWApiKey") == "") {
            stop(cat("Please place token file in .Renviron."))
      } else {
            key <- Sys.getenv("TWApiKey") 
      }

      headers <- c("X-Mashape-Key" = key)                           
      url <- parse_url("https://twinword-sentiment-analysis.p.mashape.com/analyze/")
      url$query <- list("text" = char)                                            
      response <- POST(build_url(url), add_headers(headers),         
                       content_type("application/x-www-form-urlencoded"),   
                       accept_json())
      results <- content(response, "parsed")
      return(results)
}

##Load tidytext package before using this function
makeSentenceDF <- function(df, colname, thresh) {
      df[[colname]][is.na(df[[colname]])] <- ""
      x <- df %>%
            rename_("FOR_ANALYSIS" = colname) %>% 
            select(-contains("comment")) %>% 
            unnest_tokens("sentences", "FOR_ANALYSIS", token = "sentences")
        
        ##Specify a length of sentence to replace with NA
        x$sentences[nchar(x$sentences) < thresh] <- NA 
        
        return(x)
}

##A function used to get the sentiment scores from the Twinword API
addSentimentTW <- function(sentence_list) {
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
                  TWresults <- callTW(sentence)
                  print(TWresults$type)
                  sentType[n] <- as.character(TWresults$type)
                  sentScore[n] <- as.numeric(TWresults$score)
                  sentRatio[n] <- as.numeric(TWresults$ratio)
            }
            print(n)
      }
      outputdf <- data.frame(sentType, sentScore, sentRatio)
      return(outputdf)
}

##A function that uses the Microsoft Azure Sentiment API
addSentimentMS <- function(sentence_list) {
      nn <- 0
      
      MSscore <- numeric()
      MStopics <- character()
      
      for (sentence in sentence_list$sentences) {
            nn <- nn + 1
                        
            if(is.na(sentence)) {
                  MSscore[nn] <- NA
                  MStopics[nn] <- NA
            } else {
                  MSresults <- textaSentiment(sentence)
                  MStopicsResults <- textaKeyPhrases(sentence)
                  
                  MSscore[nn] <- MSresults$results$score
                  if (length(MStopicsResults$results$keyPhrases[[1]]) > 0) {
                        MStopics[nn] <- paste(MStopicsResults$results$keyPhrases[[1]], collapse = ", ")
                  } else {
                        MStopics[nn] <- NA
                  }
                  
            }
            print(nn)
            Sys.sleep(60/100)
      }
      outputdf <- data.frame(MSscore, MStopics)
      return(outputdf)
}

checkExists <- function(dir, col) {
      files <- list.files(dir)
      files <- gsub("(.*)\\.RDS$", "\\1", files)
      
      if(col %in% files) {
            stop("An object already exists with the supplied name.")
      }
}

###UNUSED FUNCTIONS

# ##Make a function that calls Twinword and Azure to calculate sentiment
# ##Use this to validate sentiment scores and compare the API's to see which is more accurate
# compareSentiment <- function(df) {
#   addSentimentTW()            ##Not fully sure how this is going to play out yet, just an idea
#   getSentimentMS() 
#   sentimentr()
# }

##A function that asks for a data frame and the maximum rows in the data frame and adds an element ID
##This will be used to rejoin data frames
# elementID <- function(df, maxID) {
#         n <- 0
#         data_clean$element_id <- NA
#         while (n < maxID) {
#                 n <- n + 1
#                 data_clean$element_id[[n]]<- n
#                 print(n)
#         }
#         return(data_clean)
# }


##A function used to parse sentences apart
# makeSentenceDF <- function(df, colname) {
#         x <- df[[colname]]
#         sent_list <- sentiment(x)
#         sentences <- get_sentences(sent_list)
#         sent_list$sentences <- unlist(sentences)
#         return(sent_list)
# }

# stripWhiteSpace <- function(data_clean) {
#   n <- 0
#   for (a in data_clean) {
#     n <- n + 1
#     data_clean$comment.overall[n] <- strsplit(comment, "\\.|\\?|\\!")
#     data_clean$comment.overall[n] <- lapply(comment, function(x) x <- x[!x == ""])
#   }
#   return(data_clean$comment.overall)
# }