##FEATURES:
##First check the .Renviron for the appropriate API key
##Second, it should use a key passed as an argument
##How can we work on keeping this versatile and calling mutliple columns as a vector?

##Make a list to stuff the functions into 
WUTIL <- list()

WUTIL$wordListtoDF <- function(list) {
  words <- unlist(lapply(list, function(x) lapply(x, "[[", "word")))
  score <- unlist(lapply(list, function(x) lapply(x, "[[", "score")))
  df <- data.frame(words = words, score = score)
  
  return(df)
}

##Get the sentiment type, score, and ratio for tweets using the twinword API
WUTIL$getSentiment <- function(df, key = NULL, max = 100) {
      df$sentType <- NA
      df$sentScore <- NA
      df$sentRatio <- NA
      
      words <- list()
      
      n <- 0
      for(a in df$text) {
            n <- n + 1
            
            headers <- c("Azure" = key)                           
            url <- parse_url("https://eastus2.api.cognitive.microsoft.com/text/analytics/v2.0")
            url$query <- list("text" = a)                                            
            response <- POST(build_url(url), add_headers(headers),         
                             content_type("application/x-www-form-urlencoded"),   
                             accept_json())
            #response <<- response
            content <- content(response, "parsed")
            
            print(paste(n, content$result_msg, sep = " "))
            
            df$sentType[n] <- content$type
            df$sentScore[n] <- content$score
            df$sentRatio[n] <- content$ratio
            
            words[[n]] <- content$keywords
      }
      
      words <- WUTIL$wordListtoDF(words)
      
      final <- list(df = df, words = words)
      return(final)
}