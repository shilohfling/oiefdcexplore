##FEATURES:
##First check the .Renviron for the appropriate API key
##Second, it should use a key passed as an argument
##How can we work on keeping this versatile and calling mutliple columns as a vector?

##Make a list to stuff the functions into 
WUTIL <- list()

##Get the sentiment type, score, and ratio for tweets using the twinword API
WUTIL$getSentiment <- function(df, key = NULL) {
      df$sentType <- NA
      df$sentScore <- NA
      df$sentRatio <- NA
      
      words <- list()
      
      n <- 0
      for(a in df$text) {
            n <- n + 1
            
            headers <- c("X-Mashape-Key" = key)                           
            url <- parse_url("https://twinword-sentiment-analysis.p.mashape.com/analyze/")
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