##Load a sentence scored object
sentDF <- readRDS("../sources/comments/comment.definingmoments.RDS")

##Create a word frequency data frame
commentWordsDF <- sentDF %>%
        unnest_tokens(WORD, sentences) %>%
        #mutate(SENT_MEAN = ) %>% 
        count(degree, campus, WORD, sort = TRUE)


##EXPLORATION - Find a way to melt the sentences into words, but carry their sentiment with them
##Then find the mean of MSscores across each word
testing <- sentDF %>%                                   ##This function only works at the sentence level
        group_by(sentences, MSscore) %>%                ##I'm not sure how to extract the sentiment score and apply it to each word in the sentence
        summarise(mean=mean(MSscore), sd=sd(MSscore))
