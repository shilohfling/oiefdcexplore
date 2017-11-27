##Load a sentence scored object
sentDF <- readRDS("../sources/comments/comment.definingmoments.RDS")

##Create a word frequency data frame
commentWordsDF <- sentDF %>%
        
        unnest_tokens(WORD, sentences, drop = FALSE) %>%
        group_by(major, WORD) %>%                       ##Subsetting the data based on Major
        mutate(MAJOR_MEAN = mean(sentiment)) %>% 
        mutate(MAJOR_COUNT = n()) %>%
        group_by(campus, WORD) %>%                      ##Subsetting the data based on Campus
        mutate(CAMPUS_MEAN = mean(sentiment)) %>% 
        mutate(CAMPUS_COUNT = n()) %>%
        group_by(department, degree, WORD) %>%          ##Subsetting the data based on Department & Degree
        mutate(DEPT_DG_MEAN = mean(sentiment)) %>%      
        mutate(DEPT_DG_COUNT = n()) %>% 
        anti_join(stop_words %>% filter(lexicon == "snowball") %>% select(word), 
                  by = c("WORD" = "word"))              ##Remove the stop words, i.e. the, i, and

##EXPLORATION - Find a way to melt the sentences into words, but carry their sentiment with them
##Then find the mean of MSscores across each word
testing <- sentDF %>%                                   ##This function only works at the sentence level
        group_by(sentences, MSscore) %>%                ##I'm not sure how to extract the sentiment score and apply it to each word in the sentence
        summarise(mean=mean(MSscore), sd=sd(MSscore))


#unique(commentWordsDF$WORD[commentWordsDF$MAJOR_COUNT == max(commentWordsDF$MAJOR_COUNT)])
#unique(commentWordsDF$sentences[commentWordsDF$MAJOR_MEAN < .1])
