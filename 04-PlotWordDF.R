library(ggplot2)
library(dplyr)
#library(ggrepel)

source("03-BuildWordDF.R")

campus_top_10 <- commentWordsDF %>% 
                #filter(CAMPUS_COUNT > 1) %>%
                group_by(campus) %>%
                #top_n(-10, COMMENT_MEAN) %>%
                #top_n(10, COMMENT_MEAN) %>%
                unnest_tokens(campus_top_10, WORD, drop = FALSE)        
        

dept_top_10 <- commentWordsDF %>% 
                #subset(DEPT_DG_COUNT > 1) %>%
                group_by(department) %>%
                top_n(-10, DEPT_DG_MEAN) %>%
                top_n(10, DEPT_DG_MEAN) %>%
                unnest_tokens(dept_top_10, WORD, drop = FALSE) 


major_top_10 <- commentWordsDF %>% 
                #subset(MAJOR_COUNT > 1) %>%
                group_by(major) %>%
                top_n(-10, MAJOR_MEAN) %>%
                top_n(10, MAJOR_MEAN) %>%
                unnest_tokens(major_top_10, WORD, drop = FALSE) 



##########EXPLORATION##########
examplePlot <- ggplot(campus_top_10, aes(WORD, COMMENT_MEAN)) + geom_point()
examplePlot

examplePlot2 <- ggplot(campus_top_10, aes(CAMPUS_COUNT, COMMENT_MEAN, label = WORD)) + geom_text()
examplePlot2


#ggsave("examplePlot2.jpg", plot = examplePlot2, width = 25, height = 25)


#x$newfield <- quantile(x$oldfield)
#campus10 <- list()
#campus10$QUANTILES <- quantile(campus_top_10$CAMPUS_COUNT)
#campus10 <- campus10$QUANTILES

campus_top_10 <- commentWordsDF
campus_top_10 <- campus_top_10 %>% mutate(CAMPUS_COUNT_QUANTILE = ntile(CAMPUS_COUNT, 6)) %>% 
        filter(campus == "Webster Groves, MO") %>% 
        filter(department == "Communications & Journalism")

examplePlot3 <- ggplot(campus_top_10, aes(x=major, y=DEPT_DG_MEAN, label=WORD, color=DEPT_DG_MEAN, size = abs(DEPT_DG_MEAN*DEPT_DG_COUNT))) + geom_text()
examplePlot3

examplePlot4 <- ggplot(campus_top_10, aes(x=major, y=CAMPUS_MEAN, label=WORD, color=CAMPUS_MEAN, size=CAMPUS_MEAN)) + geom_text()
examplePlot4



