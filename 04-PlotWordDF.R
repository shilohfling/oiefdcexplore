library(ggplot2)
library(dplyr)
library(ggrepel)

source("03-BuildWordDF.R")

campus_top_10 <- commentWordsDF %>% 
                filter(CAMPUS_COUNT > 1) %>%
                group_by(campus) %>%
                top_n(-10, COMMENT_MEAN) %>%
                #top_n(10, CAMPUS_MEAN) %>%
                unnest_tokens(campus_top_10, WORD, drop = FALSE)        
        

dept_top_10 <- commentWordsDF %>% 
                subset(DEPT_DG_COUNT > 1) %>%
                group_by(department) %>%
                top_n(-10, DEPT_DG_MEAN) %>%
                top_n(10, DEPT_DG_MEAN) %>%
                unnest_tokens(dept_top_10, WORD, drop = FALSE) 


major_top_10 <- commentWordsDF %>% 
                subset(MAJOR_COUNT > 1) %>%
                group_by(major) %>%
                top_n(-10, MAJOR_MEAN) %>%
                top_n(10, MAJOR_MEAN) %>%
                unnest_tokens(major_top_10, WORD, drop = FALSE) 



##########EXPLORATION##########
examplePlot <- ggplot(campus_top_10, aes(CAMPUS_COUNT, campus)) + geom_point()
examplePlot

examplePlot2 <- ggplot(campus_top_10, aes(CAMPUS_COUNT, COMMENT_MEAN, label = WORD)) + geom_text()
examplePlot2


ggsave("examplePlot2.jpg", plot = examplePlot2, width = 25, height = 25)





